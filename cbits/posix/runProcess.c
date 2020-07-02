/* ----------------------------------------------------------------------------
   (c) The University of Glasgow 2004-2020

   Support for System.Process
   ------------------------------------------------------------------------- */

/* XXX This is a nasty hack; should put everything necessary in this package */
#include "HsBase.h"
#include "Rts.h"

#include "runProcess.h"

#include "execvpe.h"

/* ----------------------------------------------------------------------------
   UNIX versions
   ------------------------------------------------------------------------- */

// If a process was terminated by a signal, the exit status we return
// via the System.Process API is (-signum). This encoding avoids collision with
// normal process termination status codes. See also #7229.
#define TERMSIG_EXITSTATUS(s) (-(WTERMSIG(s)))

static long max_fd = 0;

// Rts internal API, not exposed in a public header file:
extern void blockUserSignals(void);
extern void unblockUserSignals(void);

// These are arbitrarily chosen -- JP
#define forkSetgidFailed 124
#define forkSetuidFailed 125

// See #1593.  The convention for the exit code when
// exec() fails seems to be 127 (gleened from C's
// system()), but there's no equivalent convention for
// chdir(), so I'm picking 126 --SimonM.
#define forkChdirFailed 126
#define forkExecFailed  127

#define forkGetpwuidFailed 128
#define forkInitgroupsFailed 129

__attribute__((__noreturn__))
static void childFailed(int pipe, int failCode) {
    int err;
    ssize_t unused __attribute__((unused));

    err = errno;
    unused = write(pipe, &failCode, sizeof(failCode));
    unused = write(pipe, &err,      sizeof(err));
    // As a fallback, exit with the failCode
    _exit(failCode);
}

ProcHandle
runInteractiveProcess (char *const args[],
                       char *workingDirectory, char **environment,
                       int fdStdIn, int fdStdOut, int fdStdErr,
                       int *pfdStdInput, int *pfdStdOutput, int *pfdStdError,
                       gid_t *childGroup, uid_t *childUser,
                       int reset_int_quit_handlers,
                       int flags,
                       char **failed_doing)
{
    int close_fds = ((flags & RUN_PROCESS_IN_CLOSE_FDS) != 0);
    int pid;
    int fdStdInput[2], fdStdOutput[2], fdStdError[2];
    int forkCommunicationFds[2];
    int r;
    int failCode, err;

    // Ordering matters here, see below [Note #431].
    if (fdStdIn == -1) {
        r = pipe(fdStdInput);
        if (r == -1) {
            *failed_doing = "runInteractiveProcess: pipe";
            return -1;
        }
    }
    if (fdStdOut == -1) {
        r = pipe(fdStdOutput);
        if (r == -1) {
            if (fdStdIn == -1) {
                close(fdStdInput[0]);
                close(fdStdInput[1]);
            }
            *failed_doing = "runInteractiveProcess: pipe";
            return -1;
        }
    }
    if (fdStdErr == -1) {
        r = pipe(fdStdError);
        if (r == -1) {
            *failed_doing = "runInteractiveProcess: pipe";
            if (fdStdIn == -1) {
                close(fdStdInput[0]);
                close(fdStdInput[1]);
            }
            if (fdStdOut == -1) {
                close(fdStdOutput[0]);
                close(fdStdOutput[1]);
            }
            return -1;
        }
    }

    r = pipe(forkCommunicationFds);
    if (r == -1) {
        *failed_doing = "runInteractiveProcess: pipe";
        if (fdStdIn == -1) {
            close(fdStdInput[0]);
            close(fdStdInput[1]);
        }
        if (fdStdOut == -1) {
            close(fdStdOutput[0]);
            close(fdStdOutput[1]);
        }
        if (fdStdErr == -1) {
            close(fdStdError[0]);
            close(fdStdError[1]);
        }
        return -1;
    }

    // Block signals with Haskell handlers.  The danger here is that
    // with the threaded RTS, a signal arrives in the child process,
    // the RTS writes the signal information into the pipe (which is
    // shared between parent and child), and the parent behaves as if
    // the signal had been raised.
    blockUserSignals();

    // See #4074.  Sometimes fork() gets interrupted by the timer
    // signal and keeps restarting indefinitely.
    stopTimer();

    switch(pid = myfork())
    {
    case -1:
        unblockUserSignals();
        startTimer();
        if (fdStdIn == -1) {
            close(fdStdInput[0]);
            close(fdStdInput[1]);
        }
        if (fdStdOut == -1) {
            close(fdStdOutput[0]);
            close(fdStdOutput[1]);
        }
        if (fdStdErr == -1) {
            close(fdStdError[0]);
            close(fdStdError[1]);
        }
        close(forkCommunicationFds[0]);
        close(forkCommunicationFds[1]);
        *failed_doing = "fork";
        return -1;

    case 0:
        // WARNING! We may now be in the child of vfork(), and any
        // memory we modify below may also be seen in the parent
        // process.

        close(forkCommunicationFds[0]);
        fcntl(forkCommunicationFds[1], F_SETFD, FD_CLOEXEC);

        if ((flags & RUN_PROCESS_NEW_SESSION) != 0) {
            setsid();
        }
        if ((flags & RUN_PROCESS_IN_NEW_GROUP) != 0) {
            setpgid(0, 0);
        }

        if ( childGroup) {
            if ( setgid( *childGroup) != 0) {
                // ERROR
                childFailed(forkCommunicationFds[1], forkSetgidFailed);
            }
        }

        if ( childUser) {
            // Using setuid properly first requires that we initgroups.
            // However, to do this we must know the username of the user we are
            // switching to.
            struct passwd pw;
            struct passwd *res = NULL;
            int buf_len = sysconf(_SC_GETPW_R_SIZE_MAX);
            char *buf = malloc(buf_len);
            gid_t suppl_gid = childGroup ? *childGroup : getgid();
            if ( getpwuid_r(*childUser, &pw, buf, buf_len, &res) != 0) {
                childFailed(forkCommunicationFds[1], forkGetpwuidFailed);
            }
            if ( res == NULL ) {
                childFailed(forkCommunicationFds[1], forkGetpwuidFailed);
            }
            if ( initgroups(res->pw_name, suppl_gid) != 0) {
                childFailed(forkCommunicationFds[1], forkInitgroupsFailed);
            }
            if ( setuid( *childUser) != 0) {
                // ERROR
                childFailed(forkCommunicationFds[1], forkSetuidFailed);
            }
        }

        unblockUserSignals();

        if (workingDirectory) {
            if (chdir (workingDirectory) < 0) {
                childFailed(forkCommunicationFds[1], forkChdirFailed);
            }
        }

        // [Note #431]: Ordering matters here.  If any of the FDs
        // 0,1,2 were initially closed, then our pipes may have used
        // these FDs.  So when we dup2 the pipe FDs down to 0,1,2, we
        // must do it in that order, otherwise we could overwrite an
        // FD that we need later.

        if (fdStdIn == -1) {
            if (fdStdInput[0] != STDIN_FILENO) {
                dup2 (fdStdInput[0], STDIN_FILENO);
                close(fdStdInput[0]);
            }
            close(fdStdInput[1]);
        } else if (fdStdIn == -2) {
            close(STDIN_FILENO);
        } else {
            dup2(fdStdIn,  STDIN_FILENO);
        }

        if (fdStdOut == -1) {
            if (fdStdOutput[1] != STDOUT_FILENO) {
                dup2 (fdStdOutput[1], STDOUT_FILENO);
                close(fdStdOutput[1]);
            }
            close(fdStdOutput[0]);
        } else if (fdStdOut == -2) {
            close(STDOUT_FILENO);
        } else {
            dup2(fdStdOut,  STDOUT_FILENO);
        }

        if (fdStdErr == -1) {
            if (fdStdError[1] != STDERR_FILENO) {
                dup2 (fdStdError[1], STDERR_FILENO);
                close(fdStdError[1]);
            }
            close(fdStdError[0]);
        } else if (fdStdErr == -2) {
            close(STDERR_FILENO);
        } else {
            dup2(fdStdErr,  STDERR_FILENO);
        }

        if (close_fds) {
            int i;
            if (max_fd == 0) {
#if HAVE_SYSCONF
                max_fd = sysconf(_SC_OPEN_MAX);
                if (max_fd == -1) {
                    max_fd = 256;
                }
#else
                max_fd = 256;
#endif
            }
            // XXX Not the pipe
            for (i = 3; i < max_fd; i++) {
                if (i != forkCommunicationFds[1]) {
                    close(i);
                }
            }
        }

        /* Reset the SIGINT/SIGQUIT signal handlers in the child, if requested
         */
        if (reset_int_quit_handlers) {
            struct sigaction dfl;
            (void)sigemptyset(&dfl.sa_mask);
            dfl.sa_flags = 0;
            dfl.sa_handler = SIG_DFL;
            (void)sigaction(SIGINT,  &dfl, NULL);
            (void)sigaction(SIGQUIT, &dfl, NULL);
        }

        /* the child */
        if (environment) {
            // XXX Check result
            execvpe(args[0], args, environment);
        } else {
            // XXX Check result
            execvp(args[0], args);
        }

        childFailed(forkCommunicationFds[1], forkExecFailed);

    default:
        if ((flags & RUN_PROCESS_IN_NEW_GROUP) != 0) {
            setpgid(pid, pid);
        }
        if (fdStdIn  == -1) {
            close(fdStdInput[0]);
            fcntl(fdStdInput[1], F_SETFD, FD_CLOEXEC);
            *pfdStdInput  = fdStdInput[1];
        }
        if (fdStdOut == -1) {
            close(fdStdOutput[1]);
            fcntl(fdStdOutput[0], F_SETFD, FD_CLOEXEC);
            *pfdStdOutput = fdStdOutput[0];
        }
        if (fdStdErr == -1) {
            close(fdStdError[1]);
            fcntl(fdStdError[0], F_SETFD, FD_CLOEXEC);
            *pfdStdError  = fdStdError[0];
        }
        close(forkCommunicationFds[1]);
        fcntl(forkCommunicationFds[0], F_SETFD, FD_CLOEXEC);

        break;
    }

    // If the child process had a problem, then it will tell us via the
    // forkCommunicationFds pipe. First we try to read what the problem
    // was. Note that if none of these conditionals match then we fall
    // through and just return pid.
    r = read(forkCommunicationFds[0], &failCode, sizeof(failCode));
    if (r == -1) {
        *failed_doing = "runInteractiveProcess: read pipe";
        pid = -1;
    }
    else if (r == sizeof(failCode)) {
        // This is the case where we successfully managed to read
        // the problem
        switch (failCode) {
        case forkChdirFailed:
            *failed_doing = "runInteractiveProcess: chdir";
            break;
        case forkExecFailed:
            *failed_doing = "runInteractiveProcess: exec";
            break;
        case forkSetgidFailed:
            *failed_doing = "runInteractiveProcess: setgid";
            break;
        case forkSetuidFailed:
            *failed_doing = "runInteractiveProcess: setuid";
            break;
        case forkGetpwuidFailed:
            *failed_doing = "runInteractiveProcess: getpwuid";
            break;
        case forkInitgroupsFailed:
            *failed_doing = "runInteractiveProcess: initgroups";
            break;
        default:
            *failed_doing = "runInteractiveProcess: unknown";
            break;
        }
        // Now we try to get the errno from the child
        r = read(forkCommunicationFds[0], &err, sizeof(err));
        if (r == -1) {
            *failed_doing = "runInteractiveProcess: read pipe";
        }
        else if (r != sizeof(failCode)) {
            *failed_doing = "runInteractiveProcess: read pipe bad length";
        }
        else {
            // If we succeed then we set errno. It'll be saved and
            // restored again below. Note that in any other case we'll
            // get the errno of whatever else went wrong instead.
            errno = err;
        }

        // We forked the child, but the child had a problem and stopped so it's
        // our responsibility to reap here as nobody else can.
        waitpid(pid, NULL, 0);

        if (fdStdIn == -1) {
            // Already closed fdStdInput[0] above
            close(fdStdInput[1]);
        }
        if (fdStdOut == -1) {
            close(fdStdOutput[0]);
            // Already closed fdStdOutput[1] above
        }
        if (fdStdErr == -1) {
            close(fdStdError[0]);
            // Already closed fdStdError[1] above
        }

        pid = -1;
    }
    else if (r != 0) {
        *failed_doing = "runInteractiveProcess: read pipe bad length";
        pid = -1;
    }

    if (pid == -1) {
        err = errno;
    }

    close(forkCommunicationFds[0]);

    unblockUserSignals();
    startTimer();

    if (pid == -1) {
        errno = err;
    }

    return pid;
}

int
terminateProcess (ProcHandle handle)
{
    return (kill(handle, SIGTERM) == 0);
}

int
getProcessExitCode (ProcHandle handle, int *pExitCode)
{
    int wstat, res;

    *pExitCode = 0;

    if ((res = waitpid(handle, &wstat, WNOHANG)) > 0)
    {
        if (WIFEXITED(wstat))
        {
            *pExitCode = WEXITSTATUS(wstat);
            return 1;
        }
        else
            if (WIFSIGNALED(wstat))
            {
                *pExitCode = TERMSIG_EXITSTATUS(wstat);
                return 1;
            }
            else
            {
                /* This should never happen */
            }
    }

    if (res == 0) return 0;

    if (errno == ECHILD)
    {
        *pExitCode = 0;
        return 1;
    }

    return -1;
}

int waitForProcess (ProcHandle handle, int *pret)
{
    int wstat;

    if (waitpid(handle, &wstat, 0) < 0)
    {
        return -1;
    }

    if (WIFEXITED(wstat)) {
        *pret = WEXITSTATUS(wstat);
        return 0;
    }
    else {
        if (WIFSIGNALED(wstat))
        {
            *pret = TERMSIG_EXITSTATUS(wstat);
            return 0;
        }
        else
        {
            /* This should never happen */
        }
    }

    return -1;
}
