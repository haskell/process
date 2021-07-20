#include "common.h"

#include <sys/types.h>
#include <sys/wait.h>
#include <errno.h>
#include <unistd.h>
#include <stdlib.h>

#if !(defined(_MSC_VER) || defined(__MINGW32__) || defined(_WIN32))
#include <pwd.h>
#include <grp.h>
#endif

#if defined(HAVE_FCNTL_H)
#include <fcntl.h>
#endif

#if defined(HAVE_SIGNAL_H)
#include <signal.h>
#endif

#if defined(HAVE_VFORK_H)
#include <vfork.h>
#endif

#include <Rts.h>

#if defined(HAVE_WORKING_VFORK)
#define myfork vfork
#elif defined(HAVE_WORKING_FORK)
#define myfork fork
// We don't need a fork command on Windows
#else
#error Cannot find a working fork command
#endif

// Rts internal API, not exposed in a public header file:
extern void blockUserSignals(void);
extern void unblockUserSignals(void);

__attribute__((__noreturn__))
static void
child_failed(int pipe, const char *failed_doing) {
    int err;
    ssize_t unused __attribute__((unused));

    err = errno;
    // Having the child send the failed_doing pointer across the pipe is safe as
    // we know that the child still has the same address space as the parent.
    unused = write(pipe, &failed_doing, sizeof(failed_doing));
    unused = write(pipe, &err,         sizeof(err));
    // As a fallback, exit
    _exit(127);
}

static int
setup_std_handle_fork(int fd,
                      struct std_handle *b,
                      int pipe)
{
    switch (b->behavior) {
    case STD_HANDLE_CLOSE:
        if (close(fd) == -1) {
            child_failed(pipe, "close");
        }
        return 0;

    case STD_HANDLE_USE_FD:
        if (dup2(b->use_fd,  fd) == -1) {
            child_failed(pipe, "dup2");
        }
        return 0;

    case STD_HANDLE_USE_PIPE:
        if (b->use_pipe.child_end != fd) {
            if (dup2(b->use_pipe.child_end, fd) == -1) {
                child_failed(pipe, "dup2(child_end)");
            }
            if (close(b->use_pipe.child_end) == -1) {
                child_failed(pipe, "close(child_end)");
            }
        }
        if (close(b->use_pipe.parent_end) == -1) {
            child_failed(pipe, "close(parent_end)");
        }
	return 0;

    default:
	// N.B. this should be unreachable but some compilers apparently can't
	// see this.
        child_failed(pipe, "setup_std_handle_fork(invalid behavior)");
    }
}

/* Try spawning with fork. */
ProcHandle
do_spawn_fork (char *const args[],
               char *workingDirectory, char **environment,
               struct std_handle *stdInHdl,
               struct std_handle *stdOutHdl,
               struct std_handle *stdErrHdl,
               gid_t *childGroup, uid_t *childUser,
               int flags,
               char **failed_doing)
{
    int forkCommunicationFds[2];
    int r = pipe(forkCommunicationFds);
    if (r == -1) {
        *failed_doing = "pipe";
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

    // N.B. execvpe is not supposed on some platforms. In this case
    // we emulate this using fork and exec. However, to safely do so
    // we need to perform all allocations *prior* to forking. Consequently, we
    // need to find_executable before forking.
#if !defined(HAVE_execvpe)
    char *exec_path;
    if (environment) {
        exec_path = find_executable(args[0]);
    }
#endif

    int pid = myfork();
    switch(pid)
    {
    case -1:
        unblockUserSignals();
        startTimer();
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

        if (childGroup) {
            if (setgid( *childGroup) != 0) {
                // ERROR
                child_failed(forkCommunicationFds[1], "setgid");
            }
        }

        if (childUser) {
            // Using setuid properly first requires that we initgroups.
            // However, to do this we must know the username of the user we are
            // switching to.
            struct passwd pw;
            struct passwd *res = NULL;
            int buf_len = sysconf(_SC_GETPW_R_SIZE_MAX);
            // TODO: Strictly speaking malloc is a no-no after fork() since it
            // may try to take a lock
            char *buf = malloc(buf_len);
            gid_t suppl_gid = childGroup ? *childGroup : getgid();
            if ( getpwuid_r(*childUser, &pw, buf, buf_len, &res) != 0) {
                child_failed(forkCommunicationFds[1], "getpwuid");
            }
            if ( res == NULL ) {
                child_failed(forkCommunicationFds[1], "getpwuid");
            }
            if ( initgroups(res->pw_name, suppl_gid) != 0) {
                child_failed(forkCommunicationFds[1], "initgroups");
            }
            if ( setuid( *childUser) != 0) {
                // ERROR
                child_failed(forkCommunicationFds[1], "setuid");
            }
        }

        unblockUserSignals();

        if (workingDirectory) {
            if (chdir (workingDirectory) < 0) {
                child_failed(forkCommunicationFds[1], "chdir");
            }
        }

        // Note [Ordering of handle closing]
        // ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
        // Ordering matters here.  If any of the FDs
        // 0,1,2 were initially closed, then our pipes may have used
        // these FDs.  So when we dup2 the pipe FDs down to 0,1,2, we
        // must do it in that order, otherwise we could overwrite an
        // FD that we need later. See ticket #431.

        setup_std_handle_fork(STDIN_FILENO,  stdInHdl,  forkCommunicationFds[1]);
        setup_std_handle_fork(STDOUT_FILENO, stdOutHdl, forkCommunicationFds[1]);
        setup_std_handle_fork(STDERR_FILENO, stdErrHdl, forkCommunicationFds[1]);

        if ((flags & RUN_PROCESS_IN_CLOSE_FDS) != 0) {
            int max_fd = get_max_fd();
            // XXX Not the pipe
            for (int i = 3; i < max_fd; i++) {
                if (i != forkCommunicationFds[1]) {
                    close(i);
                }
            }
        }

        /* Reset the SIGINT/SIGQUIT signal handlers in the child, if requested
         */
        if ((flags & RESET_INT_QUIT_HANDLERS) != 0) {
            struct sigaction dfl;
            (void)sigemptyset(&dfl.sa_mask);
            dfl.sa_flags = 0;
            dfl.sa_handler = SIG_DFL;
            (void)sigaction(SIGINT,  &dfl, NULL);
            (void)sigaction(SIGQUIT, &dfl, NULL);
        }

        /* the child */
        if (environment) {
#if defined(HAVE_execvpe)
            // XXX Check result
            execvpe(args[0], args, environment);
#else
            // XXX Check result
            execve(exec_path, args, environment);
#endif
        } else {
            // XXX Check result
            execvp(args[0], args);
        }

        child_failed(forkCommunicationFds[1], "exec");

    default:
        if ((flags & RUN_PROCESS_IN_NEW_GROUP) != 0) {
            setpgid(pid, pid);
        }
        close(forkCommunicationFds[1]);
        fcntl(forkCommunicationFds[0], F_SETFD, FD_CLOEXEC);

        break;
    }

    // If the child process had a problem, then it will tell us via the
    // forkCommunicationFds pipe. First we try to read what the problem
    // was. Note that if none of these conditionals match then we fall
    // through and just return pid.
    char *fail_reason;
    r = read(forkCommunicationFds[0], &fail_reason, sizeof(fail_reason));
    if (r == -1) {
        *failed_doing = "read pipe";
        pid = -1;
    }
    else if (r == sizeof(fail_reason)) {
        *failed_doing = fail_reason;

        // Now we try to get the errno from the child
        int err;
        r = read(forkCommunicationFds[0], &err, sizeof(err));
        if (r == -1) {
            *failed_doing = "read pipe";
        } else if (r != sizeof(err)) {
            *failed_doing = "read pipe bad length";
        } else {
            // If we succeed then we set errno. It'll be saved and
            // restored again below. Note that in any other case we'll
            // get the errno of whatever else went wrong instead.
            errno = err;
        }

        // We forked the child, but the child had a problem and stopped so it's
        // our responsibility to reap here as nobody else can.
        waitpid(pid, NULL, 0);

        // Already closed child ends above
        if (stdInHdl->behavior == STD_HANDLE_USE_PIPE) {
            close(stdInHdl->use_pipe.parent_end);
        }
        if (stdOutHdl->behavior == STD_HANDLE_USE_PIPE) {
            close(stdOutHdl->use_pipe.parent_end);
        }
        if (stdErrHdl->behavior == STD_HANDLE_USE_PIPE) {
            close(stdErrHdl->use_pipe.parent_end);
        }

        pid = -1;
    }
    else if (r != 0) {
        *failed_doing = "read pipe bad length";
        pid = -1;
    }

    close(forkCommunicationFds[0]);

    unblockUserSignals();
    startTimer();

    return pid;
}
