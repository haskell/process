/* ----------------------------------------------------------------------------
   (c) The University of Glasgow 2004-2020

   Support for System.Process
   ------------------------------------------------------------------------- */

#include "runProcess.h"
#include "common.h"

#if defined(HAVE_FORK)

#include <unistd.h>
#include <errno.h>
#include <sys/wait.h>

#ifdef HAVE_FCNTL_H
#include <fcntl.h>
#endif

#if defined(HAVE_SIGNAL_H)
#include <signal.h>
#endif

int
get_max_fd()
{
    static int cache = 0;
    if (cache == 0) {
#if HAVE_SYSCONF
        cache = sysconf(_SC_OPEN_MAX);
        if (cache == -1) {
            cache = 256;
        }
#else
        cache = 256;
#endif
    }
    return cache;
}

// If a process was terminated by a signal, the exit status we return
// via the System.Process API is (-signum). This encoding avoids collision with
// normal process termination status codes. See also #7229.
#define TERMSIG_EXITSTATUS(s) (-(WTERMSIG(s)))

/*
 * Spawn a new process. We first try posix_spawn but since this isn't supported
 * on all platforms we fall back to fork/exec in some cases on some platforms.
 */
static ProcHandle
do_spawn (char *const args[],
          char *workingDirectory, char **environment,
          struct std_handle *stdInHdl,
          struct std_handle *stdOutHdl,
          struct std_handle *stdErrHdl,
          gid_t *childGroup, uid_t *childUser,
          int flags,
          char **failed_doing)
{
    ProcHandle r;
    r = do_spawn_posix(args,
                       workingDirectory, environment,
                       stdInHdl, stdOutHdl, stdErrHdl,
                       childGroup, childUser,
                       flags,
                       failed_doing);
    if (r == -2) {
        // configuration not supported by posix_spawn, fall back to fork/exec
    } else {
        return r;
    }

    r = do_spawn_fork(args,
                      workingDirectory, environment,
                      stdInHdl, stdOutHdl, stdErrHdl,
                      childGroup, childUser,
                      flags,
                      failed_doing);
    return r;
}

enum pipe_direction {
    CHILD_READS, CHILD_WRITES
};

/* Initialize a std_handle_behavior from a "pseudo-fd":
 *   - fd == -1 means create a pipe
 *   - fd == -2 means close the handle
 *   - otherwise use fd
 * Returns 0 on success, -1 otherwise.
 */
static int
init_std_handle(int fd, enum pipe_direction direction,
                         /* out */ struct std_handle *hdl,
                         char **failed_doing)
{
    switch (fd) {
    case -1: {
        int pipe_fds[2];
        int r = pipe(pipe_fds);
        if (r == -1) {
            *failed_doing = "pipe";
            return -1;
        }

        int child_end  = direction == CHILD_READS ? 0 : 1;
        int parent_end = direction == CHILD_READS ? 1 : 0;
        *hdl = (struct std_handle) {
            .behavior = STD_HANDLE_USE_PIPE,
            .use_pipe = { .child_end = pipe_fds[child_end], .parent_end = pipe_fds[parent_end] }
        };
        break;
    }
    case -2:
        *hdl = (struct std_handle) {
            .behavior = STD_HANDLE_CLOSE
        };
        break;
    default:
        *hdl = (struct std_handle) {
            .behavior = STD_HANDLE_USE_FD,
            .use_fd = fd
        };
        break;
    }
    return 0;
}

ProcHandle
runInteractiveProcess (char *const args[],
                       char *workingDirectory, char **environment,
                       // handles to use for the standard handles. -1 indicates
                       // create pipe, -2 indicates close.
                       int fdStdIn, int fdStdOut, int fdStdErr,
                       // output arguments to return created pipe handle to caller
                       int *pfdStdInput, int *pfdStdOutput, int *pfdStdError,
                       gid_t *childGroup, uid_t *childUser,
                       int flags,
                       char **failed_doing)
{
    struct std_handle stdInHdl, stdOutHdl, stdErrHdl;
    ProcHandle r;

    // A bit of paranoia to ensure that we catch if we fail to set this on
    // failure.
    *failed_doing = NULL;

    // Ordering matters here, see below Note [Ordering of handle closing].
    if (init_std_handle(fdStdIn, CHILD_READS, &stdInHdl, failed_doing) != 0) {
        goto fail;
    }

    if (init_std_handle(fdStdOut, CHILD_WRITES, &stdOutHdl, failed_doing) != 0) {
        goto fail;
    }

    if (init_std_handle(fdStdErr, CHILD_WRITES, &stdErrHdl, failed_doing) != 0) {
        goto fail;
    }

    r = do_spawn(args,
                 workingDirectory, environment,
                 &stdInHdl, &stdOutHdl, &stdErrHdl,
                 childGroup, childUser,
                 flags,
                 failed_doing);
    if (r == -1) {
        goto fail;
    }

    // Close the remote ends of any pipes we created.
#define FINALISE_STD_HANDLE(hdl, pfd) \
    if (hdl.behavior == STD_HANDLE_USE_PIPE) { \
        close(hdl.use_pipe.child_end); \
        fcntl(hdl.use_pipe.parent_end, F_SETFD, FD_CLOEXEC); \
        *pfd  = hdl.use_pipe.parent_end; \
    }

    FINALISE_STD_HANDLE(stdInHdl,  pfdStdInput);
    FINALISE_STD_HANDLE(stdOutHdl, pfdStdOutput);
    FINALISE_STD_HANDLE(stdErrHdl, pfdStdError);
#undef FINALISE_STD_HANDLE

    return r;

fail:
#define CLOSE_PIPE(hdl) \
    if (hdl.behavior == STD_HANDLE_USE_PIPE) { \
        close(hdl.use_pipe.child_end); \
        close(hdl.use_pipe.parent_end); \
    }

    CLOSE_PIPE(stdInHdl);
    CLOSE_PIPE(stdOutHdl);
    CLOSE_PIPE(stdErrHdl);
#undef CLOSE_PIPE

    return -1;
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

    if ((res = waitpid(handle, &wstat, WNOHANG)) > 0) {
        if (WIFEXITED(wstat)) {
            *pExitCode = WEXITSTATUS(wstat);
            return 1;
        } else {
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
    }

    if (res == 0) return 0;

    if (errno == ECHILD)
    {
        *pExitCode = 0;
        return 1;
    }

    return -1;
}

int
waitForProcess (ProcHandle handle, int *pret)
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

#endif // HAVE_FORK
