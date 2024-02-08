#include "runProcess.h"
#include "common.h"

#include <unistd.h>
#include <errno.h>

#if defined(HAVE_SIGNAL_H)
#include <signal.h>
#include <fcntl.h>
#endif

#if !defined(USE_POSIX_SPAWN)
ProcHandle
do_spawn_posix (char *const args[],
                char *workingDirectory, char **environment,
                struct std_handle *stdInHdl,
                struct std_handle *stdOutHdl,
                struct std_handle *stdErrHdl,
                gid_t *childGroup, uid_t *childUser,
                int flags,
                char **failed_doing)
{
    return -2;
}

#else

// Necessary for POSIX_SPAWN_SETSID under glibc.
#define _GNU_SOURCE
#include <spawn.h>

extern char **environ;

static int
setup_std_handle_spawn (int fd,
                        struct std_handle *hdl,
                        posix_spawn_file_actions_t *fa,
                        char **failed_doing)
{
    switch (hdl->behavior) {
    case STD_HANDLE_CLOSE:
        // N.B. POSIX specifies that addclose() may result in spawnp() failing
        // if the fd to-be-closed is already closed. Consequently, we must
        // first open a file (e.g. /dev/null) and before attempting to close
        // the fd. Fixes #251.
        if (posix_spawn_file_actions_addopen(fa, fd, "/dev/null", O_RDONLY, 0) != 0) {
            *failed_doing = "posix_spawn_file_actions_addopen";
            return -1;
        }
        if (posix_spawn_file_actions_addclose(fa, fd) != 0) {
            *failed_doing = "posix_spawn_file_actions_addclose";
            return -1;
        }
        return 0;

    case STD_HANDLE_USE_FD:
        // N.B. POSIX specifies that dup2(x,x) should be a no-op, but
        // naturally Apple ignores this and rather fails in posix_spawn on Big
        // Sur.
        if (hdl->use_fd != fd) {
            if (posix_spawn_file_actions_adddup2(fa, hdl->use_fd, fd) != 0) {
                *failed_doing = "posix_spawn_file_actions_adddup2";
                return -1;
            }
       }
        return 0;

    case STD_HANDLE_USE_PIPE:
        if (hdl->use_pipe.child_end != fd) {
            if (posix_spawn_file_actions_adddup2(fa, hdl->use_pipe.child_end, fd) != 0) {
                *failed_doing = "posix_spawn_file_actions_adddup2(child_end)";
                return -1;
            }
            if (posix_spawn_file_actions_addclose(fa, hdl->use_pipe.child_end) != 0) {
                *failed_doing = "posix_spawn_file_actions_addclose(child_end)";
                return -1;
            }
        }
        if (posix_spawn_file_actions_addclose(fa, hdl->use_pipe.parent_end) != 0) {
            *failed_doing = "posix_spawn_file_actions_addclose(parent_end)";
            return -1;
        }
        return 0;

    default:
	// N.B. this should be unreachable
	// but some compilers apparently can't
	// see this.
        *failed_doing = "posix_spawn_file_actions_addclose(invalid behavior)";
        return -1;
    }
}

/* Try spawning with posix_spawn. Returns -1 if posix_spawn (or the particular
 * requested configuration) is not supported.
 */
ProcHandle
do_spawn_posix (char *const args[],
                char *workingDirectory, char **environment,
                struct std_handle *stdInHdl,
                struct std_handle *stdOutHdl,
                struct std_handle *stdErrHdl,
                gid_t *childGroup, uid_t *childUser,
                int flags,
                char **failed_doing)
{
    // First do catch some cases that posix_spawn simply can't handle...
    if (childGroup || childUser) {
        return -2;
    }
    if ((flags & RUN_PROCESS_IN_CLOSE_FDS) != 0) {
        // TODO: can this be efficiently supported?
        return -2;
    }

    // Now the main act...
    pid_t pid;
    posix_spawn_file_actions_t fa;
    posix_spawnattr_t sa;
    int r;
    ProcHandle ret;
    short spawn_flags = 0;

    r = posix_spawn_file_actions_init(&fa);
    if (r != 0) {
        *failed_doing = "posix_spawn_file_actions_init";
        return -1;
    }

    r = posix_spawnattr_init(&sa);
    if (r != 0) {
        posix_spawn_file_actions_destroy(&fa);
        *failed_doing = "posix_spawnattr_init";
        return -1;
    }

    if (workingDirectory) {
#if defined(HAVE_posix_spawn_file_actions_addchdir)
        r = posix_spawn_file_actions_addchdir(&fa, workingDirectory);
        if (r != 0) {
            *failed_doing = "posix_spawn_file_actions_addchdir";
            goto fail;
        }
#elif defined(HAVE_posix_spawn_file_actions_addchdir_np)
        // N.B. this function is broken on macOS.
        // See https://github.com/rust-lang/rust/pull/80537.
        r = posix_spawn_file_actions_addchdir_np(&fa, workingDirectory);
        if (r != 0) {
            *failed_doing = "posix_spawn_file_actions_addchdir_np";
            goto fail;
        }
#else
        goto not_supported;
#endif
    }

    if ((flags & RUN_PROCESS_NEW_SESSION) != 0) {
#if defined(HAVE_POSIX_SPAWN_SETSID)
        // N.B. POSIX_SPAWN_SETSID is only supported on macOS 10.15 and later,
        // but an proper error is not returned in earlier versions.
        // See https://bugs.python.org/issue37586.
        if (posix_spawnattr_setflags(&sa, POSIX_SPAWN_SETSID) != 0) {
            *failed_doing = "posix_spawnattr_setflags(POSIX_SPAWN_SETSID)";
            goto fail;
        }
#elif defined(HAVE_POSIX_SPAWN_SETSID_NP)
        if (posix_spawnattr_setflags(&sa, POSIX_SPAWN_SETSID_NP) != 0) {
            *failed_doing = "posix_spawnattr_setflags(POSIX_SPAWN_SETSID_NP)";
            goto fail;
        }
#else
        goto not_supported;
#endif
    }

    if ((flags & RUN_PROCESS_IN_NEW_GROUP) != 0) {
#if defined(HAVE_POSIX_SPAWN_SETPGROUP)
        spawn_flags |= POSIX_SPAWN_SETPGROUP;
#else
        goto not_supported;
#endif
    }

    if (setup_std_handle_spawn(STDIN_FILENO,  stdInHdl,  &fa, failed_doing) != 0) {
        goto fail;
    }
    if (setup_std_handle_spawn(STDOUT_FILENO, stdOutHdl, &fa, failed_doing) != 0) {
        goto fail;
    }
    if (setup_std_handle_spawn(STDERR_FILENO, stdErrHdl, &fa, failed_doing) != 0) {
        goto fail;
    }

    sigset_t ss;
    if ((flags & RESET_INT_QUIT_HANDLERS) != 0) {
        if (sigemptyset(&ss) == -1) {
            *failed_doing = "sigemptyset";
            goto fail;
        }
        if (sigaddset(&ss, SIGINT) == -1) {
            *failed_doing = "sigaddset(SIGINT)";
            goto fail;
        }
        if (sigaddset(&ss, SIGQUIT) == -1) {
            *failed_doing = "sigaddset(SIGQUIT)";
            goto fail;
        }
        if (posix_spawnattr_setsigdefault(&sa, &ss) != 0) {
            *failed_doing = "posix_spawnattr_setsigdefault";
            goto fail;
        }
        spawn_flags |= POSIX_SPAWN_SETSIGDEF;
    }

    if (posix_spawnattr_setflags(&sa, spawn_flags) != 0) {
        *failed_doing = "posix_spawnattr_setflags";
        goto fail;
    }

    r = posix_spawnp(&pid, args[0], &fa, &sa, args, environment ? environment : environ);
    if (r != 0) {
        errno = r; // posix_spawn doesn't necessarily set errno; see #227.
        *failed_doing = "posix_spawnp";
        goto fail;
    } else {
        ret = pid;
        goto finish;
    }

not_supported:
    ret = -2;
    goto finish;

fail:
    ret = -1;
    goto finish;

finish:
    posix_spawn_file_actions_destroy(&fa);
    posix_spawnattr_destroy(&sa);
    return ret;
}

#endif
