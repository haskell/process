#pragma once

#include "runProcess.h"

enum std_handle_behavior {
    // Close the handle
    STD_HANDLE_CLOSE,
    // dup2 the specified fd to standard handle
    STD_HANDLE_USE_FD,
    // dup2 the appropriate end of the given pipe to the standard handle and
    // close the other end.
    STD_HANDLE_USE_PIPE
};

struct std_handle {
    enum std_handle_behavior behavior;
    union {
        int use_fd;
        struct {
            int parent_end, child_end;
        } use_pipe;
    };
};

int get_max_fd(void);

// defined in find_executable.c
#if !defined(HAVE_EXECVPE)
char *find_executable(char *workingDirectory, char *filename);
#endif

// defined in fork_exec.c
ProcHandle
do_spawn_fork (char *const args[],
               char *workingDirectory, char **environment,
               struct std_handle *stdInHdl,
               struct std_handle *stdOutHdl,
               struct std_handle *stdErrHdl,
               gid_t *childGroup, uid_t *childUser,
               int flags,
               char **failed_doing);

// defined in posix_spawn.c
ProcHandle
do_spawn_posix (char *const args[],
                char *workingDirectory, char **environment,
                struct std_handle *stdInHdl,
                struct std_handle *stdOutHdl,
                struct std_handle *stdErrHdl,
                gid_t *childGroup, uid_t *childUser,
                int flags,
                char **failed_doing);
