AC_INIT([Haskell process package],[1.0],[libraries@haskell.org],[process])

# Safety check: Ensure that we are in the correct source directory.
AC_CONFIG_SRCDIR([include/runProcess.h])

AC_CONFIG_HEADERS([include/HsProcessConfig.h])

AC_PROG_CC

dnl ** Working fork?
AC_FUNC_FORK

# check for specific header (.h) files that we are interested in
AC_CHECK_HEADERS([signal.h sys/wait.h fcntl.h])

AC_CHECK_FUNCS([setitimer sysconf])
AC_CHECK_FUNCS([execvpe])
AC_CHECK_FUNCS([pipe2],[],[],[
  #define _GNU_SOURCE
  #include <unistd.h>
  #include <fcntl.h>
])

# posix_spawn checks
AC_CHECK_HEADERS([spawn.h])
AC_CHECK_FUNCS([posix_spawnp posix_spawn_file_actions_addchdir_np posix_spawn_file_actions_addchdir],[],[],[
  #define _GNU_SOURCE
  #include <spawn.h>
])
AC_CHECK_DECLS([POSIX_SPAWN_SETSID, POSIX_SPAWN_SETSID_NP],[],[],[
  #define _GNU_SOURCE
  #include <spawn.h>
])
AC_CHECK_DECLS([POSIX_SPAWN_SETPGROUP],[],[],[
  #define _GNU_SOURCE
  #include <spawn.h>
])

if test "$ac_cv_func_posix_spawnp" = "yes"; then
    dnl On OpenBSD posix_spawnp doesn't report ENOENT when
    dnl the user attempts to spawn a process with a non-existent
    dnl executable. Don't attempt to use such posix_spawn
    dnl implementations to ensure errors are reported correctly.
    dnl See #224.

    AC_MSG_CHECKING(whether posix_spawn reports errors sensibly)
    AC_RUN_IFELSE(
        [AC_LANG_PROGRAM([
             #include <stddef.h>
             #include <stdbool.h>
             #include <errno.h>
             #include <spawn.h>
           ], [[
                 pid_t pid;
                 char *prog = "__nonexistent_program__";
                 char *args[] = {prog, NULL};
                 char *env[] = {NULL};

                 // This should fail with ENOENT
                 int ret = posix_spawnp(&pid, prog, NULL, NULL, args, env);
                 bool okay = ret == ENOENT;
                 return !okay;
           ]]
         )],
         [AC_DEFINE([USE_POSIX_SPAWN], [], [posix_spawn should be used])
          AC_MSG_RESULT(yes)],
         [AC_MSG_RESULT([no, falling back to fork/exec])],
         [AC_DEFINE([USE_POSIX_SPAWN], [], [])
          AC_MSG_RESULT(cross-compiling - assuming yes)]
    )
fi


FP_CHECK_CONSTS([SIG_DFL SIG_IGN])

AC_OUTPUT
