AC_INIT([Haskell process package], [1.0], [libraries@haskell.org], [process])

# Safety check: Ensure that we are in the correct source directory.
AC_CONFIG_SRCDIR([include/runProcess.h])

AC_CONFIG_HEADERS([include/HsProcessConfig.h])

AC_PROG_CC

dnl ** Working vfork?
AC_FUNC_FORK

# check for specific header (.h) files that we are interested in
AC_CHECK_HEADERS([signal.h sys/wait.h fcntl.h])

AC_CHECK_FUNCS([setitimer sysconf])
AC_CHECK_FUNCS([execvpe])

# posix_spawn checks
AC_CHECK_HEADERS([spawn.h])
AC_CHECK_FUNCS([posix_spawnp posix_spawn_file_actions_addchdir],[],[],[
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

FP_CHECK_CONSTS([SIG_DFL SIG_IGN])

AC_OUTPUT
