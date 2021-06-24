/* ----------------------------------------------------------------------------
   (c) The University of Glasgow 2004

   Interface for code in runProcess.c (providing support for System.Process)
   ------------------------------------------------------------------------- */

#pragma once

#include "HsProcessConfig.h"
// Otherwise these clash with similar definitions from other packages:
#undef PACKAGE_BUGREPORT
#undef PACKAGE_NAME
#undef PACKAGE_STRING
#undef PACKAGE_TARNAME
#undef PACKAGE_VERSION

#if defined(_MSC_VER) || defined(__MINGW32__) || defined(_WIN32)
#define UNICODE
#include <windows.h>
#include <stdlib.h>
#include <stdbool.h>
#endif

#if !(defined(_MSC_VER) || defined(__MINGW32__) || defined(_WIN32))
#include <sys/types.h>
typedef pid_t ProcHandle;
#else
// Should really be intptr_t, but we don't have that type on the Haskell side
typedef PHANDLE ProcHandle;
#endif

#include "processFlags.h"

#if !(defined(_MSC_VER) || defined(__MINGW32__) || defined(_WIN32))

#include <pwd.h>
#include <grp.h>

extern ProcHandle runInteractiveProcess( char *const args[],
                                         char *workingDirectory,
                                         char **environment,
                                         int fdStdIn,
                                         int fdStdOut,
                                         int fdStdErr,
                                         int *pfdStdInput,
                                         int *pfdStdOutput,
                                         int *pfdStdError,
                                         gid_t *childGroup,
                                         uid_t *childUser,
                                         int flags,
                                         char **failed_doing);

#else

extern ProcHandle runInteractiveProcess( wchar_t *cmd,
                                         wchar_t *workingDirectory,
                                         wchar_t *environment,
                                         int fdStdIn,
                                         int fdStdOut,
                                         int fdStdErr,
                                         int *pfdStdInput,
                                         int *pfdStdOutput,
                                         int *pfdStdError,
                                         int flags,
                                         bool useJobObject,
                                         HANDLE *hJob );

extern ProcHandle runInteractiveProcessHANDLE ( wchar_t *cmd,
                                                wchar_t *workingDirectory,
                                                wchar_t *environment,
                                                HANDLE _stdin,
                                                HANDLE _stdout,
                                                HANDLE _stderr,
                                                HANDLE *pStdInput,
                                                HANDLE *pStdOutput,
                                                HANDLE *pStdError,
                                                int flags,
                                                bool useJobObject,
                                                HANDLE *hJob);

extern int terminateJob( ProcHandle handle );
extern int waitForJobCompletion( HANDLE hJob );

#endif

extern int terminateProcess( ProcHandle handle );
extern int getProcessExitCode( ProcHandle handle, int *pExitCode );
extern int waitForProcess( ProcHandle handle, int *ret );
