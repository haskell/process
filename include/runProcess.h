/* ----------------------------------------------------------------------------
   (c) The University of Glasgow 2004

   Interface for code in runProcess.c (providing support for System.Process)
   ------------------------------------------------------------------------- */

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
#endif

#include <unistd.h>
#include <sys/types.h>

#ifdef HAVE_FCNTL_H
#include <fcntl.h>
#endif

#ifdef HAVE_VFORK_H
#include <vfork.h>
#endif

#ifdef HAVE_VFORK
#define fork vfork
#endif

#ifdef HAVE_SIGNAL_H
#include <signal.h>
#endif

#if !(defined(_MSC_VER) || defined(__MINGW32__) || defined(_WIN32))
typedef pid_t ProcHandle;
#else
// Should really be intptr_t, but we don't have that type on the Haskell side
typedef PHANDLE ProcHandle;
#endif

#include "processFlags.h"

#if !(defined(_MSC_VER) || defined(__MINGW32__) || defined(_WIN32))

extern ProcHandle runInteractiveProcess( char *const args[], 
					 char *workingDirectory, 
					 char **environment, 
                                         int fdStdIn, int fdStdOut, int fdStdErr,
					 int *pfdStdInput, 
					 int *pfdStdOutput, 
					 int *pfdStdError,
                                         int set_inthandler, long inthandler, 
                                         int set_quithandler, long quithandler,
                                         int flags);

#else

extern ProcHandle runInteractiveProcess( wchar_t *cmd,
					 wchar_t *workingDirectory,
					 wchar_t *environment,
                                         int fdStdIn, int fdStdOut, int fdStdErr,
					 int *pfdStdInput,
					 int *pfdStdOutput,
					 int *pfdStdError,
                                         int flags);

#endif

extern int terminateProcess( ProcHandle handle );
extern int getProcessExitCode( ProcHandle handle, int *pExitCode );
extern int waitForProcess( ProcHandle handle, int *ret );
