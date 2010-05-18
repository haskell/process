/* ----------------------------------------------------------------------------
   (c) The University of Glasgow 2004
   
   Support for System.Process
   ------------------------------------------------------------------------- */

#if defined(_MSC_VER) || defined(__MINGW32__) || defined(_WIN32)
#define UNICODE
#endif

/* XXX This is a nasty hack; should put everything necessary in this package */
#include "HsBase.h"
#include "Rts.h"

#include "runProcess.h"

#if !(defined(_MSC_VER) || defined(__MINGW32__) || defined(_WIN32))

#include "execvpe.h"

/* ----------------------------------------------------------------------------
   UNIX versions
   ------------------------------------------------------------------------- */

static long max_fd = 0;

// Rts internal API, not exposed in a public header file:
extern void blockUserSignals(void);
extern void unblockUserSignals(void);

ProcHandle
runInteractiveProcess (char *const args[], 
		       char *workingDirectory, char **environment,
                       int fdStdIn, int fdStdOut, int fdStdErr,
		       int *pfdStdInput, int *pfdStdOutput, int *pfdStdError,
                       int set_inthandler, long inthandler, 
                       int set_quithandler, long quithandler,
                       int close_fds)
{
    int pid;
    int fdStdInput[2], fdStdOutput[2], fdStdError[2];
    int r;
    struct sigaction dfl;

    // Ordering matters here, see below [Note #431].
    if (fdStdIn == -1) {
        r = pipe(fdStdInput);
        if (r == -1) { 
            sysErrorBelch("runInteractiveProcess: pipe");
            return -1;
        }
        
    }
    if (fdStdOut == -1) {
        r = pipe(fdStdOutput);
        if (r == -1) { 
            sysErrorBelch("runInteractiveProcess: pipe");
            return -1;
        }
    }
    if (fdStdErr == -1) {
        r = pipe(fdStdError);
        if (r == -1) { 
            sysErrorBelch("runInteractiveProcess: pipe");
            return -1;
        }
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

    switch(pid = fork())
    {
    case -1:
        unblockUserSignals();
#if __GLASGOW_HASKELL__ > 612
        startTimer();
#endif
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
	
    case 0:
    {
        // WARNING!  we are now in the child of vfork(), so any memory
        // we modify below will also be seen in the parent process.

        unblockUserSignals();

	if (workingDirectory) {
	    if (chdir (workingDirectory) < 0) {
                // See #1593.  The convention for the exit code when
                // exec() fails seems to be 127 (gleened from C's
                // system()), but there's no equivalent convention for
                // chdir(), so I'm picking 126 --SimonM.
                _exit(126);
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
        } else {
            dup2(fdStdIn,  STDIN_FILENO);
        }

        if (fdStdOut == -1) {
            if (fdStdOutput[1] != STDOUT_FILENO) {
                dup2 (fdStdOutput[1], STDOUT_FILENO);
                close(fdStdOutput[1]);
            }
            close(fdStdOutput[0]);
        } else {
            dup2(fdStdOut,  STDOUT_FILENO);
        }

        if (fdStdErr == -1) {
            if (fdStdError[1] != STDERR_FILENO) {
                dup2 (fdStdError[1], STDERR_FILENO);
                close(fdStdError[1]);
            }
            close(fdStdError[0]);
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
            for (i = 3; i < max_fd; i++) {
                close(i);
            }
        }

	/* Set the SIGINT/SIGQUIT signal handlers in the child, if requested 
	 */
        (void)sigemptyset(&dfl.sa_mask);
        dfl.sa_flags = 0;
	if (set_inthandler) {
	    dfl.sa_handler = (void *)inthandler;
	    (void)sigaction(SIGINT, &dfl, NULL);
	}
	if (set_quithandler) {
	    dfl.sa_handler = (void *)quithandler;
	    (void)sigaction(SIGQUIT,  &dfl, NULL);
	}

	/* the child */
	if (environment) {
	    execvpe(args[0], args, environment);
	} else {
	    execvp(args[0], args);
	}
    }
    _exit(127);
    
    default:
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
	break;
    }
    unblockUserSignals();
    startTimer();
    
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
		errno = EINTR;
		return -1;
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

int waitForProcess (ProcHandle handle)
{
    int wstat;
    
    while (waitpid(handle, &wstat, 0) < 0)
    {
	if (errno != EINTR)
	{
	    return -1;
	}
    }
    
    if (WIFEXITED(wstat))
	return WEXITSTATUS(wstat);
    else
	if (WIFSIGNALED(wstat))
	{
	    return wstat;
	}
	else
	{
	    /* This should never happen */
	}
    
    return -1;
}

#else
/* ----------------------------------------------------------------------------
   Win32 versions
   ------------------------------------------------------------------------- */

/* -------------------- WINDOWS VERSION --------------------- */

/*
 * Function: mkAnonPipe
 *
 * Purpose:  create an anonymous pipe with read and write ends being
 *           optionally (non-)inheritable.
 */
static BOOL
mkAnonPipe (HANDLE* pHandleIn, BOOL isInheritableIn, 
	    HANDLE* pHandleOut, BOOL isInheritableOut)
{
	HANDLE hTemporaryIn  = NULL;
	HANDLE hTemporaryOut = NULL;

	/* Create the anon pipe with both ends inheritable */
	if (!CreatePipe(&hTemporaryIn, &hTemporaryOut, NULL, 0))
	{
		maperrno();
		*pHandleIn  = NULL;
		*pHandleOut = NULL;
		return FALSE;
	}

	if (isInheritableIn) {
            // SetHandleInformation requires at least Win2k
            if (!SetHandleInformation(hTemporaryIn,
                                      HANDLE_FLAG_INHERIT, 
                                      HANDLE_FLAG_INHERIT))
            {
                maperrno();
                *pHandleIn  = NULL;
                *pHandleOut = NULL;
                CloseHandle(hTemporaryIn);
                CloseHandle(hTemporaryOut);
                return FALSE;
            }
	}
        *pHandleIn = hTemporaryIn;

	if (isInheritableOut) {
            if (!SetHandleInformation(hTemporaryOut,
                                      HANDLE_FLAG_INHERIT, 
                                      HANDLE_FLAG_INHERIT))
            {
                maperrno();
                *pHandleIn  = NULL;
                *pHandleOut = NULL;
                CloseHandle(hTemporaryIn);
                CloseHandle(hTemporaryOut);
                return FALSE;
            }
        }
        *pHandleOut = hTemporaryOut;
        
	return TRUE;
}

ProcHandle
runInteractiveProcess (wchar_t *cmd, wchar_t *workingDirectory, 
                       void *environment,
                       int fdStdIn, int fdStdOut, int fdStdErr,
		       int *pfdStdInput, int *pfdStdOutput, int *pfdStdError,
                       int close_fds)
{
	STARTUPINFO sInfo;
	PROCESS_INFORMATION pInfo;
	HANDLE hStdInputRead   = INVALID_HANDLE_VALUE;
        HANDLE hStdInputWrite  = INVALID_HANDLE_VALUE;
	HANDLE hStdOutputRead  = INVALID_HANDLE_VALUE;
        HANDLE hStdOutputWrite = INVALID_HANDLE_VALUE;
	HANDLE hStdErrorRead   = INVALID_HANDLE_VALUE;
        HANDLE hStdErrorWrite  = INVALID_HANDLE_VALUE;
	DWORD flags;
	BOOL status;
        BOOL inherit;

	ZeroMemory(&sInfo, sizeof(sInfo));
	sInfo.cb = sizeof(sInfo);
	sInfo.dwFlags = STARTF_USESTDHANDLES;

	if (fdStdIn == -1) {
            if (!mkAnonPipe(&hStdInputRead,  TRUE, &hStdInputWrite,  FALSE))
                goto cleanup_err;
            sInfo.hStdInput = hStdInputRead;
        } else if (fdStdIn == 0) {
            // Don't duplicate stdin, as console handles cannot be
            // duplicated and inherited. urg.
            sInfo.hStdInput = GetStdHandle(STD_INPUT_HANDLE);
        } else {
            // The handle might not be inheritable, so duplicate it
            status = DuplicateHandle(GetCurrentProcess(), 
                                     (HANDLE) _get_osfhandle(fdStdIn),
                                     GetCurrentProcess(), &hStdInputRead,
                                     0,
                                     TRUE, /* inheritable */
                                     DUPLICATE_SAME_ACCESS);
            if (!status) goto cleanup_err;
            sInfo.hStdInput = hStdInputRead;
        }

	if (fdStdOut == -1) {
            if (!mkAnonPipe(&hStdOutputRead,  FALSE, &hStdOutputWrite,  TRUE))
                goto cleanup_err;
            sInfo.hStdOutput = hStdOutputWrite;
        } else if (fdStdOut == 1) {
            // Don't duplicate stdout, as console handles cannot be
            // duplicated and inherited. urg.
            sInfo.hStdOutput = GetStdHandle(STD_OUTPUT_HANDLE);
        } else {
            // The handle might not be inheritable, so duplicate it
            status = DuplicateHandle(GetCurrentProcess(), 
                                     (HANDLE) _get_osfhandle(fdStdOut),
                                     GetCurrentProcess(), &hStdOutputWrite,
                                     0,
                                     TRUE, /* inheritable */
                                     DUPLICATE_SAME_ACCESS);
            if (!status) goto cleanup_err;
            sInfo.hStdOutput = hStdOutputWrite;
        }

	if (fdStdErr == -1) {
            if (!mkAnonPipe(&hStdErrorRead,  TRUE, &hStdErrorWrite,  TRUE))
                goto cleanup_err;
            sInfo.hStdError = hStdErrorWrite;
        } else if (fdStdErr == 2) {
            // Don't duplicate stderr, as console handles cannot be
            // duplicated and inherited. urg.
            sInfo.hStdError = GetStdHandle(STD_ERROR_HANDLE);
        } else {
            /* The handle might not be inheritable, so duplicate it */
            status = DuplicateHandle(GetCurrentProcess(), 
                                     (HANDLE) _get_osfhandle(fdStdErr),
                                     GetCurrentProcess(), &hStdErrorWrite,
                                     0,
                                     TRUE, /* inheritable */
                                     DUPLICATE_SAME_ACCESS);
            if (!status) goto cleanup_err;
            sInfo.hStdError = hStdErrorWrite;
        }

	if (sInfo.hStdInput  != GetStdHandle(STD_INPUT_HANDLE)  &&
	    sInfo.hStdOutput != GetStdHandle(STD_OUTPUT_HANDLE) &&
	    sInfo.hStdError  != GetStdHandle(STD_ERROR_HANDLE))
		flags = CREATE_NO_WINDOW;   // Run without console window only when both output and error are redirected
	else
		flags = 0;

        // See #3231
        if (close_fds && fdStdIn == 0 && fdStdOut == 1 && fdStdErr == 2) {
            inherit = FALSE;
        } else {
            inherit = TRUE;
        }

	if (!CreateProcess(NULL, cmd, NULL, NULL, inherit, flags, environment, workingDirectory, &sInfo, &pInfo))
	{
                goto cleanup_err;
	}
	CloseHandle(pInfo.hThread);

	// Close the ends of the pipes that were inherited by the
	// child process.  This is important, otherwise we won't see
	// EOF on these pipes when the child process exits.
        if (hStdInputRead   != INVALID_HANDLE_VALUE) CloseHandle(hStdInputRead);
        if (hStdOutputWrite != INVALID_HANDLE_VALUE) CloseHandle(hStdOutputWrite);
        if (hStdErrorWrite  != INVALID_HANDLE_VALUE) CloseHandle(hStdErrorWrite);

	*pfdStdInput  = _open_osfhandle((intptr_t) hStdInputWrite, _O_WRONLY);
	*pfdStdOutput = _open_osfhandle((intptr_t) hStdOutputRead, _O_RDONLY);
  	*pfdStdError  = _open_osfhandle((intptr_t) hStdErrorRead,  _O_RDONLY);

  	return (int) pInfo.hProcess;

cleanup_err:
        if (hStdInputRead   != INVALID_HANDLE_VALUE) CloseHandle(hStdInputRead);
        if (hStdInputWrite  != INVALID_HANDLE_VALUE) CloseHandle(hStdInputWrite);
        if (hStdOutputRead  != INVALID_HANDLE_VALUE) CloseHandle(hStdOutputRead);
        if (hStdOutputWrite != INVALID_HANDLE_VALUE) CloseHandle(hStdOutputWrite);
        if (hStdErrorRead   != INVALID_HANDLE_VALUE) CloseHandle(hStdErrorRead);
        if (hStdErrorWrite  != INVALID_HANDLE_VALUE) CloseHandle(hStdErrorWrite);
        maperrno();
        return -1;
}

int
terminateProcess (ProcHandle handle)
{
    if (!TerminateProcess((HANDLE) handle, 1)) {
	maperrno();
	return -1;
    }
    return 0;
}

int
getProcessExitCode (ProcHandle handle, int *pExitCode)
{
    *pExitCode = 0;

    if (WaitForSingleObject((HANDLE) handle, 1) == WAIT_OBJECT_0)
    {
	if (GetExitCodeProcess((HANDLE) handle, (DWORD *) pExitCode) == 0)
	{
	    maperrno();
	    return -1;
	}
	return 1;
    }
    
    return 0;
}

int
waitForProcess (ProcHandle handle)
{
    DWORD retCode;

    if (WaitForSingleObject((HANDLE) handle, INFINITE) == WAIT_OBJECT_0)
    {
	if (GetExitCodeProcess((HANDLE) handle, &retCode) == 0)
	{
	    maperrno();
	    return -1;
	}
	return retCode;
    }
    
    maperrno();
    return -1;
}

#endif /* Win32 */
