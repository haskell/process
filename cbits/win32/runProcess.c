/* ----------------------------------------------------------------------------
   (c) The University of Glasgow 2004-2022

   Support for System.Process
   ------------------------------------------------------------------------- */

#define UNICODE

/* XXX This is a nasty hack; should put everything necessary in this package */
#include "HsBase.h"
#include "Rts.h"

#include "runProcess.h"

#include <assert.h>
#include <windows.h>
#include <io.h>
#include <objbase.h>
#include <wchar.h>

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

/*
 * Function: mkNamedPipe
 *
 * Purpose:  create an named pipe with read and write ends being
 *           optionally (non-)inheritable. Named pipes can be read
 *           asynchronously while anonymous pipes require blocking calls.
 */
BOOL
mkNamedPipe (HANDLE* pHandleIn, BOOL isInheritableIn,
             HANDLE* pHandleOut, BOOL isInheritableOut)
{
    HANDLE hTemporaryIn  = INVALID_HANDLE_VALUE;
    HANDLE hTemporaryOut = INVALID_HANDLE_VALUE;
    RPC_WSTR guidStr = NULL;
    GUID guid;

    /* First we create a new GUID to make the name of the pipe unique. Since
       GUID are guaranteed to be unique system wide we don't need to retry.  */
    ZeroMemory (&guid, sizeof (guid));
    if (CoCreateGuid (&guid) != S_OK)
        goto fail;

    if (UuidToStringW ((UUID*)&guid, &guidStr) != S_OK)
        goto fail;

    /* Now we create the pipe name.  */
    wchar_t pipeName[MAX_PATH];
    if (-1 == swprintf_s (&pipeName[0],  MAX_PATH, L"\\\\.\\pipe\\haskell:process:%ls\n",  guidStr))
        goto fail;

    const int buffer_size = 8 * 1024;

    RpcStringFreeW (&guidStr);

    SECURITY_ATTRIBUTES secAttr;
    ZeroMemory (&secAttr, sizeof(secAttr));
    secAttr.nLength = sizeof(SECURITY_ATTRIBUTES);
    secAttr.lpSecurityDescriptor = NULL;
    secAttr.bInheritHandle = isInheritableIn;

    /* Create one end of the pipe. Named pipes are a bit less secure than
       anonymous pipes.  Because of this we restrict the pipe's access to only
       one client and also only the local host.  This means after we create the
       other end of the pipe it should be as secure as an anonymous pipe.  */
    hTemporaryIn
      = CreateNamedPipeW (&pipeName[0],
                          PIPE_ACCESS_INBOUND | FILE_FLAG_OVERLAPPED | FILE_FLAG_FIRST_PIPE_INSTANCE,
                          PIPE_TYPE_MESSAGE | PIPE_REJECT_REMOTE_CLIENTS,
                          1, buffer_size, buffer_size,
                          0,
                          &secAttr);
    if (hTemporaryIn == INVALID_HANDLE_VALUE)
      goto fail;

    /* And now create the other end using the inverse access permissions.  This
       will give us the read and write ends of the pipe.  */
    secAttr.bInheritHandle = isInheritableOut;
    hTemporaryOut
      = CreateFileW (&pipeName[0],
                     GENERIC_WRITE,
                     FILE_SHARE_WRITE,
                     &secAttr,
                     OPEN_EXISTING,
                     FILE_FLAG_OVERLAPPED,
                     NULL);
    if (hTemporaryOut == INVALID_HANDLE_VALUE)
      goto fail;

    /* Set some optimization flags to make the I/O manager operate more
       efficiently on these handles.  These mirrors those in
       `optimizeFileAccess` but we set them here to do so before any data has
       been put in the HANDLEs.  However these don't always work for sockets and
       pipes.  So we set them, but can't rely on it.  */
#if defined(FILE_SKIP_SET_EVENT_ON_HANDLE) && \
    defined(FILE_SKIP_COMPLETION_PORT_ON_SUCCESS)
    UCHAR flags = FILE_SKIP_COMPLETION_PORT_ON_SUCCESS
                  | FILE_SKIP_SET_EVENT_ON_HANDLE;
    SetFileCompletionNotificationModes (hTemporaryIn, flags);
    SetFileCompletionNotificationModes (hTemporaryOut, flags);
#endif

    /* Everything has succeeded so now copy the pointers to the results.  */
    *pHandleIn = hTemporaryIn;
    *pHandleOut = hTemporaryOut;

    return TRUE;

fail:
    /* We have to save the current error before we do another API call.  */
    maperrno();
    RpcStringFreeW (&guidStr);
    if (INVALID_HANDLE_VALUE != hTemporaryIn ) CloseHandle (hTemporaryIn);
    if (INVALID_HANDLE_VALUE != hTemporaryOut) CloseHandle (hTemporaryOut);
    return FALSE;
}

static HANDLE
createJob ()
{
    HANDLE hJob = CreateJobObject (NULL, NULL);
    JOBOBJECT_EXTENDED_LIMIT_INFORMATION jeli;
    ZeroMemory(&jeli, sizeof(JOBOBJECT_EXTENDED_LIMIT_INFORMATION));
    // Configure all child processes associated with the job to terminate when the
    // Last process in the job terminates. This prevent half dead processes.
    jeli.BasicLimitInformation.LimitFlags = JOB_OBJECT_LIMIT_KILL_ON_JOB_CLOSE;

    if (SetInformationJobObject (hJob, JobObjectExtendedLimitInformation,
                                 &jeli, sizeof(JOBOBJECT_EXTENDED_LIMIT_INFORMATION)))
    {
        return hJob;
    }

    maperrno();
    return NULL;
}

/* Small helper function that determines how the std handle should be used.
   if _STDHANDLE is:
     -1: A new pipe is created.  If ASYNCHRONOUS an asynchronous pipe is created
         with FILE_FLAG_OVERLAPPED set.  If not then an anonymouse pipe is
         created without that flag.
     -2: No handle is created, DESTINATION is NULL.
     std: If the handle matches the default std handle for the type (i.e. if the
          handle for input _stdHandle mathed STDIN) then set DESTINATION to that
          handle.
     otherwise: We just duplicate the handle to make it inheritable and pass it
                on.  */

static inline bool
setStdHandleInfo (LPHANDLE destination, HANDLE _stdhandle,
                  LPHANDLE hStdRead, LPHANDLE hStdWrite, HANDLE defaultStd,
                  BOOL isInhertibleIn, BOOL isInhertibleOut, BOOL asynchronous)
{
    BOOL status;
    assert (destination);
    assert (hStdRead);
    assert (hStdWrite);

    LPHANDLE tmpHandle = isInhertibleOut ? hStdWrite : hStdRead;

    if (_stdhandle == (HANDLE)-1) {
        if (!asynchronous
            && !mkAnonPipe(hStdRead, isInhertibleIn, hStdWrite, isInhertibleOut))
            return false;
        if (asynchronous
            && !mkNamedPipe(hStdRead, isInhertibleIn, hStdWrite, isInhertibleOut))
            return false;
        *destination = *tmpHandle;
    } else if (_stdhandle == (HANDLE)-2) {
        *destination = NULL;
    } else if (_stdhandle == defaultStd) {
        // Don't duplicate standard handle, as console handles cannot be
        // duplicated and inherited. urg.
        *destination = defaultStd;
    } else {
        // The handle might not be inheritable, so duplicate it
        status = DuplicateHandle(GetCurrentProcess(),
                                 _stdhandle,
                                 GetCurrentProcess(), tmpHandle,
                                 0,
                                 TRUE, /* inheritable */
                                 DUPLICATE_SAME_ACCESS);
        if (!status) return false;
        *destination = *tmpHandle;
    }

    return true;
}

/* Common functionality between the Posix FD version and native HANDLE version
   of runInteractiveProcess.  The main difference lies in the use of
   ASYNCHRONOUS which indicates whether the pipes that are created allow for
   asynchronous access or not.  */

static ProcHandle
runInteractiveProcessWrapper (
    wchar_t *cmd, wchar_t *workingDirectory,
    wchar_t *environment,
    HANDLE _stdin, HANDLE _stdout, HANDLE _stderr,
    HANDLE *pStdInput, HANDLE *pStdOutput, HANDLE *pStdError,
    int flags, bool useJobObject, HANDLE *hJob, bool asynchronous)
{
    STARTUPINFO sInfo;
    PROCESS_INFORMATION pInfo;
    HANDLE hStdInputRead   = INVALID_HANDLE_VALUE;
    HANDLE hStdInputWrite  = INVALID_HANDLE_VALUE;
    HANDLE hStdOutputRead  = INVALID_HANDLE_VALUE;
    HANDLE hStdOutputWrite = INVALID_HANDLE_VALUE;
    HANDLE hStdErrorRead   = INVALID_HANDLE_VALUE;
    HANDLE hStdErrorWrite  = INVALID_HANDLE_VALUE;
    BOOL close_fds = ((flags & RUN_PROCESS_IN_CLOSE_FDS) != 0);
    // We always pass a wide environment block, so we MUST set this flag
    DWORD dwFlags = CREATE_UNICODE_ENVIRONMENT;
    BOOL inherit;

    ZeroMemory(&sInfo, sizeof(sInfo));
    sInfo.cb = sizeof(sInfo);
    sInfo.dwFlags = STARTF_USESTDHANDLES;
    ZeroMemory(&pInfo, sizeof(pInfo));

    HANDLE defaultStdIn     = GetStdHandle(STD_INPUT_HANDLE);
    HANDLE defaultStdOutput = GetStdHandle(STD_OUTPUT_HANDLE);
    HANDLE defaultStdError  = GetStdHandle(STD_ERROR_HANDLE);

    if (!setStdHandleInfo (&sInfo.hStdInput, _stdin, &hStdInputRead,
                           &hStdInputWrite, defaultStdIn, TRUE, FALSE,
                           asynchronous))
      goto cleanup_err;

    if (!setStdHandleInfo (&sInfo.hStdOutput, _stdout, &hStdOutputRead,
                           &hStdOutputWrite, defaultStdOutput, FALSE, TRUE,
                           asynchronous))
      goto cleanup_err;

    if (!setStdHandleInfo (&sInfo.hStdError, _stderr, &hStdErrorRead,
                           &hStdErrorWrite, defaultStdError, FALSE, TRUE,
                           asynchronous))
      goto cleanup_err;

    if (sInfo.hStdInput     !=  defaultStdIn
        && sInfo.hStdOutput !=  defaultStdOutput
        && sInfo.hStdError  !=  defaultStdError
        && (flags & RUN_PROCESS_IN_NEW_GROUP) == 0)
            dwFlags |= CREATE_NO_WINDOW;   // Run without console window only when both output and error are redirected

    // See #3231
    if (close_fds
        && _stdin == defaultStdIn
        && _stdout == defaultStdOutput
        && _stderr == defaultStdError) {
        inherit = FALSE;
    } else {
        inherit = TRUE;
    }

    if ((flags & RUN_PROCESS_IN_NEW_GROUP) != 0) {
        dwFlags |= CREATE_NEW_PROCESS_GROUP;
    }
    if ((flags & RUN_PROCESS_DETACHED) != 0) {
        dwFlags |= DETACHED_PROCESS;
    }
    if ((flags & RUN_PROCESS_NEW_CONSOLE) != 0) {
        dwFlags |= CREATE_NEW_CONSOLE;
    }

    /* If we're going to use a job object, then we have to create
       the thread suspended.
       See Note [Windows exec interaction].  */
    if (useJobObject)
    {
        dwFlags |= CREATE_SUSPENDED;
        *hJob = createJob();
        if (!*hJob)
        {
            goto cleanup_err;
        }
    } else {
        *hJob = NULL;
    }

    if (!CreateProcess(NULL, cmd, NULL, NULL, inherit, dwFlags, environment, workingDirectory, &sInfo, &pInfo))
    {
        goto cleanup_err;
    }

    if (useJobObject && hJob && *hJob)
    {
        // Then associate the process and the job;
        if (!AssignProcessToJobObject (*hJob, pInfo.hProcess))
        {
            goto cleanup_err;
        }

        // And now that we've associated the new process with the job
        // we can actively resume it.
        ResumeThread (pInfo.hThread);
    }

    CloseHandle(pInfo.hThread);

    // Close the ends of the pipes that were inherited by the
    // child process.  This is important, otherwise we won't see
    // EOF on these pipes when the child process exits.
    if (hStdInputRead   != INVALID_HANDLE_VALUE) CloseHandle(hStdInputRead);
    if (hStdOutputWrite != INVALID_HANDLE_VALUE) CloseHandle(hStdOutputWrite);
    if (hStdErrorWrite  != INVALID_HANDLE_VALUE) CloseHandle(hStdErrorWrite);

    // Return the pointers to the handles we need.
    *pStdInput  = hStdInputWrite;
    *pStdOutput = hStdOutputRead;
    *pStdError  = hStdErrorRead;

    return pInfo.hProcess;

cleanup_err:
    if (hStdInputRead   != INVALID_HANDLE_VALUE) CloseHandle(hStdInputRead);
    if (hStdInputWrite  != INVALID_HANDLE_VALUE) CloseHandle(hStdInputWrite);
    if (hStdOutputRead  != INVALID_HANDLE_VALUE) CloseHandle(hStdOutputRead);
    if (hStdOutputWrite != INVALID_HANDLE_VALUE) CloseHandle(hStdOutputWrite);
    if (hStdErrorRead   != INVALID_HANDLE_VALUE) CloseHandle(hStdErrorRead);
    if (hStdErrorWrite  != INVALID_HANDLE_VALUE) CloseHandle(hStdErrorWrite);
    if (useJobObject && hJob      && *hJob     ) CloseHandle(*hJob);

    maperrno();
    return NULL;
}

/* Note [Windows exec interaction]

   The basic issue that process jobs tried to solve is this:

   Say you have two programs A and B. Now A calls B. There are two ways to do this.

   1) You can use the normal CreateProcess API, which is what normal Windows code do.
      Using this approach, the current waitForProcess works absolutely fine.
   2) You can call the emulated POSIX function _exec, which of course is supposed to
      allow the child process to replace the parent.

    With approach 2) waitForProcess falls apart because the Win32's process model does
    not allow this the same way as linux. _exec is emulated by first making a call to
    CreateProcess to spawn B and then immediately exiting from A. So you have two
    different processes.

    waitForProcess is waiting on the termination of A. Because A is immediately killed,
    waitForProcess will return even though B is still running. This is why for instance
    the GHC testsuite on Windows had lots of file locked errors.

    This approach creates a new Job and assigned A to the job, but also all future
    processes spawned by A. This allows us to listen in on events, such as, when all
    processes in the job are finished, but also allows us to propagate exit codes from
    _exec calls.

    The only reason we need this at all is because we don't interact with just actual
    native code on Windows, and instead have a lot of ported POSIX code.

    The Job handle is returned to the user because Jobs have additional benefits as well,
    such as allowing you to specify resource limits on the to be spawned process.
 */
ProcHandle
runInteractiveProcess (wchar_t *cmd, wchar_t *workingDirectory,
                       wchar_t *environment,
                       int fdStdIn, int fdStdOut, int fdStdErr,
                       int *pfdStdInput, int *pfdStdOutput, int *pfdStdError,
                       int flags, bool useJobObject, HANDLE *hJob)
{
    HANDLE pStdInput = INVALID_HANDLE_VALUE;
    HANDLE pStdOutput = INVALID_HANDLE_VALUE;
    HANDLE pStdError = INVALID_HANDLE_VALUE;

    ProcHandle result
      = runInteractiveProcessWrapper (cmd, workingDirectory, environment,
                                      (HANDLE) (fdStdIn  < 0 ? fdStdIn : _get_osfhandle(fdStdIn)),
                                      (HANDLE) (fdStdOut < 0 ? fdStdOut : _get_osfhandle(fdStdOut)),
                                      (HANDLE) (fdStdErr < 0 ? fdStdErr : _get_osfhandle(fdStdErr)),
                                      &pStdInput, &pStdOutput, &pStdError,
                                      flags, useJobObject, hJob, FALSE);

    if (result) {
        *pfdStdInput  = _open_osfhandle((intptr_t) pStdInput,  _O_WRONLY);
        *pfdStdOutput = _open_osfhandle((intptr_t) pStdOutput, _O_RDONLY);
        *pfdStdError  = _open_osfhandle((intptr_t) pStdError,  _O_RDONLY);
    }

    return result;
}

/* This function is the same as runInteractiveProcess except it works directly
   on Windows HANDLE rather than pseudo FDs.  This allows us to use the pipes
   returned here asynchronously and also need less system calls while working
   with the new I/O manager.  */
ProcHandle
runInteractiveProcessHANDLE (
    wchar_t *cmd, wchar_t *workingDirectory,
    wchar_t *environment,
    HANDLE _stdin, HANDLE _stdout, HANDLE _stderr,
    HANDLE *pStdInput, HANDLE *pStdOutput, HANDLE *pStdError,
    int flags, bool useJobObject, HANDLE *hJob)
{
  return runInteractiveProcessWrapper (cmd, workingDirectory, environment,
                                       _stdin, _stdout, _stderr,
                                       pStdInput, pStdOutput, pStdError,
                                       flags, useJobObject, hJob, TRUE);
}

int
terminateProcess (ProcHandle handle)
{
    if (!TerminateProcess ((HANDLE) handle, 1)) {
        DWORD e = GetLastError();
        DWORD exitCode;
        /*
        This is a crude workaround that is taken from libuv. For some reason
        TerminateProcess() can fail with ERROR_ACCESS_DENIED if the process
        already terminated. This situation can be detected by using
        GetExitCodeProcess() to check if the exit code is availble. Unfortunately
        this function succeeds and gives exit code 259 (STILL_ACTIVE) if the
        process is still running. So there is no way to ditinguish a process
        that exited with 259 and a process that did not exit because we had
        insufficient access to terminate it.
        One would expect WaitForSingleObject() to be the solid solution. But this
        function does return WAIT_TIMEOUT in that situation. Even if called
        after GetExitCodeProcess().
        */
        if (e == ERROR_ACCESS_DENIED && GetExitCodeProcess((HANDLE) handle, &exitCode) && exitCode != STILL_ACTIVE)
            return 0;

        SetLastError(e);
        maperrno();
        return -1;
    }
    return 0;
}

int
terminateJob (ProcHandle handle)
{
    if (!TerminateJobObject ((HANDLE)handle, 1)) {
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
waitForProcess (ProcHandle handle, int *pret)
{
    DWORD retCode;

    if (WaitForSingleObject((HANDLE) handle, INFINITE) == WAIT_OBJECT_0)
    {
        if (GetExitCodeProcess((HANDLE) handle, &retCode) == 0)
        {
            maperrno();
            return -1;
        }
        *pret = retCode;
        return 0;
    }

    maperrno();
    return -1;
}

// Returns true on success.
int
waitForJobCompletion ( HANDLE hJob )
{
    int process_count = 16;
    JOBOBJECT_BASIC_PROCESS_ID_LIST *pid_list = NULL;

    while (true) {
      size_t pid_list_size = sizeof(JOBOBJECT_BASIC_PROCESS_ID_LIST) + sizeof(ULONG_PTR) * (process_count - 1);

      if (pid_list == NULL) {
        pid_list = malloc(pid_list_size);
        if (pid_list == NULL) {
          errno = ENOMEM;
          return false;
        }
        pid_list->NumberOfAssignedProcesses = process_count;
      }

      // Find a process in the job...
      bool success = QueryInformationJobObject(
          hJob,
          JobObjectBasicProcessIdList,
          pid_list,
          pid_list_size,
          NULL);

      if (!success && GetLastError() == ERROR_MORE_DATA) {
        process_count *= 2;
        free(pid_list);
        pid_list = NULL;
        continue;
      } else if (!success) {
        free(pid_list);
        maperrno();
        return false;
      }
      if (pid_list->NumberOfProcessIdsInList == 0) {
        // We're done
        free(pid_list);
        return true;
      }

      HANDLE pHwnd = OpenProcess(SYNCHRONIZE, TRUE, pid_list->ProcessIdList[0]);
      if (pHwnd == NULL) {
        switch (GetLastError()) {
          case ERROR_INVALID_PARAMETER:
          case ERROR_INVALID_HANDLE:
            // Presumably the process terminated; try again.
            continue;
          default:
            free(pid_list);
            maperrno();
            return false;
        }
      }

      // Wait for it to finish...
      if (WaitForSingleObject(pHwnd, INFINITE) != WAIT_OBJECT_0) {
        free(pid_list);
        maperrno();
        CloseHandle(pHwnd);
        return false;
      }

      // The process signalled, loop again to try the next process.
      CloseHandle(pHwnd);
    }
}
