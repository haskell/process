/* ----------------------------------------------------------------------------
   (c) The University of Glasgow 2004

   Support for System.Process
   ------------------------------------------------------------------------- */
   
#include "pipes.h"

BOOL createInheritablePipe(int *phandles, unsigned int psize, int textmode) {
    HANDLE hTemporaryIn  = NULL;
    HANDLE hTemporaryOut = NULL;
    
    BOOL isInheritableOut = TRUE;
    BOOL isInheritableIn = TRUE;

    /* Create the anon pipe with both ends inheritable */
    if (!CreatePipe(&hTemporaryIn, &hTemporaryOut, NULL, 0))
    {
        maperrno();
        phandles[0]  = NULL;
        phandles[1]  = NULL;
        return FALSE;
    }

    if (isInheritableIn) {
        // SetHandleInformation requires at least Win2k
        if (!SetHandleInformation(hTemporaryIn,
                                  HANDLE_FLAG_INHERIT,
                                  HANDLE_FLAG_INHERIT))
        {
            maperrno();
            phandles[0]   = NULL;
            phandles[1]  = NULL;
            CloseHandle(hTemporaryIn);
            CloseHandle(hTemporaryOut);
            return FALSE;
        }
    }
    phandles[0]  = _open_osfhandle((int)hTemporaryIn, textmode);

    if (isInheritableOut) {
        if (!SetHandleInformation(hTemporaryOut,
                                  HANDLE_FLAG_INHERIT,
                                  HANDLE_FLAG_INHERIT))
        {
            maperrno();
            phandles[0]   = NULL;
            phandles[1]  = NULL;
            CloseHandle(hTemporaryIn);
            CloseHandle(hTemporaryOut);
            return FALSE;
        }
    }
    phandles[1]  = _open_osfhandle((int)hTemporaryOut, textmode);

    return TRUE;
}