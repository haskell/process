/* ----------------------------------------------------------------------------
   (c) The University of Glasgow 2004

   Interface for code in pipes.c (providing support for System.Process)
   ------------------------------------------------------------------------- */

#if defined(_MSC_VER) || defined(__MINGW32__) || defined(_WIN32)
#define UNICODE
#include <windows.h>
#include <stdlib.h>
#include <io.h>
#endif

#if defined(_MSC_VER) || defined(__MINGW32__) || defined(_WIN32)

extern BOOL createInheritablePipe(int *phandles, unsigned int psize, int textmode);

#endif
