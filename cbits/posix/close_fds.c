
#include "common.h"

#include <unistd.h>

#if defined(HAVE_CLOSERANGE)
#include <linux/close_range.h>
#endif


void closefrom_excluding(int lowfd, int excludingFd) {
#if defined(HAVE_CLOSERANGE)
    close_range(lowfd, excludingFd - 1);
    closefrom(excludingFd + 1);
#else
    for (int i = lowfd; i < excludingFd; i++) {
        close(i);
    }

    closefrom(excludingFd + 1);
#endif
}
