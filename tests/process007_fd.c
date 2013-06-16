
#include <errno.h>
#include <stdio.h>
#include <stdlib.h>
#include <unistd.h>

#define SIZE 1024

int main(int argc, char **argv) {
    int fd;
    char buf[SIZE];
    int nRead, nWrite;

    if (argc != 2) {
        printf("Bad arguments\n");
        exit(1);
    }

    fd = atoi(argv[1]);

    while (nRead = read(fd, buf, SIZE) != 0) {
        if (nRead > 0) {
            nWrite = printf("%s", buf);
            if (nWrite < 0) {
                perror("printf failed");
                exit(1);
            }
        }
        else if (errno != EAGAIN && errno != EWOULDBLOCK && errno != EINTR) {
            perror("read failed");
            exit(1);
        }
    }

    return 0;
}

