/* ----------------------------------------------------------------------------
 * search path search logic
 * (c) Ben Gamari 2021
 */

#include <string.h>
#include <unistd.h>
#include <stdlib.h>
#include <stdio.h>
#include <stdbool.h>

#include "common.h"

// the below is only necessary when we need to emulate execvpe.
#if !defined(HAVE_execvpe)

/* Return true if the given file exists and is an executable. */
static bool is_executable(const char *path) {
    return access(path, X_OK) == 0;
}

/* Find an executable with the given filename in the given search path. The
 * result must be freed by the caller. Returns NULL if a matching file is not
 * found.
  */
static char *find_in_search_path(char *search_path, const char *filename) {
    const int filename_len = strlen(filename);
    char *tokbuf;
    char *path = strtok_r(search_path, ":", &tokbuf);
    while (path != NULL) {
	// N.B. gcc 6.3.0, used by Debian 9, inexplicably warns that `path`
	// may not be initialised with -Wall.  Silence this warning. See #210.
#if defined(__GNUC__) && __GNUC__ == 6 && __GNUC_MINOR__ == 3
#pragma GCC diagnostic push
#pragma GCC diagnostic ignored "-Wmaybe-uninitialized"
#endif
        const int tmp_len = filename_len + 1 + strlen(path) + 1;
#if defined(__GNUC__) && __GNUC__ == 6 && __GNUC_MINOR__ == 3
#pragma GCC diagnostic pop
#endif
        char *tmp = malloc(tmp_len);
        snprintf(tmp, tmp_len, "%s/%s", path, filename);
        if (is_executable(tmp)) {
            return tmp;
        } else {
            free(tmp);
        }

        path = strtok_r(NULL, ":", &tokbuf);
    }
    return NULL;
}

/* Identify the executable search path. The result must be freed by the caller. */
static char *get_executable_search_path(void) {
    char *search_path;

    search_path = getenv("PATH");
    if (search_path) {
        search_path = strdup(search_path);
        return search_path;
    }

#if defined(HAVE_CONFSTR)
    int len = confstr(_CS_PATH, NULL, 0);
    search_path = malloc(len + 1)
    if (search_path != NULL) {
        search_path[0] = ':';
        (void) confstr (_CS_PATH, search_path + 1, len);
        return search_path;
    }
#endif

    return strdup(":");
}

/* Find the given executable in the executable search path. */
char *find_executable(char *filename) {
    /* If it's an absolute or relative path name, it's easy. */
    if (strchr(filename, '/') && is_executable(filename)) {
        return filename;
    }

    char *search_path = get_executable_search_path();
    return find_in_search_path(search_path, filename);
}

#endif