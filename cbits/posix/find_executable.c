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
#if !defined(HAVE_EXECVPE)

/* A quick check for whether the given path is absolute. */
static bool is_absolute(const char *path) {
    return path[0] == '/';
}

static char *concat_paths(const char *path1, const char *path2) {
    if (is_absolute(path2)) {
        return strdup(path2);
    } else {
        int len = strlen(path1) + 1 + strlen(path2) + 1;
        char *tmp = malloc(len);
        int ret = snprintf(tmp, len, "%s/%s", path1, path2);
        if (ret < 0) {
            free(tmp);
            return NULL;
        }
        return tmp;
    }
}

/* Return true if the given file exists and is an executable, optionally
 * relative to the given working directory.
 */
static bool is_executable(char *working_dir, const char *path) {
    if (working_dir && !is_absolute(path)) {
        char *tmp = concat_paths(working_dir, path);
        bool ret = access(tmp, X_OK) == 0;
        free(tmp);
        return ret;
    } else {
        return access(path, X_OK) == 0;
    }
}

/* Find an executable with the given filename in the given search path. The
 * result must be freed by the caller. Returns NULL if a matching file is not
 * found.
  */
static char *find_in_search_path(char *working_dir, char *search_path, const char *filename) {
    int workdir_len = strlen(working_dir);
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
        char *tmp;
        if (is_absolute(path)) {
            const int tmp_len = strlen(path) + 1 + filename_len + 1;
            tmp = malloc(tmp_len);
            snprintf(tmp, tmp_len, "%s/%s", path, filename);
        } else {
            const int tmp_len = workdir_len + 1 + strlen(path) + 1 + filename_len + 1;
            tmp = malloc(tmp_len);
            snprintf(tmp, tmp_len, "%s/%s/%s", working_dir, path, filename);
        }
#if defined(__GNUC__) && __GNUC__ == 6 && __GNUC_MINOR__ == 3
#pragma GCC diagnostic pop
#endif

        if (is_executable(working_dir, tmp)) {
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

/* Find the given executable in the executable search path relative to
 * workingDirectory (or the current directory, if NULL).
 * N.B. the caller is responsible for free()ing the result.
 */
char *find_executable(char *working_dir, char *filename) {
    /* Drop trailing slash from working directory if necessary */
    if (working_dir) {
        int workdir_len = strlen(working_dir);
        if (working_dir[workdir_len-1] == '/') {
            working_dir[workdir_len-1] = '\0';
        }
    }

    if (is_absolute(filename)) {
        /* If it's an absolute path name, it's easy. */
        return filename;

    } else if (strchr(filename, '/')) {
        /* If it's a relative path name, we must look for executables relative
         * to the working directory. */
        if (is_executable(working_dir, filename)) {
            return filename;
        }
    }

    /* Otherwise look through the search path... */
    char *search_path = get_executable_search_path();
    char *result = find_in_search_path(working_dir, search_path, filename);
    free(search_path);
    return result;
}

#endif
