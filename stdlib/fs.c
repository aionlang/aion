/*
 * C half of std.fs: files and directories over POSIX.
 *
 * Failures are reported as NULL / false / -1; the reason for the latest one is kept for
 * `fs.lastError()`.
 */

#include "aion.h"

#include <dirent.h>
#include <errno.h>
#include <fcntl.h>
#include <stdio.h>
#include <stdlib.h>
#include <string.h>
#include <sys/stat.h>
#include <unistd.h>
#ifdef _WIN32
#include <direct.h>
#include <windows.h>
#define mkdir(path, mode) _mkdir(path)
#define lstat stat
#endif

static int last_errno;

static void note(void) { last_errno = errno; }

const char *aion_fs_last_error(void) { return aion_string(last_errno ? strerror(last_errno) : ""); }

/* The whole file, or NULL. */
void *aion_fs_read_bytes(const char *path) {
    FILE *f = fopen(path, "rb");
    if (!f) return note(), NULL;
    size_t cap = 4096, len = 0;
    char *buf = malloc(cap);
    if (!buf) aion_panic("out of memory");
    for (;;) {
        size_t got = fread(buf + len, 1, cap - len, f);
        len += got;
        if (len < cap) break;
        cap *= 2;
        buf = realloc(buf, cap);
        if (!buf) aion_panic("out of memory");
    }
    bool bad = ferror(f);
    if (bad) note();
    fclose(f);
    void *out = NULL;
    if (!bad) {
        out = aion_bytes_new((int64_t)len);
        memcpy(aion_bytes_data(out), buf, len);
    }
    free(buf);
    return out;
}

bool aion_fs_write_bytes(const char *path, const void *data, bool append) {
    FILE *f = fopen(path, append ? "ab" : "wb");
    if (!f) return note(), false;
    int64_t n = aion_bytes_size(data);
    bool ok = fwrite(data, 1, (size_t)n, f) == (size_t)n;
    if (!ok) note();
    if (fclose(f) != 0 && ok) ok = (note(), false);
    return ok;
}

bool aion_fs_write_text(const char *path, const char *text, bool append) {
    FILE *f = fopen(path, append ? "ab" : "wb");
    if (!f) return note(), false;
    size_t n = strlen(text);
    bool ok = fwrite(text, 1, n, f) == n;
    if (!ok) note();
    if (fclose(f) != 0 && ok) ok = (note(), false);
    return ok;
}

bool aion_fs_exists(const char *path) {
    struct stat st;
    return stat(path, &st) == 0;
}

bool aion_fs_is_dir(const char *path) {
    struct stat st;
    return stat(path, &st) == 0 && S_ISDIR(st.st_mode);
}

bool aion_fs_is_file(const char *path) {
    struct stat st;
    return stat(path, &st) == 0 && S_ISREG(st.st_mode);
}

/* Size in bytes, or -1. */
int64_t aion_fs_size(const char *path) {
    struct stat st;
    if (stat(path, &st) != 0) return note(), -1;
    return (int64_t)st.st_size;
}

/* Last modification in milliseconds since 1970, or -1. */
int64_t aion_fs_modified(const char *path) {
    struct stat st;
    if (stat(path, &st) != 0) return note(), -1;
#ifdef _WIN32
    return (int64_t)st.st_mtime * 1000;
#else
    return (int64_t)st.st_mtime * 1000 + (int64_t)(st.st_mtim.tv_nsec / 1000000);
#endif
}

static int by_name(const void *a, const void *b) { return strcmp(*(char *const *)a, *(char *const *)b); }

/* The names in a directory, sorted and separated by line breaks; NULL if it can't be read. */
const char *aion_fs_list(const char *path) {
    DIR *d = opendir(path);
    if (!d) return note(), NULL;
    size_t count = 0, cap = 16, bytes = 0;
    char **names = malloc(cap * sizeof *names);
    if (!names) aion_panic("out of memory");
    struct dirent *e;
    while ((e = readdir(d))) {
        if (!strcmp(e->d_name, ".") || !strcmp(e->d_name, "..")) continue;
        if (count == cap) {
            cap *= 2;
            names = realloc(names, cap * sizeof *names);
            if (!names) aion_panic("out of memory");
        }
        names[count++] = strdup(e->d_name);
        bytes += strlen(e->d_name) + 1;
    }
    closedir(d);
    qsort(names, count, sizeof *names, by_name);
    char *text = malloc(bytes + 1);
    if (!text) aion_panic("out of memory");
    char *at = text;
    for (size_t i = 0; i < count; i++) {
        size_t n = strlen(names[i]);
        memcpy(at, names[i], n);
        at += n;
        if (i + 1 < count) *at++ = '\n';
        free(names[i]);
    }
    free(names);
    const char *out = aion_string_n(text, (size_t)(at - text));
    free(text);
    return out;
}

bool aion_fs_mkdir(const char *path) {
    if (mkdir(path, 0777) != 0) return note(), false;
    return true;
}

/* Create the directory and any missing parents; fine if it is already there. */
bool aion_fs_mkdirs(const char *path) {
    char *copy = strdup(path);
    if (!copy) aion_panic("out of memory");
    /* a drive such as "C:" is not made, and both kinds of slash separate the folders on Windows */
    for (char *p = copy + 1; *p; p++) {
#ifdef _WIN32
        if (*p != '/' && *p != '\\') continue;
        if (p == copy + 2 && copy[1] == ':') continue;
#else
        if (*p != '/') continue;
#endif
        char slash = *p;
        *p = '\0';
        if (mkdir(copy, 0777) != 0 && errno != EEXIST) {
            note();
            free(copy);
            return false;
        }
        *p = slash;
    }
    bool ok = mkdir(copy, 0777) == 0 || (errno == EEXIST && aion_fs_is_dir(copy));
    if (!ok) note();
    free(copy);
    return ok;
}

/* A file or an empty directory. */
bool aion_fs_remove(const char *path) {
#ifdef _WIN32
    if (aion_fs_is_dir(path)) return rmdir(path) == 0 || (note(), false);
#endif
    if (remove(path) != 0) return note(), false;
    return true;
}

static bool remove_tree(const char *path) {
    struct stat st;
    if (lstat(path, &st) != 0) return note(), false;
    if (S_ISDIR(st.st_mode)) {
        DIR *d = opendir(path);
        if (!d) return note(), false;
        struct dirent *e;
        bool ok = true;
        while (ok && (e = readdir(d))) {
            if (!strcmp(e->d_name, ".") || !strcmp(e->d_name, "..")) continue;
            size_t n = strlen(path) + strlen(e->d_name) + 2;
            char *child = malloc(n);
            if (!child) aion_panic("out of memory");
            snprintf(child, n, "%s/%s", path, e->d_name);
            ok = remove_tree(child);
            free(child);
        }
        closedir(d);
        if (!ok) return false;
        if (rmdir(path) != 0) return note(), false;
        return true;
    }
    if (unlink(path) != 0) return note(), false;
    return true;
}

/* A file, or a directory with everything in it. */
bool aion_fs_remove_all(const char *path) { return remove_tree(path); }

bool aion_fs_rename(const char *from, const char *to) {
#ifdef _WIN32
    /* rename() refuses to replace a file on Windows, which the POSIX one does */
    if (!MoveFileExA(from, to, MOVEFILE_REPLACE_EXISTING | MOVEFILE_COPY_ALLOWED)) return errno = EACCES, note(), false;
    return true;
#else
    if (rename(from, to) != 0) return note(), false;
    return true;
#endif
}

bool aion_fs_copy(const char *from, const char *to) {
    FILE *in = fopen(from, "rb");
    if (!in) return note(), false;
    FILE *out = fopen(to, "wb");
    if (!out) {
        note();
        fclose(in);
        return false;
    }
    char buf[65536];
    size_t n;
    bool ok = true;
    while ((n = fread(buf, 1, sizeof buf, in)) > 0) {
        if (fwrite(buf, 1, n, out) != n) {
            ok = false;
            break;
        }
    }
    if (ferror(in)) ok = false;
    if (!ok) note();
    fclose(in);
    if (fclose(out) != 0 && ok) ok = (note(), false);
    return ok;
}

const char *aion_fs_cwd(void) {
    char buf[4096];
    if (!getcwd(buf, sizeof buf)) return note(), aion_string("");
#ifdef _WIN32
    for (char *p = buf; *p; p++)
        if (*p == '\\') *p = '/';
#endif
    return aion_string(buf);
}

bool aion_fs_chdir(const char *path) {
    if (chdir(path) != 0) return note(), false;
    return true;
}

/* The absolute path with symbolic links and `..` resolved; NULL if it does not exist. */
const char *aion_fs_absolute(const char *path) {
#ifdef _WIN32
    char buf[MAX_PATH * 4];
    if (!_fullpath(buf, path, sizeof buf) || !aion_fs_exists(path)) return errno = ENOENT, note(), NULL;
    for (char *p = buf; *p; p++)
        if (*p == '\\') *p = '/';
    return aion_string(buf);
#else
    char *resolved = realpath(path, NULL);
    if (!resolved) return note(), NULL;
    const char *out = aion_string(resolved);
    free(resolved);
    return out;
#endif
}

static const char *temp_base(void) {
#ifdef _WIN32
    static char buf[MAX_PATH * 2];
    DWORD n = GetTempPathA((DWORD)sizeof buf, buf);
    if (n == 0 || n >= sizeof buf) return "C:/Windows/Temp";
    while (n > 1 && (buf[n - 1] == '\\' || buf[n - 1] == '/')) buf[--n] = '\0';
    for (char *p = buf; *p; p++)
        if (*p == '\\') *p = '/';
    return buf;
#else
    const char *t = getenv("TMPDIR");
    return t && *t ? t : "/tmp";
#endif
}

const char *aion_fs_temp_dir(void) { return aion_string(temp_base()); }

/* A new, empty directory named like `<prefix>XXXXXX` under the temporary directory. */
const char *aion_fs_make_temp_dir(const char *prefix) {
    const char *base = temp_base();
    size_t n = strlen(base) + strlen(prefix) + 9;
    char *tmpl = malloc(n);
    if (!tmpl) aion_panic("out of memory");
#ifdef _WIN32
    static const char letters[] = "abcdefghijklmnopqrstuvwxyz0123456789";
    const char *out = NULL;
    for (int attempt = 0; attempt < 100 && !out; attempt++) {
        unsigned x = (unsigned)GetTickCount() * 2654435761u + (unsigned)attempt * 40503u + (unsigned)GetCurrentProcessId();
        char suffix[7];
        for (int i = 0; i < 6; i++, x = x * 1103515245u + 12345u) suffix[i] = letters[(x >> 16) % (sizeof letters - 1)];
        suffix[6] = '\0';
        snprintf(tmpl, n, "%s/%s%s", base, prefix, suffix);
        if (mkdir(tmpl, 0700) == 0) out = aion_string(tmpl);
        else if (errno != EEXIST) break;
    }
    if (!out) note();
#else
    snprintf(tmpl, n, "%s/%sXXXXXX", base, prefix);
    const char *out = mkdtemp(tmpl) ? aion_string(tmpl) : (note(), NULL);
#endif
    free(tmpl);
    return out;
}
