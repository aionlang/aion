/*
 * C half of std.archive: reading and writing zip, tar (with gzip, bzip2, xz or zstd), 7z and more (POSIX).
 * The work is done by the libarchive library, loaded at run time with dlopen like OpenSSL in std.crypto: a
 * program that never imports std.archive needs nothing installed, and no header files are required to build.
 *
 * Extraction is careful: an entry whose name is absolute or climbs out with `..` stops the extraction, links and
 * devices are never created, and the total size is limited, so a hostile archive cannot write outside the
 * destination or fill the disk.
 */

#define _GNU_SOURCE
#include "aion.h"

#include "aion_platform.h"
#include <errno.h>
#include <locale.h>
#include <stdio.h>
#include <stdlib.h>
#include <string.h>
#include <sys/stat.h>
#include <unistd.h>
#ifdef _WIN32
#include <direct.h>
#define mkdir(path, mode) _mkdir(path)
#endif

static char error_text[512];

const char *aion_archive_error(void) { return aion_string(error_text); }

static bool fail(const char *message) {
    snprintf(error_text, sizeof error_text, "%s", message);
    return false;
}

#define ARCHIVE_OK 0
#define ARCHIVE_EOF 1
#define ARCHIVE_WARN (-20)
#define AE_IFMT 0170000
#define AE_IFREG 0100000
#define AE_IFDIR 0040000
#define AE_IFLNK 0120000

static struct {
    bool tried, ok;
    void *(*read_new)(void);
    int (*read_support_filter_all)(void *);
    int (*read_support_format_all)(void *);
    int (*read_open_filename)(void *, const char *, size_t);
    int (*read_next_header)(void *, void **);
    ssize_t (*read_data)(void *, void *, size_t);
    int (*read_data_skip)(void *);
    int (*read_free)(void *);
    const char *(*error_string)(void *);
    const char *(*entry_pathname)(void *);
    int64_t (*entry_size)(void *);
    unsigned (*entry_filetype)(void *);
    unsigned (*entry_perm)(void *);
    int64_t (*entry_mtime)(void *);
    void *(*write_new)(void);
    int (*write_set_format_by_name)(void *, const char *);
    int (*write_add_filter_by_name)(void *, const char *);
    int (*write_open_filename)(void *, const char *);
    int (*write_header)(void *, void *);
    ssize_t (*write_data)(void *, const void *, size_t);
    int (*write_close)(void *);
    int (*write_free)(void *);
    void *(*entry_new)(void);
    void (*entry_free)(void *);
    void (*entry_set_pathname)(void *, const char *);
    void (*entry_set_size)(void *, int64_t);
    void (*entry_set_filetype)(void *, unsigned);
    void (*entry_set_perm)(void *, unsigned);
    void (*entry_set_mtime)(void *, int64_t, long);
} la;

static bool load(void) {
    if (la.tried) return la.ok;
    la.tried = true;
    #ifdef _WIN32
    static const char *const names[] = {"libarchive-13.dll", "libarchive.dll", NULL};
    #else
    static const char *const names[] = {"libarchive.so.13", "libarchive.so", "libarchive.13.dylib", "libarchive.dylib", NULL};
    #endif
    void *lib = aion_dlopen(names);
    if (!lib) return fail("std.archive needs the libarchive library: install it (for example the libarchive13t64 package)");
#define LOAD(field, symbol) \
    if (!(*(void **)&la.field = aion_dlsym(lib, symbol))) return fail("the installed libarchive is missing " symbol);
    LOAD(read_new, "archive_read_new") LOAD(read_support_filter_all, "archive_read_support_filter_all")
    LOAD(read_support_format_all, "archive_read_support_format_all") LOAD(read_open_filename, "archive_read_open_filename")
    LOAD(read_next_header, "archive_read_next_header") LOAD(read_data, "archive_read_data")
    LOAD(read_data_skip, "archive_read_data_skip") LOAD(read_free, "archive_read_free") LOAD(error_string, "archive_error_string")
    LOAD(entry_pathname, "archive_entry_pathname") LOAD(entry_size, "archive_entry_size")
    LOAD(entry_filetype, "archive_entry_filetype") LOAD(entry_perm, "archive_entry_perm") LOAD(entry_mtime, "archive_entry_mtime")
    LOAD(write_new, "archive_write_new") LOAD(write_set_format_by_name, "archive_write_set_format_by_name")
    LOAD(write_add_filter_by_name, "archive_write_add_filter_by_name") LOAD(write_open_filename, "archive_write_open_filename")
    LOAD(write_header, "archive_write_header") LOAD(write_data, "archive_write_data") LOAD(write_close, "archive_write_close")
    LOAD(write_free, "archive_write_free") LOAD(entry_new, "archive_entry_new") LOAD(entry_free, "archive_entry_free")
    LOAD(entry_set_pathname, "archive_entry_set_pathname") LOAD(entry_set_size, "archive_entry_set_size")
    LOAD(entry_set_filetype, "archive_entry_set_filetype") LOAD(entry_set_perm, "archive_entry_set_perm")
    LOAD(entry_set_mtime, "archive_entry_set_mtime")
#undef LOAD
    /* libarchive converts names between UTF-8 and the locale's character set, and fails for letters the "C" locale
     * cannot hold; a UTF-8 locale makes every name pass through unchanged */
    if (!setlocale(LC_CTYPE, "C.UTF-8") && !setlocale(LC_CTYPE, "en_US.UTF-8")) setlocale(LC_CTYPE, "");
    la.ok = true;
    return true;
}

bool aion_archive_available(void) { return load(); }

/* ── Names ───────────────────────────────────────────────────────── */

static const char *name_of(void *entry) { return la.entry_pathname(entry); }

/* Is `name` a relative path that stays inside the folder it is extracted to? */
static bool safe_name(const char *name) {
    if (!*name || name[0] == '/' || name[0] == '\\') return false;
    if (((name[0] >= 'A' && name[0] <= 'Z') || (name[0] >= 'a' && name[0] <= 'z')) && name[1] == ':') return false;
    for (const char *p = name; *p;) {
        const char *end = p;
        while (*end && *end != '/' && *end != '\\') end++;
        size_t n = (size_t)(end - p);
        if (n == 2 && p[0] == '.' && p[1] == '.') return false;
        for (const char *c = p; c < end; c++)
            if ((unsigned char)*c < 32) return false;
        p = *end ? end + 1 : end;
    }
    return true;
}

/* The name without a leading "./". */
static const char *plain_name(const char *name) {
    while (name[0] == '.' && name[1] == '/') name += 2;
    return name;
}

static void *open_reader(const char *path) {
    if (!load()) return NULL;
    void *a = la.read_new();
    if (!a) return fail("out of memory"), NULL;
    la.read_support_filter_all(a);
    la.read_support_format_all(a);
    if (la.read_open_filename(a, path, 65536) != ARCHIVE_OK) {
        snprintf(error_text, sizeof error_text, "cannot open %s: %s", path, la.error_string(a) ? la.error_string(a) : "unknown error");
        la.read_free(a);
        return NULL;
    }
    return a;
}

/* ── Listing and reading ─────────────────────────────────────────── */

static void escape_into(char *out, size_t *used, size_t capacity, const char *text) {
    for (; *text && *used + 3 < capacity; text++) {
        if (*text == '\t' || *text == '\n' || *text == '\\') {
            out[(*used)++] = '\\';
            out[(*used)++] = *text == '\t' ? 't' : *text == '\n' ? 'n' : '\\';
        } else {
            out[(*used)++] = *text;
        }
    }
}

/* One line per entry: kind (f file, d folder, l link, o other), size, modified (seconds), permissions (octal) and
 * the escaped name, separated by tabs.  NULL if the archive cannot be read. */
const char *aion_archive_list(const char *path) {
    void *a = open_reader(path);
    if (!a) return NULL;
    size_t capacity = 1 << 16, used = 0;
    char *out = malloc(capacity);
    void *entry;
    int r;
    while ((r = la.read_next_header(a, &entry)) == ARCHIVE_OK || r == ARCHIVE_WARN) {
        const char *name = name_of(entry);
        if (!name) name = "";
        if (used + 4 * strlen(name) + 128 > capacity) {
            capacity = capacity * 2 + 4 * strlen(name) + 128;
            out = realloc(out, capacity);
        }
        unsigned type = la.entry_filetype(entry) & AE_IFMT;
        char kind = type == AE_IFREG ? 'f' : type == AE_IFDIR ? 'd' : type == AE_IFLNK ? 'l' : 'o';
        int64_t size = la.entry_size(entry);
        used += (size_t)snprintf(out + used, 100, "%c\t%lld\t%lld\t%o\t", kind, (long long)(size < 0 ? 0 : size),
                                 (long long)la.entry_mtime(entry), la.entry_perm(entry));
        escape_into(out, &used, capacity, name);
        out[used++] = '\n';
        la.read_data_skip(a);
    }
    bool bad = r != ARCHIVE_EOF;
    if (bad) snprintf(error_text, sizeof error_text, "cannot read %s: %s", path, la.error_string(a) ? la.error_string(a) : "damaged archive");
    la.read_free(a);
    if (bad) {
        free(out);
        return NULL;
    }
    out[used] = 0;
    const char *result = aion_string_n(out, used);
    free(out);
    return result;
}

/* The contents of the entry called `name`; NULL (with an error text starting "no entry") if there is none. */
void *aion_archive_read(const char *path, const char *name, int64_t max_bytes) {
    void *a = open_reader(path);
    if (!a) return NULL;
    void *entry;
    int r;
    const char *wanted = plain_name(name);
    while ((r = la.read_next_header(a, &entry)) == ARCHIVE_OK || r == ARCHIVE_WARN) {
        const char *found = name_of(entry);
        if (!found || strcmp(plain_name(found), wanted) != 0) {
            la.read_data_skip(a);
            continue;
        }
        if ((la.entry_filetype(entry) & AE_IFMT) == AE_IFDIR) {
            la.read_free(a);
            return fail("that entry is a folder"), NULL;
        }
        size_t capacity = 1 << 16, used = 0;
        uint8_t *data = malloc(capacity);
        for (;;) {
            if (used == capacity) {
                capacity *= 2;
                data = realloc(data, capacity);
            }
            ssize_t n = la.read_data(a, data + used, capacity - used);
            if (n == 0) break;
            if (n < 0) {
                snprintf(error_text, sizeof error_text, "cannot read %s: %s", name, la.error_string(a) ? la.error_string(a) : "damaged archive");
                free(data);
                la.read_free(a);
                return NULL;
            }
            used += (size_t)n;
            if ((int64_t)used > max_bytes) {
                free(data);
                la.read_free(a);
                return fail("the entry is larger than the limit"), NULL;
            }
        }
        void *result = aion_bytes_new((int64_t)used);
        memcpy(aion_bytes_data(result), data, used);
        free(data);
        la.read_free(a);
        return result;
    }
    la.read_free(a);
    snprintf(error_text, sizeof error_text, "no entry called %s", name);
    return NULL;
}

/* ── Extraction ──────────────────────────────────────────────────── */

static bool make_parents(char *path) {
    for (char *p = path + 1; *p; p++) {
        if (*p != '/') continue;
        *p = 0;
        if (mkdir(path, 0777) != 0 && errno != EEXIST) {
            *p = '/';
            return false;
        }
        *p = '/';
    }
    return true;
}

/* Extract every file and folder into `dest`.  Gives the number of files written, or -1 with an error text. */
int64_t aion_archive_extract(const char *path, const char *dest, int64_t max_total) {
    void *a = open_reader(path);
    if (!a) return -1;
    if (mkdir(dest, 0777) != 0 && errno != EEXIST) {
        snprintf(error_text, sizeof error_text, "cannot create %s: %s", dest, strerror(errno));
        la.read_free(a);
        return -1;
    }
    int64_t files = 0, total = 0;
    void *entry;
    int r;
    char target[4096];
    while ((r = la.read_next_header(a, &entry)) == ARCHIVE_OK || r == ARCHIVE_WARN) {
        const char *raw = name_of(entry);
        const char *name = plain_name(raw ? raw : "");
        if (!*name || strcmp(name, ".") == 0) {
            la.read_data_skip(a);
            continue;
        }
        if (!safe_name(name)) {
            snprintf(error_text, sizeof error_text, "the archive has an unsafe name, which would leave the destination: %s", raw ? raw : "");
            files = -1;
            break;
        }
        if (snprintf(target, sizeof target, "%s/%s", dest, name) >= (int)sizeof target) {
            snprintf(error_text, sizeof error_text, "a name in the archive is too long: %s", name);
            files = -1;
            break;
        }
        unsigned type = la.entry_filetype(entry) & AE_IFMT;
        unsigned perm = la.entry_perm(entry) & 0777;
        if (type == AE_IFDIR) {
            size_t n = strlen(target);
            while (n > 1 && target[n - 1] == '/') target[--n] = 0;
            if (!make_parents(target) || (mkdir(target, 0777) != 0 && errno != EEXIST)) {
                snprintf(error_text, sizeof error_text, "cannot create %s: %s", target, strerror(errno));
                files = -1;
                break;
            }
            continue;
        }
        if (type != AE_IFREG) { /* links, devices, pipes: never created */
            la.read_data_skip(a);
            continue;
        }
        if (!make_parents(target)) {
            snprintf(error_text, sizeof error_text, "cannot create the folders for %s: %s", target, strerror(errno));
            files = -1;
            break;
        }
        FILE *f = fopen(target, "wb");
        if (!f) {
            snprintf(error_text, sizeof error_text, "cannot write %s: %s", target, strerror(errno));
            files = -1;
            break;
        }
        char buffer[65536];
        ssize_t n;
        bool good = true;
        while ((n = la.read_data(a, buffer, sizeof buffer)) > 0) {
            total += n;
            if (total > max_total) {
                fail("the archive expands to more than the limit");
                good = false;
                break;
            }
            if (fwrite(buffer, 1, (size_t)n, f) != (size_t)n) {
                snprintf(error_text, sizeof error_text, "cannot write %s: %s", target, strerror(errno));
                good = false;
                break;
            }
        }
        if (n < 0 && good) {
            snprintf(error_text, sizeof error_text, "cannot read %s: %s", name, la.error_string(a) ? la.error_string(a) : "damaged archive");
            good = false;
        }
        fclose(f);
        if (!good) {
            files = -1;
            break;
        }
        chmod(target, perm ? perm : 0644);
        files++;
    }
    if (files >= 0 && r != ARCHIVE_EOF) {
        snprintf(error_text, sizeof error_text, "cannot read %s: %s", path, la.error_string(a) ? la.error_string(a) : "damaged archive");
        files = -1;
    }
    la.read_free(a);
    return files;
}

/* ── Creation ────────────────────────────────────────────────────── */

static void *open_writer(const char *path, const char *format, const char *filter) {
    if (!load()) return NULL;
    void *w = la.write_new();
    if (!w) return fail("out of memory"), NULL;
    if (la.write_set_format_by_name(w, format) != ARCHIVE_OK ||
        (*filter && la.write_add_filter_by_name(w, filter) != ARCHIVE_OK) || la.write_open_filename(w, path) != ARCHIVE_OK) {
        snprintf(error_text, sizeof error_text, "cannot create %s: %s", path, la.error_string(w) ? la.error_string(w) : "unknown error");
        la.write_free(w);
        return NULL;
    }
    return w;
}

static bool put_header(void *w, const char *name, unsigned type, unsigned perm, int64_t size, int64_t mtime) {
    void *e = la.entry_new();
    la.entry_set_pathname(e, name);
    la.entry_set_filetype(e, type);
    la.entry_set_perm(e, perm);
    la.entry_set_size(e, type == AE_IFDIR ? 0 : size);
    la.entry_set_mtime(e, mtime, 0);
    int r = la.write_header(w, e);
    la.entry_free(e);
    if (r < ARCHIVE_WARN) {
        snprintf(error_text, sizeof error_text, "cannot write the entry %s: %s", name, la.error_string(w) ? la.error_string(w) : "unknown error");
        return false;
    }
    return true;
}

static bool finish(void *w, const char *path, bool good) {
    int r = la.write_close(w);
    if (good && r < ARCHIVE_WARN) {
        snprintf(error_text, sizeof error_text, "cannot finish %s: %s", path, la.error_string(w) ? la.error_string(w) : "unknown error");
        good = false;
    }
    la.write_free(w);
    return good;
}

static uint32_t be32(const uint8_t *p) { return (uint32_t)p[0] << 24 | (uint32_t)p[1] << 16 | (uint32_t)p[2] << 8 | p[3]; }

/* Create an archive from entries held in memory.  `records` is, for each entry: a 4-byte name length, the name, a
 * 4-byte permission value, a 1-byte kind (0 file, 1 folder), an 8-byte size and that many bytes of contents. */
bool aion_archive_create(const char *path, const char *format, const char *filter, const void *records, int64_t mtime) {
    void *w = open_writer(path, format, filter);
    if (!w) return false;
    const uint8_t *p = aion_bytes_data((void *)records), *end = p + aion_bytes_size(records);
    bool good = true;
    while (good && p < end) {
        if (end - p < 4) { good = fail("damaged entry list"); break; }
        uint32_t name_len = be32(p);
        p += 4;
        if ((int64_t)(end - p) < (int64_t)name_len + 13) { good = fail("damaged entry list"); break; }
        char *name = malloc(name_len + 1);
        memcpy(name, p, name_len);
        name[name_len] = 0;
        p += name_len;
        unsigned perm = be32(p);
        int kind = p[4];
        uint64_t size = (uint64_t)be32(p + 5) << 32 | be32(p + 9);
        p += 13;
        if ((uint64_t)(end - p) < size) { free(name); good = fail("damaged entry list"); break; }
        good = put_header(w, name, kind == 1 ? AE_IFDIR : AE_IFREG, perm, (int64_t)size, mtime);
        if (good && kind != 1 && size > 0 && la.write_data(w, p, (size_t)size) < 0) {
            snprintf(error_text, sizeof error_text, "cannot write %s: %s", name, la.error_string(w) ? la.error_string(w) : "unknown error");
            good = false;
        }
        free(name);
        p += size;
    }
    return finish(w, path, good);
}

/* Create an archive from files on disk.  `list` has one line per entry: the name inside the archive, a tab, the
 * path on disk, a tab and "f" or "d". */
bool aion_archive_create_files(const char *path, const char *format, const char *filter, const char *list) {
    void *w = open_writer(path, format, filter);
    if (!w) return false;
    bool good = true;
    char *copy = strdup(list), *save = NULL;
    for (char *line = strtok_r(copy, "\n", &save); good && line; line = strtok_r(NULL, "\n", &save)) {
        char *name = line, *disk = strchr(line, '\t');
        if (!disk) continue;
        *disk++ = 0;
        char *kind = strchr(disk, '\t');
        if (!kind) continue;
        *kind++ = 0;
        struct stat st;
        if (stat(disk, &st) != 0) {
            snprintf(error_text, sizeof error_text, "cannot read %s: %s", disk, strerror(errno));
            good = false;
            break;
        }
        if (*kind == 'd') {
            good = put_header(w, name, AE_IFDIR, st.st_mode & 0777, 0, st.st_mtime);
            continue;
        }
        FILE *f = fopen(disk, "rb");
        if (!f) {
            snprintf(error_text, sizeof error_text, "cannot read %s: %s", disk, strerror(errno));
            good = false;
            break;
        }
        good = put_header(w, name, AE_IFREG, st.st_mode & 0777, st.st_size, st.st_mtime);
        char buffer[65536];
        size_t n;
        int64_t sent = 0;
        while (good && (n = fread(buffer, 1, sizeof buffer, f)) > 0) {
            if (la.write_data(w, buffer, n) < 0) {
                snprintf(error_text, sizeof error_text, "cannot write %s: %s", name, la.error_string(w) ? la.error_string(w) : "unknown error");
                good = false;
            }
            sent += (int64_t)n;
        }
        fclose(f);
        if (good && sent != (int64_t)st.st_size) good = fail("a file changed while it was being added");
    }
    free(copy);
    return finish(w, path, good);
}
