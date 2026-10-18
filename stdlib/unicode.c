/*
 * C half of std.unicode: normalization, case mapping, character categories and widths, and grapheme clusters
 * (POSIX).  The data comes from the utf8proc library, loaded at run time with dlopen like OpenSSL in std.crypto:
 * a program that never imports std.unicode needs nothing installed, and no header files are required to build.
 */

#define _GNU_SOURCE
#include "aion.h"

#include <dlfcn.h>
#include <stdio.h>
#include <stdlib.h>
#include <string.h>

static char error_text[256];

const char *aion_unicode_error(void) { return aion_string(error_text); }

typedef ssize_t (*map_fn)(const uint8_t *, ssize_t, uint8_t **, int);

static struct {
    bool tried, ok;
    ssize_t (*utf8proc_map)(const uint8_t *, ssize_t, uint8_t **, int);
    ssize_t (*utf8proc_iterate)(const uint8_t *, ssize_t, int32_t *);
    ssize_t (*utf8proc_encode_char)(int32_t, uint8_t *);
    int32_t (*utf8proc_tolower)(int32_t);
    int32_t (*utf8proc_toupper)(int32_t);
    int32_t (*utf8proc_totitle)(int32_t);
    const char *(*utf8proc_category_string)(int32_t);
    int (*utf8proc_charwidth)(int32_t);
    bool (*utf8proc_grapheme_break_stateful)(int32_t, int32_t, int32_t *);
    const char *(*utf8proc_version)(void);
} u8;

static bool load(void) {
    if (u8.tried) return u8.ok;
    u8.tried = true;
    static const char *const names[] = {"libutf8proc.so.3", "libutf8proc.so.2", "libutf8proc.so", "libutf8proc.3.dylib",
                                        "libutf8proc.2.dylib", "libutf8proc.dylib", NULL};
    void *lib = NULL;
    for (const char *const *n = names; *n && !lib; n++) lib = dlopen(*n, RTLD_NOW | RTLD_GLOBAL);
    if (!lib) {
        snprintf(error_text, sizeof error_text, "std.unicode needs the utf8proc library: install libutf8proc (for example the libutf8proc3 package)");
        return false;
    }
#define LOAD(name) \
    if (!(*(void **)&u8.name = dlsym(lib, #name))) { snprintf(error_text, sizeof error_text, "the installed utf8proc is missing " #name); return false; }
    LOAD(utf8proc_map) LOAD(utf8proc_iterate) LOAD(utf8proc_encode_char) LOAD(utf8proc_tolower) LOAD(utf8proc_toupper)
    LOAD(utf8proc_totitle) LOAD(utf8proc_category_string) LOAD(utf8proc_charwidth) LOAD(utf8proc_grapheme_break_stateful)
    LOAD(utf8proc_version)
#undef LOAD
    u8.ok = true;
    return true;
}

bool aion_unicode_available(void) { return load(); }

const char *aion_unicode_version(void) { return aion_string(load() ? u8.utf8proc_version() : ""); }

/* utf8proc_option_t bits */
enum { NULLTERM = 1 << 0, STABLE = 1 << 1, COMPAT = 1 << 2, COMPOSE = 1 << 3, DECOMPOSE = 1 << 4, IGNORE = 1 << 5,
       STRIPCC = 1 << 9, CASEFOLD = 1 << 10, STRIPMARK = 1 << 13 };

/* Normalization and folding: `mode` 0 NFC, 1 NFD, 2 NFKC, 3 NFKD, 4 case folding (with NFKC), 5 accents removed.
 * Invalid UTF-8 gives NULL. */
const char *aion_unicode_map(const char *text, int64_t mode) {
    if (!load()) return NULL;
    int options = NULLTERM | STABLE;
    switch (mode) {
    case 0: options |= COMPOSE; break;
    case 1: options |= DECOMPOSE; break;
    case 2: options |= COMPOSE | COMPAT; break;
    case 3: options |= DECOMPOSE | COMPAT; break;
    case 4: options |= COMPOSE | COMPAT | CASEFOLD; break;
    default: options |= DECOMPOSE | STRIPMARK; break;
    }
    uint8_t *out = NULL;
    ssize_t n = u8.utf8proc_map((const uint8_t *)text, 0, &out, options);
    if (n < 0) return NULL;
    const char *result = aion_string_n((const char *)out, (size_t)n);
    free(out);
    return result;
}

/* Change the case of every character: `mode` 0 lower, 1 upper; title case (2) capitalises the first letter of
 * each word and lowers the rest.  Invalid UTF-8 gives NULL. */
const char *aion_unicode_case(const char *text, int64_t mode) {
    if (!load()) return NULL;
    size_t len = strlen(text);
    char *out = malloc(len * 4 + 8);
    if (!out) return NULL;
    size_t used = 0;
    bool in_word = false;
    for (size_t i = 0; i < len;) {
        int32_t c;
        ssize_t n = u8.utf8proc_iterate((const uint8_t *)text + i, (ssize_t)(len - i), &c);
        if (n < 0) {
            free(out);
            return NULL;
        }
        i += (size_t)n;
        int32_t mapped;
        if (mode == 0) mapped = u8.utf8proc_tolower(c);
        else if (mode == 1) mapped = u8.utf8proc_toupper(c);
        else {
            const char *cat = u8.utf8proc_category_string(c);
            bool letter = cat[0] == 'L' || cat[0] == 'N' || (cat[0] == 'M') || c == '\'';
            mapped = !letter ? c : in_word ? u8.utf8proc_tolower(c) : u8.utf8proc_totitle(c);
            in_word = letter;
        }
        used += (size_t)u8.utf8proc_encode_char(mapped, (uint8_t *)out + used);
    }
    out[used] = 0;
    const char *result = aion_string_n(out, used);
    free(out);
    return result;
}

/* The two-letter category of a code point: "Lu", "Ll", "Nd", "Zs", ... */
const char *aion_unicode_category(int64_t c) {
    if (!load() || c < 0 || c > 0x10ffff) return aion_string("");
    return aion_string(u8.utf8proc_category_string((int32_t)c));
}

/* The columns a text takes on a terminal: wide East Asian characters 2, combining marks 0. */
int64_t aion_unicode_width(const char *text) {
    if (!load()) return -1;
    size_t len = strlen(text);
    int64_t width = 0;
    for (size_t i = 0; i < len;) {
        int32_t c;
        ssize_t n = u8.utf8proc_iterate((const uint8_t *)text + i, (ssize_t)(len - i), &c);
        if (n < 0) return -1;
        i += (size_t)n;
        width += u8.utf8proc_charwidth(c);
    }
    return width;
}

/* The user-perceived characters (extended grapheme clusters) of a text, separated by the byte 0x1f; NULL if
 * the text is not valid UTF-8. */
const char *aion_unicode_graphemes(const char *text) {
    if (!load()) return NULL;
    size_t len = strlen(text);
    char *out = malloc(len * 2 + 2);
    if (!out) return NULL;
    size_t used = 0;
    int32_t state = 0, previous = -1;
    for (size_t i = 0; i < len;) {
        int32_t c;
        ssize_t n = u8.utf8proc_iterate((const uint8_t *)text + i, (ssize_t)(len - i), &c);
        if (n < 0) {
            free(out);
            return NULL;
        }
        if (previous >= 0 && u8.utf8proc_grapheme_break_stateful(previous, c, &state)) out[used++] = 0x1f;
        memcpy(out + used, text + i, (size_t)n);
        used += (size_t)n;
        i += (size_t)n;
        previous = c;
    }
    out[used] = 0;
    const char *result = aion_string_n(out, used);
    free(out);
    return result;
}
