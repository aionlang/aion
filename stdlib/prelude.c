#include "aion.h"

#include <ctype.h>
#include <errno.h>
#include <stdio.h>
#include <stdlib.h>
#include <string.h>

int64_t aion_strings_length(const char *s) { return (int64_t)strlen(s); }

bool aion_strings_contains(const char *s, const char *needle) { return strstr(s, needle) != NULL; }

bool aion_strings_starts_with(const char *s, const char *prefix) {
    return strncmp(s, prefix, strlen(prefix)) == 0;
}

/* Returned strings are made with aion_string*() so the GC owns them. */
static const char *map_case(const char *s, int (*f)(int)) {
    size_t n = strlen(s);
    char *copy = (char *)aion_string_n(s, n);
    for (size_t i = 0; i < n; i++) copy[i] = (char)f((unsigned char)copy[i]);
    return copy;
}

const char *aion_strings_upper(const char *s) { return map_case(s, toupper); }
const char *aion_strings_lower(const char *s) { return map_case(s, tolower); }

/* substring(start, end): bytes [start, end). */
const char *aion_strings_substring(const char *s, int64_t start, int64_t end) {
    int64_t n = (int64_t)strlen(s);
    if (start < 0 || end > n || start > end) {
        char msg[128];
        snprintf(msg, sizeof msg, "substring(%lld, %lld) is out of range for length %lld", (long long)start,
                 (long long)end, (long long)n);
        aion_panic(msg);
    }
    return aion_string_n(s + start, (size_t)(end - start));
}

int64_t aion_strings_index_of(const char *s, const char *needle) {
    const char *p = strstr(s, needle);
    return p ? (int64_t)(p - s) : -1;
}

const char *aion_strings_trim(const char *s) {
    while (isspace((unsigned char)*s)) s++;
    size_t n = strlen(s);
    while (n > 0 && isspace((unsigned char)s[n - 1])) n--;
    return aion_string_n(s, n);
}

const char *aion_strings_replace(const char *s, const char *from, const char *to) {
    size_t lf = strlen(from);
    if (lf == 0) return aion_string(s);
    size_t lt = strlen(to), count = 0;
    for (const char *p = s; (p = strstr(p, from)); p += lf) count++;
    size_t n = strlen(s) + count * lt - count * lf;
    char *buf = malloc(n + 1);
    if (!buf) aion_panic("out of memory");
    char *out = buf;
    const char *p = s, *hit;
    while ((hit = strstr(p, from))) {
        memcpy(out, p, (size_t)(hit - p));
        out += hit - p;
        memcpy(out, to, lt);
        out += lt;
        p = hit + lf;
    }
    strcpy(out, p);
    const char *result = aion_string_n(buf, n);
    free(buf);
    return result;
}

int64_t aion_strings_to_int_or(const char *s, int64_t fallback) {
    char *end;
    errno = 0;
    long long v = strtoll(s, &end, 10);
    if (end == s || *end != '\0' || errno != 0) return fallback;
    return (int64_t)v;
}

bool aion_strings_is_int(const char *s) {
    char *end;
    errno = 0;
    (void)strtoll(s, &end, 10);
    return end != s && *end == '\0' && errno == 0;
}

/* ── Standard input ───────────────────────────────────────────── */

static char *grow(char *buf, size_t *cap) {
    *cap *= 2;
    buf = realloc(buf, *cap);
    if (!buf) aion_panic("out of memory");
    return buf;
}

/* One line without its line break, or NULL at the end of the input. */
const char *aion_read_line(void) {
    fflush(stdout); /* so a prompt printed with print() is visible */
    size_t cap = 128, len = 0;
    char *buf = malloc(cap);
    if (!buf) aion_panic("out of memory");
    int c;
    while ((c = getchar()) != EOF && c != '\n') {
        if (len + 1 >= cap) buf = grow(buf, &cap);
        buf[len++] = (char)c;
    }
    if (c == EOF && len == 0) {
        free(buf);
        return NULL;
    }
    if (len > 0 && buf[len - 1] == '\r') len--;
    const char *line = aion_string_n(buf, len);
    free(buf);
    return line;
}

/* Everything left on standard input. */
const char *aion_read_text(void) {
    fflush(stdout);
    size_t cap = 1024, len = 0;
    char *buf = malloc(cap);
    if (!buf) aion_panic("out of memory");
    int c;
    while ((c = getchar()) != EOF) {
        if (len + 1 >= cap) buf = grow(buf, &cap);
        buf[len++] = (char)c;
    }
    const char *text = aion_string_n(buf, len);
    free(buf);
    return text;
}

/* ── Characters (Unicode code points held in an int64_t) ──────── */

/* The code point that starts at byte `i` of `s`; U+FFFD for malformed UTF-8. */
int64_t aion_str_char_at(const char *s, int64_t i) {
    int64_t n = (int64_t)strlen(s);
    if (i < 0 || i >= n) {
        char msg[96];
        snprintf(msg, sizeof msg, "index %lld is out of bounds for length %lld", (long long)i, (long long)n);
        aion_panic(msg);
    }
    const unsigned char *p = (const unsigned char *)s + i;
    int len = p[0] < 0x80 ? 1 : (p[0] >> 5) == 6 ? 2 : (p[0] >> 4) == 14 ? 3 : (p[0] >> 3) == 30 ? 4 : 0;
    if (len == 0 || i + len > n) return 0xFFFD;
    if (len == 1) return p[0];
    int64_t c = p[0] & (0x7F >> len);
    for (int k = 1; k < len; k++) {
        if ((p[k] & 0xC0) != 0x80) return 0xFFFD;
        c = (c << 6) | (p[k] & 0x3F);
    }
    return c;
}

/* How many bytes the code point takes in UTF-8. */
int64_t aion_char_utf8_len(int64_t c) {
    if (c < 0 || c > 0x10FFFF || (c >= 0xD800 && c <= 0xDFFF)) return 3; /* U+FFFD */
    return c < 0x80 ? 1 : c < 0x800 ? 2 : c < 0x10000 ? 3 : 4;
}

bool aion_char_is_digit(int64_t c) { return c >= '0' && c <= '9'; }

/* ASCII and Latin-1 letters, Greek and Cyrillic, and other scripts as a rule of thumb:
 * anything above U+00BF that is not in the punctuation, symbol or emoji blocks. */
bool aion_char_is_letter(int64_t c) {
    if (c < 0x80) return (c >= 'a' && c <= 'z') || (c >= 'A' && c <= 'Z');
    if (c < 0xC0 || c == 0xD7 || c == 0xF7) return false;
    if (c >= 0x2000 && c <= 0x2BFF) return false;
    if (c >= 0x3000 && c <= 0x303F) return false;
    return c < 0x1F000;
}

bool aion_char_is_upper(int64_t c) {
    return (c >= 'A' && c <= 'Z') || (c >= 0xC0 && c <= 0xDE && c != 0xD7) || (c >= 0x391 && c <= 0x3A9) ||
           (c >= 0x400 && c <= 0x42F);
}

bool aion_char_is_lower(int64_t c) {
    return (c >= 'a' && c <= 'z') || (c >= 0xDF && c <= 0xFF && c != 0xF7) || (c >= 0x3B1 && c <= 0x3C9) ||
           (c >= 0x430 && c <= 0x45F);
}

bool aion_char_is_space(int64_t c) {
    return c == ' ' || (c >= 9 && c <= 13) || c == 0x85 || c == 0xA0 || c == 0x2028 || c == 0x2029 || c == 0x3000;
}

int64_t aion_char_upper(int64_t c) {
    if (c >= 'a' && c <= 'z') return c - 32;
    if ((c >= 0xE0 && c <= 0xFE && c != 0xF7)) return c - 32;
    if (c >= 0x3B1 && c <= 0x3C9 && c != 0x3C2) return c - 32;
    if (c >= 0x430 && c <= 0x44F) return c - 32;
    if (c >= 0x450 && c <= 0x45F) return c - 80;
    return c;
}

int64_t aion_char_lower(int64_t c) {
    if (c >= 'A' && c <= 'Z') return c + 32;
    if (c >= 0xC0 && c <= 0xDE && c != 0xD7) return c + 32;
    if (c >= 0x391 && c <= 0x3A9) return c + 32;
    if (c >= 0x410 && c <= 0x42F) return c + 32;
    if (c >= 0x400 && c <= 0x40F) return c + 80;
    return c;
}
