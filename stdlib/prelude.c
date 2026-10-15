#include "aion.h"

#include <ctype.h>
#include <errno.h>
#include <math.h>
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

bool aion_strings_ends_with(const char *s, const char *suffix) {
    size_t n = strlen(s), m = strlen(suffix);
    return m <= n && memcmp(s + n - m, suffix, m) == 0;
}

int64_t aion_strings_last_index_of(const char *s, const char *needle) {
    size_t n = strlen(s), m = strlen(needle);
    if (m > n) return -1;
    for (size_t i = n - m + 1; i-- > 0;)
        if (memcmp(s + i, needle, m) == 0) return (int64_t)i;
    return -1;
}

const char *aion_strings_trim_start(const char *s) {
    while (isspace((unsigned char)*s)) s++;
    return aion_string(s);
}

const char *aion_strings_trim_end(const char *s) {
    size_t n = strlen(s);
    while (n > 0 && isspace((unsigned char)s[n - 1])) n--;
    return aion_string_n(s, n);
}

const char *aion_strings_repeat(const char *s, int64_t times) {
    if (times < 0) aion_panic("repeat needs a non-negative count");
    size_t n = strlen(s);
    if (n != 0 && (uint64_t)times > (UINT32_MAX - 1) / n) aion_panic("the repeated text is too large");
    size_t total = n * (size_t)times;
    char *buf = malloc(total + 1);
    if (!buf) aion_panic("out of memory");
    for (int64_t i = 0; i < times; i++) memcpy(buf + (size_t)i * n, s, n);
    const char *r = aion_string_n(buf, total);
    free(buf);
    return r;
}

/* Code points in the text: every byte that is not a UTF-8 continuation byte starts one. */
int64_t aion_strings_char_count(const char *s) {
    int64_t n = 0;
    for (; *s; s++) n += ((unsigned char)*s & 0xC0) != 0x80;
    return n;
}

/* Pad with copies of `fill` (one character) up to `width` characters. */
const char *aion_strings_pad(const char *s, int64_t width, const char *fill, bool left) {
    int64_t have = aion_strings_char_count(s);
    size_t flen = strlen(fill);
    if (aion_strings_char_count(fill) != 1) aion_panic("the padding must be exactly one character");
    if (width <= have) return aion_string(s);
    int64_t missing = width - have;
    size_t n = strlen(s);
    if ((uint64_t)missing > (UINT32_MAX - 1) / flen) aion_panic("the padded text is too large");
    size_t total = n + (size_t)missing * flen;
    char *buf = malloc(total + 1);
    if (!buf) aion_panic("out of memory");
    char *pad = buf + (left ? 0 : n);
    for (int64_t i = 0; i < missing; i++) memcpy(pad + (size_t)i * flen, fill, flen);
    memcpy(left ? buf + (size_t)missing * flen : buf, s, n);
    const char *r = aion_string_n(buf, total);
    free(buf);
    return r;
}

/* A plain decimal number: digits, a point and an exponent (no hex, no "nan"/"inf", no spaces). */
bool aion_strings_is_float(const char *s) {
    if (*s == '\0' || strspn(s, "+-0123456789.eE") != strlen(s)) return false;
    char *end;
    errno = 0;
    (void)strtod(s, &end);
    return end != s && *end == '\0';
}

double aion_strings_to_float_or(const char *s, double fallback) {
    return aion_strings_is_float(s) ? strtod(s, NULL) : fallback;
}

/* ── Numbers ──────────────────────────────────────────────────── */

int64_t aion_float_to_int(double x) {
    if (x != x) aion_panic("cannot turn NaN into an Int");
    if (x >= 9223372036854775808.0) return INT64_MAX;
    if (x <= -9223372036854775808.0) return INT64_MIN;
    return (int64_t)x;
}

int64_t aion_float_round(double x) { return aion_float_to_int(round(x)); }
int64_t aion_float_floor(double x) { return aion_float_to_int(floor(x)); }
int64_t aion_float_ceil(double x) { return aion_float_to_int(ceil(x)); }
bool aion_float_is_nan(double x) { return isnan(x); }
bool aion_float_is_inf(double x) { return isinf(x); }

static char *format_float(double x, int64_t decimals, int *length) {
    if (decimals < 0 || decimals > 100) aion_panic("the number of decimals must be between 0 and 100");
    int n = snprintf(NULL, 0, "%.*f", (int)decimals, x);
    char *buf = malloc((size_t)n + 1);
    if (!buf) aion_panic("out of memory");
    snprintf(buf, (size_t)n + 1, "%.*f", (int)decimals, x);
    *length = n;
    return buf;
}

const char *aion_float_format(double x, int64_t decimals) {
    int n;
    char *buf = format_float(x, decimals, &n);
    const char *s = aion_string_n(buf, (size_t)n);
    free(buf);
    return s;
}

double aion_float_round_to(double x, int64_t decimals) {
    int n;
    char *buf = format_float(x, decimals, &n);
    double r = strtod(buf, NULL);
    free(buf);
    return r;
}

const char *aion_int_to_radix(int64_t v, int64_t radix) {
    if (radix < 2 || radix > 36) aion_panic("the base must be between 2 and 36");
    char buf[72];
    int at = (int)sizeof buf;
    uint64_t u = v < 0 ? 0 - (uint64_t)v : (uint64_t)v;
    do {
        buf[--at] = "0123456789abcdefghijklmnopqrstuvwxyz"[u % (uint64_t)radix];
        u /= (uint64_t)radix;
    } while (u);
    if (v < 0) buf[--at] = '-';
    return aion_string_n(buf + at, sizeof buf - (size_t)at);
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

/* ── Bytes ────────────────────────────────────────────────────── */

void *aion_bytes_make(int64_t size, int64_t fill) {
    void *b = aion_bytes_new(size);
    if (fill) memset(aion_bytes_data(b), (int)(fill & 0xFF), (size_t)size);
    return b;
}

int64_t aion_bytes_get(const void *b, int64_t i) {
    int64_t n = aion_bytes_size(b);
    if (i < 0 || i >= n) {
        char msg[96];
        snprintf(msg, sizeof msg, "index %lld is out of bounds for size %lld", (long long)i, (long long)n);
        aion_panic(msg);
    }
    return ((const uint8_t *)b)[i];
}

void aion_bytes_set(void *b, int64_t i, int64_t v) {
    int64_t n = aion_bytes_size(b);
    if (i < 0 || i >= n) {
        char msg[96];
        snprintf(msg, sizeof msg, "index %lld is out of bounds for size %lld", (long long)i, (long long)n);
        aion_panic(msg);
    }
    if (v < 0 || v > 255) aion_panic("a byte is a number from 0 to 255");
    ((uint8_t *)b)[i] = (uint8_t)v;
}

void *aion_bytes_slice(const void *b, int64_t from, int64_t to) {
    int64_t n = aion_bytes_size(b);
    if (from < 0 || to > n || from > to) {
        char msg[128];
        snprintf(msg, sizeof msg, "slice bounds [%lld..%lld] are out of range for size %lld", (long long)from,
                 (long long)to, (long long)n);
        aion_panic(msg);
    }
    void *out = aion_bytes_new(to - from);
    memcpy(aion_bytes_data(out), (const uint8_t *)b + from, (size_t)(to - from));
    return out;
}

void *aion_bytes_concat(const void *a, const void *b) {
    int64_t na = aion_bytes_size(a), nb = aion_bytes_size(b);
    void *out = aion_bytes_new(na + nb);
    memcpy(aion_bytes_data(out), a, (size_t)na);
    memcpy(aion_bytes_data(out) + na, b, (size_t)nb);
    return out;
}

bool aion_bytes_equals(const void *a, const void *b) {
    int64_t n = aion_bytes_size(a);
    return n == aion_bytes_size(b) && memcmp(a, b, (size_t)n) == 0;
}

int64_t aion_bytes_hash(const void *b) {
    uint64_t h = 0xcbf29ce484222325ull;
    for (int64_t i = 0, n = aion_bytes_size(b); i < n; i++) h = (h ^ ((const uint8_t *)b)[i]) * 0x100000001b3ull;
    return (int64_t)(h >> 1);
}

int64_t aion_bytes_index_of(const void *b, int64_t value, int64_t from) {
    int64_t n = aion_bytes_size(b);
    for (int64_t i = from < 0 ? 0 : from; i < n; i++)
        if (((const uint8_t *)b)[i] == (uint8_t)value) return i;
    return -1;
}

/* The bytes of a String (without its NUL). */
void *aion_str_to_bytes(const char *s) {
    size_t n = strlen(s);
    void *out = aion_bytes_new((int64_t)n);
    memcpy(aion_bytes_data(out), s, n);
    return out;
}

/* The bytes as text; the text ends at the first NUL byte, if there is one. */
const char *aion_bytes_to_str(const void *b) {
    return aion_string_n((const char *)b, strnlen((const char *)b, (size_t)aion_bytes_size(b)));
}

const char *aion_bytes_to_hex(const void *b) {
    static const char digits[] = "0123456789abcdef";
    int64_t n = aion_bytes_size(b);
    char *buf = malloc((size_t)n * 2 + 1);
    if (!buf) aion_panic("out of memory");
    for (int64_t i = 0; i < n; i++) {
        uint8_t v = ((const uint8_t *)b)[i];
        buf[2 * i] = digits[v >> 4];
        buf[2 * i + 1] = digits[v & 15];
    }
    const char *s = aion_string_n(buf, (size_t)n * 2);
    free(buf);
    return s;
}

/* Hex text to bytes; NULL if it is not an even number of hex digits. */
void *aion_bytes_from_hex(const char *s) {
    size_t n = strlen(s);
    if (n % 2) return NULL;
    void *out = aion_bytes_new((int64_t)(n / 2));
    for (size_t i = 0; i < n; i += 2) {
        int hi = isxdigit((unsigned char)s[i]) ? (isdigit((unsigned char)s[i]) ? s[i] - '0' : (s[i] | 32) - 'a' + 10) : -1;
        int lo = isxdigit((unsigned char)s[i + 1]) ? (isdigit((unsigned char)s[i + 1]) ? s[i + 1] - '0' : (s[i + 1] | 32) - 'a' + 10) : -1;
        if (hi < 0 || lo < 0) return NULL;
        aion_bytes_data(out)[i / 2] = (uint8_t)(hi << 4 | lo);
    }
    return out;
}
