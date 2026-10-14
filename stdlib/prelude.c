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
