#include "aion.h"

#include <ctype.h>
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
