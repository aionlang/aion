/*
 * C half of std.time: clocks, calendar fields, formatting and parsing (POSIX).
 */

#define _GNU_SOURCE
#include "aion.h"

#include <stdio.h>
#include <stdlib.h>
#include <string.h>
#include <time.h>

/* Milliseconds since 1970-01-01 00:00:00 UTC. */
int64_t aion_time_now(void) {
    struct timespec ts;
    clock_gettime(CLOCK_REALTIME, &ts);
    return (int64_t)ts.tv_sec * 1000 + ts.tv_nsec / 1000000;
}

/* A clock that only goes forward, in nanoseconds from an arbitrary start; for measuring. */
int64_t aion_time_nanos(void) {
    struct timespec ts;
    clock_gettime(CLOCK_MONOTONIC, &ts);
    return (int64_t)ts.tv_sec * 1000000000 + ts.tv_nsec;
}

/* Split a time (ms since 1970) into the calendar fields of UTC or the local zone. */
static bool split(int64_t ms, bool utc, struct tm *out) {
    time_t secs = (time_t)(ms >= 0 ? ms / 1000 : -((-ms + 999) / 1000));
    return (utc ? gmtime_r(&secs, out) : localtime_r(&secs, out)) != NULL;
}

/* A calendar field: 0 year, 1 month (1-12), 2 day, 3 hour, 4 minute, 5 second,
 * 6 weekday (0 = Sunday), 7 day of the year (1-366), 8 offset from UTC in seconds. */
int64_t aion_time_field(int64_t ms, bool utc, int64_t which) {
    struct tm tm;
    if (!split(ms, utc, &tm)) aion_panic("time is out of range");
    switch (which) {
    case 0: return tm.tm_year + 1900;
    case 1: return tm.tm_mon + 1;
    case 2: return tm.tm_mday;
    case 3: return tm.tm_hour;
    case 4: return tm.tm_min;
    case 5: return tm.tm_sec;
    case 6: return tm.tm_wday;
    case 7: return tm.tm_yday + 1;
    default: return tm.tm_gmtoff;
    }
}

/* strftime: "%Y-%m-%d %H:%M:%S", "%A", "%b" ... */
const char *aion_time_format(int64_t ms, bool utc, const char *pattern) {
    struct tm tm;
    if (!split(ms, utc, &tm)) aion_panic("time is out of range");
    size_t cap = strlen(pattern) * 8 + 64;
    char *buf = malloc(cap);
    if (!buf) aion_panic("out of memory");
    size_t n = strftime(buf, cap, pattern, &tm);
    const char *out = aion_string_n(buf, n);
    free(buf);
    return out;
}

/* The time for calendar fields (out-of-range fields carry over: month 13 is January of next year),
 * as ms since 1970. */
int64_t aion_time_make(int64_t year, int64_t month, int64_t day, int64_t hour, int64_t minute, int64_t second,
                       bool utc) {
    struct tm tm;
    memset(&tm, 0, sizeof tm);
    tm.tm_year = (int)(year - 1900);
    tm.tm_mon = (int)(month - 1);
    tm.tm_mday = (int)day;
    tm.tm_hour = (int)hour;
    tm.tm_min = (int)minute;
    tm.tm_sec = (int)second;
    tm.tm_isdst = -1;
    time_t t = utc ? timegm(&tm) : mktime(&tm);
    return (int64_t)t * 1000;
}

static bool parse_failed;

/* strptime: the time the text spells in the given pattern, as ms since 1970. */
int64_t aion_time_parse(const char *text, const char *pattern, bool utc) {
    struct tm tm;
    memset(&tm, 0, sizeof tm);
    tm.tm_mday = 1;
    tm.tm_isdst = -1;
    const char *end = strptime(text, pattern, &tm);
    parse_failed = end == NULL || *end != '\0';
    if (parse_failed) return 0;
    time_t t = utc ? timegm(&tm) : mktime(&tm);
    return (int64_t)t * 1000;
}

bool aion_time_parse_failed(void) { return parse_failed; }
