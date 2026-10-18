/*
 * C half of std.time: clocks, calendar fields, formatting and parsing (POSIX).
 */

#define _GNU_SOURCE
#include "aion.h"

#include <stdio.h>
#include <stdlib.h>
#include <string.h>
#include <time.h>
#ifdef _WIN32
#include <windows.h>
#endif

/* Milliseconds since 1970-01-01 00:00:00 UTC. */
int64_t aion_time_now(void) {
#ifdef _WIN32
    FILETIME ft;
    GetSystemTimePreciseAsFileTime(&ft);
    uint64_t ticks = ((uint64_t)ft.dwHighDateTime << 32) | ft.dwLowDateTime; /* 100 ns since 1601 */
    return (int64_t)(ticks / 10000 - 11644473600000ull);
#else
    struct timespec ts;
    clock_gettime(CLOCK_REALTIME, &ts);
    return (int64_t)ts.tv_sec * 1000 + ts.tv_nsec / 1000000;
#endif
}

/* A clock that only goes forward, in nanoseconds from an arbitrary start; for measuring. */
int64_t aion_time_nanos(void) {
#ifdef _WIN32
    static LARGE_INTEGER frequency;
    LARGE_INTEGER now;
    if (!frequency.QuadPart) QueryPerformanceFrequency(&frequency);
    QueryPerformanceCounter(&now);
    return (int64_t)((double)now.QuadPart * 1e9 / (double)frequency.QuadPart);
#else
    struct timespec ts;
    clock_gettime(CLOCK_MONOTONIC, &ts);
    return (int64_t)ts.tv_sec * 1000000000 + ts.tv_nsec;
#endif
}

#ifdef _WIN32
/*
 * The Windows C library has no timegm, strptime, strftime for %F and %T, tm_gmtoff, nor dates before 1970 for
 * gmtime and mktime.  These are the few forms std.time uses, written out (the calendar arithmetic is Howard
 * Hinnant's civil-date algorithm).
 */
static int64_t days_from_civil(int64_t y, int m, int d) {
    y -= m <= 2;
    int64_t era = (y >= 0 ? y : y - 399) / 400;
    int64_t yoe = y - era * 400;
    int64_t doy = (153 * (m + (m > 2 ? -3 : 9)) + 2) / 5 + d - 1;
    int64_t doe = yoe * 365 + yoe / 4 - yoe / 100 + doy;
    return era * 146097 + doe - 719468;
}

static void civil_from_days(int64_t z, int64_t *y, int *m, int *d) {
    z += 719468;
    int64_t era = (z >= 0 ? z : z - 146096) / 146097;
    int64_t doe = z - era * 146097;
    int64_t yoe = (doe - doe / 1460 + doe / 36524 - doe / 146096) / 365;
    int64_t doy = doe - (365 * yoe + yoe / 4 - yoe / 100);
    int64_t mp = (5 * doy + 2) / 153;
    *d = (int)(doy - (153 * mp + 2) / 5 + 1);
    *m = (int)(mp < 10 ? mp + 3 : mp - 9);
    *y = yoe + era * 400 + (*m <= 2);
}

/* UTC calendar fields for any second count. */
static void utc_split(int64_t secs, struct tm *out) {
    int64_t days = secs >= 0 ? secs / 86400 : -((-secs + 86399) / 86400);
    int64_t rest = secs - days * 86400;
    int64_t y;
    int m, d;
    civil_from_days(days, &y, &m, &d);
    memset(out, 0, sizeof *out);
    out->tm_year = (int)(y - 1900);
    out->tm_mon = m - 1;
    out->tm_mday = d;
    out->tm_hour = (int)(rest / 3600);
    out->tm_min = (int)(rest % 3600 / 60);
    out->tm_sec = (int)(rest % 60);
    out->tm_wday = (int)(((days % 7) + 11) % 7); /* 1970-01-01 was a Thursday */
    out->tm_yday = (int)(days - days_from_civil(y, 1, 1));
}

/* Seconds since 1970 for UTC fields, which may be out of range (month 13, day 35): they carry over. */
static int64_t timegm(struct tm *tm) {
    int64_t month = tm->tm_mon, year = tm->tm_year + 1900 + (month >= 0 ? month / 12 : -((-month + 11) / 12));
    month = ((month % 12) + 12) % 12;
    int64_t days = days_from_civil(year, (int)month + 1, 1) + (tm->tm_mday - 1);
    return days * 86400 + (int64_t)tm->tm_hour * 3600 + (int64_t)tm->tm_min * 60 + tm->tm_sec;
}

/* The local zone's offset from UTC at a moment (seconds); times before 1970 use the offset of 1970. */
static long local_offset(int64_t secs) {
    time_t t = (time_t)(secs < 0 ? 0 : secs);
    struct tm local, utc;
    if (localtime_s(&local, &t) != 0 || gmtime_s(&utc, &t) != 0) return 0;
    utc.tm_isdst = local.tm_isdst;
    return (long)difftime(t, mktime(&utc));
}

static long offset_from_utc(const struct tm *local, time_t secs) {
    (void)local;
    return local_offset((int64_t)secs);
}

/* mktime for the local zone, from fields that may be out of range. */
static int64_t local_make(struct tm *tm) {
    int64_t as_utc = timegm(tm);
    long guess = local_offset(as_utc);
    long exact = local_offset(as_utc - guess);
    return as_utc - exact;
}

static const char *const month_names[] = {"january", "february", "march", "april", "may", "june", "july", "august",
                                          "september", "october", "november", "december"};
static const char *const day_names[] = {"sunday", "monday", "tuesday", "wednesday", "thursday", "friday", "saturday"};

/* Does `s` start with `name` (or its first three letters), ignoring case?  Gives the length matched, or 0. */
static int match_name(const char *s, const char *name) {
    size_t full = strlen(name);
    if (_strnicmp(s, name, full) == 0) return (int)full;
    if (_strnicmp(s, name, 3) == 0) return 3;
    return 0;
}

static const char *number(const char *s, int digits, int low, int high, int *out) {
    int v = 0, n = 0;
    while (n < digits && *s >= '0' && *s <= '9') v = v * 10 + (*s++ - '0'), n++;
    if (n == 0 || v < low || v > high) return NULL;
    *out = v;
    return s;
}

static char *strptime(const char *s, const char *fmt, struct tm *tm) {
    int pm = -1, hour12 = -1;
    for (; *fmt; fmt++) {
        if (*fmt == ' ') {
            while (*s == ' ' || *s == '\t' || *s == '\n') s++;
        } else if (*fmt != '%') {
            if (*s++ != *fmt) return NULL;
        } else {
            int v;
            switch (*++fmt) {
            case 'Y': if (!(s = number(s, 4, 0, 9999, &v))) return NULL; tm->tm_year = v - 1900; break;
            case 'y': if (!(s = number(s, 2, 0, 99, &v))) return NULL; tm->tm_year = v < 69 ? v + 100 : v; break;
            case 'm': if (!(s = number(s, 2, 1, 12, &v))) return NULL; tm->tm_mon = v - 1; break;
            case 'd': case 'e': while (*s == ' ') s++; if (!(s = number(s, 2, 1, 31, &v))) return NULL; tm->tm_mday = v; break;
            case 'H': if (!(s = number(s, 2, 0, 23, &v))) return NULL; tm->tm_hour = v; break;
            case 'I': if (!(s = number(s, 2, 1, 12, &v))) return NULL; hour12 = v; break;
            case 'M': if (!(s = number(s, 2, 0, 59, &v))) return NULL; tm->tm_min = v; break;
            case 'S': if (!(s = number(s, 2, 0, 61, &v))) return NULL; tm->tm_sec = v; break;
            case 'j': if (!(s = number(s, 3, 1, 366, &v))) return NULL; tm->tm_yday = v - 1; break;
            case 'p': case 'P':
                if (_strnicmp(s, "am", 2) == 0) pm = 0;
                else if (_strnicmp(s, "pm", 2) == 0) pm = 1;
                else return NULL;
                s += 2;
                break;
            case 'b': case 'B': case 'h': {
                int i, n = 0;
                for (i = 0; i < 12 && !(n = match_name(s, month_names[i])); i++) {}
                if (i == 12) return NULL;
                tm->tm_mon = i, s += n;
                break;
            }
            case 'a': case 'A': {
                int i, n = 0;
                for (i = 0; i < 7 && !(n = match_name(s, day_names[i])); i++) {}
                if (i == 7) return NULL;
                tm->tm_wday = i, s += n;
                break;
            }
            case '%': if (*s++ != '%') return NULL; break;
            default: return NULL;
            }
        }
    }
    if (hour12 >= 0) tm->tm_hour = hour12 % 12 + (pm == 1 ? 12 : 0);
    else if (pm == 1 && tm->tm_hour < 12) tm->tm_hour += 12;
    return (char *)s;
}
#endif

/* Split a time (ms since 1970) into the calendar fields of UTC or the local zone. */
static bool split(int64_t ms, bool utc, struct tm *out) {
    time_t secs = (time_t)(ms >= 0 ? ms / 1000 : -((-ms + 999) / 1000));
#ifdef _WIN32
    if (secs > 253402300799) return false; /* year 9999 */
    if (utc) {
        utc_split(secs, out);
    } else {
        long offset = local_offset(secs);
        utc_split(secs + offset, out);
        out->tm_isdst = 0;
        if (secs >= 0) {
            struct tm real;
            if (localtime_s(&real, &secs) == 0) *out = real;
        }
    }
    return true;
#else
    return (utc ? gmtime_r(&secs, out) : localtime_r(&secs, out)) != NULL;
#endif
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
    default:
#ifdef _WIN32
        return utc ? 0 : offset_from_utc(&tm, (time_t)(ms / 1000));
#else
        return tm.tm_gmtoff;
#endif
    }
}

#ifdef _WIN32
/* strftime with the specifiers the Windows one lacks (%e %F %T %D %R %h %n %t %u %z %k %l %P %C %s). */
static size_t format_time(char *buf, size_t cap, const char *pattern, const struct tm *tm, int64_t ms, bool utc) {
    size_t used = 0;
    char piece[128];
    for (const char *p = pattern; *p && used + 1 < cap; p++) {
        if (*p != '%' || !p[1]) {
            buf[used++] = *p;
            continue;
        }
        char spec = *++p;
        const char *text = piece;
        piece[0] = '\0';
        switch (spec) {
        case 'F': format_time(piece, sizeof piece, "%Y-%m-%d", tm, ms, utc); break;
        case 'T': format_time(piece, sizeof piece, "%H:%M:%S", tm, ms, utc); break;
        case 'D': format_time(piece, sizeof piece, "%m/%d/%y", tm, ms, utc); break;
        case 'R': format_time(piece, sizeof piece, "%H:%M", tm, ms, utc); break;
        case 'h': format_time(piece, sizeof piece, "%b", tm, ms, utc); break;
        case 'e': snprintf(piece, sizeof piece, "%2d", tm->tm_mday); break;
        case 'k': snprintf(piece, sizeof piece, "%2d", tm->tm_hour); break;
        case 'l': snprintf(piece, sizeof piece, "%2d", tm->tm_hour % 12 == 0 ? 12 : tm->tm_hour % 12); break;
        case 'u': snprintf(piece, sizeof piece, "%d", tm->tm_wday == 0 ? 7 : tm->tm_wday); break;
        case 'C': snprintf(piece, sizeof piece, "%02d", (tm->tm_year + 1900) / 100); break;
        case 'P': snprintf(piece, sizeof piece, "%s", tm->tm_hour < 12 ? "am" : "pm"); break;
        case 'n': text = "\n"; break;
        case 't': text = "\t"; break;
        case 's': snprintf(piece, sizeof piece, "%lld", (long long)(ms >= 0 ? ms / 1000 : -((-ms + 999) / 1000))); break;
        case 'z': {
            long off = utc ? 0 : local_offset(ms >= 0 ? ms / 1000 : 0);
            snprintf(piece, sizeof piece, "%c%02ld%02ld", off < 0 ? '-' : '+', labs(off) / 3600, labs(off) % 3600 / 60);
            break;
        }
        case 'Z': snprintf(piece, sizeof piece, "%s", utc ? "UTC" : "local"); break;
        case '%': text = "%"; break;
        case 'a': case 'A': case 'b': case 'B': case 'c': case 'd': case 'H': case 'I': case 'j': case 'm': case 'M':
        case 'p': case 'S': case 'U': case 'w': case 'W': case 'x': case 'X': case 'y': case 'Y': {
            char one[3] = {'%', spec, '\0'};
            if (strftime(piece, sizeof piece, one, tm) == 0) piece[0] = '\0';
            break;
        }
        default: snprintf(piece, sizeof piece, "%%%c", spec); break;
        }
        size_t n = strlen(text);
        if (used + n + 1 >= cap) break;
        memcpy(buf + used, text, n);
        used += n;
    }
    buf[used] = '\0';
    return used;
}
#endif

/* strftime: "%Y-%m-%d %H:%M:%S", "%A", "%b" ... */
const char *aion_time_format(int64_t ms, bool utc, const char *pattern) {
    struct tm tm;
    if (!split(ms, utc, &tm)) aion_panic("time is out of range");
    size_t cap = strlen(pattern) * 8 + 64;
    char *buf = malloc(cap);
    if (!buf) aion_panic("out of memory");
#ifdef _WIN32
    size_t n = format_time(buf, cap, pattern, &tm, ms, utc);
#else
    size_t n = strftime(buf, cap, pattern, &tm);
#endif
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
#ifdef _WIN32
    int64_t t = utc ? timegm(&tm) : local_make(&tm);
#else
    time_t t = utc ? timegm(&tm) : mktime(&tm);
#endif
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
#ifdef _WIN32
    int64_t t = utc ? timegm(&tm) : local_make(&tm);
#else
    time_t t = utc ? timegm(&tm) : mktime(&tm);
#endif
    return (int64_t)t * 1000;
}

bool aion_time_parse_failed(void) { return parse_failed; }
