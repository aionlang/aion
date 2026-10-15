/*
 * C half of std.zlib: deflate/inflate in the zlib, gzip and raw formats.
 * Needs the zlib development files (zlib.h and libz).
 */

#include "aion.h"

#include <stdio.h>
#include <stdlib.h>
#include <string.h>
#include <zlib.h>

static char error_text[160];

const char *aion_zlib_error(void) { return aion_string(error_text); }

/* Compress `data`.  window_bits: 15 zlib, 31 gzip, -15 raw deflate.  NULL on failure. */
void *aion_zlib_deflate(const void *data, int64_t level, int64_t window_bits) {
    z_stream z;
    memset(&z, 0, sizeof z);
    if (level < -1 || level > 9) return snprintf(error_text, sizeof error_text, "the level must be from 0 to 9"), NULL;
    if (deflateInit2(&z, (int)level, Z_DEFLATED, (int)window_bits, 8, Z_DEFAULT_STRATEGY) != Z_OK)
        return snprintf(error_text, sizeof error_text, "cannot start compressing"), NULL;
    uint64_t size = (uint64_t)aion_bytes_size(data);
    uint64_t cap = deflateBound(&z, (uLong)size) + 64;
    uint8_t *out = malloc(cap);
    if (!out) aion_panic("out of memory");
    z.next_in = (Bytef *)data;
    z.avail_in = (uInt)size;
    z.next_out = out;
    z.avail_out = (uInt)cap;
    int r = deflate(&z, Z_FINISH);
    void *result = NULL;
    if (r == Z_STREAM_END) {
        result = aion_bytes_new((int64_t)z.total_out);
        memcpy(aion_bytes_data(result), out, z.total_out);
    } else {
        snprintf(error_text, sizeof error_text, "compression failed");
    }
    deflateEnd(&z);
    free(out);
    return result;
}

/* Decompress.  window_bits: 15 zlib, 31 gzip, 47 either of those, -15 raw.  Fails (NULL) on damaged input and
 * when the result would pass `max_size` bytes (a guard against decompression bombs). */
void *aion_zlib_inflate(const void *data, int64_t window_bits, int64_t max_size) {
    z_stream z;
    memset(&z, 0, sizeof z);
    if (inflateInit2(&z, (int)window_bits) != Z_OK) return snprintf(error_text, sizeof error_text, "cannot start decompressing"), NULL;
    z.next_in = (Bytef *)data;
    z.avail_in = (uInt)aion_bytes_size(data);
    size_t cap = 65536, len = 0;
    uint8_t *out = malloc(cap);
    if (!out) aion_panic("out of memory");
    int r;
    void *result = NULL;
    for (;;) {
        if (len == cap) {
            cap *= 2;
            out = realloc(out, cap);
            if (!out) aion_panic("out of memory");
        }
        z.next_out = out + len;
        z.avail_out = (uInt)(cap - len > UINT32_MAX ? UINT32_MAX : cap - len);
        r = inflate(&z, Z_NO_FLUSH);
        len = z.total_out;
        if ((int64_t)len > max_size) {
            snprintf(error_text, sizeof error_text, "the data expands to more than %lld bytes", (long long)max_size);
            break;
        }
        if (r == Z_STREAM_END) {
            result = aion_bytes_new((int64_t)len);
            memcpy(aion_bytes_data(result), out, len);
            break;
        }
        if (r == Z_BUF_ERROR && z.avail_in == 0) {
            snprintf(error_text, sizeof error_text, "the compressed data is cut short");
            break;
        }
        if (r != Z_OK && r != Z_BUF_ERROR) {
            snprintf(error_text, sizeof error_text, "the data is not valid compressed data%s%s", z.msg ? ": " : "", z.msg ? z.msg : "");
            break;
        }
    }
    inflateEnd(&z);
    free(out);
    return result;
}

