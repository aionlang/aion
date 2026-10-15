/*
 * C half of std.encoding: Base64 and percent-encoding.
 */

#include "aion.h"

#include <stdlib.h>
#include <string.h>

static const char STANDARD[] = "ABCDEFGHIJKLMNOPQRSTUVWXYZabcdefghijklmnopqrstuvwxyz0123456789+/";
static const char URLSAFE[] = "ABCDEFGHIJKLMNOPQRSTUVWXYZabcdefghijklmnopqrstuvwxyz0123456789-_";

/* Base64 text; `url` picks the URL-safe alphabet (- and _), `pad` appends = to a multiple of 4. */
const char *aion_enc_base64_encode(const void *data, bool url, bool pad) {
    const uint8_t *in = data;
    size_t n = (size_t)aion_bytes_size(data);
    const char *alphabet = url ? URLSAFE : STANDARD;
    char *out = malloc((n + 2) / 3 * 4 + 1), *at = out;
    if (!out) aion_panic("out of memory");
    size_t i = 0;
    for (; i + 2 < n; i += 3) {
        uint32_t v = (uint32_t)in[i] << 16 | (uint32_t)in[i + 1] << 8 | in[i + 2];
        *at++ = alphabet[v >> 18], *at++ = alphabet[v >> 12 & 63], *at++ = alphabet[v >> 6 & 63], *at++ = alphabet[v & 63];
    }
    if (n - i == 1) {
        uint32_t v = (uint32_t)in[i] << 16;
        *at++ = alphabet[v >> 18], *at++ = alphabet[v >> 12 & 63];
        if (pad) *at++ = '=', *at++ = '=';
    } else if (n - i == 2) {
        uint32_t v = (uint32_t)in[i] << 16 | (uint32_t)in[i + 1] << 8;
        *at++ = alphabet[v >> 18], *at++ = alphabet[v >> 12 & 63], *at++ = alphabet[v >> 6 & 63];
        if (pad) *at++ = '=';
    }
    const char *s = aion_string_n(out, (size_t)(at - out));
    free(out);
    return s;
}

/* The bytes a Base64 text stands for; NULL if it is not valid.  Either alphabet is accepted, padding is
 * optional, and white space is ignored. */
void *aion_enc_base64_decode(const char *text) {
    size_t n = strlen(text);
    uint8_t *out = malloc(n / 4 * 3 + 3), *at = out;
    if (!out) aion_panic("out of memory");
    uint32_t acc = 0;
    int bits = 0, seen = 0;
    bool padding = false, ok = true;
    for (size_t i = 0; i < n && ok; i++) {
        unsigned char c = (unsigned char)text[i];
        int v;
        if (c >= 'A' && c <= 'Z') v = c - 'A';
        else if (c >= 'a' && c <= 'z') v = c - 'a' + 26;
        else if (c >= '0' && c <= '9') v = c - '0' + 52;
        else if (c == '+' || c == '-') v = 62;
        else if (c == '/' || c == '_') v = 63;
        else if (c == '=') { padding = true; seen++; continue; }
        else if (c == ' ' || c == '\n' || c == '\r' || c == '\t') continue;
        else { ok = false; break; }
        if (padding) { ok = false; break; } /* data after = */
        acc = acc << 6 | (uint32_t)v;
        seen++;
        bits += 6;
        if (bits >= 8) bits -= 8, *at++ = (uint8_t)(acc >> bits), acc &= (1u << bits) - 1;
    }
    /* A lone leftover sextet cannot be a byte; leftover bits must be zero; padding must complete a group of 4. */
    if (ok && (bits >= 6 || acc != 0 || (padding && seen % 4 != 0))) ok = false;
    void *result = NULL;
    if (ok) {
        result = aion_bytes_new((int64_t)(at - out));
        memcpy(aion_bytes_data(result), out, (size_t)(at - out));
    }
    free(out);
    return result;
}

/* Percent-encode everything except letters, digits and - . _ ~ (and "/" when `keep_slash`). */
const char *aion_enc_url_encode(const char *text, bool keep_slash) {
    static const char hex[] = "0123456789ABCDEF";
    size_t n = strlen(text);
    char *out = malloc(n * 3 + 1), *at = out;
    if (!out) aion_panic("out of memory");
    for (size_t i = 0; i < n; i++) {
        unsigned char c = (unsigned char)text[i];
        bool plain = (c >= 'A' && c <= 'Z') || (c >= 'a' && c <= 'z') || (c >= '0' && c <= '9') || c == '-' ||
                     c == '.' || c == '_' || c == '~' || (keep_slash && c == '/');
        if (plain) *at++ = (char)c;
        else *at++ = '%', *at++ = hex[c >> 4], *at++ = hex[c & 15];
    }
    const char *s = aion_string_n(out, (size_t)(at - out));
    free(out);
    return s;
}

static int hex_value(char c) {
    if (c >= '0' && c <= '9') return c - '0';
    if (c >= 'a' && c <= 'f') return c - 'a' + 10;
    if (c >= 'A' && c <= 'F') return c - 'A' + 10;
    return -1;
}

/* Undo percent-encoding (and, with `plus_is_space`, turn + into a space); NULL for a bad %xx. */
const char *aion_enc_url_decode(const char *text, bool plus_is_space) {
    size_t n = strlen(text);
    char *out = malloc(n + 1), *at = out;
    if (!out) aion_panic("out of memory");
    for (size_t i = 0; i < n; i++) {
        if (text[i] == '%') {
            if (i + 2 >= n) { free(out); return NULL; }
            int a = hex_value(text[i + 1]), b = hex_value(text[i + 2]);
            if (a < 0 || b < 0) { free(out); return NULL; }
            if (a == 0 && b == 0) { free(out); return NULL; } /* a String cannot hold a zero byte */
            *at++ = (char)(a << 4 | b);
            i += 2;
        } else if (text[i] == '+' && plus_is_space) {
            *at++ = ' ';
        } else {
            *at++ = text[i];
        }
    }
    const char *s = aion_string_n(out, (size_t)(at - out));
    free(out);
    return s;
}
