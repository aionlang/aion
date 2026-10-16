/*
 * C code that an Aion program can call.  Every function is declared on the Aion side with `extern fn`
 * (see lib/native.aion); "aion.h" provides the helpers for passing text and bytes across.
 *
 * Aion type   C type
 * Int         int64_t        Float   double       Bool   bool
 * String      const char *   Bytes   void * (aion_bytes_size / aion_bytes_data)
 */

#include "aion.h"

#include <math.h>
#include <stdlib.h>
#include <string.h>

/* How many bits are set. */
int64_t bits_popcount(int64_t n) { return __builtin_popcountll((uint64_t)n); }

/* Primality by trial division - fast enough in C to be worth calling for big ranges. */
bool bits_is_prime(int64_t n) {
    if (n < 2) return false;
    if (n % 2 == 0) return n == 2;
    for (int64_t d = 3; d * d <= n; d += 2)
        if (n % d == 0) return false;
    return true;
}

int64_t bits_count_primes(int64_t limit) {
    int64_t count = 0;
    for (int64_t n = 2; n <= limit; n++) count += bits_is_prime(n);
    return count;
}

/* Reverse a UTF-8 string character by character.  Text returned to Aion must be made with aion_string*(),
 * so that the garbage collector owns it. */
const char *bits_reverse_text(const char *s) {
    size_t n = strlen(s);
    char *out = malloc(n + 1);
    if (!out) aion_panic("out of memory");
    size_t at = n;
    for (size_t i = 0; i < n;) {
        size_t len = 1;
        unsigned char c = (unsigned char)s[i];
        if (c >= 0xF0) len = 4;
        else if (c >= 0xE0) len = 3;
        else if (c >= 0xC0) len = 2;
        if (i + len > n) len = n - i;
        at -= len;
        memcpy(out + at, s + i, len);
        i += len;
    }
    out[n] = '\0';
    const char *result = aion_string_n(out, n);
    free(out);
    return result;
}

/* Adler-32 of some bytes: the checksum zlib uses. */
int64_t bits_adler32(const void *bytes) {
    const uint8_t *p = aion_bytes_data((void *)bytes);
    int64_t n = aion_bytes_size(bytes);
    uint32_t a = 1, b = 0;
    for (int64_t i = 0; i < n; i++) {
        a = (a + p[i]) % 65521;
        b = (b + a) % 65521;
    }
    return (int64_t)((b << 16) | a);
}

/* A new Bytes with every byte XORed with the key bytes (repeating): applying it twice gives the original. */
void *bits_xor(const void *data, const void *key) {
    int64_t n = aion_bytes_size(data), k = aion_bytes_size(key);
    void *out = aion_bytes_new(n);
    const uint8_t *in = aion_bytes_data((void *)data), *kp = aion_bytes_data((void *)key);
    uint8_t *dst = aion_bytes_data(out);
    for (int64_t i = 0; i < n; i++) dst[i] = in[i] ^ (k ? kp[i % k] : 0);
    return out;
}

/* The length of a 3-D vector, with libm's sqrt. */
double bits_length3(double x, double y, double z) { return sqrt(x * x + y * y + z * z); }
