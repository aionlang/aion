/*
 * C half of std.random: a xoshiro256** generator (fast, good statistics, NOT for secrets), and
 * operating-system randomness for `random.secureBytes`.
 */

#include "aion.h"

#include <stdio.h>
#include <stdlib.h>
#include <string.h>
#include <time.h>
#ifdef _WIN32
#include <windows.h>
#include <bcrypt.h>
#else
#include <unistd.h>
#endif

static uint64_t state[4];
static bool seeded;

static uint64_t splitmix(uint64_t *x) {
    uint64_t z = (*x += 0x9E3779B97F4A7C15ull);
    z = (z ^ (z >> 30)) * 0xBF58476D1CE4E5B9ull;
    z = (z ^ (z >> 27)) * 0x94D049BB133111EBull;
    return z ^ (z >> 31);
}

void aion_random_seed(int64_t seed) {
    uint64_t x = (uint64_t)seed;
    for (int i = 0; i < 4; i++) state[i] = splitmix(&x);
    seeded = true;
}

static void os_random(void *buf, size_t n) {
#ifdef _WIN32
    if (BCryptGenRandom(NULL, buf, (ULONG)n, BCRYPT_USE_SYSTEM_PREFERRED_RNG) != 0) aion_panic("cannot read the system random numbers");
#else
    FILE *f = fopen("/dev/urandom", "rb");
    if (!f || fread(buf, 1, n, f) != n) aion_panic("cannot read /dev/urandom");
    fclose(f);
#endif
}

static uint64_t rotl(uint64_t x, int k) { return (x << k) | (x >> (64 - k)); }

static uint64_t next(void) {
    if (!seeded) {
        uint64_t s;
        os_random(&s, sizeof s);
        aion_random_seed((int64_t)s);
    }
    uint64_t result = rotl(state[1] * 5, 7) * 9, t = state[1] << 17;
    state[2] ^= state[0], state[3] ^= state[1], state[1] ^= state[2], state[0] ^= state[3];
    state[2] ^= t;
    state[3] = rotl(state[3], 45);
    return result;
}

/* A number in [0, bound), without modulo bias. */
int64_t aion_random_below(int64_t bound) {
    if (bound <= 0) aion_panic("the bound must be positive");
    uint64_t b = (uint64_t)bound, limit = UINT64_MAX - UINT64_MAX % b;
    uint64_t r;
    do r = next(); while (r >= limit);
    return (int64_t)(r % b);
}

/* A number in [0, 1) with 53 random bits. */
double aion_random_float(void) { return (double)(next() >> 11) * (1.0 / 9007199254740992.0); }

/* Any Int. */
int64_t aion_random_int(void) { return (int64_t)next(); }

/* Random bytes from the operating system: unpredictable, for keys and tokens. */
void *aion_random_secure_bytes(int64_t n) {
    void *b = aion_bytes_new(n);
    os_random(aion_bytes_data(b), (size_t)n);
    return b;
}
