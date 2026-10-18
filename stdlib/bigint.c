/*
 * C half of std.bigint: arbitrary-size integers (POSIX).  The arithmetic is the GMP library's, loaded at run
 * time with dlopen like OpenSSL in std.crypto: a program that never imports std.bigint needs nothing installed,
 * and no header files are required to build.
 *
 * An Aion BigInt keeps its value as a Bytes: one sign byte (0 for zero and positive, 1 for negative) followed
 * by the magnitude, most significant byte first, without leading zeros.  Every call turns its operands into GMP
 * numbers, computes, and turns the result back; the values are immutable and need no clean-up by the collector.
 */

#define _GNU_SOURCE
#include "aion.h"

#include <dlfcn.h>
#include <math.h>
#include <stdio.h>
#include <stdlib.h>
#include <string.h>

static char error_text[256];

const char *aion_bigint_error(void) { return aion_string(error_text); }

static bool fail(const char *message) {
    snprintf(error_text, sizeof error_text, "%s", message);
    return false;
}

/* GMP's mpz_t: the layout is part of its documented interface. */
typedef struct {
    int alloc;
    int size; /* negative for a negative number, zero for zero */
    void *limbs;
} Mpz;

typedef unsigned long ulong;

static struct {
    bool tried, ok;
    void (*init)(Mpz *);
    void (*clear)(Mpz *);
    void (*import)(Mpz *, size_t, int, size_t, int, size_t, const void *);
    void *(*export_)(void *, size_t *, int, size_t, int, size_t, const Mpz *);
    size_t (*sizeinbase)(const Mpz *, int);
    int (*set_str)(Mpz *, const char *, int);
    char *(*get_str)(char *, int, const Mpz *);
    void (*set_si)(Mpz *, long);
    long (*get_si)(const Mpz *);
    int (*fits_slong)(const Mpz *);
    void (*set_d)(Mpz *, double);
    double (*get_d)(const Mpz *);
    void (*add)(Mpz *, const Mpz *, const Mpz *);
    void (*sub)(Mpz *, const Mpz *, const Mpz *);
    void (*mul)(Mpz *, const Mpz *, const Mpz *);
    void (*tdiv_q)(Mpz *, const Mpz *, const Mpz *);
    void (*tdiv_r)(Mpz *, const Mpz *, const Mpz *);
    void (*fdiv_q)(Mpz *, const Mpz *, const Mpz *);
    void (*fdiv_r)(Mpz *, const Mpz *, const Mpz *);
    void (*gcd)(Mpz *, const Mpz *, const Mpz *);
    void (*lcm)(Mpz *, const Mpz *, const Mpz *);
    void (*and_)(Mpz *, const Mpz *, const Mpz *);
    void (*ior)(Mpz *, const Mpz *, const Mpz *);
    void (*xor_)(Mpz *, const Mpz *, const Mpz *);
    void (*com)(Mpz *, const Mpz *);
    void (*neg)(Mpz *, const Mpz *);
    void (*abs_)(Mpz *, const Mpz *);
    void (*pow_ui)(Mpz *, const Mpz *, ulong);
    void (*sqrt_)(Mpz *, const Mpz *);
    int (*root)(Mpz *, const Mpz *, ulong);
    void (*mul_2exp)(Mpz *, const Mpz *, ulong);
    void (*fdiv_q_2exp)(Mpz *, const Mpz *, ulong);
    void (*nextprime)(Mpz *, const Mpz *);
    void (*fac_ui)(Mpz *, ulong);
    void (*fib_ui)(Mpz *, ulong);
    void (*bin_ui)(Mpz *, const Mpz *, ulong);
    void (*powm)(Mpz *, const Mpz *, const Mpz *, const Mpz *);
    int (*invert)(Mpz *, const Mpz *, const Mpz *);
    int (*cmp)(const Mpz *, const Mpz *);
    int (*probab_prime_p)(const Mpz *, int);
} g;

static bool load(void) {
    if (g.tried) return g.ok;
    g.tried = true;
    static const char *const names[] = {"libgmp.so.10", "libgmp.so", "libgmp.10.dylib", "libgmp.dylib", NULL};
    void *lib = NULL;
    for (const char *const *n = names; *n && !lib; n++) lib = dlopen(*n, RTLD_NOW | RTLD_GLOBAL);
    if (!lib) return fail("std.bigint needs the GMP library: install libgmp (for example the libgmp10 package)");
#define LOAD(field, symbol) \
    if (!(*(void **)&g.field = dlsym(lib, symbol))) return fail("the installed GMP is missing " symbol);
    LOAD(init, "__gmpz_init") LOAD(clear, "__gmpz_clear") LOAD(import, "__gmpz_import") LOAD(export_, "__gmpz_export")
    LOAD(sizeinbase, "__gmpz_sizeinbase") LOAD(set_str, "__gmpz_set_str") LOAD(get_str, "__gmpz_get_str")
    LOAD(set_si, "__gmpz_set_si") LOAD(get_si, "__gmpz_get_si") LOAD(fits_slong, "__gmpz_fits_slong_p")
    LOAD(set_d, "__gmpz_set_d") LOAD(get_d, "__gmpz_get_d") LOAD(add, "__gmpz_add") LOAD(sub, "__gmpz_sub")
    LOAD(mul, "__gmpz_mul") LOAD(tdiv_q, "__gmpz_tdiv_q") LOAD(tdiv_r, "__gmpz_tdiv_r") LOAD(fdiv_q, "__gmpz_fdiv_q")
    LOAD(fdiv_r, "__gmpz_fdiv_r") LOAD(gcd, "__gmpz_gcd") LOAD(lcm, "__gmpz_lcm") LOAD(and_, "__gmpz_and")
    LOAD(ior, "__gmpz_ior") LOAD(xor_, "__gmpz_xor") LOAD(com, "__gmpz_com") LOAD(neg, "__gmpz_neg")
    LOAD(abs_, "__gmpz_abs") LOAD(pow_ui, "__gmpz_pow_ui") LOAD(sqrt_, "__gmpz_sqrt") LOAD(root, "__gmpz_root")
    LOAD(mul_2exp, "__gmpz_mul_2exp") LOAD(fdiv_q_2exp, "__gmpz_fdiv_q_2exp") LOAD(nextprime, "__gmpz_nextprime")
    LOAD(fac_ui, "__gmpz_fac_ui") LOAD(fib_ui, "__gmpz_fib_ui") LOAD(bin_ui, "__gmpz_bin_ui") LOAD(powm, "__gmpz_powm")
    LOAD(invert, "__gmpz_invert") LOAD(cmp, "__gmpz_cmp") LOAD(probab_prime_p, "__gmpz_probab_prime_p")
#undef LOAD
    g.ok = true;
    return true;
}

bool aion_bigint_available(void) { return load(); }

/* ── Conversion between Bytes and GMP numbers ────────────────────── */

static void read_value(Mpz *z, const void *bytes) {
    g.init(z);
    int64_t n = aion_bytes_size(bytes);
    const uint8_t *d = aion_bytes_data((void *)bytes);
    if (n > 1) g.import(z, (size_t)(n - 1), 1, 1, 0, 0, d + 1);
    if (n > 0 && d[0] == 1) g.neg(z, z);
}

static void *write_value(Mpz *z) {
    if (z->size == 0) return aion_bytes_new(1);
    int negative = z->size < 0;
    size_t bits = g.sizeinbase(z, 2), bytes = (bits + 7) / 8, count = 0;
    Mpz magnitude;
    g.init(&magnitude);
    g.abs_(&magnitude, z);
    void *out = aion_bytes_new((int64_t)bytes + 1);
    uint8_t *d = aion_bytes_data(out);
    g.export_(d + 1, &count, 1, 1, 0, 0, &magnitude);
    g.clear(&magnitude);
    d[0] = negative ? 1 : 0;
    return out;
}

/* Binary operations: 0 add, 1 subtract, 2 multiply, 3 truncated quotient, 4 truncated remainder, 5 floor quotient,
 * 6 floor remainder (modulo), 7 gcd, 8 lcm, 9 and, 10 or, 11 xor.  NULL for a division by zero. */
void *aion_bigint_binary(int64_t op, const void *a, const void *b) {
    if (!load()) return NULL;
    Mpz x, y, r;
    read_value(&x, a);
    read_value(&y, b);
    g.init(&r);
    void *out = NULL;
    if (op >= 3 && op <= 6 && y.size == 0) {
        fail("division by zero");
    } else {
        switch (op) {
        case 0: g.add(&r, &x, &y); break;
        case 1: g.sub(&r, &x, &y); break;
        case 2: g.mul(&r, &x, &y); break;
        case 3: g.tdiv_q(&r, &x, &y); break;
        case 4: g.tdiv_r(&r, &x, &y); break;
        case 5: g.fdiv_q(&r, &x, &y); break;
        case 6: g.fdiv_r(&r, &x, &y); break;
        case 7: g.gcd(&r, &x, &y); break;
        case 8: g.lcm(&r, &x, &y); break;
        case 9: g.and_(&r, &x, &y); break;
        case 10: g.ior(&r, &x, &y); break;
        default: g.xor_(&r, &x, &y); break;
        }
        out = write_value(&r);
    }
    g.clear(&x);
    g.clear(&y);
    g.clear(&r);
    return out;
}

/* Operations with a number argument `n`: 0 negate, 1 absolute value, 2 power, 3 square root (rounded down; NULL for a
 * negative number), 4 shift left, 5 shift right (rounding down), 6 next prime, 7 complement (-x - 1), 8 root of
 * degree n (NULL if even and negative), 9 binomial coefficient (x choose n). */
void *aion_bigint_unary(int64_t op, const void *a, int64_t n) {
    if (!load()) return NULL;
    Mpz x, r;
    read_value(&x, a);
    g.init(&r);
    void *out = NULL;
    bool ok = true;
    if ((op == 2 || op == 4 || op == 5 || op == 8 || op == 9) && n < 0) ok = fail("the number must not be negative");
    else if (op == 3 && x.size < 0) ok = fail("the square root of a negative number");
    else if (op == 8 && (n == 0 || (x.size < 0 && n % 2 == 0))) ok = fail("no such root");
    else if (op == 2 && n > 0 && g.sizeinbase(&x, 2) > 1 && (uint64_t)n * g.sizeinbase(&x, 2) > ((uint64_t)1 << 34)) ok = fail("the result would be too large");
    else if (op == 4 && n > ((int64_t)1 << 34)) ok = fail("the result would be too large");
    if (ok) {
        switch (op) {
        case 0: g.neg(&r, &x); break;
        case 1: g.abs_(&r, &x); break;
        case 2: g.pow_ui(&r, &x, (ulong)n); break;
        case 3: g.sqrt_(&r, &x); break;
        case 4: g.mul_2exp(&r, &x, (ulong)n); break;
        case 5: g.fdiv_q_2exp(&r, &x, (ulong)n); break;
        case 6: g.nextprime(&r, &x); break;
        case 7: g.com(&r, &x); break;
        case 8: g.root(&r, &x, (ulong)n); break;
        default: g.bin_ui(&r, &x, (ulong)n); break;
        }
        out = write_value(&r);
    }
    g.clear(&x);
    g.clear(&r);
    return out;
}

/* n!, or the n-th Fibonacci number (`fibonacci`). */
void *aion_bigint_sequence(bool fibonacci, int64_t n) {
    if (!load()) return NULL;
    if (n < 0) return fail("the number must not be negative"), NULL;
    if (n > 20000000) return fail("the result would be too large"), NULL;
    Mpz r;
    g.init(&r);
    if (fibonacci) g.fib_ui(&r, (ulong)n);
    else g.fac_ui(&r, (ulong)n);
    void *out = write_value(&r);
    g.clear(&r);
    return out;
}

/* base ^ exponent mod modulus; NULL when the modulus is zero or a negative exponent has no inverse. */
void *aion_bigint_modpow(const void *base, const void *exponent, const void *modulus) {
    if (!load()) return NULL;
    Mpz b, e, m, r;
    read_value(&b, base);
    read_value(&e, exponent);
    read_value(&m, modulus);
    g.init(&r);
    void *out = NULL;
    if (m.size == 0) fail("the modulus is zero");
    else {
        /* mpz_powm with a negative exponent needs the base to be invertible; GMP reports it by returning 0 */
        if (e.size < 0) {
            Mpz inv;
            g.init(&inv);
            if (!g.invert(&inv, &b, &m)) {
                fail("the base has no inverse for this modulus");
                g.clear(&inv);
                goto done;
            }
            Mpz positive;
            g.init(&positive);
            g.neg(&positive, &e);
            g.powm(&r, &inv, &positive, &m);
            g.clear(&inv);
            g.clear(&positive);
        } else {
            g.powm(&r, &b, &e, &m);
        }
        out = write_value(&r);
    }
done:
    g.clear(&b);
    g.clear(&e);
    g.clear(&m);
    g.clear(&r);
    return out;
}

/* The inverse of a modulo m (the x with a*x = 1 mod m), or NULL if there is none. */
void *aion_bigint_invert(const void *a, const void *modulus) {
    if (!load()) return NULL;
    Mpz x, m, r;
    read_value(&x, a);
    read_value(&m, modulus);
    g.init(&r);
    void *out = NULL;
    if (m.size == 0) fail("the modulus is zero");
    else if (!g.invert(&r, &x, &m)) fail("the number has no inverse for this modulus");
    else out = write_value(&r);
    g.clear(&x);
    g.clear(&m);
    g.clear(&r);
    return out;
}

int64_t aion_bigint_compare(const void *a, const void *b) {
    if (!load()) return 0;
    Mpz x, y;
    read_value(&x, a);
    read_value(&y, b);
    int c = g.cmp(&x, &y);
    g.clear(&x);
    g.clear(&y);
    return c < 0 ? -1 : c > 0 ? 1 : 0;
}

/* 0 composite, 1 probably prime, 2 certainly prime. */
int64_t aion_bigint_prime(const void *a, int64_t rounds) {
    if (!load()) return 0;
    Mpz x;
    read_value(&x, a);
    int r = g.probab_prime_p(&x, (int)(rounds < 1 ? 1 : rounds > 1000 ? 1000 : rounds));
    g.clear(&x);
    return r;
}

int64_t aion_bigint_bit_length(const void *a) {
    if (!load()) return 0;
    Mpz x;
    read_value(&x, a);
    int64_t bits = x.size == 0 ? 0 : (int64_t)g.sizeinbase(&x, 2);
    g.clear(&x);
    return bits;
}

/* The number as text in a base from 2 to 36 (lower case digits), with a minus sign if negative. */
const char *aion_bigint_to_string(const void *a, int64_t base) {
    if (!load()) return aion_string("");
    if (base < 2 || base > 36) base = 10;
    Mpz x;
    read_value(&x, a);
    char *text = malloc(g.sizeinbase(&x, (int)base) + 3);
    const char *result = "";
    if (text) {
        g.get_str(text, (int)base, &x);
        result = aion_string(text);
        free(text);
    }
    g.clear(&x);
    return result;
}

/* Parse text in a base from 2 to 36 (an optional sign, then digits; no spaces); NULL if it is not a number. */
void *aion_bigint_from_string(const char *text, int64_t base) {
    if (!load()) return NULL;
    if (base < 2 || base > 36) return fail("the base must be from 2 to 36"), NULL;
    if (!*text) return fail("there are no digits"), NULL;
    Mpz x;
    g.init(&x);
    void *out = NULL;
    if (g.set_str(&x, text, (int)base) != 0) fail("not a number");
    else out = write_value(&x);
    g.clear(&x);
    return out;
}

void *aion_bigint_from_int(int64_t n) {
    if (!load()) return NULL;
    Mpz x;
    g.init(&x);
    g.set_si(&x, (long)n);
    void *out = write_value(&x);
    g.clear(&x);
    return out;
}

void *aion_bigint_from_float(double d) {
    if (!load()) return NULL;
    if (isnan(d) || isinf(d)) return fail("not a finite number"), NULL;
    Mpz x;
    g.init(&x);
    g.set_d(&x, d); /* truncates toward zero */
    void *out = write_value(&x);
    g.clear(&x);
    return out;
}

/* Does the number fit an Int? */
bool aion_bigint_fits(const void *a) {
    if (!load()) return false;
    Mpz x;
    read_value(&x, a);
    bool fits = g.fits_slong(&x) != 0;
    g.clear(&x);
    return fits;
}

int64_t aion_bigint_to_int(const void *a) {
    if (!load()) return 0;
    Mpz x;
    read_value(&x, a);
    int64_t n = (int64_t)g.get_si(&x);
    g.clear(&x);
    return n;
}

double aion_bigint_to_float(const void *a) {
    if (!load()) return 0;
    Mpz x;
    read_value(&x, a);
    double d = g.get_d(&x);
    g.clear(&x);
    return d;
}
