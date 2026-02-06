/*
 * Aion Runtime Library
 *
 * Built-in functions that the Aion compiler emits calls to.
 * This is compiled to a static library (libaion_runtime.a) and
 * linked into every Aion program.
 *
 * Extend this file to add new runtime capabilities:
 *   - aion_alloc / aion_free      (custom memory management)
 *   - aion_concat                 (string operations)
 *   - aion_panic                  (error handling)
 *   - aion_spawn / aion_yield     (concurrency primitives)
 */

#include <stdio.h>
#include <stdlib.h>
#include <string.h>

/* ── I/O ─────────────────────────────────────────────────────── */

/* Print a string followed by a newline. */
void aion_print(const char *s) {
    puts(s);
}

/* Print a 64-bit integer followed by a newline. */
void aion_print_int(long long n) {
    printf("%lld\n", n);
}

/* Print a double followed by a newline. */
void aion_print_float(double f) {
    printf("%g\n", f);
}

/* ── Process control ─────────────────────────────────────────── */

/* Abort with a message (used by the compiler for unreachable / panic). */
void aion_panic(const char *msg) {
    fprintf(stderr, "[aion] panic: %s\n", msg);
    exit(1);
}

/* ── Memory ──────────────────────────────────────────────────── */

/* Allocate `size` bytes or panic. */
void *aion_alloc(long long size) {
    void *ptr = malloc((size_t)size);
    if (!ptr) {
        aion_panic("out of memory");
    }
    return ptr;
}

/* Free memory previously allocated by aion_alloc. */
void aion_free(void *ptr) {
    free(ptr);
}
