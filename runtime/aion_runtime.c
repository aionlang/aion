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

/* Print a string (no trailing newline). */
void aion_print(const char *s) {
    printf("%s", s);
}

/* Print a 64-bit integer (no trailing newline). */
void aion_print_int(long long n) {
    printf("%lld", n);
}

/* Print a double (no trailing newline). */
void aion_print_float(double f) {
    printf("%g", f);
}

/* Print a string followed by a newline. */
void aion_println(const char *s) {
    printf("%s\n", s);
}

/* Print a 64-bit integer followed by a newline. */
void aion_println_int(long long n) {
    printf("%lld\n", n);
}

/* Print a double followed by a newline. */
void aion_println_float(double f) {
    printf("%g\n", f);
}

/* Concatenate two strings. Returns a heap-allocated string. */
char* aion_concat(const char* a, const char* b) {
    size_t len = strlen(a) + strlen(b);
    char* res = (char*)malloc(len + 1);
    if (!res) {
        fprintf(stderr, "Out of memory in aion_concat\\n");
        exit(1);
    }
    strcpy(res, a);
    strcat(res, b);
    return res;
}

/* Convert int to string. Returns a heap-allocated string. */
char* aion_int_to_str(long long n) {
    // 20 chars enough for 64-bit int (approx 19 digits) + sign + null
    char buffer[32]; 
    snprintf(buffer, sizeof(buffer), "%lld", n);
    size_t len = strlen(buffer);
    char* res = (char*)malloc(len + 1);
    if (!res) {
        fprintf(stderr, "Out of memory in aion_int_to_str\\n");
        exit(1);
    }
    strcpy(res, buffer);
    return res;
}

/* Convert float to string. Returns a heap-allocated string. */
char* aion_float_to_str(double f) {
    char buffer[64];
    snprintf(buffer, sizeof(buffer), "%g", f);
    size_t len = strlen(buffer);
    char* res = (char*)malloc(len + 1);
    if (!res) {
        fprintf(stderr, "Out of memory in aion_float_to_str\\n");
        exit(1);
    }
    strcpy(res, buffer);
    return res;
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
