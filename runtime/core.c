/*
 * Aion core runtime: garbage collector, strings and printing.
 *
 * The collector is a precise, non-moving mark-sweep.  Roots are the slots of
 * every frame on the `aion_frame_top` chain (see aion.h); objects are traced
 * through their `ptr_mask`.  Collection only ever happens inside an
 * allocation, and compiled code keeps every live pointer in a frame slot or a
 * global variable, so no live value can be missed.
 */

#include "aion.h"

#include <inttypes.h>
#include <stdio.h>
#include <stdlib.h>
#include <string.h>

AionFrame *aion_frame_top = NULL;

/* Module-level variables holding pointers: the compiler emits a table of their
 * addresses into every program (possibly empty). */
extern void **const aion_roots[];
extern const int64_t aion_roots_len;

#define MIN_THRESHOLD ((size_t)4 << 20)

static AionHeader *heap = NULL;      /* every collectable object        */
static size_t heap_bytes = 0;        /* bytes currently allocated       */
static size_t threshold = MIN_THRESHOLD;

#define HEADER_OF(p) ((AionHeader *)(p) - 1)

/* ── Panics ───────────────────────────────────────────────────── */

void aion_panic(const char *msg) {
    fflush(stdout);
    fprintf(stderr, "[aion] panic: %s\n", msg);
    exit(1);
}

void aion_panic_null(void) { aion_panic("null reference"); }
void aion_panic_div_zero(void) { aion_panic("division by zero"); }

/* ── Mark & sweep ─────────────────────────────────────────────── */

/* AION_GC_STRESS=1 collects on every allocation and poisons freed memory: any
 * value the compiler forgot to keep reachable is destroyed at once, which
 * makes such bugs loud instead of intermittent. */
static int stress_mode(void) {
    static int cached = -1;
    if (cached < 0) cached = getenv("AION_GC_STRESS") != NULL;
    return cached;
}

static AionHeader **mark_stack = NULL;
static size_t mark_len = 0, mark_cap = 0;

static void mark(void *p) {
    if (!p) return;
    AionHeader *h = HEADER_OF(p);
    if (h->flags & (AION_F_MARK | AION_F_STATIC)) return;
    h->flags |= AION_F_MARK;
    if (!(h->flags & AION_F_SCAN)) return;
    if (mark_len == mark_cap) {
        mark_cap = mark_cap ? mark_cap * 2 : 1024;
        mark_stack = realloc(mark_stack, mark_cap * sizeof *mark_stack);
        if (!mark_stack) { fputs("[aion] out of memory\n", stderr); exit(1); }
    }
    mark_stack[mark_len++] = h;
}

/* Trace children iteratively so long linked structures can't overflow the stack. */
static void drain(void) {
    while (mark_len) {
        AionHeader *h = mark_stack[--mark_len];
        void **slots = (void **)(h + 1);
        for (uint64_t m = h->ptr_mask, i = 0; m; m >>= 1, i++)
            if (m & 1) mark(slots[i]);
    }
}

void aion_gc_collect(void) {
    for (int64_t i = 0; i < aion_roots_len; i++) mark(*aion_roots[i]);
    for (AionFrame *f = aion_frame_top; f; f = f->prev)
        for (uint64_t i = 0; i < f->n; i++) mark(f->slots[i]);
    drain();

    size_t live = 0;
    AionHeader **link = &heap;
    while (*link) {
        AionHeader *h = *link;
        if (h->flags & AION_F_MARK) {
            h->flags &= ~AION_F_MARK;
            live += sizeof *h + h->size;
            link = &h->next;
        } else {
            *link = h->next;
            if (stress_mode()) memset(h, 0xDD, sizeof *h + h->size); /* make use-after-free loud */
            free(h);
        }
    }
    heap_bytes = live;
    threshold = live * 2 > MIN_THRESHOLD ? live * 2 : MIN_THRESHOLD;
}

static void *gc_alloc(size_t size, uint32_t flags, uint64_t ptr_mask) {
    if (size > UINT32_MAX) aion_panic("allocation too large");
    if (stress_mode() || heap_bytes + sizeof(AionHeader) + size > threshold) aion_gc_collect();

    AionHeader *h = calloc(1, sizeof *h + size);
    if (!h) {
        aion_gc_collect();
        h = calloc(1, sizeof *h + size);
        if (!h) aion_panic("out of memory");
    }
    h->size = (uint32_t)size;
    h->flags = flags;
    h->ptr_mask = ptr_mask;
    h->next = heap;
    heap = h;
    heap_bytes += sizeof *h + size;
    return h + 1;
}

/* Statistics, exposed to Aion through stdlib/gc.aion. */
int64_t aion_gc_heap_size(void) { return (int64_t)heap_bytes; }
int64_t aion_gc_object_count(void) {
    int64_t n = 0;
    for (AionHeader *h = heap; h; h = h->next) n++;
    return n;
}

/* ── Objects & strings ────────────────────────────────────────── */

void *aion_new_object(int64_t nslots, int64_t ptr_mask) {
    return gc_alloc((size_t)nslots * 8, AION_F_SCAN, (uint64_t)ptr_mask);
}

const char *aion_string_n(const char *s, size_t n) {
    char *r = gc_alloc(n + 1, 0, 0);
    memcpy(r, s, n);
    return r;
}

const char *aion_string(const char *s) { return aion_string_n(s, strlen(s)); }

const char *aion_concat(const char *a, const char *b) {
    size_t la = strlen(a), lb = strlen(b);
    char *r = gc_alloc(la + lb + 1, 0, 0);
    memcpy(r, a, la);
    memcpy(r + la, b, lb);
    return r;
}

int64_t aion_str_eq(const char *a, const char *b) { return strcmp(a, b) == 0; }

const char *aion_int_to_str(int64_t n) {
    char buf[32];
    snprintf(buf, sizeof buf, "%" PRId64, n);
    return aion_string(buf);
}

const char *aion_float_to_str(double f) {
    char buf[64];
    snprintf(buf, sizeof buf, "%g", f);
    return aion_string(buf);
}

const char *aion_bool_to_str(int64_t b) { return aion_string(b ? "true" : "false"); }

/* ── Printing ─────────────────────────────────────────────────── */

static void end_line(int64_t newline) {
    if (newline) putchar('\n');
}

void aion_print_str(const char *s, int64_t nl) { fputs(s, stdout); end_line(nl); }
void aion_print_int(int64_t n, int64_t nl) { printf("%" PRId64, n); end_line(nl); }
void aion_print_float(double f, int64_t nl) { printf("%g", f); end_line(nl); }
void aion_print_bool(int64_t b, int64_t nl) { fputs(b ? "true" : "false", stdout); end_line(nl); }
