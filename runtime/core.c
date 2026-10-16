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
#include <math.h>
#include <setjmp.h>
#include <stdio.h>
#include <stdlib.h>
#include <string.h>

AionFrame *aion_frame_top = NULL;
AionTrace *aion_trace_top = NULL;

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

/* ── Closures, defer and try ──────────────────────────────────── */

/* A function value is a GC object whose first slot is the code pointer; the
 * object itself is passed to the code as a hidden first argument. */
static void call0(void *closure) { (*(void (**)(void *))closure)(closure); }
static void call1(void *closure, const char *arg) { (*(void (**)(void *, const char *))closure)(closure, arg); }

/* `defer`red closures, innermost last.  They are GC roots. */
static void **defer_stack = NULL;
static size_t defer_len = 0, defer_cap = 0;

void aion_defer_push(void *closure) {
    if (defer_len == defer_cap) {
        defer_cap = defer_cap ? defer_cap * 2 : 64;
        defer_stack = realloc(defer_stack, defer_cap * sizeof *defer_stack);
        if (!defer_stack) { fputs("[aion] out of memory\n", stderr); exit(1); }
    }
    defer_stack[defer_len++] = closure;
}

int64_t aion_defer_mark(void) { return (int64_t)defer_len; }

/* Run deferred closures, newest first, until only `mark` remain. */
void aion_defer_run(int64_t mark) {
    while (defer_len > (size_t)mark) {
        void *closure = defer_stack[--defer_len];
        call0(closure);
    }
}

/* One per active `try`: where to jump back to when something panics. */
typedef struct TryContext {
    struct TryContext *prev;
    jmp_buf jump;
    AionFrame *frame; /* shadow stack to restore */
    AionTrace *trace; /* stack trace to restore */
    size_t defers;    /* deferred closures to run before jumping */
} TryContext;

static TryContext *try_top = NULL;

/* Switching tasks swaps these (see tasks.c). */
void aion_task_save(AionTaskState *s) {
    s->frame = aion_frame_top;
    s->trace = aion_trace_top;
    s->try_top = try_top;
    s->defers = defer_stack;
    s->defer_len = defer_len;
    s->defer_cap = defer_cap;
}

void aion_task_load(const AionTaskState *s) {
    aion_frame_top = s->frame;
    aion_trace_top = s->trace;
    try_top = s->try_top;
    defer_stack = s->defers;
    defer_len = s->defer_len;
    defer_cap = s->defer_cap;
}
static char panic_message[512];
static int fatal_panic = 0;

/* The functions that were running, innermost first.  A long chain (deep recursion) shows its ends. */
static void print_trace(void) {
    size_t n = 0;
    for (AionTrace *t = aion_trace_top; t; t = t->prev) n++;
    size_t i = 0;
    for (AionTrace *t = aion_trace_top; t; t = t->prev, i++) {
        if (n > 20 && i == 12) {
            fprintf(stderr, "    ... %zu more calls ...\n", n - 15);
        }
        if (n > 20 && i >= 12 && i < n - 3) continue;
        fprintf(stderr, "    at %s (%s:%lld)\n", t->info->name, t->info->file, (long long)t->line);
    }
}

void aion_panic(const char *msg) {
    if (try_top) {
        TryContext *ctx = try_top;
        snprintf(panic_message, sizeof panic_message, "%s", msg);
        try_top = ctx->prev; /* a panic inside a deferred closure goes to the next handler */
        aion_defer_run((int64_t)ctx->defers);
        longjmp(ctx->jump, 1);
    }
    if (!fatal_panic) { /* run the cleanups, once, before dying */
        fatal_panic = 1;
        aion_defer_run(0);
    }
    fflush(stdout);
    fprintf(stderr, "[aion] panic: %s\n", msg);
    print_trace();
    exit(1);
}

/* try { body } catch (message) { handler }: both are closures.  One entry point per
 * kind of result the closures return (the compiler picks the one that matches). */
#define DEFINE_TRY(NAME, RET, CODE_RET, FIX)                                                      \
    RET NAME(void *body, void *handler) {                                                         \
        TryContext ctx;                                                                           \
        ctx.prev = try_top;                                                                       \
        ctx.frame = aion_frame_top;                                                               \
        ctx.trace = aion_trace_top;                                                               \
        ctx.defers = defer_len;                                                                   \
        CODE_RET raw;                                                                             \
        try_top = &ctx;                                                                           \
        if (setjmp(ctx.jump) == 0) {                                                              \
            raw = (*(CODE_RET (**)(void *))body)(body);                                           \
            try_top = ctx.prev;                                                                   \
        } else {                                                                                  \
            aion_frame_top = ctx.frame;                                                           \
            aion_trace_top = ctx.trace;                                                           \
            raw = (*(CODE_RET (**)(void *, const char *))handler)(handler, aion_string(panic_message)); \
        }                                                                                         \
        return FIX(raw);                                                                          \
    }

#define KEEP(x) (x)
#define AS_BOOL(x) ((bool)((x) & 1)) /* the upper bits of an i1 return are unspecified */
DEFINE_TRY(aion_try_word, int64_t, int64_t, KEEP)
DEFINE_TRY(aion_try_float, double, double, KEEP)
DEFINE_TRY(aion_try_ptr, const char *, const char *, KEEP)
DEFINE_TRY(aion_try_bool, bool, uint8_t, AS_BOOL)

void aion_try(void *body, void *handler) {
    TryContext ctx;
    ctx.prev = try_top;
    ctx.frame = aion_frame_top;
    ctx.trace = aion_trace_top;
    ctx.defers = defer_len;
    try_top = &ctx;
    if (setjmp(ctx.jump) == 0) {
        call0(body);
        try_top = ctx.prev;
    } else {
        aion_frame_top = ctx.frame;
        aion_trace_top = ctx.trace;
        call1(handler, aion_string(panic_message));
    }
}

void aion_panic_null(void) { aion_panic("null reference"); }
void aion_panic_div_zero(void) { aion_panic("division by zero"); }
void aion_panic_unwrap(void) { aion_panic("unexpected null value"); }

void aion_panic_cast(const char *type) {
    char msg[160];
    snprintf(msg, sizeof msg, "cannot cast to %s", type);
    aion_panic(msg);
}

void aion_panic_index(int64_t index, int64_t len) {
    char msg[96];
    snprintf(msg, sizeof msg, "index %" PRId64 " is out of bounds for size %" PRId64, index, len);
    aion_panic(msg);
}

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
        if (h->flags & AION_F_PTRS) {
            for (uint32_t i = 0, n = h->size / 8; i < n; i++) mark(slots[i]);
            continue;
        }
        for (uint64_t m = h->ptr_mask, i = 0; m; m >>= 1, i++)
            if (m & 1) mark(slots[i]);
    }
}

void aion_gc_collect(void) {
    for (int64_t i = 0; i < aion_roots_len; i++) mark(*aion_roots[i]);
    for (AionFrame *f = aion_frame_top; f; f = f->prev)
        for (uint64_t i = 0; i < f->n; i++) mark(f->slots[i]);
    for (size_t i = 0; i < defer_len; i++) mark(defer_stack[i]);
    aion_tasks_mark(mark);
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

/* `type_id` is the compiler's number for the object's type plus one (0: not a
 * user type); calls through an interface read it back from the header. */
void *aion_new_object(int64_t nslots, int64_t ptr_mask, int64_t type_id) {
    return gc_alloc((size_t)nslots * 8, AION_F_SCAN | ((uint32_t)type_id << AION_F_TYPE_SHIFT), (uint64_t)ptr_mask);
}

/* An Array<T>: `len` 8-byte slots, all set to `fill` (the slot's bit pattern).
 * `is_ptr` arrays are scanned by the collector. */
void *aion_array_new(int64_t len, int64_t is_ptr, int64_t fill) {
    if (len < 0) aion_panic("negative array size");
    if ((uint64_t)len > UINT32_MAX / 8) aion_panic("array too large");
    int64_t *a = gc_alloc((size_t)len * 8, is_ptr ? AION_F_SCAN | AION_F_PTRS : 0, 0);
    if (fill)
        for (int64_t i = 0; i < len; i++) a[i] = fill;
    return a;
}

/* ── Bytes ────────────────────────────────────────────────────── */

void *aion_bytes_new(int64_t len) {
    if (len < 0) aion_panic("negative size");
    if ((uint64_t)len > UINT32_MAX) aion_panic("a Bytes value is limited to 4 GiB");
    return gc_alloc((size_t)len, 0, 0);
}

int64_t aion_bytes_size(const void *bytes) { return HEADER_OF(bytes)->size; }
uint8_t *aion_bytes_data(void *bytes) { return (uint8_t *)bytes; }

/* Hashes for Map keys: non-negative, well mixed in the low bits. */
int64_t aion_hash_int(int64_t x) {
    uint64_t z = (uint64_t)x + 0x9e3779b97f4a7c15ull;
    z = (z ^ (z >> 30)) * 0xbf58476d1ce4e5b9ull;
    z = (z ^ (z >> 27)) * 0x94d049bb133111ebull;
    return (int64_t)((z ^ (z >> 31)) >> 1);
}

int64_t aion_hash_str(const char *s) {
    uint64_t h = 0xcbf29ce484222325ull;
    for (; *s; s++) h = (h ^ (unsigned char)*s) * 0x100000001b3ull;
    return (int64_t)(h >> 1);
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

/* -1, 0 or 1 by byte order (UTF-8 order matches code-point order). */
int64_t aion_str_cmp(const char *a, const char *b) {
    int c = strcmp(a, b);
    return c < 0 ? -1 : c > 0;
}

const char *aion_int_to_str(int64_t n) {
    char buf[32];
    snprintf(buf, sizeof buf, "%" PRId64, n);
    return aion_string(buf);
}

/* Encode a code point as UTF-8 (invalid ones become U+FFFD).  Returns the length. */
static size_t utf8_encode(int64_t c, char out[4]) {
    if (c < 0 || c > 0x10FFFF || (c >= 0xD800 && c <= 0xDFFF)) c = 0xFFFD;
    if (c < 0x80) {
        out[0] = (char)c;
        return 1;
    }
    if (c < 0x800) {
        out[0] = (char)(0xC0 | (c >> 6));
        out[1] = (char)(0x80 | (c & 0x3F));
        return 2;
    }
    if (c < 0x10000) {
        out[0] = (char)(0xE0 | (c >> 12));
        out[1] = (char)(0x80 | ((c >> 6) & 0x3F));
        out[2] = (char)(0x80 | (c & 0x3F));
        return 3;
    }
    out[0] = (char)(0xF0 | (c >> 18));
    out[1] = (char)(0x80 | ((c >> 12) & 0x3F));
    out[2] = (char)(0x80 | ((c >> 6) & 0x3F));
    out[3] = (char)(0x80 | (c & 0x3F));
    return 4;
}

const char *aion_char_to_str(int64_t c) {
    char buf[4];
    size_t n = utf8_encode(c, buf);
    return aion_string_n(buf, n);
}

/* The shortest text that reads back as exactly `f`: 0.1 prints as 0.1, 1/3 as 0.3333333333333333,
 * a whole number without a point (12), and very large or small values with an exponent (1e+20). */
static void float_text(double f, char *out, size_t cap) {
    if (isnan(f)) { snprintf(out, cap, "nan"); return; }
    if (isinf(f)) { snprintf(out, cap, f < 0 ? "-inf" : "inf"); return; }
    int digits = 1;
    for (; digits < 17; digits++) {
        snprintf(out, cap, "%.*e", digits - 1, f);
        if (strtod(out, NULL) == f) break;
    }
    snprintf(out, cap, "%.*e", digits - 1, f);
    int exponent = atoi(strchr(out, 'e') + 1);
    if (f == 0 || (exponent >= -5 && exponent < 16)) {
        int decimals = digits - 1 - exponent;
        snprintf(out, cap, "%.*f", decimals < 0 ? 0 : decimals, f);
    }
}

const char *aion_float_to_str(double f) {
    char buf[400];
    float_text(f, buf, sizeof buf);
    return aion_string(buf);
}

const char *aion_bool_to_str(int64_t b) { return aion_string(b ? "true" : "false"); }

/* ── Program arguments ────────────────────────────────────────── */

static int program_argc = 0;
static char **program_argv = NULL;

void aion_set_args(int argc, char **argv) {
    program_argc = argc;
    program_argv = argv;
}

/* The arguments after the program name. */
int64_t aion_argc(void) { return program_argc > 0 ? program_argc - 1 : 0; }
const char *aion_argv(int64_t i) {
    if (i < 0 || i >= aion_argc()) aion_panic("argument index out of range");
    return aion_string(program_argv[i + 1]);
}

/* ── Printing ─────────────────────────────────────────────────── */

static void end_line(int64_t newline) {
    if (newline) putchar('\n');
}

void aion_print_str(const char *s, int64_t nl) { fputs(s, stdout); end_line(nl); }
void aion_print_char(int64_t c, int64_t nl) {
    char buf[4];
    fwrite(buf, 1, utf8_encode(c, buf), stdout);
    end_line(nl);
}
void aion_print_int(int64_t n, int64_t nl) { printf("%" PRId64, n); end_line(nl); }
void aion_print_float(double f, int64_t nl) {
    char buf[400];
    float_text(f, buf, sizeof buf);
    fputs(buf, stdout);
    end_line(nl);
}
void aion_print_bool(int64_t b, int64_t nl) { fputs(b ? "true" : "false", stdout); end_line(nl); }
