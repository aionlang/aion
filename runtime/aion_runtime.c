/*
 * Aion Runtime Library
 *
 * Built-in functions that the Aion compiler emits calls to.
 * This is compiled to a static library (libaion_runtime.a) and
 * linked into every Aion program.
 *
 * ┌──────────────────────────────────────────────────────────────┐
 * │ Mark-Sweep Garbage Collector                                 │
 * │                                                              │
 * │  Every heap allocation goes through aion_alloc(), which      │
 * │  prepends an ObjHeader and links the object into a global    │
 * │  chain.  When allocated bytes exceed gc_threshold the        │
 * │  collector runs:                                             │
 * │                                                              │
 * │    1. CLEAR  – reset all mark bits                           │
 * │    2. MARK   – walk the LLVM shadow-stack root chain via     │
 * │               aion_gc_enumerate_roots() and mark each        │
 * │               reachable heap object                          │
 * │    3. SWEEP  – free every unmarked object and remove it      │
 * │               from the chain                                 │
 * │                                                              │
 * │  The threshold doubles after each collection that reclaims   │
 * │  less than half the heap.                                    │
 * └──────────────────────────────────────────────────────────────┘
 */

#include <stdio.h>
#include <stdlib.h>
#include <string.h>
#include <stdint.h>

/* ── Forward declarations (from aion_shadowstack.c) ──────────── */

extern void aion_gc_enumerate_roots(void (*visitor)(void *));

/* ── GC object header ────────────────────────────────────────── */

#define GC_MAGIC 0xA10DEAD1u

/*
 * Every heap allocation returned by aion_alloc is preceded by this
 * header.  The user-visible pointer is (ObjHeader*)+1.
 */
typedef struct ObjHeader {
    uint32_t          magic;     /* GC_MAGIC — validates GC ownership  */
    uint32_t          marked;    /* 0 = white, 1 = marked (reachable)  */
    size_t            size;      /* payload size in bytes               */
    struct ObjHeader *next;      /* intrusive linked list → gc_heap     */
} ObjHeader;

/* ── GC globals ──────────────────────────────────────────────── */

static ObjHeader *gc_heap      = NULL;        /* head of all live objects  */
static size_t     gc_allocated = 0;           /* total bytes on the heap   */
static size_t     gc_threshold = 1024 * 1024; /* 1 MB initial threshold    */
static int        gc_collecting = 0;          /* re-entrance guard         */

/* ── I/O ─────────────────────────────────────────────────────── */

void aion_print(const char *s)       { printf("%s", s);    }
void aion_print_int(long long n)     { printf("%lld", n);  }
void aion_print_float(double f)      { printf("%g", f);    }
void aion_println(const char *s)     { printf("%s\n", s);  }
void aion_println_int(long long n)   { printf("%lld\n", n);}
void aion_println_float(double f)    { printf("%g\n", f);  }

/* ── Process control ─────────────────────────────────────────── */

void aion_panic(const char *msg) {
    fprintf(stderr, "[aion] panic: %s\n", msg);
    exit(1);
}

/* ── GC core ─────────────────────────────────────────────────── */

/*
 * Mark visitor — called for each live root pointer discovered
 * on the shadow stack.  Validates that the pointer actually
 * belongs to our GC heap (via the magic sentinel) before marking.
 */
static void gc_mark(void *root) {
    if (!root) return;
    ObjHeader *hdr = (ObjHeader *)root - 1;
    if (hdr->magic == GC_MAGIC) {
        hdr->marked = 1;
    }
}

/*
 * Run a full mark-sweep collection cycle.
 * Safe to call at any time; the re-entrance guard prevents
 * recursive collections (e.g. if a finalizer allocates).
 */
void aion_gc_collect(void) {
    if (gc_collecting) return;
    gc_collecting = 1;

    /* Phase 1 — clear all mark bits. */
    for (ObjHeader *h = gc_heap; h; h = h->next) {
        h->marked = 0;
    }

    /* Phase 2 — mark reachable objects via shadow-stack roots. */
    aion_gc_enumerate_roots(gc_mark);

    /* Phase 3 — sweep: free every unmarked object. */
    size_t freed = 0;
    ObjHeader **pp = &gc_heap;
    while (*pp) {
        if (!(*pp)->marked) {
            ObjHeader *garbage = *pp;
            *pp = garbage->next;
            freed += garbage->size + sizeof(ObjHeader);
            garbage->magic = 0;   /* invalidate header */
            free(garbage);
        } else {
            pp = &(*pp)->next;
        }
    }
    gc_allocated -= freed;

    /* Adaptive threshold: grow if we kept more than half. */
    if (gc_allocated > gc_threshold / 2) {
        gc_threshold *= 2;
    }

    gc_collecting = 0;
}

/* ── Memory (GC-managed) ─────────────────────────────────────── */

/*
 * Check if a collection is needed and run one if so.
 *
 * This must only be called at "safe points" — places where no
 * un-rooted temporaries are live (e.g. the top of a loop body,
 * between top-level statements).  The compiler emits calls to
 * this at the start of every loop iteration.
 */
void aion_gc_safepoint(void) {
    if (gc_allocated > gc_threshold && !gc_collecting) {
        aion_gc_collect();
    }
}

/*
 * Allocate `size` bytes on the GC-managed heap.
 * Does NOT trigger collection — that happens at safe points.
 * Returns a pointer to the payload (header is hidden).
 */
void *aion_alloc(long long size) {
    size_t req = (size_t)size;

    ObjHeader *hdr = (ObjHeader *)malloc(sizeof(ObjHeader) + req);
    if (!hdr) {
        /* Last-resort: try a collection then retry once. */
        aion_gc_collect();
        hdr = (ObjHeader *)malloc(sizeof(ObjHeader) + req);
        if (!hdr) aion_panic("out of memory");
    }

    hdr->magic  = GC_MAGIC;
    hdr->marked = 0;
    hdr->size   = req;
    hdr->next   = gc_heap;
    gc_heap     = hdr;
    gc_allocated += req + sizeof(ObjHeader);

    return (void *)(hdr + 1);
}

/*
 * Explicit free — removes the object from the GC chain and
 * releases its memory immediately.  Accepts non-GC pointers
 * gracefully (falls back to plain free).
 */
void aion_free(void *ptr) {
    if (!ptr) return;
    ObjHeader *hdr = (ObjHeader *)ptr - 1;
    if (hdr->magic != GC_MAGIC) {
        /* Not a GC object (e.g. C library allocation). */
        free(ptr);
        return;
    }
    /* Unlink from heap chain. */
    ObjHeader **pp = &gc_heap;
    while (*pp) {
        if (*pp == hdr) {
            *pp = hdr->next;
            break;
        }
        pp = &(*pp)->next;
    }
    gc_allocated -= hdr->size + sizeof(ObjHeader);
    hdr->magic = 0;
    free(hdr);
}

/* ── GC diagnostics ──────────────────────────────────────────── */

/* Return total bytes currently allocated on the GC heap. */
long long aion_gc_heap_size(void) {
    return (long long)gc_allocated;
}

/* Return the current collection threshold in bytes. */
long long aion_gc_get_threshold(void) {
    return (long long)gc_threshold;
}

/* Set the collection threshold (bytes). */
void aion_gc_set_threshold(long long t) {
    gc_threshold = (size_t)(t > 0 ? t : 1024);
}

/* Return the number of live objects on the GC heap. */
long long aion_gc_object_count(void) {
    long long n = 0;
    for (ObjHeader *h = gc_heap; h; h = h->next) n++;
    return n;
}

/* ── String helpers (allocations go through aion_alloc) ──────── */

char* aion_concat(const char* a, const char* b) {
    size_t len = strlen(a) + strlen(b);
    char* res = (char*)aion_alloc((long long)(len + 1));
    strcpy(res, a);
    strcat(res, b);
    return res;
}

char* aion_int_to_str(long long n) {
    char buffer[32];
    snprintf(buffer, sizeof(buffer), "%lld", n);
    size_t len = strlen(buffer);
    char* res = (char*)aion_alloc((long long)(len + 1));
    strcpy(res, buffer);
    return res;
}

char* aion_float_to_str(double f) {
    char buffer[64];
    snprintf(buffer, sizeof(buffer), "%g", f);
    size_t len = strlen(buffer);
    char* res = (char*)aion_alloc((long long)(len + 1));
    strcpy(res, buffer);
    return res;
}
