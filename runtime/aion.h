/*
 * Aion runtime ABI.
 *
 * Include this from any C file in `stdlib/` (it is on the include path when
 * the compiler builds your module).  The types line up with Aion's like so:
 *
 *     Aion     C
 *     ------   ------------------------------------------------------
 *     Int      int64_t
 *     Float    double
 *     Bool     bool                (<stdbool.h>)
 *     String   const char *        NUL-terminated, owned by the GC
 *
 * Strings *passed to* C are ordinary C strings: read them, never free them,
 * and don't keep them after you return.  Strings *returned from* C must be
 * created with aion_string() so the collector knows about them.
 */
#ifndef AION_H
#define AION_H

#include <stdbool.h>
#include <stddef.h>
#include <stdint.h>

/* ── Public API for C modules ─────────────────────────────────── */

/* A Bytes value: raw bytes (any value, NUL included) in a GC object; the length is the
 * object's size.  Never NULL: an empty one is a static object. */
void *aion_bytes_new(int64_t len);                 /* zero-filled */
int64_t aion_bytes_size(const void *bytes);
uint8_t *aion_bytes_data(void *bytes);

/* Copy `s` into a new Aion String. */
const char *aion_string(const char *s);
/* Copy the first `n` bytes of `s` into a new Aion String. */
const char *aion_string_n(const char *s, size_t n);
/* Print "[aion] panic: <msg>" to stderr and exit(1). */
void aion_panic(const char *msg) __attribute__((noreturn));

/* ── Layout shared with the code generator (do not change lightly) ── */

#define AION_F_MARK 1u   /* reachable in the current collection        */
#define AION_F_STATIC 2u /* emitted by the compiler; never freed        */
#define AION_F_SCAN 4u   /* object: payload slots listed in ptr_mask    */
#define AION_F_PTRS 8u   /* array: every payload slot is a pointer      */
#define AION_F_TYPE_SHIFT 4 /* the bits above hold the object's type id + 1 */

/* Precedes every String and object; the value pointer is `header + 1`. */
typedef struct AionHeader {
    struct AionHeader *next; /* all-objects list (unused for statics)   */
    uint32_t size;           /* payload bytes                           */
    uint32_t flags;
    uint64_t ptr_mask;       /* bit i set: 8-byte slot i holds a pointer */
} AionHeader;

/* One per active function that holds GC pointers; lives on the C stack. */
typedef struct AionFrame {
    struct AionFrame *prev;
    uint64_t n;
    void *slots[];
} AionFrame;

extern AionFrame *aion_frame_top;

/* ── Per-task state (see tasks.c) ─────────────────────────────── */

/* What belongs to one task and is swapped when the scheduler switches: the shadow
 * stack, the innermost `try`, and the `defer` stack. */
typedef struct AionTaskState {
    AionFrame *frame;
    void *try_top;
    void **defers;
    size_t defer_len, defer_cap;
} AionTaskState;

void aion_task_save(AionTaskState *into);
void aion_task_load(const AionTaskState *from);
/* Mark everything the suspended tasks keep alive. */
void aion_tasks_mark(void (*mark)(void *));

#endif
