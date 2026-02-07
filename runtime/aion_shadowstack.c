/*
 * Aion Shadow Stack – LLVM Built-in GC Runtime Support
 *
 * This file provides the C runtime structures and helpers required
 * by LLVM's built-in "shadow-stack" GC strategy.
 *
 * ┌──────────────────────────────────────────────────────────────┐
 * │ How it works                                                 │
 * │                                                              │
 * │  The compiler sets  gc "shadow-stack"  on every LLVM         │
 * │  function and marks pointer-typed allocas with the           │
 * │  @llvm.gcroot intrinsic.                                     │
 * │                                                              │
 * │  LLVM's ShadowStackGC lowering pass then automatically:      │
 * │    1. Generates a static FrameMap per function describing    │
 * │       the number and metadata of its GC roots.               │
 * │    2. Emits prologue code that pushes a StackEntry onto the  │
 * │       global `llvm_gc_root_chain` linked list.               │
 * │    3. Emits epilogue code at every return to pop the entry.  │
 * │                                                              │
 * │  This file defines the global `llvm_gc_root_chain` symbol    │
 * │  that LLVM reads/writes, plus enumeration helpers the        │
 * │  garbage collector uses to discover live roots.              │
 * └──────────────────────────────────────────────────────────────┘
 */

#include <stdint.h>
#include <stdio.h>

/* ── Personality stub for LLVM-generated unwind tables ────────── */

/*
 * LLVM's ShadowStackGC lowering emits SEH / DWARF unwind info that
 * references a personality function.  Aion does not use exceptions,
 * so we provide a no-op stub to satisfy the linker.
 */
#ifdef _WIN32
int __gcc_personality_v0(int version, int actions,
                         unsigned long long exception_class,
                         void *exception_object, void *context) {
    (void)version; (void)actions; (void)exception_class;
    (void)exception_object; (void)context;
    return 0;
}
#endif

/* ── LLVM shadow-stack structures ─────────────────────────────── */

/*
 * FrameMap – compile-time constant emitted by LLVM for each
 * function that has @llvm.gcroot calls.
 */
struct FrameMap {
    int32_t NumRoots;   /* number of GC roots in this frame  */
    int32_t NumMeta;    /* number of metadata entries         */
    /* const void *Meta[];  — metadata pointers (NumMeta entries) */
};

/*
 * StackEntry – one per live activation frame on the shadow stack.
 * LLVM allocates these on the hardware stack and links them
 * through `llvm_gc_root_chain`.
 */
struct StackEntry {
    struct StackEntry      *Next;   /* caller's stack entry  */
    const struct FrameMap  *Map;    /* pointer to FrameMap   */
    /* void *Roots[];  — in-place array of GC root pointers  */
};

/* ── Global root-chain head ──────────────────────────────────── */

/*
 * LLVM-generated code reads and writes this directly.
 * Must be a non-static global named exactly `llvm_gc_root_chain`.
 */
struct StackEntry *llvm_gc_root_chain = 0;

/* ── Enumeration helpers for the garbage collector ───────────── */

/*
 * Walk every live GC root on the shadow stack and call
 * `visitor(root)` for each non-NULL root pointer.
 *
 * The garbage collector calls this at the start of a collection
 * cycle to discover the root set.
 */
void aion_gc_enumerate_roots(void (*visitor)(void *)) {
    struct StackEntry *entry = llvm_gc_root_chain;
    while (entry) {
        int32_t num_roots = entry->Map->NumRoots;
        void **roots = (void **)(entry + 1);   /* Roots[] follows the header */
        for (int32_t i = 0; i < num_roots; i++) {
            if (roots[i]) {
                visitor(roots[i]);
            }
        }
        entry = entry->Next;
    }
}

/*
 * Return the number of live (non-NULL) roots currently on the
 * shadow stack.  Useful for diagnostics and testing.
 */
long long aion_gc_count_roots(void) {
    long long count = 0;
    struct StackEntry *entry = llvm_gc_root_chain;
    while (entry) {
        int32_t num_roots = entry->Map->NumRoots;
        void **roots = (void **)(entry + 1);
        for (int32_t i = 0; i < num_roots; i++) {
            if (roots[i]) count++;
        }
        entry = entry->Next;
    }
    return count;
}

/*
 * Return the current depth (number of active frames) on the
 * shadow stack.
 */
long long aion_gc_depth(void) {
    long long depth = 0;
    struct StackEntry *entry = llvm_gc_root_chain;
    while (entry) {
        depth++;
        entry = entry->Next;
    }
    return depth;
}
