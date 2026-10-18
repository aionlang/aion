/*
 * Aion threads: operating-system threads that run Aion code side by side.
 *
 * Every thread has its own shadow stack, `try` chain and `defer` stack (they are thread-local in core.c) and its own
 * task scheduler (tasks.c).  What they share is the heap, the module variables and a few runtime structures.
 *
 * The collector stops the world.  Collection only happens inside an allocation, and the thread that collects waits until
 * every other thread is either at a safepoint (it is allocating too, or waiting) or inside a blocking region (a wait
 * for a lock, a condition or the network), after publishing what it keeps alive in its `Thread` record.  The others
 * stay put until the collection is over.  A thread that computes for a long time without allocating delays a collection
 * until it does; threads that only wait never do.
 *
 * Programs with a single thread never take any of these locks: `aion_multi` is false until the first thread starts.
 *
 * The monitor lock (aion_sync_lock) is the one lock of the language-level library: channels, jobs, mutexes and atomics
 * check and change their state while holding it, and a task that has to wait gives it up and takes it back (aion_park).
 */

#include "aion.h"
#include "aion_sync.h"

#include <stdio.h>
#include <stdlib.h>
#include <string.h>

#ifdef _WIN32
#include <process.h>
#else
#include <unistd.h>
#endif

volatile bool aion_multi = false;

typedef struct Thread {
    struct Thread *next;
    AionTaskState cur; /* what the running task keeps alive; published when the thread stops */
    void *closure;     /* the body, until the thread has started */
    bool published;    /* `cur` is current: the thread is at a safepoint or blocked */
} Thread;

static AionMutex world_mu = AION_MUTEX_INIT;
static AionCond world_cv;
static bool world_cv_ready = false;
static Thread main_thread = {.published = false};
static Thread *threads = &main_thread;
static int64_t running = 1; /* threads that execute Aion code right now */
static int64_t live = 1;    /* threads that exist */
static volatile bool stopping = false;
static _Thread_local Thread *self = NULL;
static _Thread_local int blocking_depth = 0;

static Thread *me(void) { return self ? self : &main_thread; }

int64_t aion_threads_live(void) { return live; }

/* ── Stopping the world ───────────────────────────────────────── */

void aion_blocking_enter(void) {
    if (!aion_multi) return;
    if (blocking_depth++ > 0) return;
    Thread *t = me();
    aion_task_save(&t->cur);
    aion_mutex_lock(&world_mu);
    t->published = true;
    running -= 1;
    if (stopping) aion_cond_broadcast(&world_cv);
    aion_mutex_unlock(&world_mu);
}

void aion_blocking_leave(void) {
    if (!aion_multi) return;
    if (--blocking_depth > 0) return;
    Thread *t = me();
    aion_mutex_lock(&world_mu);
    while (stopping) aion_cond_wait(&world_cv, &world_mu, -1);
    running += 1;
    t->published = false;
    aion_mutex_unlock(&world_mu);
}

void aion_safepoint(void) {
    if (!stopping) return;
    aion_blocking_enter();
    aion_blocking_leave();
}

bool aion_world_stop(void) {
    aion_mutex_lock(&world_mu);
    if (stopping) { /* somebody else is collecting: be stopped, and let them do it */
        aion_mutex_unlock(&world_mu);
        aion_safepoint();
        return false;
    }
    stopping = true;
    running -= 1; /* this thread is the collector */
    while (running > 0) aion_cond_wait(&world_cv, &world_mu, -1);
    aion_mutex_unlock(&world_mu);
    return true;
}

void aion_world_resume(void) {
    aion_mutex_lock(&world_mu);
    stopping = false;
    running += 1;
    aion_cond_broadcast(&world_cv);
    aion_mutex_unlock(&world_mu);
}

void aion_threads_mark(void (*mark)(void *)) {
    Thread *mine = me();
    for (Thread *t = threads; t; t = t->next) {
        mark(t->closure);
        if (t == mine || !t->published) continue;
        for (AionFrame *f = t->cur.frame; f; f = f->prev)
            for (uint64_t i = 0; i < f->n; i++) mark(f->slots[i]);
        for (size_t i = 0; i < t->cur.defer_len; i++) mark(t->cur.defers[i]);
    }
}

/* ── Starting threads ─────────────────────────────────────────── */

#define THREAD_STACK ((size_t)8 * 1024 * 1024)

static void thread_body(Thread *t) {
    self = t;
    blocking_depth = 1;
    aion_blocking_leave(); /* the record was made in the blocked state; wait for any collection under way */
    void *closure = t->closure;
    (*(void (**)(void *))closure)(closure);
    aion_sched_exit(); /* the tasks of this thread go away with it */

    aion_mutex_lock(&world_mu);
    for (Thread **link = &threads; *link; link = &(*link)->next)
        if (*link == t) {
            *link = t->next;
            break;
        }
    running -= 1;
    live -= 1;
    aion_cond_broadcast(&world_cv);
    aion_mutex_unlock(&world_mu);
    aion_sched_poke(); /* a thread that was waiting for this one may now be deadlocked */
    free(t);
}

#ifdef _WIN32
static unsigned __stdcall thread_entry(void *arg) {
    thread_body(arg);
    return 0;
}
#else
static void *thread_entry(void *arg) {
    thread_body(arg);
    return NULL;
}
#endif

/* Run `closure` (a `() -> Unit`) on a new operating-system thread. */
void aion_thread_start(void *closure) {
    Thread *t = calloc(1, sizeof *t);
    if (!t) aion_panic("cannot create a thread: out of memory");
    t->closure = closure;
    t->published = true; /* not running yet: counted as blocked until the thread says otherwise */
    aion_mutex_lock(&world_mu);
    if (!world_cv_ready) {
        aion_cond_init(&world_cv);
        world_cv_ready = true;
    }
    aion_multi = true;
    t->next = threads;
    threads = t;
    live += 1;
    aion_mutex_unlock(&world_mu);
#ifdef _WIN32
    uintptr_t h = _beginthreadex(NULL, (unsigned)THREAD_STACK, thread_entry, t, 0, NULL);
    int failed = h == 0;
    if (!failed) CloseHandle((HANDLE)h);
#else
    pthread_attr_t attr;
    pthread_attr_init(&attr);
    pthread_attr_setstacksize(&attr, THREAD_STACK);
    pthread_attr_setdetachstate(&attr, PTHREAD_CREATE_DETACHED);
    pthread_t id;
    int failed = pthread_create(&id, &attr, thread_entry, t);
    pthread_attr_destroy(&attr);
#endif
    if (failed) aion_panic("cannot create a thread");
}

/* The number of processors, at least 1. */
int64_t aion_cpu_count(void) {
#ifdef _WIN32
    SYSTEM_INFO info;
    GetSystemInfo(&info);
    return info.dwNumberOfProcessors > 0 ? (int64_t)info.dwNumberOfProcessors : 1;
#else
    long n = sysconf(_SC_NPROCESSORS_ONLN);
    return n > 0 ? n : 1;
#endif
}

/* A number that tells this thread from the others: 0 for the main thread. */
int64_t aion_thread_id(void) {
    static int64_t next_id = 1;
    static _Thread_local int64_t id = -1;
    if (id < 0) id = self ? __atomic_fetch_add(&next_id, 1, __ATOMIC_RELAXED) : 0;
    return id;
}

/* ── The monitor lock ─────────────────────────────────────────── */

static AionMutex sync_mu = AION_MUTEX_INIT;
static _Thread_local int64_t sync_depth = 0;
static _Thread_local bool sync_real = false; /* this thread holds sync_mu (it was taken while threads existed) */

static void sync_take(void) {
    if (aion_mutex_trylock(&sync_mu)) return;
    aion_blocking_enter();
    aion_mutex_lock(&sync_mu);
    aion_blocking_leave();
}

void aion_sync_lock(void) {
    if (sync_depth++ > 0) return;
    if (aion_multi) {
        sync_take();
        sync_real = true;
    }
}

void aion_sync_unlock(void) {
    if (sync_depth <= 0) aion_panic("unlock of a lock that is not held");
    if (--sync_depth > 0) return;
    if (sync_real) {
        sync_real = false;
        aion_mutex_unlock(&sync_mu);
    }
}

int64_t aion_sync_release(void) {
    int64_t depth = sync_depth;
    if (depth > 0 && sync_real) {
        sync_real = false;
        aion_mutex_unlock(&sync_mu);
    }
    sync_depth = 0;
    return depth;
}

void aion_sync_reacquire(int64_t depth) {
    if (depth > 0 && aion_multi) {
        sync_take();
        sync_real = true;
    }
    sync_depth = depth;
}

int64_t aion_sync_depth(void) { return sync_depth; }

void aion_sync_unwind(int64_t depth) {
    while (sync_depth > depth) aion_sync_unlock();
}
