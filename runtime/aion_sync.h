/*
 * Mutexes, condition variables and the thread layer of the runtime, shared by core.c, tasks.c and threads.c.
 * Everything here is static inline except the functions declared at the end, which threads.c defines.
 */
#ifndef AION_SYNC_H
#define AION_SYNC_H

#include <stdbool.h>
#include <stddef.h>
#include <stdint.h>

#ifdef _WIN32
#include <windows.h>

typedef SRWLOCK AionMutex;
typedef CONDITION_VARIABLE AionCond;
#define AION_MUTEX_INIT SRWLOCK_INIT

static inline void aion_mutex_lock(AionMutex *m) { AcquireSRWLockExclusive(m); }
static inline bool aion_mutex_trylock(AionMutex *m) { return TryAcquireSRWLockExclusive(m) != 0; }
static inline void aion_mutex_unlock(AionMutex *m) { ReleaseSRWLockExclusive(m); }
static inline void aion_cond_init(AionCond *c) { InitializeConditionVariable(c); }
/* Wait for a signal, at most `ms` milliseconds (negative: no limit); the mutex must be held. */
static inline void aion_cond_wait(AionCond *c, AionMutex *m, int64_t ms) {
    SleepConditionVariableSRW(c, m, ms < 0 ? INFINITE : (DWORD)(ms > 0x7fffffff ? 0x7fffffff : ms), 0);
}
static inline void aion_cond_signal(AionCond *c) { WakeConditionVariable(c); }
static inline void aion_cond_broadcast(AionCond *c) { WakeAllConditionVariable(c); }
#else
#include <pthread.h>
#include <time.h>

typedef pthread_mutex_t AionMutex;
typedef pthread_cond_t AionCond;
#define AION_MUTEX_INIT PTHREAD_MUTEX_INITIALIZER

static inline void aion_mutex_lock(AionMutex *m) { pthread_mutex_lock(m); }
static inline bool aion_mutex_trylock(AionMutex *m) { return pthread_mutex_trylock(m) == 0; }
static inline void aion_mutex_unlock(AionMutex *m) { pthread_mutex_unlock(m); }
static inline void aion_cond_init(AionCond *c) { pthread_cond_init(c, NULL); }
static inline void aion_cond_wait(AionCond *c, AionMutex *m, int64_t ms) {
    if (ms < 0) {
        pthread_cond_wait(c, m);
        return;
    }
    struct timespec ts;
    clock_gettime(CLOCK_REALTIME, &ts);
    ts.tv_sec += ms / 1000;
    ts.tv_nsec += (ms % 1000) * 1000000;
    if (ts.tv_nsec >= 1000000000) {
        ts.tv_sec += 1;
        ts.tv_nsec -= 1000000000;
    }
    pthread_cond_timedwait(c, m, &ts);
}
static inline void aion_cond_signal(AionCond *c) { pthread_cond_signal(c); }
static inline void aion_cond_broadcast(AionCond *c) { pthread_cond_broadcast(c); }
#endif

/* ── Threads (threads.c) ──────────────────────────────────────── */

/* True once a second thread has been started; until then none of the locking below does anything. */
extern volatile bool aion_multi;

/* Threads that are running Aion code (the main one included). */
int64_t aion_threads_live(void);

/* Stopping the world for a collection.  A thread that is about to wait for something (a lock, a condition, the
 * network) brackets the wait with aion_blocking_enter() and aion_blocking_leave(): meanwhile it may not touch the heap,
 * and a collection does not wait for it.  Threads that allocate reach a safepoint on their own. */
void aion_blocking_enter(void);
void aion_blocking_leave(void);
void aion_safepoint(void);
/* The collecting thread stops all the others and returns true; false means another thread collected meanwhile. */
bool aion_world_stop(void);
void aion_world_resume(void);
/* Mark what the other threads keep alive (their running tasks). */
void aion_threads_mark(void (*mark)(void *));

/* Task scheduling (tasks.c) as the thread layer needs it: forget this thread's tasks when it ends, and let every idle
 * thread look again whether the program is deadlocked. */
void aion_sched_exit(void);
void aion_sched_poke(void);
void aion_thread_start(void *closure);

/* The monitor lock that makes channels, jobs and the like safe between threads.  It nests (per thread). */
void aion_sync_lock(void);
void aion_sync_unlock(void);
/* Give up the lock completely, returning how deep it was held, and take it again to that depth. */
int64_t aion_sync_release(void);
void aion_sync_reacquire(int64_t depth);
int64_t aion_sync_depth(void);
/* After a panic was caught: release down to `depth`. */
void aion_sync_unwind(int64_t depth);

#endif
