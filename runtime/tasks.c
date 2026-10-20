/*
 * Aion tasks: cooperative threads of control.
 *
 * Every task has its own C stack (and so its own shadow stack of GC roots, `try`
 * contexts and `defer` stack, which core.c lets us swap).  The tasks of one operating-system thread take turns:
 * a task runs until it waits, sleeps, yields or ends, so tasks of the same thread never race.  Every thread has
 * its own set of tasks (a `Sched`).  The collector sees suspended tasks through aion_tasks_mark().
 *
 * Waiting is deliberately simple: aion_park() sleeps until *anything* calls
 * aion_notify(), which wakes every parked task of every thread so it can re-check its condition (aion_park_for()
 * also wakes after a time limit).  A task that waits gives up the monitor lock (aion_sync_lock) and takes it back
 * when it runs again, so checking a condition and parking is one step as far as other threads can tell.  A task can
 * also wait for a file descriptor (aion_wait_fd), so a server can serve many connections while
 * each one blocks on its socket.  When no task can run and none is sleeping or waiting for
 * input or output, the thread is idle; when every thread is idle the program is deadlocked and says so.
 *
 * `sched_mu` guards the task lists and the states of all tasks: other threads only ever change a task from PARKED
 * or TIMED to READY, in aion_notify().  A thread that has nothing to run waits on its own condition variable, or polls
 * (in short slices, so that notifications are noticed) if tasks wait for descriptors.
 *
 * On POSIX systems a task's stack is an mmap'ed block switched to with ucontext; on Windows it is a fiber.
 * A descriptor is waited for with poll() or, on Windows, WSAPoll() (so only sockets can be waited for there;
 * anything else is reported ready at once and its owner blocks in the call itself).
 */

#ifdef __APPLE__
/* macOS declares getcontext() and the like only when _XOPEN_SOURCE is defined, and then hides its own extensions
 * (MAP_ANON and others) unless _DARWIN_C_SOURCE is defined too.  This must come before every system header. */
#define _XOPEN_SOURCE 600
#define _DARWIN_C_SOURCE
#endif

#ifdef _WIN32
#include <winsock2.h> /* before windows.h, which aion_sync.h includes */
#endif

#include "aion.h"
#include "aion_sync.h"

#include <errno.h>
#include <stdbool.h>
#include <stdint.h>
#include <stdio.h>
#include <stdlib.h>

#ifdef _WIN32
#include <winsock2.h>
#include <windows.h>
#else
#include <poll.h>
#include <sys/mman.h>
#include <time.h>
#include <ucontext.h>
#endif

#define STACK_SIZE ((size_t)256 * 1024)
#define GUARD_SIZE ((size_t)4096)
/* How long a thread whose tasks wait for descriptors goes without looking at notifications from other threads. */
#define POLL_SLICE_MS 5

typedef enum { READY, RUNNING, PARKED, TIMED, SLEEPING, WAITING, DONE } State;

typedef struct Task {
    struct Task *next;
#ifdef _WIN32
    void *fiber; /* the fiber this task runs on */
#else
    ucontext_t context;
    void *stack; /* mmap'ed, with a guard page at the low end; NULL for the main task */
#endif
    State state;
    void *closure;
    int64_t wake_at; /* SLEEPING, TIMED, WAITING: monotonic milliseconds (INT64_MAX = no deadline) */
    int wait_fd, wait_events, wait_result; /* WAITING: what for, and how it ended (1 ready, 0 timed out) */
    AionTaskState saved;
} Task;

/* The tasks of one thread. */
typedef struct Sched {
    struct Sched *next;
    Task main_task; /* the first of the list: the thread's own code */
    Task *current;
    Task *finished; /* a DONE task whose stack we can free once we are off it */
    AionCond wake;  /* the thread waits here when it is idle */
    bool idle;      /* waiting for another thread's notification */
    bool events;    /* idle, but a sleeper or a timed wait will end it by itself */
} Sched;

static AionMutex sched_mu = AION_MUTEX_INIT;
static Sched *scheds = NULL; /* every thread's, under sched_mu */
static _Thread_local Sched *mine = NULL;
/* How deep the monitor lock was held by the task that is parking; a deadlock is reported with it held again, so that
 * the cleanups of the panic find things as they expect. */
static _Thread_local int64_t parked_depth = 0;

static Sched *sched_get(void) {
    if (mine) return mine;
    Sched *s = calloc(1, sizeof *s);
    if (!s) aion_panic("out of memory");
    s->main_task.state = RUNNING;
    s->current = &s->main_task;
    aion_cond_init(&s->wake);
    aion_mutex_lock(&sched_mu);
    s->next = scheds;
    scheds = s;
    aion_mutex_unlock(&sched_mu);
    mine = s;
    return s;
}

static int64_t now_ms(void) {
#ifdef _WIN32
    return (int64_t)GetTickCount64();
#else
    struct timespec ts;
    clock_gettime(CLOCK_MONOTONIC, &ts);
    return (int64_t)ts.tv_sec * 1000 + ts.tv_nsec / 1000000;
#endif
}

static void free_stack(Task *t) {
#ifdef _WIN32
    DeleteFiber(t->fiber);
#else
    munmap(t->stack, STACK_SIZE);
#endif
}

/* Free the finished task, unless we are still running on its stack.  Takes the lock itself. */
static void reap(Sched *s) {
    if (!s->finished || s->finished == s->current) return;
    Task *gone = s->finished;
    aion_mutex_lock(&sched_mu);
    Task **link = &s->main_task.next;
    while (*link != gone) link = &(*link)->next;
    *link = gone->next;
    aion_mutex_unlock(&sched_mu);
    free_stack(gone);
    free(gone);
    s->finished = NULL;
}

/* Run `next`.  Called with sched_mu held; it is released before the switch. */
static void switch_to(Sched *s, Task *next) {
    Task *prev = s->current;
    aion_task_save(&prev->saved);
    s->current = next;
    next->state = RUNNING;
    aion_mutex_unlock(&sched_mu);
    if (prev->state == DONE) {
        reap(s); /* an earlier finished task, if any */
        s->finished = prev;
    }
    aion_task_load(&next->saved);
#ifdef _WIN32
    SwitchToFiber(next->fiber);
#else
    swapcontext(&prev->context, &next->context);
#endif
    reap(sched_get()); /* back in `prev`, on its own stack */
}

/* The sleeping task whose time has come, earliest deadline first. */
static Task *due_sleeper(Sched *s) {
    int64_t now = now_ms();
    Task *best = NULL;
    for (Task *t = &s->main_task; t; t = t->next)
        if ((t->state == SLEEPING || t->state == TIMED) && t->wake_at <= now && (!best || t->wake_at < best->wake_at))
            best = t;
    return best;
}

#ifdef _WIN32
typedef WSAPOLLFD PollFd;
static int poll_fds(PollFd *fds, int n, int64_t timeout_ms) {
    int r = WSAPoll(fds, (ULONG)n, timeout_ms > INT32_MAX ? INT32_MAX : (int)timeout_ms);
    if (r == SOCKET_ERROR) { /* something that is not a socket: let its owner find out by trying */
        for (int i = 0; i < n; i++) fds[i].revents = fds[i].events;
        return n;
    }
    return r;
}
static PollFd make_pollfd(int fd, int events) { return (PollFd){(SOCKET)fd, (SHORT)events, 0}; }
#else
typedef struct pollfd PollFd;
static int poll_fds(PollFd *fds, int n, int64_t timeout_ms) {
    return poll(fds, (nfds_t)n, timeout_ms > INT32_MAX ? INT32_MAX : (int)timeout_ms);
}
static PollFd make_pollfd(int fd, int events) { return (PollFd){fd, (short)events, 0}; }
#endif

/*
 * Look at the tasks waiting for descriptors: any that are ready (or whose deadline has passed) become READY.
 * Blocks for up to `timeout_ms` (-1 = until something happens) if none is.  Returns whether any task woke.
 * Called with sched_mu held; a blocking wait lets go of it (and of the world) meanwhile.
 */
static bool poll_waiters(Sched *s, int64_t timeout_ms) {
    int count = 0;
    for (Task *t = &s->main_task; t; t = t->next) count += t->state == WAITING;
    if (count == 0) return false;
    PollFd *fds = malloc((size_t)count * sizeof *fds);
    Task **who = malloc((size_t)count * sizeof *who);
    if (!fds || !who) aion_panic("out of memory");
    int n = 0;
    for (Task *t = &s->main_task; t; t = t->next)
        if (t->state == WAITING) fds[n] = make_pollfd(t->wait_fd, t->wait_events), who[n++] = t;
    if (timeout_ms != 0) fflush(stdout);
    int got;
    if (timeout_ms != 0 && aion_multi) {
        aion_mutex_unlock(&sched_mu);
        aion_blocking_enter();
        got = poll_fds(fds, n, timeout_ms);
        aion_blocking_leave();
        aion_mutex_lock(&sched_mu);
    } else {
        got = poll_fds(fds, n, timeout_ms);
    }
    bool woke = false;
    int64_t now = now_ms();
    for (int i = 0; i < n; i++) {
        if (got > 0 && fds[i].revents) who[i]->wait_result = 1, who[i]->state = READY, woke = true;
        else if (who[i]->wake_at <= now) who[i]->wait_result = 0, who[i]->state = READY, woke = true;
    }
    free(fds), free(who);
    return woke;
}

/* The next task that can run, going round the list from the current one. */
static Task *find_ready(Sched *s) {
    Task *t = s->current;
    do {
        t = t->next ? t->next : &s->main_task;
        if (t->state == READY) return t;
    } while (t != s->current);
    return NULL;
}

static void nap(int64_t ms) {
#ifdef _WIN32
    Sleep((DWORD)(ms > 0x7fffffff ? 0x7fffffff : ms));
#else
    struct timespec ts = {.tv_sec = ms / 1000, .tv_nsec = (ms % 1000) * 1000000};
    nanosleep(&ts, NULL);
#endif
}

/* Is every thread idle with nothing that could wake it?  Called with sched_mu held. */
static bool deadlocked(void) {
    int64_t stuck = 0;
    for (Sched *x = scheds; x; x = x->next) stuck += x->idle && !x->events;
    return stuck >= aion_threads_live();
}

/* The current task has set its own state; run something else (or itself again). */
static void schedule(void) {
    Sched *s = sched_get();
    aion_mutex_lock(&sched_mu);
    for (;;) {
        poll_waiters(s, 0);
        Task *next = due_sleeper(s);
        if (next) next->state = READY;
        else next = find_ready(s);
        if (next) {
            if (next == s->current) {
                s->current->state = RUNNING;
                aion_mutex_unlock(&sched_mu);
            } else {
                switch_to(s, next);
            }
            return;
        }
        int64_t earliest = INT64_MAX;
        bool waiting = false;
        for (Task *t = &s->main_task; t; t = t->next) {
            if (t->state == WAITING) waiting = true;
            if ((t->state == SLEEPING || t->state == TIMED || t->state == WAITING) && t->wake_at < earliest)
                earliest = t->wake_at;
        }
        int64_t wait = earliest == INT64_MAX ? -1 : earliest > now_ms() ? earliest - now_ms() : 0; /* -1: no limit */
        if (!aion_multi) {
            if (earliest == INT64_MAX && !waiting) {
                aion_mutex_unlock(&sched_mu);
                aion_sync_reacquire(parked_depth);
                aion_panic("deadlock: every task is waiting and none can continue");
            }
            if (waiting) {
                poll_waiters(s, wait);
            } else if (wait > 0) {
                fflush(stdout);
                nap(wait);
            }
            continue;
        }
        if (waiting) { /* look at the descriptors, but come back often: another thread may notify us */
            poll_waiters(s, wait >= 0 && wait <= POLL_SLICE_MS ? wait : POLL_SLICE_MS);
            continue;
        }
        s->idle = true;
        s->events = earliest != INT64_MAX;
        if (deadlocked()) {
            aion_mutex_unlock(&sched_mu);
            aion_sync_reacquire(parked_depth);
            aion_panic("deadlock: every task is waiting and none can continue");
        }
        if (wait != 0) {
            fflush(stdout);
            aion_blocking_enter();
            aion_cond_wait(&s->wake, &sched_mu, wait);
            aion_mutex_unlock(&sched_mu);
            aion_blocking_leave();
            aion_mutex_lock(&sched_mu);
        }
        s->idle = false;
    }
}

/* Every task of every thread that waits in aion_park() or aion_park_for() looks at its condition again. */
void aion_notify(void) {
    aion_mutex_lock(&sched_mu);
    for (Sched *x = scheds; x; x = x->next) {
        bool woke = false;
        for (Task *t = &x->main_task; t; t = t->next)
            if (t->state == PARKED || t->state == TIMED) t->state = READY, woke = true;
        if (woke && x->idle) {
            x->idle = false; /* it has something to do now, so it does not count towards a deadlock */
            aion_cond_signal(&x->wake);
        }
    }
    aion_mutex_unlock(&sched_mu);
}

/* Wake the idle threads so that they find out whether the program is deadlocked. */
void aion_sched_poke(void) {
    aion_mutex_lock(&sched_mu);
    for (Sched *x = scheds; x; x = x->next)
        if (x->idle) aion_cond_signal(&x->wake);
    aion_mutex_unlock(&sched_mu);
}

static void set_state(Task *t, State state) {
    aion_mutex_lock(&sched_mu);
    t->state = state;
    aion_mutex_unlock(&sched_mu);
}

static void task_main(void) {
    Sched *s = sched_get();
    reap(s);
    ((void (*)(void *))(*(void **)s->current->closure))(s->current->closure);
    set_state(s->current, DONE);
    aion_notify(); /* whoever waits for the end of a task can look again */
    schedule();
    abort(); /* a finished task is never resumed */
}

#ifdef _WIN32
static VOID CALLBACK fiber_entry(LPVOID unused) {
    (void)unused;
    task_main();
}
#endif

/* Start running `closure` (a `() -> Unit`) as a new task; it first runs when its creator waits. */
void aion_spawn(void *closure) {
    Sched *s = sched_get();
    Task *t = calloc(1, sizeof *t);
    if (!t) aion_panic("cannot create a task: out of memory");
#ifdef _WIN32
    if (!s->main_task.fiber) s->main_task.fiber = ConvertThreadToFiber(NULL);
    t->fiber = CreateFiberEx(16 * 1024, STACK_SIZE, 0, fiber_entry, NULL);
    if (!s->main_task.fiber || !t->fiber) aion_panic("cannot create a task: out of memory");
#else
    void *stack = mmap(NULL, STACK_SIZE, PROT_READ | PROT_WRITE, MAP_PRIVATE | MAP_ANONYMOUS, -1, 0);
    if (stack == MAP_FAILED) aion_panic("cannot create a task: out of memory");
    mprotect(stack, GUARD_SIZE, PROT_NONE); /* overflowing the stack faults instead of corrupting memory */
    t->stack = stack;
    getcontext(&t->context);
    t->context.uc_stack.ss_sp = stack;
    t->context.uc_stack.ss_size = STACK_SIZE;
    t->context.uc_link = NULL;
    makecontext(&t->context, task_main, 0);
#endif
    t->closure = closure;
    t->state = READY;
    aion_mutex_lock(&sched_mu);
    Task *last = &s->main_task;
    while (last->next) last = last->next;
    last->next = t;
    aion_mutex_unlock(&sched_mu);
}

/* The thread is ending: forget its tasks. */
void aion_sched_exit(void) {
    Sched *s = mine;
    if (!s) return;
    aion_mutex_lock(&sched_mu);
    for (Sched **link = &scheds; *link; link = &(*link)->next)
        if (*link == s) {
            *link = s->next;
            break;
        }
    aion_mutex_unlock(&sched_mu);
    mine = NULL;
    for (Task *t = s->main_task.next; t;) {
        Task *next = t->next;
        if (t != s->current) free_stack(t);
        free(t);
        t = next;
    }
#ifdef _WIN32
    if (s->main_task.fiber) ConvertFiberToThread();
#endif
    free(s);
}

void aion_yield(void) {
    set_state(sched_get()->current, READY);
    schedule();
}

void aion_park(void) {
    set_state(sched_get()->current, PARKED);
    int64_t depth = aion_sync_release(); /* now another thread's notify can reach us */
    parked_depth = depth;
    schedule();
    parked_depth = 0;
    aion_sync_reacquire(depth);
}

/* Like aion_park(), but also wakes once `ms` milliseconds have passed, for a `select` with a timeout. */
void aion_park_for(int64_t ms) {
    Sched *s = sched_get();
    aion_mutex_lock(&sched_mu);
    s->current->wake_at = now_ms() + (ms < 0 ? 0 : ms);
    s->current->state = TIMED;
    aion_mutex_unlock(&sched_mu);
    int64_t depth = aion_sync_release();
    parked_depth = depth;
    schedule();
    parked_depth = 0;
    aion_sync_reacquire(depth);
}

/* The clock tasks sleep by, in milliseconds. */
int64_t aion_clock_ms(void) { return now_ms(); }

void aion_sleep(int64_t ms) {
    if (ms <= 0) {
        aion_yield();
        return;
    }
    Sched *s = sched_get();
    aion_mutex_lock(&sched_mu);
    s->current->wake_at = now_ms() + ms;
    s->current->state = SLEEPING;
    aion_mutex_unlock(&sched_mu);
    schedule();
}

/*
 * Wait until `fd` is ready for `events` (POLLIN / POLLOUT) or `timeout_ms` passes (-1 = no limit); other tasks
 * run meanwhile.  Returns 1 when ready, 0 on timeout, -1 if the wait itself failed.
 */
int aion_wait_fd(int fd, int events, int64_t timeout_ms) {
    Sched *s = sched_get();
    int others = 0;
    aion_mutex_lock(&sched_mu);
    for (Task *t = &s->main_task; t; t = t->next) others += t != s->current && t->state != DONE;
    aion_mutex_unlock(&sched_mu);
    if (others == 0) { /* nobody else could run: just block */
        PollFd p = make_pollfd(fd, events);
        fflush(stdout);
        int r;
        aion_blocking_enter();
#ifdef _WIN32
        r = poll_fds(&p, 1, timeout_ms);
#else
        do r = poll_fds(&p, 1, timeout_ms); while (r < 0 && errno == EINTR);
#endif
        aion_blocking_leave();
        return r < 0 ? -1 : r > 0;
    }
    Task *cur = s->current;
    aion_mutex_lock(&sched_mu);
    cur->wait_fd = fd, cur->wait_events = events, cur->wait_result = 0;
    cur->wake_at = timeout_ms < 0 ? INT64_MAX : now_ms() + timeout_ms;
    cur->state = WAITING;
    aion_mutex_unlock(&sched_mu);
    schedule();
    return cur->wait_result;
}

/* Tasks of this thread that have not finished, the main one included. */
int64_t aion_task_count(void) {
    Sched *s = sched_get();
    int64_t n = 0;
    aion_mutex_lock(&sched_mu);
    for (Task *t = &s->main_task; t; t = t->next) n += t->state != DONE;
    aion_mutex_unlock(&sched_mu);
    return n;
}

/* The collector runs with every other thread stopped, so the lists stand still. */
void aion_tasks_mark(void (*mark)(void *)) {
    for (Sched *x = scheds; x; x = x->next) {
        for (Task *t = &x->main_task; t; t = t->next) {
            if (t->state == DONE) continue;
            mark(t->closure);
            if (t == x->current) continue; /* its roots are the live ones, marked by the collector or published */
            for (AionFrame *f = t->saved.frame; f; f = f->prev)
                for (uint64_t i = 0; i < f->n; i++) mark(f->slots[i]);
            for (size_t i = 0; i < t->saved.defer_len; i++) mark(t->saved.defers[i]);
        }
    }
}
