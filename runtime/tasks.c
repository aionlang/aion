/*
 * Aion tasks: cooperative threads of control.
 *
 * Every task has its own C stack (and so its own shadow stack of GC roots, `try`
 * contexts and `defer` stack, which core.c lets us swap).  Only one runs at a time and
 * a task runs until it waits, sleeps, yields or ends, so there are no data races and no
 * locks.  The collector sees suspended tasks through aion_tasks_mark().
 *
 * Waiting is deliberately simple: aion_park() sleeps until *anything* calls
 * aion_notify(), which wakes every parked task so it can re-check its condition (aion_park_for() also
 * wakes after a time limit).  A task can also wait for a file descriptor (aion_wait_fd), so a server can
 * serve many connections while each one blocks on its socket.  When no task can run and none is sleeping or waiting for
 * input or output, the program is deadlocked and says so.
 *
 * On POSIX systems a task's stack is an mmap'ed block switched to with ucontext; on Windows it is a fiber.
 * A descriptor is waited for with poll() or, on Windows, WSAPoll() (so only sockets can be waited for there;
 * anything else is reported ready at once and its owner blocks in the call itself).
 */

#include "aion.h"

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

static Task main_task = {.state = RUNNING};
static Task *all_tasks = &main_task;
static Task *current = &main_task;
static Task *finished = NULL; /* a DONE task whose stack we can free once we are off it */

static int64_t now_ms(void) {
#ifdef _WIN32
    return (int64_t)GetTickCount64();
#else
    struct timespec ts;
    clock_gettime(CLOCK_MONOTONIC, &ts);
    return (int64_t)ts.tv_sec * 1000 + ts.tv_nsec / 1000000;
#endif
}

static void reap(void) {
    if (!finished || finished == current) return;
    Task **link = &all_tasks;
    while (*link != finished) link = &(*link)->next;
    *link = finished->next;
#ifdef _WIN32
    DeleteFiber(finished->fiber);
#else
    munmap(finished->stack, STACK_SIZE);
#endif
    free(finished);
    finished = NULL;
}

static void switch_to(Task *next) {
    Task *prev = current;
    aion_task_save(&prev->saved);
    if (prev->state == DONE) {
        reap(); /* an earlier finished task, if any */
        finished = prev;
    }
    current = next;
    next->state = RUNNING;
    aion_task_load(&next->saved);
#ifdef _WIN32
    SwitchToFiber(next->fiber);
#else
    swapcontext(&prev->context, &next->context);
#endif
    reap(); /* back in `prev`, on its own stack */
}

/* The sleeping task whose time has come, earliest deadline first. */
static Task *due_sleeper(void) {
    int64_t now = now_ms();
    Task *best = NULL;
    for (Task *t = all_tasks; t; t = t->next)
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
 */
static bool poll_waiters(int64_t timeout_ms) {
    int count = 0;
    for (Task *t = all_tasks; t; t = t->next) count += t->state == WAITING;
    if (count == 0) return false;
    PollFd *fds = malloc((size_t)count * sizeof *fds);
    Task **who = malloc((size_t)count * sizeof *who);
    if (!fds || !who) aion_panic("out of memory");
    int n = 0;
    for (Task *t = all_tasks; t; t = t->next)
        if (t->state == WAITING) fds[n] = make_pollfd(t->wait_fd, t->wait_events), who[n++] = t;
    if (timeout_ms != 0) fflush(stdout);
    int got = poll_fds(fds, n, timeout_ms);
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
static Task *find_ready(void) {
    Task *t = current;
    do {
        t = t->next ? t->next : all_tasks;
        if (t->state == READY) return t;
    } while (t != current);
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

/* The current task has set its own state; run something else (or itself again). */
static void schedule(void) {
    for (;;) {
        poll_waiters(0);
        Task *next = due_sleeper();
        if (next) next->state = READY;
        else next = find_ready();
        if (next) {
            if (next == current) current->state = RUNNING;
            else switch_to(next);
            return;
        }
        int64_t earliest = INT64_MAX;
        bool waiting = false;
        for (Task *t = all_tasks; t; t = t->next) {
            if (t->state == WAITING) waiting = true;
            if ((t->state == SLEEPING || t->state == TIMED || t->state == WAITING) && t->wake_at < earliest)
                earliest = t->wake_at;
        }
        if (earliest == INT64_MAX && !waiting) aion_panic("deadlock: every task is waiting and none can continue");
        int64_t wait = earliest == INT64_MAX ? -1 : earliest - now_ms();
        if (waiting) {
            poll_waiters(wait < 0 && earliest != INT64_MAX ? 0 : wait);
        } else if (wait > 0) {
            fflush(stdout);
            nap(wait);
        }
    }
}

void aion_notify(void) {
    for (Task *t = all_tasks; t; t = t->next)
        if (t->state == PARKED || t->state == TIMED) t->state = READY;
}

static void task_main(void) {
    reap();
    ((void (*)(void *))(*(void **)current->closure))(current->closure);
    current->state = DONE;
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
    Task *t = calloc(1, sizeof *t);
    if (!t) aion_panic("cannot create a task: out of memory");
#ifdef _WIN32
    if (!main_task.fiber) main_task.fiber = ConvertThreadToFiber(NULL);
    t->fiber = CreateFiberEx(16 * 1024, STACK_SIZE, 0, fiber_entry, NULL);
    if (!main_task.fiber || !t->fiber) aion_panic("cannot create a task: out of memory");
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
    Task *last = all_tasks;
    while (last->next) last = last->next;
    last->next = t;
}

void aion_yield(void) {
    current->state = READY;
    schedule();
}

void aion_park(void) {
    current->state = PARKED;
    schedule();
}

/* Like aion_park(), but also wakes once `ms` milliseconds have passed, for a `select` with a timeout. */
void aion_park_for(int64_t ms) {
    current->wake_at = now_ms() + (ms < 0 ? 0 : ms);
    current->state = TIMED;
    schedule();
}

/* The clock tasks sleep by, in milliseconds. */
int64_t aion_clock_ms(void) { return now_ms(); }

void aion_sleep(int64_t ms) {
    if (ms <= 0) {
        aion_yield();
        return;
    }
    current->wake_at = now_ms() + ms;
    current->state = SLEEPING;
    schedule();
}

/*
 * Wait until `fd` is ready for `events` (POLLIN / POLLOUT) or `timeout_ms` passes (-1 = no limit); other tasks
 * run meanwhile.  Returns 1 when ready, 0 on timeout, -1 if the wait itself failed.
 */
int aion_wait_fd(int fd, int events, int64_t timeout_ms) {
    int others = 0;
    for (Task *t = all_tasks; t; t = t->next) others += t != current && t->state != DONE;
    if (others == 0) { /* nobody else could run: just block */
        PollFd p = make_pollfd(fd, events);
        fflush(stdout);
        int r;
#ifdef _WIN32
        r = poll_fds(&p, 1, timeout_ms);
#else
        do r = poll_fds(&p, 1, timeout_ms); while (r < 0 && errno == EINTR);
#endif
        return r < 0 ? -1 : r > 0;
    }
    current->wait_fd = fd, current->wait_events = events, current->wait_result = 0;
    current->wake_at = timeout_ms < 0 ? INT64_MAX : now_ms() + timeout_ms;
    current->state = WAITING;
    schedule();
    return current->wait_result;
}

/* Tasks that have not finished, the main one included. */
int64_t aion_task_count(void) {
    int64_t n = 0;
    for (Task *t = all_tasks; t; t = t->next) n += t->state != DONE;
    return n;
}

void aion_tasks_mark(void (*mark)(void *)) {
    for (Task *t = all_tasks; t; t = t->next) {
        if (t->state == DONE) continue;
        mark(t->closure);
        if (t == current) continue; /* its roots are the live ones, marked by the collector */
        for (AionFrame *f = t->saved.frame; f; f = f->prev)
            for (uint64_t i = 0; i < f->n; i++) mark(f->slots[i]);
        for (size_t i = 0; i < t->saved.defer_len; i++) mark(t->saved.defers[i]);
    }
}
