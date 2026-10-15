/*
 * Aion tasks: cooperative threads of control.
 *
 * Every task has its own C stack (and so its own shadow stack of GC roots, `try`
 * contexts and `defer` stack, which core.c lets us swap).  Only one runs at a time and
 * a task runs until it waits, sleeps, yields or ends, so there are no data races and no
 * locks.  The collector sees suspended tasks through aion_tasks_mark().
 *
 * Waiting is deliberately simple: aion_park() sleeps until *anything* calls
 * aion_notify(), which wakes every parked task so it can re-check its condition.  A task can
 * also wait for a file descriptor (aion_wait_fd), so a server can serve many connections while
 * each one blocks on its socket.  When no task can run and none is sleeping or waiting for
 * input or output, the program is deadlocked and says so.
 */

#include "aion.h"

#include <errno.h>
#include <stdbool.h>
#include <stdint.h>
#include <stdio.h>
#include <stdlib.h>

#ifdef _WIN32

int aion_wait_fd(int fd, int events, int64_t timeout_ms) { (void)fd, (void)events, (void)timeout_ms; return 1; }

void aion_spawn(void *closure) { (void)closure; aion_panic("tasks are not supported on Windows yet"); }
void aion_yield(void) {}
void aion_park(void) { aion_panic("tasks are not supported on Windows yet"); }
void aion_notify(void) {}
void aion_sleep(int64_t ms) { (void)ms; }
int64_t aion_task_count(void) { return 1; }
void aion_tasks_mark(void (*mark)(void *)) { (void)mark; }

#else

#include <poll.h>
#include <sys/mman.h>
#include <time.h>
#include <ucontext.h>

#define STACK_SIZE ((size_t)256 * 1024)
#define GUARD_SIZE ((size_t)4096)

typedef enum { READY, RUNNING, PARKED, SLEEPING, WAITING, DONE } State;

typedef struct Task {
    struct Task *next;
    ucontext_t context;
    State state;
    void *stack; /* mmap'ed, with a guard page at the low end; NULL for the main task */
    void *closure;
    int64_t wake_at; /* SLEEPING, WAITING: monotonic milliseconds (INT64_MAX = no deadline) */
    int wait_fd, wait_events, wait_result; /* WAITING: what for, and how it ended (1 ready, 0 timed out) */
    AionTaskState saved;
} Task;

static Task main_task = {.state = RUNNING};
static Task *all_tasks = &main_task;
static Task *current = &main_task;
static Task *finished = NULL; /* a DONE task whose stack we can free once we are off it */

static int64_t now_ms(void) {
    struct timespec ts;
    clock_gettime(CLOCK_MONOTONIC, &ts);
    return (int64_t)ts.tv_sec * 1000 + ts.tv_nsec / 1000000;
}

static void reap(void) {
    if (!finished || finished == current) return;
    Task **link = &all_tasks;
    while (*link != finished) link = &(*link)->next;
    *link = finished->next;
    munmap(finished->stack, STACK_SIZE);
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
    swapcontext(&prev->context, &next->context);
    reap(); /* back in `prev`, on its own stack */
}

/* The sleeping task whose time has come, earliest deadline first. */
static Task *due_sleeper(void) {
    int64_t now = now_ms();
    Task *best = NULL;
    for (Task *t = all_tasks; t; t = t->next)
        if (t->state == SLEEPING && t->wake_at <= now && (!best || t->wake_at < best->wake_at)) best = t;
    return best;
}

/*
 * Look at the tasks waiting for descriptors: any that are ready (or whose deadline has passed) become READY.
 * Blocks for up to `timeout_ms` (-1 = until something happens) if none is.  Returns whether any task woke.
 */
static bool poll_waiters(int64_t timeout_ms) {
    int count = 0;
    for (Task *t = all_tasks; t; t = t->next) count += t->state == WAITING;
    if (count == 0) return false;
    struct pollfd *fds = malloc((size_t)count * sizeof *fds);
    Task **who = malloc((size_t)count * sizeof *who);
    if (!fds || !who) aion_panic("out of memory");
    int n = 0;
    for (Task *t = all_tasks; t; t = t->next)
        if (t->state == WAITING) fds[n] = (struct pollfd){t->wait_fd, (short)t->wait_events, 0}, who[n++] = t;
    if (timeout_ms != 0) fflush(stdout);
    int got = poll(fds, (nfds_t)n, timeout_ms > INT32_MAX ? INT32_MAX : (int)timeout_ms);
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
            if ((t->state == SLEEPING || t->state == WAITING) && t->wake_at < earliest) earliest = t->wake_at;
        }
        if (earliest == INT64_MAX && !waiting) aion_panic("deadlock: every task is waiting and none can continue");
        int64_t wait = earliest == INT64_MAX ? -1 : earliest - now_ms();
        if (waiting) {
            poll_waiters(wait < 0 && earliest != INT64_MAX ? 0 : wait);
        } else if (wait > 0) {
            fflush(stdout);
            struct timespec ts = {.tv_sec = wait / 1000, .tv_nsec = (wait % 1000) * 1000000};
            nanosleep(&ts, NULL);
        }
    }
}

void aion_notify(void) {
    for (Task *t = all_tasks; t; t = t->next)
        if (t->state == PARKED) t->state = READY;
}

static void task_main(void) {
    reap();
    ((void (*)(void *))(*(void **)current->closure))(current->closure);
    current->state = DONE;
    aion_notify(); /* whoever waits for the end of a task can look again */
    schedule();
    abort(); /* a finished task is never resumed */
}

/* Start running `closure` (a `() -> Unit`) as a new task; it first runs when its creator waits. */
void aion_spawn(void *closure) {
    Task *t = calloc(1, sizeof *t);
    void *stack = mmap(NULL, STACK_SIZE, PROT_READ | PROT_WRITE, MAP_PRIVATE | MAP_ANONYMOUS, -1, 0);
    if (!t || stack == MAP_FAILED) aion_panic("cannot create a task: out of memory");
    mprotect(stack, GUARD_SIZE, PROT_NONE); /* overflowing the stack faults instead of corrupting memory */
    t->stack = stack;
    t->closure = closure;
    t->state = READY;
    getcontext(&t->context);
    t->context.uc_stack.ss_sp = stack;
    t->context.uc_stack.ss_size = STACK_SIZE;
    t->context.uc_link = NULL;
    makecontext(&t->context, task_main, 0);
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
        struct pollfd p = {fd, (short)events, 0};
        fflush(stdout);
        int r;
        do r = poll(&p, 1, timeout_ms > INT32_MAX ? INT32_MAX : (int)timeout_ms); while (r < 0 && errno == EINTR);
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

#endif
