/*
 * C half of std.os: environment, process information and running other programs (POSIX).
 */

#include "aion.h"

#include <errno.h>
#include <fcntl.h>
#include <poll.h>
#include <signal.h>
#include <stdio.h>
#include <stdlib.h>
#include <string.h>
#include <sys/wait.h>
#include <unistd.h>

extern char **environ;

/* Variable NAME, or NULL when it is not set. */
const char *aion_os_env(const char *name) {
    const char *v = getenv(name);
    return v ? aion_string(v) : NULL;
}

bool aion_os_set_env(const char *name, const char *value) { return setenv(name, value, 1) == 0; }
bool aion_os_unset_env(const char *name) { return unsetenv(name) == 0; }

/* Every variable as NAME=value, one per line. */
const char *aion_os_environ(void) {
    size_t bytes = 0;
    for (char **e = environ; e && *e; e++) bytes += strlen(*e) + 1;
    char *buf = malloc(bytes + 1), *at = buf;
    if (!buf) aion_panic("out of memory");
    for (char **e = environ; e && *e; e++) {
        size_t n = strlen(*e);
        memcpy(at, *e, n);
        at += n;
        *at++ = '\n';
    }
    if (at > buf) at--; /* no line break after the last one */
    const char *out = aion_string_n(buf, (size_t)(at - buf));
    free(buf);
    return out;
}

void aion_os_exit(int64_t code) {
    fflush(NULL);
    exit((int)code);
}

int64_t aion_os_pid(void) { return (int64_t)getpid(); }
int64_t aion_os_cpu_count(void) {
    long n = sysconf(_SC_NPROCESSORS_ONLN);
    return n > 0 ? n : 1;
}

const char *aion_os_hostname(void) {
    char buf[256];
    if (gethostname(buf, sizeof buf) != 0) return aion_string("");
    buf[sizeof buf - 1] = '\0';
    return aion_string(buf);
}

const char *aion_os_platform(void) {
#if defined(__linux__)
    return aion_string("linux");
#elif defined(__APPLE__)
    return aion_string("macos");
#elif defined(__FreeBSD__) || defined(__OpenBSD__) || defined(__NetBSD__)
    return aion_string("bsd");
#else
    return aion_string("unknown");
#endif
}

const char *aion_os_arch(void) {
#if defined(__x86_64__)
    return aion_string("x86_64");
#elif defined(__aarch64__)
    return aion_string("aarch64");
#elif defined(__i386__)
    return aion_string("x86");
#elif defined(__arm__)
    return aion_string("arm");
#else
    return aion_string("unknown");
#endif
}

/* ── Running programs ─────────────────────────────────────────── */

static char *captured_out, *captured_err;
static size_t captured_out_len, captured_err_len;

const char *aion_os_captured_stdout(void) { return aion_string_n(captured_out ? captured_out : "", captured_out_len); }
const char *aion_os_captured_stderr(void) { return aion_string_n(captured_err ? captured_err : "", captured_err_len); }

static void append(char **buf, size_t *len, const char *data, size_t n) {
    *buf = realloc(*buf, *len + n + 1);
    if (!*buf) aion_panic("out of memory");
    memcpy(*buf + *len, data, n);
    *len += n;
    (*buf)[*len] = '\0';
}

#define ARG_SEPARATOR '\x1f'

/* The program's arguments arrive in one string, separated by 0x1F, so a list can cross the boundary. */
static char **split_args(const char *program, const char *joined, int *count) {
    int n = 1;
    if (*joined) {
        n++;
        for (const char *p = joined; *p; p++) n += *p == ARG_SEPARATOR;
    }
    char **argv = calloc((size_t)n + 1, sizeof *argv);
    if (!argv) aion_panic("out of memory");
    argv[0] = strdup(program);
    int at = 1;
    if (*joined) {
        const char *start = joined;
        for (const char *p = joined;; p++) {
            if (*p == ARG_SEPARATOR || *p == '\0') {
                argv[at++] = strndup(start, (size_t)(p - start));
                start = p + 1;
                if (*p == '\0') break;
            }
        }
    }
    *count = at;
    return argv;
}

static int64_t wait_status(int status) {
    if (WIFEXITED(status)) return WEXITSTATUS(status);
    if (WIFSIGNALED(status)) return 128 + WTERMSIG(status);
    return -1;
}

/*
 * Run `program` with the arguments, feed it `input`, and collect what it writes to stdout and stderr
 * (read them with the aion_os_captured_* calls).  With `capture` false the program shares this
 * program's input and output instead.  Returns the exit status, 128+N if signal N ended it, -1 if it
 * could not be started.
 */
int64_t aion_os_exec(const char *program, const char *args, const char *input, bool capture) {
    free(captured_out), free(captured_err);
    captured_out = captured_err = NULL;
    captured_out_len = captured_err_len = 0;

    int argc;
    char **argv = split_args(program, args, &argc);
    int in_pipe[2] = {-1, -1}, out_pipe[2] = {-1, -1}, err_pipe[2] = {-1, -1}, exec_pipe[2];
    if (pipe(exec_pipe) != 0) return -1;
    fcntl(exec_pipe[1], F_SETFD, FD_CLOEXEC);
    if (capture && (pipe(in_pipe) != 0 || pipe(out_pipe) != 0 || pipe(err_pipe) != 0)) return -1;

    fflush(NULL);
    pid_t pid = fork();
    if (pid < 0) return -1;
    if (pid == 0) {
        if (capture) {
            dup2(in_pipe[0], 0), dup2(out_pipe[1], 1), dup2(err_pipe[1], 2);
            close(in_pipe[0]), close(in_pipe[1]), close(out_pipe[0]);
            close(out_pipe[1]), close(err_pipe[0]), close(err_pipe[1]);
        }
        close(exec_pipe[0]);
        execvp(program, argv);
        int e = errno;
        (void)!write(exec_pipe[1], &e, sizeof e);
        _exit(127);
    }
    close(exec_pipe[1]);
    for (int i = 0; i < argc; i++) free(argv[i]);
    free(argv);

    int exec_errno = 0;
    ssize_t got = read(exec_pipe[0], &exec_errno, sizeof exec_errno);
    close(exec_pipe[0]);
    if (got > 0) {
        int status;
        waitpid(pid, &status, 0);
        if (capture) {
            close(in_pipe[0]), close(in_pipe[1]), close(out_pipe[0]);
            close(out_pipe[1]), close(err_pipe[0]), close(err_pipe[1]);
        }
        append(&captured_err, &captured_err_len, strerror(exec_errno), strlen(strerror(exec_errno)));
        return -1;
    }

    if (capture) {
        close(in_pipe[0]), close(out_pipe[1]), close(err_pipe[1]);
        signal(SIGPIPE, SIG_IGN);
        size_t sent = 0, total = strlen(input);
        if (total == 0) close(in_pipe[1]), in_pipe[1] = -1;
        struct pollfd fds[3] = {{in_pipe[1], POLLOUT, 0}, {out_pipe[0], POLLIN, 0}, {err_pipe[0], POLLIN, 0}};
        char buf[8192];
        while (fds[1].fd >= 0 || fds[2].fd >= 0) {
            if (poll(fds, 3, -1) < 0 && errno != EINTR) break;
            if (fds[0].fd >= 0 && fds[0].revents) {
                ssize_t n = write(fds[0].fd, input + sent, total - sent);
                if (n > 0) sent += (size_t)n;
                if (n < 0 || sent == total) close(fds[0].fd), fds[0].fd = -1;
            }
            for (int i = 1; i <= 2; i++) {
                if (fds[i].fd < 0 || !fds[i].revents) continue;
                ssize_t n = read(fds[i].fd, buf, sizeof buf);
                if (n > 0) {
                    if (i == 1) append(&captured_out, &captured_out_len, buf, (size_t)n);
                    else append(&captured_err, &captured_err_len, buf, (size_t)n);
                } else {
                    close(fds[i].fd), fds[i].fd = -1;
                }
            }
        }
        if (fds[0].fd >= 0) close(fds[0].fd);
    }
    int status = 0;
    while (waitpid(pid, &status, 0) < 0 && errno == EINTR) {}
    return wait_status(status);
}
