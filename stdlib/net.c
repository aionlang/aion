/*
 * C half of std.net: TCP connections and listeners, and TLS for clients (POSIX).
 *
 * Every socket is non-blocking and every wait goes through aion_wait_fd(), so while one task waits for
 * the network the others keep running.  TLS uses OpenSSL's libssl, loaded at run time with dlopen: a
 * program that never uses https needs no OpenSSL at all, and no header files are required to build.
 */

#define _GNU_SOURCE
#include "aion.h"

#include <arpa/inet.h>
#include <dlfcn.h>
#include <errno.h>
#include <fcntl.h>
#include <netdb.h>
#include <netinet/in.h>
#include <netinet/tcp.h>
#include <poll.h>
#include <signal.h>
#include <stdio.h>
#include <stdlib.h>
#include <string.h>
#include <sys/socket.h>
#include <time.h>
#include <unistd.h>

typedef struct {
    int fd;
    void *ssl;
    bool used, listener;
    int64_t timeout_ms;
} Conn;

static Conn *conns;
static int64_t nconns;
static char error_text[256];

const char *aion_net_error(void) { return aion_string(error_text); }

static int64_t now_ms(void) {
    struct timespec ts;
    clock_gettime(CLOCK_MONOTONIC, &ts);
    return (int64_t)ts.tv_sec * 1000 + ts.tv_nsec / 1000000;
}

static void set_error(const char *what) { snprintf(error_text, sizeof error_text, "%s", what); }
static void set_errno(const char *what) { snprintf(error_text, sizeof error_text, "%s: %s", what, strerror(errno)); }

static Conn *get(int64_t h) {
    if (h < 1 || h > nconns || !conns[h - 1].used) return NULL;
    return &conns[h - 1];
}

static int64_t add(int fd, bool listener, int64_t timeout_ms) {
    static bool init;
    if (!init) signal(SIGPIPE, SIG_IGN), init = true;
    int64_t i = 0;
    while (i < nconns && conns[i].used) i++;
    if (i == nconns) {
        conns = realloc(conns, (size_t)(nconns + 16) * sizeof *conns);
        if (!conns) aion_panic("out of memory");
        memset(conns + nconns, 0, 16 * sizeof *conns);
        nconns += 16;
    }
    conns[i] = (Conn){.fd = fd, .used = true, .listener = listener, .timeout_ms = timeout_ms};
    return i + 1;
}

static void make_nonblocking(int fd) {
    fcntl(fd, F_SETFL, fcntl(fd, F_GETFL, 0) | O_NONBLOCK);
    fcntl(fd, F_SETFD, FD_CLOEXEC);
}

/* Milliseconds left until `deadline` (-1 = no deadline). */
static int64_t remaining(int64_t deadline) {
    if (deadline < 0) return -1;
    int64_t left = deadline - now_ms();
    return left < 0 ? 0 : left;
}

static int64_t deadline_for(int64_t timeout_ms) { return timeout_ms < 0 ? -1 : now_ms() + timeout_ms; }

/* ── Connecting and listening ─────────────────────────────────── */

int64_t aion_net_connect(const char *host, int64_t port, int64_t timeout_ms) {
    char service[16];
    snprintf(service, sizeof service, "%lld", (long long)port);
    struct addrinfo hints, *list = NULL;
    memset(&hints, 0, sizeof hints);
    hints.ai_socktype = SOCK_STREAM;
    int rc = getaddrinfo(host, service, &hints, &list);
    if (rc != 0) {
        snprintf(error_text, sizeof error_text, "cannot resolve %s: %s", host, gai_strerror(rc));
        return -1;
    }
    int64_t deadline = deadline_for(timeout_ms);
    snprintf(error_text, sizeof error_text, "cannot connect to %s:%s", host, service);
    for (struct addrinfo *a = list; a; a = a->ai_next) {
        int fd = socket(a->ai_family, a->ai_socktype, a->ai_protocol);
        if (fd < 0) continue;
        make_nonblocking(fd);
        int r = connect(fd, a->ai_addr, a->ai_addrlen);
        if (r < 0 && errno == EINPROGRESS) {
            int w = aion_wait_fd(fd, POLLOUT, remaining(deadline));
            if (w == 0) {
                snprintf(error_text, sizeof error_text, "connecting to %s:%s timed out", host, service);
                close(fd);
                continue;
            }
            int err = 0;
            socklen_t len = sizeof err;
            getsockopt(fd, SOL_SOCKET, SO_ERROR, &err, &len);
            if (w < 0 || err) {
                snprintf(error_text, sizeof error_text, "cannot connect to %s:%s: %s", host, service, strerror(err ? err : errno));
                close(fd);
                continue;
            }
        } else if (r < 0) {
            snprintf(error_text, sizeof error_text, "cannot connect to %s:%s: %s", host, service, strerror(errno));
            close(fd);
            continue;
        }
        int one = 1;
        setsockopt(fd, IPPROTO_TCP, TCP_NODELAY, &one, sizeof one);
        freeaddrinfo(list);
        return add(fd, false, timeout_ms);
    }
    freeaddrinfo(list);
    return -1;
}

/* Listen on `host` (empty = every interface) and `port` (0 = pick a free one). */
int64_t aion_net_listen(const char *host, int64_t port, int64_t backlog) {
    char service[16];
    snprintf(service, sizeof service, "%lld", (long long)port);
    struct addrinfo hints, *list = NULL;
    memset(&hints, 0, sizeof hints);
    hints.ai_socktype = SOCK_STREAM;
    hints.ai_flags = AI_PASSIVE;
    int rc = getaddrinfo(*host ? host : NULL, service, &hints, &list);
    if (rc != 0) {
        snprintf(error_text, sizeof error_text, "cannot resolve %s: %s", host, gai_strerror(rc));
        return -1;
    }
    for (struct addrinfo *a = list; a; a = a->ai_next) {
        int fd = socket(a->ai_family, a->ai_socktype, a->ai_protocol);
        if (fd < 0) continue;
        int one = 1;
        setsockopt(fd, SOL_SOCKET, SO_REUSEADDR, &one, sizeof one);
        if (bind(fd, a->ai_addr, a->ai_addrlen) != 0 || listen(fd, (int)backlog) != 0) {
            set_errno("cannot listen");
            close(fd);
            continue;
        }
        make_nonblocking(fd);
        freeaddrinfo(list);
        return add(fd, true, -1);
    }
    freeaddrinfo(list);
    if (!*error_text) set_error("cannot listen");
    return -1;
}

/* The local port of a listener or connection. */
/* ── Name lookup ─────────────────────────────────────────────────── */

/* The addresses of a host name, one per line: IPv4 first, each once.  "" when it cannot be resolved.
 * (getaddrinfo blocks, so other tasks wait for a slow answer.) */
const char *aion_net_lookup(const char *host) {
    struct addrinfo hints, *list = NULL;
    memset(&hints, 0, sizeof hints);
    hints.ai_socktype = SOCK_STREAM;
    int rc = getaddrinfo(host, NULL, &hints, &list);
    if (rc != 0) {
        snprintf(error_text, sizeof error_text, "cannot resolve %s: %s", host, gai_strerror(rc));
        return aion_string("");
    }
    char out[4096] = "";
    size_t used = 0;
    for (int family = AF_INET; family != 0; family = family == AF_INET ? AF_INET6 : 0) {
        for (struct addrinfo *a = list; a; a = a->ai_next) {
            char address[INET6_ADDRSTRLEN];
            if (a->ai_family != family || getnameinfo(a->ai_addr, a->ai_addrlen, address, sizeof address, NULL, 0, NI_NUMERICHOST) != 0)
                continue;
            size_t n = strlen(address);
            if (used + n + 2 > sizeof out) continue;
            /* skip an address already listed (a name is often returned once per socket type) */
            bool seen = false;
            for (size_t at = 0; at < used && !seen;) {
                const char *eol = strchr(out + at, '\n');
                size_t len = eol ? (size_t)(eol - (out + at)) : strlen(out + at);
                seen = len == n && memcmp(out + at, address, n) == 0;
                at += len + 1;
            }
            if (seen) continue;
            memcpy(out + used, address, n);
            used += n;
            out[used++] = '\n';
            out[used] = 0;
        }
    }
    freeaddrinfo(list);
    if (used > 0) out[used - 1] = 0;
    else snprintf(error_text, sizeof error_text, "%s has no IPv4 or IPv6 address", host);
    return aion_string(out);
}

/* The name of an IPv4 or IPv6 address, or NULL if it has none (or is not an address). */
const char *aion_net_reverse(const char *ip) {
    struct sockaddr_storage storage;
    memset(&storage, 0, sizeof storage);
    socklen_t length;
    struct sockaddr_in *v4 = (struct sockaddr_in *)&storage;
    struct sockaddr_in6 *v6 = (struct sockaddr_in6 *)&storage;
    if (inet_pton(AF_INET, ip, &v4->sin_addr) == 1) {
        v4->sin_family = AF_INET;
        length = sizeof *v4;
    } else if (inet_pton(AF_INET6, ip, &v6->sin6_addr) == 1) {
        v6->sin6_family = AF_INET6;
        length = sizeof *v6;
    } else {
        snprintf(error_text, sizeof error_text, "%s is not an IP address", ip);
        return NULL;
    }
    char name[1025];
    int rc = getnameinfo((struct sockaddr *)&storage, length, name, sizeof name, NULL, 0, NI_NAMEREQD);
    if (rc != 0) {
        snprintf(error_text, sizeof error_text, "no name for %s: %s", ip, gai_strerror(rc));
        return NULL;
    }
    return aion_string(name);
}

int64_t aion_net_port(int64_t h) {
    Conn *c = get(h);
    if (!c) return -1;
    struct sockaddr_storage ss;
    socklen_t len = sizeof ss;
    if (getsockname(c->fd, (struct sockaddr *)&ss, &len) != 0) return -1;
    if (ss.ss_family == AF_INET) return ntohs(((struct sockaddr_in *)&ss)->sin_port);
    if (ss.ss_family == AF_INET6) return ntohs(((struct sockaddr_in6 *)&ss)->sin6_port);
    return -1;
}

/* Wait for a connection to a listener; -1 on timeout or when the listener was closed. */
int64_t aion_net_accept(int64_t h, int64_t timeout_ms) {
    int64_t deadline = deadline_for(timeout_ms);
    for (;;) {
        Conn *c = get(h);
        if (!c || !c->listener) return set_error("the listener is closed"), -1;
        int fd = accept(c->fd, NULL, NULL);
        if (fd >= 0) {
            make_nonblocking(fd);
            int one = 1;
            setsockopt(fd, IPPROTO_TCP, TCP_NODELAY, &one, sizeof one);
            return add(fd, false, 30000);
        }
        if (errno != EAGAIN && errno != EWOULDBLOCK && errno != EINTR) return set_errno("accept failed"), -1;
        int w = aion_wait_fd(c->fd, POLLIN, remaining(deadline));
        if (w == 0) return set_error("timed out waiting for a connection"), -1;
        if (w < 0) return set_error("waiting for a connection failed"), -1;
    }
}

void aion_net_set_timeout(int64_t h, int64_t timeout_ms) {
    Conn *c = get(h);
    if (c) c->timeout_ms = timeout_ms;
}

/* The address of the other end, "ip:port". */
const char *aion_net_peer(int64_t h) {
    Conn *c = get(h);
    if (!c) return aion_string("");
    struct sockaddr_storage ss;
    socklen_t len = sizeof ss;
    char ip[INET6_ADDRSTRLEN] = "", text[INET6_ADDRSTRLEN + 16];
    int port = 0;
    if (getpeername(c->fd, (struct sockaddr *)&ss, &len) != 0) return aion_string("");
    if (ss.ss_family == AF_INET) {
        inet_ntop(AF_INET, &((struct sockaddr_in *)&ss)->sin_addr, ip, sizeof ip);
        port = ntohs(((struct sockaddr_in *)&ss)->sin_port);
    } else if (ss.ss_family == AF_INET6) {
        inet_ntop(AF_INET6, &((struct sockaddr_in6 *)&ss)->sin6_addr, ip, sizeof ip);
        port = ntohs(((struct sockaddr_in6 *)&ss)->sin6_port);
    }
    snprintf(text, sizeof text, "%s:%d", ip, port);
    return aion_string(text);
}

/* ── TLS (libssl through dlopen) ──────────────────────────────── */

static struct {
    bool tried, ok;
    const void *(*TLS_client_method)(void);
    void *(*SSL_CTX_new)(const void *);
    void (*SSL_CTX_free)(void *);
    int (*SSL_CTX_set_default_verify_paths)(void *);
    int (*SSL_CTX_load_verify_locations)(void *, const char *, const char *);
    void (*SSL_CTX_set_verify)(void *, int, void *);
    void *(*SSL_new)(void *);
    int (*SSL_set_fd)(void *, int);
    long (*SSL_ctrl)(void *, int, long, void *);
    int (*SSL_set1_host)(void *, const char *);
    int (*SSL_connect)(void *);
    int (*SSL_read)(void *, void *, int);
    int (*SSL_write)(void *, const void *, int);
    int (*SSL_get_error)(const void *, int);
    int (*SSL_shutdown)(void *);
    void (*SSL_free)(void *);
    long (*SSL_get_verify_result)(const void *);
    const char *(*X509_verify_cert_error_string)(long);
    unsigned long (*ERR_get_error)(void);
    void (*ERR_error_string_n)(unsigned long, char *, size_t);
    void (*ERR_clear_error)(void);
} tls;

static void *open_library(const char *const *names) {
    for (; *names; names++) {
        void *lib = dlopen(*names, RTLD_NOW | RTLD_GLOBAL);
        if (lib) return lib;
    }
    return NULL;
}

static bool load_tls(void) {
    if (tls.tried) return tls.ok;
    tls.tried = true;
    static const char *const crypto_names[] = {"libcrypto.so.3", "libcrypto.so.1.1", "libcrypto.so", "libcrypto.3.dylib", "libcrypto.dylib", NULL};
    static const char *const ssl_names[] = {"libssl.so.3", "libssl.so.1.1", "libssl.so", "libssl.3.dylib", "libssl.dylib", NULL};
    void *crypto = open_library(crypto_names), *ssl = open_library(ssl_names);
    if (!ssl || !crypto) {
        set_error("HTTPS needs OpenSSL: install libssl (for example the libssl3 or openssl package)");
        return false;
    }
#define LOAD(lib, name) \
    if (!(*(void **)&tls.name = dlsym(lib, #name))) { set_error("the installed OpenSSL is missing " #name); return false; }
    LOAD(ssl, TLS_client_method) LOAD(ssl, SSL_CTX_new) LOAD(ssl, SSL_CTX_free) LOAD(ssl, SSL_CTX_set_default_verify_paths)
    LOAD(ssl, SSL_CTX_load_verify_locations) LOAD(ssl, SSL_CTX_set_verify) LOAD(ssl, SSL_new) LOAD(ssl, SSL_set_fd)
    LOAD(ssl, SSL_ctrl) LOAD(ssl, SSL_set1_host) LOAD(ssl, SSL_connect) LOAD(ssl, SSL_read) LOAD(ssl, SSL_write)
    LOAD(ssl, SSL_get_error) LOAD(ssl, SSL_shutdown) LOAD(ssl, SSL_free) LOAD(ssl, SSL_get_verify_result)
    LOAD(crypto, X509_verify_cert_error_string) LOAD(crypto, ERR_get_error) LOAD(crypto, ERR_error_string_n)
    LOAD(crypto, ERR_clear_error)
#undef LOAD
    tls.ok = true;
    return true;
}

bool aion_net_tls_available(void) { return load_tls(); }

static void tls_error(const char *what) {
    unsigned long e = tls.ERR_get_error();
    char detail[160] = "";
    if (e) tls.ERR_error_string_n(e, detail, sizeof detail);
    snprintf(error_text, sizeof error_text, "%s%s%s", what, *detail ? ": " : "", detail);
}

/* Run the TLS handshake on a connected socket.  `verify` checks the server's certificate and host name
 * against the system's trusted roots (or `cafile`, or the SSL_CERT_FILE environment variable). */
bool aion_net_start_tls(int64_t h, const char *host, bool verify, const char *cafile) {
    Conn *c = get(h);
    if (!c) return set_error("the connection is closed"), false;
    if (!load_tls()) return false;
    void *ctx = tls.SSL_CTX_new(tls.TLS_client_method());
    if (!ctx) return tls_error("cannot create a TLS context"), false;
    if (verify) {
        if (*cafile ? !tls.SSL_CTX_load_verify_locations(ctx, cafile, NULL) : !tls.SSL_CTX_set_default_verify_paths(ctx)) {
            tls_error("cannot load trusted certificates");
            tls.SSL_CTX_free(ctx);
            return false;
        }
        tls.SSL_CTX_set_verify(ctx, 1, NULL); /* SSL_VERIFY_PEER */
    }
    void *ssl = tls.SSL_new(ctx);
    tls.SSL_CTX_free(ctx);
    if (!ssl) return tls_error("cannot create a TLS session"), false;
    tls.SSL_set_fd(ssl, c->fd);
    tls.SSL_ctrl(ssl, 55, 0, (void *)host); /* SSL_set_tlsext_host_name: SNI */
    if (verify) tls.SSL_set1_host(ssl, host);
    int64_t deadline = deadline_for(c->timeout_ms);
    for (;;) {
        tls.ERR_clear_error();
        int r = tls.SSL_connect(ssl);
        if (r == 1) break;
        int err = tls.SSL_get_error(ssl, r);
        if (err == 2 || err == 3) { /* SSL_ERROR_WANT_READ / WANT_WRITE */
            int w = aion_wait_fd(c->fd, err == 2 ? POLLIN : POLLOUT, remaining(deadline));
            if (w > 0) continue;
            set_error(w == 0 ? "the TLS handshake timed out" : "waiting for the TLS handshake failed");
        } else {
            long vr = tls.SSL_get_verify_result(ssl);
            if (verify && vr != 0) snprintf(error_text, sizeof error_text, "certificate verification failed: %s", tls.X509_verify_cert_error_string(vr));
            else tls_error("TLS handshake failed");
        }
        tls.SSL_free(ssl);
        return false;
    }
    c->ssl = ssl;
    return true;
}

/* ── Sending and receiving ────────────────────────────────────── */

/* Send all of `data`. */
bool aion_net_send(int64_t h, const void *data) {
    Conn *c = get(h);
    if (!c) return set_error("the connection is closed"), false;
    int64_t total = aion_bytes_size(data), sent = 0;
    int64_t deadline = deadline_for(c->timeout_ms);
    while (sent < total) {
        c = get(h);
        if (!c) return set_error("the connection was closed"), false;
        const uint8_t *p = (const uint8_t *)data + sent;
        int want = 0;
        if (c->ssl) {
            tls.ERR_clear_error();
            int n = tls.SSL_write(c->ssl, p, (int)(total - sent > 1 << 20 ? 1 << 20 : total - sent));
            if (n > 0) { sent += n; continue; }
            int err = tls.SSL_get_error(c->ssl, n);
            if (err != 2 && err != 3) return tls_error("TLS write failed"), false;
            want = err == 2 ? POLLIN : POLLOUT;
        } else {
            ssize_t n = send(c->fd, p, (size_t)(total - sent), MSG_NOSIGNAL);
            if (n >= 0) { sent += n; continue; }
            if (errno != EAGAIN && errno != EWOULDBLOCK && errno != EINTR) return set_errno("send failed"), false;
            want = POLLOUT;
        }
        int w = aion_wait_fd(c->fd, want, remaining(deadline));
        if (w == 0) return set_error("sending timed out"), false;
        if (w < 0) return set_error("waiting to send failed"), false;
    }
    return true;
}

/*
 * Read up to `max` bytes: whatever arrives first.  An empty result means the other side closed the connection;
 * NULL means an error or a timeout (aion_net_error says which).
 */
void *aion_net_receive(int64_t h, int64_t max) {
    Conn *c = get(h);
    if (!c) return set_error("the connection is closed"), NULL;
    if (max < 1) max = 1;
    if (max > (1 << 24)) max = 1 << 24;
    uint8_t stack[65536];
    uint8_t *buf = max <= (int64_t)sizeof stack ? stack : malloc((size_t)max);
    if (!buf) aion_panic("out of memory");
    int64_t deadline = deadline_for(c->timeout_ms);
    void *result = NULL;
    for (;;) {
        c = get(h);
        if (!c) { set_error("the connection was closed"); break; }
        int want = POLLIN;
        if (c->ssl) {
            tls.ERR_clear_error();
            int n = tls.SSL_read(c->ssl, buf, (int)max);
            if (n > 0) { result = aion_bytes_new(n), memcpy(aion_bytes_data(result), buf, (size_t)n); break; }
            int err = tls.SSL_get_error(c->ssl, n);
            if (err == 6 || (err == 5 && errno == 0) || (err == 1 && n == 0)) { result = aion_bytes_new(0); break; } /* closed */
            if (err != 2 && err != 3) { tls_error("TLS read failed"); break; }
            want = err == 2 ? POLLIN : POLLOUT;
        } else {
            ssize_t n = recv(c->fd, buf, (size_t)max, 0);
            if (n > 0) { result = aion_bytes_new(n), memcpy(aion_bytes_data(result), buf, (size_t)n); break; }
            if (n == 0) { result = aion_bytes_new(0); break; }
            if (errno != EAGAIN && errno != EWOULDBLOCK && errno != EINTR) { set_errno("receive failed"); break; }
        }
        int w = aion_wait_fd(c->fd, want, remaining(deadline));
        if (w == 0) { set_error("timed out waiting for data"); break; }
        if (w < 0) { set_error("waiting for data failed"); break; }
    }
    if (buf != stack) free(buf);
    return result;
}

void aion_net_close(int64_t h) {
    Conn *c = get(h);
    if (!c) return;
    if (c->ssl) {
        tls.SSL_shutdown(c->ssl);
        tls.SSL_free(c->ssl);
    }
    close(c->fd);
    c->used = false;
}
