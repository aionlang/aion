/*
 * C half of std.sockets: thin wrappers over BSD sockets / Winsock.
 * The Aion half (sockets.aion) declares these with `extern fn`.
 */

#include "aion.h"

#include <stdlib.h>
#include <string.h>

#ifdef _WIN32
#include <winsock2.h>
#include <ws2tcpip.h>
typedef SOCKET socket_t;
#define CLOSE_SOCKET closesocket
#define SEND_FLAGS 0
#else
#include <arpa/inet.h>
#include <errno.h>
#include <fcntl.h>
#include <netdb.h>
#include <netinet/in.h>
#include <sys/socket.h>
#include <unistd.h>
typedef int socket_t;
#define CLOSE_SOCKET close
#ifdef MSG_NOSIGNAL
#define SEND_FLAGS MSG_NOSIGNAL /* a closed peer must not kill the program */
#else
#define SEND_FLAGS 0
#endif
#endif

#define FD(x) ((socket_t)(x))

#ifdef _WIN32
static void ensure_init(void) {
    static int ready = 0;
    WSADATA data;
    if (!ready && WSAStartup(MAKEWORD(2, 2), &data) != 0) aion_panic("WSAStartup failed");
    ready = 1;
}
#else
static void ensure_init(void) {}
#endif

int64_t aion_socket_create(int64_t domain, int64_t type, int64_t protocol) {
    ensure_init();
    return (int64_t)socket((int)domain, (int)type, (int)protocol);
}

int64_t aion_socket_bind(int64_t fd, int64_t port) {
    struct sockaddr_in addr;
    memset(&addr, 0, sizeof addr);
    addr.sin_family = AF_INET;
    addr.sin_addr.s_addr = htonl(INADDR_ANY);
    addr.sin_port = htons((unsigned short)port);
    return bind(FD(fd), (struct sockaddr *)&addr, sizeof addr);
}

int64_t aion_socket_connect(int64_t fd, const char *host, int64_t port) {
    struct sockaddr_in addr;
    memset(&addr, 0, sizeof addr);
    addr.sin_family = AF_INET;
    addr.sin_port = htons((unsigned short)port);
    if (inet_pton(AF_INET, host, &addr.sin_addr) <= 0) { /* not a dotted quad: resolve it */
        struct hostent *he = gethostbyname(host);
        if (!he) return -1;
        memcpy(&addr.sin_addr, he->h_addr_list[0], (size_t)he->h_length);
    }
    return connect(FD(fd), (struct sockaddr *)&addr, sizeof addr);
}

int64_t aion_socket_listen(int64_t fd, int64_t backlog) { return listen(FD(fd), (int)backlog); }

int64_t aion_socket_accept(int64_t fd) {
    struct sockaddr_in peer;
    socklen_t len = sizeof peer;
    return (int64_t)accept(FD(fd), (struct sockaddr *)&peer, &len);
}

/* Send the whole string; returns bytes sent, or -1 on error. */
int64_t aion_socket_send(int64_t fd, const char *data) {
    size_t total = strlen(data), sent = 0;
    while (sent < total) {
        int n = (int)send(FD(fd), data + sent, (int)(total - sent), SEND_FLAGS);
        if (n <= 0) return -1;
        sent += (size_t)n;
    }
    return (int64_t)sent;
}

/* Read up to `max_bytes`; returns "" on end-of-stream or error. */
const char *aion_socket_recv(int64_t fd, int64_t max_bytes) {
    if (max_bytes <= 0) return aion_string("");
    char *buf = malloc((size_t)max_bytes);
    if (!buf) aion_panic("out of memory");
    int n = (int)recv(FD(fd), buf, (int)max_bytes, 0);
    const char *result = aion_string_n(buf, n > 0 ? (size_t)n : 0);
    free(buf);
    return result;
}

int64_t aion_socket_close(int64_t fd) { return CLOSE_SOCKET(FD(fd)); }

/* how: 0 = reading, 1 = writing, 2 = both */
int64_t aion_socket_shutdown(int64_t fd, int64_t how) {
#ifdef _WIN32
    int mode = how == 0 ? SD_RECEIVE : how == 1 ? SD_SEND : SD_BOTH;
#else
    int mode = how == 0 ? SHUT_RD : how == 1 ? SHUT_WR : SHUT_RDWR;
#endif
    return shutdown(FD(fd), mode);
}

/* option: 1 = SO_REUSEADDR, 2 = non-blocking */
int64_t aion_socket_setopt(int64_t fd, int64_t option, int64_t value) {
    int on = value != 0;
    if (option == 1) return setsockopt(FD(fd), SOL_SOCKET, SO_REUSEADDR, (const char *)&on, sizeof on);
    if (option == 2) {
#ifdef _WIN32
        u_long mode = on;
        return ioctlsocket(FD(fd), FIONBIO, &mode);
#else
        int flags = fcntl(FD(fd), F_GETFL, 0);
        if (flags == -1) return -1;
        return fcntl(FD(fd), F_SETFL, on ? (flags | O_NONBLOCK) : (flags & ~O_NONBLOCK));
#endif
    }
    return -1;
}

int64_t aion_socket_error(void) {
#ifdef _WIN32
    return WSAGetLastError();
#else
    return errno;
#endif
}

void aion_socket_cleanup(void) {
#ifdef _WIN32
    WSACleanup();
#endif
}
