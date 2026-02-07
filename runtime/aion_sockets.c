/*
 * Aion Standard Library — sockets module (low-level C bindings)
 *
 * These are the raw system call wrappers. The high-level API is
 * implemented in Aion (stdlib/sockets.aion) using these primitives.
 */

#include <stdio.h>
#include <stdlib.h>
#include <string.h>

#ifdef _WIN32
    #include <winsock2.h>
    #include <ws2tcpip.h>
    #pragma comment(lib, "ws2_32.lib")

    typedef SOCKET socket_t;
    #define INVALID_SOCKET_FD INVALID_SOCKET
    #define CLOSE_SOCKET closesocket
#else
    #include <sys/socket.h>
    #include <sys/types.h>
    #include <netinet/in.h>
    #include <arpa/inet.h>
    #include <netdb.h>
    #include <unistd.h>
    #include <fcntl.h>
    #include <errno.h>

    typedef int socket_t;
    #define INVALID_SOCKET_FD -1
    #define CLOSE_SOCKET close
#endif

extern void aion_panic(const char *msg);

static int winsock_initialized = 0;

/* ══════════════════════════════════════════════════════════════════
 * Internal helpers (not exposed to Aion)
 * ══════════════════════════════════════════════════════════════════ */

static void ensure_init(void) {
#ifdef _WIN32
    if (!winsock_initialized) {
        WSADATA wsa_data;
        if (WSAStartup(MAKEWORD(2, 2), &wsa_data) != 0) {
            aion_panic("WSAStartup failed");
        }
        winsock_initialized = 1;
    }
#endif
}

/* ══════════════════════════════════════════════════════════════════
 * Low-level primitives exposed to Aion
 *
 * All integer values use `long long` (Aion Int = i64).
 * All string values use `const char*` (Aion String = ptr).
 * ══════════════════════════════════════════════════════════════════ */

/* Create a socket: domain (2=AF_INET), type (1=STREAM, 2=DGRAM), protocol (0=auto) */
long long aion_socket_raw_create(long long domain, long long type, long long protocol) {
    ensure_init();
    socket_t sock = socket((int)domain, (int)type, (int)protocol);
    return (long long)sock;
}

/* Bind to address: sock, port */
long long aion_socket_raw_bind(long long sock_fd, long long port) {
    socket_t sock = (socket_t)sock_fd;

    struct sockaddr_in addr;
    memset(&addr, 0, sizeof(addr));
    addr.sin_family = AF_INET;
    addr.sin_addr.s_addr = INADDR_ANY;
    addr.sin_port = htons((unsigned short)port);

    return (long long)bind(sock, (struct sockaddr*)&addr, sizeof(addr));
}

/* Connect to host:port */
long long aion_socket_raw_connect(long long sock_fd, const char* host, long long port) {
    socket_t sock = (socket_t)sock_fd;

    struct sockaddr_in server_addr;
    memset(&server_addr, 0, sizeof(server_addr));
    server_addr.sin_family = AF_INET;
    server_addr.sin_port = htons((unsigned short)port);

    /* Try IP address first */
    if (inet_pton(AF_INET, host, &server_addr.sin_addr) <= 0) {
        /* Resolve hostname */
        struct hostent* he = gethostbyname(host);
        if (he == NULL) {
            return -1;
        }
        memcpy(&server_addr.sin_addr, he->h_addr_list[0], he->h_length);
    }

    return (long long)connect(sock, (struct sockaddr*)&server_addr, sizeof(server_addr));
}

/* Listen with backlog */
long long aion_socket_raw_listen(long long sock_fd, long long backlog) {
    socket_t sock = (socket_t)sock_fd;
    return (long long)listen(sock, (int)backlog);
}

/* Accept connection — returns new socket fd */
long long aion_socket_raw_accept(long long sock_fd) {
    socket_t sock = (socket_t)sock_fd;

    struct sockaddr_in client_addr;
    socklen_t client_len = sizeof(client_addr);

    socket_t client = accept(sock, (struct sockaddr*)&client_addr, &client_len);
    return (long long)client;
}

/* Send data — returns bytes sent or -1 */
long long aion_socket_raw_send(long long sock_fd, const char* data, long long len) {
    socket_t sock = (socket_t)sock_fd;
    int sent = send(sock, data, (int)len, 0);
    return (long long)sent;
}

/* Receive data — returns heap-allocated string (never NULL) */
const char* aion_socket_raw_recv(long long sock_fd, long long max_bytes) {
    socket_t sock = (socket_t)sock_fd;
    int buf_size = (int)max_bytes;

    char* buffer = (char*)malloc(buf_size + 1);
    if (!buffer) {
        char* empty = (char*)malloc(1);
        if (empty) empty[0] = '\0';
        return empty;
    }

    int received = recv(sock, buffer, buf_size, 0);
    if (received <= 0) {
        free(buffer);
        char* empty = (char*)malloc(1);
        if (empty) empty[0] = '\0';
        return empty;
    }

    buffer[received] = '\0';
    return buffer;
}

/* Close socket */
long long aion_socket_raw_close(long long sock_fd) {
    socket_t sock = (socket_t)sock_fd;
    return (long long)CLOSE_SOCKET(sock);
}

/* Shutdown socket: how (0=RD, 1=WR, 2=RDWR) */
long long aion_socket_raw_shutdown(long long sock_fd, long long how) {
    socket_t sock = (socket_t)sock_fd;

#ifdef _WIN32
    int how_val = (how == 0) ? SD_RECEIVE : (how == 1) ? SD_SEND : SD_BOTH;
#else
    int how_val = (how == 0) ? SHUT_RD : (how == 1) ? SHUT_WR : SHUT_RDWR;
#endif

    return (long long)shutdown(sock, how_val);
}

/* Set socket option: sock, option (1=REUSEADDR, 2=NONBLOCK), value */
long long aion_socket_raw_setopt(long long sock_fd, long long option, long long value) {
    socket_t sock = (socket_t)sock_fd;
    int val = (int)value;

    if ((int)option == 1) {
        /* SO_REUSEADDR */
        return (long long)setsockopt(sock, SOL_SOCKET, SO_REUSEADDR,
                                     (const char*)&val, sizeof(val));
    } else if ((int)option == 2) {
        /* Non-blocking mode */
#ifdef _WIN32
        u_long mode = val ? 1 : 0;
        return (long long)ioctlsocket(sock, FIONBIO, &mode);
#else
        int flags = fcntl(sock, F_GETFL, 0);
        if (flags == -1) return -1;
        flags = val ? (flags | O_NONBLOCK) : (flags & ~O_NONBLOCK);
        return (long long)fcntl(sock, F_SETFL, flags);
#endif
    }

    return -1;
}

/* Get last error code */
long long aion_socket_raw_error(void) {
#ifdef _WIN32
    return (long long)WSAGetLastError();
#else
    return (long long)errno;
#endif
}

/* Check if socket is valid */
long long aion_socket_raw_is_valid(long long sock_fd) {
    socket_t sock = (socket_t)sock_fd;
    return (sock != INVALID_SOCKET_FD) ? 1 : 0;
}

/* String length helper (for send operations) */
long long aion_socket_strlen(const char* str) {
    return (long long)strlen(str);
}

/* Cleanup Winsock */
void aion_socket_raw_cleanup(void) {
#ifdef _WIN32
    if (winsock_initialized) {
        WSACleanup();
        winsock_initialized = 0;
    }
#endif
}
