/*
 * C half of std.term: terminal size, raw keyboard input and "is this a terminal?" (POSIX and Windows).
 *
 * On Windows the console is switched to deliver keys as ANSI escape sequences (virtual terminal input), so the
 * same key-reading code serves both systems.
 */

#include "aion.h"

#include <errno.h>
#include <stdio.h>
#include <stdlib.h>
#include <string.h>

#ifdef _WIN32
#include <conio.h>
#include <io.h>
#include <windows.h>
#define STDIN_FILENO 0
#else
#include <poll.h>
#include <sys/ioctl.h>
#include <termios.h>
#include <unistd.h>
#endif

#ifdef _WIN32
/* Windows reports the NUL device as a terminal; a console is what answers to GetConsoleMode. */
static int console_isatty(int fd) {
    intptr_t handle = _get_osfhandle(fd);
    DWORD mode;
    return handle != -1 && GetFileType((HANDLE)handle) == FILE_TYPE_CHAR && GetConsoleMode((HANDLE)handle, &mode);
}
#endif

#ifdef _WIN32
#define isatty console_isatty
#endif

bool aion_term_is_tty(int64_t fd) { return isatty((int)fd) == 1; }

/* The size of the terminal on standard output as columns * 65536 + rows, or 0 if it has none. */
int64_t aion_term_size(void) {
#ifdef _WIN32
    CONSOLE_SCREEN_BUFFER_INFO info;
    if (!GetConsoleScreenBufferInfo(GetStdHandle(STD_OUTPUT_HANDLE), &info)) return 0;
    int cols = info.srWindow.Right - info.srWindow.Left + 1, rows = info.srWindow.Bottom - info.srWindow.Top + 1;
    return cols > 0 ? (int64_t)cols * 65536 + rows : 0;
#else
    struct winsize w;
    if (ioctl(STDOUT_FILENO, TIOCGWINSZ, &w) != 0 || w.ws_col == 0) return 0;
    return (int64_t)w.ws_col * 65536 + w.ws_row;
#endif
}

#ifdef _WIN32
static DWORD saved_mode;
static bool raw_on;

static void restore(void) {
    if (raw_on) SetConsoleMode(GetStdHandle(STD_INPUT_HANDLE), saved_mode), raw_on = false;
}

/* Raw mode: keys arrive one at a time, unechoed.  The console is restored when the program exits. */
bool aion_term_raw(bool on) {
    HANDLE in = GetStdHandle(STD_INPUT_HANDLE);
    if (!isatty(STDIN_FILENO)) return false;
    if (on && !raw_on) {
        if (!GetConsoleMode(in, &saved_mode)) return false;
        DWORD mode = saved_mode & ~(DWORD)(ENABLE_LINE_INPUT | ENABLE_ECHO_INPUT | ENABLE_PROCESSED_INPUT);
        mode |= 0x0200; /* ENABLE_VIRTUAL_TERMINAL_INPUT: arrow keys arrive as escape sequences */
        if (!SetConsoleMode(in, mode)) return false;
        static bool registered;
        if (!registered) atexit(restore), registered = true;
        raw_on = true;
    } else if (!on) {
        restore();
    }
    return true;
}

/* Is there something to read on standard input right now (or has it ended)? */
static bool input_ready(void) {
    HANDLE in = GetStdHandle(STD_INPUT_HANDLE);
    switch (GetFileType(in)) {
    case FILE_TYPE_CHAR: return _kbhit() != 0;
    case FILE_TYPE_PIPE: {
        DWORD available = 0;
        return !PeekNamedPipe(in, NULL, 0, NULL, &available, NULL) || available > 0;
    }
    default: return true;
    }
}

/* Wait up to `timeout_ms` (-1 = for ever) for input, letting other tasks run; 1 when ready, 0 on timeout. */
static int wait_input(int64_t timeout_ms) {
    int64_t waited = 0;
    while (!input_ready()) {
        if (timeout_ms >= 0 && waited >= timeout_ms) return 0;
        aion_sleep(10);
        waited += 10;
    }
    return 1;
}

static bool more_input(int64_t ms) { return wait_input(ms) > 0; }
#else
static struct termios saved;
static bool raw_on;

static void restore(void) {
    if (raw_on) tcsetattr(STDIN_FILENO, TCSAFLUSH, &saved), raw_on = false;
}

/* Raw mode: keys arrive one at a time, unechoed.  The terminal is restored when the program exits. */
bool aion_term_raw(bool on) {
    if (!isatty(STDIN_FILENO)) return false;
    if (on && !raw_on) {
        if (tcgetattr(STDIN_FILENO, &saved) != 0) return false;
        struct termios t = saved;
        t.c_lflag &= ~(tcflag_t)(ECHO | ICANON | IEXTEN);
        t.c_iflag &= ~(tcflag_t)(IXON | ICRNL | BRKINT | INPCK | ISTRIP);
        t.c_cc[VMIN] = 1, t.c_cc[VTIME] = 0;
        if (tcsetattr(STDIN_FILENO, TCSAFLUSH, &t) != 0) return false;
        static bool registered;
        if (!registered) atexit(restore), registered = true;
        raw_on = true;
    } else if (!on) {
        restore();
    }
    return true;
}

static int wait_input(int64_t timeout_ms) { return aion_wait_fd(STDIN_FILENO, POLLIN, timeout_ms); }

static bool more_input(int64_t ms) {
    struct pollfd p = {STDIN_FILENO, POLLIN, 0};
    return poll(&p, 1, (int)ms) > 0;
}
#endif

/*
 * Wait up to `timeout_ms` (-1 = for ever) for a key and return its name: a letter or symbol as itself ("a", "é", " "),
 * and "enter", "tab", "backspace", "escape", "up", "down", "left", "right", "home", "end", "delete", "pageup",
 * "pagedown", "ctrl-a".."ctrl-z" for the rest.  An empty string means the time ran out; NULL means the input ended.
 * Other tasks keep running while this waits.
 */
const char *aion_term_key(int64_t timeout_ms) {
    int w = wait_input(timeout_ms);
    if (w == 0) return aion_string("");
    unsigned char buf[16];
    int n = (int)read(STDIN_FILENO, buf, 1);
    if (n <= 0) return NULL;
    unsigned char c = buf[0];
    if (c == 27) { /* an escape sequence if more bytes follow at once */
        if (!more_input(25)) return aion_string("escape");
        n = (int)read(STDIN_FILENO, buf, 1);
        if (n <= 0 || (buf[0] != '[' && buf[0] != 'O')) return aion_string("escape");
        char seq[8];
        size_t len = 0;
        while (len < sizeof seq - 1 && more_input(25) && read(STDIN_FILENO, seq + len, 1) == 1) {
            len++;
            if ((unsigned char)seq[len - 1] >= 0x40 && (unsigned char)seq[len - 1] <= 0x7e) break;
        }
        seq[len] = '\0';
        if (!strcmp(seq, "A")) return aion_string("up");
        if (!strcmp(seq, "B")) return aion_string("down");
        if (!strcmp(seq, "C")) return aion_string("right");
        if (!strcmp(seq, "D")) return aion_string("left");
        if (!strcmp(seq, "H") || !strcmp(seq, "1~")) return aion_string("home");
        if (!strcmp(seq, "F") || !strcmp(seq, "4~")) return aion_string("end");
        if (!strcmp(seq, "3~")) return aion_string("delete");
        if (!strcmp(seq, "5~")) return aion_string("pageup");
        if (!strcmp(seq, "6~")) return aion_string("pagedown");
        return aion_string("unknown");
    }
    if (c == '\r' || c == '\n') return aion_string("enter");
    if (c == '\t') return aion_string("tab");
    if (c == 127 || c == 8) return aion_string("backspace");
    if (c < 32) {
        char name[16];
        snprintf(name, sizeof name, "ctrl-%c", 'a' + c - 1);
        return aion_string(name);
    }
    char key[8] = {(char)c};
    size_t len = 1, need = c < 0x80 ? 1 : (c >> 5) == 6 ? 2 : (c >> 4) == 14 ? 3 : 4;
    while (len < need && read(STDIN_FILENO, key + len, 1) == 1) len++;
    return aion_string_n(key, len);
}
