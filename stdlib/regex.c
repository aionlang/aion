/*
 * C half of std.regex: a regular-expression engine that needs no external library.
 *
 * Syntax: literals, `.`, classes `[a-z\d_]` / `[^...]`, `\d \D \w \W \s \S \b \B \A \z`, anchors `^ $`,
 * groups `( )`, `(?: )`, `(?<name> )`, alternation `|`, and the quantifiers `* + ? {n} {n,} {n,m}`
 * with a `?` suffix for the lazy form.  Flags: i (ignore case), m (^ and $ match at lines), s (. matches
 * line breaks).  No backreferences or lookaround: matching is a Pike VM, so the time is always
 * proportional to pattern size times text length -- no pattern can blow up.
 *
 * A compiled pattern is a Bytes value: header, instructions, character ranges, group names.
 */

#include "aion.h"

#include <stdio.h>
#include <stdlib.h>
#include <string.h>

int64_t aion_char_upper(int64_t c);
int64_t aion_char_lower(int64_t c);

enum { I_CHAR, I_ANY, I_ANYNL, I_CLASS, I_SPLIT, I_JMP, I_SAVE, I_BOL, I_EOL, I_BOT, I_EOT, I_WORDB, I_NWORDB, I_MATCH };
enum { F_ICASE = 1, F_MULTI = 2, F_DOTALL = 4 };
#define MAX_INST 20000
#define MAX_REPEAT 1000
#define INF (-1)

typedef struct { int32_t op, x, y; } Inst;
typedef struct { int32_t magic, ninst, nranges, ngroups, flags, names_len; } Header;
#define MAGIC 0x52584731

static char error_text[160];
const char *aion_regex_error(void) { return aion_string(error_text); }

/* ── Characters ───────────────────────────────────────────────── */

static int decode(const char *s, int64_t len, int64_t at, uint32_t *cp) {
    const unsigned char *p = (const unsigned char *)s + at;
    int64_t left = len - at;
    unsigned char c = p[0];
    int n = c < 0x80 ? 1 : (c >> 5) == 6 ? 2 : (c >> 4) == 14 ? 3 : (c >> 3) == 30 ? 4 : 0;
    if (n == 0 || n > left) return *cp = 0xFFFD, 1;
    uint32_t v = n == 1 ? c : n == 2 ? c & 0x1F : n == 3 ? c & 0x0F : c & 0x07;
    for (int i = 1; i < n; i++) {
        if ((p[i] & 0xC0) != 0x80) return *cp = 0xFFFD, 1;
        v = v << 6 | (p[i] & 0x3F);
    }
    return *cp = v, n;
}

static uint32_t fold(uint32_t c) { return (uint32_t)aion_char_lower(c); }

static const uint32_t WORD_RANGES[] = {'0', '9', 'A', 'Z', '_', '_', 'a', 'z', 0xC0, 0xD6, 0xD8, 0xF6, 0xF8, 0x24F,
                                       0x370, 0x3FF, 0x400, 0x52F, 0x3040, 0x30FF, 0x4E00, 0x9FFF, 0xAC00, 0xD7AF};
static const uint32_t SPACE_RANGES[] = {'\t', '\r', ' ', ' ', 0x85, 0x85, 0xA0, 0xA0, 0x2000, 0x200A, 0x2028, 0x2029, 0x3000, 0x3000};

static bool in_ranges(const uint32_t *r, int n, uint32_t c) {
    for (int i = 0; i < n; i++)
        if (c >= r[2 * i] && c <= r[2 * i + 1]) return true;
    return false;
}

static bool is_word(uint32_t c) { return in_ranges(WORD_RANGES, (int)(sizeof WORD_RANGES / sizeof *WORD_RANGES / 2), c); }

/* ── Compiler: pattern -> syntax tree -> instructions ─────────── */

enum { N_EMPTY, N_CHAR, N_ANY, N_CLASS, N_CAT, N_ALT, N_REPEAT, N_GROUP, N_ASSERT };
typedef struct Node {
    int type, a, b, min, max;
    bool greedy;
    struct Node *child, *next; /* child list for CAT and ALT; `child` alone for REPEAT and GROUP */
} Node;

typedef struct {
    const char *pat;
    size_t len, at;
    int flags, ngroups;
    Node **nodes;
    size_t nnodes, cap_nodes;
    uint32_t *ranges;
    int nranges, cap_ranges;
    char *names; /* NUL-terminated names of groups 1.., one per line */
    size_t names_len, names_cap;
    Inst *code;
    int ncode, cap_code;
    bool failed;
} C;

static void fail(C *c, const char *what) {
    if (!c->failed) snprintf(error_text, sizeof error_text, "%s at position %zu", what, c->at);
    c->failed = true;
}

static Node *node(C *c, int type) {
    Node *n = calloc(1, sizeof *n);
    if (!n) aion_panic("out of memory");
    n->type = type, n->greedy = true;
    if (c->nnodes == c->cap_nodes) {
        c->cap_nodes = c->cap_nodes ? c->cap_nodes * 2 : 64;
        c->nodes = realloc(c->nodes, c->cap_nodes * sizeof *c->nodes);
        if (!c->nodes) aion_panic("out of memory");
    }
    c->nodes[c->nnodes++] = n;
    return n;
}

static void add_range(C *c, uint32_t lo, uint32_t hi) {
    if (c->nranges == c->cap_ranges) {
        c->cap_ranges = c->cap_ranges ? c->cap_ranges * 2 : 32;
        c->ranges = realloc(c->ranges, (size_t)c->cap_ranges * 2 * sizeof *c->ranges);
        if (!c->ranges) aion_panic("out of memory");
    }
    c->ranges[2 * c->nranges] = lo, c->ranges[2 * c->nranges + 1] = hi;
    c->nranges++;
}

static bool more(C *c) { return c->at < c->len; }
static int peek(C *c) { return more(c) ? (unsigned char)c->pat[c->at] : -1; }

static uint32_t next_char(C *c) {
    uint32_t cp;
    c->at += (size_t)decode(c->pat, (int64_t)c->len, (int64_t)c->at, &cp);
    return cp;
}

/* Add the ranges of \d \w \s (or their complements, as ranges over the whole code space). */
static void add_escape_class(C *c, int letter) {
    const uint32_t digit[] = {'0', '9'};
    const uint32_t *src;
    int n;
    switch (letter | 32) {
    case 'd': src = digit, n = 1; break;
    case 'w': src = WORD_RANGES, n = (int)(sizeof WORD_RANGES / sizeof *WORD_RANGES / 2); break;
    default: src = SPACE_RANGES, n = (int)(sizeof SPACE_RANGES / sizeof *SPACE_RANGES / 2); break;
    }
    if (letter >= 'a') {
        for (int i = 0; i < n; i++) add_range(c, src[2 * i], src[2 * i + 1]);
    } else { /* complement */
        uint32_t next = 0;
        for (int i = 0; i < n; i++) {
            if (src[2 * i] > next) add_range(c, next, src[2 * i] - 1);
            next = src[2 * i + 1] + 1;
        }
        add_range(c, next, 0x10FFFF);
    }
}

static int hex_digit(int ch) {
    if (ch >= '0' && ch <= '9') return ch - '0';
    if (ch >= 'a' && ch <= 'f') return ch - 'a' + 10;
    if (ch >= 'A' && ch <= 'F') return ch - 'A' + 10;
    return -1;
}

/* After a backslash: a single character an escape stands for, or -1 when it is a class/assertion letter. */
static int64_t escape_char(C *c, int e) {
    switch (e) {
    case 'n': return '\n';
    case 't': return '\t';
    case 'r': return '\r';
    case 'f': return '\f';
    case 'v': return '\v';
    case '0': return 0;
    case 'x': {
        int hi = more(c) ? hex_digit(peek(c)) : -1;
        if (hi < 0) return fail(c, "invalid \\x escape"), -2;
        c->at++;
        int lo = more(c) ? hex_digit(peek(c)) : -1;
        if (lo < 0) return fail(c, "invalid \\x escape"), -2;
        c->at++;
        return hi << 4 | lo;
    }
    case 'u': {
        if (peek(c) != '{') return fail(c, "expected \\u{...}"), -2;
        c->at++;
        uint32_t v = 0;
        int digits = 0;
        while (more(c) && peek(c) != '}' && hex_digit(peek(c)) >= 0 && digits < 7) v = v << 4 | (uint32_t)hex_digit(c->pat[c->at++]), digits++;
        if (peek(c) != '}' || digits == 0 || v > 0x10FFFF) return fail(c, "invalid \\u{...} escape"), -2;
        c->at++;
        return v;
    }
    default: return -1;
    }
}

static Node *parse_alt(C *c);

static Node *parse_class(C *c) {
    Node *n = node(c, N_CLASS);
    int first = c->nranges;
    bool negate = false;
    if (peek(c) == '^') negate = true, c->at++;
    bool any = false;
    while (true) {
        if (!more(c)) return fail(c, "unclosed character class"), n;
        int ch = peek(c);
        if (ch == ']' && any) { c->at++; break; }
        any = true;
        uint32_t lo;
        if (ch == '\\') {
            c->at++;
            if (!more(c)) return fail(c, "dangling backslash"), n;
            int e = (unsigned char)c->pat[c->at++];
            if (e == 'd' || e == 'D' || e == 'w' || e == 'W' || e == 's' || e == 'S') { add_escape_class(c, e); continue; }
            int64_t v = escape_char(c, e);
            if (v == -2) return n;
            if (v == -1) { c->at--; v = next_char(c); }
            lo = (uint32_t)v;
        } else {
            lo = next_char(c);
        }
        uint32_t hi = lo;
        if (peek(c) == '-' && c->at + 1 < c->len && c->pat[c->at + 1] != ']') {
            c->at++;
            if (peek(c) == '\\') {
                c->at++;
                if (!more(c)) return fail(c, "dangling backslash"), n;
                int e = (unsigned char)c->pat[c->at++];
                int64_t v = escape_char(c, e);
                if (v == -2) return n;
                if (v == -1) { c->at--; v = next_char(c); }
                hi = (uint32_t)v;
            } else {
                hi = next_char(c);
            }
            if (hi < lo) return fail(c, "invalid range in character class"), n;
        }
        add_range(c, lo, hi);
    }
    n->a = first, n->b = c->nranges - first, n->min = negate;
    return n;
}

static Node *parse_atom(C *c) {
    int ch = peek(c);
    if (ch == '(') {
        c->at++;
        int capture = 1;
        const char *name = NULL;
        size_t name_len = 0;
        if (peek(c) == '?') {
            c->at++;
            int k = peek(c);
            if (k == ':') {
                c->at++, capture = 0;
            } else if (k == '<' || k == 'P') {
                if (k == 'P') c->at++;
                if (peek(c) != '<') return fail(c, "unknown group syntax"), node(c, N_EMPTY);
                c->at++;
                if (peek(c) == '=' || peek(c) == '!') return fail(c, "lookaround is not supported"), node(c, N_EMPTY);
                name = c->pat + c->at;
                while (more(c) && peek(c) != '>') c->at++;
                if (!more(c) || (name_len = (size_t)(c->pat + c->at - name)) == 0) return fail(c, "invalid group name"), node(c, N_EMPTY);
                c->at++;
            } else if (k == 'i' || k == 'm' || k == 's' || k == '-') {
                int on = 1, flags = c->flags;
                while (more(c) && peek(c) != ')' && peek(c) != ':') {
                    int f = peek(c) == 'i' ? F_ICASE : peek(c) == 'm' ? F_MULTI : peek(c) == 's' ? F_DOTALL : 0;
                    if (peek(c) == '-') on = 0;
                    else if (!f) return fail(c, "unknown flag"), node(c, N_EMPTY);
                    else flags = on ? flags | f : flags & ~f;
                    c->at++;
                }
                c->flags = flags;
                if (peek(c) == ')') return c->at++, node(c, N_EMPTY);
                c->at++, capture = 0; /* (?i:...) */
            } else if (k == '=' || k == '!') {
                return fail(c, "lookaround is not supported"), node(c, N_EMPTY);
            } else {
                return fail(c, "unknown group syntax"), node(c, N_EMPTY);
            }
        }
        int index = 0;
        if (capture) {
            index = ++c->ngroups;
            size_t need = c->names_len + name_len + 2;
            if (need > c->names_cap) {
                c->names_cap = need * 2;
                c->names = realloc(c->names, c->names_cap);
                if (!c->names) aion_panic("out of memory");
            }
            if (name) memcpy(c->names + c->names_len, name, name_len), c->names_len += name_len;
            c->names[c->names_len++] = '\n';
        }
        Node *inner = parse_alt(c);
        if (peek(c) != ')') return fail(c, "unclosed group"), inner;
        c->at++;
        if (!capture) return inner;
        Node *g = node(c, N_GROUP);
        g->a = index, g->child = inner;
        return g;
    }
    if (ch == '[') { c->at++; return parse_class(c); }
    if (ch == '.') { c->at++; return node(c, N_ANY); }
    if (ch == '^') { c->at++; Node *n = node(c, N_ASSERT); n->a = I_BOL; return n; }
    if (ch == '$') { c->at++; Node *n = node(c, N_ASSERT); n->a = I_EOL; return n; }
    if (ch == '*' || ch == '+' || ch == '?') return fail(c, "nothing to repeat"), node(c, N_EMPTY);
    if (ch == '\\') {
        c->at++;
        if (!more(c)) return fail(c, "dangling backslash"), node(c, N_EMPTY);
        int e = (unsigned char)c->pat[c->at++];
        if (e == 'd' || e == 'D' || e == 'w' || e == 'W' || e == 's' || e == 'S') {
            Node *n = node(c, N_CLASS);
            int first = c->nranges;
            add_escape_class(c, e);
            n->a = first, n->b = c->nranges - first, n->min = 0;
            return n;
        }
        if (e == 'b' || e == 'B' || e == 'A' || e == 'z') {
            Node *n = node(c, N_ASSERT);
            n->a = e == 'b' ? I_WORDB : e == 'B' ? I_NWORDB : e == 'A' ? I_BOT : I_EOT;
            return n;
        }
        if (e >= '1' && e <= '9') return fail(c, "backreferences are not supported"), node(c, N_EMPTY);
        int64_t v = escape_char(c, e);
        if (v == -2) return node(c, N_EMPTY);
        if (v == -1) { c->at--; v = next_char(c); }
        Node *n = node(c, N_CHAR);
        n->a = (int)v;
        return n;
    }
    Node *n = node(c, N_CHAR);
    n->a = (int)next_char(c);
    return n;
}

/* `{n}`, `{n,}` or `{n,m}` at the cursor (after the `{`): true and the bounds, or false and the cursor unchanged. */
static bool parse_counts(C *c, int *min, int *max) {
    size_t save = c->at;
    long lo = 0, hi;
    int digits = 0;
    while (more(c) && peek(c) >= '0' && peek(c) <= '9' && digits < 6) lo = lo * 10 + (c->pat[c->at++] - '0'), digits++;
    if (!digits) return c->at = save, false;
    hi = lo;
    if (peek(c) == ',') {
        c->at++;
        if (peek(c) == '}') hi = INF;
        else {
            hi = 0, digits = 0;
            while (more(c) && peek(c) >= '0' && peek(c) <= '9' && digits < 6) hi = hi * 10 + (c->pat[c->at++] - '0'), digits++;
            if (!digits) return c->at = save, false;
        }
    }
    if (peek(c) != '}') return c->at = save, false;
    c->at++;
    *min = (int)lo, *max = (int)hi;
    return true;
}

static Node *parse_repeat(C *c) {
    Node *atom = parse_atom(c);
    while (!c->failed) {
        int ch = peek(c), min, max;
        if (ch == '*') min = 0, max = INF, c->at++;
        else if (ch == '+') min = 1, max = INF, c->at++;
        else if (ch == '?') min = 0, max = 1, c->at++;
        else if (ch == '{') {
            c->at++;
            if (!parse_counts(c, &min, &max)) { c->at--; break; }
            if (max != INF && max < min) return fail(c, "invalid repetition count"), atom;
            if (min > MAX_REPEAT || max > MAX_REPEAT) return fail(c, "repetition count too large"), atom;
        } else break;
        Node *r = node(c, N_REPEAT);
        r->min = min, r->max = max, r->child = atom;
        if (peek(c) == '?') r->greedy = false, c->at++;
        atom = r;
    }
    return atom;
}

static Node *parse_cat(C *c) {
    Node *cat = node(c, N_CAT), **tail = &cat->child;
    while (more(c) && peek(c) != '|' && peek(c) != ')' && !c->failed) {
        Node *item = parse_repeat(c);
        *tail = item;
        tail = &item->next;
    }
    return cat;
}

static Node *parse_alt(C *c) {
    Node *first = parse_cat(c);
    if (peek(c) != '|') return first;
    Node *alt = node(c, N_ALT), **tail = &alt->child;
    *tail = first;
    tail = &first->next;
    while (peek(c) == '|' && !c->failed) {
        c->at++;
        Node *branch = parse_cat(c);
        *tail = branch;
        tail = &branch->next;
    }
    return alt;
}

static int emit(C *c, int op, int x, int y) {
    if (c->ncode >= MAX_INST) { fail(c, "pattern too large"); return 0; }
    if (c->ncode == c->cap_code) {
        c->cap_code = c->cap_code ? c->cap_code * 2 : 64;
        c->code = realloc(c->code, (size_t)c->cap_code * sizeof *c->code);
        if (!c->code) aion_panic("out of memory");
    }
    c->code[c->ncode] = (Inst){op, x, y};
    return c->ncode++;
}

static void gen(C *c, Node *n) {
    if (c->failed) return;
    switch (n->type) {
    case N_EMPTY: break;
    case N_CHAR: emit(c, I_CHAR, (c->flags & F_ICASE) ? (int)fold((uint32_t)n->a) : n->a, 0); break;
    case N_ANY: emit(c, (c->flags & F_DOTALL) ? I_ANYNL : I_ANY, 0, 0); break;
    case N_CLASS: emit(c, I_CLASS, n->a, n->b << 1 | n->min); break;
    case N_ASSERT: {
        int op = n->a;
        if (op == I_BOL && !(c->flags & F_MULTI)) op = I_BOT;
        if (op == I_EOL && !(c->flags & F_MULTI)) op = I_EOT;
        emit(c, op, 0, 0);
        break;
    }
    case N_CAT: for (Node *k = n->child; k; k = k->next) gen(c, k); break;
    case N_GROUP:
        emit(c, I_SAVE, 2 * n->a, 0);
        gen(c, n->child);
        emit(c, I_SAVE, 2 * n->a + 1, 0);
        break;
    case N_ALT: {
        int branches = 0;
        for (Node *k = n->child; k; k = k->next) branches++;
        int *jumps = malloc((size_t)branches * sizeof *jumps), nj = 0;
        if (!jumps) aion_panic("out of memory");
        Node *k = n->child;
        for (; k->next; k = k->next) {
            int split = emit(c, I_SPLIT, 0, 0);
            c->code[split].x = c->ncode;
            gen(c, k);
            jumps[nj++] = emit(c, I_JMP, 0, 0);
            c->code[split].y = c->ncode;
        }
        gen(c, k);
        for (int i = 0; i < nj && !c->failed; i++) c->code[jumps[i]].x = c->ncode;
        free(jumps);
        break;
    }
    case N_REPEAT: {
        int min = n->min, max = n->max;
        for (int i = 0; i < min - (max == INF && min > 0 ? 1 : 0); i++) gen(c, n->child);
        if (max == INF) {
            if (min == 0) {
                int split = emit(c, I_SPLIT, 0, 0);
                int body = c->ncode;
                gen(c, n->child);
                emit(c, I_JMP, split, 0);
                c->code[split].x = n->greedy ? body : c->ncode;
                c->code[split].y = n->greedy ? c->ncode : body;
            } else {
                int body = c->ncode;
                gen(c, n->child);
                int split = emit(c, I_SPLIT, 0, 0);
                c->code[split].x = n->greedy ? body : c->ncode;
                c->code[split].y = n->greedy ? c->ncode : body;
            }
        } else {
            int skips[MAX_REPEAT], ns = 0;
            for (int i = min; i < max; i++) {
                int split = emit(c, I_SPLIT, 0, 0);
                skips[ns++] = split;
                int body = c->ncode;
                gen(c, n->child);
                if (n->greedy) c->code[split].x = body; else c->code[split].y = body;
            }
            for (int i = 0; i < ns; i++) {
                if (n->greedy) c->code[skips[i]].y = c->ncode; else c->code[skips[i]].x = c->ncode;
            }
        }
        break;
    }
    }
}

/* Compile `pattern` with flags from {i, m, s}; NULL (and aion_regex_error) if it is invalid. */
void *aion_regex_compile(const char *pattern, const char *flags) {
    C c;
    memset(&c, 0, sizeof c);
    c.pat = pattern, c.len = strlen(pattern);
    for (const char *f = flags; *f; f++) {
        if (*f == 'i') c.flags |= F_ICASE;
        else if (*f == 'm') c.flags |= F_MULTI;
        else if (*f == 's') c.flags |= F_DOTALL;
        else { snprintf(error_text, sizeof error_text, "unknown flag '%c' (use i, m or s)", *f); return NULL; }
    }
    Node *root = parse_alt(&c);
    if (!c.failed && more(&c)) fail(&c, peek(&c) == ')' ? "unmatched )" : "unexpected character");
    void *result = NULL;
    if (!c.failed) {
        int final_flags = c.flags;
        emit(&c, I_SAVE, 0, 0);
        gen(&c, root);
        emit(&c, I_SAVE, 1, 0);
        emit(&c, I_MATCH, 0, 0);
        if (!c.failed) {
            size_t bytes = sizeof(Header) + (size_t)c.ncode * sizeof(Inst) + (size_t)c.nranges * 8 + c.names_len;
            result = aion_bytes_new((int64_t)bytes);
            uint8_t *p = aion_bytes_data(result);
            Header h = {MAGIC, c.ncode, c.nranges, c.ngroups, final_flags, (int32_t)c.names_len};
            memcpy(p, &h, sizeof h);
            memcpy(p + sizeof h, c.code, (size_t)c.ncode * sizeof(Inst));
            memcpy(p + sizeof h + (size_t)c.ncode * sizeof(Inst), c.ranges, (size_t)c.nranges * 8);
            memcpy(p + sizeof h + (size_t)c.ncode * sizeof(Inst) + (size_t)c.nranges * 8, c.names, c.names_len);
        }
    }
    for (size_t i = 0; i < c.nnodes; i++) free(c.nodes[i]);
    free(c.nodes), free(c.ranges), free(c.names), free(c.code);
    return result;
}

int64_t aion_regex_groups(const void *prog) { return ((const Header *)prog)->ngroups; }

/* The names of groups 1.. separated by line breaks (an unnamed group is an empty line). */
const char *aion_regex_names(const void *prog) {
    const Header *h = prog;
    const uint8_t *p = (const uint8_t *)prog;
    const char *names = (const char *)(p + sizeof *h + (size_t)h->ninst * sizeof(Inst) + (size_t)h->nranges * 8);
    return aion_string_n(names, h->names_len > 0 ? (size_t)h->names_len - 1 : 0);
}

/* ── Matcher: a Pike VM ───────────────────────────────────────── */

typedef struct {
    const Header *h;
    const Inst *code;
    const uint32_t *ranges;
    const char *text;
    int64_t len;
    int ncap;
} M;

static int64_t *result_caps;
static int64_t result_len;

int64_t aion_regex_cap(int64_t i) { return i >= 0 && i < result_len ? result_caps[i] : -1; }

static bool class_has(const M *m, const Inst *in, uint32_t c) {
    int count = in->y >> 1;
    const uint32_t *r = m->ranges + 2 * in->x;
    bool hit = in_ranges(r, count, c);
    if (!hit && (m->h->flags & F_ICASE)) {
        uint32_t lo = (uint32_t)aion_char_lower(c), up = (uint32_t)aion_char_upper(c);
        hit = (lo != c && in_ranges(r, count, lo)) || (up != c && in_ranges(r, count, up));
    }
    return hit != (in->y & 1);
}

static bool word_at(const M *m, int64_t at) {
    if (at < 0 || at >= m->len) return false;
    int64_t start = at;
    if (at > 0 && ((unsigned char)m->text[at] & 0xC0) == 0x80) { /* a continuation byte: find the lead byte */
        while (start > 0 && ((unsigned char)m->text[start] & 0xC0) == 0x80) start--;
    }
    uint32_t cp;
    decode(m->text, m->len, start, &cp);
    return is_word(cp);
}

static bool word_before(const M *m, int64_t at) {
    if (at <= 0) return false;
    int64_t start = at - 1;
    while (start > 0 && ((unsigned char)m->text[start] & 0xC0) == 0x80) start--;
    return word_at(m, start);
}

typedef struct { int pc; int slot; int64_t old; } Frame;

typedef struct {
    int *pcs;
    int64_t *caps;
    int n;
} List;

static bool assertion(const M *m, int op, int64_t sp) {
    switch (op) {
    case I_BOT: return sp == 0;
    case I_EOT: return sp == m->len;
    case I_BOL: return sp == 0 || m->text[sp - 1] == '\n';
    case I_EOL: return sp == m->len || m->text[sp] == '\n';
    case I_WORDB: return word_before(m, sp) != word_at(m, sp);
    default: return word_before(m, sp) == word_at(m, sp);
    }
}

/* Follow jumps, splits, saves and assertions from `pc0` at position `sp`, adding the threads that wait for a character. */
static void add_thread(const M *m, List *l, int *mark, int gen_id, Frame *stack, int pc0, int64_t *caps, int64_t sp) {
    int top = 0;
    stack[top++] = (Frame){pc0, -1, 0};
    while (top > 0) {
        Frame f = stack[--top];
        if (f.slot >= 0) { caps[f.slot] = f.old; continue; }
        int pc = f.pc;
        if (mark[pc] == gen_id) continue;
        mark[pc] = gen_id;
        const Inst *in = &m->code[pc];
        switch (in->op) {
        case I_JMP: stack[top++] = (Frame){in->x, -1, 0}; break;
        case I_SPLIT:
            stack[top++] = (Frame){in->y, -1, 0};
            stack[top++] = (Frame){in->x, -1, 0};
            break;
        case I_SAVE:
            if (in->x < m->ncap) {
                stack[top++] = (Frame){0, in->x, caps[in->x]};
                caps[in->x] = sp;
            }
            stack[top++] = (Frame){pc + 1, -1, 0};
            break;
        case I_BOL: case I_EOL: case I_BOT: case I_EOT: case I_WORDB: case I_NWORDB:
            if (assertion(m, in->op, sp)) stack[top++] = (Frame){pc + 1, -1, 0};
            break;
        default: /* CHAR, ANY, ANYNL, CLASS, MATCH wait in the list */
            l->pcs[l->n] = pc;
            memcpy(l->caps + (size_t)l->n * (size_t)m->ncap, caps, (size_t)m->ncap * sizeof *caps);
            l->n++;
        }
    }
}

/*
 * Find the leftmost match starting at or after `start` (exactly at `start` if `anchored`).  The positions of the
 * whole match and of each group are then read with aion_regex_cap(2*i) and aion_regex_cap(2*i+1); -1 = unset.
 */
bool aion_regex_search(const void *prog, const char *text, int64_t start, bool anchored) {
    M m;
    m.h = prog;
    if (m.h->magic != MAGIC) aion_panic("not a compiled regex");
    m.code = (const Inst *)((const uint8_t *)prog + sizeof(Header));
    m.ranges = (const uint32_t *)(m.code + m.h->ninst);
    m.text = text, m.len = (int64_t)strlen(text);
    m.ncap = 2 * (m.h->ngroups + 1);
    if (start < 0 || start > m.len) return false;

    size_t n = (size_t)m.h->ninst, ncap = (size_t)m.ncap;
    List lists[2];
    for (int i = 0; i < 2; i++) {
        lists[i].pcs = malloc(n * sizeof(int));
        lists[i].caps = malloc(n * ncap * sizeof(int64_t));
        lists[i].n = 0;
    }
    int *mark = calloc(n, sizeof(int));
    Frame *stack = malloc((2 * n + 8) * sizeof(Frame) * 2);
    int64_t *caps = malloc(ncap * sizeof(int64_t));
    int64_t *best = malloc(ncap * sizeof(int64_t));
    if (!lists[0].pcs || !lists[0].caps || !lists[1].pcs || !lists[1].caps || !mark || !stack || !caps || !best) aion_panic("out of memory");

    bool matched = false;
    int cur = 0, gen_id = 1, cur_gen = 1;
    for (int64_t sp = start;; ) {
        List *clist = &lists[cur], *nlist = &lists[1 - cur];
        if (!matched && (!anchored || sp == start)) {
            for (size_t i = 0; i < ncap; i++) caps[i] = -1;
            add_thread(&m, clist, mark, cur_gen, stack, 0, caps, sp);
        }
        if (clist->n == 0 && (matched || anchored || sp >= m.len)) break;
        uint32_t c = 0;
        int clen = 0;
        if (sp < m.len) clen = decode(text, m.len, sp, &c);
        uint32_t fc = (m.h->flags & F_ICASE) && sp < m.len ? fold(c) : c;
        nlist->n = 0;
        int next_gen = ++gen_id;
        for (int t = 0; t < clist->n; t++) {
            const Inst *in = &m.code[clist->pcs[t]];
            int64_t *tc = clist->caps + (size_t)t * ncap;
            bool take = false;
            switch (in->op) {
            case I_MATCH:
                matched = true;
                memcpy(best, tc, ncap * sizeof *best);
                t = clist->n; /* lower-priority threads are cut off */
                break;
            case I_CHAR: take = sp < m.len && fc == (uint32_t)in->x; break;
            case I_ANY: take = sp < m.len && c != '\n'; break;
            case I_ANYNL: take = sp < m.len; break;
            case I_CLASS: take = sp < m.len && class_has(&m, in, c); break;
            }
            if (take) {
                memcpy(caps, tc, ncap * sizeof *caps); /* add_thread edits and restores its copy */
                add_thread(&m, nlist, mark, next_gen, stack, clist->pcs[t] + 1, caps, sp + clen);
            }
        }
        if (sp >= m.len) break;
        sp += clen;
        cur = 1 - cur;
        cur_gen = next_gen;
    }

    if (matched) {
        free(result_caps);
        result_caps = malloc(ncap * sizeof *result_caps);
        memcpy(result_caps, best, ncap * sizeof *best);
        result_len = (int64_t)ncap;
    }
    for (int i = 0; i < 2; i++) free(lists[i].pcs), free(lists[i].caps);
    free(mark), free(stack), free(caps), free(best);
    return matched;
}
