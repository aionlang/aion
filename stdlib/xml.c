/*
 * C half of std.xml: an XML parser (POSIX).  The parsing is done by the expat library, loaded at run time with
 * dlopen like OpenSSL in std.crypto: a program that never imports std.xml needs nothing installed, and no header
 * files are required to build.
 *
 * The parser's events are written into a compact byte string that the Aion half turns into a tree:
 *   1 start of an element: name, a count of attributes (2 bytes), and for each its name and value;
 *   2 end of an element;
 *   3 text; 4 a comment;
 * and every string is a 4-byte length followed by its UTF-8 bytes.  Expat does not load external entities, and
 * it stops documents whose entities expand far beyond their size.
 */

#define _GNU_SOURCE
#include "aion.h"

#include <dlfcn.h>
#include <stdio.h>
#include <stdlib.h>
#include <string.h>

static char error_text[512];

const char *aion_xml_error(void) { return aion_string(error_text); }

static bool fail(const char *message) {
    snprintf(error_text, sizeof error_text, "%s", message);
    return false;
}

typedef void *Parser;

static struct {
    bool tried, ok;
    Parser (*create)(const char *);
    void (*set_user_data)(Parser, void *);
    void (*set_element_handler)(Parser, void (*)(void *, const char *, const char **), void (*)(void *, const char *));
    void (*set_character_handler)(Parser, void (*)(void *, const char *, int));
    void (*set_comment_handler)(Parser, void (*)(void *, const char *));
    int (*parse)(Parser, const char *, int, int);
    int (*stop)(Parser, int);
    int (*error_code)(Parser);
    const char *(*error_string)(int);
    unsigned long (*line)(Parser);
    unsigned long (*column)(Parser);
    void (*free_parser)(Parser);
} ex;

static bool load(void) {
    if (ex.tried) return ex.ok;
    ex.tried = true;
    static const char *const names[] = {"libexpat.so.1", "libexpat.so", "libexpat.1.dylib", "libexpat.dylib", NULL};
    void *lib = NULL;
    for (const char *const *n = names; *n && !lib; n++) lib = dlopen(*n, RTLD_NOW | RTLD_GLOBAL);
    if (!lib) return fail("std.xml needs the expat library: install it (for example the libexpat1 package)");
#define LOAD(field, symbol) \
    if (!(*(void **)&ex.field = dlsym(lib, symbol))) return fail("the installed expat is missing " symbol);
    LOAD(create, "XML_ParserCreate") LOAD(set_user_data, "XML_SetUserData") LOAD(set_element_handler, "XML_SetElementHandler")
    LOAD(set_character_handler, "XML_SetCharacterDataHandler") LOAD(set_comment_handler, "XML_SetCommentHandler")
    LOAD(parse, "XML_Parse") LOAD(stop, "XML_StopParser") LOAD(error_code, "XML_GetErrorCode") LOAD(error_string, "XML_ErrorString")
    LOAD(line, "XML_GetCurrentLineNumber") LOAD(column, "XML_GetCurrentColumnNumber") LOAD(free_parser, "XML_ParserFree")
#undef LOAD
    ex.ok = true;
    return true;
}

bool aion_xml_available(void) { return load(); }

/* ── Collecting events ───────────────────────────────────────────── */

typedef struct {
    Parser parser;
    uint8_t *data;
    size_t used, capacity;
    int depth;
    const char *problem;
} Out;

enum { MAX_OUTPUT = 512 << 20, MAX_DEPTH = 500 };

static bool room(Out *o, size_t more) {
    if (o->problem) return false;
    if (o->used + more > MAX_OUTPUT) {
        o->problem = "the document is too large";
        ex.stop(o->parser, 0);
        return false;
    }
    if (o->used + more > o->capacity) {
        size_t capacity = o->capacity ? o->capacity : 4096;
        while (capacity < o->used + more) capacity *= 2;
        uint8_t *bigger = realloc(o->data, capacity);
        if (!bigger) {
            o->problem = "out of memory";
            ex.stop(o->parser, 0);
            return false;
        }
        o->data = bigger;
        o->capacity = capacity;
    }
    return true;
}

static void put_byte(Out *o, uint8_t b) {
    if (room(o, 1)) o->data[o->used++] = b;
}

static void put_text(Out *o, const char *s, size_t n) {
    if (!room(o, n + 4)) return;
    o->data[o->used++] = (uint8_t)(n >> 24);
    o->data[o->used++] = (uint8_t)(n >> 16);
    o->data[o->used++] = (uint8_t)(n >> 8);
    o->data[o->used++] = (uint8_t)n;
    memcpy(o->data + o->used, s, n);
    o->used += n;
}

static void on_start(void *user, const char *name, const char **atts) {
    Out *o = user;
    if (++o->depth > MAX_DEPTH) {
        o->problem = "the elements are nested too deeply";
        ex.stop(o->parser, 0);
        return;
    }
    put_byte(o, 1);
    put_text(o, name, strlen(name));
    size_t count = 0;
    while (atts[2 * count]) count++;
    put_byte(o, (uint8_t)(count >> 8));
    put_byte(o, (uint8_t)count);
    for (size_t i = 0; i < count; i++) {
        put_text(o, atts[2 * i], strlen(atts[2 * i]));
        put_text(o, atts[2 * i + 1], strlen(atts[2 * i + 1]));
    }
}

static void on_end(void *user, const char *name) {
    Out *o = user;
    (void)name;
    o->depth--;
    put_byte(o, 2);
}

static void on_text(void *user, const char *s, int len) {
    Out *o = user;
    put_byte(o, 3);
    put_text(o, s, (size_t)len);
}

static void on_comment(void *user, const char *s) {
    Out *o = user;
    put_byte(o, 4);
    put_text(o, s, strlen(s));
}

/* Parse a document.  NULL when it is not well formed (the error text says where). */
void *aion_xml_parse(const void *text) {
    if (!load()) return NULL;
    Parser p = ex.create("UTF-8");
    if (!p) return fail("out of memory"), NULL;
    Out out = {.parser = p};
    ex.set_user_data(p, &out);
    ex.set_element_handler(p, on_start, on_end);
    ex.set_character_handler(p, on_text);
    ex.set_comment_handler(p, on_comment);
    int64_t n = aion_bytes_size(text);
    void *result = NULL;
    int ok = n <= 0x7fffffff && ex.parse(p, (const char *)aion_bytes_data((void *)text), (int)n, 1);
    if (out.problem) {
        fail(out.problem);
    } else if (!ok) {
        snprintf(error_text, sizeof error_text, "%s at line %lu, column %lu", ex.error_string(ex.error_code(p)), ex.line(p), ex.column(p) + 1);
    } else {
        result = aion_bytes_new((int64_t)out.used);
        memcpy(aion_bytes_data(result), out.data, out.used);
    }
    free(out.data);
    ex.free_parser(p);
    return result;
}
