/*
 * C half of std.sqlite: databases and prepared statements, as small integer handles.
 * SQLite is the copy that comes with Aion (vendor/sqlite3), built into every program that imports std.sqlite.
 */

#include "aion.h"

#include <sqlite3.h>
#include <stdio.h>
#include <stdlib.h>
#include <string.h>

typedef struct { void *ptr; } Slot;
static Slot *dbs, *stmts;
static int64_t ndbs, nstmts;
static char error_text[512];

const char *aion_sqlite_error(void) { return aion_string(error_text); }
void aion_sqlite_set_error(const char *message) { snprintf(error_text, sizeof error_text, "%s", message); }

static int64_t add(Slot **table, int64_t *n, void *ptr) {
    for (int64_t i = 0; i < *n; i++)
        if (!(*table)[i].ptr) return (*table)[i].ptr = ptr, i + 1;
    *table = realloc(*table, (size_t)(*n + 16) * sizeof **table);
    if (!*table) aion_panic("out of memory");
    memset(*table + *n, 0, 16 * sizeof **table);
    (*table)[*n].ptr = ptr;
    *n += 16;
    return *n - 15;
}

static sqlite3 *db_of(int64_t h) { return h >= 1 && h <= ndbs ? dbs[h - 1].ptr : NULL; }
static sqlite3_stmt *stmt_of(int64_t h) { return h >= 1 && h <= nstmts ? stmts[h - 1].ptr : NULL; }

static void remember(sqlite3 *db) { snprintf(error_text, sizeof error_text, "%s", db ? sqlite3_errmsg(db) : "out of memory"); }

int64_t aion_sqlite_open(const char *path) {
    sqlite3 *db = NULL;
    if (sqlite3_open(path, &db) != SQLITE_OK) {
        remember(db);
        sqlite3_close(db);
        return -1;
    }
    sqlite3_busy_timeout(db, 5000);
    return add(&dbs, &ndbs, db);
}

void aion_sqlite_close(int64_t h) {
    sqlite3 *db = db_of(h);
    if (!db) return;
    sqlite3_stmt *s;
    while ((s = sqlite3_next_stmt(db, NULL))) sqlite3_finalize(s); /* open statements would make close fail */
    sqlite3_close(db);
    dbs[h - 1].ptr = NULL;
}

/* Run SQL text that may hold several statements and returns no rows. */
bool aion_sqlite_script(int64_t h, const char *sql) {
    sqlite3 *db = db_of(h);
    if (!db) return snprintf(error_text, sizeof error_text, "the database is closed"), false;
    char *message = NULL;
    if (sqlite3_exec(db, sql, NULL, NULL, &message) != SQLITE_OK) {
        snprintf(error_text, sizeof error_text, "%s", message ? message : "error");
        sqlite3_free(message);
        return false;
    }
    return true;
}

int64_t aion_sqlite_prepare(int64_t h, const char *sql) {
    sqlite3 *db = db_of(h);
    if (!db) return snprintf(error_text, sizeof error_text, "the database is closed"), -1;
    sqlite3_stmt *s = NULL;
    if (sqlite3_prepare_v2(db, sql, -1, &s, NULL) != SQLITE_OK) return remember(db), -1;
    if (!s) return snprintf(error_text, sizeof error_text, "the SQL text holds no statement"), -1;
    return add(&stmts, &nstmts, s);
}

void aion_sqlite_finalize(int64_t h) {
    sqlite3_stmt *s = stmt_of(h);
    if (!s) return;
    sqlite3_finalize(s);
    stmts[h - 1].ptr = NULL;
}

bool aion_sqlite_reset(int64_t h) {
    sqlite3_stmt *s = stmt_of(h);
    if (!s) return false;
    sqlite3_reset(s);
    sqlite3_clear_bindings(s);
    return true;
}

int64_t aion_sqlite_parameter_count(int64_t h) {
    sqlite3_stmt *s = stmt_of(h);
    return s ? sqlite3_bind_parameter_count(s) : 0;
}

/* Parameters are numbered from 1. */
bool aion_sqlite_bind_null(int64_t h, int64_t i) {
    sqlite3_stmt *s = stmt_of(h);
    return s && sqlite3_bind_null(s, (int)i) == SQLITE_OK;
}
bool aion_sqlite_bind_int(int64_t h, int64_t i, int64_t v) {
    sqlite3_stmt *s = stmt_of(h);
    return s && sqlite3_bind_int64(s, (int)i, v) == SQLITE_OK;
}
bool aion_sqlite_bind_float(int64_t h, int64_t i, double v) {
    sqlite3_stmt *s = stmt_of(h);
    return s && sqlite3_bind_double(s, (int)i, v) == SQLITE_OK;
}
bool aion_sqlite_bind_text(int64_t h, int64_t i, const char *v) {
    sqlite3_stmt *s = stmt_of(h);
    return s && sqlite3_bind_text(s, (int)i, v, -1, SQLITE_TRANSIENT) == SQLITE_OK;
}
bool aion_sqlite_bind_blob(int64_t h, int64_t i, const void *v) {
    sqlite3_stmt *s = stmt_of(h);
    return s && sqlite3_bind_blob64(s, (int)i, v, (sqlite3_uint64)aion_bytes_size(v), SQLITE_TRANSIENT) == SQLITE_OK;
}

/* 1: a row is ready, 0: finished, -1: error. */
int64_t aion_sqlite_step(int64_t h) {
    sqlite3_stmt *s = stmt_of(h);
    if (!s) return snprintf(error_text, sizeof error_text, "the statement is finalized"), -1;
    int r = sqlite3_step(s);
    if (r == SQLITE_ROW) return 1;
    if (r == SQLITE_DONE) return 0;
    remember(sqlite3_db_handle(s));
    return -1;
}

int64_t aion_sqlite_column_count(int64_t h) {
    sqlite3_stmt *s = stmt_of(h);
    return s ? sqlite3_column_count(s) : 0;
}
const char *aion_sqlite_column_name(int64_t h, int64_t i) {
    sqlite3_stmt *s = stmt_of(h);
    const char *n = s ? sqlite3_column_name(s, (int)i) : NULL;
    return aion_string(n ? n : "");
}
/* 1 integer, 2 float, 3 text, 4 blob, 5 null */
int64_t aion_sqlite_column_type(int64_t h, int64_t i) {
    sqlite3_stmt *s = stmt_of(h);
    return s ? sqlite3_column_type(s, (int)i) : 5;
}
int64_t aion_sqlite_column_int(int64_t h, int64_t i) {
    sqlite3_stmt *s = stmt_of(h);
    return s ? sqlite3_column_int64(s, (int)i) : 0;
}
double aion_sqlite_column_float(int64_t h, int64_t i) {
    sqlite3_stmt *s = stmt_of(h);
    return s ? sqlite3_column_double(s, (int)i) : 0;
}
const char *aion_sqlite_column_text(int64_t h, int64_t i) {
    sqlite3_stmt *s = stmt_of(h);
    if (!s) return aion_string("");
    const unsigned char *t = sqlite3_column_text(s, (int)i);
    int n = sqlite3_column_bytes(s, (int)i);
    return aion_string_n(t ? (const char *)t : "", t ? strnlen((const char *)t, (size_t)n) : 0);
}
void *aion_sqlite_column_blob(int64_t h, int64_t i) {
    sqlite3_stmt *s = stmt_of(h);
    const void *p = s ? sqlite3_column_blob(s, (int)i) : NULL;
    int n = s ? sqlite3_column_bytes(s, (int)i) : 0;
    void *out = aion_bytes_new(n);
    if (n) memcpy(aion_bytes_data(out), p, (size_t)n);
    return out;
}

int64_t aion_sqlite_changes(int64_t h) {
    sqlite3 *db = db_of(h);
    return db ? sqlite3_changes(db) : 0;
}
int64_t aion_sqlite_last_insert_id(int64_t h) {
    sqlite3 *db = db_of(h);
    return db ? sqlite3_last_insert_rowid(db) : 0;
}
const char *aion_sqlite_version(void) { return aion_string(sqlite3_libversion()); }
