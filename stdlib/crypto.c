/*
 * C half of std.crypto: authenticated encryption (AES-GCM, ChaCha20-Poly1305), PBKDF2, Ed25519 signatures
 * and a constant-time comparison (POSIX).
 *
 * The algorithms come from OpenSSL's libcrypto, loaded at run time with dlopen like the TLS of std.net: a
 * program that never imports std.crypto needs no OpenSSL, and no header files are required to build.
 * Nothing here is implemented by hand except the comparison.
 */

#define _GNU_SOURCE
#include "aion.h"

#include "aion_platform.h"
#include <stdio.h>
#include <stdlib.h>
#include <string.h>

static char error_text[256];

const char *aion_crypto_error(void) { return aion_string(error_text); }

static bool fail(const char *message) {
    snprintf(error_text, sizeof error_text, "%s", message);
    return false;
}

enum { EVP_CTRL_AEAD_SET_IVLEN = 0x9, EVP_CTRL_AEAD_GET_TAG = 0x10, EVP_CTRL_AEAD_SET_TAG = 0x11, EVP_PKEY_ED25519 = 1087 };

static struct {
    bool tried, ok;
    void *(*EVP_CIPHER_CTX_new)(void);
    void (*EVP_CIPHER_CTX_free)(void *);
    int (*EVP_CIPHER_CTX_ctrl)(void *, int, int, void *);
    const void *(*EVP_aes_128_gcm)(void);
    const void *(*EVP_aes_192_gcm)(void);
    const void *(*EVP_aes_256_gcm)(void);
    const void *(*EVP_chacha20_poly1305)(void);
    int (*EVP_EncryptInit_ex)(void *, const void *, void *, const unsigned char *, const unsigned char *);
    int (*EVP_EncryptUpdate)(void *, unsigned char *, int *, const unsigned char *, int);
    int (*EVP_EncryptFinal_ex)(void *, unsigned char *, int *);
    int (*EVP_DecryptInit_ex)(void *, const void *, void *, const unsigned char *, const unsigned char *);
    int (*EVP_DecryptUpdate)(void *, unsigned char *, int *, const unsigned char *, int);
    int (*EVP_DecryptFinal_ex)(void *, unsigned char *, int *);
    int (*RAND_bytes)(unsigned char *, int);
    const void *(*EVP_sha1)(void);
    const void *(*EVP_sha256)(void);
    const void *(*EVP_sha512)(void);
    int (*PKCS5_PBKDF2_HMAC)(const char *, int, const unsigned char *, int, int, const void *, int, unsigned char *);
    void *(*EVP_PKEY_CTX_new_id)(int, void *);
    int (*EVP_PKEY_keygen_init)(void *);
    int (*EVP_PKEY_keygen)(void *, void **);
    void (*EVP_PKEY_CTX_free)(void *);
    void (*EVP_PKEY_free)(void *);
    int (*EVP_PKEY_get_raw_private_key)(const void *, unsigned char *, size_t *);
    int (*EVP_PKEY_get_raw_public_key)(const void *, unsigned char *, size_t *);
    void *(*EVP_PKEY_new_raw_private_key)(int, void *, const unsigned char *, size_t);
    void *(*EVP_PKEY_new_raw_public_key)(int, void *, const unsigned char *, size_t);
    void *(*EVP_MD_CTX_new)(void);
    void (*EVP_MD_CTX_free)(void *);
    int (*EVP_DigestSignInit)(void *, void **, const void *, void *, void *);
    int (*EVP_DigestSign)(void *, unsigned char *, size_t *, const unsigned char *, size_t);
    int (*EVP_DigestVerifyInit)(void *, void **, const void *, void *, void *);
    int (*EVP_DigestVerify)(void *, const unsigned char *, size_t, const unsigned char *, size_t);
} ssl;

static bool load(void) {
    if (ssl.tried) return ssl.ok;
    ssl.tried = true;
    #ifdef _WIN32
    static const char *const names[] = {"libcrypto-3-x64.dll", "libcrypto-1_1-x64.dll", "libcrypto-3.dll", NULL};
    #else
    static const char *const names[] = {"libcrypto.so.3", "libcrypto.so.1.1", "libcrypto.so", "libcrypto.3.dylib", "libcrypto.dylib", NULL};
    #endif
    void *lib = aion_dlopen(names);
    if (!lib) return fail("std.crypto needs OpenSSL: install libssl (for example the libssl3 or openssl package)");
#define LOAD(name) \
    if (!(*(void **)&ssl.name = aion_dlsym(lib, #name))) return fail("the installed OpenSSL is missing " #name);
    LOAD(EVP_CIPHER_CTX_new) LOAD(EVP_CIPHER_CTX_free) LOAD(EVP_CIPHER_CTX_ctrl) LOAD(EVP_aes_128_gcm) LOAD(EVP_aes_192_gcm)
    LOAD(EVP_aes_256_gcm) LOAD(EVP_chacha20_poly1305) LOAD(EVP_EncryptInit_ex) LOAD(EVP_EncryptUpdate) LOAD(EVP_EncryptFinal_ex)
    LOAD(EVP_DecryptInit_ex) LOAD(EVP_DecryptUpdate) LOAD(EVP_DecryptFinal_ex) LOAD(RAND_bytes) LOAD(EVP_sha1) LOAD(EVP_sha256)
    LOAD(EVP_sha512) LOAD(PKCS5_PBKDF2_HMAC) LOAD(EVP_PKEY_CTX_new_id) LOAD(EVP_PKEY_keygen_init) LOAD(EVP_PKEY_keygen)
    LOAD(EVP_PKEY_CTX_free) LOAD(EVP_PKEY_free) LOAD(EVP_PKEY_get_raw_private_key) LOAD(EVP_PKEY_get_raw_public_key)
    LOAD(EVP_PKEY_new_raw_private_key) LOAD(EVP_PKEY_new_raw_public_key) LOAD(EVP_MD_CTX_new) LOAD(EVP_MD_CTX_free)
    LOAD(EVP_DigestSignInit) LOAD(EVP_DigestSign) LOAD(EVP_DigestVerifyInit) LOAD(EVP_DigestVerify)
#undef LOAD
    ssl.ok = true;
    return true;
}

enum { NONCE = 12, TAG = 16 };

/* Encrypt (`encrypt` set) or decrypt `data` with the key.  Encrypting gives nonce || ciphertext || tag with a
 * fresh random nonce; decrypting expects that layout and gives NULL when the tag does not match. */
void *aion_crypto_aead(int64_t algorithm, bool encrypt, const void *key_bytes, const void *data_bytes, const void *aad_bytes) {
    if (!load()) return NULL;
    const uint8_t *key = aion_bytes_data((void *)key_bytes), *data = aion_bytes_data((void *)data_bytes);
    const uint8_t *aad = aion_bytes_data((void *)aad_bytes);
    int64_t key_len = aion_bytes_size(key_bytes), data_len = aion_bytes_size(data_bytes), aad_len = aion_bytes_size(aad_bytes);
    const void *cipher;
    if (algorithm == 0) {
        cipher = key_len == 16 ? ssl.EVP_aes_128_gcm() : key_len == 24 ? ssl.EVP_aes_192_gcm() : key_len == 32 ? ssl.EVP_aes_256_gcm() : NULL;
        if (!cipher) return fail("an AES key must be 16, 24 or 32 bytes long"), NULL;
    } else {
        if (key_len != 32) return fail("a ChaCha20 key must be 32 bytes long"), NULL;
        cipher = ssl.EVP_chacha20_poly1305();
    }
    if (!encrypt && data_len < NONCE + TAG) return fail("the data is too short to be encrypted data"), NULL;
    int64_t text_len = encrypt ? data_len : data_len - NONCE - TAG;
    if (text_len > 0x7fffff00 || aad_len > 0x7fffff00) return fail("the data is too large"), NULL;

    uint8_t nonce[NONCE], tag[TAG];
    const uint8_t *input = data;
    if (encrypt) {
        if (ssl.RAND_bytes(nonce, NONCE) != 1) return fail("the random number generator failed"), NULL;
    } else {
        memcpy(nonce, data, NONCE);
        memcpy(tag, data + data_len - TAG, TAG);
        input = data + NONCE;
    }
    void *ctx = ssl.EVP_CIPHER_CTX_new();
    if (!ctx) return fail("out of memory"), NULL;
    uint8_t *out = malloc(text_len + 16);
    void *result = NULL;
    int len = 0, total = 0, ok;
    if (!out) {
        fail("out of memory");
        goto done;
    }
    ok = encrypt ? ssl.EVP_EncryptInit_ex(ctx, cipher, NULL, NULL, NULL) : ssl.EVP_DecryptInit_ex(ctx, cipher, NULL, NULL, NULL);
    ok = ok && ssl.EVP_CIPHER_CTX_ctrl(ctx, EVP_CTRL_AEAD_SET_IVLEN, NONCE, NULL);
    ok = ok && (encrypt ? ssl.EVP_EncryptInit_ex(ctx, NULL, NULL, key, nonce) : ssl.EVP_DecryptInit_ex(ctx, NULL, NULL, key, nonce));
    if (ok && aad_len > 0) ok = encrypt ? ssl.EVP_EncryptUpdate(ctx, NULL, &len, aad, (int)aad_len) : ssl.EVP_DecryptUpdate(ctx, NULL, &len, aad, (int)aad_len);
    if (ok && text_len > 0) {
        ok = encrypt ? ssl.EVP_EncryptUpdate(ctx, out, &len, input, (int)text_len) : ssl.EVP_DecryptUpdate(ctx, out, &len, input, (int)text_len);
        total = len;
    }
    if (ok && !encrypt) ok = ssl.EVP_CIPHER_CTX_ctrl(ctx, EVP_CTRL_AEAD_SET_TAG, TAG, tag);
    if (ok) {
        ok = encrypt ? ssl.EVP_EncryptFinal_ex(ctx, out + total, &len) : ssl.EVP_DecryptFinal_ex(ctx, out + total, &len);
        if (!ok && !encrypt) {
            fail("the data was changed or the key is wrong");
            goto done;
        }
        total += len;
    }
    if (ok && encrypt) ok = ssl.EVP_CIPHER_CTX_ctrl(ctx, EVP_CTRL_AEAD_GET_TAG, TAG, tag);
    if (!ok) {
        fail("encryption failed");
        goto done;
    }
    if (encrypt) {
        result = aion_bytes_new(NONCE + total + TAG);
        uint8_t *dst = aion_bytes_data(result);
        memcpy(dst, nonce, NONCE);
        memcpy(dst + NONCE, out, total);
        memcpy(dst + NONCE + total, tag, TAG);
    } else {
        result = aion_bytes_new(total);
        memcpy(aion_bytes_data(result), out, total);
    }
done:
    free(out);
    ssl.EVP_CIPHER_CTX_free(ctx);
    return result;
}

/* PBKDF2 with HMAC-SHA1 (1), SHA-256 (2) or SHA-512 (3). */
void *aion_crypto_pbkdf2(const void *password, const void *salt, int64_t iterations, int64_t length, int64_t digest) {
    if (!load()) return NULL;
    if (iterations < 1 || iterations > 0x7fffffff) return fail("the iteration count must be at least 1"), NULL;
    if (length < 1 || length > 1 << 20) return fail("the length must be between 1 and 1048576 bytes"), NULL;
    const void *md = digest == 1 ? ssl.EVP_sha1() : digest == 3 ? ssl.EVP_sha512() : ssl.EVP_sha256();
    void *out = aion_bytes_new(length);
    int ok = ssl.PKCS5_PBKDF2_HMAC((const char *)aion_bytes_data((void *)password), (int)aion_bytes_size(password),
                                   aion_bytes_data((void *)salt), (int)aion_bytes_size(salt), (int)iterations, md, (int)length,
                                   aion_bytes_data(out));
    if (!ok) return fail("key derivation failed"), NULL;
    return out;
}

/* Time depends only on the length, not on where the contents differ. */
bool aion_crypto_equal(const void *a, const void *b) {
    int64_t n = aion_bytes_size(a);
    if (n != aion_bytes_size(b)) return false;
    const uint8_t *x = aion_bytes_data((void *)a), *y = aion_bytes_data((void *)b);
    uint8_t diff = 0;
    for (int64_t i = 0; i < n; i++) diff |= x[i] ^ y[i];
    return diff == 0;
}

/* A new Ed25519 key: the 32-byte private key followed by the 32-byte public key. */
void *aion_crypto_ed25519_generate(void) {
    if (!load()) return NULL;
    void *ctx = ssl.EVP_PKEY_CTX_new_id(EVP_PKEY_ED25519, NULL), *key = NULL, *out = NULL;
    if (!ctx || ssl.EVP_PKEY_keygen_init(ctx) != 1 || ssl.EVP_PKEY_keygen(ctx, &key) != 1) {
        fail("this OpenSSL cannot make Ed25519 keys");
    } else {
        out = aion_bytes_new(64);
        uint8_t *d = aion_bytes_data(out);
        size_t n = 32, m = 32;
        if (ssl.EVP_PKEY_get_raw_private_key(key, d, &n) != 1 || ssl.EVP_PKEY_get_raw_public_key(key, d + 32, &m) != 1) {
            fail("cannot read the new key");
            out = NULL;
        }
    }
    if (key) ssl.EVP_PKEY_free(key);
    if (ctx) ssl.EVP_PKEY_CTX_free(ctx);
    return out;
}

/* The public key (32 bytes) for a private key (32 bytes). */
void *aion_crypto_ed25519_public(const void *private_key) {
    if (!load()) return NULL;
    if (aion_bytes_size(private_key) != 32) return fail("an Ed25519 private key is 32 bytes"), NULL;
    void *key = ssl.EVP_PKEY_new_raw_private_key(EVP_PKEY_ED25519, NULL, aion_bytes_data((void *)private_key), 32), *out = NULL;
    if (!key) return fail("not a usable Ed25519 private key"), NULL;
    size_t n = 32;
    out = aion_bytes_new(32);
    if (ssl.EVP_PKEY_get_raw_public_key(key, aion_bytes_data(out), &n) != 1) {
        fail("cannot compute the public key");
        out = NULL;
    }
    ssl.EVP_PKEY_free(key);
    return out;
}

void *aion_crypto_ed25519_sign(const void *private_key, const void *message) {
    if (!load()) return NULL;
    if (aion_bytes_size(private_key) != 32) return fail("an Ed25519 private key is 32 bytes"), NULL;
    void *key = ssl.EVP_PKEY_new_raw_private_key(EVP_PKEY_ED25519, NULL, aion_bytes_data((void *)private_key), 32), *out = NULL;
    void *md = ssl.EVP_MD_CTX_new();
    if (!key || !md) {
        fail("not a usable Ed25519 private key");
    } else {
        size_t n = 64;
        out = aion_bytes_new(64);
        if (ssl.EVP_DigestSignInit(md, NULL, NULL, NULL, key) != 1 ||
            ssl.EVP_DigestSign(md, aion_bytes_data(out), &n, aion_bytes_data((void *)message), aion_bytes_size(message)) != 1) {
            fail("signing failed");
            out = NULL;
        }
    }
    if (md) ssl.EVP_MD_CTX_free(md);
    if (key) ssl.EVP_PKEY_free(key);
    return out;
}

bool aion_crypto_ed25519_verify(const void *public_key, const void *message, const void *signature) {
    if (!load()) return false;
    if (aion_bytes_size(public_key) != 32 || aion_bytes_size(signature) != 64) return false;
    void *key = ssl.EVP_PKEY_new_raw_public_key(EVP_PKEY_ED25519, NULL, aion_bytes_data((void *)public_key), 32);
    void *md = ssl.EVP_MD_CTX_new();
    bool good = false;
    if (key && md && ssl.EVP_DigestVerifyInit(md, NULL, NULL, NULL, key) == 1) {
        good = ssl.EVP_DigestVerify(md, aion_bytes_data((void *)signature), 64, aion_bytes_data((void *)message),
                                    aion_bytes_size(message)) == 1;
    }
    if (md) ssl.EVP_MD_CTX_free(md);
    if (key) ssl.EVP_PKEY_free(key);
    return good;
}
