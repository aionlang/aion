/*
 * C half of std.hash: MD5, SHA-1, SHA-256 and SHA-512 digests, HMAC and CRC-32.
 * Self-contained; the round constants of SHA-2 and MD5 are derived from roots and sines
 * of small numbers, as in the standards.  MD5 and SHA-1 are broken for security use; they
 * are here for checksums and compatibility.
 */

#include "aion.h"

#include <stdio.h>
#include <stdlib.h>
#include <string.h>

enum { MD5, SHA1, SHA256, SHA512 };

typedef struct Hasher {
    int algo;
    uint64_t state[8];
    uint8_t buf[128];
    size_t fill, block_size, digest_size;
    uint64_t total;
} Hasher;

static const uint32_t K256[64] = {
    0x428a2f98u, 0x71374491u, 0xb5c0fbcfu, 0xe9b5dba5u,
    0x3956c25bu, 0x59f111f1u, 0x923f82a4u, 0xab1c5ed5u,
    0xd807aa98u, 0x12835b01u, 0x243185beu, 0x550c7dc3u,
    0x72be5d74u, 0x80deb1feu, 0x9bdc06a7u, 0xc19bf174u,
    0xe49b69c1u, 0xefbe4786u, 0x0fc19dc6u, 0x240ca1ccu,
    0x2de92c6fu, 0x4a7484aau, 0x5cb0a9dcu, 0x76f988dau,
    0x983e5152u, 0xa831c66du, 0xb00327c8u, 0xbf597fc7u,
    0xc6e00bf3u, 0xd5a79147u, 0x06ca6351u, 0x14292967u,
    0x27b70a85u, 0x2e1b2138u, 0x4d2c6dfcu, 0x53380d13u,
    0x650a7354u, 0x766a0abbu, 0x81c2c92eu, 0x92722c85u,
    0xa2bfe8a1u, 0xa81a664bu, 0xc24b8b70u, 0xc76c51a3u,
    0xd192e819u, 0xd6990624u, 0xf40e3585u, 0x106aa070u,
    0x19a4c116u, 0x1e376c08u, 0x2748774cu, 0x34b0bcb5u,
    0x391c0cb3u, 0x4ed8aa4au, 0x5b9cca4fu, 0x682e6ff3u,
    0x748f82eeu, 0x78a5636fu, 0x84c87814u, 0x8cc70208u,
    0x90befffau, 0xa4506cebu, 0xbef9a3f7u, 0xc67178f2u,
};
static const uint32_t H256[8] = {
    0x6a09e667u, 0xbb67ae85u, 0x3c6ef372u, 0xa54ff53au,
    0x510e527fu, 0x9b05688cu, 0x1f83d9abu, 0x5be0cd19u,
};
static const uint64_t K512[80] = {
    0x428a2f98d728ae22ull, 0x7137449123ef65cdull,
    0xb5c0fbcfec4d3b2full, 0xe9b5dba58189dbbcull,
    0x3956c25bf348b538ull, 0x59f111f1b605d019ull,
    0x923f82a4af194f9bull, 0xab1c5ed5da6d8118ull,
    0xd807aa98a3030242ull, 0x12835b0145706fbeull,
    0x243185be4ee4b28cull, 0x550c7dc3d5ffb4e2ull,
    0x72be5d74f27b896full, 0x80deb1fe3b1696b1ull,
    0x9bdc06a725c71235ull, 0xc19bf174cf692694ull,
    0xe49b69c19ef14ad2ull, 0xefbe4786384f25e3ull,
    0x0fc19dc68b8cd5b5ull, 0x240ca1cc77ac9c65ull,
    0x2de92c6f592b0275ull, 0x4a7484aa6ea6e483ull,
    0x5cb0a9dcbd41fbd4ull, 0x76f988da831153b5ull,
    0x983e5152ee66dfabull, 0xa831c66d2db43210ull,
    0xb00327c898fb213full, 0xbf597fc7beef0ee4ull,
    0xc6e00bf33da88fc2ull, 0xd5a79147930aa725ull,
    0x06ca6351e003826full, 0x142929670a0e6e70ull,
    0x27b70a8546d22ffcull, 0x2e1b21385c26c926ull,
    0x4d2c6dfc5ac42aedull, 0x53380d139d95b3dfull,
    0x650a73548baf63deull, 0x766a0abb3c77b2a8ull,
    0x81c2c92e47edaee6ull, 0x92722c851482353bull,
    0xa2bfe8a14cf10364ull, 0xa81a664bbc423001ull,
    0xc24b8b70d0f89791ull, 0xc76c51a30654be30ull,
    0xd192e819d6ef5218ull, 0xd69906245565a910ull,
    0xf40e35855771202aull, 0x106aa07032bbd1b8ull,
    0x19a4c116b8d2d0c8ull, 0x1e376c085141ab53ull,
    0x2748774cdf8eeb99ull, 0x34b0bcb5e19b48a8ull,
    0x391c0cb3c5c95a63ull, 0x4ed8aa4ae3418acbull,
    0x5b9cca4f7763e373ull, 0x682e6ff3d6b2b8a3ull,
    0x748f82ee5defb2fcull, 0x78a5636f43172f60ull,
    0x84c87814a1f0ab72ull, 0x8cc702081a6439ecull,
    0x90befffa23631e28ull, 0xa4506cebde82bde9ull,
    0xbef9a3f7b2c67915ull, 0xc67178f2e372532bull,
    0xca273eceea26619cull, 0xd186b8c721c0c207ull,
    0xeada7dd6cde0eb1eull, 0xf57d4f7fee6ed178ull,
    0x06f067aa72176fbaull, 0x0a637dc5a2c898a6ull,
    0x113f9804bef90daeull, 0x1b710b35131c471bull,
    0x28db77f523047d84ull, 0x32caab7b40c72493ull,
    0x3c9ebe0a15c9bebcull, 0x431d67c49c100d4cull,
    0x4cc5d4becb3e42b6ull, 0x597f299cfc657e2aull,
    0x5fcb6fab3ad6faecull, 0x6c44198c4a475817ull,
};
static const uint64_t H512[8] = {
    0x6a09e667f3bcc908ull, 0xbb67ae8584caa73bull,
    0x3c6ef372fe94f82bull, 0xa54ff53a5f1d36f1ull,
    0x510e527fade682d1ull, 0x9b05688c2b3e6c1full,
    0x1f83d9abfb41bd6bull, 0x5be0cd19137e2179ull,
};
static const uint32_t MD5_K[64] = {
    0xd76aa478u, 0xe8c7b756u, 0x242070dbu, 0xc1bdceeeu,
    0xf57c0fafu, 0x4787c62au, 0xa8304613u, 0xfd469501u,
    0x698098d8u, 0x8b44f7afu, 0xffff5bb1u, 0x895cd7beu,
    0x6b901122u, 0xfd987193u, 0xa679438eu, 0x49b40821u,
    0xf61e2562u, 0xc040b340u, 0x265e5a51u, 0xe9b6c7aau,
    0xd62f105du, 0x02441453u, 0xd8a1e681u, 0xe7d3fbc8u,
    0x21e1cde6u, 0xc33707d6u, 0xf4d50d87u, 0x455a14edu,
    0xa9e3e905u, 0xfcefa3f8u, 0x676f02d9u, 0x8d2a4c8au,
    0xfffa3942u, 0x8771f681u, 0x6d9d6122u, 0xfde5380cu,
    0xa4beea44u, 0x4bdecfa9u, 0xf6bb4b60u, 0xbebfbc70u,
    0x289b7ec6u, 0xeaa127fau, 0xd4ef3085u, 0x04881d05u,
    0xd9d4d039u, 0xe6db99e5u, 0x1fa27cf8u, 0xc4ac5665u,
    0xf4292244u, 0x432aff97u, 0xab9423a7u, 0xfc93a039u,
    0x655b59c3u, 0x8f0ccc92u, 0xffeff47du, 0x85845dd1u,
    0x6fa87e4fu, 0xfe2ce6e0u, 0xa3014314u, 0x4e0811a1u,
    0xf7537e82u, 0xbd3af235u, 0x2ad7d2bbu, 0xeb86d391u,
};
static const int MD5_S[64] = {7, 12, 17, 22, 7, 12, 17, 22, 7, 12, 17, 22, 7, 12, 17, 22, 5, 9,  14, 20, 5, 9,
                              14, 20, 5, 9,  14, 20, 5, 9,  14, 20, 4, 11, 16, 23, 4, 11, 16, 23, 4, 11, 16, 23,
                              4, 11, 16, 23, 6, 10, 15, 21, 6, 10, 15, 21, 6, 10, 15, 21, 6, 10, 15, 21};

#define ROTL32(x, n) (((x) << (n)) | ((x) >> (32 - (n))))
#define ROTR32(x, n) (((x) >> (n)) | ((x) << (32 - (n))))
#define ROTR64(x, n) (((x) >> (n)) | ((x) << (64 - (n))))

static uint32_t load_be32(const uint8_t *p) { return (uint32_t)p[0] << 24 | (uint32_t)p[1] << 16 | (uint32_t)p[2] << 8 | p[3]; }
static uint32_t load_le32(const uint8_t *p) { return (uint32_t)p[3] << 24 | (uint32_t)p[2] << 16 | (uint32_t)p[1] << 8 | p[0]; }
static uint64_t load_be64(const uint8_t *p) { return (uint64_t)load_be32(p) << 32 | load_be32(p + 4); }

static void md5_block(Hasher *h, const uint8_t *p) {
    uint32_t m[16], a = (uint32_t)h->state[0], b = (uint32_t)h->state[1], c = (uint32_t)h->state[2], d = (uint32_t)h->state[3];
    for (int i = 0; i < 16; i++) m[i] = load_le32(p + 4 * i);
    for (int i = 0; i < 64; i++) {
        uint32_t f;
        int g;
        if (i < 16) f = (b & c) | (~b & d), g = i;
        else if (i < 32) f = (d & b) | (~d & c), g = (5 * i + 1) % 16;
        else if (i < 48) f = b ^ c ^ d, g = (3 * i + 5) % 16;
        else f = c ^ (b | ~d), g = (7 * i) % 16;
        uint32_t t = d;
        d = c, c = b;
        b = b + ROTL32(a + f + MD5_K[i] + m[g], MD5_S[i]);
        a = t;
    }
    h->state[0] = (uint32_t)(h->state[0] + a), h->state[1] = (uint32_t)(h->state[1] + b);
    h->state[2] = (uint32_t)(h->state[2] + c), h->state[3] = (uint32_t)(h->state[3] + d);
}

static void sha1_block(Hasher *h, const uint8_t *p) {
    uint32_t w[80], a = (uint32_t)h->state[0], b = (uint32_t)h->state[1], c = (uint32_t)h->state[2],
             d = (uint32_t)h->state[3], e = (uint32_t)h->state[4];
    for (int i = 0; i < 16; i++) w[i] = load_be32(p + 4 * i);
    for (int i = 16; i < 80; i++) w[i] = ROTL32(w[i - 3] ^ w[i - 8] ^ w[i - 14] ^ w[i - 16], 1);
    for (int i = 0; i < 80; i++) {
        uint32_t f, k;
        if (i < 20) f = (b & c) | (~b & d), k = 0x5A827999u;
        else if (i < 40) f = b ^ c ^ d, k = 0x6ED9EBA1u;
        else if (i < 60) f = (b & c) | (b & d) | (c & d), k = 0x8F1BBCDCu;
        else f = b ^ c ^ d, k = 0xCA62C1D6u;
        uint32_t t = ROTL32(a, 5) + f + e + k + w[i];
        e = d, d = c, c = ROTL32(b, 30), b = a, a = t;
    }
    h->state[0] = (uint32_t)(h->state[0] + a), h->state[1] = (uint32_t)(h->state[1] + b);
    h->state[2] = (uint32_t)(h->state[2] + c), h->state[3] = (uint32_t)(h->state[3] + d);
    h->state[4] = (uint32_t)(h->state[4] + e);
}

static void sha256_block(Hasher *h, const uint8_t *p) {
    uint32_t w[64], v[8];
    for (int i = 0; i < 8; i++) v[i] = (uint32_t)h->state[i];
    for (int i = 0; i < 16; i++) w[i] = load_be32(p + 4 * i);
    for (int i = 16; i < 64; i++) {
        uint32_t s0 = ROTR32(w[i - 15], 7) ^ ROTR32(w[i - 15], 18) ^ (w[i - 15] >> 3);
        uint32_t s1 = ROTR32(w[i - 2], 17) ^ ROTR32(w[i - 2], 19) ^ (w[i - 2] >> 10);
        w[i] = w[i - 16] + s0 + w[i - 7] + s1;
    }
    for (int i = 0; i < 64; i++) {
        uint32_t s1 = ROTR32(v[4], 6) ^ ROTR32(v[4], 11) ^ ROTR32(v[4], 25);
        uint32_t ch = (v[4] & v[5]) ^ (~v[4] & v[6]);
        uint32_t t1 = v[7] + s1 + ch + K256[i] + w[i];
        uint32_t s0 = ROTR32(v[0], 2) ^ ROTR32(v[0], 13) ^ ROTR32(v[0], 22);
        uint32_t maj = (v[0] & v[1]) ^ (v[0] & v[2]) ^ (v[1] & v[2]);
        uint32_t t2 = s0 + maj;
        v[7] = v[6], v[6] = v[5], v[5] = v[4], v[4] = v[3] + t1;
        v[3] = v[2], v[2] = v[1], v[1] = v[0], v[0] = t1 + t2;
    }
    for (int i = 0; i < 8; i++) h->state[i] = (uint32_t)(h->state[i] + v[i]);
}

static void sha512_block(Hasher *h, const uint8_t *p) {
    uint64_t w[80], v[8];
    for (int i = 0; i < 8; i++) v[i] = h->state[i];
    for (int i = 0; i < 16; i++) w[i] = load_be64(p + 8 * i);
    for (int i = 16; i < 80; i++) {
        uint64_t s0 = ROTR64(w[i - 15], 1) ^ ROTR64(w[i - 15], 8) ^ (w[i - 15] >> 7);
        uint64_t s1 = ROTR64(w[i - 2], 19) ^ ROTR64(w[i - 2], 61) ^ (w[i - 2] >> 6);
        w[i] = w[i - 16] + s0 + w[i - 7] + s1;
    }
    for (int i = 0; i < 80; i++) {
        uint64_t s1 = ROTR64(v[4], 14) ^ ROTR64(v[4], 18) ^ ROTR64(v[4], 41);
        uint64_t ch = (v[4] & v[5]) ^ (~v[4] & v[6]);
        uint64_t t1 = v[7] + s1 + ch + K512[i] + w[i];
        uint64_t s0 = ROTR64(v[0], 28) ^ ROTR64(v[0], 34) ^ ROTR64(v[0], 39);
        uint64_t maj = (v[0] & v[1]) ^ (v[0] & v[2]) ^ (v[1] & v[2]);
        uint64_t t2 = s0 + maj;
        v[7] = v[6], v[6] = v[5], v[5] = v[4], v[4] = v[3] + t1;
        v[3] = v[2], v[2] = v[1], v[1] = v[0], v[0] = t1 + t2;
    }
    for (int i = 0; i < 8; i++) h->state[i] += v[i];
}

static void init(Hasher *h, int algo) {
    memset(h, 0, sizeof *h);
    h->algo = algo;
    switch (algo) {
    case MD5:
        h->state[0] = 0x67452301u, h->state[1] = 0xefcdab89u, h->state[2] = 0x98badcfeu, h->state[3] = 0x10325476u;
        h->block_size = 64, h->digest_size = 16;
        break;
    case SHA1:
        h->state[0] = 0x67452301u, h->state[1] = 0xefcdab89u, h->state[2] = 0x98badcfeu;
        h->state[3] = 0x10325476u, h->state[4] = 0xc3d2e1f0u;
        h->block_size = 64, h->digest_size = 20;
        break;
    case SHA256:
        for (int i = 0; i < 8; i++) h->state[i] = H256[i];
        h->block_size = 64, h->digest_size = 32;
        break;
    default:
        for (int i = 0; i < 8; i++) h->state[i] = H512[i];
        h->block_size = 128, h->digest_size = 64;
    }
}

static void block(Hasher *h, const uint8_t *p) {
    switch (h->algo) {
    case MD5: md5_block(h, p); break;
    case SHA1: sha1_block(h, p); break;
    case SHA256: sha256_block(h, p); break;
    default: sha512_block(h, p);
    }
}

static void update(Hasher *h, const uint8_t *data, size_t n) {
    h->total += n;
    while (n > 0) {
        if (h->fill == 0 && n >= h->block_size) {
            block(h, data);
            data += h->block_size, n -= h->block_size;
            continue;
        }
        size_t take = h->block_size - h->fill;
        if (take > n) take = n;
        memcpy(h->buf + h->fill, data, take);
        h->fill += take, data += take, n -= take;
        if (h->fill == h->block_size) block(h, h->buf), h->fill = 0;
    }
}

static void finish(Hasher *h, uint8_t *out) {
    uint64_t bits = h->total * 8;
    size_t len_bytes = h->algo == SHA512 ? 16 : 8;
    uint8_t pad[256] = {0x80};
    size_t zeros = (h->block_size + h->block_size - len_bytes - 1 - h->fill % h->block_size) % h->block_size;
    size_t n = 1 + zeros;
    for (size_t i = 0; i < 8; i++) {
        uint8_t byte = (uint8_t)(bits >> (8 * i));
        if (h->algo == MD5) pad[n + i] = byte;                      /* little-endian length */
        else pad[n + len_bytes - 1 - i] = byte;                     /* big-endian, low 64 bits last */
    }
    update(h, pad, n + len_bytes);
    h->total -= n + len_bytes;
    int word = h->algo == SHA512 ? 8 : 4;
    for (size_t i = 0; i < h->digest_size; i++) {
        uint64_t v = h->state[i / (size_t)word];
        int shift = h->algo == MD5 ? 8 * (int)(i % 4) : 8 * (word - 1 - (int)(i % (size_t)word));
        out[i] = (uint8_t)(v >> shift);
    }
}

static void digest(int algo, const uint8_t *data, size_t n, uint8_t *out) {
    Hasher h;
    init(&h, algo);
    update(&h, data, n);
    finish(&h, out);
}

static size_t digest_size(int64_t algo) {
    if (algo < MD5 || algo > SHA512) aion_panic("unknown hash algorithm");
    return algo == MD5 ? 16 : algo == SHA1 ? 20 : algo == SHA256 ? 32 : 64;
}

/* algorithm: 0 MD5, 1 SHA-1, 2 SHA-256, 3 SHA-512 */
void *aion_hash_digest(int64_t algo, const void *data) {
    void *out = aion_bytes_new((int64_t)digest_size(algo));
    digest((int)algo, data, (size_t)aion_bytes_size(data), aion_bytes_data(out));
    return out;
}

void *aion_hash_hmac(int64_t algo, const void *key, const void *message) {
    size_t size = digest_size(algo), block_size = algo == SHA512 ? 128 : 64;
    uint8_t k[128] = {0}, inner[128], outer[128], tmp[64];
    size_t key_len = (size_t)aion_bytes_size(key);
    if (key_len > block_size) digest((int)algo, key, key_len, k);
    else memcpy(k, key, key_len);
    for (size_t i = 0; i < block_size; i++) inner[i] = k[i] ^ 0x36, outer[i] = k[i] ^ 0x5c;
    Hasher h;
    init(&h, (int)algo);
    update(&h, inner, block_size);
    update(&h, message, (size_t)aion_bytes_size(message));
    finish(&h, tmp);
    init(&h, (int)algo);
    update(&h, outer, block_size);
    update(&h, tmp, size);
    void *out = aion_bytes_new((int64_t)size);
    finish(&h, aion_bytes_data(out));
    return out;
}

/* CRC-32 (the one used by zip, gzip and PNG), continuing from `crc` (0 to start). */
int64_t aion_hash_crc32(const void *data, int64_t crc) {
    static uint32_t table[256];
    if (!table[1]) {
        for (uint32_t i = 0; i < 256; i++) {
            uint32_t c = i;
            for (int k = 0; k < 8; k++) c = c & 1 ? 0xEDB88320u ^ (c >> 1) : c >> 1;
            table[i] = c;
        }
    }
    uint32_t c = ~(uint32_t)crc;
    const uint8_t *p = data;
    for (int64_t i = 0, n = aion_bytes_size(data); i < n; i++) c = table[(c ^ p[i]) & 0xFF] ^ (c >> 8);
    return (int64_t)(uint32_t)~c;
}
