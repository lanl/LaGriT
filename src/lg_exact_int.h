/*
 * lg_exact_int.h -- exact integer arithmetic for the geometric
 * predicates (lg_predicates2d.c, lg_predicates3d.c). Included by each;
 * every function is static.
 *
 * A coordinate difference that is exact in double (or exactly hi + lo
 * in double-double) is an integer in units of the smallest last-bit
 * value among the differences of a test. The determinants of the
 * predicates are then integer polynomials, computed here exactly:
 *   - fixed width: every integer below 2**62 (points close together:
 *     the usual case); 64 x 64 -> 128-bit products, accumulated in
 *     128-bit columns;
 *   - multi-word (big): anything else, up to BIG_W 64-bit words.
 * Written for LaGriT, Nov 2026.
 */
#ifndef LG_EXACT_INT_H
#define LG_EXACT_INT_H
#include <math.h>
#include <stdint.h>
#include <string.h>

/* exact x * 2**-u as an integer, |x * 2**-u| < 2**62; 0 if not */
static int to_int62(double x, int u, int64_t *k)
{
    uint64_t bits;
    double sc, t;
    if (u < -1000 || u > 1000) return 0;
    bits = (uint64_t) (1023 - u) << 52;
    memcpy(&sc, &bits, 8);
    t = x * sc;
    if (!(fabs(t) < 4611686018427387904.0)) return 0;   /* 2**62 */
    *k = (int64_t) t;
    return (double) *k == t;
}

/* the exponent of x's last nonzero bit (x normal, nonzero) */
static int lsb_exp_bits(double x, int *ok)
{
    uint64_t b;
    int e;
    memcpy(&b, &x, 8);
    e = (int) ((b >> 52) & 0x7ff);
    if (e == 0 || e == 0x7ff) { *ok = 0; return 0; }
    b = (b & 0xfffffffffffffULL) | 0x10000000000000ULL;
    return e - 1075 + __builtin_ctzll(b);
}

/* Column accumulators: a value is P - N, with P and N sums of
   unsigned 128-bit columns (column j weighs 2**(64 j)). ACC adds an
   unsigned 128-bit product x times 2**(64 k) to P or, when neg, to N:
   its low word to column k and its high word to column k + 1. A column
   receives a few dozen words at most, far from overflowing. */
#define MUL64(a, b) ((unsigned __int128) (uint64_t) (a) * (uint64_t) (b))
#define ACC(PN, k, x, neg) do { unsigned __int128 x_ = (x); \
    unsigned __int128 *t_ = (PN)[neg]; \
    t_[k] += (uint64_t) x_; t_[(k) + 1] += (uint64_t) (x_ >> 64); } while (0)

/* carry-propagate n columns into n words (the last column must fit) */
static void acc_words(const unsigned __int128 *c, int n, uint64_t *w)
{
    unsigned __int128 cy = 0;
    int j;
    for (j = 0; j < n; j++) {
        cy += c[j];
        w[j] = (uint64_t) cy;
        cy >>= 64;
    }
}

/* sign of P - N and |P - N| in nw words (P, N given as nw words) */
static int words_diff(const uint64_t *p, const uint64_t *q, int nw, uint64_t *w)
{
    int j, cmp = 0;
    const uint64_t *a, *b;
    unsigned __int128 br = 0;
    for (j = nw - 1; j >= 0 && cmp == 0; j--)
        if (p[j] != q[j]) cmp = p[j] > q[j] ? 1 : -1;
    if (cmp == 0) {
        for (j = 0; j < nw; j++) w[j] = 0;
        return 0;
    }
    a = cmp > 0 ? p : q;
    b = cmp > 0 ? q : p;
    for (j = 0; j < nw; j++) {
        unsigned __int128 dd_ = (unsigned __int128) a[j] - b[j] - br;
        w[j] = (uint64_t) dd_;
        br = (dd_ >> 64) & 1;
    }
    return cmp;
}


/* ------------------------------------------------------------------
   Exact stage: multi-word integers. Every coordinate difference is
   exactly hi + lo (two doubles from two_diff); written in units of the
   smallest last-bit value among them, each is an integer, and the
   determinant is computed exactly in multi-word integer arithmetic.
   Used only where double-double cannot decide (the determinant is
   below about 1e-28 of its scale, or exactly zero). If the integers
   would need more than BIG_W words (coordinate differences spanning
   more than about 500 binary orders of magnitude) the double-double
   answer is kept.
   ------------------------------------------------------------------ */
#define BIG_W 48
typedef struct { int n, neg; uint64_t w[BIG_W]; } big;   /* |v| = w[0..n) */

static void big_copy(big *r, const big *a)
{
    int i;
    r->n = a->n;
    r->neg = a->neg;
    for (i = 0; i < a->n; i++) r->w[i] = a->w[i];
}

static void big_norm(big *a)
{
    while (a->n > 0 && a->w[a->n - 1] == 0) a->n--;
    if (a->n == 0) a->neg = 0;
}

static int big_cmpabs(const big *a, const big *b)
{
    int i;
    if (a->n != b->n) return a->n < b->n ? -1 : 1;
    for (i = a->n - 1; i >= 0; i--)
        if (a->w[i] != b->w[i]) return a->w[i] < b->w[i] ? -1 : 1;
    return 0;
}

/* |r| = |a| + |b| */
static int big_addabs(big *r, const big *a, const big *b)
{
    int i, n = a->n > b->n ? a->n : b->n;
    unsigned __int128 c = 0;
    for (i = 0; i < n; i++) {
        c += (unsigned __int128) (i < a->n ? a->w[i] : 0) + (i < b->n ? b->w[i] : 0);
        r->w[i] = (uint64_t) c;
        c >>= 64;
    }
    if (c) {
        if (n >= BIG_W) return 0;
        r->w[n++] = (uint64_t) c;
    }
    r->n = n;
    return 1;
}

/* |r| = |a| - |b|, |a| >= |b| */
static void big_subabs(big *r, const big *a, const big *b)
{
    int i;
    uint64_t br = 0;
    for (i = 0; i < a->n; i++) {
        uint64_t bi = i < b->n ? b->w[i] : 0;
        uint64_t d = a->w[i] - bi - br;
        br = (a->w[i] < bi) || (a->w[i] == bi && br) ? 1 : 0;
        r->w[i] = d;
    }
    r->n = a->n;
    big_norm(r);
}

/* r = a + sb * b, sb = 1 or -1 */
static int big_add(big *r, const big *a, const big *b, int sb)
{
    int bneg = sb < 0 ? !b->neg : b->neg;
    big t;
    if (b->n == 0) { big_copy(r, a); return 1; }
    if (a->n == 0) { big_copy(r, b); r->neg = bneg; return 1; }
    if (a->neg == bneg) {
        if (!big_addabs(&t, a, b)) return 0;
        t.neg = a->neg;
    } else if (big_cmpabs(a, b) >= 0) {
        big_subabs(&t, a, b);
        t.neg = t.n ? a->neg : 0;
    } else {
        big_subabs(&t, b, a);
        t.neg = t.n ? bneg : 0;
    }
    big_copy(r, &t);
    return 1;
}

static int big_mul(big *r, const big *a, const big *b)
{
    big t;
    int i, j;
    if (a->n == 0 || b->n == 0) { r->n = 0; r->neg = 0; return 1; }
    if (a->n + b->n > BIG_W) return 0;
    for (i = 0; i < a->n + b->n; i++) t.w[i] = 0;
    for (i = 0; i < a->n; i++) {
        unsigned __int128 c = 0;
        for (j = 0; j < b->n; j++) {
            c += (unsigned __int128) a->w[i] * b->w[j] + t.w[i + j];
            t.w[i + j] = (uint64_t) c;
            c >>= 64;
        }
        t.w[i + b->n] = (uint64_t) c;
    }
    t.n = a->n + b->n;
    t.neg = a->neg != b->neg;
    big_norm(&t);
    big_copy(r, &t);
    return 1;
}

/* the exponent of the last bit of x (x = m * 2**e, m an odd integer) */
static int lsb_exp(double x)
{
    int e;
    double m = frexp(x, &e);          /* x = m * 2**e, 0.5 <= |m| < 1 */
    int64_t k = (int64_t) ldexp(m, 53); /* exact 53-bit integer */
    e -= 53;
    if (k == 0) return 0;
    while ((k & 1) == 0) { k >>= 1; e++; }
    return e;
}

/* exact x in units of 2**u (x a multiple of 2**u) */
static int big_from(big *r, double x, int u)
{
    int e, sh, i;
    double m;
    uint64_t k;
    r->n = 0; r->neg = 0;
    if (x == 0.0) return 1;
    m = frexp(fabs(x), &e);
    k = (uint64_t) ldexp(m, 53);
    sh = e - 53 - u;                   /* x = k * 2**(sh + u), sh >= 0 */
    if (sh < 0) {                      /* (low bits of k are zero) */
        k >>= -sh;
        sh = 0;
    }
    if (sh / 64 + 2 > BIG_W) return 0;
    for (i = 0; i < sh / 64 + 2; i++) r->w[i] = 0;
    r->w[sh / 64] = k << (sh % 64);
    if (sh % 64) r->w[sh / 64 + 1] = k >> (64 - sh % 64);
    r->n = sh / 64 + 2;
    r->neg = x < 0.0;
    big_norm(r);
    return 1;
}


#endif
