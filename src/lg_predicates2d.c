/*
 * lg_predicates2d.c -- orientation and in-circle tests for 2D connect
 *
 * Two geometric tests whose sign is reliable even when the points are
 * nearly collinear or nearly cocircular:
 *
 *   lg_orient2d_    is c left of, on, or right of the line a -> b?
 *   lg_incircle2d_  is p inside, on, or outside the circle through
 *                   a, b and c (whatever the order of a, b, c)?
 *
 * Each test is first done in ordinary double precision, translated to
 * one of the points so that large coordinates (UTM, say) do not lose
 * digits. If the result is larger than a bound on its rounding error
 * (the static bounds of J. R. Shewchuk, "Adaptive precision floating-
 * point arithmetic and fast robust geometric predicates", Discrete &
 * Computational Geometry 18, 1997), its sign is certain. Otherwise
 * the test is done again in double-double arithmetic (about 106 bits).
 * If that result is also within its error bound, the test is done
 * exactly in integer arithmetic (lg_exact_int.h): the answer 0 ("on
 * the line" or "on the circle") then means exactly on it.
 *
 * This is not Shewchuk's predicates.c; it uses only his error bounds.
 * Written for LaGriT, Nov 2026.
 *
 * Called from Fortran (8-byte integers):
 *   call lg_orient2d(ax,ay,bx,by,cx,cy,isign)
 *   call lg_incircle2d(ax,ay,bx,by,cx,cy,px,py,isign)
 * isign = 1, 0 or -1.
 */

#include <math.h>
#include <stdint.h>
#include "lg_exact_int.h"

/* unit roundoff of a double, 2**-53 */
#define LG_EPS 1.1102230246251565e-16
/* Shewchuk's first-stage bounds */
#define CCW_BOUND ((3.0 + 16.0 * LG_EPS) * LG_EPS)
#define ICC_BOUND ((10.0 + 96.0 * LG_EPS) * LG_EPS)
/* double-double stage: about 2**-104 relative per operation; a
   generous multiple covers the few operations of each test */
#define DD_EPS 4.930380657631324e-32
#define DD_CCW_BOUND (16.0 * DD_EPS)
#define DD_ICC_BOUND (256.0 * DD_EPS)

typedef struct { double hi, lo; } dd;

/* a + b = s + err exactly */
static dd two_sum(double a, double b)
{
    dd r;
    double bb;
    r.hi = a + b;
    bb = r.hi - a;
    r.lo = (a - (r.hi - bb)) + (b - bb);
    return r;
}

/* a - b = s + err exactly */
static dd two_diff(double a, double b)
{
    return two_sum(a, -b);
}

/* a * b = p + err exactly (fma is exact by definition) */
static dd two_prod(double a, double b)
{
    dd r;
    r.hi = a * b;
    r.lo = fma(a, b, -r.hi);
    return r;
}

static dd dd_norm(double s, double e)
{
    dd r;
    r.hi = s + e;
    r.lo = e - (r.hi - s);
    return r;
}

static dd dd_add(dd a, dd b)
{
    dd s = two_sum(a.hi, b.hi);
    return dd_norm(s.hi, s.lo + a.lo + b.lo);
}

static dd dd_neg(dd a)
{
    dd r;
    r.hi = -a.hi;
    r.lo = -a.lo;
    return r;
}

static dd dd_mul(dd a, dd b)
{
    dd p = two_prod(a.hi, b.hi);
    return dd_norm(p.hi, p.lo + (a.hi * b.lo + a.lo * b.hi));
}

static int64_t sign_of(double v)
{
    return (v > 0.0) - (v < 0.0);
}

static int64_t dd_sign(dd v)
{
    return v.hi != 0.0 ? sign_of(v.hi) : sign_of(v.lo);
}


/* the unit (power of two) of the smallest last bit among n values, for
   lsb_exp_bits / big_from; 0 if all are zero. ok = 0 for a subnormal. */
static int unit_of(const double *x, int n, int *ok)
{
    int i, u = 0, first = 1;
    *ok = 1;
    for (i = 0; i < n; i++)
        if (x[i] != 0.0) {
            int e = lsb_exp_bits(x[i], ok);
            if (!*ok) return 0;
            if (first || e < u) u = e;
            first = 0;
        }
    return u;
}

/* exact sign of adx bdy - ady bdx from the differences (each hi + lo) */
static int64_t orient2d_exact(const dd *D)
{
    double x[8];
    int i, ok, u, allhi = 1;
    for (i = 0; i < 4; i++) {
        x[2 * i] = D[i].hi;
        x[2 * i + 1] = D[i].lo;
        if (D[i].lo != 0.0) allhi = 0;
    }
    u = unit_of(x, 8, &ok);
    if (ok && allhi) {
        int64_t v[4];
        for (i = 0; i < 4; i++)
            if (!to_int62(D[i].hi, u, &v[i])) break;
        if (i == 4) {
            __int128 det = (__int128) v[0] * v[3] - (__int128) v[1] * v[2];
            return (det > 0) - (det < 0);
        }
    }
    {
        big b[4], t, w;
        for (i = 0; i < 4; i++) {
            big h;
            if (!big_from(&h, D[i].hi, u) || !big_from(&t, D[i].lo, u) ||
                !big_add(&b[i], &h, &t, 1))
                return 2;
        }
        if (!big_mul(&t, &b[0], &b[3]) || !big_mul(&w, &b[1], &b[2]) ||
            !big_add(&t, &t, &w, -1))
            return 2;
        return t.n == 0 ? 0 : (t.neg ? -1 : 1);
    }
}

/* exact sign of alift (bdx cdy - cdx bdy) + blift (cdx ady - adx cdy)
   + clift (adx bdy - bdx ady) from the differences (each hi + lo) */
static int64_t incircle_exact(const dd *D)
{
    double x[12];
    int i, j, k, ok, u, allhi = 1;
    static const int M[3][4] = {{2, 5, 4, 3}, {4, 1, 0, 5}, {0, 3, 2, 1}};
    for (i = 0; i < 6; i++) {
        x[2 * i] = D[i].hi;
        x[2 * i + 1] = D[i].lo;
        if (D[i].lo != 0.0) allhi = 0;
    }
    u = unit_of(x, 12, &ok);
    if (ok && allhi) {
        int64_t v[6];
        uint64_t av[6], pw[5], nw[5], w[5];
        unsigned __int128 PN[2][6];
        for (i = 0; i < 6; i++) {
            if (!to_int62(D[i].hi, u, &v[i])) break;
            av[i] = (uint64_t) (v[i] < 0 ? -v[i] : v[i]);
        }
        if (i == 6) {
            /* |v| < 2**62: minors < 2**125, lifts < 2**125, products
               < 2**250 */
            memset(PN, 0, sizeof PN);
            for (k = 0; k < 3; k++) {
                const int *q = M[k];
                __int128 m = (__int128) v[q[0]] * v[q[1]]
                           - (__int128) v[q[2]] * v[q[3]];
                unsigned __int128 am = (unsigned __int128) (m < 0 ? -m : m);
                unsigned __int128 l = MUL64(av[2 * k], av[2 * k])
                                    + MUL64(av[2 * k + 1], av[2 * k + 1]);
                uint64_t mw[2] = {(uint64_t) am, (uint64_t) (am >> 64)};
                uint64_t lw[2] = {(uint64_t) l, (uint64_t) (l >> 64)};
                int neg = m < 0;
                for (i = 0; i < 2; i++)
                    for (j = 0; j < 2; j++)
                        ACC(PN, i + j, MUL64(lw[i], mw[j]), neg);
            }
            acc_words(PN[0], 5, pw);
            acc_words(PN[1], 5, nw);
            return words_diff(pw, nw, 5, w);
        }
    }
    {
        big b[6], t, h, m, l, acc;
        acc.n = 0; acc.neg = 0;
        for (i = 0; i < 6; i++)
            if (!big_from(&h, D[i].hi, u) || !big_from(&t, D[i].lo, u) ||
                !big_add(&b[i], &h, &t, 1))
                return 2;
        for (k = 0; k < 3; k++) {
            const int *q = M[k];
            if (!big_mul(&t, &b[q[0]], &b[q[1]]) ||
                !big_mul(&h, &b[q[2]], &b[q[3]]) || !big_add(&m, &t, &h, -1))
                return 2;
            if (!big_mul(&t, &b[2 * k], &b[2 * k]) ||
                !big_mul(&h, &b[2 * k + 1], &b[2 * k + 1]) ||
                !big_add(&l, &t, &h, 1))
                return 2;
            if (!big_mul(&t, &l, &m) || !big_add(&acc, &acc, &t, 1))
                return 2;
        }
        return acc.n == 0 ? 0 : (acc.neg ? -1 : 1);
    }
}

/* sign of (b - a) x (c - a): 1 if a, b, c turn left (counterclockwise) */
static int64_t orient2d(double ax, double ay, double bx, double by,
                        double cx, double cy)
{
    double detl = (ax - cx) * (by - cy);
    double detr = (ay - cy) * (bx - cx);
    double det = detl - detr;
    double bound = CCW_BOUND * (fabs(detl) + fabs(detr));
    dd l, r, d;

    if (det > bound || -det > bound)
        return sign_of(det);
    l = dd_mul(two_diff(ax, cx), two_diff(by, cy));
    r = dd_mul(two_diff(ay, cy), two_diff(bx, cx));
    d = dd_add(l, dd_neg(r));
    if (fabs(d.hi) <= DD_CCW_BOUND * (fabs(l.hi) + fabs(r.hi))) {
        /* too close for double-double: exactly, in integers */
        dd D[4] = {two_diff(ax, cx), two_diff(ay, cy), two_diff(bx, cx),
                   two_diff(by, cy)};
        int64_t e = orient2d_exact(D);
        return e == 2 ? 0 : e;
    }
    return dd_sign(d);
}

/* sign of the in-circle determinant for a, b, c counterclockwise:
   1 if p is inside the circle */
static int64_t incircle_ccw(double ax, double ay, double bx, double by,
                            double cx, double cy, double px, double py)
{
    double adx = ax - px, ady = ay - py;
    double bdx = bx - px, bdy = by - py;
    double cdx = cx - px, cdy = cy - py;
    double bdxcdy = bdx * cdy, cdxbdy = cdx * bdy;
    double cdxady = cdx * ady, adxcdy = adx * cdy;
    double adxbdy = adx * bdy, bdxady = bdx * ady;
    double alift = adx * adx + ady * ady;
    double blift = bdx * bdx + bdy * bdy;
    double clift = cdx * cdx + cdy * cdy;
    double det = alift * (bdxcdy - cdxbdy)
               + blift * (cdxady - adxcdy)
               + clift * (adxbdy - bdxady);
    double perm = (fabs(bdxcdy) + fabs(cdxbdy)) * alift
                + (fabs(cdxady) + fabs(adxcdy)) * blift
                + (fabs(adxbdy) + fabs(bdxady)) * clift;
    dd Adx, Ady, Bdx, Bdy, Cdx, Cdy, Al, Bl, Cl, m1, m2, m3, D;

    if (det > ICC_BOUND * perm || -det > ICC_BOUND * perm)
        return sign_of(det);

    Adx = two_diff(ax, px); Ady = two_diff(ay, py);
    Bdx = two_diff(bx, px); Bdy = two_diff(by, py);
    Cdx = two_diff(cx, px); Cdy = two_diff(cy, py);
    Al = dd_add(dd_mul(Adx, Adx), dd_mul(Ady, Ady));
    Bl = dd_add(dd_mul(Bdx, Bdx), dd_mul(Bdy, Bdy));
    Cl = dd_add(dd_mul(Cdx, Cdx), dd_mul(Cdy, Cdy));
    m1 = dd_add(dd_mul(Bdx, Cdy), dd_neg(dd_mul(Cdx, Bdy)));
    m2 = dd_add(dd_mul(Cdx, Ady), dd_neg(dd_mul(Adx, Cdy)));
    m3 = dd_add(dd_mul(Adx, Bdy), dd_neg(dd_mul(Bdx, Ady)));
    D = dd_add(dd_add(dd_mul(Al, m1), dd_mul(Bl, m2)), dd_mul(Cl, m3));
    if (fabs(D.hi) <= DD_ICC_BOUND * perm) {
        /* too close for double-double: exactly, in integers */
        dd Dv[6] = {Adx, Ady, Bdx, Bdy, Cdx, Cdy};
        int64_t e = incircle_exact(Dv);
        return e == 2 ? 0 : e;
    }
    return dd_sign(D);
}

void lg_orient2d_(const double *ax, const double *ay,
                  const double *bx, const double *by,
                  const double *cx, const double *cy, int64_t *isign)
{
    *isign = orient2d(*ax, *ay, *bx, *by, *cx, *cy);
}

void lg_incircle2d_(const double *ax, const double *ay,
                    const double *bx, const double *by,
                    const double *cx, const double *cy,
                    const double *px, const double *py, int64_t *isign)
{
    int64_t o = orient2d(*ax, *ay, *bx, *by, *cx, *cy);
    if (o == 0) {
        *isign = 0;
        return;
    }
    *isign = o * incircle_ccw(*ax, *ay, *bx, *by, *cx, *cy, *px, *py);
}
