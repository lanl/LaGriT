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
 * the test is done again in double-double arithmetic (about 106 bits);
 * only if that result is also within its error bound is the answer 0
 * ("on the line" or "on the circle"). The coordinate differences are
 * exact in double-double, so this is wrong only for points that are
 * cocircular to about 1 part in 1e30.
 *
 * This is not Shewchuk's predicates.c (which is exact in every case);
 * it uses only his error bounds. Written for LaGriT, Nov 2026.
 *
 * Called from Fortran (8-byte integers):
 *   call lg_orient2d(ax,ay,bx,by,cx,cy,isign)
 *   call lg_incircle2d(ax,ay,bx,by,cx,cy,px,py,isign)
 * isign = 1, 0 or -1.
 */

#include <math.h>
#include <stdint.h>

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
    if (fabs(d.hi) <= DD_CCW_BOUND * (fabs(l.hi) + fabs(r.hi)))
        return 0;
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
    if (fabs(D.hi) <= DD_ICC_BOUND * perm)
        return 0;
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
