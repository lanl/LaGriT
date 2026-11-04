/*
 * lg_predicates3d.c -- orientation and in-sphere tests for 3D connect
 *
 *   lg_orient3d_    which side of the plane through a, b, c is d on?
 *   lg_insphere3d_  is e inside, on, or outside the sphere through
 *                   a, b, c, d (whatever the order of a, b, c, d)?
 *
 * As in lg_predicates2d.c: each test is first done in double precision,
 * translated to one of the points; if the result is larger than a bound
 * on its rounding error (the static bounds of J. R. Shewchuk, "Adaptive
 * precision floating-point arithmetic and fast robust geometric
 * predicates", Discrete & Computational Geometry 18, 1997), its sign is
 * certain. Otherwise it is done again in double-double arithmetic
 * (about 106 bits, coordinate differences exact); only if that result
 * is also within its error bound is the answer 0.
 *
 * Only Shewchuk's error bounds are used, not his predicates.c.
 * Written for LaGriT, Nov 2026.
 *
 * Called from Fortran (8-byte integers):
 *   call lg_orient3d(ax,ay,az,bx,by,bz,cx,cy,cz,dx,dy,dz,isign)
 *   call lg_insphere3d(a(3),b(3),c(3),d(3),e(3),isign)
 *        (each point as 3 consecutive reals: x, y, z)
 * isign = 1, 0 or -1. lg_insphere3d: 1 = e strictly inside.
 * lg_orient3d: the sign of det(a - d, b - d, c - d).
 */

#include <math.h>
#include <stdint.h>

#define LG_EPS 1.1102230246251565e-16
#define O3D_BOUND ((7.0 + 56.0 * LG_EPS) * LG_EPS)
#define ISP_BOUND ((16.0 + 224.0 * LG_EPS) * LG_EPS)
/* double-double stage: about 2**-104 relative per operation; generous
   multiples cover the operations of each test */
#define DD_EPS 4.930380657631324e-32
#define DD_O3D_BOUND (64.0 * DD_EPS)
#define DD_ISP_BOUND (4096.0 * DD_EPS)

typedef struct { double hi, lo; } dd;

static dd two_sum(double a, double b)
{
    dd r;
    double bb;
    r.hi = a + b;
    bb = r.hi - a;
    r.lo = (a - (r.hi - bb)) + (b - bb);
    return r;
}

static dd two_diff(double a, double b) { return two_sum(a, -b); }

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

static dd dd_sub(dd a, dd b)
{
    b.hi = -b.hi;
    b.lo = -b.lo;
    return dd_add(a, b);
}

static dd dd_mul(dd a, dd b)
{
    dd p;
    p.hi = a.hi * b.hi;
    p.lo = fma(a.hi, b.hi, -p.hi);
    return dd_norm(p.hi, p.lo + (a.hi * b.lo + a.lo * b.hi));
}

static int64_t sign_of(double v) { return (v > 0.0) - (v < 0.0); }

static int64_t dd_sign(dd v)
{
    return v.hi != 0.0 ? sign_of(v.hi) : sign_of(v.lo);
}

/* sign of det(a - d, b - d, c - d) */
static int64_t orient3d(const double *a, const double *b, const double *c,
                        const double *d)
{
    double adx = a[0] - d[0], bdx = b[0] - d[0], cdx = c[0] - d[0];
    double ady = a[1] - d[1], bdy = b[1] - d[1], cdy = c[1] - d[1];
    double adz = a[2] - d[2], bdz = b[2] - d[2], cdz = c[2] - d[2];
    double bdxcdy = bdx * cdy, cdxbdy = cdx * bdy;
    double cdxady = cdx * ady, adxcdy = adx * cdy;
    double adxbdy = adx * bdy, bdxady = bdx * ady;
    double det = adz * (bdxcdy - cdxbdy) + bdz * (cdxady - adxcdy)
               + cdz * (adxbdy - bdxady);
    double perm = (fabs(bdxcdy) + fabs(cdxbdy)) * fabs(adz)
                + (fabs(cdxady) + fabs(adxcdy)) * fabs(bdz)
                + (fabs(adxbdy) + fabs(bdxady)) * fabs(cdz);
    dd Ax, Ay, Az, Bx, By, Bz, Cx, Cy, Cz, D;

    if (det > O3D_BOUND * perm || -det > O3D_BOUND * perm)
        return sign_of(det);
    Ax = two_diff(a[0], d[0]); Ay = two_diff(a[1], d[1]);
    Az = two_diff(a[2], d[2]);
    Bx = two_diff(b[0], d[0]); By = two_diff(b[1], d[1]);
    Bz = two_diff(b[2], d[2]);
    Cx = two_diff(c[0], d[0]); Cy = two_diff(c[1], d[1]);
    Cz = two_diff(c[2], d[2]);
    D = dd_add(dd_add(dd_mul(Az, dd_sub(dd_mul(Bx, Cy), dd_mul(Cx, By))),
                      dd_mul(Bz, dd_sub(dd_mul(Cx, Ay), dd_mul(Ax, Cy)))),
               dd_mul(Cz, dd_sub(dd_mul(Ax, By), dd_mul(Bx, Ay))));
    if (fabs(D.hi) <= DD_O3D_BOUND * perm)
        return 0;
    return dd_sign(D);
}

/* Shewchuk's insphere: positive when e is inside the sphere through
   a, b, c, d and orient3d(a, b, c, d) is positive */
static int64_t insphere(const double *a, const double *b, const double *c,
                        const double *d, const double *e)
{
    double aex = a[0] - e[0], bex = b[0] - e[0], cex = c[0] - e[0],
           dex = d[0] - e[0];
    double aey = a[1] - e[1], bey = b[1] - e[1], cey = c[1] - e[1],
           dey = d[1] - e[1];
    double aez = a[2] - e[2], bez = b[2] - e[2], cez = c[2] - e[2],
           dez = d[2] - e[2];
    double aexbey = aex * bey, bexaey = bex * aey, ab = aexbey - bexaey;
    double bexcey = bex * cey, cexbey = cex * bey, bc = bexcey - cexbey;
    double cexdey = cex * dey, dexcey = dex * cey, cd = cexdey - dexcey;
    double dexaey = dex * aey, aexdey = aex * dey, da = dexaey - aexdey;
    double aexcey = aex * cey, cexaey = cex * aey, ac = aexcey - cexaey;
    double bexdey = bex * dey, dexbey = dex * bey, bd = bexdey - dexbey;
    double abc = aez * bc - bez * ac + cez * ab;
    double bcd = bez * cd - cez * bd + dez * bc;
    double cda = cez * da + dez * ac + aez * cd;
    double dab = dez * ab + aez * bd + bez * da;
    double alift = aex * aex + aey * aey + aez * aez;
    double blift = bex * bex + bey * bey + bez * bez;
    double clift = cex * cex + cey * cey + cez * cez;
    double dlift = dex * dex + dey * dey + dez * dez;
    double det = (dlift * abc - clift * dab) + (blift * cda - alift * bcd);
    double aezplus = fabs(aez), bezplus = fabs(bez), cezplus = fabs(cez),
           dezplus = fabs(dez);
    double aexbeyplus = fabs(aexbey) + fabs(bexaey);
    double bexceyplus = fabs(bexcey) + fabs(cexbey);
    double cexdeyplus = fabs(cexdey) + fabs(dexcey);
    double dexaeyplus = fabs(dexaey) + fabs(aexdey);
    double aexceyplus = fabs(aexcey) + fabs(cexaey);
    double bexdeyplus = fabs(bexdey) + fabs(dexbey);
    double perm = ((cexdeyplus * bezplus + bexdeyplus * cezplus
                    + bexceyplus * dezplus) * alift)
                + ((dexaeyplus * cezplus + aexceyplus * dezplus
                    + cexdeyplus * aezplus) * blift)
                + ((aexbeyplus * dezplus + bexdeyplus * aezplus
                    + dexaeyplus * bezplus) * clift)
                + ((bexceyplus * aezplus + aexceyplus * bezplus
                    + aexbeyplus * cezplus) * dlift);
    dd Ax, Ay, Az, Bx, By, Bz, Cx, Cy, Cz, Dx, Dy, Dz;
    dd AB, BC, CD, DA, AC, BD, ABC, BCD, CDA, DAB, AL, BL, CL, DL, R;

    if (det > ISP_BOUND * perm || -det > ISP_BOUND * perm)
        return sign_of(det);

    Ax = two_diff(a[0], e[0]); Ay = two_diff(a[1], e[1]);
    Az = two_diff(a[2], e[2]);
    Bx = two_diff(b[0], e[0]); By = two_diff(b[1], e[1]);
    Bz = two_diff(b[2], e[2]);
    Cx = two_diff(c[0], e[0]); Cy = two_diff(c[1], e[1]);
    Cz = two_diff(c[2], e[2]);
    Dx = two_diff(d[0], e[0]); Dy = two_diff(d[1], e[1]);
    Dz = two_diff(d[2], e[2]);
    AB = dd_sub(dd_mul(Ax, By), dd_mul(Bx, Ay));
    BC = dd_sub(dd_mul(Bx, Cy), dd_mul(Cx, By));
    CD = dd_sub(dd_mul(Cx, Dy), dd_mul(Dx, Cy));
    DA = dd_sub(dd_mul(Dx, Ay), dd_mul(Ax, Dy));
    AC = dd_sub(dd_mul(Ax, Cy), dd_mul(Cx, Ay));
    BD = dd_sub(dd_mul(Bx, Dy), dd_mul(Dx, By));
    ABC = dd_add(dd_sub(dd_mul(Az, BC), dd_mul(Bz, AC)), dd_mul(Cz, AB));
    BCD = dd_add(dd_sub(dd_mul(Bz, CD), dd_mul(Cz, BD)), dd_mul(Dz, BC));
    CDA = dd_add(dd_add(dd_mul(Cz, DA), dd_mul(Dz, AC)), dd_mul(Az, CD));
    DAB = dd_add(dd_add(dd_mul(Dz, AB), dd_mul(Az, BD)), dd_mul(Bz, DA));
    AL = dd_add(dd_add(dd_mul(Ax, Ax), dd_mul(Ay, Ay)), dd_mul(Az, Az));
    BL = dd_add(dd_add(dd_mul(Bx, Bx), dd_mul(By, By)), dd_mul(Bz, Bz));
    CL = dd_add(dd_add(dd_mul(Cx, Cx), dd_mul(Cy, Cy)), dd_mul(Cz, Cz));
    DL = dd_add(dd_add(dd_mul(Dx, Dx), dd_mul(Dy, Dy)), dd_mul(Dz, Dz));
    R = dd_add(dd_sub(dd_mul(DL, ABC), dd_mul(CL, DAB)),
               dd_sub(dd_mul(BL, CDA), dd_mul(AL, BCD)));
    if (fabs(R.hi) <= DD_ISP_BOUND * perm)
        return 0;
    return dd_sign(R);
}

void lg_orient3d_(const double *ax, const double *ay, const double *az,
                  const double *bx, const double *by, const double *bz,
                  const double *cx, const double *cy, const double *cz,
                  const double *dx, const double *dy, const double *dz,
                  int64_t *isign)
{
    double a[3] = {*ax, *ay, *az}, b[3] = {*bx, *by, *bz};
    double c[3] = {*cx, *cy, *cz}, d[3] = {*dx, *dy, *dz};
    *isign = orient3d(a, b, c, d);
}

/* 1 when e is strictly inside the sphere through a, b, c, d, whatever
   their order; 0 on it, or when a, b, c, d are coplanar */
void lg_insphere3d_(const double *ax, const double *ay, const double *az,
                    const double *bx, const double *by, const double *bz,
                    const double *cx, const double *cy, const double *cz,
                    const double *dx, const double *dy, const double *dz,
                    const double *ex, const double *ey, const double *ez,
                    int64_t *isign)
{
    double a[3] = {*ax, *ay, *az}, b[3] = {*bx, *by, *bz};
    double c[3] = {*cx, *cy, *cz}, d[3] = {*dx, *dy, *dz};
    double e[3] = {*ex, *ey, *ez};
    int64_t o = orient3d(a, b, c, d);
    if (o == 0) {
        *isign = 0;
        return;
    }
    *isign = o * insphere(a, b, c, d, e);
}
