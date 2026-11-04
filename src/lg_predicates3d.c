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
 *   call lg_insphere3d(ax,ay,az,...,ex,ey,ez,isign)
 *   call lg_insphere3d_or(ax,ay,az,...,ex,ey,ez,io,isign)
 *        io: the orientation of a, b, c, d if already known
 * isign = 1, 0 or -1. lg_insphere3d: 1 = e strictly inside.
 * lg_orient3d: the sign of det(a - d, b - d, c - d).
 */

#include <math.h>
#include <stdint.h>
#include <string.h>

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

/* the exact product of two doubles, as a double-double */
static dd two_prod(double a, double b)
{
    dd p;
    p.hi = a * b;
    p.lo = fma(a, b, -p.hi);
    return p;
}

/* a double-double times a double */
static dd dd_mul_d(dd a, double b)
{
    dd p = two_prod(a.hi, b);
    return dd_norm(p.hi, p.lo + a.lo * b);
}

/* Integer stage. If every coordinate difference is an integer multiple
   of a common power of two and at most 2**20 times it (points on a
   lattice with a spacing such as 1, 0.5, 10 or 25, at any offset), the
   in-sphere determinant is computed exactly in 64- and 128-bit
   integers, much faster than in double-double. Returns 2 when the
   differences do not qualify. */
static int ints20(const double *v, int n, int64_t *iv)
{
    double m = 0.0, sc;
    uint64_t bits;
    int i, e;
    for (i = 0; i < n; i++)
        if (fabs(v[i]) > m) m = fabs(v[i]);
    if (m == 0.0) {
        for (i = 0; i < n; i++) iv[i] = 0;
        return 1;
    }
    memcpy(&bits, &m, 8);
    e = (int) ((bits >> 52) & 0x7ff) - 1022;     /* m < 2**e */
    if (e < -900 || e > 900) return 0;
    bits = (uint64_t) (1023 + 20 - e) << 52;      /* sc = 2**(20 - e) */
    memcpy(&sc, &bits, 8);
    for (i = 0; i < n; i++) {
        double t = v[i] * sc;                    /* exact: |t| < 2**20 */
        int64_t k = (int64_t) t;
        if ((double) k != t) return 0;
        iv[i] = k;
    }
    return 1;
}

static int insphere_int(double d[12])
{
    int64_t q[12];
    int64_t ax, ay, az, bx, by, bz, cx, cy, cz, dx, dy, dz;
    int64_t ab, bc, cd, da, ac, bd, abc, bcd, cda, dab, al, bl, cl, dl;
    __int128 r;
    if (!ints20(d, 12, q)) return 2;
    ax = q[0]; ay = q[1]; az = q[2]; bx = q[3]; by = q[4]; bz = q[5];
    cx = q[6]; cy = q[7]; cz = q[8]; dx = q[9]; dy = q[10]; dz = q[11];
    /* |q| < 2**20: 2 x 2 minors < 2**41, 3 x 3 < 2**63, lifts < 2**42 */
    ab = ax * by - bx * ay; bc = bx * cy - cx * by;
    cd = cx * dy - dx * cy; da = dx * ay - ax * dy;
    ac = ax * cy - cx * ay; bd = bx * dy - dx * by;
    abc = az * bc - bz * ac + cz * ab;
    bcd = bz * cd - cz * bd + dz * bc;
    cda = cz * da + dz * ac + az * cd;
    dab = dz * ab + az * bd + bz * da;
    al = ax * ax + ay * ay + az * az; bl = bx * bx + by * by + bz * bz;
    cl = cx * cx + cy * cy + cz * cz; dl = dx * dx + dy * dy + dz * dz;
    r = ((__int128) dl * abc - (__int128) cl * dab)
      + ((__int128) bl * cda - (__int128) al * bcd);
    return (r > 0) - (r < 0);
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
static double isp_perm(const double *a, const double *b, const double *c,
                       const double *d, const double *e, double *pdet)
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

    *pdet = det;
    return perm;
}

static int64_t insphere_s(const double *a, const double *b, const double *c,
                          const double *d, const double *e, int skip);

static int64_t insphere(const double *a, const double *b, const double *c,
                        const double *d, const double *e)
{
    return insphere_s(a, b, c, d, e, 0);
}

/* skip = 1: the caller's own quick test was too close to call, so the
   double-precision stage is skipped (it would almost always fail too) */
static int64_t insphere_s(const double *a, const double *b, const double *c,
                          const double *d, const double *e, int skip)
{
    dd Ax, Ay, Az, Bx, By, Bz, Cx, Cy, Cz, Dx, Dy, Dz;
    dd AB, BC, CD, DA, AC, BD, ABC, BCD, CDA, DAB, AL, BL, CL, DL, R;
    double det = 0.0, perm = 0.0;
    int haveperm = 0;

    if (!skip) {
        perm = isp_perm(a, b, c, d, e, &det);
        haveperm = 1;
        if (det > ISP_BOUND * perm || -det > ISP_BOUND * perm)
            return sign_of(det);
    }

    Ax = two_diff(a[0], e[0]); Ay = two_diff(a[1], e[1]);
    Az = two_diff(a[2], e[2]);
    Bx = two_diff(b[0], e[0]); By = two_diff(b[1], e[1]);
    Bz = two_diff(b[2], e[2]);
    Cx = two_diff(c[0], e[0]); Cy = two_diff(c[1], e[1]);
    Cz = two_diff(c[2], e[2]);
    Dx = two_diff(d[0], e[0]); Dy = two_diff(d[1], e[1]);
    Dz = two_diff(d[2], e[2]);
    if (Ax.lo == 0.0 && Ay.lo == 0.0 && Az.lo == 0.0 && Bx.lo == 0.0 &&
        By.lo == 0.0 && Bz.lo == 0.0 && Cx.lo == 0.0 && Cy.lo == 0.0 &&
        Cz.lo == 0.0 && Dx.lo == 0.0 && Dy.lo == 0.0 && Dz.lo == 0.0) {
        double dv[12] = {Ax.hi, Ay.hi, Az.hi, Bx.hi, By.hi, Bz.hi,
                         Cx.hi, Cy.hi, Cz.hi, Dx.hi, Dy.hi, Dz.hi};
        int ri = insphere_int(dv);
        if (ri != 2) return ri;
        /* every difference is exact in double (the usual case: points
           close together, a lattice for one): products of two
           differences are exact with two_prod, and the 3 x 3 minors
           and lifts need only a double-double times a double */
        double ax = Ax.hi, ay = Ay.hi, az = Az.hi, bx = Bx.hi, by = By.hi,
               bz = Bz.hi, cx = Cx.hi, cy = Cy.hi, cz = Cz.hi, dx = Dx.hi,
               dy = Dy.hi, dz = Dz.hi;
        AB = dd_sub(two_prod(ax, by), two_prod(bx, ay));
        BC = dd_sub(two_prod(bx, cy), two_prod(cx, by));
        CD = dd_sub(two_prod(cx, dy), two_prod(dx, cy));
        DA = dd_sub(two_prod(dx, ay), two_prod(ax, dy));
        AC = dd_sub(two_prod(ax, cy), two_prod(cx, ay));
        BD = dd_sub(two_prod(bx, dy), two_prod(dx, by));
        ABC = dd_add(dd_sub(dd_mul_d(BC, az), dd_mul_d(AC, bz)),
                     dd_mul_d(AB, cz));
        BCD = dd_add(dd_sub(dd_mul_d(CD, bz), dd_mul_d(BD, cz)),
                     dd_mul_d(BC, dz));
        CDA = dd_add(dd_add(dd_mul_d(DA, cz), dd_mul_d(AC, dz)),
                     dd_mul_d(CD, az));
        DAB = dd_add(dd_add(dd_mul_d(AB, dz), dd_mul_d(BD, az)),
                     dd_mul_d(DA, bz));
        AL = dd_add(dd_add(two_prod(ax, ax), two_prod(ay, ay)),
                    two_prod(az, az));
        BL = dd_add(dd_add(two_prod(bx, bx), two_prod(by, by)),
                    two_prod(bz, bz));
        CL = dd_add(dd_add(two_prod(cx, cx), two_prod(cy, cy)),
                    two_prod(cz, cz));
        DL = dd_add(dd_add(two_prod(dx, dx), two_prod(dy, dy)),
                    two_prod(dz, dz));
    } else {
    AB = dd_sub(dd_mul(Ax, By), dd_mul(Bx, Ay));
    BC = dd_sub(dd_mul(Bx, Cy), dd_mul(Cx, By));
    CD = dd_sub(dd_mul(Cx, Dy), dd_mul(Dx, Cy));
    DA = dd_sub(dd_mul(Dx, Ay), dd_mul(Ax, Dy));
    AC = dd_sub(dd_mul(Ax, Cy), dd_mul(Cx, Ay));
    BD = dd_sub(dd_mul(Bx, Dy), dd_mul(Dx, By));
    AL = dd_add(dd_add(dd_mul(Ax, Ax), dd_mul(Ay, Ay)), dd_mul(Az, Az));
    BL = dd_add(dd_add(dd_mul(Bx, Bx), dd_mul(By, By)), dd_mul(Bz, Bz));
    CL = dd_add(dd_add(dd_mul(Cx, Cx), dd_mul(Cy, Cy)), dd_mul(Cz, Cz));
    DL = dd_add(dd_add(dd_mul(Dx, Dx), dd_mul(Dy, Dy)), dd_mul(Dz, Dz));
    ABC = dd_add(dd_sub(dd_mul(Az, BC), dd_mul(Bz, AC)), dd_mul(Cz, AB));
    BCD = dd_add(dd_sub(dd_mul(Bz, CD), dd_mul(Cz, BD)), dd_mul(Dz, BC));
    CDA = dd_add(dd_add(dd_mul(Cz, DA), dd_mul(Dz, AC)), dd_mul(Az, CD));
    DAB = dd_add(dd_add(dd_mul(Dz, AB), dd_mul(Az, BD)), dd_mul(Bz, DA));
    }
    R = dd_add(dd_sub(dd_mul(DL, ABC), dd_mul(CL, DAB)),
               dd_sub(dd_mul(BL, CDA), dd_mul(AL, BCD)));
    if (!haveperm) perm = isp_perm(a, b, c, d, e, &det);
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

/* lg_insphere3d_or_: the same as lg_insphere3d_ for a tet whose
   orientation is known: *io is orient3d(a, b, c, d) (1 or -1), so it
   is not computed again; *io = 2 or -2: the same, and the caller's own
   quick test was too close to call, so the double stage is skipped */
void lg_insphere3d_or_(const double *ax, const double *ay, const double *az,
                       const double *bx, const double *by, const double *bz,
                       const double *cx, const double *cy, const double *cz,
                       const double *dx, const double *dy, const double *dz,
                       const double *ex, const double *ey, const double *ez,
                       const int64_t *io, int64_t *isign)
{
    double a[3] = {*ax, *ay, *az}, b[3] = {*bx, *by, *bz};
    double c[3] = {*cx, *cy, *cz}, d[3] = {*dx, *dy, *dz};
    double e[3] = {*ex, *ey, *ez};
    *isign = (*io > 0 ? 1 : -1) *
             insphere_s(a, b, c, d, e, *io > 1 || *io < -1);
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
