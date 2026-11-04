/*
 * lg_connect3d_hull.c -- finish 3D connect: fill to the convex hull
 *
 * connect (3D) inserts the points into a large enclosing tetrahedron
 * and then deletes every tet touching its vertices. A thin tet along
 * the convex hull has a circumsphere large enough to reach those
 * vertices, so it is deleted with them, and the mesh misses parts of
 * the hull (1% of its volume for random points, more for long thin
 * domains). This puts them back.
 *
 * The tets kept are Delaunay tets of the points (their spheres hold no
 * point), so the missing ones are the Delaunay tets of the points in
 * the pockets between the mesh and its convex hull. They are found by
 * gift wrapping: for every boundary face F, the Delaunay tet on its
 * outer side is F plus the vertex p, among the points strictly outside
 * F's plane, whose sphere through F holds none of the others (a scan
 * with the exact in-sphere test, keeping the better point). Only
 * boundary vertices can be such a p: every vertex of a missing tet is
 * on the mesh boundary. If no point is outside F's plane, F is a facet
 * of the convex hull and stays on the boundary. Each new tet is glued
 * to the boundary faces it shares; its other faces join the boundary
 * and are wrapped in turn. All decisions use the exact tests of
 * lg_predicates3d.c.
 *
 * A tet is not added (its pocket is left open) when
 *   - it is flatter than FLAT (its height over F less than 1e-6 of its
 *     longest edge): that is round-off in points meant to lie in a
 *     plane, and the sliver would only harm the mesh;
 *   - the choice of p is a tie (another point exactly on the same
 *     sphere): either choice is Delaunay, but it might not match the
 *     choices already made in the mesh;
 *   - its vertices that are not interface points have different
 *     materials (imt1): the interface pass of connect has already run.
 *
 * Boundary vertices are kept in a coarse grid (each cell with the box
 * of its vertices), so for each face only the cells that reach past
 * its plane are searched.
 *
 * Called from Fortran (8-byte integers):
 *   call lg_connect3d_hull(npoints,ntet,maxtet,itet,jtet,xic,yic,zic,
 *        imt1,isint,mbndry,nadded,nskip,ierr)
 *   ntet         in: tets; out: with the added ones
 *   maxtet       room in itet and jtet (tets)
 *   itet(4,*)    vertex numbers (1-based), positively oriented
 *   jtet(4,*)    in: connect's, 4*(jt-1)+jf across face jf (face f is
 *                opposite vertex f), mbndry (or 0) on the boundary;
 *                out: the same for every tet
 *   imt1(*), isint(*)   material and interface flag (1) per vertex
 *   nadded, nskip   out: tets added; pockets left (flat, tie or
 *                material)
 *   ierr         out: 0 ok; 1 an edge or face in too many tets
 *                (nothing added); 4 no memory; 5 out of room (partly
 *                filled, valid)
 *
 * Written for LaGriT, Nov 2026.
 */

#include <math.h>
#include <stdint.h>
#include <stdlib.h>
#include <string.h>

void lg_orient3d_(const double *, const double *, const double *,
                  const double *, const double *, const double *,
                  const double *, const double *, const double *,
                  const double *, const double *, const double *,
                  int64_t *);
void lg_insphere3d_(const double *, const double *, const double *,
                    const double *, const double *, const double *,
                    const double *, const double *, const double *,
                    const double *, const double *, const double *,
                    const double *, const double *, const double *,
                    int64_t *);

#define FLAT 1.0e-6

static const double *X, *Y, *Z;

/* Shewchuk's orient3d sign: det(a - d, b - d, c - d) */
static int64_t orient(int64_t a, int64_t b, int64_t c, int64_t d)
{
    int64_t s;
    lg_orient3d_(&X[a], &Y[a], &Z[a], &X[b], &Y[b], &Z[b], &X[c], &Y[c],
                 &Z[c], &X[d], &Y[d], &Z[d], &s);
    return s;
}

/* 1 when e is strictly inside the sphere through a, b, c, d */
static int64_t insph(int64_t a, int64_t b, int64_t c, int64_t d, int64_t e)
{
    int64_t s;
    lg_insphere3d_(&X[a], &Y[a], &Z[a], &X[b], &Y[b], &Z[b], &X[c], &Y[c],
                   &Z[c], &X[d], &Y[d], &Z[d], &X[e], &Y[e], &Z[e], &s);
    return s;
}

/* ---- faces: hash of sorted vertex triples -> 4t+f ---- */
typedef struct { int64_t a, b, c, tf; } fkey;

static int64_t hsize;
static fkey *htab;

static uint64_t hmix(int64_t a, int64_t b, int64_t c)
{
    uint64_t h = (uint64_t) a * 0x9e3779b97f4a7c15ULL;
    h ^= (uint64_t) b * 0xbf58476d1ce4e5b9ULL + (h << 6) + (h >> 2);
    h ^= (uint64_t) c * 0x94d049bb133111ebULL + (h << 6) + (h >> 2);
    return h;
}

static void sort3(int64_t *a, int64_t *b, int64_t *c)
{
    int64_t t;
    if (*a > *b) { t = *a; *a = *b; *b = t; }
    if (*b > *c) { t = *b; *b = *c; *c = t; }
    if (*a > *b) { t = *a; *a = *b; *b = t; }
}

/* find the slot of face (a,b,c); create it (tf = -1) when absent */
static fkey *hslot(int64_t a, int64_t b, int64_t c)
{
    uint64_t i;
    sort3(&a, &b, &c);
    i = hmix(a, b, c) & (uint64_t) (hsize - 1);
    for (;;) {
        fkey *k = &htab[i];
        if (k->tf == -2) {          /* empty */
            k->a = a; k->b = b; k->c = c; k->tf = -1;
            return k;
        }
        if (k->a == a && k->b == b && k->c == c) return k;
        i = (i + 1) & (uint64_t) (hsize - 1);
    }
}

static int cmp_bedge(const void *p, const void *q)
{
    const int64_t *e = p, *f = q;           /* a, b first */
    if (e[0] != f[0]) return e[0] < f[0] ? -1 : 1;
    if (e[1] != f[1]) return e[1] < f[1] ? -1 : 1;
    return 0;
}

/* face f of tet t is opposite vertex f */
#define V(t, k) (it[4 * (t) + (k)] - 1)

/* ---- the grid of boundary vertices ---- */
static int64_t gG, gncell, *gcstart, *gcellv;
static double *gcbox, glo[3], gcs[3];

/* does cell k reach past the plane n.x = off (with slack)? */
static int cell_outside(int64_t k, const double *n, double off, double slack)
{
    const double *bx = &gcbox[6 * k];
    double sup = (n[0] > 0 ? n[0] * bx[3] : n[0] * bx[0])
               + (n[1] > 0 ? n[1] * bx[4] : n[1] * bx[1])
               + (n[2] > 0 ? n[2] * bx[5] : n[2] * bx[2]);
    return sup - off >= -slack;
}

/* the circumsphere of a, b, c, p in floating point, with a radius that
   covers its own rounding (from the spread of the four distances) */
static int ball(int64_t a, int64_t b, int64_t c, int64_t p, double *cen,
                double *rad)
{
    double A[3] = {X[b] - X[a], Y[b] - Y[a], Z[b] - Z[a]};
    double B[3] = {X[c] - X[a], Y[c] - Y[a], Z[c] - Z[a]};
    double C[3] = {X[p] - X[a], Y[p] - Y[a], Z[p] - Z[a]};
    double la = A[0]*A[0]+A[1]*A[1]+A[2]*A[2], lb = B[0]*B[0]+B[1]*B[1]+B[2]*B[2],
           lc = C[0]*C[0]+C[1]*C[1]+C[2]*C[2];
    double bxc[3] = {B[1]*C[2]-B[2]*C[1], B[2]*C[0]-B[0]*C[2], B[0]*C[1]-B[1]*C[0]};
    double cxa[3] = {C[1]*A[2]-C[2]*A[1], C[2]*A[0]-C[0]*A[2], C[0]*A[1]-C[1]*A[0]};
    double axb[3] = {A[1]*B[2]-A[2]*B[1], A[2]*B[0]-A[0]*B[2], A[0]*B[1]-A[1]*B[0]};
    double det = 2.0 * (A[0]*bxc[0] + A[1]*bxc[1] + A[2]*bxc[2]);
    double r, rmin, rmax, dv;
    int64_t vv[4] = {a, b, c, p};
    int k;
    if (det == 0.0) return 0;
    for (k = 0; k < 3; k++)
        cen[k] = (la * bxc[k] + lb * cxa[k] + lc * axb[k]) / det;
    cen[0] += X[a]; cen[1] += Y[a]; cen[2] += Z[a];
    rmin = INFINITY; rmax = 0.0;
    for (k = 0; k < 4; k++) {
        double dx = X[vv[k]] - cen[0], dy = Y[vv[k]] - cen[1], dz = Z[vv[k]] - cen[2];
        r = sqrt(dx * dx + dy * dy + dz * dz);
        if (r < rmin) rmin = r;
        if (r > rmax) rmax = r;
    }
    dv = rmax - rmin;
    *rad = rmax + 4.0 * dv + 1e-9 * (rmax + fabs(cen[0]) + fabs(cen[1]) + fabs(cen[2]));
    if (!isfinite(*rad)) return 0;
    return 1;
}

/* the apex of the Delaunay tet outside face (a, b, c): the boundary
   vertex strictly outside whose sphere through a, b, c holds no other;
   -1 when no vertex is outside (a hull facet). tie: another vertex is
   exactly on that sphere. n: the outward normal (approximate). */
static int64_t find_apex(int64_t a, int64_t b, int64_t c, const double *n,
                         int64_t *tie)
{
    double off = n[0] * X[a] + n[1] * Y[a] + n[2] * Z[a];
    double nn = fabs(n[0]) + fabs(n[1]) + fabs(n[2]);
    double scale = fabs(X[a]) + fabs(Y[a]) + fabs(Z[a]) + gcs[0] * gG + gcs[1] * gG + gcs[2] * gG;
    double slack = 1e-9 * nn * scale, cen[3], rad;
    int64_t p = -1, k, j, r, c0[3], have;
    double g[3] = {(X[a] + X[b] + X[c]) / 3, (Y[a] + Y[b] + Y[c]) / 3, (Z[a] + Z[b] + Z[c]) / 3};
    *tie = 0;
    for (k = 0; k < 3; k++) {
        c0[k] = (int64_t) ((g[k] - glo[k]) / gcs[k]);
        if (c0[k] < 0) c0[k] = 0;
        if (c0[k] >= gG) c0[k] = gG - 1;
    }
    /* 1. a first apex: rings of cells around the face, nearest first */
    for (r = 0; r < gG && p < 0; r++) {
        int64_t ix, iy, iz;
        for (iz = c0[2] - r; iz <= c0[2] + r; iz++)
            for (iy = c0[1] - r; iy <= c0[1] + r; iy++)
                for (ix = c0[0] - r; ix <= c0[0] + r; ix++) {
                    int64_t cell;
                    if (ix < 0 || iy < 0 || iz < 0 || ix >= gG || iy >= gG || iz >= gG) continue;
                    if (labs(ix - c0[0]) != r && labs(iy - c0[1]) != r && labs(iz - c0[2]) != r) continue;
                    cell = (iz * gG + iy) * gG + ix;
                    if (gcstart[cell] == gcstart[cell + 1]) continue;
                    if (!cell_outside(cell, n, off, slack)) continue;
                    for (j = gcstart[cell]; j < gcstart[cell + 1]; j++) {
                        int64_t q = gcellv[j];
                        if (q == a || q == b || q == c) continue;
                        if (orient(a, b, c, q) <= 0) continue;
                        if (p < 0 || insph(a, b, c, p, q) > 0) p = q;
                    }
                }
    }
    if (p < 0) return -1;
    /* 2. improve within the ball until no vertex is inside it; then a
       last pass for ties */
    for (have = 0; have < 2; ) {
        int64_t lo3[3], hi3[3], ix, iy, iz, changed = 0;
        if (!ball(a, b, c, p, cen, &rad)) {
            /* flat to round-off: search everything */
            for (k = 0; k < 3; k++) { lo3[k] = 0; hi3[k] = gG - 1; }
            rad = INFINITY;
        } else {
            for (k = 0; k < 3; k++) {
                double l = (cen[k] - rad - glo[k]) / gcs[k], h = (cen[k] + rad - glo[k]) / gcs[k];
                lo3[k] = l < 0 ? 0 : (l >= gG ? gG - 1 : (int64_t) l);
                hi3[k] = h < 0 ? 0 : (h >= gG ? gG - 1 : (int64_t) h);
            }
        }
        for (iz = lo3[2]; iz <= hi3[2]; iz++)
            for (iy = lo3[1]; iy <= hi3[1]; iy++)
                for (ix = lo3[0]; ix <= hi3[0]; ix++) {
                    int64_t cell = (iz * gG + iy) * gG + ix;
                    if (gcstart[cell] == gcstart[cell + 1]) continue;
                    if (!cell_outside(cell, n, off, slack)) continue;
                    for (j = gcstart[cell]; j < gcstart[cell + 1]; j++) {
                        int64_t q = gcellv[j], s;
                        double dx, dy, dz;
                        if (q == a || q == b || q == c || q == p) continue;
                        dx = X[q] - cen[0]; dy = Y[q] - cen[1]; dz = Z[q] - cen[2];
                        if (rad < INFINITY && dx * dx + dy * dy + dz * dz > rad * rad) continue;
                        if (orient(a, b, c, q) <= 0) continue;
                        s = insph(a, b, c, p, q);
                        if (s > 0) { p = q; changed = 1; }
                        else if (s == 0 && have == 1) *tie = 1;
                    }
                }
        if (changed) have = 0;      /* a better apex: a new ball */
        else have++;                /* 0 -> last pass for ties -> done */
    }
    return p;
}

void lg_connect3d_hull_(int64_t *npoints, int64_t *ntet, int64_t *maxtet,
                        int64_t *it, int64_t *jt, double *xic, double *yic,
                        double *zic, int64_t *imt1, int64_t *isint,
                        int64_t *mbndry, int64_t *nadded, int64_t *nskip,
                        int64_t *ierr)
{
    int64_t np = *npoints, nt = *ntet, mt = *maxtet;
    int64_t *nb = NULL, *queue = NULL, *bv = NULL, *cellv = NULL;
    int64_t *cstart = NULL;
    double *cbox = NULL;
    char *isb = NULL, *inq = NULL;
    int64_t t, f, i, nq = 0, qcap, nbv = 0, G, ncell;
    double lo[3], hi[3], cs[3];

    *nadded = 0;
    *nskip = 0;
    *ierr = 0;
    if (nt < 1) return;
    X = xic; Y = yic; Z = zic;

    nb = malloc(sizeof(int64_t) * 4 * mt);
    isb = calloc(np, 1);
    if (!nb || !isb) { *ierr = 4; goto done; }
    /* neighbours from connect's jtet (4*(t-1)+f, 0 on the boundary),
       checked for consistency */
    for (i = 0; i < 4 * nt; i++) {
        int64_t v = jt[i];
        if (v <= 0 || v >= *mbndry) nb[i] = -1;
        else if (v > 4 * nt) { *ierr = 1; goto done; }
        else nb[i] = v - 1;
    }
    for (i = 0; i < 4 * nt; i++)
        if (nb[i] >= 0 && nb[nb[i]] != i) { *ierr = 1; goto done; }
    /* the face hash holds only boundary faces (and those to come) */
    {
        int64_t nbf = 0;
        for (i = 0; i < 4 * nt; i++) if (nb[i] < 0) nbf++;
        for (hsize = 1; hsize < 4 * (nbf + 4 * (mt - nt)) + 64; hsize <<= 1) ;
    }
    htab = malloc(sizeof(fkey) * hsize);
    if (!htab) { *ierr = 4; goto done; }
    for (i = 0; i < hsize; i++) htab[i].tf = -2;
    for (t = 0; t < nt; t++)
        for (f = 0; f < 4; f++) {
            int64_t v[3], k = 0, j;
            fkey *sl;
            if (nb[4 * t + f] >= 0) continue;
            for (j = 0; j < 4; j++) if (j != f) v[k++] = V(t, j);
            sl = hslot(v[0], v[1], v[2]);
            if (sl->tf != -1) { *ierr = 1; goto done; }  /* twice */
            sl->tf = 4 * t + f;
        }

    /* boundary faces and vertices */
    qcap = 4 * mt + 16;
    queue = malloc(sizeof(int64_t) * qcap);
    inq = calloc(4 * mt, 1);
    if (!queue || !inq) { *ierr = 4; goto done; }
    {
        /* boundary edges (sorted): a pocket shows as a reflex edge, where
           the far vertex of one boundary face is strictly outside the
           other; only the faces at such edges seed the wrapping (a
           boundary that is locally convex at every edge is convex) */
        typedef struct { int64_t a, b, tf, w; } bedge;
        bedge *E;
        int64_t ne = 0, nbf = 0, j;
        for (t = 0; t < nt; t++)
            for (f = 0; f < 4; f++)
                if (nb[4 * t + f] < 0) nbf++;
        E = malloc(sizeof(bedge) * 3 * (nbf + 1));
        if (!E) { *ierr = 4; goto done; }
        for (t = 0; t < nt; t++)
            for (f = 0; f < 4; f++) {
                int64_t v[3], kk = 0, e;
                if (nb[4 * t + f] >= 0) continue;
                for (j = 0; j < 4; j++) if (j != f) v[kk++] = V(t, j);
                for (j = 0; j < 3; j++) isb[v[j]] = 1;
                for (e = 0; e < 3; e++) {
                    int64_t x = v[e], y = v[(e + 1) % 3], w = v[(e + 2) % 3];
                    E[ne].a = x < y ? x : y;
                    E[ne].b = x < y ? y : x;
                    E[ne].tf = 4 * t + f;
                    E[ne].w = w;             /* the face's third vertex */
                    ne++;
                }
            }
        qsort(E, ne, sizeof(bedge), cmp_bedge);
        for (i = 0; i < ne; ) {
            int64_t k2 = i + 1, reflex = 0;
            while (k2 < ne && E[k2].a == E[i].a && E[k2].b == E[i].b) k2++;
            if (k2 - i != 2) reflex = 1;     /* pinched: look anyway */
            else {
                int64_t l;
                for (l = 0; l < 2 && !reflex; l++) {
                    int64_t tf1 = E[i + l].tf, w2 = E[i + 1 - l].w;
                    int64_t tt = tf1 / 4, ff = tf1 % 4, v[3], kk = 0, o;
                    for (j = 0; j < 4; j++) if (j != ff) v[kk++] = V(tt, j);
                    o = orient(v[0], v[1], v[2], V(tt, ff));
                    if (o != 0 && orient(v[0], v[1], v[2], w2) == -o)
                        reflex = 1;          /* w2 outside face tf1 */
                }
            }
            if (reflex) {
                int64_t l;
                for (l = i; l < k2; l++)
                    if (!inq[E[l].tf]) {
                        inq[E[l].tf] = 1;
                        queue[nq++] = E[l].tf;
                    }
            }
            i = k2;
        }
        free(E);
    }
    for (i = 0; i < np; i++) if (isb[i]) nbv++;
    if (nq == 0 || nbv < 4) goto rebuild;

    /* coarse grid of boundary vertices; per cell the box of its points */
    bv = malloc(sizeof(int64_t) * nbv);
    if (!bv) { *ierr = 4; goto done; }
    nbv = 0;
    for (i = 0; i < np; i++)
        if (isb[i]) {
            double p[3] = {X[i], Y[i], Z[i]};
            int k;
            if (nbv == 0) for (k = 0; k < 3; k++) lo[k] = hi[k] = p[k];
            for (k = 0; k < 3; k++) {
                if (p[k] < lo[k]) lo[k] = p[k];
                if (p[k] > hi[k]) hi[k] = p[k];
            }
            bv[nbv++] = i;
        }
    G = (int64_t) cbrt((double) nbv / 8.0);
    if (G < 1) G = 1;
    if (G > 64) G = 64;
    ncell = G * G * G;
    for (i = 0; i < 3; i++) cs[i] = (hi[i] - lo[i]) / G + 1e-300;
    cstart = calloc(ncell + 1, sizeof(int64_t));
    cellv = malloc(sizeof(int64_t) * nbv);
    cbox = malloc(sizeof(double) * 6 * ncell);
    if (!cstart || !cellv || !cbox) { *ierr = 4; goto done; }
    {
        int64_t *cid = malloc(sizeof(int64_t) * nbv), *fill;
        if (!cid) { *ierr = 4; goto done; }
        for (i = 0; i < nbv; i++) {
            int64_t p = bv[i], c[3];
            int k;
            double q[3] = {X[p], Y[p], Z[p]};
            for (k = 0; k < 3; k++) {
                c[k] = (int64_t) ((q[k] - lo[k]) / cs[k]);
                if (c[k] >= G) c[k] = G - 1;
                if (c[k] < 0) c[k] = 0;
            }
            cid[i] = (c[2] * G + c[1]) * G + c[0];
            cstart[cid[i] + 1]++;
        }
        for (i = 0; i < ncell; i++) cstart[i + 1] += cstart[i];
        fill = malloc(sizeof(int64_t) * ncell);
        if (!fill) { free(cid); *ierr = 4; goto done; }
        for (i = 0; i < ncell; i++) {
            fill[i] = cstart[i];
            cbox[6 * i] = cbox[6 * i + 1] = cbox[6 * i + 2] = INFINITY;
            cbox[6 * i + 3] = cbox[6 * i + 4] = cbox[6 * i + 5] = -INFINITY;
        }
        for (i = 0; i < nbv; i++) {
            int64_t c = cid[i], p = bv[i];
            double q[3] = {X[p], Y[p], Z[p]};
            int k;
            cellv[fill[c]++] = p;
            for (k = 0; k < 3; k++) {
                if (q[k] < cbox[6 * c + k]) cbox[6 * c + k] = q[k];
                if (q[k] > cbox[6 * c + 3 + k]) cbox[6 * c + 3 + k] = q[k];
            }
        }
        free(fill);
        free(cid);
    }
    gG = G; gncell = ncell; gcstart = cstart; gcellv = cellv; gcbox = cbox;
    for (i = 0; i < 3; i++) { glo[i] = lo[i]; gcs[i] = cs[i]; }

    /* wrap */
    while (nq > 0) {
        int64_t tf = queue[--nq], a, b, c, d, p = -1, tie = 0;
        double n[3], e1[3], e2[3];
        int k;
        t = tf / 4;
        f = tf % 4;
        if (nb[tf] >= 0) continue;              /* glued since queued */
        /* F = (a, b, c) oriented so that the tet's own vertex d is on
           the negative side: orient(a, b, c, x) > 0 means x is outside */
        {
            int64_t v[3], kk = 0, j;
            for (j = 0; j < 4; j++) if (j != f) v[kk++] = V(t, j);
            d = V(t, f);
            a = v[0]; b = v[1]; c = v[2];
            {
                int64_t o = orient(a, b, c, d);
                if (o == 0) continue;           /* a flat tet: leave it */
                if (o > 0) { int64_t s = b; b = c; c = s; }
            }
        }
        /* outward normal (approximate, for the cell search and the
           flatness test): orient(a,b,c,x) = -(b-a)x(c-a).(x-a), so
           outside is -(b-a)x(c-a) */
        e1[0] = X[b] - X[a]; e1[1] = Y[b] - Y[a]; e1[2] = Z[b] - Z[a];
        e2[0] = X[c] - X[a]; e2[1] = Y[c] - Y[a]; e2[2] = Z[c] - Z[a];
        n[0] = -(e1[1] * e2[2] - e1[2] * e2[1]);
        n[1] = -(e1[2] * e2[0] - e1[0] * e2[2]);
        n[2] = -(e1[0] * e2[1] - e1[1] * e2[0]);
        p = find_apex(a, b, c, n, &tie);
        if (p < 0) continue;                    /* a hull facet */
        if (tie) { (*nskip)++; continue; }
        /* flatness: height of p over F against the longest edge */
        {
            double ar[3], h, L = 0.0, P[4][3];
            int64_t vv[4] = {a, b, c, p};
            int x1, x2;
            for (k = 0; k < 4; k++) {
                P[k][0] = X[vv[k]]; P[k][1] = Y[vv[k]]; P[k][2] = Z[vv[k]];
            }
            for (x1 = 0; x1 < 4; x1++)
                for (x2 = x1 + 1; x2 < 4; x2++) {
                    double dx = P[x1][0] - P[x2][0], dy = P[x1][1] - P[x2][1],
                           dz = P[x1][2] - P[x2][2];
                    double l = sqrt(dx * dx + dy * dy + dz * dz);
                    if (l > L) L = l;
                }
            ar[0] = n[0]; ar[1] = n[1]; ar[2] = n[2];
            h = fabs(ar[0] * (P[3][0] - P[0][0]) + ar[1] * (P[3][1] - P[0][1])
                     + ar[2] * (P[3][2] - P[0][2]))
                / sqrt(ar[0] * ar[0] + ar[1] * ar[1] + ar[2] * ar[2] + 1e-300);
            if (h < FLAT * L) { (*nskip)++; continue; }
        }
        /* materials */
        {
            int64_t vv[4] = {a, b, c, p}, m = -1, bad = 0;
            for (k = 0; k < 4; k++) {
                if (isint[vv[k]]) continue;
                if (m < 0) m = imt1[vv[k]];
                else if (imt1[vv[k]] != m) bad = 1;
            }
            if (bad) { (*nskip)++; continue; }
        }
        if (nt >= mt) { *ierr = 5; break; }
        /* the three new faces: each must be new, or a boundary face of
           a tet on its other side */
        {
            int64_t vv[4] = {a, c, b, p}, j, okf = 1;
            for (j = 0; j < 3 && okf; j++) {
                int64_t w[3], kk = 0, l;
                fkey *s;
                for (l = 0; l < 4; l++) if (l != j) w[kk++] = vv[l];
                s = hslot(w[0], w[1], w[2]);
                if (s->tf == -3) okf = 0;
                else if (s->tf >= 0) {
                    int64_t ot = s->tf / 4, of = s->tf % 4;
                    int64_t o1 = orient(w[0], w[1], w[2], vv[j]);
                    int64_t o2 = orient(w[0], w[1], w[2], V(ot, of));
                    if (o1 == 0 || o1 == o2) okf = 0;
                }
            }
            if (!okf) { (*nskip)++; continue; }
        }
        /* the new tet (a, c, b, p): orient(a, c, b, p) < 0, LaGriT's
           positive orientation; its face 3 (opposite p) is F */
        {
            int64_t nt4 = 4 * nt, j;
            int64_t vv[4] = {a, c, b, p};
            for (j = 0; j < 4; j++) it[nt4 + j] = vv[j] + 1;
            for (j = 0; j < 4; j++) nb[nt4 + j] = -1;
            nb[nt4 + 3] = tf;
            nb[tf] = nt4 + 3;
            hslot(a, b, c)->tf = -3;
            for (j = 0; j < 3; j++) {
                int64_t w[3], kk = 0, l;
                fkey *s;
                for (l = 0; l < 4; l++) if (l != j) w[kk++] = vv[l];
                s = hslot(w[0], w[1], w[2]);
                if (s->tf >= 0) {             /* glue to a boundary face */
                    int64_t o = s->tf;
                    nb[o] = nt4 + j;
                    nb[nt4 + j] = o;
                    s->tf = -3;
                } else if (s->tf == -1) {
                    s->tf = nt4 + j;
                    if (nq >= qcap) { *ierr = 5; break; }
                    queue[nq++] = nt4 + j;
                } else {                      /* face already interior */
                    *ierr = 1;
                }
            }
            nt++;
            (*nadded)++;
            if (*ierr) break;
        }
    }

rebuild:
    if (*ierr == 1) {
        /* undo: keep the original tets only */
        nt = *ntet;
        *nadded = 0;
    }
    for (t = 0; t < nt; t++)
        for (f = 0; f < 4; f++) {
            int64_t o = nb[4 * t + f];
            if (*ierr == 1 && o >= 4 * nt) o = -1;
            jt[4 * t + f] = o < 0 ? *mbndry : 4 * (o / 4) + (o % 4) + 1;
        }
    *ntet = nt;
done:
    free(htab); free(nb); free(isb); free(queue); free(bv);
    free(cstart); free(cellv); free(cbox); free(inq);
    htab = NULL;
}
