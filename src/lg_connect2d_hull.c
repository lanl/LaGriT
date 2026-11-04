/*
 * lg_connect2d_hull.c -- finish 2D connect: fill to the convex hull,
 *                        then flip to a Delaunay triangulation
 *
 * connect (2D) inserts the points into a large enclosing triangle and
 * then deletes every triangle touching it. A thin triangle along the
 * convex hull has a circumcircle large enough to reach the enclosing
 * triangle's vertices, so it is replaced by triangles to those
 * vertices and deleted with them: the mesh misses slivers of the hull.
 * Here the outer boundary is walked once, in the manner of a Graham
 * scan, and a triangle is added at every inward (reflex) corner until
 * the boundary is convex. A corner flatter than FLAT (the triangle's
 * height less than 1e-6 of its base) counts as straight: that is
 * round-off in points meant to lie on a line, and the sliver it would
 * add only harms the mesh (smoothing inverts it). Then edges are flipped (Lawson) until every
 * interior edge is locally Delaunay, which makes the whole
 * triangulation Delaunay. Both use lg_predicates2d.c, so the decisions
 * do not depend on tolerances.
 *
 * The triangles may lie in any plane: the points are projected onto
 * the coordinate plane the mesh is most nearly parallel to, which keeps
 * left and right turns (so the hull fill is right in any plane). Circles
 * are kept only when the plane is a coordinate plane (one coordinate
 * the same for every vertex); in a tilted plane no edge is flipped and
 * connect's recon / 0 does that as before.
 *
 * Edges are not flipped between triangles of different colour
 * (itetclr) or along an interface (both vertices interface points).
 *
 * Called from Fortran (8-byte integers):
 *   call lg_connect2d_hull(npoints,ntri,maxtri,itet,itetclr,jtet,
 *        mbndry,isint,xic,yic,zic,iflip,nadded,nflips,ierr)
 *   npoints        number of vertices
 *   ntri           in: number of triangles; out: with the added ones
 *   maxtri         room in itet, itetclr and jtet (triangles)
 *   itet(3,*)      vertex numbers (1-based); changed by flips, added to
 *   itetclr(*)     colour; an added triangle takes the colour of its
 *                  neighbour
 *   jtet(3,*)      in: connect's (used for the neighbours if it is
 *                  consistent, else they are found by sorting edges);
 *                  out: rebuilt for every triangle, LaGriT encoding
 *                  (3*(jt-1)+jf; plus mbndry across an interface or a
 *                  colour change; mbndry on the boundary)
 *   isint(*)       1 for an interface vertex, else 0
 *   iflip          1: flip to Delaunay (coordinate plane only)
 *   nadded,nflips  out: triangles added, edges flipped
 *   ierr           out: 0 ok; 1 the boundary is not one simple loop
 *                  (nothing added, flips still done); 2 an edge in
 *                  three or more triangles (nothing done); 3 a
 *                  pocket could not be filled completely; 4 no memory
 *
 * Written for LaGriT, Nov 2026.
 */

#include <stdint.h>
#include <stdlib.h>
#include <math.h>

void lg_orient2d_(const double *, const double *, const double *,
                  const double *, const double *, const double *,
                  int64_t *);
void lg_incircle2d_(const double *, const double *, const double *,
                    const double *, const double *, const double *,
                    const double *, const double *, int64_t *);

/* largest height / base of a hull corner treated as straight */
#define FLAT 1.0e-6

typedef struct {
    int64_t a, b;   /* edge vertices (0-based), a < b */
    int64_t tf;     /* 3*t + f */
} edge_t;

static int cmp_edge(const void *p, const void *q)
{
    const edge_t *e = p, *f = q;
    if (e->a != f->a) return e->a < f->a ? -1 : 1;
    if (e->b != f->b) return e->b < f->b ? -1 : 1;
    return 0;
}

/* the projected coordinates */
static const double *U, *V;
static double osign;          /* +1 or -1: makes the mesh's triangles positive */

static int64_t orient(int64_t a, int64_t b, int64_t c)
{
    int64_t s;
    lg_orient2d_(&U[a], &V[a], &U[b], &V[b], &U[c], &V[c], &s);
    return osign > 0 ? s : -s;
}

static int64_t incircle(int64_t a, int64_t b, int64_t c, int64_t p)
{
    int64_t s;
    lg_incircle2d_(&U[a], &V[a], &U[b], &V[b], &U[c], &V[c],
                   &U[p], &V[p], &s);
    return s;
}

/* the vertex of triangle t opposite face f is itet[3t+f]; face f is
   the edge (itet[3t+(f+1)%3], itet[3t+(f+2)%3]) */
#define VTX(t, k) (it[3 * (t) + (k)] - 1)

/* 1 when the last call flipped the mesh to Delaunay with the exact
   tests; connect then has no need of recon / 0 */
static int64_t last_exact = 0;

void lg_connect2d_wasexact_(int64_t *iexact)
{
    *iexact = last_exact;
}

void lg_connect2d_hull_(int64_t *npoints, int64_t *ntri, int64_t *maxtri,
                        int64_t *it, int64_t *itetclr, int64_t *jtet,
                        int64_t *mbndry, int64_t *isint,
                        double *xic, double *yic, double *zic,
                        int64_t *iflip, int64_t *nadded, int64_t *nflips,
                        int64_t *ierr)
{
    int64_t np = *npoints, nt = *ntri, mt = *maxtri;
    int64_t *nb = NULL, *succ = NULL, *owner = NULL, *loop = NULL;
    int64_t *stk = NULL, *fstack = NULL;
    edge_t *E = NULL;
    double N[3] = {0.0, 0.0, 0.0};
    const double *C[3];
    int64_t t, f, i, k, ne, drop, coordplane, nloop, nstk;
    int64_t start = -1;

    *nadded = 0;
    *nflips = 0;
    *ierr = 0;
    last_exact = 0;
    if (nt < 1) return;

    C[0] = xic; C[1] = yic; C[2] = zic;
    /* Newell normal: the sum of the triangle normals */
    for (t = 0; t < nt; t++) {
        int64_t a = VTX(t, 0), b = VTX(t, 1), c = VTX(t, 2);
        double e1[3], e2[3];
        for (k = 0; k < 3; k++) {
            e1[k] = C[k][b] - C[k][a];
            e2[k] = C[k][c] - C[k][a];
        }
        N[0] += e1[1] * e2[2] - e1[2] * e2[1];
        N[1] += e1[2] * e2[0] - e1[0] * e2[2];
        N[2] += e1[0] * e2[1] - e1[1] * e2[0];
    }
    drop = 2;
    if (fabs(N[0]) >= fabs(N[1]) && fabs(N[0]) >= fabs(N[2])) drop = 0;
    else if (fabs(N[1]) >= fabs(N[2])) drop = 1;
    U = C[(drop + 1) % 3];
    V = C[(drop + 2) % 3];
    osign = N[drop] >= 0.0 ? 1.0 : -1.0;
    coordplane = 1;
    for (i = 1; i < np && coordplane; i++)
        if (C[drop][i] != C[drop][0]) coordplane = 0;

    /* neighbours: nb[3t+f] = 3t'+f' across face f, or -1 */
    nb = malloc(sizeof(int64_t) * 3 * mt);
    if (!nb) { *ierr = 4; goto done; }
    /* from connect's jtet when it is consistent (the usual case) */
    {
        int64_t ok = 1;
        for (i = 0; i < 3 * nt && ok; i++) {
            int64_t v = jtet[i];
            if (v == *mbndry || v <= 0) { nb[i] = -1; continue; }
            if (v > *mbndry) v -= *mbndry;
            if (v < 1 || v > 3 * nt) ok = 0;
            else nb[i] = v - 1;
        }
        for (i = 0; i < 3 * nt && ok; i++) {
            int64_t o = nb[i], a1, b1, a2, b2;
            if (o < 0) continue;
            if (nb[o] != i) { ok = 0; break; }
            a1 = VTX(i / 3, (i % 3 + 1) % 3);
            b1 = VTX(i / 3, (i % 3 + 2) % 3);
            a2 = VTX(o / 3, (o % 3 + 1) % 3);
            b2 = VTX(o / 3, (o % 3 + 2) % 3);
            if (!(a1 == b2 && b1 == a2)) ok = 0;
        }
        if (ok) goto have_nb;
    }
    /* otherwise by sorting the edges */
    E = malloc(sizeof(edge_t) * 3 * nt);
    if (!E) { *ierr = 4; goto done; }
    for (t = 0; t < nt; t++)
        for (f = 0; f < 3; f++) {
            int64_t a = VTX(t, (f + 1) % 3), b = VTX(t, (f + 2) % 3);
            E[3 * t + f].a = a < b ? a : b;
            E[3 * t + f].b = a < b ? b : a;
            E[3 * t + f].tf = 3 * t + f;
            nb[3 * t + f] = -1;
        }
    ne = 3 * nt;
    qsort(E, ne, sizeof(edge_t), cmp_edge);
    for (i = 0; i < ne; ) {
        int64_t j = i + 1;
        while (j < ne && E[j].a == E[i].a && E[j].b == E[i].b) j++;
        if (j - i == 2) {
            nb[E[i].tf] = E[i + 1].tf;
            nb[E[i + 1].tf] = E[i].tf;
        } else if (j - i > 2) {
            *ierr = 2;
            goto done;
        }
        i = j;
    }
    free(E);
    E = NULL;
have_nb:

    /* the boundary: succ[a] = b for a boundary edge a -> b (interior on
       the left), owner[a] = 3t+f of the triangle that has it */
    succ = malloc(sizeof(int64_t) * np);
    owner = malloc(sizeof(int64_t) * np);
    loop = malloc(sizeof(int64_t) * (np + 1));
    stk = malloc(sizeof(int64_t) * (np + 1));
    if (!succ || !owner || !loop || !stk) { *ierr = 4; goto done; }
    for (i = 0; i < np; i++) succ[i] = -1;
    for (t = 0; t < nt; t++)
        for (f = 0; f < 3; f++) {
            int64_t a, b;
            if (nb[3 * t + f] >= 0) continue;
            a = VTX(t, (f + 1) % 3);
            b = VTX(t, (f + 2) % 3);
            if (succ[a] >= 0) { *ierr = 1; break; }  /* pinched */
            succ[a] = b;
            owner[a] = 3 * t + f;
            if (start < 0 || U[a] < U[start] ||
                (U[a] == U[start] && V[a] < V[start]))
                start = a;
        }
    if (*ierr == 0 && start >= 0) {
        /* one loop through start; any other boundary edge means more
           than one loop (a hole or a second piece): leave it alone */
        int64_t nbnd = 0, v;
        for (i = 0; i < np; i++) if (succ[i] >= 0) nbnd++;
        nloop = 0;
        v = start;
        do {
            loop[nloop++] = v;
            v = succ[v];
        } while (v >= 0 && v != start && nloop <= np);
        if (v != start || nloop != nbnd) *ierr = 1;
    }
    if (*ierr == 0 && start >= 0) {
        /* Graham scan along the loop; start is a hull vertex (least u,
           then least v). An inward corner v between u and w (a right
           turn u -> v -> w) gets the triangle (u, w, v), unless another
           boundary vertex is inside it or on it. */
        int64_t j, w;
        nstk = 0;
        stk[nstk++] = loop[0];
        for (j = 1; j <= nloop; j++) {
            w = loop[j % nloop];
            while (nstk >= 2) {
                int64_t u = stk[nstk - 2], v = stk[nstk - 1], q, clear = 1;
                int64_t tn, o1, o2;
                if (orient(u, v, w) >= 0) break;
                {   /* a corner whose triangle is flatter than FLAT
                       (height / base) counts as straight: such a
                       corner is round-off in points meant to be on a
                       line, and its sliver would only harm the mesh */
                    double ux = U[w] - U[u], uy = V[w] - V[u];
                    double vx = U[v] - U[u], vy = V[v] - V[u];
                    double a2 = fabs(ux * vy - uy * vx);
                    if (a2 <= FLAT * (ux * ux + uy * uy)) break;
                }
                for (q = 0; q < nloop && clear; q++) {
                    int64_t p = loop[q];
                    if (p == u || p == v || p == w) continue;
                    if (orient(u, w, p) >= 0 && orient(w, v, p) >= 0 &&
                        orient(v, u, p) >= 0) clear = 0;
                }
                if (!clear || nt >= mt) { *ierr = 3; break; }
                /* new triangle (u, w, v): face 0 (w,v) faces the
                   triangle owning v -> w, face 1 (v,u) the one owning
                   u -> v, face 2 (u,w) is the new boundary edge */
                tn = nt++;
                it[3 * tn] = u + 1;
                it[3 * tn + 1] = w + 1;
                it[3 * tn + 2] = v + 1;
                o1 = owner[v];
                o2 = owner[u];
                itetclr[tn] = itetclr[o2 / 3];
                nb[3 * tn] = o1;
                nb[o1] = 3 * tn;
                nb[3 * tn + 1] = o2;
                nb[o2] = 3 * tn + 1;
                nb[3 * tn + 2] = -1;
                succ[u] = w;
                owner[u] = 3 * tn + 2;
                succ[v] = -1;
                (*nadded)++;
                nstk--;
            }
            if (*ierr == 3) break;
            if (j < nloop) stk[nstk++] = w;
        }
    }

    /* Lawson flips, exact, coordinate planes only */
    if (*iflip == 1 && coordplane) {
        int64_t nf = 0, cap = 3 * nt + 16;
        fstack = malloc(sizeof(int64_t) * cap);
        if (!fstack) { *ierr = 4; goto done; }
        for (t = 0; t < nt; t++)
            for (f = 0; f < 3; f++)
                if (nb[3 * t + f] > 3 * t + f) fstack[nf++] = 3 * t + f;
        while (nf > 0) {
            int64_t tf = fstack[--nf], t2f2, t1, f1, t2, f2;
            int64_t a, b, c, d, oab, oca, obd, odc, k2;
            t2f2 = nb[tf];
            if (t2f2 < 0) continue;
            t1 = tf / 3; f1 = tf % 3;
            t2 = t2f2 / 3; f2 = t2f2 % 3;
            if (itetclr[t1] != itetclr[t2]) continue;
            a = VTX(t1, f1);
            b = VTX(t1, (f1 + 1) % 3);
            c = VTX(t1, (f1 + 2) % 3);
            d = VTX(t2, f2);
            if (isint[b] && isint[c]) continue;
            if (incircle(a, b, c, d) <= 0) continue;
            /* the outer neighbours, before the flip: (a,b) is t1's face
               opposite c, (c,a) opposite b; t2 is (d, c, b) in some
               rotation: (b,d) is opposite c, (d,c) opposite b */
            oab = nb[3 * t1 + (f1 + 2) % 3];
            oca = nb[3 * t1 + (f1 + 1) % 3];
            for (k2 = 0; k2 < 3; k2++) if (VTX(t2, k2) == c) break;
            obd = nb[3 * t2 + k2];
            for (k2 = 0; k2 < 3; k2++) if (VTX(t2, k2) == b) break;
            odc = nb[3 * t2 + k2];
            /* t1 = (a, b, d), t2 = (a, d, c) */
            it[3 * t1] = a + 1; it[3 * t1 + 1] = b + 1; it[3 * t1 + 2] = d + 1;
            it[3 * t2] = a + 1; it[3 * t2 + 1] = d + 1; it[3 * t2 + 2] = c + 1;
            /* t1 faces: 0 (b,d) -> obd, 1 (d,a) -> t2 face 2,
               2 (a,b) -> oab; t2 faces: 0 (d,c) -> odc,
               1 (c,a) -> oca, 2 (a,d) -> t1 face 1 */
            nb[3 * t1] = obd;
            if (obd >= 0) nb[obd] = 3 * t1;
            nb[3 * t1 + 1] = 3 * t2 + 2;
            nb[3 * t1 + 2] = oab;
            if (oab >= 0) nb[oab] = 3 * t1 + 2;
            nb[3 * t2] = odc;
            if (odc >= 0) nb[odc] = 3 * t2;
            nb[3 * t2 + 1] = oca;
            if (oca >= 0) nb[oca] = 3 * t2 + 1;
            nb[3 * t2 + 2] = 3 * t1 + 1;
            (*nflips)++;
            if (nf + 4 > cap) {
                int64_t *g;
                cap = 2 * cap + 16;
                g = realloc(fstack, sizeof(int64_t) * cap);
                if (!g) { *ierr = 4; goto done; }
                fstack = g;
            }
            fstack[nf++] = 3 * t1;
            fstack[nf++] = 3 * t1 + 2;
            fstack[nf++] = 3 * t2;
            fstack[nf++] = 3 * t2 + 1;
        }
        last_exact = 1;
    }

    /* jtet in LaGriT's encoding */
    for (t = 0; t < nt; t++)
        for (f = 0; f < 3; f++) {
            int64_t o = nb[3 * t + f], val;
            if (o < 0) {
                jtet[3 * t + f] = *mbndry;
                continue;
            }
            val = 3 * (o / 3) + (o % 3) + 1;
            if (itetclr[o / 3] != itetclr[t] ||
                (isint[VTX(t, (f + 1) % 3)] && isint[VTX(t, (f + 2) % 3)]))
                val += *mbndry;
            jtet[3 * t + f] = val;
        }
    *ntri = nt;

done:
    free(nb);
    free(E);
    free(succ);
    free(owner);
    free(loop);
    free(stk);
    free(fstack);
}
