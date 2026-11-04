/*
 * lg_brio_order.c -- insertion order for connect (2D)
 *
 * connect inserts the points one at a time; each insertion starts its
 * search for the triangle containing the new point from the triangle
 * made last. When consecutive points are far apart, each search walks
 * across the mesh and the time grows close to N**2.
 *
 * lg_brio_order puts a list of points in a Biased Randomized Insertion
 * Order (N. Amenta, S. Choi, G. Rote, "Incremental constructions con
 * BRIO", Symposium on Computational Geometry 2003): the points are
 * split at random into rounds that double in size (about half the
 * points in the last round, a quarter in the one before, ...), each
 * round is sorted along a Morton (Z-order) curve, and the rounds are
 * inserted smallest first. Within a round consecutive points are close
 * (short searches); the random rounds keep the expected-case bounds of
 * random insertion. The random choices come from a fixed seed, so a run
 * is reproducible.
 *
 * The Morton key uses the two coordinates with the largest spread of
 * the listed points (any plane: xy, xz, yz or tilted).
 *
 * lg_connect_order_set(iold) records the connect option old_order
 * (1: keep the order of LaGriT V3.3.4 and earlier, 0: BRIO, the
 * default); lg_connect_order_get(iold) returns it.
 *
 * Called from Fortran (8-byte integers):
 *   call lg_brio_order(n,list,xic,yic,zic)
 *   n        number of entries in list
 *   list(n)  point numbers (1-based); reordered in place
 *
 * Written for LaGriT, Nov 2026.
 */

#include <stdint.h>
#include <stdlib.h>

static int64_t old_order = 0;

void lg_connect_order_set_(const int64_t *iold)
{
    old_order = *iold ? 1 : 0;
}

void lg_connect_order_get_(int64_t *iold)
{
    *iold = old_order;
}

typedef struct {
    uint64_t key;   /* round in the top bits, then the Morton key */
    int64_t pt;
} item_t;

static int cmp_item(const void *p, const void *q)
{
    const item_t *a = p, *b = q;
    if (a->key != b->key) return a->key < b->key ? -1 : 1;
    return a->pt < b->pt ? -1 : (a->pt > b->pt);
}

/* spread the low 29 bits of v to the even bit positions */
static uint64_t spread(uint64_t v)
{
    v &= 0x1fffffffULL;
    v = (v | (v << 16)) & 0x0000ffff0000ffffULL;
    v = (v | (v << 8)) & 0x00ff00ff00ff00ffULL;
    v = (v | (v << 4)) & 0x0f0f0f0f0f0f0f0fULL;
    v = (v | (v << 2)) & 0x3333333333333333ULL;
    v = (v | (v << 1)) & 0x5555555555555555ULL;
    return v;
}

/* splitmix64: a small, well-mixed generator */
static uint64_t next_random(uint64_t *s)
{
    uint64_t z = (*s += 0x9e3779b97f4a7c15ULL);
    z = (z ^ (z >> 30)) * 0xbf58476d1ce4e5b9ULL;
    z = (z ^ (z >> 27)) * 0x94d049bb133111ebULL;
    return z ^ (z >> 31);
}

void lg_brio_order_(const int64_t *n, int64_t *list, const double *xic,
                    const double *yic, const double *zic)
{
    const double *C[3];
    double lo[3], hi[3], sp[3], scale[2];
    int ax[2], k, nround;
    int64_t i, m = *n;
    uint64_t seed = 0x4c614772695400ULL ^ (uint64_t) m;   /* "LaGriT" */
    item_t *it;

    if (m < 2) return;
    it = malloc(sizeof(item_t) * m);
    if (!it) return;                 /* keep the order given */

    C[0] = xic; C[1] = yic; C[2] = zic;
    for (k = 0; k < 3; k++) {
        lo[k] = hi[k] = C[k][list[0] - 1];
        for (i = 1; i < m; i++) {
            double v = C[k][list[i] - 1];
            if (v < lo[k]) lo[k] = v;
            if (v > hi[k]) hi[k] = v;
        }
        sp[k] = hi[k] - lo[k];
    }
    /* the two coordinates with the largest spread */
    k = (sp[0] <= sp[1] && sp[0] <= sp[2]) ? 0 : (sp[1] <= sp[2] ? 1 : 2);
    ax[0] = (k + 1) % 3;
    ax[1] = (k + 2) % 3;
    if (ax[0] > ax[1]) { int t = ax[0]; ax[0] = ax[1]; ax[1] = t; }
    for (k = 0; k < 2; k++)
        scale[k] = sp[ax[k]] > 0.0 ? 536870911.0 / sp[ax[k]] : 0.0;

    /* number of rounds: the first round has about 100 points or more */
    nround = 0;
    while ((m >> (nround + 1)) >= 100 && nround < 30) nround++;

    for (i = 0; i < m; i++) {
        int64_t p = list[i] - 1;
        uint64_t u = (uint64_t) ((C[ax[0]][p] - lo[ax[0]]) * scale[0]);
        uint64_t v = (uint64_t) ((C[ax[1]][p] - lo[ax[1]]) * scale[1]);
        uint64_t r = next_random(&seed);
        int round = 0;
        /* round = number of trailing one bits, capped: 0 with
           probability 1/2 (the last round), 1 with 1/4, ... */
        while (round < nround && (r & 1)) { round++; r >>= 1; }
        /* rounds with more trailing ones go first */
        it[i].key = ((uint64_t) (nround - round) << 58) |
                    (spread(u) | (spread(v) << 1));
        it[i].pt = list[i];
    }
    qsort(it, m, sizeof(item_t), cmp_item);
    for (i = 0; i < m; i++) list[i] = it[i].pt;
    free(it);
}
