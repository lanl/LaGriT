/*
 * improve_tets_core.cpp  -- core of the LaGriT command improve_tets
 *
 * Improves the shape of the tets of a tet mesh without adding or removing
 * vertices, so the spatial variation of the mesh resolution is kept. The
 * strategy follows Klingner & Shewchuk, "Aggressive Tetrahedral Mesh
 * Improvement" (Proc. 16th International Meshing Roundtable, 2007,
 * pp. 3-23), restricted to the operations that keep the vertex set:
 *
 *   quality     the smallest sine of the six dihedral angles of a tet
 *               (small and large angles both count); optionally the sine
 *               of an obtuse angle is multiplied by a factor < 1 so that
 *               large angles are penalised more (as Stellar's measure 5)
 *   smoothing   each vertex of a poor tet is moved to raise the smallest
 *               quality among its tets (non-smooth optimisation: steepest
 *               ascent of the worst tet(s), with a line search). Vertices
 *               on the boundary or on a material interface slide in their
 *               plane (one plane) or along their line (two planes) or stay
 *               fixed (three or more planes, or a curved surface)
 *   flips       edge removal (an interior edge with a ring of 3 .. maxring
 *               tets is replaced by the best triangulation of the ring,
 *               Klincsek-style dynamic programming; ring 3 = 3-2 flip,
 *               ring 4 = 4-4 flip) and face removal (2-3 flip). A change is
 *               made only if the worst tet it touches gets better.
 *               Boundary faces and material-interface faces are never
 *               changed, and tets of different colours are never mixed.
 *   schedule    passes of smoothing then flips over the tets below the
 *               target quality, until a pass no longer improves the
 *               worst tet or the number of tets below the target.
 *
 * Nothing here calls or changes any other LaGriT routine; the Fortran
 * command improve_tets.f passes the arrays in and copies the result back.
 *
 * Orientation: the sign of det(v2-v1, v3-v1, v4-v1) for a valid tet is
 * taken from the input mesh (all tets must agree: > 0 inside LaGriT, < 0
 * in an AVS file LaGriT wrote); the tets written back follow the same
 * rule.
 *
 * Oct 2026.
 */

#include <algorithm>
#include <array>
#include <chrono>
#include <cmath>
#include <cstdint>
#include <cstdio>
#include <cstdlib>
#include <cstring>
#include <map>
#include <unordered_map>
#include <vector>

/* progress lines go through LaGriT's log (improve_tets_progress in
   improve_tets.f formats them); the standalone driver prints them */
#ifdef IMPROVE_TETS_STANDALONE
static void improve_tets_progress_(long long *kind, long long *iv, double *rv) {
    std::printf("progress %lld: %lld %lld %lld %lld  %g %g %g\n", *kind, iv[0], iv[1], iv[2], iv[3],
                rv[0], rv[1], rv[2]);
    std::fflush(stdout);
}
#else
extern "C" void improve_tets_progress_(long long *kind, long long *iv, double *rv);
#endif

namespace {

typedef int64_t lgint;
typedef std::array<double, 3> P3;

inline P3 sub(const P3 &a, const P3 &b) { return {a[0] - b[0], a[1] - b[1], a[2] - b[2]}; }
inline P3 add(const P3 &a, const P3 &b) { return {a[0] + b[0], a[1] + b[1], a[2] + b[2]}; }
inline P3 mul(const P3 &a, double s) { return {a[0] * s, a[1] * s, a[2] * s}; }
inline double dot(const P3 &a, const P3 &b) { return a[0] * b[0] + a[1] * b[1] + a[2] * b[2]; }
inline P3 cross(const P3 &a, const P3 &b) {
    return {a[1] * b[2] - a[2] * b[1], a[2] * b[0] - a[0] * b[2], a[0] * b[1] - a[1] * b[0]};
}
inline double norm(const P3 &a) { return std::sqrt(dot(a, a)); }

/* orientation of the mesh being improved: +1 when a valid tet has
   det(v2-v1, v3-v1, v4-v1) < 0 (the order in an AVS file LaGriT writes),
   -1 when it has det > 0 (LaGriT's internal order: read / avs swaps the
   last two vertices). Set from the input mesh by improve_tets_run. */
double g_orient = 1.0;

/* 6 x volume, positive for a valid tet of the mesh being improved */
inline double vol6(const P3 &a, const P3 &b, const P3 &c, const P3 &d) {
    return -g_orient * dot(sub(b, a), cross(sub(c, a), sub(d, a)));
}

const int EDGE[6][2] = {{0, 1}, {0, 2}, {0, 3}, {1, 2}, {1, 3}, {2, 3}};
/* the two vertices not on edge e */
const int OPP[6][2] = {{2, 3}, {1, 3}, {1, 2}, {0, 3}, {0, 2}, {0, 1}};

struct Options {
    int passes = 40;           /* maximum passes (all stages) */
    int maxring = 7;           /* largest ring for edge removal */
    double obtuse = 0.75;      /* factor on the sine of an obtuse angle (1 = plain min sine) */
    double target = 0.60;      /* final target: tets below it are worked on (sine) */
    double maxmove = 1.0;      /* largest distance a vertex may move, in its local spacing */
    double planetol = 1.0e-6;  /* tolerance for coplanar boundary faces (sine of angle) */
    int smooth = 1, flip = 1, slide = 1, multiface = 1, global = 1;
    int progress = 1;          /* progress lines while it runs */
};

struct Stats {
    double qmin0 = 0, qmean0 = 0, qmin1 = 0, qmean1 = 0;
    double amin0 = 0, amax0 = 0, amin1 = 0, amax1 = 0;
    lgint below0 = 0, below1 = 0, passes = 0, moves = 0, edgeflips = 0, faceflips = 0;
    lgint free_v = 0, plane_v = 0, line_v = 0, fixed_v = 0;
    lgint ntets0 = 0, ntets1 = 0;
    lgint ring[16] = {0};
    lgint patch[16] = {0};
    /* resolution report: vertex moves in local spacings, and the local
       spacing after / before at each vertex */
    double move_med = 0, move_max = 0, h_p01 = 1, h_med = 1, h_p99 = 1;
};

class Improver {
public:
    std::vector<P3> X;                       /* vertex coordinates */
    std::vector<std::array<lgint, 4>> T;     /* tets (0-based vertex ids) */
    std::vector<lgint> col;                  /* tet colour */
    std::vector<char> alive;
    std::vector<double> q;                   /* tet quality */
    std::vector<std::vector<lgint>> vt;      /* vertex -> tets */
    std::vector<char> movable;               /* from the pset */
    std::vector<int> vclass;                 /* 0 free, 1 plane, 2 line, 3 fixed */
    std::vector<P3> vdir;                    /* plane normal or line direction */
    std::vector<P3> X0;                      /* original positions */
    std::vector<double> h0;                  /* local spacing */
    Options o;
    Stats s;

    /* ---------------------------------------------------------- quality */
    /* smallest sine of the dihedral angles; obtuse angles weighted by o.obtuse;
       <= 0 for a flat or inverted tet (its 6 x volume, negative) */
    double quality(const P3 &a, const P3 &b, const P3 &c, const P3 &d) const {
        double v = vol6(a, b, c, d);
        if (v <= 0) return v - 1.0;
        const P3 *p[4] = {&a, &b, &c, &d};
        /* face i is opposite vertex i; outward normals (area x 2) */
        P3 n[4];
        double ar[4];
        const int F[4][3] = {{1, 2, 3}, {0, 2, 3}, {0, 1, 3}, {0, 1, 2}};
        for (int i = 0; i < 4; ++i) {
            P3 m = cross(sub(*p[F[i][1]], *p[F[i][0]]), sub(*p[F[i][2]], *p[F[i][0]]));
            /* point it away from vertex i */
            if (dot(m, sub(*p[i], *p[F[i][0]])) > 0) m = mul(m, -1.0);
            n[i] = m;
            ar[i] = norm(m);
            if (ar[i] == 0) return -1.0;
        }
        double best = 1e30;
        for (int e = 0; e < 6; ++e) {
            /* edge e (vertices EDGE[e]) is shared by the faces opposite OPP[e] */
            int f1 = OPP[e][0], f2 = OPP[e][1];
            double L = norm(sub(*p[EDGE[e][1]], *p[EDGE[e][0]]));
            /* sin(dihedral) = (3/2) V L / (A1 A2) with V = v/6, A = ar/2 */
            double sn = (v / 6.0) * L * 1.5 / ((ar[f1] / 2.0) * (ar[f2] / 2.0));
            if (sn > 1.0) sn = 1.0;
            /* interior dihedral obtuse <=> outward normals point the same way */
            if (dot(n[f1], n[f2]) > 0) sn *= o.obtuse;
            best = std::min(best, sn);
        }
        return best;
    }
    double tq(const std::array<lgint, 4> &t) const { return quality(X[t[0]], X[t[1]], X[t[2]], X[t[3]]); }

    /* dihedral angle range of a tet in degrees (reporting only) */
    void angles(const std::array<lgint, 4> &t, double &mn, double &mx) const {
        const P3 *p[4] = {&X[t[0]], &X[t[1]], &X[t[2]], &X[t[3]]};
        const int F[4][3] = {{1, 2, 3}, {0, 2, 3}, {0, 1, 3}, {0, 1, 2}};
        P3 n[4];
        for (int i = 0; i < 4; ++i) {
            P3 m = cross(sub(*p[F[i][1]], *p[F[i][0]]), sub(*p[F[i][2]], *p[F[i][0]]));
            if (dot(m, sub(*p[i], *p[F[i][0]])) > 0) m = mul(m, -1.0);
            n[i] = mul(m, 1.0 / norm(m));
        }
        mn = 180;
        mx = 0;
        for (int e = 0; e < 6; ++e) {
            double c = -dot(n[OPP[e][0]], n[OPP[e][1]]);
            c = std::max(-1.0, std::min(1.0, c));
            double a = std::acos(c) * 180.0 / 3.14159265358979323846;
            mn = std::min(mn, a);
            mx = std::max(mx, a);
        }
    }

    /* ---------------------------------------------------------- set up */
    void build_vt() {
        vt.assign(X.size(), {});
        for (size_t i = 0; i < T.size(); ++i)
            if (alive[i])
                for (int k = 0; k < 4; ++k) vt[T[i][k]].push_back((lgint)i);
    }

    static std::array<lgint, 3> face_key(lgint a, lgint b, lgint c) {
        std::array<lgint, 3> f = {a, b, c};
        std::sort(f.begin(), f.end());
        return f;
    }

    /* boundary and material-interface faces decide how each vertex may move */
    void classify() {
        std::map<std::array<lgint, 3>, std::vector<lgint>> faces;
        const int F[4][3] = {{1, 2, 3}, {0, 2, 3}, {0, 1, 3}, {0, 1, 2}};
        for (size_t i = 0; i < T.size(); ++i)
            for (int k = 0; k < 4; ++k)
                faces[face_key(T[i][F[k][0]], T[i][F[k][1]], T[i][F[k][2]])].push_back((lgint)i);
        std::vector<std::vector<P3>> normals(X.size());
        for (auto &kv : faces) {
            bool constrained = kv.second.size() == 1 ||
                               (kv.second.size() == 2 && col[kv.second[0]] != col[kv.second[1]]);
            if (!constrained) continue;
            const auto &f = kv.first;
            P3 nrm = cross(sub(X[f[1]], X[f[0]]), sub(X[f[2]], X[f[0]]));
            double l = norm(nrm);
            if (l == 0) continue;
            nrm = mul(nrm, 1.0 / l);
            for (int k = 0; k < 3; ++k) normals[f[k]].push_back(nrm);
        }
        vclass.assign(X.size(), 0);
        vdir.assign(X.size(), {0, 0, 0});
        for (size_t v = 0; v < X.size(); ++v) {
            if (!movable[v]) { vclass[v] = 3; continue; }
            std::vector<P3> planes;
            for (const P3 &n : normals[v]) {
                bool same = false;
                for (const P3 &m : planes)
                    if (norm(cross(n, m)) < o.planetol) { same = true; break; }
                if (!same) planes.push_back(n);
            }
            if (planes.empty()) vclass[v] = 0;
            else if (!o.slide) vclass[v] = 3;
            else if (planes.size() == 1) { vclass[v] = 1; vdir[v] = planes[0]; }
            else if (planes.size() == 2) {
                P3 d = cross(planes[0], planes[1]);
                vclass[v] = 2;
                vdir[v] = mul(d, 1.0 / norm(d));
            } else vclass[v] = 3;
        }
        for (int c : vclass) {
            if (c == 0) s.free_v++;
            else if (c == 1) s.plane_v++;
            else if (c == 2) s.line_v++;
            else s.fixed_v++;
        }
    }

    /* mean length of the edges at each vertex (each edge counted once per
       tet it belongs to) */
    std::vector<double> mean_edge() const {
        std::vector<double> sum(X.size(), 0.0);
        std::vector<int> cnt(X.size(), 0);
        for (size_t i = 0; i < T.size(); ++i) {
            if (!alive.empty() && !alive[i]) continue;
            for (int e = 0; e < 6; ++e) {
                lgint a = T[i][EDGE[e][0]], b = T[i][EDGE[e][1]];
                double L = norm(sub(X[a], X[b]));
                sum[a] += L; cnt[a]++;
                sum[b] += L; cnt[b]++;
            }
        }
        for (size_t v = 0; v < X.size(); ++v) sum[v] = cnt[v] ? sum[v] / cnt[v] : 0.0;
        return sum;
    }

    void resolution_report() {
        std::vector<double> h1 = mean_edge(), mv, rt;
        for (size_t v = 0; v < X.size(); ++v) {
            if (h0[v] <= 0 || h1[v] <= 0) continue;   /* not in any tet */
            mv.push_back(norm(sub(X[v], X0[v])) / h0[v]);
            rt.push_back(h1[v] / h0[v]);
        }
        if (mv.empty()) return;
        std::sort(mv.begin(), mv.end());
        std::sort(rt.begin(), rt.end());
        size_t n = rt.size();
        s.move_med = mv[n / 2];
        s.move_max = mv.back();
        s.h_p01 = rt[n / 100];
        s.h_med = rt[n / 2];
        s.h_p99 = rt[std::min(n - 1, (99 * n) / 100)];
    }

    /* the local spacing of each vertex in the input mesh, and where it starts */
    void local_spacing() {
        h0 = mean_edge();
        X0 = X;
    }

    /* ---------------------------------------------------------- smoothing */
    /* constrain a displacement to what the vertex class allows */
    P3 constrain(lgint v, const P3 &d) const {
        switch (vclass[v]) {
        case 0: return d;
        case 1: return sub(d, mul(vdir[v], dot(d, vdir[v])));
        case 2: return mul(vdir[v], dot(d, vdir[v]));
        default: return {0, 0, 0};
        }
    }
    /* put a moved vertex back exactly on its plane / line (axis-aligned
       planes keep the coordinate bit for bit) */
    P3 snap(lgint v, P3 p) const {
        if (vclass[v] == 1 || vclass[v] == 2) {
            P3 d = sub(p, X0[v]);
            p = add(X0[v], constrain(v, d));
            for (int k = 0; k < 3; ++k) {
                bool keep = false;
                if (vclass[v] == 1) keep = std::fabs(std::fabs(vdir[v][k]) - 1.0) < 1e-12;
                else keep = std::fabs(vdir[v][k]) < 1e-12;   /* line normal to axis k */
                if (keep) p[k] = X0[v][k];
            }
        }
        return p;
    }

    double star_min(lgint v, const P3 &p, int *argmin = nullptr) {
        P3 save = X[v];
        X[v] = p;
        double m = 1e30;
        for (lgint t : vt[v]) {
            double qq = tq(T[t]);
            if (qq < m) { m = qq; if (argmin) *argmin = (int)t; }
        }
        X[v] = save;
        return m;
    }

    /* gradient of the quality of tet t with respect to vertex v (central differences) */
    P3 grad(lgint t, lgint v, double hstep) {
        P3 g;
        P3 save = X[v];
        for (int k = 0; k < 3; ++k) {
            X[v] = save; X[v][k] += hstep;
            double qp = tq(T[t]);
            X[v] = save; X[v][k] -= hstep;
            double qm = tq(T[t]);
            g[k] = (qp - qm) / (2 * hstep);
        }
        X[v] = save;
        return g;
    }

    /* the point of smallest norm in the convex hull of the vectors G */
    P3 min_norm_hull(const std::vector<P3> &G) {
        if (G.size() == 1) return G[0];
        std::vector<double> w(G.size(), 1.0 / G.size());
        for (int it = 0; it < 200; ++it) {
            P3 x = {0, 0, 0};
            for (size_t i = 0; i < G.size(); ++i) x = add(x, mul(G[i], w[i]));
            /* Frank-Wolfe step towards the vertex with the smallest <x, g> */
            size_t j = 0;
            double best = 1e300;
            for (size_t i = 0; i < G.size(); ++i) {
                double d = dot(x, G[i]);
                if (d < best) { best = d; j = i; }
            }
            P3 dir = sub(G[j], x);
            double dd = dot(dir, dir);
            if (dd < 1e-30) break;
            double gam = -dot(x, dir) / dd;
            if (gam <= 1e-12) break;
            gam = std::min(gam, 1.0);
            for (size_t i = 0; i < G.size(); ++i) w[i] *= (1 - gam);
            w[j] += gam;
        }
        P3 x = {0, 0, 0};
        for (size_t i = 0; i < G.size(); ++i) x = add(x, mul(G[i], w[i]));
        return x;
    }

    /* raise the smallest quality around vertex v; true if it moved */
    bool smooth_vertex(lgint v) {
        if (vclass[v] == 3 || vt[v].empty()) return false;
        bool moved = false;
        double hloc = h0[v] > 0 ? h0[v] : 1.0;
        double hstep = 1e-7 * hloc;
        for (int iter = 0; iter < 25; ++iter) {
            double qmin = star_min(v, X[v]);
            /* active set: tets within a small band of the minimum */
            std::vector<P3> G;
            for (lgint t : vt[v]) {
                double qq = tq(T[t]);
                if (qq <= qmin + 1e-3 * std::max(qmin, 1e-3)) G.push_back(constrain(v, grad(t, v, hstep)));
            }
            P3 d = min_norm_hull(G);
            double dn = norm(d);
            if (dn < 1e-12) break;
            /* every active gradient must increase along d */
            double rate = 1e30;
            for (const P3 &g : G) rate = std::min(rate, dot(g, d));
            if (rate <= 0) break;
            /* line search: start with a step that would raise the minimum by
               about the gap to the next tet, capped at 0.2 local spacing */
            double step = std::min(0.2 * hloc / dn, 1.0 / dn);
            bool ok = false;
            for (int ls = 0; ls < 30; ++ls) {
                P3 np = snap(v, add(X[v], mul(d, step)));
                if (norm(sub(np, X0[v])) <= o.maxmove * hloc) {
                    double nq = star_min(v, np);
                    if (nq > qmin + 1e-9) { X[v] = np; ok = true; break; }
                }
                step *= 0.5;
            }
            if (!ok) break;
            moved = true;
        }
        if (moved) for (lgint t : vt[v]) q[t] = tq(T[t]);
        return moved;
    }

    /* ---------------------------------------------------------- flips */
    /* flips change only tets whose four vertices are all in the pset */
    bool all_movable(lgint t) const {
        return movable[T[t][0]] && movable[T[t][1]] && movable[T[t][2]] && movable[T[t][3]];
    }
    void kill(lgint t) {
        alive[t] = 0;
        for (int k = 0; k < 4; ++k) {
            auto &l = vt[T[t][k]];
            l.erase(std::remove(l.begin(), l.end(), t), l.end());
        }
    }
    lgint make(const std::array<lgint, 4> &t, lgint c) {
        T.push_back(t);
        col.push_back(c);
        alive.push_back(1);
        q.push_back(tq(t));
        lgint id = (lgint)T.size() - 1;
        for (int k = 0; k < 4; ++k) vt[t[k]].push_back(id);
        return id;
    }
    /* orient (a,b,c,d) LaGriT-positive; false if flat */
    bool orient(std::array<lgint, 4> &t) const {
        double v = vol6(X[t[0]], X[t[1]], X[t[2]], X[t[3]]);
        if (v == 0) return false;
        if (v < 0) std::swap(t[2], t[3]);
        return true;
    }

    /* tets around edge (u,w); fills the ring of vertices in order; false if
       the edge is on the boundary or an interface, colours differ, or the
       ring is not one closed cycle */
    bool edge_ring(lgint u, lgint w, std::vector<lgint> &tets, std::vector<lgint> &ring) {
        tets.clear();
        for (lgint t : vt[u]) {
            const auto &a = T[t];
            if (a[0] == w || a[1] == w || a[2] == w || a[3] == w) tets.push_back(t);
        }
        size_t n = tets.size();
        if (n < 3 || n > (size_t)o.maxring) return false;
        for (lgint t : tets) {
            if (col[t] != col[tets[0]]) return false;
            if (!all_movable(t)) return false;      /* outside the pset */
        }
        /* each tet gives a ring edge between its two other vertices */
        std::vector<std::array<lgint, 2>> e;
        for (lgint t : tets) {
            std::array<lgint, 2> r;
            int k = 0;
            for (int i = 0; i < 4; ++i) if (T[t][i] != u && T[t][i] != w) r[k++] = T[t][i];
            e.push_back(r);
        }
        ring.clear();
        ring.push_back(e[0][0]);
        ring.push_back(e[0][1]);
        std::vector<char> used(n, 0);
        used[0] = 1;
        for (size_t step = 1; step < n; ++step) {
            bool found = false;
            for (size_t i = 0; i < n; ++i) {
                if (used[i]) continue;
                lgint last = ring.back();
                if (e[i][0] == last) { ring.push_back(e[i][1]); used[i] = 1; found = true; break; }
                if (e[i][1] == last) { ring.push_back(e[i][0]); used[i] = 1; found = true; break; }
            }
            if (!found) return false;              /* open ring: boundary edge */
        }
        if (ring.back() != ring.front()) return false;
        ring.pop_back();
        return ring.size() == n;
    }

    /* quality of the two tets triangle (a,b,c) makes with u and w; -1 if the
       triangle does not separate u from w */
    double tri_q(lgint a, lgint b, lgint c, lgint u, lgint w) {
        double su = vol6(X[a], X[b], X[c], X[u]);
        double sw = vol6(X[a], X[b], X[c], X[w]);
        if (!((su > 0 && sw < 0) || (su < 0 && sw > 0))) return -1.0;
        std::array<lgint, 4> t1 = {a, b, c, u}, t2 = {a, b, c, w};
        if (!orient(t1) || !orient(t2)) return -1.0;
        return std::min(tq(t1), tq(t2));
    }

    bool remove_edge(lgint u, lgint w) {
        std::vector<lgint> tets, ring;
        if (!edge_ring(u, w, tets, ring)) return false;
        double old = 1e30;
        for (lgint t : tets) old = std::min(old, q[t]);
        int n = (int)ring.size();
        /* Klincsek: Q[i][j] best min quality of triangulating ring[i..j] */
        std::vector<std::vector<double>> Q(n, std::vector<double>(n, 1e30));
        std::vector<std::vector<int>> K(n, std::vector<int>(n, -1));
        for (int len = 2; len < n; ++len)
            for (int i = 0; i + len < n; ++i) {
                int j = i + len;
                Q[i][j] = -1e30;
                for (int k = i + 1; k < j; ++k) {
                    double v = tri_q(ring[i], ring[k], ring[j], u, w);
                    if (k > i + 1) v = std::min(v, Q[i][k]);
                    if (j > k + 1) v = std::min(v, Q[k][j]);
                    if (v > Q[i][j]) { Q[i][j] = v; K[i][j] = k; }
                }
            }
        double best = Q[0][n - 1];
        if (!(best > old + 1e-9)) return false;
        /* collect the triangles and replace the ring */
        std::vector<std::array<lgint, 3>> tris;
        std::vector<std::array<int, 2>> stack = {{0, n - 1}};
        while (!stack.empty()) {
            auto ij = stack.back(); stack.pop_back();
            int i = ij[0], j = ij[1];
            if (j - i < 2) continue;
            int k = K[i][j];
            tris.push_back({ring[i], ring[k], ring[j]});
            stack.push_back({i, k});
            stack.push_back({k, j});
        }
        lgint c = col[tets[0]];
        double vold = 0, vnew = 0;
        for (lgint t : tets) vold += vol6(X[T[t][0]], X[T[t][1]], X[T[t][2]], X[T[t][3]]);
        std::vector<std::array<lgint, 4>> nt;
        for (auto &tr : tris) {
            std::array<lgint, 4> t1 = {tr[0], tr[1], tr[2], u}, t2 = {tr[0], tr[1], tr[2], w};
            orient(t1); orient(t2);
            vnew += vol6(X[t1[0]], X[t1[1]], X[t1[2]], X[t1[3]]) + vol6(X[t2[0]], X[t2[1]], X[t2[2]], X[t2[3]]);
            nt.push_back(t1); nt.push_back(t2);
        }
        if (std::fabs(vnew - vold) > 1e-9 * std::fabs(vold)) return false;   /* not the same region */
        for (lgint t : tets) kill(t);
        for (auto &t : nt) make(t, c);
        s.edgeflips++;
        if (n < 16) s.ring[n]++;
        return true;
    }

    /* 2-3 flip of face (a,b,c) shared by tets t1 (apex d) and t2 (apex e) */
    bool remove_face(lgint t1, int k) {
        const int F[4][3] = {{1, 2, 3}, {0, 2, 3}, {0, 1, 3}, {0, 1, 2}};
        lgint a = T[t1][F[k][0]], b = T[t1][F[k][1]], c = T[t1][F[k][2]], d = T[t1][k];
        lgint t2 = -1, e = -1;
        for (lgint t : vt[a]) {
            if (t == t1) continue;
            const auto &x = T[t];
            int hit = 0;
            lgint other = -1;
            for (int i = 0; i < 4; ++i) {
                if (x[i] == a || x[i] == b || x[i] == c) hit++;
                else other = x[i];
            }
            if (hit == 3) { t2 = t; e = other; break; }
        }
        if (t2 < 0 || col[t2] != col[t1]) return false;    /* boundary or interface face */
        if (!all_movable(t1) || !all_movable(t2)) return false;
        double old = std::min(q[t1], q[t2]);
        std::array<lgint, 4> n1 = {d, e, a, b}, n2 = {d, e, b, c}, n3 = {d, e, c, a};
        /* all three must be valid with the same handedness */
        double v1 = vol6(X[d], X[e], X[a], X[b]), v2 = vol6(X[d], X[e], X[b], X[c]),
               v3 = vol6(X[d], X[e], X[c], X[a]);
        if (!((v1 > 0 && v2 > 0 && v3 > 0) || (v1 < 0 && v2 < 0 && v3 < 0))) return false;
        orient(n1); orient(n2); orient(n3);
        double nq = std::min(tq(n1), std::min(tq(n2), tq(n3)));
        if (!(nq > old + 1e-9)) return false;
        lgint cc = col[t1];
        kill(t1); kill(t2);
        make(n1, cc); make(n2, cc); make(n3, cc);
        s.faceflips++;
        return true;
    }

    /* the live tet with exactly the vertices a, b, c, d; -1 if none */
    lgint find_tet4(lgint a, lgint b, lgint c, lgint d) const {
        for (lgint t : vt[a]) {
            const auto &x = T[t];
            int hit = 0;
            for (int i = 0; i < 4; ++i)
                if (x[i] == b || x[i] == c || x[i] == d) hit++;
            if (hit == 3) return t;
        }
        return -1;
    }
    /* the live tet holding a, b, c other than 'not'; its fourth vertex in apex */
    lgint find_tet3(lgint a, lgint b, lgint c, lgint notthis, lgint &apex) const {
        for (lgint t : vt[a]) {
            if (t == notthis) continue;
            const auto &x = T[t];
            int hit = 0;
            lgint other = -1;
            for (int i = 0; i < 4; ++i) {
                if (x[i] == a || x[i] == b || x[i] == c) hit++;
                else other = x[i];
            }
            if (hit == 3) { apex = other; return t; }
        }
        return -1;
    }
    static int sgn(double v) { return v > 0 ? 1 : (v < 0 ? -1 : 0); }

    /* Multi-face removal (the inverse of edge removal): the patch of faces
       sandwiched between vertices u and w (each face has a tet with apex u
       on one side and a tet with apex w on the other) is removed and the new
       edge u-w is made; dynamic programming over the patch (a tree of faces)
       chooses the sub-patch whose new tets (u, w, boundary edge) have the
       best worst quality. */
    struct PFace { lgint v[3]; lgint tu, tw; int parent, pedge; int child[3]; };
    bool remove_faces(lgint t1, int k) {
        const int F[4][3] = {{1, 2, 3}, {0, 2, 3}, {0, 1, 3}, {0, 1, 2}};
        lgint u = T[t1][k];
        lgint a = T[t1][F[k][0]], b = T[t1][F[k][1]], c = T[t1][F[k][2]];
        lgint w = -1;
        lgint t2 = find_tet3(a, b, c, t1, w);
        if (t2 < 0 || col[t2] != col[t1]) return false;
        if (!all_movable(t1) || !all_movable(t2)) return false;
        for (lgint t : vt[u])
            for (int i = 0; i < 4; ++i) if (T[t][i] == w) return false;   /* u-w is already an edge */
        lgint c0 = col[t1];
        std::vector<PFace> P;
        P.push_back({{a, b, c}, t1, t2, -1, -1, {-1, -1, -1}});
        std::map<std::array<lgint, 3>, int> seen;
        seen[face_key(a, b, c)] = 0;
        for (size_t f = 0; f < P.size() && P.size() < 24; ++f) {
            for (int e = 0; e < 3; ++e) {
                if ((int)e == P[f].pedge) continue;
                lgint e0 = P[f].v[(e + 1) % 3], e1 = P[f].v[(e + 2) % 3], x0 = P[f].v[e];
                lgint x = -1;
                lgint tu2 = find_tet3(u, e0, e1, P[f].tu, x);
                if (tu2 < 0 || x == w || x == x0 || col[tu2] != c0) continue;
                lgint tw2 = find_tet4(w, e0, e1, x);
                if (tw2 < 0 || col[tw2] != c0) continue;
                if (!all_movable(tu2) || !all_movable(tw2)) continue;
                auto key = face_key(e0, e1, x);
                if (seen.count(key)) continue;             /* not a tree: leave it */
                seen[key] = (int)P.size();
                /* in the child, the edge shared with the parent is opposite x */
                P[f].child[e] = (int)P.size();
                P.push_back({{x, e0, e1}, tu2, tw2, (int)f, 0, {-1, -1, -1}});
            }
        }
        /* quality of the new tet (u, w, e0, e1) built on the patch edge e0-e1
           of a face whose third vertex is x; -1 if it would not fit */
        auto newq = [&](lgint e0, lgint e1, lgint x) -> double {
            if (sgn(vol6(X[u], X[e0], X[e1], X[w])) != sgn(vol6(X[u], X[e0], X[e1], X[x]))) return -1.0;
            if (sgn(vol6(X[w], X[e0], X[e1], X[u])) != sgn(vol6(X[w], X[e0], X[e1], X[x]))) return -1.0;
            std::array<lgint, 4> t = {u, w, e0, e1};
            if (!orient(t)) return -1.0;
            return tq(t);
        };
        /* bottom-up: val[f] = best worst quality of the sub-patch rooted at f */
        int n = (int)P.size();
        std::vector<double> val(n, 0.0);
        std::vector<std::array<char, 3>> take(n);
        for (int f = n - 1; f >= 0; --f) {
            double v = 1e30;
            for (int e = 0; e < 3; ++e) {
                take[f][e] = 0;
                if (e == P[f].pedge) continue;
                lgint e0 = P[f].v[(e + 1) % 3], e1 = P[f].v[(e + 2) % 3];
                double stop = newq(e0, e1, P[f].v[e]);
                double go = P[f].child[e] >= 0 ? val[P[f].child[e]] : -1e30;
                if (go > stop) { take[f][e] = 1; v = std::min(v, go); }
                else v = std::min(v, stop);
            }
            val[f] = v;
        }
        /* the chosen faces; the old worst quality among their tets */
        std::vector<int> S, st = {0};
        while (!st.empty()) {
            int f = st.back(); st.pop_back();
            S.push_back(f);
            for (int e = 0; e < 3; ++e) if (take[f][e]) st.push_back(P[f].child[e]);
        }
        double old = 1e30, vold = 0;
        for (int f : S) {
            old = std::min(old, std::min(q[P[f].tu], q[P[f].tw]));
            vold += vol6(X[T[P[f].tu][0]], X[T[P[f].tu][1]], X[T[P[f].tu][2]], X[T[P[f].tu][3]]);
            vold += vol6(X[T[P[f].tw][0]], X[T[P[f].tw][1]], X[T[P[f].tw][2]], X[T[P[f].tw][3]]);
        }
        if (!(val[0] > old + 1e-9)) return false;
        std::vector<std::array<lgint, 4>> nt;
        double vnew = 0;
        for (int f : S)
            for (int e = 0; e < 3; ++e) {
                if (e == P[f].pedge || take[f][e]) continue;
                std::array<lgint, 4> t = {u, w, P[f].v[(e + 1) % 3], P[f].v[(e + 2) % 3]};
                orient(t);
                vnew += vol6(X[t[0]], X[t[1]], X[t[2]], X[t[3]]);
                nt.push_back(t);
            }
        if (std::fabs(vnew - vold) > 1e-9 * std::fabs(vold)) return false;   /* not the same region */
        for (int f : S) { kill(P[f].tu); kill(P[f].tw); }
        for (auto &t : nt) make(t, c0);
        s.faceflips++;
        if (S.size() < 16) s.patch[S.size()]++;
        return true;
    }

    bool flip_tet(lgint t) {
        if (!alive[t]) return false;
        /* edges first (their rings include 3-2 and 4-4 flips), then faces
           (multi-face removal; a single face is the 2-3 flip) */
        std::array<lgint, 4> a = T[t];
        for (int e = 0; e < 6; ++e)
            if (remove_edge(a[EDGE[e][0]], a[EDGE[e][1]])) return true;
        for (int k = 0; k < 4; ++k)
            if (alive[t] && (o.multiface ? remove_faces(t, k) : remove_face(t, k))) return true;
        return false;
    }

    /* -------------------------------------------------------- progress */
    /* kind 1 start (vertices, tets, below; worst, target)
            2 opening flip pass starts
            3 opening flip pass done (flips)
            4 pass done (pass, below, moves, flips; target, worst)
            5 still working (pass, phase 1 smoothing / 2 flips / 3 opening
              flips, percent done); at most every 15 s
            6 finished (passes)
            7 the heading of the pass table
       rv[2] is always the time since the start, in seconds */
    std::chrono::steady_clock::time_point t_start, t_last;
    double elapsed() const {
        return std::chrono::duration<double>(std::chrono::steady_clock::now() - t_start).count();
    }
    void note(long long kind, long long a = 0, long long b = 0, long long c = 0, long long d = 0,
              double r0 = 0, double r1 = 0) {
        if (!o.progress) return;
        long long iv[4] = {a, b, c, d};
        double rv[3] = {r0, r1, elapsed()};
        improve_tets_progress_(&kind, iv, rv);
        t_last = std::chrono::steady_clock::now();
    }
    void tick(int pass, int phase, size_t done, size_t total) {
        if (!o.progress || (done & 1023) != 0 || total == 0) return;
        if (std::chrono::duration<double>(std::chrono::steady_clock::now() - t_last).count() < 15.0) return;
        note(5, pass, phase, (long long)(100.0 * (double)done / (double)total));
    }

    /* ---------------------------------------------------------- driver */
    void summary(double &qmin, double &qmean, lgint &below, double &amin, double &amax) {
        qmin = 1e30; qmean = 0; below = 0; amin = 180; amax = 0;
        lgint n = 0;
        for (size_t i = 0; i < T.size(); ++i) {
            if (!alive[i]) continue;
            qmin = std::min(qmin, q[i]);
            qmean += q[i];
            if (q[i] < o.target) below++;
            double mn, mx;
            angles(T[i], mn, mx);
            amin = std::min(amin, mn);
            amax = std::max(amax, mx);
            n++;
        }
        qmean /= std::max<lgint>(n, 1);
    }

    void run() {
        t_start = t_last = std::chrono::steady_clock::now();
        alive.assign(T.size(), 1);
        q.resize(T.size());
        for (size_t i = 0; i < T.size(); ++i) q[i] = tq(T[i]);
        build_vt();
        classify();
        local_spacing();
        s.ntets0 = (lgint)T.size();
        summary(s.qmin0, s.qmean0, s.below0, s.amin0, s.amax0);
        note(1, (long long)X.size(), (long long)T.size(), s.below0, 0, s.qmin0, o.target);
        /* one opening flip pass over all tets (as Stellar's first topological
           pass), before smoothing settles the worst tets in a local optimum */
        if (o.flip && o.global) {
            std::vector<std::pair<double, lgint>> work;
            for (size_t i = 0; i < T.size(); ++i)
                if (alive[i]) work.push_back({q[i], (lgint)i});
            std::sort(work.begin(), work.end());
            note(2);
            lgint f0 = s.edgeflips + s.faceflips;
            for (size_t k = 0; k < work.size(); ++k) {
                flip_tet(work[k].second);
                tick(0, 3, k, work.size());
            }
            note(3, s.edgeflips + s.faceflips - f0);
        }
        /* raise the target in stages so each pass works on the worst tets */
        std::vector<double> stages;
        for (double ts : {0.45, 0.55, 0.65})
            if (ts < o.target) stages.push_back(ts);
        stages.push_back(o.target);
        const double final_target = o.target;
        int pass = 0;
        note(7);
        for (double ts : stages) {
            o.target = ts;
            double lastmin = -1e30;
            lgint lastbelow = (lgint)1 << 60;
            int stall = 0, spass = 0;
            while (pass < o.passes && spass < 10) {
                ++spass;
                ++pass;
                s.passes = pass;
                lgint m0 = s.moves, f0 = s.edgeflips + s.faceflips;
                if (o.smooth) {
                    std::vector<char> done(X.size(), 0);
                    std::vector<std::pair<double, lgint>> work;
                    for (size_t i = 0; i < T.size(); ++i)
                        if (alive[i] && q[i] < o.target) work.push_back({q[i], (lgint)i});
                    std::sort(work.begin(), work.end());
                    for (size_t kw = 0; kw < work.size(); ++kw) {
                        tick(pass, 1, kw, work.size());
                        lgint t = work[kw].second;
                        if (!alive[t]) continue;
                        for (int k = 0; k < 4; ++k) {
                            lgint v = T[t][k];
                            if (done[v]) continue;
                            done[v] = 1;
                            if (smooth_vertex(v)) s.moves++;
                        }
                    }
                }
                if (o.flip) {
                    std::vector<std::pair<double, lgint>> work;
                    for (size_t i = 0; i < T.size(); ++i)
                        if (alive[i] && q[i] < o.target) work.push_back({q[i], (lgint)i});
                    std::sort(work.begin(), work.end());
                    for (size_t kw = 0; kw < work.size(); ++kw) {
                        flip_tet(work[kw].second);
                        tick(pass, 2, kw, work.size());
                    }
                }
                double qmin = 1e30;
                lgint below = 0;
                for (size_t i = 0; i < T.size(); ++i)
                    if (alive[i]) { qmin = std::min(qmin, q[i]); if (q[i] < o.target) below++; }
                note(4, pass, below, s.moves - m0, s.edgeflips + s.faceflips - f0, o.target, qmin);
                if (below == 0) break;
                bool better = qmin > lastmin + 1e-5 || below < lastbelow;
                lastmin = std::max(lastmin, qmin);
                lastbelow = std::min(lastbelow, below);
                stall = better ? 0 : stall + 1;
                if (stall >= 2) break;
            }
        }
        o.target = final_target;
        note(6, pass);
        summary(s.qmin1, s.qmean1, s.below1, s.amin1, s.amax1);
        /* compact */
        std::vector<std::array<lgint, 4>> T2;
        std::vector<lgint> c2;
        for (size_t i = 0; i < T.size(); ++i)
            if (alive[i]) { T2.push_back(T[i]); c2.push_back(col[i]); }
        T.swap(T2);
        col.swap(c2);
        alive.assign(T.size(), 1);
        s.ntets1 = (lgint)T.size();
        resolution_report();
    }
};

Improver *g_imp = nullptr;

}  // namespace

/* ------------------------------------------------------------------------
 * Fortran interface (LaGriT integers are 8 bytes).
 *
 * improve_tets_run(nnodes, ntets, x, y, z, itet, itetclr, movable,
 *                  iopt, ropt, nnew, stats, ierr)
 *   itet     4*ntets vertex numbers (1-based, cell by cell)
 *   movable  nnodes flags: 1 = may move (in the pset), 0 = fixed
 *   iopt     passes, maxring, smooth, flip, slide, multiface, global,
 *            progress
 *   ropt     obtuse factor, target, maxmove, planetol
 *   x, y, z  updated in place
 *   nnew     number of tets afterwards (fetch them with improve_tets_fetch)
 *   stats    30 reals: see the Fortran caller (26-30: vertex move median and
 *            largest in local spacings; local spacing after / before at the
 *            1%, 50% and 99% points)
 * improve_tets_fetch(itet, itetclr, ierr) copies the new tets and frees.
 * --------------------------------------------------------------------- */
extern "C" void improve_tets_run_(lgint *nnodes, lgint *ntets, double *x, double *y, double *z,
                                  lgint *itet, lgint *itetclr, lgint *movable, lgint *iopt,
                                  double *ropt, lgint *nnew, double *stats, lgint *ierr) {
    *ierr = 0;
    delete g_imp;
    g_imp = new Improver();
    Improver &m = *g_imp;
    m.o.passes = (int)iopt[0];
    m.o.maxring = (int)std::max<lgint>(3, std::min<lgint>(iopt[1], 12));
    m.o.smooth = (int)iopt[2];
    m.o.flip = (int)iopt[3];
    m.o.slide = (int)iopt[4];
    m.o.multiface = (int)iopt[5];
    m.o.global = (int)iopt[6];
    m.o.progress = (int)iopt[7];
    m.o.obtuse = ropt[0];
    m.o.target = ropt[1];
    m.o.maxmove = ropt[2];
    m.o.planetol = ropt[3];
    lgint nn = *nnodes, nt = *ntets;
    m.X.resize(nn);
    for (lgint i = 0; i < nn; ++i) m.X[i] = {x[i], y[i], z[i]};
    m.movable.resize(nn);
    for (lgint i = 0; i < nn; ++i) m.movable[i] = movable[i] != 0;
    m.T.resize(nt);
    m.col.resize(nt);
    for (lgint i = 0; i < nt; ++i) {
        for (int k = 0; k < 4; ++k) {
            lgint v = itet[4 * i + k] - 1;
            if (v < 0 || v >= nn) { *ierr = 1; return; }
            m.T[i][k] = v;
        }
        m.col[i] = itetclr[i];
    }
    /* the orientation convention: every tet must have the same sign */
    lgint npos = 0, nneg = 0;
    for (lgint i = 0; i < nt; ++i) {
        const auto &t = m.T[i];
        double d = dot(sub(m.X[t[1]], m.X[t[0]]), cross(sub(m.X[t[2]], m.X[t[0]]), sub(m.X[t[3]], m.X[t[0]])));
        if (d > 0) npos++;
        else if (d < 0) nneg++;
    }
    if (npos + nneg < nt || (npos > 0 && nneg > 0)) {
        if (std::getenv("IMPROVE_TETS_DEBUG"))
            std::fprintf(stderr, "improve_tets: %lld tets with det > 0, %lld with det < 0, %lld flat\n",
                         (long long)npos, (long long)nneg, (long long)(nt - npos - nneg));
        *ierr = 2;
        return;
    }
    g_orient = nneg > 0 ? 1.0 : -1.0;
    m.run();
    for (lgint i = 0; i < nn; ++i) { x[i] = m.X[i][0]; y[i] = m.X[i][1]; z[i] = m.X[i][2]; }
    *nnew = (lgint)m.T.size();
    const Stats &s = m.s;
    double st[30] = {s.qmin0, s.qmean0, (double)s.below0, s.amin0, s.amax0,
                     s.qmin1, s.qmean1, (double)s.below1, s.amin1, s.amax1,
                     (double)s.passes, (double)s.moves, (double)s.edgeflips, (double)s.faceflips,
                     (double)s.free_v, (double)s.plane_v, (double)s.line_v, (double)s.fixed_v,
                     (double)s.ntets0, (double)s.ntets1,
                     (double)s.ring[3], (double)s.ring[4], (double)s.ring[5], (double)s.ring[6],
                     (double)s.ring[7], s.move_med, s.move_max, s.h_p01, s.h_med, s.h_p99};
    for (int i = 0; i < 30; ++i) stats[i] = st[i];
}

extern "C" void improve_tets_fetch_(lgint *itet, lgint *itetclr, lgint *ierr) {
    *ierr = 0;
    if (!g_imp) { *ierr = 1; return; }
    Improver &m = *g_imp;
    for (size_t i = 0; i < m.T.size(); ++i) {
        for (int k = 0; k < 4; ++k) itet[4 * i + k] = m.T[i][k] + 1;
        itetclr[i] = m.col[i];
    }
    delete g_imp;
    g_imp = nullptr;
}

#ifdef IMPROVE_TETS_STANDALONE
/* test driver: improve_tets_test in.inp out.inp [target] [passes] [obtuse]
   (AVS UCD, tets only); not part of the LaGriT build */
#include <fstream>
#include <sstream>
#include <string>
int main(int argc, char **argv) {
    if (argc < 3) { std::fprintf(stderr, "usage: %s in.inp out.inp\n", argv[0]); return 2; }
    std::ifstream f(argv[1]);
    lgint nn, ne, a, b, c;
    f >> nn >> ne >> a >> b >> c;
    std::vector<double> x(nn), y(nn), z(nn);
    for (lgint i = 0; i < nn; ++i) { lgint id; f >> id >> x[i] >> y[i] >> z[i]; }
    std::vector<lgint> itet(4 * ne), clr(ne);
    for (lgint i = 0; i < ne; ++i) {
        lgint id; std::string ty;
        f >> id >> clr[i] >> ty;
        for (int k = 0; k < 4; ++k) f >> itet[4 * i + k];
    }
    std::vector<lgint> mov(nn, 1);
    lgint iopt[8] = {40, 7, 1, 1, 1, 1, 1, 1};
    double ropt[4] = {0.75, 0.60, 1.0, 1e-6};
    if (argc > 3) ropt[1] = std::atof(argv[3]);
    if (argc > 4) iopt[0] = std::atol(argv[4]);
    if (argc > 5) ropt[0] = std::atof(argv[5]);
    if (argc > 6) iopt[1] = std::atol(argv[6]);
    if (argc > 7) iopt[2] = std::atol(argv[7]);
    if (argc > 8) iopt[3] = std::atol(argv[8]);
    if (argc > 9) iopt[4] = std::atol(argv[9]);
    if (argc > 10) iopt[5] = std::atol(argv[10]);
    if (argc > 11) iopt[6] = std::atol(argv[11]);
    lgint nnew, ierr;
    double st[30];
    improve_tets_run_(&nn, &ne, x.data(), y.data(), z.data(), itet.data(), clr.data(), mov.data(), iopt, ropt,
                      &nnew, st, &ierr);
    if (ierr) { std::fprintf(stderr, "error %lld\n", (long long)ierr); return 1; }
    std::vector<lgint> it2(4 * nnew), c2(nnew);
    improve_tets_fetch_(it2.data(), c2.data(), &ierr);
    std::ofstream o(argv[2]);
    o << nn << " " << nnew << " 0 0 0\n";
    char buf[200];
    for (lgint i = 0; i < nn; ++i) {
        std::snprintf(buf, sizeof buf, "%lld %.17g %.17g %.17g\n", (long long)(i + 1), x[i], y[i], z[i]);
        o << buf;
    }
    for (lgint i = 0; i < nnew; ++i)
        o << i + 1 << " " << c2[i] << " tet " << it2[4 * i] << " " << it2[4 * i + 1] << " " << it2[4 * i + 2] << " "
          << it2[4 * i + 3] << "\n";
    std::printf("min sine %.4f -> %.4f  mean %.4f -> %.4f  below target %lld -> %lld  dihedral %.2f/%.2f -> "
                "%.2f/%.2f  passes %g moves %g edge flips %g face flips %g  vertices free/plane/line/fixed "
                "%g/%g/%g/%g  tets %g -> %g\n",
                st[0], st[5], st[1], st[6], (long long)st[2], (long long)st[7], st[3], st[4], st[8], st[9], st[10],
                st[11], st[12], st[13], st[14], st[15], st[16], st[17], st[18], st[19]);
    return 0;
}
#endif
