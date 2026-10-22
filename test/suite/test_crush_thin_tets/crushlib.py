"""crushlib.py - shared code for test_crush_thin_tets (Python 3, standard
library only).

  read_avs / write_avs     AVS UCD meshes (vertices, tets, vertex attributes)
  relwidth(P)              crush_thin_tets' own thinness measure: smallest
                           altitude / sqrt(sum of squared edge lengths / 9);
                           1 for a regular tet, 0 for a flat one (signed:
                           negative for an inverted tet)
  radius_ratio(P)          3 * inradius / circumradius (LaGriT's "aspect
                           ratio" in the quality command)
  dihedrals(P)             the six dihedral angles, degrees
  classify(P, tol)         NEEDLE, WEDGE, CAP, SLIVER or UNMATCHED: the first
                           pair of tet parts closer than tol x scale length,
                           in crush_thin_tets' order (a vertex and a vertex,
                           a vertex and an edge, a vertex and a face, two
                           opposite edges)
  Mesh.replay(t, tol)      what crush_thin_tets would do with tet t now,
                           step by step as src/crush_thin_tets.f decides it
                           (find_00, find_01, find_11; find_02 is disabled
                           in LaGriT and reported as such), and why not

Words: VERTEX, never node. LaGriT counts a tet positive when face 1-2-3
seen from vertex 4 is clockwise: det(v2-v1, v3-v1, v4-v1) < 0.
"""
import math

# LaGriT's local tet tables (src/blockcom.h): the 6 edges (pairs of local
# vertices, 0-based) in LaGriT's order; edge I is opposite edge 7-I
EDGES = [(0, 1), (0, 2), (0, 3), (1, 2), (1, 3), (2, 3)]
# the face opposite each vertex
FACES = [(1, 2, 3), (0, 2, 3), (0, 1, 3), (0, 1, 2)]

TYPES = ("NEEDLE", "WEDGE", "CAP", "SLIVER")
KINDS = TYPES + ("UNMATCHED",)


def sub(a, b):
    return (a[0] - b[0], a[1] - b[1], a[2] - b[2])


def dot(a, b):
    return a[0] * b[0] + a[1] * b[1] + a[2] * b[2]


def cross(a, b):
    return (a[1] * b[2] - a[2] * b[1], a[2] * b[0] - a[0] * b[2], a[0] * b[1] - a[1] * b[0])


def norm(a):
    return math.sqrt(dot(a, a))


def det6(P):
    """det(v2-v1, v3-v1, v4-v1): LaGriT positive volume <=> det < 0"""
    return dot(sub(P[1], P[0]), cross(sub(P[2], P[0]), sub(P[3], P[0])))


def volume(P):
    """volume, positive as LaGriT counts it"""
    return -det6(P) / 6.0


def scalelen(P):
    return math.sqrt(sum(dot(sub(P[a], P[b]), sub(P[a], P[b])) for a, b in EDGES) / 9.0)


def relwidth(P):
    """smallest signed altitude / scale length (crush_thin_tets.f,
    function relative_width); the sign is the tet's orientation"""
    s = scalelen(P)
    if s == 0.0:
        return 0.0
    v6 = -det6(P)                      # 6 * LaGriT-positive volume
    alt = min(v6 / max(norm(cross(sub(P[f[1]], P[f[0]]), sub(P[f[2]], P[f[0]]))), 1e-300)
              for f in FACES)
    return alt / s


def radius_ratio(P):
    """3 r_in / r_circ: 1 for a regular tet, 0 for a flat one"""
    v = abs(det6(P)) / 6.0
    area = sum(0.5 * norm(cross(sub(P[f[1]], P[f[0]]), sub(P[f[2]], P[f[0]]))) for f in FACES)
    if v == 0.0 or area == 0.0:
        return 0.0
    r_in = 3.0 * v / area
    a, b, c = sub(P[1], P[0]), sub(P[2], P[0]), sub(P[3], P[0])
    num = (cross(b, c), cross(c, a), cross(a, b))
    d = 2.0 * dot(a, cross(b, c))
    cc = tuple((dot(a, a) * num[0][m] + dot(b, b) * num[1][m] + dot(c, c) * num[2][m]) / d
               for m in range(3))
    return 3.0 * r_in / norm(cc)


def dihedrals(P):
    out = []
    for a, b in EDGES:
        o = [k for k in range(4) if k not in (a, b)]
        e = sub(P[b], P[a])
        n1 = cross(e, sub(P[o[0]], P[a]))
        n2 = cross(e, sub(P[o[1]], P[a]))
        c = dot(n1, n2) / max(norm(n1) * norm(n2), 1e-300)
        out.append(math.degrees(math.acos(max(-1.0, min(1.0, c)))))
    return out


def _vertex_edge(P, v, a, b):
    """distance from vertex v to edge a-b when the foot is strictly inside"""
    w = sub(P[b], P[a])
    lam = dot(sub(P[v], P[a]), w) / max(dot(w, w), 1e-300)
    if not 0.0 < lam < 1.0:
        return None
    foot = tuple(P[a][m] + lam * w[m] for m in range(3))
    return norm(sub(P[v], foot)), foot


def _vertex_face(P, v, f):
    """distance from vertex v to face f when the foot is strictly inside"""
    a, b, c = (P[k] for k in f)
    n = cross(sub(b, a), sub(c, a))
    nn = norm(n)
    if nn == 0.0:
        return None
    n = (n[0] / nn, n[1] / nn, n[2] / nn)
    s = dot(sub(P[v], a), n)
    foot = tuple(P[v][m] - s * n[m] for m in range(3))
    w1 = dot(cross(sub(b, foot), sub(c, foot)), n)
    w2 = dot(cross(sub(c, foot), sub(a, foot)), n)
    w3 = dot(cross(sub(a, foot), sub(b, foot)), n)
    if not (w1 > 0 and w2 > 0 and w3 > 0):
        return None
    return abs(s), foot


def _edge_edge(P, i):
    """closest points of edge i and its opposite edge 7-i (1-based), when
    both feet are strictly inside the edges"""
    a, b = EDGES[i]
    c, d = EDGES[5 - i]
    x1, x2 = P[a], P[c]
    l1, l2 = norm(sub(P[b], x1)), norm(sub(P[d], x2))
    if l1 == 0 or l2 == 0:
        return None
    t1 = tuple(m / l1 for m in sub(P[b], x1))
    t2 = tuple(m / l2 for m in sub(P[d], x2))
    dd = dot(t1, t2)
    vp1 = tuple(t1[m] - t2[m] * dd for m in range(3))
    vp2 = tuple(t2[m] - t1[m] * dd for m in range(3))
    vsq = dot(vp1, vp1)
    if vsq == 0:
        return None
    lam = dot(vp1, sub(x2, x1)) / vsq
    mu = dot(vp2, sub(x1, x2)) / vsq
    if not (0 < lam < l1 and 0 < mu < l2):
        return None
    p1 = tuple(x1[m] + lam * t1[m] for m in range(3))
    p2 = tuple(x2[m] + mu * t2[m] for m in range(3))
    return norm(sub(p1, p2)), p1, p2


def closeness(P):
    """smallest relative distance of each kind (None when no foot falls
    inside): NEEDLE vertex-vertex, WEDGE vertex-edge, CAP vertex-face,
    SLIVER edge-edge, each divided by the tet's scale length"""
    s = scalelen(P)
    out = {}
    out["NEEDLE"] = min(norm(sub(P[a], P[b])) for a, b in EDGES) / s
    best = None
    for f in FACES:
        for e in ((f[0], f[1]), (f[0], f[2]), (f[1], f[2])):
            v = [k for k in f if k not in e][0]
            r = _vertex_edge(P, v, *e)
            if r and (best is None or r[0] < best):
                best = r[0]
    out["WEDGE"] = None if best is None else best / s
    best = None
    for v in range(4):
        r = _vertex_face(P, v, FACES[v])
        if r and (best is None or r[0] < best):
            best = r[0]
    out["CAP"] = None if best is None else best / s
    best = None
    for i in range(3):
        r = _edge_edge(P, i)
        if r and (best is None or r[0] < best):
            best = r[0]
    out["SLIVER"] = None if best is None else best / s
    return out


def classify(P, tol=0.1):
    """the first kind that matches in crush_thin_tets' own order, each
    relative distance tested against tol (TOLCRUSH): NEEDLE (short edge),
    WEDGE (vertex near an edge), CAP (vertex near the opposite face),
    SLIVER (opposite edges nearly crossing); UNMATCHED when the tet is
    thin but none of the four distances is below tol"""
    c = closeness(P)
    for k in TYPES:
        if c[k] is not None and c[k] < tol:
            return k
    return "UNMATCHED"


# ---------------------------------------------------------------- AVS

def read_avs(path):
    L = open(path).read().split("\n")
    h = L[0].split()
    nn, nc, nnd = int(h[0]), int(h[1]), int(h[2])
    V = [tuple(map(float, l.split()[1:4])) for l in L[1:1 + nn]]
    C = []
    k = 1 + nn
    for l in L[k:k + nc]:
        p = l.split()
        C.append([int(x) - 1 for x in p[3:]])
    k += nc
    va = {}
    if nnd:
        na = int(L[k].split()[0])
        names = [L[k + 1 + i].split(",")[0].strip() for i in range(na)]
        rows = [l.split() for l in L[k + 1 + na:k + 1 + na + nn]]
        for j, n in enumerate(names):
            va[n] = [float(r[1 + j]) for r in rows]
    return V, C, va


def write_avs(path, V, C, vatt=(), catt=()):
    with open(path, "w") as f:
        f.write("%d %d %d %d 0\n" % (len(V), len(C), len(vatt), len(catt)))
        for i, p in enumerate(V):
            f.write("%d %.17g %.17g %.17g\n" % (i + 1, p[0], p[1], p[2]))
        for i, c in enumerate(C):
            f.write("%d 1 tet %s\n" % (i + 1, " ".join(str(k + 1) for k in c)))
        for atts, n in ((vatt, len(V)), (catt, len(C))):
            if atts:
                f.write("%d %s\n" % (len(atts), " ".join("1" for _ in atts)))
                for name, _ in atts:
                    f.write("%s, real\n" % name)
                for i in range(n):
                    f.write("%d %s\n" % (i + 1, " ".join("%.10g" % a[1][i] for a in atts)))


# ---------------------------------------------------------------- crush replay

def _mergeable(lives, dies):
    """crush_thin_tets.f function mergeable: a vertex of point type `dies`
    may merge into one of type `lives` only if it is no more constrained
    (boundary and material count, from itp 0, 2, 10, 12)"""
    m = {0: (0, 1), 2: (0, 2), 10: (1, 1), 12: (1, 2)}
    if lives not in m or dies not in m:
        return False
    return m[dies][0] <= m[lives][0] and m[dies][1] <= m[lives][1]


def _itp_result(a, b):
    m = {0: (0, 1), 2: (0, 2), 10: (1, 1), 12: (1, 2)}
    if a not in m or b not in m:
        return 0
    v = (min(m[a][0], m[b][0]), min(m[a][1], m[b][1]))
    return {(0, 1): 0, (0, 2): 2, (1, 1): 10, (1, 2): 12}[v]


class Mesh:
    """a tet mesh with vertex point types (itp1), for replaying crush"""

    def __init__(self, V, C, itp):
        self.V, self.C, self.itp = V, C, [int(round(x)) for x in itp]
        self.around = [[] for _ in V]
        for t, c in enumerate(C):
            for v in c:
                self.around[v].append(t)

    def scale_v1234(self):
        """the scale length crush_thin_tets used for every tet before the
        Oct 2026 fix: that of mesh vertices 1, 2, 3, 4"""
        return scalelen(self.V[:4])

    def P(self, t):
        return [self.V[k] for k in self.C[t]]

    def tets_on_edge(self, a, b):
        return [t for t in self.around[a] if b in self.C[t]]

    def minwidth(self, lst, x0=None):
        """min relwidth over a list of vertex lists; -1 stands for x0"""
        return min(relwidth([x0 if k == -1 else self.V[k] for k in t]) for t in lst)

    def replay(self, t, tol, scale=None):
        """what crush_thin_tets would do with tet t at TOLCRUSH tol.
        scale: the length the distance tests are compared with; None =
        the tet's own scale length (as intended). LaGriT before the Oct
        2026 fix used the scale length of mesh vertices 1-4 for every
        tet (pass Mesh.scale_v1234()).
        Returns (action or None, list of reasons tried)."""
        c = self.C[t]
        P = self.P(t)
        s = scalelen(P) if scale is None else scale
        why = []
        # ---- 00 NEEDLE: a short edge; merge one end into the other
        for a, b in EDGES:
            i1, i2 = c[a], c[b]
            d = norm(sub(self.V[i1], self.V[i2]))
            if d >= tol * s:
                continue
            old = sorted(set(self.around[i1]) | set(self.around[i2]))
            new = []
            for u in self.around[i1]:
                if i2 not in self.C[u]:
                    new.append([-1 if k == i1 else k for k in self.C[u]])
            for u in self.around[i2]:
                if i1 not in self.C[u]:
                    new.append([-1 if k == i2 else k for k in self.C[u]])
            orig = self.minwidth([self.C[u] for u in old])
            m1 = self.minwidth(new, self.V[i2]) if _mergeable(self.itp[i2], self.itp[i1]) else None
            m2 = self.minwidth(new, self.V[i1]) if _mergeable(self.itp[i1], self.itp[i2]) else None
            best = max([m for m in (m1, m2) if m is not None], default=None)
            if best is None:
                why.append("NEEDLE: short edge %d-%d, but neither end may merge "
                           "(point types %d, %d)" % (i1 + 1, i2 + 1, self.itp[i1], self.itp[i2]))
            elif best > 1.01 * orig:
                return ("NEEDLE merge", why)
            else:
                why.append("NEEDLE: merging edge %d-%d would leave the worst nearby "
                           "tet at %.3f (now %.3f): not 1%% better" % (i1 + 1, i2 + 1, best, orig))
        # ---- 01 WEDGE: a vertex close to an edge of a face it shares
        for fv in range(4):
            f = FACES[fv]
            for e in ((f[0], f[1]), (f[0], f[2]), (f[1], f[2])):
                v = [k for k in f if k not in e][0]
                r = _vertex_edge(P, v, *e)
                if not r or r[0] > tol * s:
                    continue
                i0, i1, i2 = c[v], c[e[0]], c[e[1]]
                foot = r[1]
                eltse = self.tets_on_edge(i1, i2)
                old, new = [], []
                share = []
                for u in eltse:
                    old.append(self.C[u])
                    if i0 in self.C[u]:
                        share.append(u)
                        continue
                    new.append([-1 if k == i1 else k for k in self.C[u]])
                    new.append([-1 if k == i2 else k for k in self.C[u]])
                for u in self.around[i0]:
                    if u in share:
                        continue
                    old.append(self.C[u])
                    new.append([-1 if k == i0 else k for k in self.C[u]])
                orig = self.minwidth(old)
                itp12 = _itp_result(self.itp[i1], self.itp[i2])
                m0 = self.minwidth(new, foot) if _mergeable(itp12, self.itp[i0]) else None
                mp = self.minwidth(new, self.V[i0]) if _mergeable(self.itp[i0], itp12) else None
                if m0 is not None and m0 > 1.01 * orig:
                    return ("WEDGE: vertex merged onto edge", why)
                if mp is not None and mp > 1.01 * orig:
                    return ("WEDGE: new edge point merged into vertex", why)
                if m0 is None and mp is None:
                    why.append("WEDGE: vertex %d near edge %d-%d, but the merge is not "
                               "allowed by point types" % (i0 + 1, i1 + 1, i2 + 1))
                else:
                    best = max(m for m in (m0, mp) if m is not None)
                    why.append("WEDGE: vertex %d onto edge %d-%d would leave the worst "
                               "nearby tet at %.3f (now %.3f): not 1%% better"
                               % (i0 + 1, i1 + 1, i2 + 1, best, orig))
        # ---- 02 CAP: disabled in LaGriT (if (.false.) around find_02)
        for v in range(4):
            r = _vertex_face(P, v, FACES[v])
            if r and r[0] <= tol * s:
                why.append("CAP: vertex %d is close to its opposite face, but the CAP "
                           "case is switched off in crush_thin_tets.f" % (c[v] + 1))
                break
        # ---- 11 SLIVER: two opposite edges nearly crossing
        for i in range(3):
            r = _edge_edge(P, i)
            if not r or r[0] >= tol * s:
                continue
            a, b = EDGES[i]
            cc, dd = EDGES[5 - i]
            e1, e2 = (c[a], c[b]), (c[cc], c[dd])
            old, new = [], []
            for u in self.tets_on_edge(*e1):
                old.append(self.C[u])
                if u != t:
                    new.append([-1 if k == e1[0] else k for k in self.C[u]])
                    new.append([-1 if k == e1[1] else k for k in self.C[u]])
            for u in self.tets_on_edge(*e2):
                if u != t:
                    old.append(self.C[u])
                    new.append([-1 if k == e2[0] else k for k in self.C[u]])
                    new.append([-1 if k == e2[1] else k for k in self.C[u]])
            orig = self.minwidth(old)
            t1 = _itp_result(self.itp[e1[0]], self.itp[e1[1]])
            t2 = _itp_result(self.itp[e2[0]], self.itp[e2[1]])
            m1 = self.minwidth(new, r[2]) if _mergeable(t2, t1) else None
            m2 = self.minwidth(new, r[1]) if _mergeable(t1, t2) else None
            best = max([m for m in (m1, m2) if m is not None], default=None)
            if best is None:
                why.append("SLIVER: edges %d-%d and %d-%d nearly cross, but the merge is "
                           "not allowed by point types" % (e1[0] + 1, e1[1] + 1, e2[0] + 1, e2[1] + 1))
            elif best > 1.01 * orig:
                return ("SLIVER: both edges split, new points merged", why)
            else:
                why.append("SLIVER: collapsing edges %d-%d and %d-%d would leave the "
                           "worst nearby tet at %.3f (now %.3f): not 1%% better"
                           % (e1[0] + 1, e1[1] + 1, e2[0] + 1, e2[1] + 1, best, orig))
        if not why:
            cl = closeness(P)
            parts = ", ".join("%s %.3f" % (k.lower(), v) for k, v in cl.items() if v is not None)
            why.append("no case matched at TOLCRUSH %.3g: closest relative distances %s "
                       "(each must be below TOLCRUSH; feet must fall strictly inside)"
                       % (tol, parts))
        return (None, why)
