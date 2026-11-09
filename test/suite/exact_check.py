"""exact_check.py: an independent, exact Delaunay oracle for the
test suites. Python 3 standard library only.

    from exact_check import read_avs, analyze

read_avs(path)  -> Mesh: xyz (floats), cells (0-based vertex lists in
                   the file's order), clr (itetclr), nen
analyze(mesh)   -> Result:
    interior   {facet: sign}   facet = sorted tuple of vertex numbers
                               (1-based, as del_vor writes them);
                               sign of delta = (c2 - c1) . n: -1 the
                               facet is not locally Delaunay (the
                               opposite vertex is strictly inside the
                               other cell's circumsphere), 0 a tie
                               (co-spherical, co-circular), +1 Delaunay
    interface  {facet: sign}   the same for facets between two itetclr
    boundary   {facet: sign}   sign of (c - x) . n_out: +1 the cell's
                               circumcenter lies outside the domain
                               beyond this boundary facet
    dl         {facet: delta/L} for interior and interface facets,
               {facet: s/L} for boundary facets (same keys): the
               value, from the exact rational rounded once to a double
               (exactly 0.0 for a tie); L the facet's mean edge length
    intersect  number of triangle edges shared by 3 or more triangles
               of 2 or more itetclr (fracture intersections)

How it is exact. Every double is m * 2^e, so multiplying all
coordinates by one power of two makes them integers, with no
rounding. Circumcenters are then rationals N / D with integer N, D
(the textbook formulas, relative to a vertex of the cell), and the
sign of delta is the sign of an integer expression. Facet normals are
left unnormalized (only signs are needed). Python integers have no
size limit, so nothing is rounded anywhere.

Pairing follows the same rules del_vor documents (del_vor_jtet): a
tet face pairs with the other tet holding it; a triangle edge pairs
with the triangle of the same itetclr holding it; with none of the
same itetclr and exactly one of another, it is an interface edge;
otherwise a boundary edge of that triangle. This file shares no code
with del_vor.
"""

import math
from fractions import Fraction


class Mesh:
    pass


class Result:
    pass


def read_avs(path):
    """AVS UCD: vertices, cells (tri or tet), itetclr. Tet vertices are
    put in LaGriT's order (AVS 1 2 3 4 -> LaGriT 1 2 4 3), as LaGriT's
    read / avs and del_vor do."""
    with open(path) as f:
        lines = [ln for ln in f if ln.strip()]
    h = lines[0].split()
    nn, nc = int(h[0]), int(h[1])
    m = Mesh()
    m.xyz = [tuple(float(v) for v in ln.split()[1:4])
             for ln in lines[1:1 + nn]]
    m.cells, m.clr = [], []
    for ln in lines[1 + nn:1 + nn + nc]:
        t = ln.split()
        v = [int(x) - 1 for x in t[3:]]
        if t[2] == 'tet':
            v = [v[0], v[1], v[3], v[2]]
        m.cells.append(v)
        m.clr.append(int(t[1]))
    m.nen = len(m.cells[0])
    return m


def _scale(xyz):
    """Integer coordinates: every value times one power of two."""
    den = 1
    for p in xyz:
        for v in p:
            den = max(den, Fraction(v).denominator)
    return [tuple(int(Fraction(v) * den) for v in p) for p in xyz], den


def _value(num, den, n):
    """num / den along the unit vector of n, as a double: sign and
    size from one rounding of the exact square."""
    if num == 0:
        return 0.0
    q = Fraction(num * num, den * den * dot(n, n))
    return math.sqrt(q) * _sign(num) * _sign(den)


def sub(a, b):
    return (a[0] - b[0], a[1] - b[1], a[2] - b[2])


def dot(a, b):
    return a[0] * b[0] + a[1] * b[1] + a[2] * b[2]


def cross(a, b):
    return (a[1] * b[2] - a[2] * b[1], a[2] * b[0] - a[0] * b[2],
            a[0] * b[1] - a[1] * b[0])


def mul(s, a):
    return (s * a[0], s * a[1], s * a[2])


def add(a, b):
    return (a[0] + b[0], a[1] + b[1], a[2] + b[2])


def _circ(P, cell):
    """Circumcenter of a cell as (p1, N, D): c = p1 + N / D."""
    p = [P[v] for v in cell]
    a = sub(p[1], p[0])
    b = sub(p[2], p[0])
    if len(cell) == 4:
        d = sub(p[3], p[0])
        N = add(add(mul(dot(a, a), cross(b, d)), mul(dot(b, b), cross(d, a))),
                mul(dot(d, d), cross(a, b)))
        D = 2 * dot(a, cross(b, d))
    else:
        axb = cross(a, b)
        N = cross(sub(mul(dot(a, a), b), mul(dot(b, b), a)), axb)
        D = 2 * dot(axb, axb)
    return p[0], N, D


def _sign(x):
    return (x > 0) - (x < 0)


# local faces, as del_vor_jtet / LaGriT's ielmface1: face j opposite
# vertex j
FACE_TRI = [(1, 2), (2, 0), (0, 1)]
FACE_TET = [(1, 2, 3), (0, 3, 2), (0, 1, 3), (0, 2, 1)]


def analyze(m):
    P, scale = _scale(m.xyz)
    faces = FACE_TET if m.nen == 4 else FACE_TRI
    circ = {}

    def cc(it):
        if it not in circ:
            circ[it] = _circ(P, m.cells[it])
        return circ[it]

    # every facet -> the (cell, opposite vertex) holding it
    holders = {}
    for it, c in enumerate(m.cells):
        for j, fj in enumerate(faces):
            key = tuple(sorted(c[q] for q in fj))
            holders.setdefault(key, []).append((it, c[j]))

    def normal(fv, toward):
        """Unnormalized normal of the facet, pointing to toward."""
        f0 = P[fv[0]]
        w = sub(P[toward], f0)
        if len(fv) == 3:
            n = cross(sub(P[fv[1]], f0), sub(P[fv[2]], f0))
        else:
            e = sub(P[fv[1]], f0)
            n = sub(mul(dot(e, e), w), mul(dot(w, e), e))
        if dot(n, w) < 0:
            n = mul(-1, n)
        return n

    def length(fv):
        x = [m.xyz[v] for v in fv]
        if len(fv) == 2:
            return math.dist(x[0], x[1])
        return (math.dist(x[0], x[1]) + math.dist(x[1], x[2])
                + math.dist(x[0], x[2])) / 3.0

    def delta_sign(fv, t1, t2, o2, key):
        n = normal(fv, o2)
        p1, N1, D1 = cc(t1)
        p2, N2, D2 = cc(t2)
        val = (dot(sub(p2, p1), n) * D1 * D2 + dot(N2, n) * D1
               - dot(N1, n) * D2)
        r.dl[key] = _value(val, D1 * D2 * scale, n) / length(fv)
        return _sign(val) * _sign(D1 * D2)

    def out_sign(fv, t, o, key):
        n = mul(-1, normal(fv, o))
        p1, N, D = cc(t)
        val = dot(sub(p1, P[fv[0]]), n) * D + dot(N, n)
        r.dl[key] = _value(val, D * scale, n) / length(fv)
        return _sign(val) * _sign(D)

    r = Result()
    r.interior, r.interface, r.boundary = {}, {}, {}
    r.dl = {}
    r.intersect = 0
    for key, hs in holders.items():
        fv = list(key)
        name = tuple(v + 1 for v in key)
        if m.nen == 4:
            if len(hs) == 2:
                (t1, o1), (t2, o2) = hs
                s = delta_sign(fv, t1, t2, o2, name)
                same = m.clr[t1] == m.clr[t2]
                (r.interior if same else r.interface)[name] = s
            elif len(hs) == 1:
                t, o = hs[0]
                r.boundary[name] = out_sign(fv, t, o, name)
            continue
        # triangles: pair within one itetclr
        clrs = {m.clr[t] for t, _ in hs}
        if len(hs) >= 3 and len(clrs) >= 2:
            r.intersect += 1
        for t, o in hs:
            same = [(u, ou) for u, ou in hs
                    if u != t and m.clr[u] == m.clr[t]]
            other = [(u, ou) for u, ou in hs if m.clr[u] != m.clr[t]]
            if len(same) == 1:
                u, ou = same[0]
                if t < u:
                    k = name + (m.clr[t],)
                    r.interior[k] = delta_sign(fv, t, u, ou, k)
            elif not same and len(other) == 1:
                u, ou = other[0]
                if t < u:
                    r.interface[name] = delta_sign(fv, t, u, ou, name)
            elif not same:
                k = name + (m.clr[t],)
                r.boundary[k] = out_sign(fv, t, o, k)
    return r


def counts(r, tol=1e-10):
    """Summary numbers to compare with del_vor's report, for the
    tolerance tol: a violation is delta/L < -tol, a tie
    |delta/L| <= tol, a boundary circumcenter outside s/L > tol."""
    pairs = list(r.interior) + list(r.interface)
    return {
        'interior': len(r.interior), 'interface': len(r.interface),
        'boundary': len(r.boundary),
        'violations': sum(1 for k in pairs if r.dl[k] < -tol),
        'ties': sum(1 for k in pairs if abs(r.dl[k]) <= tol),
        'boundary_out': sum(1 for k in r.boundary if r.dl[k] > tol),
        'intersect': r.intersect,
    }


def flagged(r, tol=1e-10, nen=4):
    """The facets del_vor must write to the violations file, as
    {(sorted vertex numbers): kind}: 1 interior violation, 2 interface
    violation, 3 boundary circumcenter outside."""
    out = {}
    for k in r.interior:
        if r.dl[k] < -tol:
            out[facet_key(k, nen)] = 1
    for k in r.interface:
        if r.dl[k] < -tol:
            out[facet_key(k, nen)] = 2
    for k in r.boundary:
        if r.dl[k] > tol:
            out[facet_key(k, nen)] = 3
    return out


def facet_key(k, nen):
    """The vertex numbers of a facet key: triangle keys of interior and
    boundary edges carry the itetclr as a third entry."""
    return k[:2] if nen == 3 else k
