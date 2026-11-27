"""hflib.py: geometry and the hfield rule for test_hfield.

Python 3 standard library only. Used by make_hfield.py (to write the
input meshes) and check_hfield.py (to compute what LaGriT should give),
so both use the same shapes. The checks here are written independently
of LaGriT's code: distances by brute force over every cell, inside by
the radius (spheres), the box test (cube) or ray crossing (polygon).
"""

import math


def hrule(sources, floor, ceiling, x):
    """h at point x. sources: list of (H(x), distance(x), band, growth)
    functions/numbers. h_i = H + growth*max(0, d - band);
    h = min(ceiling, max(floor, min h_i))."""
    h = ceiling
    for H, dist, band, growth in sources:
        hi = H(x) + growth * max(0.0, dist(x) - band)
        h = min(h, hi)
    return min(ceiling, max(floor, h))


def d_seg(p, a, b):
    ab = [b[i] - a[i] for i in range(3)]
    ap = [p[i] - a[i] for i in range(3)]
    t = sum(ab[i] * ap[i] for i in range(3))
    L = sum(c * c for c in ab)
    t = 0.0 if L == 0 else max(0.0, min(1.0, t / L))
    return math.dist(p, [a[i] + t * ab[i] for i in range(3)])


def d_tri(p, a, b, c):
    """Distance from p to triangle abc: inside the triangle's plane
    region, or else the nearest of its three sides."""
    ab = [b[i] - a[i] for i in range(3)]
    ac = [c[i] - a[i] for i in range(3)]
    n = [ab[1] * ac[2] - ab[2] * ac[1], ab[2] * ac[0] - ab[0] * ac[2],
         ab[0] * ac[1] - ab[1] * ac[0]]
    nn = math.sqrt(sum(x * x for x in n))
    if nn > 0:
        n = [x / nn for x in n]
        s = sum((p[i] - a[i]) * n[i] for i in range(3))
        q = [p[i] - s * n[i] for i in range(3)]
        # q inside abc: the same side of all three edges
        def side(u, v):
            e = [v[i] - u[i] for i in range(3)]
            w = [q[i] - u[i] for i in range(3)]
            cr = [e[1] * w[2] - e[2] * w[1], e[2] * w[0] - e[0] * w[2],
                  e[0] * w[1] - e[1] * w[0]]
            return sum(cr[i] * n[i] for i in range(3))
        if side(a, b) >= 0 and side(b, c) >= 0 and side(c, a) >= 0:
            return abs(s)
    return min(d_seg(p, a, b), d_seg(p, b, c), d_seg(p, c, a))


def icosphere(r, center, levels):
    """Vertices and triangles of an icosphere (levels subdivisions)."""
    t = (1 + 5 ** 0.5) / 2
    V = [(-1, t, 0), (1, t, 0), (-1, -t, 0), (1, -t, 0), (0, -1, t),
         (0, 1, t), (0, -1, -t), (0, 1, -t), (t, 0, -1), (t, 0, 1),
         (-t, 0, -1), (-t, 0, 1)]
    V = [tuple(c / math.sqrt(sum(x * x for x in v)) for c in v) for v in V]
    F = [(0, 11, 5), (0, 5, 1), (0, 1, 7), (0, 7, 10), (0, 10, 11),
         (1, 5, 9), (5, 11, 4), (11, 10, 2), (10, 7, 6), (7, 1, 8),
         (3, 9, 4), (3, 4, 2), (3, 2, 6), (3, 6, 8), (3, 8, 9),
         (4, 9, 5), (2, 4, 11), (6, 2, 10), (8, 6, 7), (9, 8, 1)]
    for _ in range(levels):
        mid = {}

        def m(i, j):
            k = (min(i, j), max(i, j))
            if k not in mid:
                p = [(V[i][c] + V[j][c]) / 2 for c in range(3)]
                s = math.sqrt(sum(x * x for x in p))
                V.append(tuple(x / s for x in p))
                mid[k] = len(V) - 1
            return mid[k]
        G = []
        for a, b, c in F:
            ab, bc, ca = m(a, b), m(b, c), m(c, a)
            G += [(a, ab, ca), (b, bc, ab), (c, ca, bc), (ab, bc, ca)]
        F = G
    V = [tuple(center[c] + r * v[c] for c in range(3)) for v in V]
    return V, F


def write_avs(path, V, cells):
    """cells: list of (type, [1-based vertex numbers])."""
    with open(path, "w", newline="\n") as f:
        f.write("%d %d 0 0 0\n" % (len(V), len(cells)))
        for i, v in enumerate(V):
            f.write("%d %.15g %.15g %.15g\n" % (i + 1, v[0], v[1], v[2]))
        for i, (t, ids) in enumerate(cells):
            f.write("%d 1 %s %s\n" % (i + 1, t, " ".join(map(str, ids))))


def read_avs(p):
    """Vertices, cells and vertex attributes of a LaGriT .inp file."""
    L = open(p).read().split("\n")
    nn, nc, nnd = map(int, L[0].split()[:3])
    V = [tuple(map(float, L[1 + i].split()[1:4])) for i in range(nn)]
    C = []
    for i in range(nc):
        w = L[1 + nn + i].split()
        C.append((w[2].lower(), [int(x) - 1 for x in w[3:]]))
    att = {}
    if nnd:
        k = 1 + nn + nc
        w = L[k].split()
        na, sizes = int(w[0]), [int(x) for x in w[1:]] or [1] * int(w[0])
        names = [L[k + 1 + j].partition(",")[0].strip() for j in range(na)]
        for i in range(nn):
            w = L[k + 1 + na + i].split()[1:]
            col = 0
            for n, sz in zip(names, sizes):
                if sz == 1:
                    att.setdefault(n, []).append(float(w[col]))
                col += sz
    return V, C, att


def in_polygon(p, P):
    """Ray crossing (even-odd) test of p in the 2D polygon P."""
    x, y = p[0], p[1]
    c = False
    for i in range(len(P)):
        (x1, y1), (x2, y2) = P[i][:2], P[(i + 1) % len(P)][:2]
        if (y1 > y) != (y2 > y):
            xc = x1 + (y - y1) * (x2 - x1) / (y2 - y1)
            if xc > x:
                c = not c
    return c


def d_polygon(p, P):
    return min(d_seg(p, P[i], P[(i + 1) % len(P)]) for i in range(len(P)))


def d_box(p, lo, hi):
    """Distance from p to the surface of the box [lo, hi], 0 inside."""
    q = [max(lo[i] - p[i], 0.0, p[i] - hi[i]) for i in range(3)]
    return math.sqrt(sum(x * x for x in q))
