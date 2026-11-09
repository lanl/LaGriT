"""make_dfn_inputs.py OUTDIR

The fracture networks of test_dfn (the same on every machine: a fixed
pseudo-random sequence, exact arithmetic). Python 3 standard library.

  dfn3.inp       three fractures (itetclr 1, 2, 3):
                 1: the unit square in the plane z = 0.5, oriented
                    with its normal along -z (clockwise seen from +z)
                 2: the unit square in the plane x = 0.5, normal +x
                 3: a 0.625 x 0.625 square tilted 30 degrees about
                    the y axis, at y from 1.3 to 1.925 (meets nothing)
                 1 and 2 cross along the line x = z = 0.5: 17 shared
                 vertices, 16 intersection edges, each in 2 triangles
                 of each fracture
  dfn3_flip.inp  the same with one triangle of fracture 2 turned over

Each fracture is triangulated in its own plane by Bowyer-Watson with
exact integer in-circle tests, so it is exactly Delaunay. Vertices are
spaced about h = 1/16; points off the intersection line keep a
distance > 0.55 h from it, so the line's segments (length h) have
empty diametral circles and are edges of both fractures' meshes.
Coordinates in the planes are multiples of 2^-12: exact doubles.
"""

import math
import os
import sys

H = 1.0 / 16
GRID = 4096            # coordinates are integers / GRID


class Lcg:
    """A fixed pseudo-random sequence (no dependence on Python's
    random module)."""

    def __init__(self, seed):
        self.s = seed

    def next(self):
        self.s = (1103515245 * self.s + 12345) % 2147483648
        return self.s / 2147483648.0


def incircle(a, b, c, d):
    """> 0 when d is inside the circle through a, b, c
    (counterclockwise); integers, exact."""
    rows = []
    for p in (a, b, c):
        dx, dy = p[0] - d[0], p[1] - d[1]
        rows.append((dx, dy, dx * dx + dy * dy))
    (a1, a2, a3), (b1, b2, b3), (c1, c2, c3) = rows
    return (a1 * (b2 * c3 - b3 * c2) - a2 * (b1 * c3 - b3 * c1)
            + a3 * (b1 * c2 - b2 * c1))


def orient(a, b, c):
    return (b[0] - a[0]) * (c[1] - a[1]) - (b[1] - a[1]) * (c[0] - a[0])


def delaunay(pts):
    """Bowyer-Watson on integer points; counterclockwise triangles of
    point indices."""
    big = 1 << 40
    sup = [(-big, -big), (big, -big), (0, big)]
    P = list(pts) + sup
    n = len(pts)
    tris = [(n, n + 1, n + 2)]
    for i in range(n):
        p = P[i]
        bad = [t for t in tris if incircle(P[t[0]], P[t[1]], P[t[2]], p)
               > 0]
        edges = {}
        for t in bad:
            for e in ((t[0], t[1]), (t[1], t[2]), (t[2], t[0])):
                k = tuple(sorted(e))
                edges[k] = edges.get(k, 0) + 1
        boundary = [e for t in bad for e in ((t[0], t[1]), (t[1], t[2]),
                                              (t[2], t[0]))
                    if edges[tuple(sorted(e))] == 1]
        tris = [t for t in tris if t not in bad]
        for a, b in boundary:
            t = (a, b, i)
            if orient(P[a], P[b], P[i]) < 0:
                t = (b, a, i)
            tris.append(t)
    return [t for t in tris if max(t) < n]


def fracture_points(size, line, seed):
    """Integer (u, v) points of a square of side size (in GRID units):
    the boundary every H, the line u = line (if given) every H, and
    dart-thrown points 0.75 H apart, more than 0.55 H from the line and
    0.5 H from the boundary."""
    s = int(round(size * GRID))
    hs = int(round(H * GRID))
    pts = set()
    for k in range(0, s + 1, hs):
        pts |= {(k, 0), (k, s), (0, k), (s, k)}
    if line is not None:
        lu = int(round(line * GRID))
        for k in range(0, s + 1, hs):
            pts.add((lu, k))
    pts = sorted(pts)
    rng = Lcg(seed)
    tries = 0
    while tries < 20000:
        tries += 1
        u = int(rng.next() * s)
        v = int(rng.next() * s)
        if min(u, v, s - u, s - v) < 0.5 * hs:
            continue
        if line is not None and abs(u - lu) <= 0.55 * hs:
            continue
        if any((u - p[0]) ** 2 + (v - p[1]) ** 2 < (0.75 * hs) ** 2
               for p in pts):
            continue
        pts.append((u, v))
    return pts


def main():
    out = sys.argv[1]
    os.makedirs(out, exist_ok=True)
    xyz = []
    index = {}

    def vid(p):
        key = tuple(round(c, 15) for c in p)
        if key not in index:
            index[key] = len(xyz)
            xyz.append(p)
        return index[key]

    cells, clr = [], []
    # fracture 1: z = 0.5, (u, v) = (x, y), line at u = 0.5
    pts = fracture_points(1.0, 0.5, 11)
    for t in delaunay(pts):
        v = [vid((pts[q][0] / GRID, pts[q][1] / GRID, 0.5)) for q in t]
        cells.append([v[0], v[2], v[1]])       # clockwise: normal -z
        clr.append(1)
    # fracture 2: x = 0.5, (u, v) = (y, z) ... the line is v = 0.5;
    # build it with u <-> v swapped so the line is u = 0.5
    pts = fracture_points(1.0, 0.5, 22)
    for t in delaunay(pts):
        # (u, v) -> (y = v, z = u); swapping the axes reverses the turn,
        # so reverse the triangle to keep the normal along +x
        v = [vid((0.5, pts[q][1] / GRID, pts[q][0] / GRID)) for q in t]
        cells.append([v[0], v[2], v[1]])
        clr.append(2)
    # fracture 3: tilted 30 degrees about y, apart from the others
    c30, s30 = math.cos(math.pi / 6), math.sin(math.pi / 6)
    pts = fracture_points(0.625, None, 33)
    for t in delaunay(pts):
        v = []
        for q in t:
            u, w = pts[q][0] / GRID, pts[q][1] / GRID
            v.append(vid((0.1 + u * c30, 1.3 + w, 0.1 + u * s30)))
        cells.append(v)
        clr.append(3)

    def write(name, cl):
        with open(os.path.join(out, name), 'w') as f:
            f.write('%d %d 1 0 0\n' % (len(xyz), len(cl)))
            for i, p in enumerate(xyz):
                f.write('%d %.17g %.17g %.17g\n' % (i + 1, *p))
            for k, c in enumerate(cl):
                f.write('%d %d tri %d %d %d\n' % (k + 1, clr[k], c[0] + 1,
                                                  c[1] + 1, c[2] + 1))
            # imt1: the fracture of each vertex's first triangle
            imt = [0] * len(xyz)
            for k, c in enumerate(cl):
                for v in c:
                    if not imt[v]:
                        imt[v] = clr[k]
            f.write('1 1\nimt1, integer\n')
            for i in range(len(xyz)):
                f.write('%d %d\n' % (i + 1, imt[i]))
    write('dfn3.inp', cells)
    flip = [list(c) for c in cells]
    k = clr.index(2) + 5
    flip[k] = [flip[k][0], flip[k][2], flip[k][1]]
    write('dfn3_flip.inp', flip)
    with open(os.path.join(out, 'flipped.txt'), 'w') as f:
        f.write('%d\n' % (k + 1))
    print('dfn3.inp: %d vertices, %d triangles (%s)' % (
        len(xyz), len(cells), ', '.join('%d in fracture %d'
                                        % (clr.count(f), f)
                                        for f in (1, 2, 3))))
    print('dfn3_flip.inp: triangle %d (fracture 2) turned over' % (k + 1))


if __name__ == '__main__':
    main()
