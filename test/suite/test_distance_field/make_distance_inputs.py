#!/usr/bin/env python3
"""make_distance_inputs.py WORK

Writes the AVS inputs of test_distance_field into WORK (standard library
only, fixed seeds, so the inputs are the same on every machine):

  df01  one tilted triangle; sink: a 13^3 grid around it (every face,
        edge and vertex region) plus 7 points on it (d = 0)
  df02  a triangulated sphere (icosahedron, 2 subdivisions, 320
        triangles, radius 1); sink: 1500 random points in [-1.6,1.6]^3
  df03  a warped 6 x 6 quad surface (not planar); sink: 1200 points
  df04  a helix of 80 line segments; sink: 1200 points
  df05  triangles and lines in one mesh object; sink: 1200 points
  df06  the sphere of df02, vertex option (5 and 6 words)
  df07  errors: a tet source, a bad 6th word, quads mixed with
        triangles, a source with no cells
  df08  a larger case: sphere with 4 subdivisions (5120 triangles),
        sink: a 41^3 grid (68921 vertices)
"""
import math
import os
import random
import sys


def write_avs(path, pts, cells, ctype=None):
    """cells: list of (type, [v1, v2, ...]) with 1-based vertex ids."""
    with open(path, "w") as f:
        f.write("%d %d 0 0 0\n" % (len(pts), len(cells)))
        for i, p in enumerate(pts):
            f.write("%d %.17g %.17g %.17g\n" % (i + 1, p[0], p[1], p[2]))
        for i, (t, vs) in enumerate(cells):
            f.write("%d 1 %s %s\n" % (i + 1, t,
                                      " ".join(str(v) for v in vs)))


def icosphere(nsub):
    t = (1.0 + 5 ** 0.5) / 2
    v = [(-1, t, 0), (1, t, 0), (-1, -t, 0), (1, -t, 0),
         (0, -1, t), (0, 1, t), (0, -1, -t), (0, 1, -t),
         (t, 0, -1), (t, 0, 1), (-t, 0, -1), (-t, 0, 1)]
    v = [tuple(c / math.sqrt(sum(x * x for x in p)) for c in p) for p in v]
    f = [(0, 11, 5), (0, 5, 1), (0, 1, 7), (0, 7, 10), (0, 10, 11),
         (1, 5, 9), (5, 11, 4), (11, 10, 2), (10, 7, 6), (7, 1, 8),
         (3, 9, 4), (3, 4, 2), (3, 2, 6), (3, 6, 8), (3, 8, 9),
         (4, 9, 5), (2, 4, 11), (6, 2, 10), (8, 6, 7), (9, 8, 1)]
    for _ in range(nsub):
        mid = {}

        def m(a, b):
            k = (min(a, b), max(a, b))
            if k not in mid:
                p = [(v[a][i] + v[b][i]) / 2 for i in range(3)]
                r = math.sqrt(sum(x * x for x in p))
                v.append(tuple(x / r for x in p))
                mid[k] = len(v) - 1
            return mid[k]
        nf = []
        for a, b, c in f:
            ab, bc, ca = m(a, b), m(b, c), m(c, a)
            nf += [(a, ab, ca), (b, bc, ab), (c, ca, bc), (ab, bc, ca)]
        f = nf
    return v, [("tri", [a + 1, b + 1, c + 1]) for a, b, c in f]


def random_pts(rng, n, lo, hi):
    return [tuple(rng.uniform(lo, hi) for _ in range(3)) for _ in range(n)]


def main(work):
    os.makedirs(work, exist_ok=True)
    rng = random.Random(20261107)
    J = lambda name: os.path.join(work, name)

    # df01: one tilted triangle
    tri = [(0.1, 0.2, 0.3), (1.2, 0.4, 0.1), (0.5, 1.1, 0.8)]
    write_avs(J("df01_src.inp"), tri, [("tri", [1, 2, 3])])
    g = [-0.6 + 2.4 * i / 12 for i in range(13)]
    sink = [(x, y, z) for z in g for y in g for x in g]
    a, b, c = tri
    on = [a, b, c,
          tuple((a[i] + b[i]) / 2 for i in range(3)),
          tuple((b[i] + c[i]) / 2 for i in range(3)),
          tuple((c[i] + a[i]) / 2 for i in range(3)),
          tuple((a[i] + b[i] + c[i]) / 3 for i in range(3))]
    write_avs(J("df01_sink.inp"), sink + on, [])

    # df02 / df06: sphere
    v, f = icosphere(2)
    write_avs(J("df02_src.inp"), v, f)
    write_avs(J("df02_sink.inp"), random_pts(rng, 1500, -1.6, 1.6), [])

    # df03: warped quads, 6 x 6 cells on [0,1]^2
    n = 6
    pts = []
    for j in range(n + 1):
        for i in range(n + 1):
            x, y = i / n, j / n
            pts.append((x, y, 0.25 * math.sin(3 * x) * math.cos(2 * y)
                        + 0.05 * rng.uniform(-1, 1)))
    cells = []
    for j in range(n):
        for i in range(n):
            k = j * (n + 1) + i + 1
            cells.append(("quad", [k, k + 1, k + n + 2, k + n + 1]))
    write_avs(J("df03_src.inp"), pts, cells)
    write_avs(J("df03_sink.inp"), random_pts(rng, 1200, -0.3, 1.3), [])

    # df04: helix of line segments
    m = 80
    pts = [(0.5 + 0.4 * math.cos(4 * math.pi * k / m),
            0.5 + 0.4 * math.sin(4 * math.pi * k / m), k / m)
           for k in range(m + 1)]
    write_avs(J("df04_src.inp"), pts,
              [("line", [k + 1, k + 2]) for k in range(m)])
    write_avs(J("df04_sink.inp"), random_pts(rng, 1200, -0.2, 1.2), [])

    # df05: a fan of triangles in z = 0.5 and lines along z
    pts = [(0.5, 0.5, 0.5)]
    for k in range(8):
        t = 2 * math.pi * k / 8
        pts.append((0.5 + 0.4 * math.cos(t), 0.5 + 0.4 * math.sin(t), 0.5))
    cells = [("tri", [1, 2 + k, 2 + (k + 1) % 8]) for k in range(8)]
    base = len(pts)
    pts += [(0.1, 0.1, 0.0), (0.1, 0.1, 1.0), (0.9, 0.2, 0.0),
            (0.8, 0.9, 1.0)]
    cells += [("line", [base + 1, base + 2]), ("line", [base + 3, base + 4])]
    write_avs(J("df05_src.inp"), pts, cells)
    write_avs(J("df05_sink.inp"), random_pts(rng, 1200, -0.2, 1.2), [])

    # df07: error inputs
    write_avs(J("df07_tet.inp"),
              [(0, 0, 0), (1, 0, 0), (0, 1, 0), (0, 0, 1)],
              [("tet", [1, 2, 3, 4])])
    write_avs(J("df07_mixed.inp"),
              [(0, 0, 0), (1, 0, 0), (1, 1, 0), (0, 1, 0), (2, 0, 0)],
              [("quad", [1, 2, 3, 4]), ("tri", [2, 5, 3])])
    write_avs(J("df07_nocell.inp"), [(0, 0, 0), (1, 0, 0)], [])

    # df08: larger sphere, grid sink
    v, f = icosphere(4)
    write_avs(J("df08_src.inp"), v, f)
    g = [-1.5 + 3.0 * i / 40 for i in range(41)]
    write_avs(J("df08_sink.inp"),
              [(x, y, z) for z in g for y in g for x in g], [])


if __name__ == "__main__":
    main(sys.argv[1] if len(sys.argv) > 1 else "work")
