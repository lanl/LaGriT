#!/usr/bin/env python3
"""make_edge_inputs.py WORKDIR

Writes the meshes test_edge_lengths measures (Python 3, standard
library only; fixed seed). Answers that hold exactly are noted; the
checker recomputes every value from the mesh itself anyway.

  el_equi.inp     equilateral triangles of side 0.5 (a 6 x 5 lattice):
                  every edge 0.5, so min = max = mean = 0.5 everywhere
  el_rect.inp     a 4 x 3 grid of 1 x 2 rectangles, each cut into two
                  triangles: edges 1, 2 and sqrt(5)
  el_quad.inp     the same grid as quads (no diagonals): edges 1 and 2;
                  a corner vertex has min 1, max 2, mean 1.5
  el_mixed.inp    quads and triangles in one mesh (hybrid)
  el_jitter.inp   a 10 x 10 triangle grid with every vertex moved at
                  random (seed 7): all different lengths
  el_lone.inp     the jitter mesh plus one vertex in no cell: 0
  el_tet.inp      a 2 x 2 x 2 brick of unit cubes, 6 tets per cube
                  (made by the deck with hextotet): edges 1, sqrt(2),
                  sqrt(3)
"""
import math
import os
import random
import sys


def write(path, V, cells):
    with open(path, "w") as f:
        f.write("%d %d 0 0 0\n" % (len(V), len(cells)))
        for i, p in enumerate(V):
            f.write("%d %.17g %.17g %.17g\n" % (i + 1, p[0], p[1], p[2]))
        for k, (t, ids) in enumerate(cells):
            f.write("%d 1 %s %s\n" % (k + 1, t, " ".join(str(i) for i in ids)))


def grid(nx, ny, dx, dy):
    V = [(i * dx, j * dy, 0.0) for j in range(ny + 1) for i in range(nx + 1)]
    idx = lambda i, j: j * (nx + 1) + i + 1
    return V, idx


def main(work):
    # equilateral lattice
    s = 0.5
    rows, cols = 5, 6
    V, ids = [], {}
    for j in range(rows + 1):
        for i in range(cols + 1):
            ids[(i, j)] = len(V) + 1
            V.append((i * s + (s / 2 if j % 2 else 0.0), j * s * math.sqrt(3) / 2, 0.0))
    C = []
    for j in range(rows):
        for i in range(cols):
            if j % 2 == 0:
                C.append(("tri", [ids[(i, j)], ids[(i + 1, j)], ids[(i, j + 1)]]))
                C.append(("tri", [ids[(i + 1, j)], ids[(i + 1, j + 1)], ids[(i, j + 1)]]))
            else:
                C.append(("tri", [ids[(i, j)], ids[(i + 1, j + 1)], ids[(i, j + 1)]]))
                C.append(("tri", [ids[(i, j)], ids[(i + 1, j)], ids[(i + 1, j + 1)]]))
    write(os.path.join(work, "el_equi.inp"), V, C)

    V, idx = grid(4, 3, 1.0, 2.0)
    C = []
    for j in range(3):
        for i in range(4):
            a, b, c, d = idx(i, j), idx(i + 1, j), idx(i + 1, j + 1), idx(i, j + 1)
            C += [("tri", [a, b, c]), ("tri", [a, c, d])]
    write(os.path.join(work, "el_rect.inp"), V, C)
    Q = [("quad", [idx(i, j), idx(i + 1, j), idx(i + 1, j + 1), idx(i, j + 1)]) for j in range(3) for i in range(4)]
    write(os.path.join(work, "el_quad.inp"), V, Q)
    M = []
    for j in range(3):
        for i in range(4):
            a, b, c, d = idx(i, j), idx(i + 1, j), idx(i + 1, j + 1), idx(i, j + 1)
            M += [("quad", [a, b, c, d])] if (i + j) % 2 == 0 else [("tri", [a, b, c]), ("tri", [a, c, d])]
    write(os.path.join(work, "el_mixed.inp"), V, M)

    rnd = random.Random(7)
    V, idx = grid(10, 10, 0.1, 0.1)
    V = [(x + rnd.uniform(-0.03, 0.03), y + rnd.uniform(-0.03, 0.03), rnd.uniform(-0.02, 0.02)) for x, y, _ in V]
    C = []
    for j in range(10):
        for i in range(10):
            a, b, c, d = idx(i, j), idx(i + 1, j), idx(i + 1, j + 1), idx(i, j + 1)
            C += [("tri", [a, b, c]), ("tri", [a, c, d])]
    write(os.path.join(work, "el_jitter.inp"), V, C)
    write(os.path.join(work, "el_lone.inp"), V + [(5.0, 5.0, 5.0)], C)


if __name__ == "__main__":
    if len(sys.argv) != 2:
        sys.exit(__doc__)
    main(sys.argv[1])
