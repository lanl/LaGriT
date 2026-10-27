#!/usr/bin/env python3
"""make_hextotet_inputs.py WORKDIR

Writes the source meshes test_hextotet splits (Python 3, standard
library only; fixed seed, so the same on every machine). Every mesh
carries a vertex attribute f = 1 + 2x + 3y + 5z (a linear field, so
a new vertex made as the average of old ones must get exactly f at its
own position) and two cell materials.

  hex_2mat.inp     2 x 2 x 2 unit hexes (unit cube, 27 vertices);
                   material 2 for z > 0.5, else 1
  hex_jitter.inp   3 x 3 x 3 hexes of the unit cube; the 8 interior
                   vertices moved by up to 0.15 of the spacing (seed
                   214), so the inner faces are not planar while the
                   outside of the cube stays flat; material 2 for
                   x > 0.5
  prism_2mat.inp   a 2 x 2 square grid, each square cut into 2
                   triangles, stacked 2 layers high (z 0 to 1):
                   16 prisms; material 2 in the upper layer
  pyr_cube.inp     the unit cube as 6 pyramids, apex at the centre,
                   one on each face; material 2 for the z faces
  quad_grid.inp    3 x 2 quads of 1 x 0.5 in the plane z = 0;
                   material 2 for x > 1.5

AVS vertex order (as LaGriT's read/avs expects, and as dump/avs
writes LaGriT's own brick): hex and prism the top face first, then the
bottom face, each counter-clockwise seen from above; pyramid apex
first, then the base counter-clockwise seen from the apex; quad
counter-clockwise.
"""
import os
import random
import sys


def f_lin(p):
    return 1.0 + 2.0 * p[0] + 3.0 * p[1] + 5.0 * p[2]


def write_avs(path, V, cells, mats):
    """cells: list of (type, [1-based vertex ids])."""
    with open(path, "w") as out:
        out.write("%d %d 1 0 0\n" % (len(V), len(cells)))
        for i, p in enumerate(V):
            out.write("%d %.17g %.17g %.17g\n" % (i + 1, p[0], p[1], p[2]))
        for k, (typ, ids) in enumerate(cells):
            out.write("%d %d %s %s\n" % (k + 1, mats[k], typ, " ".join(str(i) for i in ids)))
        out.write("1 1\nf, real\n")
        for i, p in enumerate(V):
            out.write("%d %.17g\n" % (i + 1, f_lin(p)))


def brick(n, jitter=0.0, seed=0):
    """(n+1)^3 vertices of the unit cube, n^3 hexes."""
    rnd = random.Random(seed)
    h = 1.0 / n
    idx = {}
    V = []
    for k in range(n + 1):
        for j in range(n + 1):
            for i in range(n + 1):
                p = [i * h, j * h, k * h]
                if jitter and 0 < i < n and 0 < j < n and 0 < k < n:
                    p = [c + rnd.uniform(-jitter, jitter) * h for c in p]
                idx[(i, j, k)] = len(V) + 1
                V.append(p)
    cells = []
    cen = []
    for k in range(n):
        for j in range(n):
            for i in range(n):
                b = [idx[(i, j, k)], idx[(i + 1, j, k)], idx[(i + 1, j + 1, k)], idx[(i, j + 1, k)]]
                t = [idx[(i, j, k + 1)], idx[(i + 1, j, k + 1)], idx[(i + 1, j + 1, k + 1)], idx[(i, j + 1, k + 1)]]
                cells.append(("hex", t + b))
                cen.append(((i + 0.5) * h, (j + 0.5) * h, (k + 0.5) * h))
    return V, cells, cen


def prisms():
    """2 x 2 squares, each cut into 2 triangles, 2 layers."""
    idx = {}
    V = []
    for k in range(3):
        for j in range(3):
            for i in range(3):
                idx[(i, j, k)] = len(V) + 1
                V.append([0.5 * i, 0.5 * j, 0.5 * k])
    cells, mats = [], []
    for k in range(2):
        for j in range(2):
            for i in range(2):
                a, b, c, d = (i, j), (i + 1, j), (i + 1, j + 1), (i, j + 1)
                # alternate the diagonal so both cuts are tested
                tris = [(a, b, c), (a, c, d)] if (i + j) % 2 == 0 else [(a, b, d), (b, c, d)]
                for t in tris:
                    bot = [idx[(p[0], p[1], k)] for p in t]
                    top = [idx[(p[0], p[1], k + 1)] for p in t]
                    cells.append(("prism", top + bot))
                    mats.append(1 if k == 0 else 2)
    return V, cells, mats


def pyramids():
    V = [[0, 0, 0], [1, 0, 0], [1, 1, 0], [0, 1, 0],
         [0, 0, 1], [1, 0, 1], [1, 1, 1], [0, 1, 1], [0.5, 0.5, 0.5]]
    c = 9
    # one base per cube face, put counter-clockwise as seen from the
    # apex (the centre), as LaGriT's pyramid expects
    bases = []
    for face, mat in (([1, 2, 3, 4], 2), ([5, 6, 7, 8], 2), ([1, 2, 6, 5], 1),
                      ([2, 3, 7, 6], 1), ([3, 4, 8, 7], 1), ([4, 1, 5, 8], 1)):
        P = [V[i - 1] for i in face]
        u = [P[1][k] - P[0][k] for k in range(3)]
        w = [P[2][k] - P[0][k] for k in range(3)]
        n = [u[1] * w[2] - u[2] * w[1], u[2] * w[0] - u[0] * w[2], u[0] * w[1] - u[1] * w[0]]
        a = [V[c - 1][k] - P[0][k] for k in range(3)]
        if sum(n[k] * a[k] for k in range(3)) < 0:
            face = face[::-1]
        bases.append((face, mat))
    cells = [("pyr", [c] + b) for b, _ in bases]
    return V, cells, [m for _, m in bases]


def quads():
    idx = {}
    V = []
    for j in range(3):
        for i in range(4):
            idx[(i, j)] = len(V) + 1
            V.append([1.0 * i, 0.5 * j, 0.0])
    cells, mats = [], []
    for j in range(2):
        for i in range(3):
            cells.append(("quad", [idx[(i, j)], idx[(i + 1, j)], idx[(i + 1, j + 1)], idx[(i, j + 1)]]))
            mats.append(2 if i + 0.5 > 1.5 else 1)
    return V, cells, mats


def main(work):
    V, C, cen = brick(2)
    write_avs(os.path.join(work, "hex_2mat.inp"), V, C, [2 if c[2] > 0.5 else 1 for c in cen])
    V, C, cen = brick(3, jitter=0.15, seed=214)
    write_avs(os.path.join(work, "hex_jitter.inp"), V, C, [2 if c[0] > 0.5 else 1 for c in cen])
    V, C, M = prisms()
    write_avs(os.path.join(work, "prism_2mat.inp"), V, C, M)
    V, C, M = pyramids()
    write_avs(os.path.join(work, "pyr_cube.inp"), V, C, M)
    V, C, M = quads()
    write_avs(os.path.join(work, "quad_grid.inp"), V, C, M)


if __name__ == "__main__":
    if len(sys.argv) != 2:
        sys.exit(__doc__)
    main(sys.argv[1])
