#!/usr/bin/env python3
"""make_demo_inputs.py DIR

Inputs for the interpolate / compute DEMONSTRATIONS (not tests): small,
picturable 2D cases (and one slice through 3D). Python 3, standard
library only; fixed seeds.

  grid_pts.inp     101 x 101 points on the unit square (z = 0): the
                   "pixels" every 2D demo interpolates onto
  grid_quads.inp   60 x 60 quads on the unit square (for map)
  vor_src.inp      25 random points, value vid = 1..25 (voronoi)
  tri_src.inp      a coarse triangle mesh (7 x 7 vertices, alternating
                   diagonals) carrying the bump field fbump (continuous)
  quad_src.inp     a 5 x 5 quad grid turned 25 degrees and large enough
                   to cover the square, cell value check = checkerboard
                   0 / 1 plus a gradient (map)
  curve_src.inp    120 points along a sine curve and a circle (distance)
  sphere_tri.inp   a triangulated sphere, radius 0.3, centre (0.5, 0.5,
                   0.5); slice_pts.inp: the 101 x 101 points of the
                   plane z = 0.5 (signed distance)
"""
import math
import os
import random
import sys


def bump(x, y):
    """the field shown in the continuous demo: a smooth peak plus a tilt"""
    return math.exp(-((x - .62) ** 2 + (y - .38) ** 2) / .03) + .4 * x


def write(path, V, cells, ctype, vatt=None, catt=None):
    with open(path, "w") as f:
        f.write("%d %d %d %d 0\n" % (len(V), len(cells), 1 if vatt else 0, 1 if catt else 0))
        for i, p in enumerate(V):
            f.write("%d %.15e %.15e %.15e\n" % ((i + 1,) + tuple(p)))
        for i, c in enumerate(cells):
            f.write("%d 1 %s %s\n" % (i + 1, ctype, " ".join(str(k + 1) for k in c)))
        for att in (vatt, catt):
            if att:
                name, vals = att
                f.write("1 1\n%s, real\n" % name)
                for i, v in enumerate(vals):
                    f.write("%d %.15e\n" % (i + 1, v))


def grid(n, z=0.0, x0=0.0, x1=1.0):
    h = (x1 - x0) / (n - 1)
    return [(x0 + i * h, x0 + j * h, z) for j in range(n) for i in range(n)]


def main(d):
    write(os.path.join(d, "grid_pts.inp"), grid(101), [], "")
    n = 61
    V = grid(n)
    Q = [[i + n * j, i + 1 + n * j, i + 1 + n * (j + 1), i + n * (j + 1)]
         for j in range(n - 1) for i in range(n - 1)]
    write(os.path.join(d, "grid_quads.inp"), V, Q, "quad")
    r = random.Random(5)
    P = [(r.uniform(.04, .96), r.uniform(.04, .96), 0.0) for _ in range(25)]
    write(os.path.join(d, "vor_src.inp"), P, [], "", ("vid", [float(i + 1) for i in range(25)]))
    m = 7
    TV = grid(m)
    T = []
    for j in range(m - 1):
        for i in range(m - 1):
            a, b, c, e = i + m * j, i + 1 + m * j, i + 1 + m * (j + 1), i + m * (j + 1)
            T += [[a, b, c], [a, c, e]] if (i + j) % 2 else [[a, b, e], [b, c, e]]
    write(os.path.join(d, "tri_src.inp"), TV, T, "tri", ("fbump", [bump(x, y) for x, y, _ in TV]))
    # turned quad grid: 6 x 6 vertices over [-0.35, 1.35]^2, turned 25
    # degrees about the centre
    k = 6
    ang = math.radians(25)
    G = grid(k, 0.0, -.35, 1.35)
    QV = [(.5 + (x - .5) * math.cos(ang) - (y - .5) * math.sin(ang),
           .5 + (x - .5) * math.sin(ang) + (y - .5) * math.cos(ang), 0.0) for x, y, _ in G]
    QQ, val = [], []
    for j in range(k - 1):
        for i in range(k - 1):
            QQ.append([i + k * j, i + 1 + k * j, i + 1 + k * (j + 1), i + k * (j + 1)])
            val.append(((i + j) % 2) * 2 + .25 * i)
    write(os.path.join(d, "quad_src.inp"), QV, QQ, "quad", None, ("check", val))
    C = [(t, .5 + .3 * math.sin(2 * math.pi * t), 0.0) for t in (i / 59 for i in range(60))]
    C += [(.72 + .12 * math.cos(a), .3 + .12 * math.sin(a), 0.0)
          for a in (2 * math.pi * i / 60 for i in range(60))]
    write(os.path.join(d, "curve_src.inp"), C, [], "")
    # sphere: latitude-longitude triangles, outward-facing
    nl, nm, R, c0 = 16, 32, .3, (.5, .5, .5)
    SV = [(c0[0], c0[1], c0[2] + R)]
    for a in range(1, nl):
        th = math.pi * a / nl
        for b in range(nm):
            ph = 2 * math.pi * b / nm
            SV.append((c0[0] + R * math.sin(th) * math.cos(ph),
                       c0[1] + R * math.sin(th) * math.sin(ph), c0[2] + R * math.cos(th)))
    SV.append((c0[0], c0[1], c0[2] - R))
    ring = lambda a, b: 1 + (a - 1) * nm + (b % nm)
    ST = [[0, ring(1, b), ring(1, b + 1)] for b in range(nm)]
    for a in range(1, nl - 1):
        for b in range(nm):
            p, q, s_, t = ring(a, b), ring(a, b + 1), ring(a + 1, b + 1), ring(a + 1, b)
            ST += [[p, t, s_], [p, s_, q]]
    last = len(SV) - 1
    ST += [[last, ring(nl - 1, b + 1), ring(nl - 1, b)] for b in range(nm)]
    write(os.path.join(d, "sphere_tri.inp"), SV, ST, "tri")
    write(os.path.join(d, "slice_pts.inp"), grid(101, .5), [], "")


if __name__ == "__main__":
    main(sys.argv[1] if len(sys.argv) > 1 else ".")
