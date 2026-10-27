#!/usr/bin/env python3
"""make_angle_inputs.py WORKDIR

Writes the meshes test_element_angles measures (Python 3, standard
library only). Every tet has its own four vertices, so each is measured
on its own.

  ea_tets.inp    213 tets: regular, right-angle corner, needle, cap,
                 wedge, sliver, nearly flat, three exactly flat, 200
                 random (about half of them inverted), and one regular
                 tet with two vertices swapped (inverted)
  ea_thin.inp    24 slivers of height h = 1e-4 ... 1e-14 (edge length
                 about 1), turned and sheared so no face lies in a
                 coordinate plane: the angles are 0, 180 and near 0
  ea_solid.inp   one hex (unit cube), one prism (right triangle x 1),
                 one pyramid (unit square base, apex 0.5 above the
                 centre); AVS order (pyramid apex first)
  ea_2d.inp      one triangle (0,0) (2,0) (0,1); one quad (0,0) (2,0)
                 (1,1) (0,1)
  ea_tets.names, ea_thin.names: one line per tet, the case name or h
"""
import math
import os
import random
import sys


def sub(a, b):
    return [a[i] - b[i] for i in range(3)]


def det(P):
    a, b, c = sub(P[1], P[0]), sub(P[2], P[0]), sub(P[3], P[0])
    return (a[0] * (b[1] * c[2] - b[2] * c[1]) - a[1] * (b[0] * c[2] - b[2] * c[0])
            + a[2] * (b[0] * c[1] - b[1] * c[0]))


def write_tets(path, tets):
    with open(path, "w") as f:
        f.write("%d %d 0 0 0\n" % (4 * len(tets), len(tets)))
        k = 0
        for P in tets:
            for p in P:
                k += 1
                f.write("%d %.17g %.17g %.17g\n" % (k, p[0], p[1], p[2]))
        for i in range(len(tets)):
            f.write("%d 1 tet %d %d %d %d\n" % (i + 1, 4 * i + 1, 4 * i + 2, 4 * i + 3, 4 * i + 4))


def main(work):
    random.seed(3)
    T = [("regular", [(1, 1, 1), (1, -1, -1), (-1, 1, -1), (-1, -1, 1)]),
         ("corner", [(0, 0, 0), (1, 0, 0), (0, 1, 0), (0, 0, 1)]),
         ("needle", [(0, 0, 0), (0.01, 0, 0), (0.005, 0.0087, 0), (0.005, 0.0029, 1.0)]),
         ("cap", [(0, 0, 0), (1, 0, 0), (0.5, 0.866, 0), (0.5, 0.29, 0.01)]),
         ("flat_square", [(0, 0, 0), (1, 0, 0.01), (0.5, 0.5, 0), (0.5, -0.5, 0.01)]),
         ("flat_square2", [(0, 0, 0), (1, 0, 0), (0, 1, 0.02), (1, 1, 0.02)]),
         ("sliver", [(0, 0, 0), (1, 0, 0), (0.5, -0.5, 0.02), (0.5, 0.5, 0.02)]),
         ("sliver_thin", [(0, 0, 0), (1, 0, 0), (0.5, -0.5, 1e-6), (0.5, 0.5, 1e-6)]),
         ("flat_exact", [(0, 0, 0), (1, 0, 0), (0, 1, 0), (0.25, 0.25, 0)]),
         ("wedge", [(0, 0, 0), (1, 0, 0), (0.5, 0.01, 0.0), (0.5, 0.3, 0.8)]),
         ("veryflat", [(0, 0, 0), (1, 0, 0), (0, 1, 0), (0.3, 0.3, 1e-9)])]
    for k in range(200):
        T.append(("random%d" % k, [tuple(random.uniform(-1, 1) for _ in range(3))
                                   for _ in range(4)]))
    # LaGriT's positive orientation: det(v2-v1, v3-v1, v4-v1) < 0. The
    # named tets are made positive; the random ones are left as drawn.
    out = []
    for n, P in T:
        if not n.startswith("random") and det(P) > 0:
            P = [P[0], P[2], P[1], P[3]]
        out.append((n, P))
    R = out[0][1]
    out.append(("inverted", [R[0], R[2], R[1], R[3]]))
    write_tets(os.path.join(work, "ea_tets.inp"), [P for _, P in out])
    open(os.path.join(work, "ea_tets.names"), "w").write("\n".join(n for n, _ in out) + "\n")

    random.seed(1)
    rows = []
    for h in (1e-4, 1e-6, 1e-7, 1e-8, 1e-9, 1e-10, 1e-12, 1e-14):
        for _ in range(3):
            a = random.uniform(0, 6.28)
            c, s = math.cos(a), math.sin(a)
            P = [(0, 0, 0), (1, 0, 0), (0.5, -0.5, h), (0.5, 0.5, h)]
            P = [(x * c - y * s + 0.3, x * s * 0.7 + y * c, z + 0.2 * x) for x, y, z in P]
            rows.append((h, P))
    write_tets(os.path.join(work, "ea_thin.inp"), [P for _, P in rows])
    open(os.path.join(work, "ea_thin.names"), "w").write("\n".join(repr(h) for h, _ in rows) + "\n")

    with open(os.path.join(work, "ea_solid.inp"), "w") as f:
        f.write("""19 3 0 0 0
1 0 0 0
2 1 0 0
3 1 1 0
4 0 1 0
5 0 0 1
6 1 0 1
7 1 1 1
8 0 1 1
9 0 0 0
10 1 0 0
11 0 1 0
12 0 0 1
13 1 0 1
14 0 1 1
15 0 0 0
16 1 0 0
17 1 1 0
18 0 1 0
19 0.5 0.5 0.5
1 1 hex 1 2 3 4 5 6 7 8
2 1 prism 9 10 11 12 13 14
3 1 pyr 19 15 16 17 18
""")
    with open(os.path.join(work, "ea_2d.inp"), "w") as f:
        f.write("""5 2 0 0 0
1 0 0 0
2 2 0 0
3 1 1 0
4 0 1 0
5 0 0 0
1 1 tri 1 2 4
2 1 quad 1 2 3 4
""")
    return 0


if __name__ == "__main__":
    sys.exit(main(sys.argv[1]))
