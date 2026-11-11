#!/usr/bin/env python3
"""make_stor_inputs.py WORK

Writes the vertex file of test_stor_utm's deck st04 into WORK:
st04_pts.inp, an AVS file of vertices only (no cells): the 8 corners
of the box [0,1] x [0,1] x [0,0.5], 25 random points on each of its 6
faces and 150 random points inside (fixed seed). Random points on the
faces give boundary tets whose circumcenters lie outside the box, so
dump / stor / ... / hybrid moves them (the hybrid branch is used).
Coordinates are written with 17 digits (exact).
"""
import os
import random
import sys


def main(work):
    os.makedirs(work, exist_ok=True)
    rnd = random.Random(7)
    size = (1.0, 1.0, 0.5)
    pts = [(x, y, z) for x in (0.0, 1.0) for y in (0.0, 1.0)
           for z in (0.0, 0.5)]
    for k in range(3):
        for side in (0, 1):
            for _ in range(25):
                p = [rnd.random() * size[i] for i in range(3)]
                p[k] = side * size[k]
                pts.append(tuple(p))
    for _ in range(150):
        pts.append(tuple(rnd.random() * size[i] for i in range(3)))
    with open(os.path.join(work, "st04_pts.inp"), "w") as f:
        f.write("%d 0 0 0 0\n" % len(pts))
        for i, p in enumerate(pts):
            f.write("%d %.17g %.17g %.17g\n" % ((i + 1,) + p))
    return 0


if __name__ == "__main__":
    sys.exit(main(sys.argv[1]))
