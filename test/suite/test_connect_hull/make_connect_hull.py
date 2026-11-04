#!/usr/bin/env python3
"""make_connect_hull.py WORKDIR

Writes the point sets and decks of test_connect_hull (Python 3,
standard library only) and WORKDIR/expect.json. Each point set is
connected with plain connect, connect / exact and connect / nohull;
the points are written with 17 digits so the checker sees exactly the
coordinates LaGriT read.

  rnd    2000 random points in the unit cube (1.3% of the hull lost
         before the fill)
  grad   2000 points crowded toward x = 0 (x = u**3)
  thin   2000 random points in a 100 x 1 x 1 box
  utm    2000 random points in a 100 m cube at UTM-sized x and y
  jit    a 10 x 10 x 10 grid moved by up to 1e-6 (round-off on the
         faces: flat pockets are left open by design)
  box    a 10 x 10 x 10 exact grid (convex already: nothing to add)
"""
import json
import os
import random
import sys

ABOUT = {
 "rnd": "Random points: thin tets along the hull reach the enclosing tet and were deleted with it (1.3% of the hull volume). The fill puts back the Delaunay tets of the pockets.",
 "grad": "Points crowded toward one face: pockets of very different sizes.",
 "thin": "A 100 x 1 x 1 box of random points: most of the hull is long thin faces, where most tets were lost (5% before).",
 "utm": "A 100 m cube at UTM-sized coordinates: the exact tests do not lose digits far from the origin.",
 "jit": "A grid moved by up to 1e-6: its faces are planes up to round-off, so the pockets there are flatter than 1e-6 of their size and are left open (the slivers would only harm the mesh).",
 "box": "An exact grid: its boundary is convex, so nothing is added.",
}


def main(work):
    os.makedirs(work, exist_ok=True)
    rng = random.Random(21)
    n = 10
    sets = {
        "rnd": [(rng.random(), rng.random(), rng.random()) for _ in range(2000)],
        "grad": [(rng.random() ** 3, rng.random(), rng.random()) for _ in range(2000)],
        "thin": [(100 * rng.random(), rng.random(), rng.random()) for _ in range(2000)],
        "utm": [(400000 + 100 * rng.random(), 3800000 + 100 * rng.random(), 1000 + 100 * rng.random())
                for _ in range(2000)],
        "jit": [(i / (n - 1) + rng.uniform(-1e-6, 1e-6), j / (n - 1) + rng.uniform(-1e-6, 1e-6),
                 k / (n - 1) + rng.uniform(-1e-6, 1e-6)) for i in range(n) for j in range(n) for k in range(n)],
        "box": [(i / (n - 1), j / (n - 1), k / (n - 1)) for i in range(n) for j in range(n) for k in range(n)],
    }
    exp = {}
    for i, (name, pts) in enumerate(sets.items()):
        with open(os.path.join(work, "pts_%s.inp" % name), "w") as f:
            f.write("%d 0 0 0 0\n" % len(pts))
            for k, p in enumerate(pts):
                f.write("%d %.17g %.17g %.17g\n" % (k + 1, *p))
        for j, (mode, opt) in enumerate((("tol", ""), ("exact", " / exact"), ("nohull", " / nohull"))):
            deck = "ch%02d%s_%s_%s" % (i + 1, "abc"[j], name, mode)
            with open(os.path.join(work, deck + ".lgi"), "w") as f:
                f.write("* test_connect_hull %s\n" % deck)
                f.write("read / avs / pts_%s.inp / mo\n" % name)
                f.write("cmo / setatt / mo / imt / 1 0 0 / 1\ncmo / setatt / mo / itp / 1 0 0 / 0\n")
                f.write("connect%s\n" % opt)
                f.write("dump / avs / out_%s.inp / mo / 1 1 0 0\nfinish\n" % deck)
            exp[deck] = {"set": name, "mode": mode, "n": len(pts), "about": ABOUT[name]}
    json.dump(exp, open(os.path.join(work, "expect.json"), "w"), indent=1)
    print("wrote %d decks to %s" % (len(exp), work))


if __name__ == "__main__":
    if len(sys.argv) != 2:
        sys.exit(__doc__)
    main(sys.argv[1])
