#!/usr/bin/env python3
"""make_connect_exact.py WORKDIR

Writes the point sets and decks of test_connect_exact (Python 3,
standard library only) and WORKDIR/expect.json. Every point set is
written with 17 significant digits, so the checker tests the mesh
against exactly the coordinates LaGriT read. Each point set is
connected twice: with plain connect (the tolerance tests) and with
connect / exact.

  g0    12 x 12 x 12 regular grid (exactly cospherical: ties)
  j12   the grid jittered by up to 1e-12
  j9    the grid jittered by up to 1e-9
  j6a   the grid jittered by up to 1e-6 (two seeds)
  j6b
  j3    the grid jittered by up to 1e-3
  rnd   3000 random points in the unit cube
  thin  3000 random points in a 100 x 1 x 1 box
  gi    12 x 12 x 12 lattice, spacing 10, at x = 400000, y = 3800000
        (UTM-like): exact ties decided by the integer stage
  gij   the same lattice moved by multiples of 1/1024 (up to 3/1024):
        near-ties the integer stage decides exactly
  sph   an r-theta-phi sphere (5 radii, 11.25 degree steps; the rtp
        points of createpts, duplicates removed): near-degenerate in
        floating point (points meant to be coplanar and cospherical)
"""
import json
import math
import os
import random
import sys

ABOUT = {
 "g0": "A regular grid: every cube's 8 corners are exactly cospherical, so many tetrahedralizations are Delaunay.",
 "j12": "The grid moved by up to 1e-12: the ties are broken, and there is exactly one Delaunay tetrahedralization, full of thin tets.",
 "j9": "The grid moved by up to 1e-9. The tolerance tests see ties; V3.3.4 made no tets at all.",
 "j6a": "The grid moved by up to 1e-6 (seed 2). The tolerance tests dud some real vertices.",
 "j6b": "The grid moved by up to 1e-6 (seed 5).",
 "j3": "The grid moved by up to 1e-3: far from ties; both tests agree.",
 "rnd": "3000 random points: no near-ties; both tests agree.",
 "thin": "3000 random points in a 100 x 1 x 1 box.",
 "gi": "A lattice with spacing 10 m at UTM-sized coordinates: the coordinate differences are small multiples of a power of two, so exact ties are decided by the fast integer stage.",
 "gij": "The spacing-10 lattice moved by multiples of 1/1024 m: near-ties, still decided exactly by the integer stage.",
 "sph": "The r-theta-phi sphere of createpts: points meant to be coplanar and cospherical, but off by round-off from sin and cos. "
        "The exact Delaunay tetrahedralization of these coordinates has tets that are flat to round-off (valid exactly, zero "
        "or negative volume in floating point): use plain connect for such input.",
}


def grid(n, jit, seed):
    rng = random.Random(seed)
    return [(i / (n - 1) + rng.uniform(-jit, jit), j / (n - 1) + rng.uniform(-jit, jit),
             k / (n - 1) + rng.uniform(-jit, jit)) for i in range(n) for j in range(n) for k in range(n)]


def sphere():
    pts, seen = [], set()
    for i in range(5):
        r = 0.5 * i
        for j in range(17):
            th = math.radians(11.25 * j)
            for k in range(33):
                ph = math.radians(11.25 * k)
                p = (r * math.sin(th) * math.cos(ph), r * math.sin(th) * math.sin(ph), r * math.cos(th))
                key = tuple(round(c, 9) for c in p)
                if key not in seen:
                    seen.add(key)
                    pts.append(p)
    return pts


def main(work):
    os.makedirs(work, exist_ok=True)
    rng = random.Random(9)
    sets = {
        "g0": grid(12, 0.0, 1), "j12": grid(12, 1e-12, 5), "j9": grid(12, 1e-9, 5),
        "j6a": grid(12, 1e-6, 2), "j6b": grid(12, 1e-6, 5), "j3": grid(12, 1e-3, 5),
        "rnd": [(rng.random(), rng.random(), rng.random()) for _ in range(3000)],
        "thin": [(100 * rng.random(), rng.random(), rng.random()) for _ in range(3000)],
        "gi": [(400000 + 10.0 * i, 3800000 + 10.0 * j, 1000 + 10.0 * k)
               for i in range(12) for j in range(12) for k in range(12)],
        "gij": [(400000 + 10.0 * i + rng.randint(-3, 3) / 1024.0, 3800000 + 10.0 * j + rng.randint(-3, 3) / 1024.0,
                 1000 + 10.0 * k + rng.randint(-3, 3) / 1024.0)
                for i in range(12) for j in range(12) for k in range(12)],
        "sph": sphere(),
    }
    exp = {}
    for i, (name, pts) in enumerate(sets.items()):
        with open(os.path.join(work, "pts_%s.inp" % name), "w") as f:
            f.write("%d 0 0 0 0\n" % len(pts))
            for k, p in enumerate(pts):
                f.write("%d %.17g %.17g %.17g\n" % (k + 1, *p))
        for mode, opt in (("tol", ""), ("exact", " / exact")):
            deck = "ce%02d%s_%s_%s" % (i + 1, "a" if mode == "tol" else "b", name, mode)
            with open(os.path.join(work, deck + ".lgi"), "w") as f:
                f.write("* test_connect_exact %s\n" % deck)
                f.write("read / avs / pts_%s.inp / mo\n" % name)
                f.write("cmo / setatt / mo / imt / 1 0 0 / 1\ncmo / setatt / mo / itp / 1 0 0 / 0\n")
                f.write("connect%s\n" % opt)
                f.write("dump / avs / out_%s.inp / mo / 1 1 0 0\nfinish\n" % deck)
            exp[deck] = {"set": name, "mode": mode, "n": len(pts), "about": ABOUT[name],
                         "flat_known": name == "sph" and mode == "exact"}
    json.dump(exp, open(os.path.join(work, "expect.json"), "w"), indent=1)
    print("wrote %d decks to %s" % (len(exp), work))


if __name__ == "__main__":
    if len(sys.argv) != 2:
        sys.exit(__doc__)
    main(sys.argv[1])
