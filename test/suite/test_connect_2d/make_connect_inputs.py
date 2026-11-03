#!/usr/bin/env python3
"""make_connect_inputs.py

Writes the point files the test_connect_2d decks read: one set of 400
points in a 100 x 80 rectangle, placed at the origin and at offsets
(cd01-cd06), and four harder sets in the xy plane (cd08-cd11): graded,
a 1000 x 1 grid, a grid with 1e-9 round-off, and rings of cocircular
points. The points come from a fixed linear congruential generator,
so the files are the same on every machine. Python 3, standard library.
"""
import sys

N = 400
LX, LY = 100.0, 80.0


def points():
    # Park-Miller minimal standard generator, fixed seed
    s = 20261102
    out = []
    for _ in range(N):
        s = (16807 * s) % 2147483647
        u = s / 2147483647.0
        s = (16807 * s) % 2147483647
        v = s / 2147483647.0
        out.append((u * LX, v * LY))
    return out


# name: (where the 2D coordinates go: x, y, z), offset (x, y, z)
SETS = {
    "cd01_origin":   ("xy", (0.0, 0.0, 0.0)),
    "cd02_utm":      ("xy", (4.0e5, 3.9e6, 0.0)),
    "cd03_negative": ("xy", (-6.5e5, -4.2e6, 0.0)),
    "cd04_y_only":   ("xy", (0.0, 3.9e6, 0.0)),
    "cd05_yz_plane": ("yz", (0.0, 2.0e5, 3.0e5)),
    "cd06_xz_plane": ("xz", (2.0e5, 0.0, 3.0e5)),
}


def write(name, plane, off, pts):
    with open(name + ".pts", "w") as f:
        f.write("%d 0 0 0 0\n" % len(pts))
        for i, (u, v) in enumerate(pts, 1):
            c = {"xy": (u, v, 0.0), "yz": (0.0, u, v), "xz": (u, 0.0, v)}[plane]
            x, y, z = (c[k] + off[k] for k in range(3))
            f.write("%d %.17g %.17g %.17g\n" % (i, x, y, z))


class Lcg:
    """Park-Miller minimal standard generator: u() in (0, 1)"""
    def __init__(self, seed):
        self.s = seed

    def u(self):
        self.s = (16807 * self.s) % 2147483647
        return self.s / 2147483647.0


def graded():
    """400 points crowded toward one corner: (u**3, v**3) * 100"""
    g = Lcg(7)
    return [(100.0 * g.u() ** 3, 100.0 * g.u() ** 3) for _ in range(N)]


def stretched():
    """a 31 x 31 grid 1000 long and 1 high"""
    return [(1000.0 * i / 30, 1.0 * j / 30) for i in range(31)
            for j in range(31)]


def jittered():
    """a 31 x 31 grid of the unit square, every point moved by up to
    1e-9: round-off in points meant to lie on the sides"""
    g = Lcg(11)
    return [(i / 30 + 1e-9 * (2 * g.u() - 1), j / 30 + 1e-9 * (2 * g.u() - 1))
            for i in range(31) for j in range(31)]


def rings():
    """a centre point and 10 rings of equally spaced points: many
    cocircular sets"""
    import math
    out = [(0.5, 0.5)]
    for k in range(1, 11):
        r = 0.05 * k
        n = int(2 * math.pi * r / 0.03)
        out += [(0.5 + r * math.cos(2 * math.pi * i / n),
                 0.5 + r * math.sin(2 * math.pi * i / n)) for i in range(n)]
    return out


EXTRA = {"cd08_graded": graded, "cd09_stretched": stretched,
         "cd10_jitter": jittered, "cd11_rings": rings}


def main():
    d = sys.argv[1] if len(sys.argv) > 1 else "."
    pts = points()
    for name, (plane, off) in SETS.items():
        write(d + "/" + name, plane, off, pts)
    for name, f in EXTRA.items():
        write(d + "/" + name, "xy", (0.0, 0.0, 0.0), f())
    return 0


if __name__ == "__main__":
    sys.exit(main())
