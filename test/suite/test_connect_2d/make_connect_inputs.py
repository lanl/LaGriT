#!/usr/bin/env python3
"""make_connect_inputs.py

Writes the point files the test_connect_2d decks read: one set of 400
points in a 100 x 80 rectangle, placed at the origin and at offsets
(cd*.pts). The points come from a fixed linear congruential generator,
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


def main():
    d = sys.argv[1] if len(sys.argv) > 1 else "."
    pts = points()
    for name, (plane, off) in SETS.items():
        write(d + "/" + name, plane, off, pts)
    return 0


if __name__ == "__main__":
    sys.exit(main())
