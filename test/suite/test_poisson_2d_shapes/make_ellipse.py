#!/usr/bin/env python3
"""
Generate a piecewise linear ellipse (or circle) as an AVS UCD point set.

    python3 make_ellipse.py --out circle_r1       --a 1 --b 1     --h 0.025
    python3 make_ellipse.py --out ellipse_2to1    --a 1 --b 0.5   --h 0.025
    python3 make_ellipse.py --out ellipse_6to1    --a 1 --b 0.16666666667 \\
                            --h 0.025

Writes <out>.poly and a <out>.json sidecar recording the parameters and
the limits the driver will check.

    x = a cos(t),  y = b sin(t),  centred at the origin, counter-clockwise
    as the driver requires.

VERTEX SPACING

Vertices are spaced EVENLY ALONG THE ARC, not evenly in the angle t. For
a 6:1 ellipse, equal steps in t crowd vertices at the ends of the major
axis and spread them out along the flat sides, by about the axis ratio.
Equal arc length gives every edge the same length.

The edge length is chosen as close as possible to --spacing times h
(default 1.0) while dividing the perimeter evenly. Edges about h long
match the spacing the sampler places along the boundary.

COORDINATES are written to 9 decimals. Near the flat sides of a 6:1
ellipse, consecutive edges turn by very little; 6 decimals of rounding
would come within about a factor of ten of that turn, enough to risk a
vertex coming out reflex and the polygon being reported nonconvex.
"""
import argparse
import json
import math

ANGLE_MIN_DEG = 9.0        # POI_ANGLE_MIN_DEG in poi_params.h
EDGE_MIN_FACTOR = 0.5      # POI_EDGE_MIN_FACTOR in poi_params.h


def arc_table(a, b, n=200000):
    """Cumulative arc length at n+1 equally spaced t in [0, 2 pi]."""
    t = [2.0 * math.pi * i / n for i in range(n + 1)]
    s = [0.0]
    for i in range(n):
        x0, y0 = a * math.cos(t[i]), b * math.sin(t[i])
        x1, y1 = a * math.cos(t[i + 1]), b * math.sin(t[i + 1])
        s.append(s[-1] + math.hypot(x1 - x0, y1 - y0))
    return t, s


def build(a, b, h, spacing):
    t, s = arc_table(a, b)
    perim = s[-1]
    nv = max(8, int(round(perim / (spacing * h))))
    pts = []
    k = 0
    for i in range(nv):
        target = perim * i / nv
        while s[k + 1] < target:
            k += 1
        f = (target - s[k]) / (s[k + 1] - s[k])
        tt = t[k] + f * (t[k + 1] - t[k])
        pts.append((a * math.cos(tt), b * math.sin(tt)))
    return pts, perim


def limits(pts):
    """Smallest vertex angle (as the driver measures it), shortest edge,
    and whether every turn is to the left (convex, CCW)."""
    n = len(pts)
    amin, emin, convex = 180.0, float('inf'), True
    for i in range(n):
        p, q, r = pts[i - 1], pts[i], pts[(i + 1) % n]
        v1 = (p[0] - q[0], p[1] - q[1])
        v2 = (r[0] - q[0], r[1] - q[1])
        c = (v1[0]*v2[0] + v1[1]*v2[1]) / (math.hypot(*v1) *
                                          math.hypot(*v2))
        amin = min(amin, math.degrees(math.acos(max(-1.0, min(1.0, c)))))
        emin = min(emin, math.hypot(*v2))
        turn = (q[0]-p[0])*(r[1]-q[1]) - (q[1]-p[1])*(r[0]-q[0])
        if turn <= 0.0:
            convex = False
    return amin, emin, convex


def write_avs(fn, pts):
    with open(fn, 'w') as f:
        f.write(f"{len(pts)} 0 0 0 0 \n")
        for i, (x, y) in enumerate(pts, 1):
            f.write(f"{i} {x:.9f} {y:.9f} 0.0\n")


def read_back(fn):
    with open(fn) as f:
        return [tuple(map(float, l.split()[1:3]))
                for l in f.readlines()[1:]]


if __name__ == '__main__':
    ap = argparse.ArgumentParser()
    ap.add_argument('--out', required=True)
    ap.add_argument('--a', type=float, required=True,
                    help='semi-axis along x')
    ap.add_argument('--b', type=float, required=True,
                    help='semi-axis along y')
    ap.add_argument('--h', type=float, required=True,
                    help='target sample spacing the polygon is meant for')
    ap.add_argument('--spacing', type=float, default=1.0,
                    help='edge length as a multiple of h (default 1)')
    a = ap.parse_args()

    pts, perim = build(a.a, a.b, a.h, a.spacing)
    write_avs(a.out + '.poly', pts)

    # Limits are measured on the WRITTEN coordinates, after rounding,
    # because that is what the driver reads.
    amin, emin, convex = limits(read_back(a.out + '.poly'))
    area = math.pi * a.a * a.b
    meta = dict(vertices=len(pts), a=a.a, b=a.b, ratio=a.a / a.b,
                h=a.h, spacing=a.spacing, perimeter=perim,
                edge=perim / len(pts), ellipse_area=area,
                min_angle_deg=amin, min_edge=emin,
                h_max=emin / EDGE_MIN_FACTOR, convex=convex,
                accepted_by_angle=(amin >= ANGLE_MIN_DEG),
                accepted_by_edge=(a.h <= emin / EDGE_MIN_FACTOR))
    with open(a.out + '.json', 'w') as f:
        json.dump(meta, f, indent=1)
    print(f"{a.out + '.poly':20} {len(pts):5} verts  ratio "
          f"{a.a / a.b:5.2f}  edge {perim / len(pts):.5f}  "
          f"min angle {amin:7.2f}  h_max {emin / EDGE_MIN_FACTOR:.4f}  "
          f"convex={convex}")
