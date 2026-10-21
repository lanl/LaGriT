#!/usr/bin/env python3
"""
make_polygons.py - writes the test polygons for the triangulate suite.

    python3 make_polygons.py            write every polygon in RECIPES
    python3 make_polygons.py sq_k10001  write just one (a RECIPES name)

Each polygon is written as <name>.poly, an AVS UCD vertex set (no cells)
that LaGriT reads with read/avs, in the order triangulate will use it.
Coordinates are written with 17 significant digits, so the polygon is
exactly the one described here. A <name>.json sidecar records the
recipe, the vertex count, the bounding box and the orientation.

Every recipe is a function of plain numbers, so a polygon can be
rebuilt, changed or added by editing RECIPES below.

Terms: a polygon has VERTICES; triangulate makes triangle CELLS.
"""
import json
import math
import sys


# ----------------------------------------------------------------------
# shapes, all built counterclockwise
# ----------------------------------------------------------------------

def subdivide(corners, ks):
    """corners in counterclockwise order; ks = segments per side (one
    number for every side, or a list with one per side)"""
    n = len(corners)
    if isinstance(ks, int):
        ks = [ks] * n
    xs, ys = [], []
    for i in range(n):
        (x0, y0), (x1, y1) = corners[i], corners[(i + 1) % n]
        for j in range(ks[i]):
            t = j / ks[i]
            xs.append(x0 + t * (x1 - x0))
            ys.append(y0 + t * (y1 - y0))
    return xs, ys


SQUARE = [(0, 0), (1, 0), (1, 1), (0, 1)]
TRIANGLE = [(0, 0), (1, 0), (0.5, 1)]
RIGHT_TRIANGLE = [(0, 0), (1, 0), (0, 1)]
L_SHAPE = [(0, 0), (2, 0), (2, 1), (1, 1), (1, 2), (0, 2)]
U_SHAPE = [(0, 0), (3, 0), (3, 2), (2, 2), (2, 1), (1, 1), (1, 2), (0, 2)]


def square(k, side=1.0, x0=0.0, y0=0.0, start=0):
    xs, ys = subdivide(SQUARE, k)
    xs = [x0 + side * x for x in xs]
    ys = [y0 + side * y for y in ys]
    return xs[start:] + xs[:start], ys[start:] + ys[:start]


def shape(corners, k, start=0):
    xs, ys = subdivide(corners, k)
    return xs[start:] + xs[:start], ys[start:] + ys[:start]


def one_side(corners, side, k, start=0):
    ks = [1] * len(corners)
    ks[side] = k
    return shape_ks(corners, ks, start)


def shape_ks(corners, ks, start=0):
    xs, ys = subdivide(corners, ks)
    return xs[start:] + xs[:start], ys[start:] + ys[:start]


def slot(gap, k_bottom=1, half_width=0.05):
    """unit square with a slot cut down from the top edge; the slot's
    sharp tip stops `gap` above the bottom edge, which is topologically
    the far side of the polygon"""
    w = half_width
    corners = [(0, 0), (1, 0), (1, 1), (0.5 + w, 1), (0.5, gap),
               (0.5 - w, 1), (0, 1)]
    return shape_ks(corners, [k_bottom, 1, 1, 1, 1, 1, 1])


def circle(n, radius=1.0):
    return ([radius * math.cos(2 * math.pi * j / n) for j in range(n)],
            [radius * math.sin(2 * math.pi * j / n) for j in range(n)])


def reverse(p):
    xs, ys = p
    return xs[::-1], ys[::-1]


def close_with_repeat(p):
    xs, ys = p
    return xs + [xs[0]], ys + [ys[0]]


# ----------------------------------------------------------------------
# stress shapes (all counterclockwise). A polygon with holes is ONE
# vertex list joined into a "keyhole": a zero-width slit from the
# outer boundary to each hole, travelled both ways, so the slit's two
# end vertices each appear twice. triangulate takes nothing else.
# ----------------------------------------------------------------------

def area2(P):
    n = len(P)
    return sum(P[i][0] * P[(i + 1) % n][1] - P[(i + 1) % n][0] * P[i][1]
               for i in range(n))


def ccw(P):
    return P if area2(P) > 0 else P[::-1]


def cw(P):
    return P[::-1] if area2(P) > 0 else P


# ---------------------------------------------------------------------
def comb(n_teeth, tooth_w=0.2, gap_w=0.2, depth=4.0, base=1.0):
    """base rectangle with n_teeth narrow teeth sticking up"""
    W = n_teeth * tooth_w + (n_teeth - 1) * gap_w
    P = [(0.0, 0.0), (W, 0.0)]
    x = W
    for t in range(n_teeth):           # right to left along the top
        P.append((x, base + depth))
        P.append((x - tooth_w, base + depth))
        x -= tooth_w
        if t < n_teeth - 1:
            P.append((x, base))
            P.append((x - gap_w, base))
            x -= gap_w
    # last appended is (0, base+depth); polygon closes to (0,0)
    return P


def zigzag_band(n, dx=0.1, amp=0.5, thick=0.2):
    """a thin band whose top and bottom are both zig-zags"""
    bot = [(i * dx, amp * (i % 2)) for i in range(n + 1)]
    top = [(x, y + thick) for x, y in bot]
    return bot + top[::-1]


def thick_path(pts, w):
    """polygon around a polyline of half width w (miter joins)"""
    def offset(side):
        out = []
        n = len(pts)
        for i in range(n):
            if i == 0:
                d0 = d1 = (pts[1][0] - pts[0][0], pts[1][1] - pts[0][1])
            elif i == n - 1:
                d0 = d1 = (pts[i][0] - pts[i - 1][0], pts[i][1] - pts[i - 1][1])
            else:
                d0 = (pts[i][0] - pts[i - 1][0], pts[i][1] - pts[i - 1][1])
                d1 = (pts[i + 1][0] - pts[i][0], pts[i + 1][1] - pts[i][1])
            def unit(d):
                L = math.hypot(*d); return (d[0] / L, d[1] / L)
            a, b = unit(d0), unit(d1)
            na, nb = (-a[1] * side, a[0] * side), (-b[1] * side, b[0] * side)
            m = (na[0] + nb[0], na[1] + nb[1])
            mL = math.hypot(*m)
            m = (m[0] / mL, m[1] / mL)
            cosh = m[0] * na[0] + m[1] * na[1]
            s = w / cosh
            out.append((pts[i][0] + m[0] * s, pts[i][1] + m[1] * s))
        return out
    left = offset(+1)
    right = offset(-1)
    return ccw(right + left[::-1])


def square_spiral(turns, corridor=1.0, wall=0.3):
    """rectilinear spiral wall: long chains of reflex corners"""
    step = corridor + wall
    pts = [(0.0, 0.0)]
    x = y = 0.0
    dirs = [(1, 0), (0, 1), (-1, 0), (0, -1)]
    L = step
    k = 0
    for seg in range(4 * turns):
        d = dirs[seg % 4]
        x += d[0] * L
        y += d[1] * L
        pts.append((x, y))
        if seg % 2 == 1:
            L += step
        k += 1
    return thick_path(pts, wall / 2)


def smooth_spiral(turns, n_per_turn=100, a=0.5, pitch=1.0, wall=0.3):
    """Archimedean spiral band, r = a + pitch*theta/(2 pi)"""
    pts = []
    N = turns * n_per_turn
    for j in range(N + 1):
        th = 2 * math.pi * j / n_per_turn
        r = a + pitch * th / (2 * math.pi)
        pts.append((r * math.cos(th), r * math.sin(th)))
    return thick_path(pts, wall / 2)


def circle_pts(n, R=1.0, cx=0.0, cy=0.0, phase=0.0):
    return [(cx + R * math.cos(2 * math.pi * j / n + phase),
             cy + R * math.sin(2 * math.pi * j / n + phase)) for j in range(n)]


def rect(x0, y0, x1, y1):
    return [(x0, y0), (x1, y0), (x1, y1), (x0, y1)]


def _seg_cross(p, q, a, b):
    def o(u, v, w):
        return (v[0] - u[0]) * (w[1] - u[1]) - (w[0] - u[0]) * (v[1] - u[1])
    if p in (a, b) or q in (a, b):
        return False
    d1, d2, d3, d4 = o(a, b, p), o(a, b, q), o(p, q, a), o(p, q, b)
    return (d1 > 0) != (d2 > 0) and (d3 > 0) != (d4 > 0)


def keyhole(outer, holes):
    """join holes into the outer ring with zero-width slits; outer is
    made counterclockwise, every hole clockwise"""
    ring = list(ccw(outer))
    holes = [list(cw(h)) for h in holes]
    for h in holes:
        others = [hh for hh in holes if hh is not h]
        best = None
        for i, p in enumerate(ring):
            for j, q in enumerate(h):
                d = math.dist(p, q)
                if best and d >= best[0]:
                    continue
                segs = [(ring[k], ring[(k + 1) % len(ring)]) for k in range(len(ring))]
                segs += [(h[k], h[(k + 1) % len(h)]) for k in range(len(h))]
                for hh in others:
                    segs += [(hh[k], hh[(k + 1) % len(hh)]) for k in range(len(hh))]
                if any(_seg_cross(p, q, a, b) for a, b in segs):
                    continue
                best = (d, i, j)
        _, i, j = best
        loop = h[j:] + h[:j] + [h[j]]          # around the hole, back to start
        ring = ring[:i + 1] + loop + [ring[i]] + ring[i + 1:]
    return ring


def sliver(k, length=1.0, thick=1e-6):
    """a rectangle 1e6 times longer than thick, k segments on long sides"""
    bot = [(length * j / k, 0.0) for j in range(k + 1)]
    top = [(length * j / k, thick) for j in range(k, -1, -1)]
    return bot + top


def starburst(n_spikes, R=1.0, r=0.01):
    P = []
    for j in range(2 * n_spikes):
        rr = R if j % 2 == 0 else r
        th = math.pi * j / n_spikes
        P.append((rr * math.cos(th), rr * math.sin(th)))
    return P


# ----------------------------------------------------------------------
# block letters spelling LaGriT (1 unit grid; capitals 5 high, lower
# case 3.5 high; 1 unit gaps). The "a" has a hole (its counter), joined
# by a keyhole slit; the "i" is two polygons (stem and dot).
# ----------------------------------------------------------------------

LETTERS = {
    "L":     [(0, 0), (3, 0), (3, 1), (1, 1), (1, 5), (0, 5)],
    "a":     keyhole([(4, 0), (7, 0), (7, 3.5), (4, 3.5), (4, 2.5),
                      (6, 2.5), (6, 2.0), (4, 2.0)],
                     [rect(5, 0.7, 6, 1.3)]),
    "G":     [(8, 0), (11, 0), (11, 2.5), (9.5, 2.5), (9.5, 1.75),
              (10, 1.75), (10, 1), (9, 1), (9, 4), (11, 4), (11, 5),
              (8, 5)],
    "r":     [(12, 0), (13, 0), (13, 2.5), (15, 2.5), (15, 3.5),
              (12, 3.5)],
    "i_stem": rect(16, 0, 17, 3.5),
    "i_dot":  rect(16, 4.2, 17, 5),
    "T":     [(19, 0), (20, 0), (20, 4), (21, 4), (21, 5), (18, 5),
              (18, 4), (19, 4)],
}


def letter(name):
    P = ccw([(float(x), float(y)) for x, y in LETTERS[name]])
    return [p[0] for p in P], [p[1] for p in P]


def pts(P):
    return [p[0] for p in P], [p[1] for p in P]


# ----------------------------------------------------------------------
# one entry per test polygon: name -> (description, builder)
# ----------------------------------------------------------------------

RECIPES = {
    "sq_k1":      ("unit square, 4 vertices",
                   lambda: square(1)),
    "sq_k2":      ("unit square, 2 segments per side",
                   lambda: square(2)),
    "sq_k3":      ("unit square, 3 segments per side",
                   lambda: square(3)),
    "sq_k5":      ("unit square, 5 segments per side",
                   lambda: square(5)),
    "sq_k10":     ("unit square, 10 segments per side",
                   lambda: square(10)),
    "sq_k100":    ("unit square, 100 segments per side",
                   lambda: square(100)),
    "sq_k1000":   ("unit square, 1000 segments per side",
                   lambda: square(1000)),
    "sq_k10001":  ("unit square, 10001 segments per side (40004 vertices)",
                   lambda: square(10001)),
    "sq_k10_mid": ("unit square, 10 segments per side, list starts mid side",
                   lambda: square(10, start=5)),
    "rtri_plus1": ("right triangle plus one vertex on its vertical leg "
                   "(4 vertices, the smallest bad case)",
                   lambda: one_side(RIGHT_TRIANGLE, 2, 2)),
    "tri_k5_s9":  ("triangle, 5 segments per side, list starts at vertex 10",
                   lambda: shape(TRIANGLE, 5, start=9)),
    "L_k20":      ("L shape, 20 segments per side",
                   lambda: shape(L_SHAPE, 20)),
    "U_k20":      ("U shape, 20 segments per side",
                   lambda: shape(U_SHAPE, 20)),
    "slot_1e-2":  ("slot whose tip stops 1e-2 above the far wall",
                   lambda: slot(1e-2)),
    "slot_1e-9":  ("slot whose tip stops 1e-9 above the far wall",
                   lambda: slot(1e-9)),
    "slot_1e-8_k10": ("slot, tip 1e-8 above the far wall, far wall split "
                      "into 10 segments",
                      lambda: slot(1e-8, k_bottom=10)),
    "circ_2917":  ("circle radius 1, 2917 vertices",
                   lambda: circle(2917)),
    "circ_r0.01_1000": ("circle radius 0.01, 1000 vertices",
                        lambda: circle(1000, 0.01)),
    "sq_small":   ("square of side 0.01, 101 segments per side",
                   lambda: square(101, side=0.01)),
    "sq_utm":     ("square of side 1000 at x=500000, y=4000000, "
                   "50 segments per side",
                   lambda: square(50, side=1000.0, x0=5.0e5, y0=4.0e6)),
    "sq_cw":      ("unit square, 5 segments per side, CLOCKWISE",
                   lambda: reverse(square(5))),
    "sq_repeat":  ("unit square, 4 vertices, first vertex repeated at "
                   "the end",
                   lambda: close_with_repeat(square(1))),
    "tri_3":      ("triangle, 3 vertices",
                   lambda: shape(TRIANGLE, 1)),
    "issue239":   ("GitHub issue 239, perimeter_bug.inp: square with a "
                   "notch whose tip (1,1) lies ON the diagonal (0,0)-(2,2); "
                   "clockwise",
                   lambda: ([0.0, 0.0, 2.0, 2.0, 1.0],
                            [0.0, 2.0, 2.0, 0.0, 1.0])),
    "issue239_works": ("GitHub issue 239, perimeter_works.inp: the same, "
                       "tip at (1, 1.000001); clockwise",
                       lambda: ([0.0, 0.0, 2.0, 2.0, 1.0],
                                [0.0, 2.0, 2.0, 0.0, 1.000001])),
    "issue239_near": ("issue 239 shape with the tip 1e-11 above the "
                      "diagonal; clockwise",
                      lambda: ([0.0, 0.0, 2.0, 2.0, 1.0],
                               [0.0, 2.0, 2.0, 0.0, 1.0 + 1e-11])),
    "comb_200":   ("comb: 200 narrow teeth, 4 deep (800 vertices)",
                   lambda: pts(comb(200))),
    "comb_1000":  ("comb: 1000 narrow teeth (4000 vertices)",
                   lambda: pts(comb(1000))),
    "zigzag_500": ("thin band with zig-zag top and bottom, 500 zigs "
                   "(1002 vertices): the slowest shape per vertex",
                   lambda: pts(zigzag_band(500))),
    "spiral_sq25": ("square spiral wall, 25 turns (202 vertices)",
                    lambda: pts(square_spiral(25))),
    "spiral_10x200": ("smooth spiral band, 10 turns x 200 vertices "
                      "(4002 vertices)",
                      lambda: pts(smooth_spiral(10, 200))),
    "hole_1":     ("square with one square hole (keyhole)",
                   lambda: pts(keyhole(rect(0, 0, 4, 4),
                                       [rect(1.5, 1.5, 2.5, 2.5)]))),
    "hole_3":     ("rectangle with three holes: two squares and a "
                   "24-vertex circle (keyhole)",
                   lambda: pts(keyhole(rect(0, 0, 10, 4),
                                       [rect(1, 1, 2, 3),
                                        circle_pts(24, 1, 5, 2),
                                        rect(8, 1, 9, 3)]))),
    "annulus_64": ("annulus: 64-vertex circle radius 2, 32-vertex hole "
                   "radius 1 (keyhole)",
                   lambda: pts(keyhole(circle_pts(64, 2.0),
                                       [circle_pts(32, 1.0)]))),
    "annulus_thin": ("thin annulus: radii 1 and 0.999, 1000 vertices "
                     "each (keyhole)",
                     lambda: pts(keyhole(circle_pts(1000, 1.0),
                                         [circle_pts(1000, 0.999)]))),
    "sliver_1000": ("sliver: 1 long, 1e-6 thick, 1000 segments on each "
                    "long side",
                    lambda: pts(sliver(1000))),
    "star_50":    ("starburst: 50 spikes, inner radius 0.01",
                   lambda: pts(starburst(50))),
    "star_500":   ("starburst: 500 spikes, inner radius 0.001",
                   lambda: pts(starburst(500, r=1e-3))),
    "letter_L":   ("letter L", lambda: letter("L")),
    "letter_a":   ("letter a, with its hole joined by a keyhole slit",
                   lambda: letter("a")),
    "letter_G":   ("letter G", lambda: letter("G")),
    "letter_r":   ("letter r", lambda: letter("r")),
    "letter_i_stem": ("letter i, the stem", lambda: letter("i_stem")),
    "letter_i_dot": ("letter i, the dot", lambda: letter("i_dot")),
    "letter_T":   ("letter T", lambda: letter("T")),
    "two_vertex": ("2 vertices: not a polygon",
                   lambda: ([0.0, 1.0], [0.0, 0.0])),
}


def signed_area2(xs, ys):
    n = len(xs)
    return sum(xs[i] * ys[(i + 1) % n] - xs[(i + 1) % n] * ys[i]
               for i in range(n))


def write(name):
    desc, build = RECIPES[name]
    xs, ys = build()
    with open(name + ".poly", "w") as f:
        f.write("%d 0 0 0 0\n" % len(xs))
        for i, (x, y) in enumerate(zip(xs, ys)):
            f.write("%d %.17e %.17e %.17e\n" % (i + 1, x, y, 0.0))
    a2 = signed_area2(xs, ys) if len(xs) >= 3 else 0.0
    meta = {"name": name, "description": desc, "vertices": len(xs),
            "xmin": min(xs), "xmax": max(xs), "ymin": min(ys),
            "ymax": max(ys),
            "orientation": ("counterclockwise" if a2 > 0 else
                            "clockwise" if a2 < 0 else "none"),
            "area": abs(a2) / 2.0}
    with open(name + ".json", "w") as f:
        json.dump(meta, f, indent=1)
        f.write("\n")
    return meta


def main(argv):
    names = argv[1:] or list(RECIPES)
    for n in names:
        if n not in RECIPES:
            sys.exit("no recipe " + n)
        m = write(n)
        print("%-16s %6d vertices  %-16s %s" % (n, m["vertices"],
                                                 m["orientation"],
                                                 m["description"]))


if __name__ == "__main__":
    main(sys.argv)
