#!/usr/bin/env python3
"""check_medial_axis.py WORK LOGDIR

Checks test_medial_axis. Written independently of LaGriT's Fortran, in
plain Python (standard library):

  every ball  - its center is P + r n, with the vertex normal n
                computed here again (3D: the angle-weighted average of
                the triangles' normals; 2D: the average of the two
                segments' normals D x z), into the ball's side;
              - it is empty: no point of the surface (brute force over
                every cell) is closer to the center than r (1 - TAU);
              - it is as large as it can be: some point of the surface
                is at distance r (a bigger tangent ball would hold it),
                unless the ball has radius rmax;
              - its flag agrees with its object angle, radius and
                normal (ma_keep 1 exactly when the ball was stopped,
                its angle is at least 180 - alpha_sharp, and it has a
                normal);
              - the surface's ma_thick_* and ma_keep_* are 2 r and the
                flag of its ball;
  each deck   - the exact answers that are known (a circle, a sphere,
                slabs, a wedge, two materials), see 00README.

Large decks are checked by brute force on a sample of their balls.
Writes LOGDIR/<deck>.check, LOGDIR/table.txt, LOGDIR/summary.html and
the pictures LOGDIR/*.svg. Exit 0 if every deck passes.
"""
import html
import math
import os
import sys

TAU = 1.0 - math.cos(math.radians(10.0))    # as in medial_axis.f
TOL = 1e-9                                  # relative to the size


# ---------------------------------------------------------------- input
def read_avs(path):
    """Vertices [(x,y,z)], cells [(type, [0-based ids])], node attrs."""
    with open(path) as f:
        lines = f.read().split("\n")
    nn, nc, nnd = (int(w) for w in lines[0].split()[:3])
    xyz = [tuple(float(w) for w in lines[1 + i].split()[1:4])
           for i in range(nn)]
    cells = []
    for i in range(nc):
        w = lines[1 + nn + i].split()
        cells.append((w[2], [int(v) - 1 for v in w[3:]]))
    att = {}
    k = 1 + nn + nc
    if nnd > 0:
        nat = int(lines[k].split()[0])
        names = [lines[k + 1 + j].split(",")[0].strip() for j in range(nat)]
        cols = [[] for _ in names]
        for i in range(nn):
            w = lines[k + 1 + nat + i].split()[1:]
            for j in range(nat):
                cols[j].append(float(w[j]))
        att = dict(zip(names, cols))
    return xyz, cells, att


# ------------------------------------------------------------ geometry
def sub(a, b):
    return (a[0] - b[0], a[1] - b[1], a[2] - b[2])


def add(a, b, s=1.0):
    return (a[0] + s * b[0], a[1] + s * b[1], a[2] + s * b[2])


def dot(a, b):
    return a[0] * b[0] + a[1] * b[1] + a[2] * b[2]


def cross(a, b):
    return (a[1] * b[2] - a[2] * b[1], a[2] * b[0] - a[0] * b[2],
            a[0] * b[1] - a[1] * b[0])


def norm(a):
    return math.sqrt(dot(a, a))


def seg_d2(p, a, b):
    ab = sub(b, a)
    s = dot(ab, ab)
    t = 0.0 if s <= 0 else max(0.0, min(1.0, dot(sub(p, a), ab) / s))
    d = sub(p, add(a, ab, t))
    return dot(d, d)


class Facet:
    """A triangle (A,B,C); a segment is (A,B,B)."""
    __slots__ = ("a", "b", "c", "n", "flat", "cen", "rad")

    def __init__(self, a, b, c):
        self.a, self.b, self.c = a, b, c
        n = cross(sub(b, a), sub(c, a))
        nn = dot(n, n)
        ab, ac = sub(b, a), sub(c, a)
        self.flat = nn <= 1e-24 * dot(ab, ab) * dot(ac, ac)
        self.n = None if self.flat else tuple(x / math.sqrt(nn) for x in n)
        self.cen = tuple((a[i] + b[i] + c[i]) / 3 for i in range(3))
        self.rad = max(norm(sub(v, self.cen)) for v in (a, b, c))

    def d2(self, p):
        a, b, c = self.a, self.b, self.c
        best = min(seg_d2(p, a, b), seg_d2(p, b, c), seg_d2(p, c, a))
        if not self.flat:
            n = self.n
            h = dot(sub(p, a), n)
            q = add(p, n, -h)
            if (dot(cross(sub(b, a), sub(q, a)), n) >= 0 and
                    dot(cross(sub(c, b), sub(q, b)), n) >= 0 and
                    dot(cross(sub(a, c), sub(q, c)), n) >= 0):
                best = min(best, h * h)
        return best


def nearest(p, F):
    """Distance from p to the facets F (brute force, nearest bounding
    sphere first; the order changes, not the answer)."""
    lb = sorted((norm(sub(p, f.cen)) - f.rad, k) for k, f in enumerate(F))
    best = math.inf
    for low, k in lb:
        if low > 0 and low * low > best:
            break
        best = min(best, F[k].d2(p))
    return math.sqrt(best)


def normals(V, cells):
    """Vertex normals and the number of cells at each vertex."""
    vn = [(0.0, 0.0, 0.0)] * len(V)
    deg = [0] * len(V)
    for t, v in cells:
        if t == "line":
            a, b = v
            d = sub(V[b], V[a])
            L = norm(d)
            deg[a] += 1
            deg[b] += 1
            if L > 0:
                u = (d[1] / L, -d[0] / L, 0.0)        # D x z
                vn[a] = add(vn[a], u)
                vn[b] = add(vn[b], u)
        else:
            a, b, c = v
            n = cross(sub(V[b], V[a]), sub(V[c], V[a]))
            L = norm(n)
            for i, j, k in ((a, b, c), (b, c, a), (c, a, b)):
                deg[i] += 1
                if L == 0:
                    continue
                e1, e2 = sub(V[j], V[i]), sub(V[k], V[i])
                ca = dot(e1, e2) / (norm(e1) * norm(e2))
                w = math.acos(max(-1.0, min(1.0, ca)))
                vn[i] = add(vn[i], n, w / L)
    out = []
    for n in vn:
        L = norm(n)
        out.append(tuple(x / L for x in n) if L > 1e-12 else None)
    # junctions: 2D a vertex not on two segments; 3D a vertex on an
    # edge of three or more triangles
    jun = [False] * len(V)
    if cells and cells[0][0] == "line":
        jun = [d != 2 for d in deg]
    else:
        ec = {}
        for t, v in cells:
            for i, j in ((v[0], v[1]), (v[1], v[2]), (v[2], v[0])):
                k = (min(i, j), max(i, j))
                ec[k] = ec.get(k, 0) + 1
        for (i, j), n in ec.items():
            if n >= 3:
                jun[i] = jun[j] = True
    return out, deg, jun


def facets(V, cells):
    F = []
    for t, v in cells:
        p = [V[i] for i in v]
        F.append(Facet(p[0], p[1], p[1]) if t == "line" else
                 Facet(p[0], p[1], p[2]))
    return F


def size_of(V):
    return max(max(v[i] for v in V) - min(v[i] for v in V) for i in range(3))


# ------------------------------------------------------------- the decks
# (deck, [(surface input, axis file, alpha, rmax or None)], what)
def _one(deck, src=None, alpha=35, rmax=None):
    src = src or deck + ".inp"
    return [(src, deck.split("_")[0] + "_axis.inp", alpha, rmax)]


CASES = [
    ("ma01_circle", _one("ma01_circle"),
     "a circle of radius 1 (128 segments), side both: in, every ball at "
     "the center; out, every ball not stopped"),
    ("ma02_sphere", _one("ma02_sphere"),
     "a sphere of radius 1 (1280 triangles): every ball at the center"),
    ("ma03_slab", [("ma03_slab.inp", "ma03_axis.inp", 35, None),
                   ("ma03_slab.inp", "ma03_axis_alpha0.inp", 0, None),
                   ("ma03_slab.inp", "ma03_axis_rmax.inp", 35, 0.3)],
     "a slab 10 x 1 (2D): thickness 1 on the long faces, the corners' "
     "branches flagged; alpha_sharp 0 flags nothing; rmax 0.3"),
    ("ma04_box", _one("ma04_box"),
     "a box 10 x 10 x 1 (3D): thickness 1 on top and bottom"),
    ("ma05_sawtooth", _one("ma05_sawtooth"),
     "teeth 0.5 high under a flat top 2 (2D, the prototype's shape)"),
    ("ma06_interface", _one("ma06_interface"),
     "two materials, 1 thick under an interface and 2 above, side both"),
    ("ma07_wedge", _one("ma07_wedge"),
     "a 3D pinch-out (wedge of 11.3 degrees): thickness 2 x tan(a/2)"),
    ("ma08_dome", _one("ma08_dome"),
     "a 3D layer 1 to 1.8 thick whose top rises in a dome"),
    ("ma09_errors", _one("ma09_errors", "ma01_circle.inp"),
     "refused: a tet, quads, no cells, no such mesh object, mo_axis the "
     "same as mo_surface, a bad side; the next command works"),
    ("ma10_biglayer", _one("ma10_biglayer"),
     "timing: a layer 20 x 20 with two domes (41,600 triangles)"),
    ("ma11_ellipse", _one("ma11_ellipse"),
     "an ellipse, semi-axes 2 and 1: centers on the major axis, radii "
     "and flags against the exact ones"),
    ("ma12_ellipsoid", _one("ma12_ellipsoid"),
     "a prolate ellipsoid, semi-axes 2, 1, 1 (5120 triangles): each "
     "ball touches along a ring; radii and flags against the exact ones"),
    ("ma13_annulus", _one("ma13_annulus"),
     "an annulus (a ring, a hole in the shape), radii 1 and 2: "
     "thickness 1 everywhere"),
    ("ma14_tube", _one("ma14_tube"),
     "a pipe (3D, a hole through it), radii 1 and 1.5: thickness 0.5 "
     "in its walls"),
    ("ma15_neck", _one("ma15_neck"),
     "a dumbbell, two disks joined by a channel 0.4 wide: 0.4 in the "
     "channel, 2 in the disks, nothing leaks through the neck"),
    ("ma16_noise", [("ma16_noise_small.inp", "ma16_axis_small.inp", 35,
                     None),
                    ("ma16_noise_large.inp", "ma16_axis_large.inp", 35,
                     None)],
     "the slab with noisy faces (up to 0.002 and 0.01): kept thickness "
     "within the noise; how many balls the noise flags"),
    ("ma17_sweep",
     [("ma17_circle_%d.inp" % n, "ma17_axis_circle_%d.inp" % n, 35, None)
      for n in (16, 32, 64, 128, 256, 512)] +
     [("ma17_sphere_%d.inp" % n, "ma17_axis_sphere_%d.inp" % n, 35, None)
      for n in (1, 2, 3, 4)],
     "resolution sweep: circles of 16 to 512 segments, spheres of 80 to "
     "5120 triangles: the error falls as h^2"),
    ("ma18_utm", [("ma18_slab_utm.inp", "ma18_axis_slab.inp", 35, None),
                  ("ma18_box_utm.inp", "ma18_axis_box.inp", 35, None)],
     "the slab and the box at UTM-sized coordinates (x + 500000, "
     "y + 4000000): the same answers as ma03 and ma04"),
    ("ma19_interface3d", _one("ma19_interface3d"),
     "two materials in 3D (a sheet inside a box), side both: 1 below, 2 "
     "above; the sheet's rim (edges of 3 or more triangles) flagged"),
]
SAMPLE = {"ma02_sphere": 4, "ma07_wedge": 2, "ma08_dome": 3,
          "ma10_biglayer": 150, "ma12_ellipsoid": 8, "ma14_tube": 8,
          "ma17_sweep": 25}


def check_balls(V, cells, A, alpha, rmax, step, lines):
    """The checks of every ball (see the top). Returns ok."""
    F = facets(V, cells)
    vn, deg, jun = normals(V, cells)
    size = max(size_of(V), 1.0)
    tol = TOL * size
    if rmax is None:
        rmax = math.sqrt(sum((max(v[i] for v in V) - min(v[i] for v in V))
                             ** 2 for i in range(3)))
    xa = list(zip(A["xic"], A["yic"], A["zic"]))
    n = len(xa)
    bad = {"center": 0, "empty": 0, "touch": 0, "flag": 0}
    worst = {"center": 0.0, "empty": 0.0, "touch": 0.0}
    nchk = 0
    for k in range(n):
        i = int(A["ma_src"][k]) - 1
        side = int(A["ma_side"][k])
        r = A["ma_r"][k]
        keep = int(A["ma_keep"][k])
        ang = A["ma_angle"][k]
        nrm = vn[i]
        junction = nrm is None or jun[i]
        stopped = r < rmax * (1 - 1e-12)
        want = 1 if (not junction and stopped and
                     (alpha <= 0 or ang >= 180 - alpha - 1e-9)) else 0
        if keep != want:
            bad["flag"] += 1
        if junction:
            if r != 0.0:
                bad["center"] += 1
            continue
        s = -1.0 if side == 1 else 1.0
        c = add(V[i], nrm, s * r)
        e = norm(sub(c, xa[k]))
        worst["center"] = max(worst["center"], e)
        if e > tol * 10 + 1e-12 * r:
            bad["center"] += 1
        if k % step:
            continue
        nchk += 1
        d = nearest(xa[k], F)
        e = r * (1 - TAU) - d                  # > 0: something inside
        worst["empty"] = max(worst["empty"], e)
        if e > tol:
            bad["empty"] += 1
        if stopped:
            e = d - r                          # > 0: touches nothing
            worst["touch"] = max(worst["touch"], e)
            if e > tol + 1e-9 * r:
                bad["touch"] += 1
    ok = not any(bad.values())
    lines.append("%s %d balls (%d by brute force): centers P + r n within "
                 "%.1e, emptiness %.1e, touching %.1e (tolerance %.0e)" %
                 ("PASS" if ok else "FAIL", n, nchk, worst["center"],
                  max(0.0, worst["empty"]), max(0.0, worst["touch"]), tol))
    if not ok:
        lines.append("     failures: " + ", ".join(
            "%s %d" % kv for kv in bad.items() if kv[1]))
    return ok


def expect(lines, name, ok, detail):
    lines.append("%s %s: %s" % ("PASS" if ok else "FAIL", name, detail))
    return ok


def by_side(A, side):
    """{surface vertex: (r, keep, center)} for one side."""
    out = {}
    for k in range(len(A["ma_r"])):
        if int(A["ma_side"][k]) == side:
            out[int(A["ma_src"][k]) - 1] = (
                A["ma_r"][k], int(A["ma_keep"][k]),
                (A["xic"][k], A["yic"][k], A["zic"][k]))
    return out


def exact(deck, V, axes, S, lines, stdout, data, work):
    """The answers known exactly for each deck. Returns ok."""
    ok = True
    A = axes[0]
    if deck == "ma01_circle":
        b = by_side(A, 1)
        e = max(norm(c) for r, kp, c in b.values())
        lo = math.cos(math.pi / 128)
        rr = [r for r, kp, c in b.values()]
        ok &= expect(lines, "in", e < 1e-3 and min(rr) >= lo - 1e-12 and
                     max(rr) <= 1 + 1e-12 and
                     all(kp == 1 for r, kp, c in b.values()),
                     "128 kept, centers within %.1e of 0, radius %.6f to "
                     "%.6f (between cos(pi/128) and 1)" % (e, min(rr),
                                                           max(rr)))
        b = by_side(A, 2)
        rm = 2 * math.sqrt(2)
        ok &= expect(lines, "out", all(abs(r - rm) < 1e-9 and kp == 0
                                       for r, kp, c in b.values()),
                     "128 not stopped: radius rmax = the box diagonal "
                     "%.6f, flagged" % rm)
    elif deck == "ma02_sphere":
        b = by_side(A, 1)
        e = max(norm(c) for r, kp, c in b.values())
        rr = [r for r, kp, c in b.values()]
        ok &= expect(lines, "sphere", e < 0.01 and min(rr) > 0.99 and
                     max(rr) <= 1 + 1e-12 and
                     all(kp == 1 for r, kp, c in b.values()),
                     "%d kept, centers within %.1e of 0, radius %.5f to "
                     "%.5f" % (len(rr), e, min(rr), max(rr)))
    elif deck == "ma03_slab":
        b = by_side(A, 1)
        long_ = [i for i, v in enumerate(V)
                 if 1 - 1e-9 <= v[0] <= 9 + 1e-9 and v[1] in (0.0, 1.0)]
        e = max(abs(2 * b[i][0] - 1) for i in long_)
        ey = max(abs(b[i][2][1] - 0.5) for i in long_)
        ok &= expect(lines, "long faces", e < 1e-9 and ey < 1e-9 and
                     all(b[i][1] == 1 for i in long_),
                     "%d vertices 1 <= x <= 9: thickness 1 within %.1e, "
                     "centers on y = 0.5 within %.1e, kept" %
                     (len(long_), e, ey))
        corners = [i for i, v in enumerate(V)
                   if v[0] in (0.0, 10.0) and v[1] in (0.0, 1.0)]
        ok &= expect(lines, "corners", len(corners) == 4 and
                     all(b[i][1] == 0 and b[i][0] == 0 for i in corners),
                     "the 4 corners: radius 0, flagged")
        b0 = by_side(axes[1], 1)
        ok &= expect(lines, "alpha_sharp 0", all(
            kp == 1 for r, kp, c in b0.values()),
            "every ball kept (%d)" % len(b0))
        br = by_side(axes[2], 1)
        ok &= expect(lines, "rmax 0.3", all(
            r <= 0.3 + 1e-12 for r, kp, c in br.values()) and all(
            abs(br[i][0] - 0.3) < 1e-12 and br[i][1] == 0 for i in long_),
            "no radius above 0.3; the long faces at 0.3, flagged")
    elif deck == "ma04_box":
        b = by_side(A, 1)
        inner = [i for i, v in enumerate(V) if v[2] in (0.0, 1.0) and
                 1 <= v[0] <= 9 and 1 <= v[1] <= 9]
        e = max(abs(2 * b[i][0] - 1) for i in inner)
        ok &= expect(lines, "top and bottom", e < 1e-9 and
                     all(b[i][1] == 1 for i in inner) and
                     max(abs(S["ma_thick_in"][i] - 1) for i in inner) < 1e-9,
                     "%d vertices 1 <= x, y <= 9: thickness 1 within "
                     "%.1e, kept" % (len(inner), e))
    elif deck == "ma05_sawtooth":
        b = by_side(A, 1)
        tips = [i for i, v in enumerate(V) if abs(v[1] - 2) < 1e-12 and
                abs(v[0] - round(v[0] - 0.5) - 0.5) < 1e-9 and
                1.4 < v[0] < 4.6]
        e = max(abs(2 * b[i][0] - 1.5) for i in tips)
        ok &= expect(lines, "above the tips", len(tips) == 4 and e < 1e-9
                     and all(b[i][1] == 1 for i in tips),
                     "%d top vertices over a tip: thickness 1.5 within "
                     "%.1e, kept" % (len(tips), e))
        val = [i for i, v in enumerate(V) if abs(v[1]) < 1e-12 and
               0.5 < v[0] < 5.5]
        ok &= expect(lines, "valleys", len(val) == 5 and
                     all(b[i][1] == 0 for i in val),
                     "the 5 valleys (90 degree corners): flagged")
    elif deck == "ma06_interface":
        bi, bo = by_side(A, 1), by_side(A, 2)
        mid = [i for i, v in enumerate(V) if abs(v[1] - 1) < 1e-12 and
               1.5 <= v[0] <= 8.5]
        e1 = max(abs(2 * bi[i][0] - 2) for i in mid)
        e2 = max(abs(2 * bo[i][0] - 1) for i in mid)
        ok &= expect(lines, "interface", e1 < 1e-9 and e2 < 1e-9 and
                     all(bi[i][1] == 1 and bo[i][1] == 1 for i in mid) and
                     max(abs(S["ma_thick_in"][i] - 2) +
                         abs(S["ma_thick_out"][i] - 1) for i in mid) < 1e-9,
                     "%d vertices 1.5 <= x <= 8.5: 2 above (in) and 1 "
                     "below (out) within %.1e, kept" %
                     (len(mid), max(e1, e2)))
        jun = [i for i, v in enumerate(V) if abs(v[1] - 1) < 1e-12 and
               v[0] in (0.0, 10.0)]
        ok &= expect(lines, "junctions", len(jun) == 2 and all(
            b[i][0] == 0 and b[i][1] == 0 for b in (bi, bo) for i in jun),
            "the 2 vertices on three segments: no ball, flagged")
    elif deck == "ma07_wedge":
        b = by_side(A, 1)
        a = math.atan(0.2)
        bot = [i for i, v in enumerate(V) if v[2] == 0.0 and
               1 <= v[0] <= 7 and 1 <= v[1] <= 3]
        e = max(abs(2 * b[i][0] - 2 * V[i][0] * math.tan(a / 2))
                for i in bot)
        ok &= expect(lines, "bottom", e < 1e-9 and
                     all(b[i][1] == 1 for i in bot),
                     "%d vertices 1 <= x <= 7, 1 <= y <= 3: thickness "
                     "2 x tan(a/2) within %.1e, kept" % (len(bot), e))
    elif deck == "ma08_dome":
        b = by_side(A, 1)
        bot = [i for i, v in enumerate(V) if v[2] == 0.0 and
               2 <= v[0] <= 6 and 2 <= v[1] <= 6]
        t = [2 * b[i][0] for i in bot]
        ok &= expect(lines, "bottom", all(b[i][1] == 1 for i in bot) and
                     min(t) > 1 - 1e-9 and max(t) < 1.8 + 1e-9,
                     "%d vertices 2 <= x, y <= 6: kept, thickness %.4f "
                     "to %.4f (the layer: 1 to 1.8)" % (len(bot), min(t),
                                                         max(t)))
    elif deck == "ma09_errors":
        want = ["the surface must be all lines or all triangles",
                "the surface must be all lines or all triangles",
                "the surface has no cells", "no mesh object mo_none",
                "mo_axis must differ from mo_surface",
                "side must be in, out or both"]
        got = [ln.split("medial_axis:", 1)[1].strip().rstrip(".")
               for ln in stdout.split("\n")
               if ln.startswith("ERROR compute / medial_axis:")]
        ok &= expect(lines, "errors", got == want,
                     "%d ERROR lines, as expected" % len(got) if got == want
                     else "got %r" % got)
        b = by_side(A, 1)
        ok &= expect(lines, "after the errors", len(b) == 128 and all(
            kp == 1 for r, kp, c in b.values()),
            "the last command made 128 balls, kept")
    elif deck == "ma10_biglayer":
        b = by_side(A, 1)
        bot = [i for i, v in enumerate(V) if v[2] == 0.0 and
               3 <= v[0] <= 17 and 3 <= v[1] <= 17]
        t = [2 * b[i][0] for i in bot]
        ok &= expect(lines, "bottom", min(t) > 1 - 1e-9 and
                     all(b[i][1] == 1 for i in bot),
                     "%d vertices 3 <= x, y <= 17: kept, thickness %.4f "
                     "to %.4f" % (len(bot), min(t), max(t)))
    elif deck in ("ma11_ellipse", "ma12_ellipsoid"):
        ok &= ellipse_check(V, A, lines, deck == "ma12_ellipsoid")
    elif deck == "ma13_annulus":
        b = by_side(A, 1)
        sag = 2 * (1 - math.cos(math.pi / 256)) + (1 - math.cos(math.pi / 128))
        t = [2 * r for r, kp, c in b.values()]
        ok &= expect(lines, "ring", all(kp == 1 for r, kp, c in b.values())
                     and max(abs(x - 1) for x in t) <= sag,
                     "%d balls (outer and inner loop) kept, thickness %.6f "
                     "to %.6f: within the facets' sag %.1e of 1" %
                     (len(t), min(t), max(t), sag))
        ok &= expect(lines, "inner loop", all(
            abs(math.hypot(c[0], c[1]) - 1.5) <= sag
            for r, kp, c in b.values()),
            "every center on the circle of radius 1.5 (the ring's middle)")
    elif deck == "ma14_tube":
        b = by_side(A, 1)
        sag = 1.5 * (1 - math.cos(math.pi / 64))
        wall = [i for i, v in enumerate(V) if 1 <= v[2] <= 5 and
                abs(math.hypot(v[0], v[1]) - 1.25) > 0.2]
        t = [2 * b[i][0] for i in wall]
        ok &= expect(lines, "walls", all(b[i][1] == 1 for i in wall) and
                     max(abs(x - 0.5) for x in t) <= sag,
                     "%d wall vertices 1 <= z <= 5: kept, thickness %.6f "
                     "to %.6f, within the facets' sag %.1e of 0.5" %
                     (len(wall), min(t), max(t), sag))
    elif deck == "ma15_neck":
        b = by_side(A, 1)
        ch = [i for i, v in enumerate(V) if abs(v[0]) <= 0.6]
        e = max(abs(2 * b[i][0] - 0.4) for i in ch)
        ok &= expect(lines, "channel", e < 1e-9 and
                     all(b[i][1] == 1 for i in ch),
                     "%d channel vertices |x| <= 0.6: thickness 0.4 within "
                     "%.1e, kept" % (len(ch), e))
        far = [i for i, v in enumerate(V) if abs(v[0]) >= 2]
        sag = 2 * (1 - math.cos(math.pi / 128))
        t = [2 * b[i][0] for i in far]
        ok &= expect(lines, "disks", max(abs(x - 2) for x in t) <= sag and
                     all(b[i][1] == 1 for i in far),
                     "%d vertices on the far halves of the disks: thickness "
                     "%.6f to %.6f (2 within the sag %.1e), kept" %
                     (len(far), min(t), max(t), sag))
        leak = [c for r, kp, c in b.values() if abs(c[0]) < 0.9 and r > 0.2
                + 1e-9]
        ok &= expect(lines, "no leak", not leak,
                     "no ball centered in the channel is wider than it")
    elif deck == "ma16_noise":
        for (V2, cells2, A2, src, f), eps, need in zip(data, (0.002, 0.01),
                                                       (0.95, 0.5)):
            b = by_side(A2, 1)
            ids = [i for i, v in enumerate(V2) if 1 <= v[0] <= 9 and
                   (abs(v[1]) < 0.05 or abs(v[1] - 1) < 0.05)]
            kept = [2 * b[i][0] for i in ids if b[i][1] == 1]
            frac = len(kept) / len(ids)
            ok &= expect(lines, "noise %g" % eps, kept and
                         min(kept) >= 1 - 2 * eps - 1e-9 and
                         max(kept) <= 1 + 2 * eps + 1e-9 and frac >= need,
                         "%d of %d face vertices 1 <= x <= 9 kept (%.0f%%; "
                         "needed %.0f%%), thickness %.5f to %.5f, within "
                         "1 +- 2 x %g" % (len(kept), len(ids), 100 * frac,
                                          100 * need, min(kept), max(kept),
                                          eps))
    elif deck == "ma17_sweep":
        ok &= sweep_check(data, lines)
    elif deck == "ma18_utm":
        for (V2, cells2, A2, src, f), ref in zip(
                data, ("ma03_axis.inp", "ma04_axis.inp")):
            P0, _, A0 = read_avs(os.path.join(work, ref))
            e = max(abs(a - b) for a, b in zip(A0["ma_r"], A2["ma_r"]))
            same = A0["ma_keep"] == A2["ma_keep"]
            ok &= expect(lines, f, e < 1e-6 and same,
                         "radii equal to %s's within %.1e, the same flags"
                         % (ref, e))
    elif deck == "ma19_interface3d":
        bi, bo = by_side(A, 1), by_side(A, 2)
        mid = [i for i, v in enumerate(V) if abs(v[2] - 1) < 1e-12 and
               1.5 <= v[0] <= 4.5 and 1.5 <= v[1] <= 4.5]
        e1 = max(abs(2 * bi[i][0] - 1) for i in mid)
        e2 = max(abs(2 * bo[i][0] - 2) for i in mid)
        ok &= expect(lines, "sheet", e1 < 1e-9 and e2 < 1e-9 and
                     all(bi[i][1] == 1 and bo[i][1] == 1 for i in mid),
                     "%d sheet vertices 1.5 <= x, y <= 4.5: 1 below (in) "
                     "and 2 above (out) within %.1e, kept" %
                     (len(mid), max(e1, e2)))
        rim = [i for i, v in enumerate(V) if abs(v[2] - 1) < 1e-12 and
               (v[0] in (0.0, 6.0) or v[1] in (0.0, 6.0))]
        ok &= expect(lines, "rim", len(rim) == 48 and all(
            b[i][0] == 0 and b[i][1] == 0 for b in (bi, bo) for i in rim),
            "the %d vertices where the sheet meets the box (edges of 3 "
            "or 4 triangles): no ball, flagged" % len(rim))
    return ok


def ellipse_check(V, A, lines, three_d):
    """Ellipse x^2/4 + y^2 = 1 (or the ellipsoid with y^2 + z^2): the
    ball at P = (x, s) is centered at (3x/4, 0) on the long axis, with
    radius |P - C|; it touches at P and at the mirror point (a ring, in
    3D), so its object angle is 2 atan(s / (x/4)). Kept balls: radius
    and center exact to the facets; flags: as the exact angle says,
    except within 2 degrees of 180 - alpha_sharp. Flagged balls near
    the ends can be too large (their touching points are within 90
    degrees of P, where the facets' sag is allowed)."""
    ok = True
    ek, eo, mis, over, n0 = 0.0, 0.0, 0, 0.0, 0
    for k in range(len(A["ma_r"])):
        x, y, z = V[int(A["ma_src"][k]) - 1]
        s = math.hypot(y, z)
        c = 0.75 * x
        r = math.hypot(x - c, s)
        ang = math.degrees(2 * math.atan2(s, abs(x - c)))
        kp = int(A["ma_keep"][k])
        if kp == 1:
            ek = max(ek, abs(A["ma_r"][k] - r) / r)
            eo = max(eo, math.hypot(A["yic"][k], A["zic"][k]),
                     abs(A["xic"][k] - c))
            n0 += 1
        else:
            over = max(over, (A["ma_r"][k] - r) / r)
        if abs(ang - 145) > 2 and (ang >= 145) != (kp == 1):
            mis += 1
    tol = 3e-3 if three_d else 1e-4
    ok &= expect(lines, "kept balls", ek <= tol and eo <= 3 * tol,
                 "%d kept: radius within %.1e of the exact one (relative; "
                 "allowed %.0e), centers within %.1e of theirs" %
                 (n0, ek, tol, eo))
    ok &= expect(lines, "flags", mis == 0,
                 "%d balls flagged against the exact object angle (beyond "
                 "2 degrees of 145)" % mis)
    lines.append("     (flagged balls near the ends: up to %.0f%% too large)"
                 % (100 * over))
    return ok


def sweep_check(data, lines):
    """Circles (N segments) and spheres (level L): the error of the
    radius and of the center against the exact 1 and 0."""
    ok = True
    rows = []
    for V, cells, A, src, f in data:
        er = max(abs(r - 1) for r in A["ma_r"])
        ec = max(math.sqrt(A["xic"][k] ** 2 + A["yic"][k] ** 2 +
                           A["zic"][k] ** 2) for k in range(len(A["ma_r"])))
        nk = sum(1 for x in A["ma_keep"] if int(x) == 1)
        rows.append((src, len(V), er, ec, nk, len(A["ma_r"]),
                     max(A["ma_r"])))
    SWEEP.clear()
    SWEEP.extend(rows)
    for kind, coarse in (("circle", 1), ("sphere", 2)):
        R = [r for r in rows if kind in r[0]]
        for src, nv, er, ec, nk, n, rmx in R:
            lines.append("     %-22s %5d vertices: radius error %.2e, "
                         "center %.2e, kept %d of %d" %
                         (src, nv, er, ec, nk, n))
        ok &= expect(lines, kind + " coarse", all(
            r[4] == 0 for r in R[:coarse]),
            "the coarsest %d: facets turning more than the 20 degrees read "
            "as smooth, so read as corners: every ball flagged" % coarse)
        fine = R[coarse:]
        ratios = [a[2] / b[2] for a, b in zip(fine, fine[1:])]
        ok &= expect(lines, kind + " convergence", all(
            r[4] == r[5] for r in fine) and all(q >= 3.0 for q in ratios),
            "the rest all kept; the error falls by %s per halving of h "
            "(h^2: 4)" % ", ".join("%.2f" % q for q in ratios))
    return ok


SWEEP = []


def roundtrip(deck, V, cells, A, lines):
    """Rebuild the shape as the union of the kept balls (side in) and
    measure, on a grid of points inside the shape, how far the farthest
    is from that union (0 when covered), and the covered fraction.
    The union never reaches out of the shape by more than the sag
    (the emptiness check). Corners whose branches are flagged stay
    uncovered; shapes without corners must be covered within 2 h."""
    spec = RT.get(deck)
    if spec is None:
        return True
    inside, n, smooth = spec
    balls = [(A["xic"][k], A["yic"][k], A["zic"][k], A["ma_r"][k])
             for k in range(len(A["ma_r"]))
             if int(A["ma_keep"][k]) == 1 and int(A["ma_side"][k]) == 1]
    two_d = cells[0][0] == "line"
    if inside is None:
        inside = polygon_inside(V, cells)
    lo = [min(v[i] for v in V) for i in range(3)]
    hi = [max(v[i] for v in V) for i in range(3)]
    pts = []
    m = n if two_d else n
    for a in range(m):
        for b in range(m):
            for c in ([0] if two_d else range(m)):
                p = [lo[0] + (hi[0] - lo[0]) * (a + 0.5) / m,
                     lo[1] + (hi[1] - lo[1]) * (b + 0.5) / m,
                     0.0 if two_d else
                     lo[2] + (hi[2] - lo[2]) * (c + 0.5) / m]
                if inside(p):
                    pts.append(p)
    worst, cov = 0.0, 0
    for p in pts:
        best = math.inf
        for x, y, z, r in balls:
            d = math.sqrt((p[0] - x) ** 2 + (p[1] - y) ** 2 +
                          (p[2] - z) ** 2) - r
            if d < best:
                best = d
                if d <= 0:
                    break
        if best <= 1e-12:
            cov += 1
        worst = max(worst, best)
    h = max(math.sqrt(dot(sub(V[v[1]], V[v[0]]), sub(V[v[1]], V[v[0]])))
            for t, v in cells)
    good = (worst <= 2 * h) if smooth else True
    return expect(lines, "round trip", good,
                  "union of %d kept balls covers %.1f%% of %d points inside;"
                  " the farthest uncovered is %.3g from it (%s)" %
                  (len(balls), 100.0 * cov / max(1, len(pts)), len(pts),
                   worst, ("no corners: must be within 2 h = %.3g" % (2 * h))
                   if smooth else "flagged branches of corners or ends: reported"))


def polygon_inside(V, cells):
    segs = [(V[v[0]], V[v[1]]) for t, v in cells]

    def inside(p):
        c = False
        for a, b in segs:
            if (a[1] > p[1]) != (b[1] > p[1]):
                x = a[0] + (p[1] - a[1]) * (b[0] - a[0]) / (b[1] - a[1])
                if x > p[0]:
                    c = not c
        return c
    return inside


def _block(Lx, Ly, top):
    return lambda p: 0 < p[0] < Lx and 0 < p[1] < Ly and 0 < p[2] < top(
        p[0], p[1])


# deck: (inside test or None for the 2D polygon, grid points per side,
#        no corners)
RT = {
    "ma01_circle": (None, 60, True),
    "ma02_sphere": (lambda p: dot(p, p) < 1, 16, True),
    "ma03_slab": (None, 80, False),
    "ma04_box": (_block(10, 10, lambda x, y: 1.0), 24, False),
    "ma05_sawtooth": (None, 70, False),
    "ma07_wedge": (_block(10, 4, lambda x, y: 0.2 * x), 24, False),
    "ma08_dome": (_block(8, 8, lambda x, y: 1 + 0.8 * math.exp(
        -((x - 4) ** 2 + (y - 4) ** 2) / 2)), 20, False),
    "ma11_ellipse": (None, 70, False),
    "ma12_ellipsoid": (lambda p: p[0] ** 2 / 4 + p[1] ** 2 + p[2] ** 2 < 1,
                       18, False),
    "ma13_annulus": (None, 70, True),
    "ma14_tube": (lambda p: 1 < math.hypot(p[0], p[1]) < 1.5 and
                  0 < p[2] < 6, 20, False),
    "ma15_neck": (None, 80, True),
}


# ------------------------------------------------------------- pictures
RAMP = [(68, 1, 84), (59, 82, 139), (33, 145, 140), (94, 201, 98),
        (253, 231, 37)]                       # viridis-like
PW, PH, TOP = 520, 440, 52


def color(t):
    t = max(0.0, min(1.0, t)) * (len(RAMP) - 1)
    i = min(int(t), len(RAMP) - 2)
    f = t - i
    c = [round(RAMP[i][k] + f * (RAMP[i + 1][k] - RAMP[i][k]))
         for k in range(3)]
    return "#%02x%02x%02x" % tuple(c)


class Frame:
    """Maps (u, v) into a panel at x offset ox, equal scales."""

    def __init__(self, us, vs, ox):
        self.u0, self.v0 = min(us), min(vs)
        du = max(max(us) - self.u0, 1e-12)
        dv = max(max(vs) - self.v0, 1e-12)
        self.s = min((PW - 40) / du, (PH - TOP - 30) / dv)
        self.ox = ox + 20 + ((PW - 40) - self.s * du) / 2
        self.oy = PH - 20 - ((PH - TOP - 30) - self.s * dv) / 2

    def __call__(self, u, v):
        return (self.ox + self.s * (u - self.u0),
                self.oy - self.s * (v - self.v0))


def heading(ox, title, note):
    return ["<text x='%d' y='20' font-size='15' font-weight='bold'>%s"
            "</text>" % (ox + 10, html.escape(title)),
            "<text x='%d' y='38' font-size='12' fill='#444'>%s</text>" %
            (ox + 10, html.escape(note))]


def panel_2d(V, cells, A, ox, title, note):
    """The line mesh in black, kept medial points (blue in, orange out),
    flagged ones in grey, every 8th kept ball as a thin circle."""
    us = [v[0] for v in V] + [x for x, k in zip(A["xic"], A["ma_keep"])
                              if k == 1]
    vs = [v[1] for v in V] + [y for y, k in zip(A["yic"], A["ma_keep"])
                              if k == 1]
    T = Frame(us, vs, ox)
    o = heading(ox, title, note)
    for t, v in cells:
        a, b = T(*V[v[0]][:2]), T(*V[v[1]][:2])
        o.append("<line x1='%.1f' y1='%.1f' x2='%.1f' y2='%.1f' "
                 "stroke='#000' stroke-width='1.3'/>" % (a + b))
    nk = 0
    for k in range(len(A["ma_r"])):
        x, y = T(A["xic"][k], A["yic"][k])
        if int(A["ma_keep"][k]) == 1:
            col = "#1f77b4" if int(A["ma_side"][k]) == 1 else "#ff7f0e"
            if nk % 8 == 0:
                o.append("<circle cx='%.1f' cy='%.1f' r='%.1f' fill='none' "
                         "stroke='%s' stroke-opacity='0.35'/>" %
                         (x, y, A["ma_r"][k] * T.s, col))
            nk += 1
            o.append("<circle cx='%.1f' cy='%.1f' r='2.2' fill='%s'/>" %
                     (x, y, col))
        elif A["ma_r"][k] > 0 and abs(x) < 1e5 and abs(y) < 1e5:
            o.append("<circle cx='%.1f' cy='%.1f' r='1.6' fill='#aaa'/>" %
                     (x, y))
    return o


def panel_section(V, cells, A, ox, title, note, y0):
    """3D: the surface cut by the plane y = y0 (black) and the medial
    points within half a cell of it (blue kept, grey flagged), in x-z."""
    segs = []
    for t, v in cells:
        p = [V[i] for i in v]
        cut = []
        for a, b in ((p[0], p[1]), (p[1], p[2]), (p[2], p[0])):
            da, db = a[1] - y0, b[1] - y0
            if (da < 0) != (db < 0):
                f = da / (da - db)
                cut.append((a[0] + f * (b[0] - a[0]),
                            a[2] + f * (b[2] - a[2])))
        if len(cut) == 2:
            segs.append(cut)
    ys = sorted(set(round(v[1], 9) for v in V))
    h = min((b - a for a, b in zip(ys, ys[1:])), default=1.0)
    near = [k for k in range(len(A["ma_r"]))
            if abs(A["yic"][k] - y0) <= 0.5 * h + 1e-9]
    kn = [k for k in near if int(A["ma_keep"][k]) == 1]
    us = [c[0] for s in segs for c in s] + [A["xic"][k] for k in kn]
    vs = [c[1] for s in segs for c in s] + [A["zic"][k] for k in kn]
    T = Frame(us, vs, ox)
    u0, u1, v0, v1 = min(us), max(us), min(vs), max(vs)
    near = [k for k in near if u0 <= A["xic"][k] <= u1 and
            v0 <= A["zic"][k] <= v1]
    o = heading(ox, title, note)
    for a, b in segs:
        o.append("<line x1='%.1f' y1='%.1f' x2='%.1f' y2='%.1f' "
                 "stroke='#000' stroke-width='1.3'/>" % (T(*a) + T(*b)))
    for k in near:
        x, y = T(A["xic"][k], A["zic"][k])
        kept = int(A["ma_keep"][k]) == 1
        col = "#1f77b4" if int(A["ma_side"][k]) == 1 else "#ff7f0e"
        o.append("<circle cx='%.1f' cy='%.1f' r='%s' fill='%s'/>" %
                 (x, y, "2.2" if kept else "1.6",
                  col if kept else "#aaa"))
    return o


def panel_sweep(ox, title, note):
    """log-log: the radius error against the edge length h, circles
    (blue) and spheres (orange); only the runs not read as corners;
    a dashed line of slope 2 (h^2)."""
    o = heading(ox, title, note)
    pts = []
    for src, nv, er, ec, nk, n, rmx in SWEEP:
        if nk == 0 or er <= 0:
            continue
        if "circle" in src:
            h, col = 2 * math.pi / nv, "#1f77b4"
        else:
            h, col = math.sqrt(4 * math.pi / (2 * nv)), "#ff7f0e"
        pts.append((math.log10(h), math.log10(er), col))
    if not pts:
        return o
    T = Frame([p[0] for p in pts], [p[1] for p in pts] +
              [min(p[1] for p in pts) - 0.4], ox)
    x0 = min(p[0] for p in pts)
    x1 = max(p[0] for p in pts)
    yb = min(p[1] for p in pts if p[0] == x0) - 0.4
    a, b = T(x0, yb), T(x1, yb + 2 * (x1 - x0))
    o.append("<line x1='%.1f' y1='%.1f' x2='%.1f' y2='%.1f' stroke='#999' "
             "stroke-dasharray='5,4'/>" % (a + b))
    for x, y, col in pts:
        X, Y = T(x, y)
        o.append("<circle cx='%.1f' cy='%.1f' r='4' fill='%s'/>" %
                 (X, Y, col))
    for (x, y, c1), (u, v, c2) in zip(pts, pts[1:]):
        if c1 == c2:
            o.append("<line x1='%.1f' y1='%.1f' x2='%.1f' y2='%.1f' "
                     "stroke='%s'/>" % (T(x, y) + T(u, v) + (c1,)))
    o.append("<text x='%d' y='%d' font-size='12' fill='#444'>log10 h "
             "(edge length) &#8594;</text>" % (ox + 20, PH - 4))
    return o


def panel_map(V, S, ox, title, note, zsel):
    """3D: the bottom (or chosen) vertices seen from above, colored by
    ma_thick_in; flagged ones as grey rings."""
    ids = [i for i, v in enumerate(V) if zsel(v)]
    us = [V[i][0] for i in ids]
    vs = [V[i][1] for i in ids]
    T = Frame(us, vs, ox)
    kept = [S["ma_thick_in"][i] for i in ids if S["ma_keep_in"][i] == 1]
    lo, hi = (min(kept), max(kept)) if kept else (0.0, 1.0)
    o = heading(ox, title, note + "  (%.3g to %.3g)" % (lo, hi))
    rad = max(1.5, min(5.0, 0.5 * T.s * (max(us) - min(us)) /
                       max(1.0, math.sqrt(len(ids)))))
    for i in ids:
        x, y = T(V[i][0], V[i][1])
        if S["ma_keep_in"][i] == 1:
            t = (S["ma_thick_in"][i] - lo) / max(hi - lo, 1e-12)
            o.append("<circle cx='%.1f' cy='%.1f' r='%.1f' fill='%s'/>" %
                     (x, y, rad, color(t)))
        else:
            o.append("<circle cx='%.1f' cy='%.1f' r='%.1f' fill='none' "
                     "stroke='#999'/>" % (x, y, rad))
    return o


def write_svg(path, panels):
    w = PW * len(panels)
    with open(path, "w") as f:
        f.write("<svg xmlns='http://www.w3.org/2000/svg' width='%d' "
                "height='%d' font-family='sans-serif'>\n" % (w, PH))
        f.write("<rect width='%d' height='%d' fill='#fff'/>\n" % (w, PH))
        for p in panels:
            f.write("\n".join(p) + "\n")
        f.write("</svg>\n")


def picture(logdir, deck, V, cells, axes, S, data):
    A = axes[0]
    name = deck + ".svg"
    note2 = ("black: the line mesh; blue: kept medial points (orange: "
             "side out); grey: flagged")
    if deck == "ma17_sweep":
        V1, c1, A1 = data[1][:3]
        V2, c2, A2 = data[-1][:3]
        P = [panel_sweep(0, "error against h", "radius error, circles "
                         "(blue) and spheres (orange); dashed: slope 2"),
             panel_2d(V1, c1, A1, PW, "the circle of 32 segments", note2),
             panel_section(V2, c2, A2, 2 * PW, "the sphere of 5120 "
                           "triangles", "section y = 0", 0.0)]
    elif cells[0][0] == "line":
        P = [panel_2d(V, cells, A, 0, deck, note2)]
        if deck == "ma03_slab":
            P.append(panel_2d(V, cells, axes[2], PW, "rmax 0.3",
                              "every ball at most 0.3: the long faces "
                              "flagged (grey)"))
        if deck == "ma16_noise":
            V2, c2, A2 = data[1][:3]
            P[0] = panel_2d(V, cells, A, 0, deck + ": noise 0.002", note2)
            P.append(panel_2d(V2, c2, A2, PW, "noise 0.01", "the noise "
                              "flags more balls (grey): they are hair"))
    else:
        ys = [v[1] for v in V]
        y0 = 0.5 * (min(ys) + max(ys))
        P = [panel_section(V, cells, A, 0, deck, "section y = %.3g: the "
                           "surface (black), kept medial points (blue; "
                           "orange: side out), grey flagged" % y0, y0)]
        if (S is not None and "ma_thick_in" in S and
                deck in ("ma04_box", "ma07_wedge", "ma08_dome",
                         "ma10_biglayer")):
            P.append(panel_map(V, S, PW, "thickness on the bottom",
                               "ma_thick_in, seen from above; rings: "
                               "flagged", lambda v: v[2] == 0.0))
        if deck == "ma18_utm":
            V1, c1, A1 = data[0][:3]
            P[0] = panel_2d(V1, c1, A1, 0, "the slab at x + 500000, "
                            "y + 4000000", note2)
    write_svg(os.path.join(logdir, name), P)
    return [name]


# ----------------------------------------------------------------- main
def verdict(ok):
    return "PASS" if ok else "FAIL"


def check_case(work, logdir, deck, runs):
    lines, ok = [], True
    stdout = open(os.path.join(work, deck + ".out"),
                  errors="replace").read()
    if "Fortran runtime error" in stdout or "Segmentation" in stdout:
        return False, ["FAIL LaGriT crashed"], [], "crash"
    axes, data = [], []
    surfs = {}
    for src, f, alpha, rmax in runs:
        if src not in surfs:
            surfs[src] = read_avs(os.path.join(work, src))[:2]
        V, cells = surfs[src]
        p = os.path.join(work, f)
        if not os.path.exists(p):
            return False, ["FAIL no output file " + f], [], ""
        P, _, A = read_avs(p)
        A["xic"] = [q[0] for q in P]
        A["yic"] = [q[1] for q in P]
        A["zic"] = [q[2] for q in P]
        axes.append(A)
        data.append((V, cells, A, src, f))
        n0 = len(lines)
        ok &= check_balls(V, cells, A, alpha, rmax, SAMPLE.get(deck, 1),
                          lines)
        if len(runs) > 1:
            lines[n0] = lines[n0][:4] + " " + f + ":" + lines[n0][4:]
    V, cells = surfs[runs[0][0]]
    S = None
    sp = os.path.join(work, deck.split("_")[0] + "_surf.inp")
    if os.path.exists(sp):
        S = read_avs(sp)[2]
        A = axes[0]
        e, n = 0.0, 0
        for side, sfx in ((1, "in"), (2, "out")):
            if "ma_thick_" + sfx not in S:
                continue
            b = by_side(A, side)
            for i, (r, kp, c) in b.items():
                n += 1
                e = max(e, abs(S["ma_thick_" + sfx][i] - 2 * r))
                if int(S["ma_keep_" + sfx][i]) != kp:
                    e = math.inf
        ok &= expect(lines, "surface attributes", n > 0 and e < 1e-9,
                     "ma_thick_* = 2 r and ma_keep_* = the ball's flag "
                     "at %d vertices" % n)
    ok &= roundtrip(deck, V, cells, axes[0], lines)
    ok &= exact(deck, V, axes, S, lines, stdout, data, work)
    stat = ""
    for ln in stdout.split("\n"):
        if ln.startswith("medial_axis:") and "kept" in ln and "balls" in ln:
            stat = " ".join(ln.split()[1:])
            break
    pics = picture(logdir, deck, V, cells, axes, S, data)
    return ok, lines, pics, stat


def main(work, logdir):
    os.makedirs(logdir, exist_ok=True)
    rows, allok = [], True
    for deck, runs, desc in CASES:
        try:
            ok, lines, pics, stat = check_case(work, logdir, deck, runs)
        except Exception as e:          # a broken output is a failure
            ok, lines, pics, stat = False, ["FAIL checker: %r" % e], [], ""
        allok &= ok
        with open(os.path.join(logdir, deck + ".check"), "w") as f:
            f.write("\n".join(lines) + "\n")
        rows.append((deck, ok, desc, lines, pics, stat))
        print("%-16s %s  %s" % (deck, verdict(ok), stat))
    with open(os.path.join(logdir, "table.txt"), "w") as f:
        for deck, ok, desc, lines, pics, stat in rows:
            f.write("%-16s %s  %s\n" % (deck, verdict(ok), stat))
    npass = sum(r[1] for r in rows)
    o = ["<!doctype html><html><head><meta charset='utf-8'>",
         "<title>test_medial_axis</title>",
         "<style>body{font-family:sans-serif;max-width:1150px;margin:20px "
         "auto;padding:0 16px}td,th{padding:4px 8px;border-bottom:1px solid "
         "#ddd;vertical-align:top}.P{color:#080}.F{color:#c00}"
         "pre{font-size:12px;margin:2px 0;white-space:pre-wrap}"
         "img{max-width:100%}</style></head><body>",
         "<p style='margin:0 0 8px;font-size:13px'><a href='../../summary"
         ".html'>&larr; all suites (top-level summary)</a></p>",
         "<h1>test_medial_axis: %d of %d decks pass</h1>" %
         (npass, len(rows)),
         "<p><b>What this tests.</b> <code>compute / medial_axis / mo_axis "
         "/ mo_surface / [in|out|both] / [alpha_sharp] / [rmax]</code>: "
         "for every vertex of a line mesh (2D) or triangle surface (3D), "
         "the largest ball tangent there that holds no part of the "
         "surface. Every ball is checked by brute force in plain Python "
         "(its center from the vertex normal, empty, touching the "
         "surface, its flag), and the known answers of each shape.</p>",
         "<table><tr><th>deck</th><th>result</th><th>what</th>"
         "<th>checks</th></tr>"]
    for deck, ok, desc, lines, pics, stat in rows:
        o.append("<tr><td><a href='%s.check'>%s</a></td><td class='%s'>%s"
                 "</td><td>%s</td><td><pre>%s</pre></td></tr>" %
                 (deck, deck, "P" if ok else "F", verdict(ok),
                  html.escape(desc), html.escape("\n".join(lines))))
    o.append("</table>")
    for deck, ok, desc, lines, pics, stat in rows:
        for p in pics:
            o.append("<h3>%s</h3><p>%s</p><img src='%s'>" %
                     (deck, html.escape(desc), p))
    o.append("</body></html>")
    with open(os.path.join(logdir, "summary.html"), "w") as f:
        f.write("\n".join(o) + "\n")
    print("%d of %d decks pass" % (npass, len(rows)))
    return 0 if allok else 1


if __name__ == "__main__":
    sys.exit(main(sys.argv[1], sys.argv[2]))
