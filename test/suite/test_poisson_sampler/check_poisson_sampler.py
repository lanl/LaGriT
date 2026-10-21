#!/usr/bin/env python3
"""
check_poisson_sampler.py - checker for the Poisson disk sampler tests
(test U, L and V, 2D and 3D). Python 3 standard library only.

    check_poisson_sampler.py outputs CASES          files the decks write
    check_poisson_sampler.py ids     CASES          case ids, in order
    check_poisson_sampler.py decks   CASES          decks, in order
    check_poisson_sampler.py case    CASES ID SVGDIR

Prints one "key value" line per quantity, then
    result PASS | FAIL
    summary <one line>
and exits 1 on FAIL. Lines prefixed "hard_" are the judged ones.

----------------------------------------------------------------------
DEFINITIONS (the 00README points here)
----------------------------------------------------------------------

h at a point     the EXACT field of the case (poisson_sampler.cases):
    uniform              h
    linear y h0 s        max(h0, h0 + s*y)
    valley y c hmin s hmax   min(hmax, max(hmin, hmin + s*|y - c|))
    These are what the .mlgi files compute on the lookup lattice.

interior vertex  not on the box boundary (within 1e-9 x box size).
    Edge and face vertices are placed with deliberately smaller radii
    (0.81 h, 0.9 h) by the sampler, so pairs are judged between
    interior vertices only ("II pairs").

pair measure     m = d / max(h_p, h_q), the sampler's own acceptance rule
    (a candidate is rejected if it is closer to an accepted vertex than
    the larger of the two radii).
    II_lt1    pairs with m < 1 - 1e-9
    II_lt095  pairs with m < 0.95
    II_lt09   pairs with m < 0.9
    II_min    smallest m

gap (coverage)   PROBES random points in the box (fixed seed); for each,
    the distance to the nearest vertex over h at the probe.
    gap_max   the largest such ratio; a maximal sampling has gap_max
              near 1 (vertices at most about h from any point)
    holes     probes with ratio > 1.5

----------------------------------------------------------------------
HARD checks
----------------------------------------------------------------------
    every case : output written, vertices > 0, every vertex inside the
                 box, no duplicate vertices (d < 1e-9 h)
    kind U, L  : II_lt1 = 0. Exact: for a uniform or linear field the
                 interpolated lookup equals the field, so a correct
                 sampler never accepts such a pair.
    kind V     : II_lt095 = 0 and gap_max <= 2.0. The 5 % allows for
                 interpolation where the field has a corner (h reaches
                 its floor or ceiling): there the lookup is up to about
                 1 % below the exact field.
"""

import math
import os
import random
import sys

TOL_REL = 1.0e-9
PROBES = 20000
GAP_LIMIT = 2.0
V_LIMIT = 0.95


# ======================================================================
# inputs
# ======================================================================

def read_cases(path):
    cases = []
    with open(path) as f:
        for raw in f:
            line = raw.split("#", 1)[0].strip()
            if not line:
                continue
            w = line.split()
            if w[0] != "case":
                continue
            d = {"id": w[1]}
            for kv in w[2:]:
                k, v = kv.split("=", 1)
                d[k] = v
            d["box"] = [float(v) for v in d["box"].split(",")]
            if d["dim"] == "2d":
                d["box"] += [0.0, 0.0]
            d["h"] = float(d["h"])
            d["field"] = d["field"].split(":")
            cases.append(d)
    return cases


def find_case(cases, cid):
    for c in cases:
        if c["id"] == cid:
            return c
    sys.exit("no case %s" % cid)


def read_points(path):
    """AVS UCD ascii, vertices only (cells are ignored)."""
    with open(path) as f:
        nn = int(f.readline().split()[0])
        pts = []
        for _ in range(nn):
            w = f.readline().split()
            pts.append((float(w[1]), float(w[2]), float(w[3])))
    return pts


def make_field(case):
    f = case["field"]
    if f[0] == "uniform":
        h0 = case["h"]
        return lambda p: h0
    ax = "xyz".index(f[1])
    if f[0] == "linear":
        h0, s = float(f[2]), float(f[3])
        return lambda p: max(h0, h0 + s * p[ax])
    if f[0] == "valley":
        c, hmin, s, hmax = map(float, f[2:6])
        return lambda p: min(hmax, max(hmin, hmin + s * abs(p[ax] - c)))
    sys.exit("unknown field " + ":".join(f))


# ======================================================================
# a bucket grid for neighbour queries
# ======================================================================

class Buckets:
    def __init__(self, pts, cell, nd):
        self.cell, self.nd, self.pts = cell, nd, pts
        self.g = {}
        for i, p in enumerate(pts):
            self.g.setdefault(self.key(p), []).append(i)

    def key(self, p):
        c = self.cell
        k = (int(math.floor(p[0] / c)), int(math.floor(p[1] / c)))
        return k + (int(math.floor(p[2] / c)),) if self.nd == 3 else k

    def within(self, p, r):
        """indices of points with a coordinate box overlap of radius r"""
        c = self.cell
        lo = [int(math.floor((p[a] - r) / c)) for a in range(self.nd)]
        hi = [int(math.floor((p[a] + r) / c)) for a in range(self.nd)]
        out = []
        if self.nd == 2:
            for a in range(lo[0], hi[0] + 1):
                for b in range(lo[1], hi[1] + 1):
                    out.extend(self.g.get((a, b), ()))
        else:
            for a in range(lo[0], hi[0] + 1):
                for b in range(lo[1], hi[1] + 1):
                    for e in range(lo[2], hi[2] + 1):
                        out.extend(self.g.get((a, b, e), ()))
        return out

    def nearest(self, p):
        """distance to the nearest point (rings of cells, outward)"""
        c, nd = self.cell, self.nd
        best = float("inf")
        k0 = self.key(p)
        r = 0
        while True:
            for k in self.ring(k0, r):
                for j in self.g.get(k, ()):
                    q = self.pts[j]
                    d2 = sum((p[a] - q[a]) ** 2 for a in range(nd))
                    if d2 < best:
                        best = d2
            if best <= (r * c) ** 2 or r > 10000:
                return math.sqrt(best)
            r += 1

    def ring(self, k0, r):
        if r == 0:
            yield k0
            return
        rng = range(-r, r + 1)
        if self.nd == 2:
            for a in rng:
                for b in rng:
                    if max(abs(a), abs(b)) == r:
                        yield (k0[0] + a, k0[1] + b)
        else:
            for a in rng:
                for b in rng:
                    for e in rng:
                        if max(abs(a), abs(b), abs(e)) == r:
                            yield (k0[0] + a, k0[1] + b, k0[2] + e)


# ======================================================================
# SVG
# ======================================================================

def esc(s):
    return s.replace("&", "&amp;").replace("<", "&lt;").replace(">", "&gt;")


def picture(case, pts, inner, close, holes, path):
    """Projection on (x, y). 3D: vertices in a slab around mid z. V: the
    window y in [c - 1, c + 1] around the fine band."""
    box = case["box"]
    nd = 3 if case["dim"] == "3d" else 2
    x0, x1, y0, y1 = box[0], box[1], box[2], box[3]
    if case["kind"] == "V":
        c = float(case["field"][2])
        y0, y1 = max(box[2], c - 1.0), min(box[3], c + 1.0)
    zm = 0.5 * (box[4] + box[5])
    slab = 2.0 * case["h"] if case["kind"] != "V" else 5.0 * case["h"]
    W, H = 900, 520
    pw = 820.0
    ph = 440.0
    sx = pw / (x1 - x0)
    sy = ph / (y1 - y0)
    s = min(sx, sy)
    # long thin windows: stretch the short axis up to 6x for visibility
    sxx = min(sx, 6 * s)
    syy = min(sy, 6 * s)
    ox, oy = 60 + (pw - (x1 - x0) * sxx) / 2, 50 + (ph - (y1 - y0) * syy) / 2

    def X(p):
        return ox + (p[0] - x0) * sxx

    def Y(p):
        return oy + (y1 - p[1]) * syy

    out = ['<svg xmlns="http://www.w3.org/2000/svg" width="%d" height="%d" '
           'viewBox="0 0 %d %d" font-family="sans-serif">' % (W, H, W, H),
           '<rect width="100%" height="100%" fill="#fff"/>',
           '<text x="10" y="20" font-size="14">%s</text>' % esc(
               "%s  %s  h %s  field %s" % (case["id"], case["dim"],
                                           case["h"], " ".join(case["field"]))),
           '<text x="10" y="38" font-size="11" fill="#444">%s</text>' % esc(
               "vertices%s: grey; red lines = interior pairs closer than "
               "%s max(h); orange + = gaps wider than 1.5 h%s" % (
                   " in a slab |z-%g|<%g" % (zm, slab) if nd == 3 else "",
                   "0.95" if case["kind"] == "V" else "1.0",
                   "; window y %g..%g" % (y0, y1) if case["kind"] == "V" else "")),
           '<rect x="%.1f" y="%.1f" width="%.1f" height="%.1f" fill="none" '
           'stroke="#444"/>' % (ox, oy, (x1 - x0) * sxx, (y1 - y0) * syy)]
    # y range labels; for V a dashed line where the fine band belongs
    out.append('<text x="%.1f" y="%.1f" font-size="10" text-anchor="end">y %g</text>'
               % (ox - 3, oy + 8, y1))
    out.append('<text x="%.1f" y="%.1f" font-size="10" text-anchor="end">y %g</text>'
               % (ox - 3, oy + (y1 - y0) * syy, y0))
    if case["kind"] == "V":
        c = float(case["field"][2])
        yc = oy + (y1 - c) * syy
        out.append('<line x1="%.1f" y1="%.1f" x2="%.1f" y2="%.1f" stroke="#06c" '
                   'stroke-dasharray="6 4"/>' % (ox, yc, ox + (x1 - x0) * sxx, yc))
        out.append('<text x="%.1f" y="%.1f" font-size="11" fill="#06c" '
                   'paint-order="stroke" stroke="#fff" stroke-width="3">'
                   'centre of the fine band, y = %g</text>' % (ox + 6, yc - 5, c))
    for i, p in enumerate(pts):
        if not (y0 <= p[1] <= y1):
            continue
        if nd == 3 and abs(p[2] - zm) > slab:
            continue
        out.append('<circle cx="%.1f" cy="%.1f" r="1.3" fill="%s"/>'
                   % (X(p), Y(p), "#888" if inner[i] else "#bbb"))
    for (i, j) in close[:3000]:
        p, q = pts[i], pts[j]
        if not (y0 <= p[1] <= y1):
            continue
        out.append('<line x1="%.1f" y1="%.1f" x2="%.1f" y2="%.1f" '
                   'stroke="#d00" stroke-width="2"/>' % (X(p), Y(p), X(q), Y(q)))
        out.append('<circle cx="%.1f" cy="%.1f" r="2.5" fill="none" '
                   'stroke="#d00"/>' % (X(p), Y(p)))
    for q in holes[:2000]:
        if not (y0 <= q[1] <= y1):
            continue
        out.append('<text x="%.1f" y="%.1f" font-size="10" fill="#e07b00" '
                   'text-anchor="middle">+</text>' % (X(q), Y(q) + 3))
    out.append('</svg>')
    with open(path, "w") as f:
        f.write("\n".join(out) + "\n")


# ======================================================================
# the case row
# ======================================================================

class Row:
    def __init__(self):
        self.fail = []

    def put(self, k, v):
        if isinstance(v, float):
            v = "nan" if v != v else "%.4g" % v
        print("%s %s" % (k, v))

    def hard(self, k, ok, v):
        self.put("hard_%s %s" % (k, "ok" if ok else "FAIL"), v)
        if not ok:
            self.fail.append(k)

    def finish(self, summary):
        res = "FAIL" if self.fail else "PASS"
        if self.fail:
            summary = "failed: " + ",".join(self.fail) + " | " + summary
        print("result " + res)
        print("summary " + summary)
        sys.exit(1 if self.fail else 0)


def row_case(cases, cid, svgdir):
    R = Row()
    case = find_case(cases, cid)
    box, h0, kind = case["box"], case["h"], case["kind"]
    nd = 3 if case["dim"] == "3d" else 2
    R.put("case", cid)
    R.put("kind", kind)
    R.put("field", " ".join(case["field"]))
    if not os.path.exists(case["inp"]):
        R.hard("written", False, case["inp"])
        R.finish("no " + case["inp"])
    pts = read_points(case["inp"])
    n = len(pts)
    R.put("vertices", n)
    R.hard("vertices_gt_0", n > 0, n)
    if n == 0:
        R.finish("no vertices")
    hf = make_field(case)
    hv = [hf(p) for p in pts]
    hmax = max(hv)
    L = max(box[1] - box[0], box[3] - box[2], box[5] - box[4])
    tol = TOL_REL * L
    nout = 0
    inner = []
    for p in pts:
        inside = all(box[2 * a] - tol <= p[a] <= box[2 * a + 1] + tol for a in range(nd))
        nout += 0 if inside else 1
        onb = any(abs(p[a] - box[2 * a]) <= tol or abs(p[a] - box[2 * a + 1]) <= tol
                  for a in range(nd))
        inner.append(not onb)
    R.hard("inside_box", nout == 0, nout)
    R.put("interior", sum(inner))

    # ---- pairs ----------------------------------------------------------
    B = Buckets(pts, 1.0001 * hmax, nd)
    lt1 = lt095 = lt09 = ndup = 0
    mmin = float("inf")
    close = []
    lim = V_LIMIT if kind == "V" else 1.0 - 1e-9
    for i, p in enumerate(pts):
        hi_ = hv[i]
        for j in B.within(p, hmax):
            if j <= i:
                continue
            q = pts[j]
            d = math.sqrt(sum((p[a] - q[a]) ** 2 for a in range(nd)))
            mh = max(hi_, hv[j])
            if d >= mh:
                continue
            if d < 1e-9 * mh:
                ndup += 1
            if not (inner[i] and inner[j]):
                continue
            m = d / mh
            mmin = min(mmin, m)
            if m < 1.0 - 1e-9:
                lt1 += 1
            if m < 0.95:
                lt095 += 1
            if m < 0.9:
                lt09 += 1
            if m < lim:
                close.append((i, j))
    R.hard("no_duplicates", ndup == 0, ndup)
    R.put("II_min", mmin if mmin < float("inf") else float("nan"))
    R.put("II_lt1", lt1)
    R.put("II_lt095", lt095)
    R.put("II_lt09", lt09)

    # ---- gaps -----------------------------------------------------------
    rnd = random.Random(12345)
    gmax = 0.0
    holes = []
    for _ in range(PROBES):
        q = [box[2 * a] + rnd.random() * (box[2 * a + 1] - box[2 * a]) for a in range(nd)]
        q += [0.0] * (3 - nd)
        g = B.nearest(q) / hf(q)
        gmax = max(gmax, g)
        if g > 1.5:
            holes.append(q)
    R.put("gap_max", gmax)
    R.put("holes_gt1.5h", len(holes))

    if kind in ("U", "L"):
        R.hard("no_interior_pair_closer_than_h", lt1 == 0, lt1)
    else:
        R.hard("no_interior_pair_closer_than_0.95h", lt095 == 0, lt095)
        R.hard("gap_max_le_2h", gmax <= GAP_LIMIT, "%.3g" % gmax)

    picture(case, pts, inner, close, holes, os.path.join(svgdir, cid + ".svg"))
    R.finish("%d vertices, II<1 %d, II<0.95 %d, min %s, gap %s" % (
        n, lt1, lt095, "%.3f" % mmin if mmin < float("inf") else "-", "%.2f" % gmax))


def main(argv):
    if len(argv) < 3:
        sys.exit(__doc__)
    mode, cases = argv[1], read_cases(argv[2])
    if mode == "outputs":
        print(" ".join(c["inp"] for c in cases))
    elif mode == "ids":
        print(" ".join(c["id"] for c in cases))
    elif mode == "decks":
        seen = []
        for c in cases:
            if c["deck"] not in seen:
                seen.append(c["deck"])
        print(" ".join(seen))
    elif mode == "case":
        row_case(cases, argv[3], argv[4])
    else:
        sys.exit("unknown mode " + mode)


if __name__ == "__main__":
    main(sys.argv)
