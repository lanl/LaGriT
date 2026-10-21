#!/usr/bin/env python3
"""
Check one connected Poisson disk mesh written by test_Poisson_2D.lgi.

    python3 check_test_poisson_2d.py --poly unit_square.poly \\
            --mesh output02_unit_square_var.inp --h0 0.01 \\
            --field absx --width 10 [--svg picture.svg]

    python3 check_test_poisson_2d.py --verify-field h_field_out.inp \\
            --h0 0.01 --field absx --width 20

FIELDS

The checker needs h(x,y) to normalise spacing. It cannot read the
field the sampler used, because the driver releases that mesh object
before returning. So the field is written out here, and MUST mirror
user_h_field_att.mlgi:

    uniform       h = h0
    absx W        h = max(h0, h0 * W * |x|)

                  user_h_field_att.mlgi computes, in order,
                  x**2, sqrt, * POI_H_FACTOR, * WIDTH_FACTOR,
                  floor at POI_H_FACTOR.  POI_H_FACTOR is h0 and
                  WIDTH_FACTOR is W.

Because a copy can drift from the original, --verify-field compares
this formula against h_field_out.inp, which the .mlgi dumps as a
diagnostic. The runner does that on every run, so an edit to the .mlgi
that is not mirrored here is reported, not silently mismeasured.

WHAT IS MEASURED, AND ON WHAT

test_Poisson_2D.lgi connects every point set, so only the FINAL mesh
is available. Its interior nodes have been SMOOTHED and recon 1 may
have added boundary nodes, so spacing measured here is the spacing of
the delivered mesh, not exactly what the sampler produced.

HARD checks (decide exit status):
  CONTAINMENT       every node inside the polygon or on its boundary
  INVERTED          triangles of the minority orientation
  BAD CONNECTIVITY  elements naming nodes that do not exist
  FIELD MATCH       (--verify-field only) formula vs dumped field

SOFT measures (reported, not judged, until calibrated):
  SEPARATION        for every PAIR of nodes p, q the ratio

                        d(p,q) / max(h(p), h(q))

                    i.e. each pair is judged against the LARGER of its
                    two targets. Because h varies, the worst pair for
                    a node need not be its nearest neighbour (a
                    slightly farther node in a coarser region can be
                    the violator), so every pair within reach is
                    examined, not just the nearest. A node's
                    separation is its smallest ratio over all pairs;
                    a CLOSE pair is one with ratio < 1. Reported:
                    minimum, 1st percentile, median, close count.

                    Reported SEPARATELY for interior nodes and for
                    nodes on the polygon boundary. The first real run
                    showed boundary nodes are placed CLOSER than h by
                    design (gaps of 0.90 to 1.37 h along the edges of
                    the uniform square, median 0.95 h), while interior
                    nodes held to 0.991 h after smoothing. Pooled, the
                    boundary drags every statistic down and hides
                    what the interior is doing.
  DENSITY           N / integral(1/h^2 dA). Equals N h^2 / A for a
                    uniform field, so all cases are on one scale.
  TRACKING          a DIFFERENT quantity from separation: nearest
                    neighbour distance / h AT THE NODE, which asks
                    whether the typical spacing follows the field.
                    Nodes are banded by local h, and the median
                    distance / h is taken per band. If spacing follows
                    the field, every band gives about the same number.
                    tracking_spread = largest band median / smallest.
                    1.0 is perfect tracking. INTERIOR nodes only: the
                    square's left and right edges sit where h is
                    largest, so boundary nodes would all land in the
                    top band and distort it.
  VERTICES FOUND    polygon corners present as mesh nodes
  AREA              mesh area against polygon area
  ANGLES            smallest triangle angle: min, 1st pct, median,
                    and % below 20 deg; largest angle: max
  RADIUS RATIO      2 * inradius / circumradius

Exit status 0 if the HARD checks pass, 1 if not, 2 on usage error.
"""
import argparse
import math
import sys

CLOSE_FACTOR = 1.0 - 1.0e-9
POOR_ANGLE_DEG = 20.0
NBANDS = 8              # tracking bands
MIN_BAND = 20           # nodes a band needs to count toward the spread
FIELD_TOL = 1.0e-9      # relative, formula vs dumped field


# ---------------------------------------------------------------------
# The field. KEEP IN STEP WITH user_h_field_att.mlgi.
# ---------------------------------------------------------------------
def make_field(kind, h0, width):
    if kind == 'uniform':
        return lambda x, y: h0
    if kind == 'absx':
        return lambda x, y: max(h0, h0 * width * abs(x))
    raise ValueError(f"unknown field '{kind}'")


# ---------------------------------------------------------------------
# Input
# ---------------------------------------------------------------------
def read_poly(fn):
    pts = []
    with open(fn) as f:
        for i, l in enumerate(f):
            if i == 0:
                continue
            p = l.split()
            if len(p) >= 4:
                pts.append((float(p[1]), float(p[2])))
    return pts


def read_avs(fn):
    """Nodes, triangles (0 based), bad connectivity count, and node
    attributes as {name: [values]}."""
    with open(fn) as f:
        lines = f.read().splitlines()
    hdr = lines[0].split()
    nn, ne, nnd = int(hdr[0]), int(hdr[1]), int(hdr[2])
    ids = {}
    nodes = []
    for l in lines[1:1 + nn]:
        p = l.split()
        ids[int(p[0])] = len(nodes)
        nodes.append((float(p[1]), float(p[2])))
    tris = []
    bad = 0
    for l in lines[1 + nn:1 + nn + ne]:
        p = l.split()
        if len(p) < 6 or p[2].lower() != 'tri':
            continue
        try:
            tris.append(tuple(ids[int(x)] for x in p[3:6]))
        except KeyError:
            bad += 1
    att = {}
    if nnd > 0:
        k = 1 + nn + ne
        nat = int(lines[k].split()[0])
        names = [lines[k + 1 + i].split(',')[0].strip()
                 for i in range(nat)]
        cols = [[] for _ in names]
        for l in lines[k + 1 + nat:k + 1 + nat + nn]:
            p = l.split()[1:]
            for i in range(nat):
                cols[i].append(float(p[i]))
        att = dict(zip(names, cols))
    return nodes, tris, bad, att


# ---------------------------------------------------------------------
# Geometry
# ---------------------------------------------------------------------
def signed_area(pts):
    a = 0.0
    n = len(pts)
    for i in range(n):
        x1, y1 = pts[i]
        x2, y2 = pts[(i + 1) % n]
        a += x1 * y2 - x2 * y1
    return a / 2.0


def seg_dist2(p, a, b):
    ex, ey = b[0] - a[0], b[1] - a[1]
    L = ex * ex + ey * ey
    t = 0.0 if L == 0.0 else \
        max(0.0, min(1.0, ((p[0]-a[0])*ex + (p[1]-a[1])*ey) / L))
    dx, dy = p[0] - (a[0] + t*ex), p[1] - (a[1] + t*ey)
    return dx * dx + dy * dy


def inside_or_on(pt, poly, tol2):
    n = len(poly)
    for i in range(n):
        if seg_dist2(pt, poly[i], poly[(i + 1) % n]) <= tol2:
            return True
    x, y = pt
    c = False
    j = n - 1
    for i in range(n):
        xi, yi = poly[i]
        xj, yj = poly[j]
        if ((yi > y) != (yj > y)) and \
           (x < (xj - xi) * (y - yi) / (yj - yi) + xi):
            c = not c
        j = i
    return c


def nearest_neighbours(pts, cell):
    grid = {}
    for k, (x, y) in enumerate(pts):
        grid.setdefault((int(math.floor(x / cell)),
                         int(math.floor(y / cell))), []).append(k)
    dist = [0.0] * len(pts)
    who = [-1] * len(pts)
    for k, (x, y) in enumerate(pts):
        ci, cj = int(math.floor(x / cell)), int(math.floor(y / cell))
        best2, bi = float('inf'), -1
        r = 1
        while True:
            for i in range(ci - r, ci + r + 1):
                for j in range(cj - r, cj + r + 1):
                    if r > 1 and abs(i - ci) < r and abs(j - cj) < r:
                        continue
                    for m in grid.get((i, j), ()):
                        if m == k:
                            continue
                        dx, dy = pts[m][0] - x, pts[m][1] - y
                        d2 = dx * dx + dy * dy
                        if d2 < best2:
                            best2, bi = d2, m
            if (bi >= 0 and best2 <= (r * cell) ** 2) or r > 2000:
                break
            r += 1
        dist[k] = math.sqrt(best2) if bi >= 0 else float('inf')
        who[k] = bi
    return dist, who


def worst_pairs(pts, hloc, cell):
    """For every node, the smallest d / max(h_p, h_q) over all other
    nodes q, and that q. Ratios are tracked up to 2. A pair with
    ratio < 2 has d < 2 * max(h), so searching each node out to twice
    the largest h in the whole field finds every such pair. (Searching
    only to max(h) finds every CLOSE pair, ratio < 1, but misses
    neighbours between 1 and 2, which biases the median.)"""
    reach = 2.0 * max(hloc)
    grid = {}
    for k, (x, y) in enumerate(pts):
        grid.setdefault((int(math.floor(x / cell)),
                         int(math.floor(y / cell))), []).append(k)
    nr = int(math.ceil(reach / cell))
    worst = [float('inf')] * len(pts)
    who = [-1] * len(pts)
    for k, (x, y) in enumerate(pts):
        ci, cj = int(math.floor(x / cell)), int(math.floor(y / cell))
        hk = hloc[k]
        for i in range(ci - nr, ci + nr + 1):
            for j in range(cj - nr, cj + nr + 1):
                for m in grid.get((i, j), ()):
                    if m <= k:
                        continue        # each pair once
                    dx, dy = pts[m][0] - x, pts[m][1] - y
                    d2 = dx * dx + dy * dy
                    H = hk if hk > hloc[m] else hloc[m]
                    if d2 >= H * H * 4.0:
                        continue        # ratio >= 2, of no interest
                    r = math.sqrt(d2) / H
                    if r < worst[k]:
                        worst[k], who[k] = r, m
                    if r < worst[m]:
                        worst[m], who[m] = r, k
    return worst, who


def tri_measures(a, b, c):
    sa = ((b[0]-a[0])*(c[1]-a[1]) - (b[1]-a[1])*(c[0]-a[0])) / 2.0
    la, lb, lc = math.dist(b, c), math.dist(a, c), math.dist(a, b)
    A = abs(sa)
    if A == 0.0 or la * lb * lc == 0.0:
        return sa, 0.0, 180.0, 0.0
    angs = []
    for (o, p, q) in ((la, lb, lc), (lb, la, lc), (lc, la, lb)):
        cv = (p * p + q * q - o * o) / (2.0 * p * q)
        angs.append(math.degrees(math.acos(max(-1.0, min(1.0, cv)))))
    R = la * lb * lc / (4.0 * A)
    r = A / ((la + lb + lc) / 2.0)
    return sa, min(angs), max(angs), 2.0 * r / R


def integral_inv_h2(poly, hfun, n=400):
    """Midpoint rule for the integral of 1/h^2 over the polygon."""
    xs = [p[0] for p in poly]
    ys = [p[1] for p in poly]
    x0, x1, y0, y1 = min(xs), max(xs), min(ys), max(ys)
    dx, dy = (x1 - x0) / n, (y1 - y0) / n
    s = 0.0
    for i in range(n):
        x = x0 + (i + 0.5) * dx
        for j in range(n):
            y = y0 + (j + 0.5) * dy
            if inside_or_on((x, y), poly, 0.0):
                s += 1.0 / hfun(x, y) ** 2
    return s * dx * dy


def pctl(v, p):
    if not v:
        return float('nan')
    return v[min(len(v) - 1, max(0, int(p * len(v))))]


# ---------------------------------------------------------------------
# Picture
# ---------------------------------------------------------------------
def angle_colour(a):
    if a < 10.0:
        return "#b30000"
    if a < 20.0:
        return "#e34a33"
    if a < 30.0:
        return "#fdbb84"
    if a < 40.0:
        return "#d9f0a3"
    return "#78c679"


def frame(x0, y0, w, h, title):
    return [f'<text x="{x0}" y="{y0 - 6}" font-family="monospace" '
            f'font-size="12">{title}</text>',
            f'<rect x="{x0}" y="{y0}" width="{w}" height="{h}" '
            'fill="none" stroke="#999"/>']


def axis_labels(x0, y0, w, h, xlo, xhi, ylo, yhi, yfmt="{:g}"):
    o = []
    for v in (xlo, (xlo + xhi) / 2.0, xhi):
        xx = x0 + (v - xlo) / (xhi - xlo) * w
        o.append(f'<text x="{xx:.1f}" y="{y0 + h + 13}" '
                 'font-family="monospace" font-size="10" '
                 f'text-anchor="middle">{v:g}</text>')
    if yhi is not None:
        for v, yy in ((ylo, y0 + h), (yhi, y0 + 8)):
            o.append(f'<text x="{x0 - 4}" y="{yy:.1f}" '
                     'font-family="monospace" font-size="10" '
                     f'text-anchor="end">{yfmt.format(v)}</text>')
    return o


def histogram(x0, y0, w, h, vals, lo, hi, nbin, title, mark=None,
              mark_label=None):
    counts = [0] * nbin
    for v in vals:
        k = int((v - lo) / (hi - lo) * nbin)
        counts[max(0, min(nbin - 1, k))] += 1
    cmax = max(counts) or 1
    bw = w / nbin
    o = frame(x0, y0, w, h, title)
    for k, cnt in enumerate(counts):
        bh = h * cnt / cmax
        o.append(f'<rect x="{x0 + k*bw:.2f}" y="{y0 + h - bh:.2f}" '
                 f'width="{bw*0.9:.2f}" height="{bh:.2f}" '
                 'fill="#4a78a8"/>')
    o += axis_labels(x0, y0, w, h, lo, hi, 0, None)
    if mark is not None and lo <= mark <= hi:
        xx = x0 + (mark - lo) / (hi - lo) * w
        o.append(f'<line x1="{xx:.1f}" y1="{y0}" x2="{xx:.1f}" '
                 f'y2="{y0 + h}" stroke="#d62828" stroke-width="1.5" '
                 'stroke-dasharray="4,3"/>')
        if mark_label:
            o.append(f'<text x="{xx + 4:.1f}" y="{y0 + 13}" '
                     'font-family="monospace" font-size="11" '
                     'stroke="white" stroke-width="3" '
                     'paint-order="stroke" '
                     f'fill="#d62828">{mark_label}</text>')
    return o


def scatter_vs_x(x0, y0, w, h, nodes, dist, hfun, poly, onb):
    """Nearest neighbour distance against x, with h(x) drawn on top.
    This is the picture of field tracking: the dots should hug the
    curve all the way across."""
    xs = [p[0] for p in poly]
    xlo, xhi = min(xs), max(xs)
    ymid = sum(p[1] for p in poly) / len(poly)
    yhi = max(max(d for d in dist if d != float('inf')),
              max(hfun(xlo + (xhi - xlo) * i / 100.0, ymid)
                  for i in range(101))) * 1.1
    # Legend as coloured words rather than "red=..." text: the panel
    # is only about 50 monospace characters wide.
    o = frame(x0, y0, w, h,
              'neighbour distance vs x: '
              '<tspan fill="#d62828">h(x)</tspan> '
              '<tspan fill="#4a78a8">interior</tspan> '
              '<tspan fill="#e07b00">boundary</tspan>')
    for ((x, y), d, b) in zip(nodes, dist, onb):
        if d == float('inf'):
            continue
        X = x0 + (x - xlo) / (xhi - xlo) * w
        Y = y0 + h - d / yhi * h
        col = "#e07b00" if b else "#4a78a8"
        o.append(f'<circle cx="{X:.1f}" cy="{Y:.1f}" r="1.1" '
                 f'fill="{col}" fill-opacity="0.7"/>')
    curve = []
    for i in range(201):
        x = xlo + (xhi - xlo) * i / 200.0
        curve.append(f"{x0 + i / 200.0 * w:.1f},"
                     f"{y0 + h - hfun(x, ymid) / yhi * h:.1f}")
    o.append(f'<polyline points="{" ".join(curve)}" fill="none" '
             'stroke="#d62828" stroke-width="1.5"/>')
    o += axis_labels(x0, y0, w, h, xlo, xhi, 0.0, yhi, "{:.3g}")
    return o


def write_svg(fn, title, poly, nodes, close, outpts, tris, tq, badsign,
              dist, ratio, hfun, onb):
    P = 440.0
    pad = 20.0
    top = 50.0
    xs = [p[0] for p in poly] + [p[0] for p in nodes]
    ys = [p[1] for p in poly] + [p[1] for p in nodes]
    x0, x1, y0, y1 = min(xs), max(xs), min(ys), max(ys)
    sc = (P - 2 * pad) / (max(x1 - x0, y1 - y0) or 1.0)
    W, H = 3 * P, P + top + 30

    def T(q, panel):
        return (panel * P + pad + (q[0] - x0) * sc,
                top + pad + (y1 - q[1]) * sc)

    def pts(seq, panel):
        return " ".join("%.2f,%.2f" % T(q, panel) for q in seq)

    o = [f'<svg xmlns="http://www.w3.org/2000/svg" width="{W:.0f}" '
         f'height="{H:.0f}" viewBox="0 0 {W:.0f} {H:.0f}">',
         '<rect width="100%" height="100%" fill="white"/>',
         f'<text x="{pad}" y="18" font-family="monospace" '
         f'font-size="13">{title}</text>',
         f'<text x="{pad}" y="36" font-family="monospace" font-size="12">'
         'left: nodes, <tspan fill="#d62828">red = pair closer than '
         'max(h_p,h_q) (not both on boundary)</tspan>, <tspan fill="#c026d3">magenta = outside</tspan>'
         '   middle: smallest angle '
         '<tspan fill="#b30000">&lt;10</tspan> '
         '<tspan fill="#e34a33">&lt;20</tspan> '
         '<tspan fill="#c98a50">&lt;30</tspan> '
         '<tspan fill="#8fae4a">&lt;40</tspan> '
         '<tspan fill="#3f8f3a">&gt;=40</tspan>, black = inverted'
         '</text>']

    # Panel 0: nodes
    dot = 1.2
    o.append(f'<polygon points="{pts(poly, 0)}" fill="#f7f7f7" '
             'stroke="black" stroke-width="1"/>')
    for q in nodes:
        X, Y = T(q, 0)
        o.append(f'<circle cx="{X:.2f}" cy="{Y:.2f}" r="{dot}" '
                 'fill="#333"/>')
    for (i, j) in close:
        if onb[i] and onb[j]:
            continue            # boundary spacing is short by design
        (Xa, Ya), (Xb, Yb) = T(nodes[i], 0), T(nodes[j], 0)
        o.append(f'<line x1="{Xa:.2f}" y1="{Ya:.2f}" x2="{Xb:.2f}" '
                 f'y2="{Yb:.2f}" stroke="#d62828" stroke-width="2"/>')
    for k in outpts:
        X, Y = T(nodes[k], 0)
        o.append(f'<circle cx="{X:.2f}" cy="{Y:.2f}" r="5" fill="none"'
                 ' stroke="#c026d3" stroke-width="2"/>')

    # Panel 1: mesh by smallest angle
    for t, (sa, amin, amax, q) in zip(tris, tq):
        bad = sa == 0.0 or (sa > 0.0) == (badsign > 0.0)
        o.append(f'<polygon points="{pts([nodes[k] for k in t], 1)}" '
                 f'fill="{"black" if bad else angle_colour(amin)}" '
                 'stroke="#555" stroke-width="0.25"/>')
    o.append(f'<polygon points="{pts(poly, 1)}" fill="none" '
             'stroke="black" stroke-width="1"/>')

    # Panel 2: tracking scatter and two histograms
    hx, hw, hh = 2 * P + 50, P - 80, 105
    o += scatter_vs_x(hx, top + 20, hw, hh, nodes, dist, hfun, poly,
                      onb)
    o += histogram(hx, top + 165, hw, hh,
                   [r for r, b in zip(ratio, onb) if not b], 0.0, 2.0,
                   40, "interior separation  d / max(h_p, h_q)",
                   mark=1.0, mark_label="d = h")
    o += histogram(hx, top + 310, hw, hh, [m[1] for m in tq], 0.0, 60.0,
                   30, "smallest triangle angle (deg)",
                   mark=POOR_ANGLE_DEG,
                   mark_label=f"{POOR_ANGLE_DEG:g} deg poor")
    o.append('</svg>')
    with open(fn, 'w') as f:
        f.write("\n".join(o) + "\n")


# ---------------------------------------------------------------------
def verify_field(fn, hfun):
    nodes, _, _, att = read_avs(fn)
    if 'h_field_att' not in att:
        print(f"  -> HARD: no h_field_att attribute in {fn}")
        return 1
    worst = 0.0
    for (x, y), v in zip(nodes, att['h_field_att']):
        f = hfun(x, y)
        worst = max(worst, abs(v - f) / f)
    print(f"  field_nodes       {len(nodes):14d}")
    print(f"  field_max_relerr  {worst:14.3e}")
    if worst > FIELD_TOL:
        print("  -> HARD: the field in this checker does not match "
              "the dumped field. user_h_field_att.mlgi has changed; "
              "update make_field().")
        print("  HARD: FAIL")
        return 1
    print("  HARD: pass")
    return 0


def main():
    ap = argparse.ArgumentParser(
        description="see the module docstring for what is measured")
    ap.add_argument('--poly')
    ap.add_argument('--mesh')
    ap.add_argument('--h0', type=float, required=True)
    ap.add_argument('--field', default='uniform',
                    choices=['uniform', 'absx'])
    ap.add_argument('--width', type=float, default=1.0)
    ap.add_argument('--svg')
    ap.add_argument('--verify-field', dest='verify')
    a = ap.parse_args()
    hfun = make_field(a.field, a.h0, a.width)

    if a.verify:
        return verify_field(a.verify, hfun)
    if not (a.poly and a.mesh):
        ap.error("--poly and --mesh are required unless --verify-field")

    poly = read_poly(a.poly)
    nodes, tris, badconn, _ = read_avs(a.mesh)
    if len(poly) < 3 or not nodes:
        print("  nothing to measure: empty polygon or mesh")
        return 1

    pa = abs(signed_area(poly))
    xs = [p[0] for p in poly]
    ys = [p[1] for p in poly]
    diag = math.hypot(max(xs) - min(xs), max(ys) - min(ys))
    tol2 = (1.0e-9 * diag) ** 2

    outpts = [k for k, p in enumerate(nodes)
              if not inside_or_on(p, poly, tol2)]

    n = len(poly)
    btol2 = (1.0e-7 * diag) ** 2
    onb = [any(seg_dist2(p, poly[i], poly[(i + 1) % n]) <= btol2
               for i in range(n)) for p in nodes]

    hloc = [hfun(x, y) for (x, y) in nodes]
    # Tracking: nearest neighbour distance / h at the node.
    dist, _ = nearest_neighbours(nodes, min(hloc))
    ratio = [d / hh for d, hh in zip(dist, hloc)]

    # Separation: every pair against the larger of its two h.
    sep, swho = worst_pairs(nodes, hloc, min(hloc))
    r_in = sorted(r for r, b in zip(sep, onb) if not b)
    r_bd = sorted(r for r, b in zip(sep, onb) if b)
    close_in = sum(1 for r in r_in if r < CLOSE_FACTOR)
    close_bd = sum(1 for r in r_bd if r < CLOSE_FACTOR)
    close = set()
    for k, r in enumerate(sep):
        if r < CLOSE_FACTOR and swho[k] >= 0:
            close.add((min(k, swho[k]), max(k, swho[k])))

    # Tracking: band by local h.
    hlo, hhi = min(hloc), max(hloc)
    bands = []
    if hhi > hlo * (1.0 + 1.0e-9):
        edges = [hlo + (hhi - hlo) * i / NBANDS for i in range(NBANDS + 1)]
        for b in range(NBANDS):
            vals = sorted(r for r, hh, bd in zip(ratio, hloc, onb)
                          if not bd and
                          (edges[b] <= hh < edges[b + 1] or
                           (b == NBANDS - 1 and hh == hhi)))
            bands.append((edges[b], edges[b + 1], len(vals),
                          pctl(vals, 0.5)))
    usable = [m for (_, _, n, m) in bands if n >= MIN_BAND]
    spread = max(usable) / min(usable) if len(usable) >= 2 else 1.0

    vfound = sum(1 for (px, py) in poly
                 if any((qx-px)**2 + (qy-py)**2 <= tol2
                        for (qx, qy) in nodes))

    tq = [tri_measures(nodes[i], nodes[j], nodes[k])
          for (i, j, k) in tris]
    npos = sum(1 for m in tq if m[0] > 0.0)
    nneg = sum(1 for m in tq if m[0] < 0.0)
    ninv = min(npos, nneg)
    badsign = -1.0 if npos >= nneg else 1.0
    ma = sum(abs(m[0]) for m in tq)
    amins = sorted(m[1] for m in tq)
    amax = max((m[2] for m in tq), default=float('nan'))
    rr = sorted(m[3] for m in tq)
    npoor = sum(1 for v in amins if v < POOR_ANGLE_DEG)
    dens = len(nodes) / integral_inv_h2(poly, hfun)

    print(f"  polygon_area      {pa:14.6f}")
    print(f"  nodes             {len(nodes):14d}")
    print(f"  elements          {len(tris):14d}")
    print(f"  h_range           {hlo:10.4f} {hhi:10.4f}")
    print(f"  density           {dens:14.4f}")
    print(f"  interior_nodes    {len(r_in):14d}")
    print(f"  boundary_nodes    {len(r_bd):14d}")
    print(f"  sep_min           {r_in[0] if r_in else 0.0:14.4f}")
    print(f"  sep_p01           {pctl(r_in, 0.01):14.4f}")
    print(f"  sep_median        {pctl(r_in, 0.50):14.4f}")
    print(f"  close_points      {close_in:14d}")
    print(f"  bnd_sep_min       {r_bd[0] if r_bd else 0.0:14.4f}")
    print(f"  bnd_sep_median    {pctl(r_bd, 0.50):14.4f}")
    print(f"  bnd_close_points  {close_bd:14d}")
    print(f"  tracking_spread   {spread:14.4f}")
    for (lo, hi, n, m) in bands:
        print(f"  band  h {lo:8.4f} - {hi:8.4f}   nodes {n:6d}   "
              f"median d/h {m:7.4f}")
    print(f"  vertices_found    {vfound:8d} / {len(poly)}")
    print(f"  area_pct          {100.0 * (ma - pa) / pa:+14.4f}")
    print(f"  angle_min         {amins[0] if amins else 0.0:14.2f}")
    print(f"  angle_min_p01     {pctl(amins, 0.01):14.2f}")
    print(f"  angle_min_median  {pctl(amins, 0.50):14.2f}")
    print(f"  angle_max         {amax:14.2f}")
    print(f"  poor_pct          "
          f"{100.0 * npoor / max(1, len(amins)):14.2f}")
    print(f"  radius_ratio_min  {rr[0] if rr else 0.0:14.4f}")
    print(f"  radius_ratio_med  {pctl(rr, 0.50):14.4f}")
    print(f"  outside_points    {len(outpts):14d}")
    print(f"  inverted          {ninv:14d}")
    if badconn:
        print(f"  bad_connectivity  {badconn:14d}")

    ok = True
    if outpts:
        print(f"  -> HARD: {len(outpts)} nodes outside the polygon")
        ok = False
    if ninv:
        print(f"  -> HARD: {ninv} inverted elements")
        ok = False
    if badconn:
        print(f"  -> HARD: {badconn} elements reference missing nodes")
        ok = False
    if not tris:
        print("  -> HARD: mesh has no triangles")
        ok = False
    print("  HARD: " + ("pass" if ok else "FAIL"))

    if a.svg:
        fdesc = (f"h = {a.h0:g}" if a.field == 'uniform' else
                 f"h = max({a.h0:g}, {a.h0:g}*{a.width:g}*|x|)")
        title = (f"{a.mesh}   {fdesc}   nodes {len(nodes)}   "
                 f"density {dens:.3f}   interior min d/h "
                 f"{r_in[0] if r_in else 0.0:.3f}   close {close_in}   "
                 f"tracking {spread:.3f}   "
                 f"min angle {amins[0] if amins else 0.0:.1f}")
        write_svg(a.svg, title, poly, nodes, sorted(close), outpts,
                  tris, tq, badsign, dist, sep, hfun, onb)
    return 0 if ok else 1


if __name__ == '__main__':
    sys.exit(main())
