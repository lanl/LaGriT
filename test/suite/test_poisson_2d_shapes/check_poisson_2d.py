#!/usr/bin/env python3
"""
Check the invariants of a 2D Poisson disk point set and the quality of
the mesh built from it.

    python3 check_poisson_2d.py <polygon>.poly <raw>.inp <mesh>.inp h
                                [picture.svg]

    raw.inp   the points straight from the sampler: createpts with
              no_connect. (The connect token is required; omitting
              it is an input error.) Spacing, containment and count are measured
              here, because the connected mesh has been SMOOTHED
              (interior points moved) and recon 1 has ADDED nodes on
              the boundary, so it no longer shows what the sampler
              produced.

    mesh.inp  the same command WITH connect. Element quality and area
              are measured here.

    h         the uniform target spacing given on the command line.

WHAT IS MEASURED

HARD checks must hold for any valid run. Failure is a bug, and they
decide the exit status.

  CONTAINMENT       every raw point inside the polygon or on its
                    boundary.
  INVERTED          mesh triangles whose signed area has the opposite
                    sign to the majority.
  BAD CONNECTIVITY  elements that reference nodes that do not exist.

SOFT measures are reported, not judged, until they are calibrated
against real output, the same way Suite B part 1 was.

  SEPARATION        for every PAIR of raw points p, q the ratio

                        d(p,q) / max(h(p), h(q))

                    i.e. each pair is judged against the LARGER of its
                    two targets. Every pair within reach is examined,
                    not only nearest neighbours, because with a
                    varying h the worst pair for a point need not be
                    its nearest. A point's separation is its smallest
                    ratio; a CLOSE pair has ratio < 1. Reported:
                    minimum, 1st percentile, median, close count.

                    h is currently uniform (the h on the command
                    line), where the rule reduces to d / h. h_at() is
                    the one place to change for a variable h field.

                    Reported SEPARATELY for interior points and for
                    points on the polygon boundary. The sampler places
                    boundary points closer than h by design (gaps of
                    0.90 to 1.37 h on the edges of test_Poisson_2D's
                    square), so pooling them hides what the interior
                    is doing.
  DENSITY           N h^2 / A. For a maximal Poisson disk sample this
                    sits near 0.7; a sampler that stops early (a
                    finite number of attempts per point) lands lower.
  VERTICES FOUND    polygon vertices present among the raw points.
  RAW vs MESH       node counts. The mesh normally has a few more,
                    added by recon 1 on boundary edges.
  AREA              mesh area against polygon area. For a convex
                    polygon, a shortfall means a corner is missing.
  ANGLES            smallest triangle angle: minimum, 1st percentile,
                    median, and the fraction below 20 degrees.
                    Largest triangle angle: maximum.
  RADIUS RATIO      2 * inradius / circumradius, 1 for equilateral.
  LARGEST GAP       largest circumradius / h. A rough measure of the
                    biggest empty circle. Measured on the SMOOTHED
                    mesh, so it is an approximation of the sampler's.

OUTPUT is one "key value" line per quantity so the runner can grep
it. Exit status 0 if the HARD checks pass, 1 if not, 2 on usage error.

With a fifth argument, also draws an SVG picture with three panels:
the raw points with close pairs marked, the mesh coloured by smallest
angle, and histograms of separation and smallest angle.
"""
import sys
import math

# Fraction of h below which two raw points count as a CLOSE PAIR.
# 1.0 means "closer than h". A small allowance absorbs coordinates
# written to about 12 significant figures by dump/avs.
CLOSE_FACTOR = 1.0 - 1.0e-9

# Smallest angle below which a triangle is counted as poor.
POOR_ANGLE_DEG = 20.0


# ---------------------------------------------------------------------
# Input
# ---------------------------------------------------------------------
def read_poly(fn):
    """AVS UCD point set: first line counts, then id x y z."""
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
    """AVS UCD as written by dump/avs. Returns node list, triangles
    as 0 based index triples, and the count of elements that
    referenced a node id that does not exist."""
    with open(fn) as f:
        lines = f.read().splitlines()
    hdr = lines[0].split()
    nn, ne = int(hdr[0]), int(hdr[1])
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
    return nodes, tris, bad


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
    """Squared distance from p to segment ab."""
    ex, ey = b[0] - a[0], b[1] - a[1]
    L = ex * ex + ey * ey
    t = 0.0 if L == 0.0 else \
        max(0.0, min(1.0, ((p[0]-a[0])*ex + (p[1]-a[1])*ey) / L))
    dx, dy = p[0] - (a[0] + t*ex), p[1] - (a[1] + t*ey)
    return dx * dx + dy * dy


def inside_or_on(pt, poly, tol2):
    """Ray casting, with points within sqrt(tol2) of an edge counted
    as inside: the sampler places points ON the boundary."""
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
    """Nearest neighbour distance and index for every point, using a
    uniform bucket grid. Pure Python; about a second for 15000
    points."""
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
                        continue        # inner rings already searched
                    for m in grid.get((i, j), ()):
                        if m == k:
                            continue
                        dx, dy = pts[m][0] - x, pts[m][1] - y
                        d2 = dx * dx + dy * dy
                        if d2 < best2:
                            best2, bi = d2, m
            # Any point beyond ring r is at least r*cell away.
            if bi >= 0 and best2 <= (r * cell) ** 2:
                break
            if r > 1000:
                break
            r += 1
        dist[k] = math.sqrt(best2) if bi >= 0 else float('inf')
        who[k] = bi
    return dist, who


def h_at(x, y, h):
    """Target spacing at (x, y). Uniform for every p2d deck so far.
    For a variable field, compute it here: everything downstream
    already uses the larger-of-the-pair rule."""
    return h


def worst_pairs(pts, hloc, cell):
    """For every point, the smallest d / max(h_p, h_q) over all other
    points q, and that q. Ratios are tracked up to 2, so each point is
    searched out to twice the largest h in the field."""
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
                        continue
                    r = math.sqrt(d2) / H
                    if r < worst[k]:
                        worst[k], who[k] = r, m
                    if r < worst[m]:
                        worst[m], who[m] = r, k
    return worst, who


def tri_measures(a, b, c):
    """Signed area, smallest and largest angle in degrees,
    circumradius, radius ratio."""
    sa = ((b[0]-a[0])*(c[1]-a[1]) - (b[1]-a[1])*(c[0]-a[0])) / 2.0
    la = math.dist(b, c)
    lb = math.dist(a, c)
    lc = math.dist(a, b)
    A = abs(sa)
    if A == 0.0 or la * lb * lc == 0.0:
        return sa, 0.0, 180.0, float('inf'), 0.0
    angs = []
    for (o, p, q) in ((la, lb, lc), (lb, la, lc), (lc, la, lb)):
        cosv = (p * p + q * q - o * o) / (2.0 * p * q)
        angs.append(math.degrees(math.acos(max(-1.0, min(1.0, cosv)))))
    R = la * lb * lc / (4.0 * A)
    r = A / ((la + lb + lc) / 2.0)
    return sa, min(angs), max(angs), R, 2.0 * r / R


def pctl(sorted_vals, p):
    if not sorted_vals:
        return float('nan')
    k = min(len(sorted_vals) - 1, max(0, int(p * len(sorted_vals))))
    return sorted_vals[k]


# ---------------------------------------------------------------------
# Picture
# ---------------------------------------------------------------------
def angle_colour(a):
    """Smallest angle -> fill colour. Red is poor, green is good."""
    if a < 10.0:
        return "#b30000"
    if a < 20.0:
        return "#e34a33"
    if a < 30.0:
        return "#fdbb84"
    if a < 40.0:
        return "#d9f0a3"
    return "#78c679"


def histogram_svg(x0, y0, w, hgt, vals, lo, hi, nbin, title, mark=None,
                  mark_label=None):
    """Bar chart of vals in [lo,hi] as SVG elements. mark draws a
    labelled vertical reference line at that value."""
    counts = [0] * nbin
    for v in vals:
        k = int((v - lo) / (hi - lo) * nbin)
        counts[max(0, min(nbin - 1, k))] += 1   # clamp, never drop
    cmax = max(counts) or 1
    bw = w / nbin
    o = [f'<text x="{x0}" y="{y0 - 6}" font-family="monospace" '
         f'font-size="12">{title}</text>',
         f'<rect x="{x0}" y="{y0}" width="{w}" height="{hgt}" '
         'fill="none" stroke="#999"/>']
    for k, cnt in enumerate(counts):
        bh = hgt * cnt / cmax
        o.append(f'<rect x="{x0 + k*bw:.2f}" y="{y0 + hgt - bh:.2f}" '
                 f'width="{bw*0.9:.2f}" height="{bh:.2f}" '
                 'fill="#4a78a8"/>')
    for v in (lo, (lo + hi) / 2.0, hi):
        xx = x0 + (v - lo) / (hi - lo) * w
        o.append(f'<text x="{xx:.1f}" y="{y0 + hgt + 14}" '
                 'font-family="monospace" font-size="11" '
                 f'text-anchor="middle">{v:g}</text>')
    if mark is not None and lo <= mark <= hi:
        xx = x0 + (mark - lo) / (hi - lo) * w
        o.append(f'<line x1="{xx:.1f}" y1="{y0}" x2="{xx:.1f}" '
                 f'y2="{y0 + hgt}" stroke="#d62828" stroke-width="1.5" '
                 'stroke-dasharray="4,3"/>')
        if mark_label:
            o.append(f'<text x="{xx + 4:.1f}" y="{y0 + 13}" '
                     'font-family="monospace" font-size="11" '
                     'stroke="white" stroke-width="3" '
                     'paint-order="stroke" '
                     f'fill="#d62828">{mark_label}</text>')
    return o


def write_svg(fn, title, poly, raw, close, outpts, mesh, tris, tq,
              badsign, sep_in, hmin, gap, onb):
    P = 440.0                    # panel size
    pad = 20.0
    top = 50.0
    xs = [p[0] for p in poly] + [p[0] for p in raw] + [p[0] for p in mesh]
    ys = [p[1] for p in poly] + [p[1] for p in raw] + [p[1] for p in mesh]
    x0, x1, y0, y1 = min(xs), max(xs), min(ys), max(ys)
    span = max(x1 - x0, y1 - y0) or 1.0
    sc = (P - 2 * pad) / span
    W = 3 * P
    H = P + top + 10

    def T(q, panel):
        return (panel * P + pad + (q[0] - x0) * sc,
                top + pad + (y1 - q[1]) * sc)

    def pts(seq, panel):
        return " ".join(f"{T(q, panel)[0]:.2f},{T(q, panel)[1]:.2f}"
                        for q in seq)

    dot = max(0.6, min(2.5, 0.12 * hmin * sc))
    o = [f'<svg xmlns="http://www.w3.org/2000/svg" width="{W:.0f}" '
         f'height="{H:.0f}" viewBox="0 0 {W:.0f} {H:.0f}">',
         '<rect width="100%" height="100%" fill="white"/>',
         f'<text x="{pad}" y="18" font-family="monospace" '
         f'font-size="13">{title}</text>',
         f'<text x="{pad}" y="36" font-family="monospace" font-size="12">'
         'left: raw points, <tspan fill="#d62828">red = pair closer '
         'than h (not both on boundary)</tspan>   middle: mesh by smallest angle'
         ' <tspan fill="#b30000">&lt;10</tspan> '
         '<tspan fill="#e34a33">&lt;20</tspan> '
         '<tspan fill="#c98a50">&lt;30</tspan> '
         '<tspan fill="#8fae4a">&lt;40</tspan> '
         '<tspan fill="#3f8f3a">&gt;=40</tspan>, '
         '<tspan fill="#1d4ed8">blue = largest gap</tspan>, '
         'black = inverted, '
         '<tspan fill="#c026d3">magenta = point outside</tspan>'
         '</text>']

    # Panel 0: raw points
    o.append(f'<polygon points="{pts(poly, 0)}" fill="#f7f7f7" '
             'stroke="black" stroke-width="1"/>')
    for (x, y) in raw:
        X, Y = T((x, y), 0)
        o.append(f'<circle cx="{X:.2f}" cy="{Y:.2f}" r="{dot:.2f}" '
                 'fill="#333"/>')
    for k in outpts:
        X, Y = T(raw[k], 0)
        o.append(f'<circle cx="{X:.2f}" cy="{Y:.2f}" '
                 f'r="{max(4.0, 3*dot):.2f}" fill="none" '
                 'stroke="#c026d3" stroke-width="2"/>')
    for (i, j) in close:
        if onb[i] and onb[j]:
            continue            # boundary spacing is short by design
        (Xa, Ya), (Xb, Yb) = T(raw[i], 0), T(raw[j], 0)
        o.append(f'<line x1="{Xa:.2f}" y1="{Ya:.2f}" x2="{Xb:.2f}" '
                 f'y2="{Yb:.2f}" stroke="#d62828" stroke-width="2"/>')
        for (X, Y) in ((Xa, Ya), (Xb, Yb)):
            o.append(f'<circle cx="{X:.2f}" cy="{Y:.2f}" '
                     f'r="{max(3.0, 2*dot):.2f}" fill="none" '
                     'stroke="#d62828" stroke-width="1.5"/>')

    # Panel 1: mesh by smallest angle
    for t, (sa, amin, amax, R, q) in zip(tris, tq):
        bad = sa == 0.0 or (sa > 0.0) == (badsign > 0.0)
        fill = "black" if bad else angle_colour(amin)
        o.append(f'<polygon points="{pts([mesh[k] for k in t], 1)}" '
                 f'fill="{fill}" stroke="#555" stroke-width="0.25"/>')
    o.append(f'<polygon points="{pts(poly, 1)}" fill="none" '
             'stroke="black" stroke-width="1"/>')
    if gap is not None:
        (cx, cy), R = gap
        X, Y = T((cx, cy), 1)
        o.append(f'<circle cx="{X:.2f}" cy="{Y:.2f}" r="{R*sc:.2f}" '
                 'fill="none" stroke="#1d4ed8" stroke-width="1.5" '
                 'stroke-dasharray="5,3"/>')

    # Panel 2: histograms
    hx = 2 * P + 50
    o += histogram_svg(hx, top + 30, P - 80, 140, sep_in, 0.0, 2.0, 40,
                       "interior separation  d / max(h_p, h_q)",
                       mark=1.0, mark_label="d = h")
    o += histogram_svg(hx, top + 240, P - 80, 140,
                       [m[1] for m in tq], 0.0, 60.0, 30,
                       "smallest triangle angle (deg)",
                       mark=POOR_ANGLE_DEG,
                       mark_label=f"{POOR_ANGLE_DEG:g} deg poor")
    o.append('</svg>')
    with open(fn, 'w') as f:
        f.write("\n".join(o) + "\n")


# ---------------------------------------------------------------------
def main():
    if len(sys.argv) not in (5, 6):
        print(__doc__)
        return 2
    poly = read_poly(sys.argv[1])
    raw, _, _ = read_avs(sys.argv[2])
    mesh, tris, badconn = read_avs(sys.argv[3])
    h = float(sys.argv[4])

    if len(poly) < 3 or not raw:
        print("  nothing to measure: empty polygon or point set")
        return 1

    pa = abs(signed_area(poly))
    xs = [p[0] for p in poly]
    ys = [p[1] for p in poly]
    diag = math.hypot(max(xs) - min(xs), max(ys) - min(ys))
    tol2 = (1.0e-9 * diag) ** 2

    # --- raw points ------------------------------------------------
    outpts = [k for k, p in enumerate(raw)
              if not inside_or_on(p, poly, tol2)]
    outside = len(outpts)

    n = len(poly)
    btol2 = (1.0e-7 * diag) ** 2
    onb = [any(seg_dist2(p, poly[i], poly[(i + 1) % n]) <= btol2
               for i in range(n)) for p in raw]

    hloc = [h_at(x, y, h) for (x, y) in raw]
    sep, swho = worst_pairs(raw, hloc, min(hloc))
    r_in = sorted(r for r, b in zip(sep, onb) if not b)
    r_bd = sorted(r for r, b in zip(sep, onb) if b)
    close_in = sum(1 for r in r_in if r < CLOSE_FACTOR)
    close_bd = sum(1 for r in r_bd if r < CLOSE_FACTOR)
    close = set()
    for k, r in enumerate(sep):
        if r < CLOSE_FACTOR and swho[k] >= 0:
            close.add((min(k, swho[k]), max(k, swho[k])))
    sep_min = r_in[0] if r_in else float('nan')

    vfound = 0
    for (px, py) in poly:
        if any((qx-px)**2 + (qy-py)**2 <= tol2 for (qx, qy) in raw):
            vfound += 1

    # --- mesh -------------------------------------------------------
    tq = [tri_measures(mesh[a], mesh[b], mesh[c]) for (a, b, c) in tris]
    npos = sum(1 for m in tq if m[0] > 0.0)
    nneg = sum(1 for m in tq if m[0] < 0.0)
    ninv = min(npos, nneg)
    badsign = -1.0 if npos >= nneg else 1.0   # the minority sign
    ma = sum(abs(m[0]) for m in tq)
    amins = sorted(m[1] for m in tq)
    amax = max((m[2] for m in tq), default=float('nan'))
    rr = sorted(m[4] for m in tq)
    npoor = sum(1 for a in amins if a < POOR_ANGLE_DEG)
    gap = None
    if tq:
        k = max(range(len(tq)), key=lambda i: tq[i][3])
        a, b, c = (mesh[i] for i in tris[k])
        d = 2.0 * ((a[0]*(b[1]-c[1]) + b[0]*(c[1]-a[1]) +
                    c[0]*(a[1]-b[1])))
        if d != 0.0:
            ux = ((a[0]**2 + a[1]**2)*(b[1]-c[1]) +
                  (b[0]**2 + b[1]**2)*(c[1]-a[1]) +
                  (c[0]**2 + c[1]**2)*(a[1]-b[1])) / d
            uy = ((a[0]**2 + a[1]**2)*(c[0]-b[0]) +
                  (b[0]**2 + b[1]**2)*(a[0]-c[0]) +
                  (c[0]**2 + c[1]**2)*(b[0]-a[0])) / d
            gap = ((ux, uy), tq[k][3])

    # --- report -----------------------------------------------------
    print(f"  polygon_area      {pa:14.6f}")
    print(f"  raw_points        {len(raw):14d}")
    print(f"  mesh_nodes        {len(mesh):14d}")
    print(f"  mesh_extra_nodes  {len(mesh) - len(raw):14d}")
    print(f"  elements          {len(tris):14d}")
    print(f"  density           {len(raw) * h * h / pa:14.4f}")
    print(f"  interior_points   {len(r_in):14d}")
    print(f"  boundary_points   {len(r_bd):14d}")
    print(f"  sep_min           {sep_min:14.4f}")
    print(f"  sep_p01           {pctl(r_in, 0.01):14.4f}")
    print(f"  sep_median        {pctl(r_in, 0.50):14.4f}")
    print(f"  close_points      {close_in:14d}")
    print(f"  bnd_sep_min       "
          f"{r_bd[0] if r_bd else float('nan'):14.4f}")
    print(f"  bnd_sep_median    {pctl(r_bd, 0.50):14.4f}")
    print(f"  bnd_close_points  {close_bd:14d}")
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
    print(f"  largest_gap       "
          f"{(gap[1] / h if gap else float('nan')):14.4f}")
    print(f"  outside_points    {outside:14d}")
    print(f"  inverted          {ninv:14d}")
    if badconn:
        print(f"  bad_connectivity  {badconn:14d}")

    ok = True
    if outside:
        print(f"  -> HARD: {outside} raw points outside the polygon")
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

    if len(sys.argv) == 6:
        title = (f"{sys.argv[3]}   h {h:g}   points {len(raw)}   "
                 f"density {len(raw)*h*h/pa:.3f}   interior min "
                 f"sep/h {sep_min:.3f}   close {close_in}   min angle "
                 f"{amins[0] if amins else 0.0:.1f}")
        write_svg(sys.argv[5], title, poly, raw, sorted(close), outpts,
                  mesh, tris, tq, badsign, r_in, h, gap, onb)
    return 0 if ok else 1


if __name__ == '__main__':
    sys.exit(main())
