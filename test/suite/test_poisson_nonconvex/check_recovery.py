#!/usr/bin/env python3
"""
Measure how faithfully the nonconvex clip recovered the polygon.

    python3 check_recovery.py <polygon>.poly <mesh>.inp [picture.svg]

With a third argument, also draws the result as an SVG picture, which
opens in any web browser. It is the quickest way to SEE what a
degraded recovery looks like:

    orange      polygon area NOT covered by the mesh      (uncovered)
    red         elements reaching OUTSIDE the polygon     (overhang)
    grey        elements wholly inside the polygon
    black line  the true polygon outline
    blue ring   polygon vertex that is not a mesh node

The clip works by triangulating the polygon outline, interpolating a
marker onto the convex hull Delaunay mesh built from the Poisson
points, and deleting every element whose centroid falls outside.

WHAT IS MEASURED, AND WHY THERE ARE TWO KINDS OF CHECK

HARD checks are properties that must hold at ANY sample spacing. A
failure is a bug, not a resolution limit. They decide the exit status.

  CENTROIDS OUTSIDE   elements whose centroid is outside the polygon
                      by more than VERTEX_TOL_REL x the bounding box
                      diagonal (closer than that counts as ON the
                      outline). Each is listed with its distance
                      outside, absolute and / the shortest polygon edge,
                      which separates a real clip miss from rounding.
                      The clip deletes exactly these, using the same
                      centroid rule, so the count should be ZERO BY
                      CONSTRUCTION. It is a "did the clip run" detector
                      (it catches a test_convex regression that skips
                      the clip) and says NOTHING about how well the
                      outline was recovered.

  MIXED ORIENTATION   triangles whose signed area has the opposite
                      sign to the majority, i.e. inverted elements.
                      Compared against the majority rather than a
                      fixed sign so the check does not depend on the
                      winding convention dump/avs uses.

SOFT measures describe recovery quality. They are EXPECTED to degrade
as h grows relative to the tooth width, so this script only REPORTS
the verdict. Whether "degraded" is a failure depends on what the deck
expects, and that decision is made by the runner.

  OVERHANG            mesh area lying OUTSIDE the polygon
  UNCOVERED           polygon area NOT covered by the mesh
  NET AREA RATIO      mesh area / polygon area

  The net ratio is kept for continuity but is the weakest of the
  three: a triangle bridging a notch adds area, a thin tooth losing
  a triangle whose centroid fell outside removes area, and the two
  CANCEL. Overhang and uncovered are one sided and cannot cancel.
  Their sum is the area of the symmetric difference, reported as
  MISMATCH, which is the single number to watch across the h sweep.

  VERTICES RECOVERED  polygon vertices that coincide with a mesh node.
                      A tooth tip that is not a node cannot be
                      reproduced, however fine the interior sampling.

There is no reference file. The expected result is a FUNCTION of the
tooth width to sample spacing ratio, not a fixed number, so a stored
reference would say nothing about whether the answer is right.

OUTPUT is one "key value" line per quantity so the runner can grep it.
Exit status 0 if the HARD checks pass, 1 if not, 2 on usage error.
"""
import sys

# SOFT tolerance on mismatch / polygon area, in per cent.
#
# CALIBRATED on saw_safe.poly (tooth half width 0.625, default seed).
# Two runs, before and after the polygons were regenerated with
# valleys at both ends:
#
#     h    half width / h   mismatch %, first run   latest run
#     0.1       6.25              0.0000              0.0000
#     0.2       3.12              0.0299              0.0000
#     0.4       1.56              0.1343              0.0000
#     0.6       1.04              0.0000              0.0000
#     0.8       0.78              1.6326              0.6718
#     1.0       0.62              1.5931              2.4795
#     1.2       0.52              3.0317              6.0593
#
# Both runs split at half width / h = 1. In the latest run the gap is
# 0.00 % (worst recovered) to 0.67 % (best degraded, nc05), so 0.5
# still separates them but with LESS margin on the degraded side than
# before (was 0.13 % to 1.59 %). Other polygons, latest run: nc08
# 0.21 %, nc12 0.02 %, nc13 0.04 %, all recovered. One seed per h;
# re-check if the seed, the sampler or the polygons change.
MISMATCH_TOL_PCT = 0.5

# Distance within which a mesh node counts as sitting on a polygon
# vertex, relative to the polygon bounding box diagonal. dump/avs
# writes coordinates in E format to about 12 significant figures.
VERTEX_TOL_REL = 1.0e-8


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


def read_mesh(fn):
    """AVS UCD mesh written by dump/avs. Returns nodes, triangles."""
    with open(fn) as f:
        lines = [l.rstrip('\n') for l in f]
    hdr = lines[0].split()
    nn, ne = int(hdr[0]), int(hdr[1])
    nodes = {}
    for l in lines[1:1 + nn]:
        p = l.split()
        nodes[int(p[0])] = (float(p[1]), float(p[2]))
    tris = []
    for l in lines[1 + nn:1 + nn + ne]:
        p = l.split()
        if len(p) < 6:
            continue
        if p[2].lower() == 'tri':
            tris.append(tuple(int(x) for x in p[3:6]))
    return nodes, tris


def signed_area(pts):
    a = 0.0
    n = len(pts)
    for i in range(n):
        x1, y1 = pts[i]
        x2, y2 = pts[(i + 1) % n]
        a += x1 * y2 - x2 * y1
    return a / 2.0


def inside(pt, poly):
    """Ray casting point in polygon."""
    x, y = pt
    n = len(poly)
    c = False
    j = n - 1
    for i in range(n):
        xi, yi = poly[i]
        xj, yj = poly[j]
        if ((yi > y) != (yj > y)) and \
           (x < (xj - xi) * (y - yi) / (yj - yi + 1e-300) + xi):
            c = not c
        j = i
    return c


def dist_to_outline(pt, poly):
    """Shortest distance from pt to the polygon outline."""
    x, y = pt
    best = float("inf")
    n = len(poly)
    for i in range(n):
        (x1, y1), (x2, y2) = poly[i], poly[(i + 1) % n]
        dx, dy = x2 - x1, y2 - y1
        L2 = dx * dx + dy * dy
        t = 0.0 if L2 == 0.0 else max(0.0, min(1.0,
            ((x - x1) * dx + (y - y1) * dy) / L2))
        ex, ey = x1 + t * dx - x, y1 + t * dy - y
        best = min(best, (ex * ex + ey * ey) ** 0.5)
    return best


def clip_to_triangle(poly, tri):
    """
    Sutherland-Hodgman: clip the (possibly nonconvex) polygon by the
    CCW triangle. The clipper must be convex; the subject need not be.
    The result may contain zero width slivers where a notch was cut,
    but its signed area is exact, which is all that is used.
    """
    out = list(poly)
    for k in range(3):
        if not out:
            break
        ax, ay = tri[k]
        bx, by = tri[(k + 1) % 3]
        ex, ey = bx - ax, by - ay

        def side(p):
            return ex * (p[1] - ay) - ey * (p[0] - ax)

        inp, out = out, []
        s = inp[-1]
        ss = side(s)
        for e in inp:
            se = side(e)
            if se >= 0.0:
                if ss < 0.0:
                    t = ss / (ss - se)
                    out.append((s[0] + t*(e[0]-s[0]), s[1] + t*(e[1]-s[1])))
                out.append(e)
            elif ss >= 0.0:
                t = ss / (ss - se)
                out.append((s[0] + t*(e[0]-s[0]), s[1] + t*(e[1]-s[1])))
            s, ss = e, se
    return abs(signed_area(out)) if len(out) >= 3 else 0.0


def write_svg(fn, poly, drawn, lost, title):
    """
    Draw polygon, mesh and failures, with the failure regions drawn
    EXACTLY rather than by whole element:

      1. polygon filled orange
      2. mesh filled grey, CLIPPED to the polygon.  Orange still
         showing after this is polygon area the mesh does not cover.
      3. mesh filled red, MASKED to outside the polygon. Only the part
         of the mesh that reaches outside the outline shows.
      4. mesh edges, polygon outline, missing vertex rings on top.
    """
    # Clip and mask IDs must be unique across a whole HTML page, or
    # every picture inlined after the first is clipped by the FIRST
    # picture's polygon. The runner inlines all of them into one
    # summary page, so derive the IDs from the file name.
    tag = "".join(ch if ch.isalnum() else "_"
                  for ch in fn.split("/")[-1].rsplit(".", 1)[0])
    W = 1400.0
    pad = 20.0
    top = 44.0
    xs = [p[0] for p in poly]
    ys = [p[1] for p in poly]
    x0, x1, y0, y1 = min(xs), max(xs), min(ys), max(ys)
    for (a, b, c, bad) in drawn:
        for q in (a, b, c):
            x0, x1 = min(x0, q[0]), max(x1, q[0])
            y0, y1 = min(y0, q[1]), max(y1, q[1])
    sc = (W - 2*pad) / max(x1 - x0, y1 - y0)
    H = (y1 - y0) * sc + 2*pad + top

    def P(q):
        return f"{pad + (q[0]-x0)*sc:.2f},{top + pad + (y1-q[1])*sc:.2f}"

    ppts = " ".join(P(q) for q in poly)
    tris = "".join(f'<polygon points="{P(a)} {P(b)} {P(c)}"/>'
                   for (a, b, c, bad) in drawn)
    o = [f'<svg xmlns="http://www.w3.org/2000/svg" width="{W:.0f}" '
         f'height="{H:.0f}" viewBox="0 0 {W:.0f} {H:.0f}">',
         '<defs>',
         f'<clipPath id="inpoly_{tag}"><polygon points="{ppts}"/></clipPath>',
         f'<mask id="outpoly_{tag}">',
         '<rect width="100%" height="100%" fill="white"/>',
         f'<polygon points="{ppts}" fill="black"/>',
         '</mask>',
         '</defs>',
         '<rect width="100%" height="100%" fill="white"/>',
         f'<text x="{pad}" y="18" font-family="monospace" '
         f'font-size="13">{title}</text>',
         f'<text x="{pad}" y="36" font-family="monospace" '
         'font-size="12">'
         '<tspan fill="#e07b00">orange = uncovered</tspan>   '
         '<tspan fill="#d62828">red = overhang</tspan>   '
         '<tspan fill="#1d4ed8">blue ring = vertex not a node</tspan>'
         '</text>',
         f'<polygon points="{ppts}" fill="#f4a261"/>',
         f'<g clip-path="url(#inpoly_{tag})" fill="#d9d9d9">{tris}</g>',
         f'<g mask="url(#outpoly_{tag})" fill="#d62828">{tris}</g>',
         f'<g fill="none" stroke="#888" stroke-width="0.3">{tris}</g>',
         f'<polygon points="{ppts}" fill="none" stroke="black" '
         'stroke-width="1.2"/>']
    for (k, px, py, kind, d) in lost:
        x, y = P((px, py)).split(',')
        o.append(f'<circle cx="{x}" cy="{y}" r="6" fill="none" '
                 'stroke="#1d4ed8" stroke-width="2"/>')
    o.append('</svg>')
    with open(fn, 'w') as f:
        f.write("\n".join(o) + "\n")


def main():
    if len(sys.argv) not in (3, 4):
        print(__doc__)
        return 2
    poly = read_poly(sys.argv[1])
    nodes, tris = read_mesh(sys.argv[2])

    if len(poly) < 3:
        print("  polygon has fewer than 3 vertices - nothing to measure")
        return 1
    if not tris:
        print("  no triangles in mesh - nothing to measure")
        return 1

    pa = abs(signed_area(poly))

    # Outline tolerance (same as the vertex tolerance) and shortest
    # edge, for judging how far outside an outside centroid lies.
    pxs = [p[0] for p in poly]
    pys = [p[1] for p in poly]
    vtol = VERTEX_TOL_REL * ((max(pxs) - min(pxs)) ** 2 +
                             (max(pys) - min(pys)) ** 2) ** 0.5
    emin = min(((poly[i][0] - poly[(i + 1) % len(poly)][0]) ** 2 +
                (poly[i][1] - poly[(i + 1) % len(poly)][1]) ** 2) ** 0.5
               for i in range(len(poly)))

    # Bounding boxes of the polygon edges, for the fast path below.
    n = len(poly)
    ebox = []
    for i in range(n):
        (x1, y1), (x2, y2) = poly[i], poly[(i + 1) % n]
        ebox.append((min(x1, x2), max(x1, x2), min(y1, y2), max(y1, y2)))

    drawn = []                    # (a, b, c, overhangs) for the SVG
    ma = 0.0
    covered = 0.0
    outside = 0
    outside_far = []              # (distance outside, centroid)
    npos = nneg = 0
    missing = 0
    for t in tris:
        try:
            a, b, c = (nodes[t[0]], nodes[t[1]], nodes[t[2]])
        except KeyError:
            missing += 1
            continue
        sa = signed_area((a, b, c))
        if sa > 0.0:
            npos += 1
        elif sa < 0.0:
            nneg += 1
        ta = abs(sa)
        ma += ta

        cen = ((a[0]+b[0]+c[0])/3.0, (a[1]+b[1]+c[1])/3.0)
        if not inside(cen, poly):
            # A centroid within VERTEX_TOL_REL of the outline is ON it,
            # not outside: ray casting is ambiguous there.
            d = dist_to_outline(cen, poly)
            if d > vtol:
                outside += 1
                outside_far.append((d, cen, (a, b, c)))

        # Fast path: no polygon edge comes near the triangle, so the
        # triangle is wholly inside or wholly outside the polygon.
        tx0, tx1 = min(a[0], b[0], c[0]), max(a[0], b[0], c[0])
        ty0, ty1 = min(a[1], b[1], c[1]), max(a[1], b[1], c[1])
        near = False
        for (x0, x1, y0, y1) in ebox:
            if x0 <= tx1 and x1 >= tx0 and y0 <= ty1 and y1 >= ty0:
                near = True
                break
        if not near:
            if inside(cen, poly):
                covered += ta
                drawn.append((a, b, c, False))
            else:
                drawn.append((a, b, c, True))
            continue

        ccw = (a, b, c) if sa > 0.0 else (a, c, b)
        part = clip_to_triangle(poly, ccw)
        covered += part
        drawn.append((a, b, c, ta - part > 1.0e-9 * ta))

    overhang = ma - covered
    uncovered = pa - covered
    mismatch = overhang + uncovered
    ratio = ma / pa

    # Vertex recovery.
    xs = [p[0] for p in poly]
    ys = [p[1] for p in poly]
    diag = ((max(xs)-min(xs))**2 + (max(ys)-min(ys))**2) ** 0.5
    tol2 = (VERTEX_TOL_REL * diag) ** 2
    nodelist = list(nodes.values())
    orient = 1.0 if signed_area(poly) > 0.0 else -1.0
    nrec = 0
    lost = []
    for k, (px, py) in enumerate(poly):
        dmin2 = min((qx-px)**2 + (qy-py)**2 for (qx, qy) in nodelist)
        if dmin2 <= tol2:
            nrec += 1
            continue
        # Classify the corner. For a CCW polygon a negative turn is
        # reflex: the tip of a notch. A missing reflex vertex is
        # expected to show up as OVERHANG (the notch tip is bridged),
        # a missing convex one as UNCOVERED (the tooth tip is cut).
        (ax, ay), (bx, by) = poly[k - 1], poly[(k + 1) % n]
        turn = orient * ((px-ax)*(by-py) - (py-ay)*(bx-px))
        kind = 'reflex' if turn < 0.0 else 'convex'
        lost.append((k + 1, px, py, kind, dmin2 ** 0.5))

    ninv = min(npos, nneg)
    pct = lambda v: 100.0 * v / pa

    print(f"  polygon_area      {pa:14.6f}")
    print(f"  mesh_area         {ma:14.6f}")
    print(f"  nodes             {len(nodes):14d}")
    print(f"  elements          {len(tris):14d}")
    print(f"  net_area_pct      {pct(ma - pa):+14.4f}")
    print(f"  overhang_pct      {pct(overhang):14.4f}")
    print(f"  uncovered_pct     {pct(uncovered):14.4f}")
    print(f"  mismatch_pct      {pct(mismatch):14.4f}")
    print(f"  vertices_found    {nrec:8d} / {len(poly)}")
    for (k, px, py, kind, d) in lost:
        print(f"  vertex_missing    {k:6d}  x {px:10.5f}  y {py:10.5f}"
              f"  {kind:6s}  nearest node {d:.3e}")
    print(f"  centroids_outside {outside:14d}")
    # How far outside, relative to the polygon's shortest edge: tells
    # a genuine clip miss (comparable to an element size) from a
    # near-boundary rounding case (tiny).
    # Each one's three nodes, and whether each sits on the outline:
    # all three on the outline = a triangle spanning the notch between
    # boundary nodes, which only the polygon triangulation or a later
    # edge flip (recon) can produce; a node off the outline = the
    # triangle was moved there by smoothing after the clip.
    for d, (cx, cy), verts in sorted(outside_far, reverse=True)[:5]:
        print(f"  centroid_outside  x {cx:10.5f}  y {cy:10.5f}"
              f"  dist {d:.3e}  dist/min_edge {d / emin:.3e}")
        for (vx, vy) in verts:
            dv = dist_to_outline((vx, vy), poly)
            if dv <= vtol:
                where = "on outline"
            elif inside((vx, vy), poly):
                where = f"inside, {dv:.3e} from outline"
            else:
                where = f"OUTSIDE, {dv:.3e} from outline"
            print(f"    node            x {vx:10.5f}  y {vy:10.5f}  {where}")
    print(f"  inverted          {ninv:14d}")
    if missing:
        print(f"  bad_connectivity  {missing:14d}")

    hard_ok = True
    if outside > 0:
        print(f"  -> HARD: {outside} element centroids outside the "
              "polygon : clip did not run or was incomplete")
        hard_ok = False
    if ninv > 0:
        print(f"  -> HARD: {ninv} inverted elements")
        hard_ok = False
    if missing > 0:
        print(f"  -> HARD: {missing} elements reference missing nodes")
        hard_ok = False

    soft_ok = pct(mismatch) <= MISMATCH_TOL_PCT
    print("  RECOVERY: " + ("recovered" if soft_ok else "degraded"))
    print("  HARD: " + ("pass" if hard_ok else "FAIL"))
    if len(sys.argv) == 4:
        write_svg(sys.argv[3], poly, drawn, lost,
                  f"{sys.argv[2]}   mismatch {pct(mismatch):.4f} %   "
                  f"overhang {pct(overhang):.4f} %   "
                  f"uncovered {pct(uncovered):.4f} %   "
                  f"vertices {nrec}/{len(poly)}")
    return 0 if hard_ok else 1


if __name__ == '__main__':
    sys.exit(main())
