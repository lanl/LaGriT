#!/usr/bin/env python3
"""check_connect_2d.py WORK LOGS

test_connect_2d checker. For every deck: LaGriT finished with no ERROR,
no point left unconnected, the expected number of triangles, every
triangle with positive area in its plane, each edge in one or two
triangles. The offset decks (cd02-cd06) must give exactly the triangles
of cd01_origin, vertex for vertex.

For the decks that read a point file (all but cd07), with the points'
full-precision coordinates from that file (the AVS dump keeps only 13
digits) and exact rational arithmetic:
  Delaunay   no interior edge has the opposite vertex strictly inside
             the circle of the triangle on the other side
  hull gap   (area of the convex hull - area of the mesh) / hull area;
             at most 1e-6 (connect leaves out only corners flatter than
             1e-6 of their base: round-off in points meant to be on a
             line)
cd10_jitter also: no angle below 40 degrees (no slivers on the sides).

Writes LOGS/summary.html and LOGS/table.txt; exit 0 if every check
passes. Python 3, standard library.
"""
import html
import math
import os
import sys
from fractions import Fraction

POINTS = ["cd01_origin", "cd02_utm", "cd03_negative", "cd04_y_only",
          "cd05_yz_plane", "cd06_xz_plane"]
# nt: triangles expected (None: not fixed); pts: the deck reads NAME.pts
# 783 = 2n - 2 - h for n = 400 points with h = 15 on the convex hull
DECKS = {n: {"nv": 400, "nt": 783, "ref": "cd01_origin", "pts": True}
         for n in POINTS}
DECKS["cd01_origin"]["ref"] = None
DECKS["cd07_square1000"] = {"nv": 4, "nt": 2, "ref": None, "pts": False}
DECKS["cd08_graded"] = {"nv": 400, "nt": None, "ref": None, "pts": True}
DECKS["cd09_stretched"] = {"nv": 961, "nt": 1800, "ref": None, "pts": True}
DECKS["cd10_jitter"] = {"nv": 961, "nt": 1800, "ref": None, "pts": True,
                        "minangle": 40.0}
DECKS["cd11_rings"] = {"nv": 571, "nt": 1036, "ref": None, "pts": True}
DECKS["cd12_old_order"] = {"nv": 400, "nt": None, "ref": "cd08_graded",
                           "pts": True}
HULL_GAP_MAX = 1e-6


def read_pts(path):
    """the full-precision coordinates of a point file"""
    with open(path) as f:
        n = int(f.readline().split()[0])
        return [tuple(float(v) for v in f.readline().split()[1:4])
                for _ in range(n)]


def exact2(P):
    """exact 2D coordinates: drop the coordinate that is constant"""
    span = [max(p[k] for p in P) - min(p[k] for p in P) for k in range(3)]
    keep = [k for k in range(3) if k != span.index(min(span))]
    return [(Fraction(p[keep[0]]), Fraction(p[keep[1]])) for p in P]


def orient(a, b, c):
    return (b[0] - a[0]) * (c[1] - a[1]) - (b[1] - a[1]) * (c[0] - a[0])


def incircle(a, b, c, d):
    """> 0 when d is inside the circle through a, b, c (any order)"""
    m = []
    for p in (a, b, c):
        x, y = p[0] - d[0], p[1] - d[1]
        m.append((x, y, x * x + y * y))
    det = (m[0][0] * (m[1][1] * m[2][2] - m[1][2] * m[2][1])
           - m[0][1] * (m[1][0] * m[2][2] - m[1][2] * m[2][0])
           + m[0][2] * (m[1][0] * m[2][1] - m[1][1] * m[2][0]))
    o = orient(a, b, c)
    return det if o > 0 else -det


def hull_area(Q):
    """area of the convex hull (monotone chain, exact)"""
    S = sorted(set(Q))
    if len(S) < 3:
        return Fraction(0)
    lo, hi = [], []
    for p in S:
        while len(lo) >= 2 and orient(lo[-2], lo[-1], p) <= 0:
            lo.pop()
        lo.append(p)
    for p in reversed(S):
        while len(hi) >= 2 and orient(hi[-2], hi[-1], p) <= 0:
            hi.pop()
        hi.append(p)
    H = lo[:-1] + hi[:-1]
    return abs(sum(H[i][0] * H[(i + 1) % len(H)][1] -
                   H[(i + 1) % len(H)][0] * H[i][1]
                   for i in range(len(H)))) / 2


def delaunay_and_hull(Q, T):
    """(non-Delaunay interior edges, hull gap, smallest angle in degrees)"""
    E = {}
    for t, (a, b, c) in enumerate(T):
        for u, v, w in ((a, b, c), (b, c, a), (c, a, b)):
            E.setdefault(frozenset((u, v)), []).append((t, w))
    bad = 0
    for L in E.values():
        if len(L) != 2:
            continue
        (t, _), (s, z) = L
        a, b, c = T[t]
        if incircle(Q[a], Q[b], Q[c], Q[z]) > 0:
            bad += 1
    area = sum(abs(orient(Q[a], Q[b], Q[c])) for a, b, c in T) / 2
    ha = hull_area(Q)
    gap = float((ha - area) / ha) if ha else 0.0
    amin = 180.0
    for a, b, c in T:
        for p, q, r in ((a, b, c), (b, c, a), (c, a, b)):
            ux, uy = float(Q[q][0] - Q[p][0]), float(Q[q][1] - Q[p][1])
            vx, vy = float(Q[r][0] - Q[p][0]), float(Q[r][1] - Q[p][1])
            cr = abs(ux * vy - uy * vx)
            dt = ux * vx + uy * vy
            amin = min(amin, math.degrees(math.atan2(cr, dt)))
    return bad, gap, amin


def read_avs(path):
    """vertices [(x, y, z)], triangles [(i, j, k)] (0-based)"""
    with open(path) as f:
        h = f.readline().split()
        nv, nc = int(h[0]), int(h[1])
        X = []
        for _ in range(nv):
            w = f.readline().split()
            X.append(tuple(float(v) for v in w[1:4]))
        T = []
        for _ in range(nc):
            w = f.readline().split()
            T.append(tuple(int(v) - 1 for v in w[3:6]))
    return X, T


def plane_coords(X):
    """the two coordinates that vary, relative to the first vertex"""
    span = [max(p[k] for p in X) - min(p[k] for p in X) for k in range(3)]
    keep = sorted(range(3), key=lambda k: -span[k])[:2]
    keep.sort()
    o = X[0]
    return [(p[keep[0]] - o[keep[0]], p[keep[1]] - o[keep[1]]) for p in X]


def check_mesh(X, T):
    """(all areas > 0 with one orientation, edges well shared, used vertices)"""
    P = plane_coords(X)
    signs = set()
    for a, b, c in T:
        ar = ((P[b][0] - P[a][0]) * (P[c][1] - P[a][1]) -
              (P[b][1] - P[a][1]) * (P[c][0] - P[a][0]))
        signs.add(ar > 0 if ar != 0 else None)
    areas_ok = None not in signs and len(signs) == 1
    E = {}
    for a, b, c in T:
        for u, v in ((a, b), (b, c), (c, a)):
            E[frozenset((u, v))] = E.get(frozenset((u, v)), 0) + 1
    edges_ok = all(n in (1, 2) for n in E.values())
    used = len(set(v for t in T for v in t))
    return areas_ok, edges_ok, used


# What each deck tests, shown under a "What this tests" button on the
# summary page.
ABOUT = {
    "cd01_origin":
        "400 random points in a 100 x 80 rectangle with a corner at the "
        "origin: the reference mesh the offset decks (cd02-cd06) must "
        "match triangle for triangle. The complete Delaunay triangulation "
        "of these points has 783 triangles (2n - 2 - h, with h = 15 points "
        "on the convex hull). LaGriT V3.3.4 made 778: it lost 5 thin "
        "triangles along the hull with the enclosing triangle.",
    "cd02_utm":
        "The same points moved to x = 400,000, y = 3,900,000 (UTM-sized "
        "coordinates). LaGriT V3.3.4 placed a vertex of its enclosing "
        "triangle at 1.01 times the centre coordinate, 4,000 m from a "
        "100 m box, so the triangle did not contain the points and "
        "connect made no triangles. A correct result is the cd01 mesh: "
        "the offset must change nothing.",
    "cd03_negative":
        "The same points moved to x = -650,000, y = -4,200,000: the "
        "enclosing-triangle fix must work for negative coordinates too.",
    "cd04_y_only":
        "The same points moved in y only (y = 3,900,000; x still starts "
        "at 0). LaGriT V3.3.4 got this one right, because the misplaced "
        "vertex depends on the x centre: a control case.",
    "cd05_yz_plane":
        "The same points in the plane x = 0 (moved to y = 200,000, "
        "z = 300,000). connect takes points in any plane; this tests the "
        "enclosing triangle's branch for a plane parallel to x = 0, and "
        "the exact tests on the y and z coordinates.",
    "cd06_xz_plane":
        "As cd05, in the plane y = 0 (moved to x = 200,000, z = 300,000).",
    "cd07_square1000":
        "The four corners of a unit square at x = 1000: the case "
        "test/level01/connect_errors was written to record as failing. "
        "Two triangles; the four corners are cocircular, so either "
        "diagonal is correct.",
    "cd08_graded":
        "400 points crowded toward the left and bottom sides ((u^3, v^3) "
        "x 100), so triangle sizes span many orders of magnitude. LaGriT "
        "V3.3.4 left 42 boundary edges off the convex hull (thin hull "
        "triangles lost with the enclosing triangle). Many points lie "
        "within about 1e-6 of a straight side; connect deliberately does "
        "not fill corners flatter than 1e-6, which leaves one long, very "
        "thin sliver along the left side (area far below 1e-6 of the "
        "hull).",
    "cd09_stretched":
        "A 31 x 31 grid 1000 long and 1 high. LaGriT V3.3.4 compared "
        "every in-circle test with a tolerance scaled by the longest side "
        "of the box squared (1,000,000), so at the short spacing every "
        "test looked degenerate and 37 points were left out. Exact tests "
        "connect all 961 points. Drawn with x and y scaled separately.",
    "cd10_jitter":
        "A 31 x 31 grid of the unit square with every coordinate moved by "
        "up to 1e-9. On the exact grid both diagonals of each square are "
        "Delaunay; after the jitter the four corners are no longer "
        "cocircular (r^2 - d^2 between 1e-13 and 2e-10), so exactly one "
        "diagonal is correct. LaGriT V3.3.4's tolerance (6.5e-9) and its "
        "recon/0 (1e-6 of the longest edge) could not see the "
        "difference, so the diagonal was decided by insertion order: 416 "
        "of 900 squares wrong (purple). The exact test gets every one "
        "right; the green squares are those 416. The jitter along the "
        "sides is round-off, so no slivers are added there (smallest "
        "angle 45 degrees).",
    "cd11_rings":
        "A centre point and 10 rings of equally spaced points: many sets "
        "of four or more cocircular points (exact ties). Any choice among "
        "tied triangulations is Delaunay; the test is that the mesh is "
        "valid and has no edge strictly non-Delaunay. LaGriT V3.3.4 left "
        "one edge just non-Delaunay (in-circle determinant 5e-21, a "
        "near-tie its tolerances could not see).",
    "cd12_old_order":
        "The points of cd08 again, with connect / old_order: the points "
        "are inserted in the order of LaGriT V3.3.4 (a fixed prime "
        "stride through the list) instead of the default BRIO order "
        "(random rounds, each sorted along a Morton curve). With exact "
        "tests the order may change only how exact ties are split, and "
        "these points have none, so the mesh must be exactly the cd08 "
        "mesh. It also checks that old_order is accepted and does not "
        "disturb the other connect options.",
}


# ---------------------------------------------------------------- pictures
#
# One SVG per deck (LOGS/<deck>.svg), shown on the summary page:
#   grey edges       the triangles connect made
#   red under it     the convex hull of the points: any red that shows is
#                    part of the hull the mesh does not cover; a boundary
#                    edge of the mesh that is not on the hull is drawn
#                    thick red, so thin gaps are visible
#   purple edge      an edge that is not Delaunay (exact test)
#   orange outline   the triangle with the smallest angle
#   green fill       (only with REF_LAGRIT) a triangle the reference
#                    LaGriT did not make, outlined thick when it is on
#                    the boundary; the reference mesh is drawn beside it
# The stretched grid (cd09) is drawn with x and y scaled separately.

PANEL = 380          # panel width in pixels


def float_plane(Q):
    """exact plane coordinates -> floats relative to the smallest corner
    (keeps the digits of UTM-sized coordinates)"""
    u0 = min(q[0] for q in Q)
    v0 = min(q[1] for q in Q)
    return [(float(q[0] - u0), float(q[1] - v0)) for q in Q]


def hull_float(P):
    S = sorted(set(P))
    if len(S) < 3:
        return S

    def cr(o, a, b):
        return (a[0] - o[0]) * (b[1] - o[1]) - (a[1] - o[1]) * (b[0] - o[0])
    lo, hi = [], []
    for p in S:
        while len(lo) >= 2 and cr(lo[-2], lo[-1], p) <= 0:
            lo.pop()
        lo.append(p)
    for p in reversed(S):
        while len(hi) >= 2 and cr(hi[-2], hi[-1], p) <= 0:
            hi.pop()
        hi.append(p)
    return lo[:-1] + hi[:-1]


def bad_edges_and_worst(Q, T):
    """the non-Delaunay edges (vertex pairs) and the index of the
    triangle with the smallest angle"""
    E = {}
    for t, (a, b, c) in enumerate(T):
        for u, v, w in ((a, b, c), (b, c, a), (c, a, b)):
            E.setdefault(frozenset((u, v)), []).append((t, w))
    bad = []
    for e, L in E.items():
        if len(L) == 2:
            (t, _), (s2, z) = L
            a, b, c = T[t]
            if incircle(Q[a], Q[b], Q[c], Q[z]) > 0:
                bad.append(tuple(e))
    worst, amin = -1, 181.0
    for t, (a, b, c) in enumerate(T):
        for p, q, r in ((a, b, c), (b, c, a), (c, a, b)):
            ux, uy = float(Q[q][0] - Q[p][0]), float(Q[q][1] - Q[p][1])
            vx, vy = float(Q[r][0] - Q[p][0]), float(Q[r][1] - Q[p][1])
            ang = math.degrees(math.atan2(abs(ux * vy - uy * vx),
                                          ux * vx + uy * vy))
            if ang < amin:
                worst, amin = t, ang
    return bad, worst, amin


def panel_scale(P):
    """(u0, v0, v1, sx, sy, height, note) for drawing P in a panel"""
    us = [p[0] for p in P]
    vs = [p[1] for p in P]
    u0, v0, u1, v1 = min(us), min(vs), max(us), max(vs)
    du, dv = (u1 - u0) or 1.0, (v1 - v0) or 1.0
    side = PANEL - 28
    # keep the aspect unless it is extreme (the stretched grid)
    if max(du / dv, dv / du) > 8:
        return u0, v0, v1, side / du, side / dv, PANEL, \
            " (x and y scaled separately)"
    s = side / max(du, dv)
    return u0, v0, v1, s, s, int(dv * s) + 28, ""


def boundary_edges(T):
    E = {}
    for a, b, c in T:
        for u, v in ((a, b), (b, c), (c, a)):
            k = frozenset((u, v))
            E[k] = E.get(k, 0) + 1
    return [tuple(k) for k, n in E.items() if n == 1]


def off_hull(Q, edges):
    """the boundary edges not on the convex hull (exact): some point is
    strictly on the far side of the edge's line. Returns (real, flat):
    flat ones have every point beyond them within 1e-6 of the size of
    the point set (round-off in points meant to be on a line, which
    connect leaves as it is)"""
    S = sorted(set(Q))
    lo, hi = [], []
    for p in S:
        while len(lo) >= 2 and orient(lo[-2], lo[-1], p) <= 0:
            lo.pop()
        lo.append(p)
    for p in reversed(S):
        while len(hi) >= 2 and orient(hi[-2], hi[-1], p) <= 0:
            hi.pop()
        hi.append(p)
    H = lo[:-1] + hi[:-1]
    us = [float(q[0]) for q in Q]
    vs = [float(q[1]) for q in Q]
    diam = math.hypot(max(us) - min(us), max(vs) - min(vs)) or 1.0
    real, flat = [], []
    for a, b in edges:
        o = [orient(Q[a], Q[b], h) for h in H]
        if min(o) < 0 and max(o) > 0:
            # the side the mesh is on is the side of most hull points
            far = min(o) if sum(1 for x in o if x > 0) >= \
                sum(1 for x in o if x < 0) else -max(o)
            ln = math.hypot(float(Q[b][0] - Q[a][0]),
                            float(Q[b][1] - Q[a][1])) or 1.0
            depth = abs(float(far)) / ln
            (flat if depth <= 1e-6 * diam else real).append((a, b))
    return real, flat


def panel_svg(P, T, title, scale, new=None, ox=0, oy=0):
    """SVG elements for one panel at (ox, oy); new = set of frozenset
    triangles to fill green"""
    u0, v0, v1, sx, sy, ph, note = scale
    m = 14

    def X(p):
        return ox + m + (p[0] - u0) * sx

    def Y(p):
        return oy + 30 + m + (v1 - p[1]) * sy
    o = ['<rect x="%d" y="%d" width="%d" height="%d" fill="#fff" '
         'stroke="#d0d7de"/>' % (ox, oy + 30, PANEL, ph)]
    o.append('<text x="%d" y="%d" font-size="12" fill="#1f2328">%s</text>'
             % (ox, oy + 18, html.escape(title + note)))
    o.append('<clipPath id="c%d_%d"><rect x="%d" y="%d" width="%d" '
             'height="%d"/></clipPath><g clip-path="url(#c%d_%d)">'
             % (ox, oy, ox, oy + 30, PANEL, ph, ox, oy))
    H = hull_float(P)
    if len(H) >= 3:
        o.append('<polygon points="%s" fill="#ff8182" stroke="none"/>' %
                 " ".join("%.2f,%.2f" % (X(p), Y(p)) for p in H))
    stroke = 0.5 if len(T) > 1500 else 0.7
    for t in T:
        fill = "#aceebb" if new is not None and frozenset(t) in new \
            else "#f6f8fa"
        o.append('<polygon points="%s" fill="%s" stroke="#57606a" '
                 'stroke-width="%.1f"/>' % (" ".join(
                     "%.2f,%.2f" % (X(P[i]), Y(P[i])) for i in t),
                     fill, stroke))
    return o, X, Y


def write_pictures(logs, deck, P, T, Q, ref):
    """LOGS/deck.svg; returns (file name, caption)"""
    scale = panel_scale(P)
    meshes = [("this LaGriT", T)]
    if ref is not None:
        meshes.append(("reference LaGriT", ref))
    newset = None
    if ref is not None:
        newset = set(frozenset(t) for t in T) - set(frozenset(t) for t in ref)
    W = len(meshes) * (PANEL + 20)
    o = ['<svg xmlns="http://www.w3.org/2000/svg" width="%d" height="%d" '
         'font-family="Helvetica,Arial,sans-serif">' % (W, scale[5] + 34)]
    caps = []
    for k, (who, mesh) in enumerate(meshes):
        ox = k * (PANEL + 20)
        if mesh:
            bad, worst, amin = bad_edges_and_worst(Q, mesh)
            gaps, flat = off_hull(Q, boundary_edges(mesh))
        else:
            bad, worst, amin, gaps, flat = [], -1, float("nan"), [], []
        caps.append("%s: %d triangles, %d not Delaunay, %d boundary edges "
                    "off the hull%s, smallest angle %.2f deg" %
                    (who, len(mesh), len(bad), len(gaps),
                     " (and %d off only by round-off, not drawn)" % len(flat)
                     if flat else "", amin))
        new = newset if k == 0 else None
        el, X, Y = panel_svg(P, mesh, "%s: %d triangles" % (who, len(mesh)),
                             scale, new, ox, 0)
        o += el
        bnd = set(frozenset(e) for e in boundary_edges(mesh)) if new else ()
        if new:
            for t in mesh:
                if frozenset(t) in new and any(
                        frozenset(e) in bnd for e in
                        ((t[0], t[1]), (t[1], t[2]), (t[2], t[0]))):
                    o.append('<polygon points="%s" fill="#aceebb" '
                             'stroke="#1a7f37" stroke-width="2"/>' % " ".join(
                                 "%.2f,%.2f" % (X(P[i]), Y(P[i])) for i in t))
        for a, b in gaps:
            o.append('<line x1="%.2f" y1="%.2f" x2="%.2f" y2="%.2f" '
                     'stroke="#cf222e" stroke-width="2.5"/>' %
                     (X(P[a]), Y(P[a]), X(P[b]), Y(P[b])))
        for a, b in bad:
            o.append('<line x1="%.2f" y1="%.2f" x2="%.2f" y2="%.2f" '
                     'stroke="#8250df" stroke-width="2.5"/>' %
                     (X(P[a]), Y(P[a]), X(P[b]), Y(P[b])))
        if worst >= 0:
            o.append('<polygon points="%s" fill="none" stroke="#bc4c00" '
                     'stroke-width="2"/>' % " ".join(
                         "%.2f,%.2f" % (X(P[i]), Y(P[i])) for i in mesh[worst]))
        o.append('</g>')
    o.append('</svg>')
    fn = deck + ".svg"
    open(os.path.join(logs, fn), "w").write("\n".join(o) + "\n")
    return fn, "<br>".join(html.escape(c) for c in caps)


def main():
    work, logs = sys.argv[1], sys.argv[2]
    rows, ok_all = [], True
    draw = {}

    def row(deck, what, good, size, detail):
        nonlocal ok_all
        rows.append((deck, what, bool(good), size, detail))
        ok_all &= bool(good)

    meshes = {}
    for deck, D in DECKS.items():
        out = open(os.path.join(work, deck + ".out"), errors="replace").read()
        fin = "LaGriT successfully completed" in out
        nerr = out.count("ERROR")
        unconn = "could not be connected" in out or "Dudding" in out
        row(deck, "LaGriT finished, no ERROR", fin and nerr == 0,
            "finished" if fin else "did not finish", "%d ERROR lines" % nerr)
        row(deck, "every point connected", not unconn,
            "", "connect reported unconnected points" if unconn else "ok")
        path = os.path.join(work, deck + ".inp")
        if not os.path.isfile(path):
            row(deck, "mesh written", False, "no %s.inp" % deck, "")
            continue
        X, T = read_avs(path)
        meshes[deck] = T
        Qd = exact2(read_pts(os.path.join(work, deck + ".pts"))) \
            if D["pts"] else exact2(X)
        refp = os.path.join(work, "ref", deck + ".inp")
        draw[deck] = (float_plane(Qd), T, Qd,
                      read_avs(refp)[1] if os.path.isfile(refp) else None)
        if D["nt"] is not None:
            row(deck, "triangle count", len(T) == D["nt"],
                "%d (expect %d)" % (len(T), D["nt"]), "")
        if not T:
            continue
        areas_ok, edges_ok, used = check_mesh(X, T)
        row(deck, "valid triangulation", areas_ok and edges_ok and
            used == D["nv"], "%d of %d vertices used" % (used, D["nv"]),
            "areas %s; edges %s" % ("ok" if areas_ok else "not all positive",
                                   "ok" if edges_ok else "shared by 3 or more"))
        if D["pts"]:
            Q = exact2(read_pts(os.path.join(work, deck + ".pts")))
            bad, gap, amin = delaunay_and_hull(Q, T)
            row(deck, "Delaunay (exact)", bad == 0,
                "%d non-Delaunay edges" % bad, "full-precision coordinates")
            row(deck, "covers the convex hull", 0.0 <= gap <= HULL_GAP_MAX,
                "hull gap %.3g" % gap, "at most %g" % HULL_GAP_MAX)
            if "minangle" in D:
                row(deck, "smallest angle", amin >= D["minangle"],
                    "%.2f deg" % amin, "at least %g" % D["minangle"])
    for deck, D in DECKS.items():
        ref = D["ref"]
        if ref is None:
            continue
        a = set(frozenset(t) for t in meshes.get(deck, []))
        b = set(frozenset(t) for t in meshes.get(ref, []))
        row(deck, "same triangles as " + ref, a == b and len(a) > 0,
            "%d differ" % (len(a ^ b) // 2), "")

    os.makedirs(logs, exist_ok=True)
    with open(os.path.join(logs, "table.txt"), "w") as t:
        for r in rows:
            t.write("%-16s %-32s %-5s %-24s %s\n" % (
                r[0], r[1], "PASS" if r[2] else "FAIL", r[3], r[4]))
    E = html.escape
    page = ["<!doctype html><html><head><meta charset='utf-8'><title>test_connect_2d</title><style>"
            "body{font-family:-apple-system,Helvetica,Arial,sans-serif;margin:16px;color:#1f2328;background:#fff}"
            "table{border-collapse:collapse;font-size:13px}td,th{border-bottom:1px solid #d0d7de;padding:3px 8px;"
            "text-align:left}.p{color:#1a7f37;font-weight:600}.f{color:#cf222e;font-weight:600}</style></head><body>",
            "<h1 style='font-size:20px'>test_connect_2d: 2D connect</h1>",
            "<p>The same 400 points at the origin and at large offsets, in the xy, yz and xz planes, "
            "the unit square at x = 1000, and graded, stretched, round-off and cocircular point sets. "
            "Every offset mesh must equal the mesh at the origin; every mesh must be exactly Delaunay "
            "and cover the convex hull (checked with exact rational arithmetic).</p><table>"
            "<tr><th>deck</th><th>check</th><th>result</th><th>size</th><th>detail</th></tr>"]
    for r in rows:
        page.append("<tr><td><a href='#%s'>%s</a></td><td>%s</td><td class='%s'>%s</td><td>%s</td><td>%s</td></tr>" % (
            E(r[0]), E(r[0]), E(r[1]), "p" if r[2] else "f", "PASS" if r[2] else "FAIL", E(r[3]), E(r[4])))
    page.append("</table>")
    page.append("<h2 style='font-size:16px;margin-top:24px'>The meshes</h2>"
                "<p style='font-size:13px'>Grey: the triangles connect made. "
                "<span style='background:#ff8182'>&nbsp;Red&nbsp;</span> underneath: the convex hull of the "
                "points; any red that shows is hull the mesh does not cover, and a thick red line is a "
                "boundary edge that is not on the hull (so thin gaps show). "
                "<span style='color:#8250df;font-weight:600'>Purple</span> edge: not Delaunay. "
                "<span style='color:#bc4c00;font-weight:600'>Orange</span> outline: the triangle with the "
                "smallest angle. With a reference LaGriT "
                "(<code>REF_LAGRIT=/path/to/lagrit sh run_connect_2d_tests.sh</code>): its mesh beside "
                "this one, and <span style='background:#aceebb'>&nbsp;green&nbsp;</span> triangles it did "
                "not make, outlined in dark green on the boundary. On a regular grid each square can be "
                "split either way (both are Delaunay), so green there only means the other diagonal.</p>")
    for deck in DECKS:
        if deck not in draw:
            continue
        P, T, Qd, ref = draw[deck]
        fn, cap = write_pictures(logs, deck, P, T, Qd, ref)
        about = ABOUT.get(deck, "")
        page.append("<h3 id='%s' style='font-size:14px;margin:22px 0 4px'>%s</h3>"
                    "%s<p style='font-size:12px;margin:4px 0 6px'>%s</p><img src='%s'>" %
                    (E(deck), E(deck),
                     "<details style='font-size:13px;max-width:780px'><summary "
                     "style='cursor:pointer;color:#0969da'>What this tests</summary>"
                     "<p style='margin:6px 0 0 14px'>%s</p></details>" % E(about)
                     if about else "", cap, E(fn)))
    page.append("</body></html>")
    open(os.path.join(logs, "summary.html"), "w").write("\n".join(page) + "\n")
    npass = sum(r[2] for r in rows)
    print("test_connect_2d: %d of %d checks pass" % (npass, len(rows)))
    for r in rows:
        if not r[2]:
            print("  FAIL %s %s: %s; %s" % (r[0], r[1], r[3], r[4]))
    return 0 if ok_all else 1


if __name__ == "__main__":
    sys.exit(main())
