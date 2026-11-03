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


def main():
    work, logs = sys.argv[1], sys.argv[2]
    rows, ok_all = [], True

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
        page.append("<tr><td>%s</td><td>%s</td><td class='%s'>%s</td><td>%s</td><td>%s</td></tr>" % (
            E(r[0]), E(r[1]), "p" if r[2] else "f", "PASS" if r[2] else "FAIL", E(r[3]), E(r[4])))
    page.append("</table></body></html>")
    open(os.path.join(logs, "summary.html"), "w").write("\n".join(page) + "\n")
    npass = sum(r[2] for r in rows)
    print("test_connect_2d: %d of %d checks pass" % (npass, len(rows)))
    for r in rows:
        if not r[2]:
            print("  FAIL %s %s: %s; %s" % (r[0], r[1], r[3], r[4]))
    return 0 if ok_all else 1


if __name__ == "__main__":
    sys.exit(main())
