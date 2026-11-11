#!/usr/bin/env python3
"""check_stor_utm.py WORK LOGDIR

Checks test_stor_utm: every deck writes the same mesh twice, at the
origin (file *_o) and moved by SHIFT = (7e5, 3.2e6, 1.5e3), a UTM-sized
offset (file *_u). The deck moves the mesh out and back first, so both
copies have the same geometry to the last bit (x - 7e5 is exact
there). For every pair of files:

  same    the two files hold the same matrix: the same rows and
          columns, and volumes and coefficients equal to TOL_SAME of
          their largest value (a writer that computes with absolute
          coordinates loses digits at the moved copy)
  sum     the Voronoi volumes add up to the volume (area in 2D) of the
          cells, computed here from the mesh file
  indep   (not for hybrid) the volumes and coefficients of the origin
          copy agree, to TOL_INDEP of their largest value, with an
          independent computation in plain Python: for every edge of
          every cell, the piece of its Voronoi face (3D: the polygon
          edge midpoint, face circumcenter, cell circumcenter, other
          face circumcenter; 2D: the segment edge midpoint to
          circumcenter), signed, gives the coefficient area / length
          and, as a pyramid over it, the volume of each end vertex

st07 (dump / pflotran, .uge) checks the volumes and connection areas
the same way, and the centers: u - SHIFT = o to 1e-6 (the .uge writes
13 digits: at 3.2e6 that is 1e-6).

Writes LOGDIR/<deck>.check, LOGDIR/table.txt, LOGDIR/summary.html and
the pictures LOGDIR/<deck>.svg (the mesh, its vertices colored by
Voronoi volume; LaGriT's volume at UTM against the independent value).
With REF_STOR=DIR (the WORK folder of a run with another lagrit, e.g.
master), the pictures also show that run's volumes at UTM.
Exit 0 if every deck passes.
"""
import html
import math
import os
import re
import sys

SHIFT = (700000.0, 3200000.0, 1500.0)
TOL_SAME = 1e-12     # origin vs UTM, of the largest value
TOL_INDEP = 1e-7     # vs the Python computation ("all" merges 1e-8)
TOL_SUM = 1e-9       # sum of Voronoi volumes vs sum of cell volumes,
                     # of the sum of |Voronoi volume| (thin hull tets
                     # have far centers: large pieces that cancel)

# Known problems, not about UTM coordinates (LAGRIT_PARKING_LOT item
# 80): on st04's mesh (near-flat tets on the hull, circumcenters far
# away) the 3D default writer's volumes add up to 0.5000006, not 0.5
# (matbld3d_stor: 0.500000000005), at the origin as well. Reported as
# KNOWN (not a failure); FIXED when it passes: then remove it here.
KNOWN = {("st04h", "sum"), ("st04v", "sum")}
TOL_KNOWN = 1e-6     # a KNOWN check still fails above this

# deck, title, pairs [(root, kind)], what it does
#   kind: "stor" (Voronoi), "hybrid" (stor, no independent check), "uge"
CASES = [
    ("st01_brick_default", "brick lattice, dump / stor (3D default)",
     [("st01", "stor")],
     "A 9 x 7 x 6 lattice in a 1 x 0.75 x 0.5 box; the interior vertices "
     "are moved at random by up to 0.03 and the mesh is connected again. "
     "dump / stor with no options: the 3D default writer "
     "(anothermatbld3d.c, compression all). Before the fix this file was "
     "wrong at UTM coordinates: the volumes added up to -113.8, with 217 "
     "negative volumes."),
    ("st02_brick_graph", "brick lattice, dump / stor / graph",
     [("st02", "stor")],
     "st01's mesh, dump / stor / ... / ascii / scalar / graph: the same "
     "3D writer with graph compression (no title lines in the file)."),
    ("st03_brick_matbld3d", "brick lattice, matbld3d_stor (none, coefs)",
     [("st03n", "stor"), ("st03c", "stor")],
     "st01's mesh, dump / stor / ... / none and / coefs: the older 3D "
     "writer matbld3d_stor.f. It lost about 1e-9 at UTM coordinates."),
    ("st04_hybrid", "random points, dump / stor / hybrid",
     [("st04h", "hybrid"), ("st04v", "stor")],
     "8 corners, 25 random points on each face and 150 inside a 1 x 1 x "
     "0.5 box (make_stor_inputs.py). Boundary tets whose circumcenters "
     "lie outside the box: dump / stor / hybrid moves those centers to "
     "the boundary (median-Voronoi hybrid). Both files, hybrid and not."),
    ("st05_poisson", "3D Poisson disk, dump / stor",
     [("st05", "stor")],
     "createpts / poisson_disk / 3d_box with h = 0.08 in a 1 x 1 x 0.5 "
     "box (857 vertices), then dump / stor (3D default): a mesh like the "
     "ones made for dfnWorks."),
    ("st06_2d", "2D triangles, dump / stor (matbld2d_stor)",
     [("st06", "stor2d")],
     "A 41 x 31 lattice in a 0.04 x 0.03 rectangle (edges about 1e-3), "
     "interior vertices moved at random, triangulated. dump / stor on a "
     "triangle mesh: matbld2d_stor.f. It lost about 7e-9 at UTM "
     "coordinates."),
    ("st07_pflotran", "brick lattice, dump / pflotran (.uge)",
     [("st07", "uge")],
     "st01's mesh, dump / pflotran: the .uge file (cell volumes and "
     "centers, connection areas and centers) from the same 3D code."),
    ("st08_long_names", "file-name roots of 26 to 32 characters",
     [("st08", "names")],
     "dump / stor, pflotran, fehm and tecplot, 3D and 2D, with file-name "
     "roots of 26 to 32 characters (32 is the longest word LaGriT "
     "reads). Every file must get its whole root and extension. Before, "
     "the name was built in 32 characters: a 30-character root gave "
     "ROOT.s, a 32-character one no extension at all, and the .uge "
     "overwrote the .stor."),
]


# ---------------------------------------------------------------- input
FORTRAN_EXP = re.compile(r"([0-9.])([+-][0-9]{3})$")


def real(w):
    """A Fortran real; with a 3-digit exponent Fortran's E format drops
    the E (-6.9e-310 is written -6.900954117507-310)."""
    try:
        return float(w)
    except ValueError:
        return float(FORTRAN_EXP.sub(r"\1E\2", w))


def read_avs(path):
    """Vertices [(x,y,z)], cells [[0-based ids]] (tet or tri)."""
    with open(path) as f:
        lines = f.read().split("\n")
    nn, nc = (int(w) for w in lines[0].split()[:2])
    xyz = [tuple(float(w) for w in lines[1 + i].split()[1:4])
           for i in range(nn)]
    cells = [[int(v) - 1 for v in lines[1 + nn + i].split()[3:]]
             for i in range(nc)]
    return xyz, cells


def read_stor(path):
    """An FEHM ASCII .stor file: volumes, the matrix rows (CSR, 0-based
    column ids) and one scalar coefficient per entry."""
    with open(path) as f:
        h = f.readline().split()
        if not h[0].isdigit():               # title lines (not graph)
            f.readline()
            h = f.readline().split()
        nuniq, neq, nrc, narea = (int(w) for w in h[:4])
        vals = f.read().split()
    nconn = nrc - neq - 1
    k = 0
    vol = [real(v) for v in vals[k:k + neq]]
    k += neq
    rc = [int(v) for v in vals[k:k + nrc]]
    k += nrc
    ptr = [int(v) for v in vals[k:k + nconn]]
    k += nconn + neq + 1 + neq               # pointers, zeros, diagonal
    uniq = [real(v) for v in vals[k:k + nuniq * narea]]
    return {"neq": neq,
            "rows": [r - neq - 1 for r in rc[:neq + 1]],
            "cols": [c - 1 for c in rc[neq + 1:]],
            "vol": vol,
            "coef": [uniq[(p - 1) * narea] if p > 0 else 0.0
                     for p in ptr]}


def read_uge(path):
    """PFLOTRAN .uge: cells [(xyz, vol)], connections [(i, j, xyz, a)]."""
    with open(path) as f:
        w = f.read().split()
    n = int(w[1])
    cells, k = [], 2
    for _ in range(n):
        cells.append((tuple(float(v) for v in w[k + 1:k + 4]),
                      float(w[k + 4])))
        k += 5
    m = int(w[k + 1])
    k += 2
    conn = []
    for _ in range(m):
        conn.append((int(w[k]), int(w[k + 1]),
                     tuple(float(v) for v in w[k + 2:k + 5]),
                     float(w[k + 5])))
        k += 6
    return cells, conn


# ------------------------------------------------------------- geometry
def sub(a, b):
    return (a[0] - b[0], a[1] - b[1], a[2] - b[2])


def add(a, b):
    return (a[0] + b[0], a[1] + b[1], a[2] + b[2])


def scale(s, a):
    return (s * a[0], s * a[1], s * a[2])


def dot(a, b):
    return a[0] * b[0] + a[1] * b[1] + a[2] * b[2]


def cross(a, b):
    return (a[1] * b[2] - a[2] * b[1], a[2] * b[0] - a[0] * b[2],
            a[0] * b[1] - a[1] * b[0])


def center_tet(p, q, r, s):
    """Circumcenter, from the edge vectors at p."""
    a, b, c = sub(q, p), sub(r, p), sub(s, p)
    x = add(add(scale(dot(a, a), cross(b, c)), scale(dot(b, b),
                                                     cross(c, a))),
            scale(dot(c, c), cross(a, b)))
    return add(p, scale(1.0 / (2.0 * dot(a, cross(b, c))), x))


def center_tri(p, q, r):
    """Circumcenter of a triangle in 3D, from the edge vectors at p."""
    a, b = sub(q, p), sub(r, p)
    n = cross(a, b)
    x = cross(sub(scale(dot(a, a), b), scale(dot(b, b), a)), n)
    return add(p, scale(1.0 / (2.0 * dot(n, n)), x))


def voronoi_3d(xyz, cells):
    """Voronoi volume per vertex, area / length per edge (i < j), and
    the cells' total volume."""
    vol = [0.0] * len(xyz)
    coef, total = {}, 0.0
    for t in cells:
        P = [xyz[i] for i in t]
        total += abs(dot(sub(P[1], P[0]), cross(sub(P[2], P[0]),
                                                sub(P[3], P[0])))) / 6
        cen = center_tet(*P)
        for i, j in ((0, 1), (0, 2), (0, 3), (1, 2), (1, 3), (2, 3)):
            k, m = [n for n in range(4) if n not in (i, j)]
            e = sub(P[j], P[i])
            if dot(e, cross(sub(P[k], P[i]), sub(P[m], P[i]))) < 0:
                k, m = m, k
            mid = scale(0.5, add(P[i], P[j]))
            fk = center_tri(P[i], P[j], P[k])
            fm = center_tri(P[i], P[j], P[m])
            area = scale(0.5, add(cross(sub(fk, mid), sub(cen, mid)),
                                  cross(sub(cen, mid), sub(fm, mid))))
            ln = math.sqrt(dot(e, e))
            a = dot(area, e) / ln            # signed, along the edge
            vol[t[i]] += ln * a / 6          # pyramid, height ln / 2
            vol[t[j]] += ln * a / 6
            key = (min(t[i], t[j]), max(t[i], t[j]))
            coef[key] = coef.get(key, 0.0) + a / ln
    return vol, coef, total


def voronoi_2d(xyz, cells):
    """The same for triangles in a plane z = const."""
    vol = [0.0] * len(xyz)
    coef, total = {}, 0.0
    for t in cells:
        P = [xyz[i] for i in t]
        ax, ay = P[1][0] - P[0][0], P[1][1] - P[0][1]
        bx, by = P[2][0] - P[0][0], P[2][1] - P[0][1]
        d = 2.0 * (ax * by - ay * bx)
        total += abs(d) / 4
        a2, b2 = ax * ax + ay * ay, bx * bx + by * by
        cx = P[0][0] + (by * a2 - ay * b2) / d
        cy = P[0][1] + (ax * b2 - bx * a2) / d
        for i, j, k in ((0, 1, 2), (1, 2, 0), (2, 0, 1)):
            ex, ey = P[j][0] - P[i][0], P[j][1] - P[i][1]
            ln = math.hypot(ex, ey)
            mx, my = (P[i][0] + P[j][0]) / 2, (P[i][1] + P[j][1]) / 2
            nx, ny = -ey / ln, ex / ln       # toward the third vertex
            if nx * (P[k][0] - mx) + ny * (P[k][1] - my) < 0:
                nx, ny = -nx, -ny
            h = nx * (cx - mx) + ny * (cy - my)
            vol[t[i]] += ln * h / 4
            vol[t[j]] += ln * h / 4
            key = (min(t[i], t[j]), max(t[i], t[j]))
            coef[key] = coef.get(key, 0.0) + h / ln
    return vol, coef, total


def maxrel(a, b):
    m = max(abs(v) for v in a) or 1.0
    return max(abs(p - q) for p, q in zip(a, b)) / m


# ------------------------------------------------------------- pictures
# SVG drawn by hand (standard library only), two panels per picture:
#   left:  the mesh (3D: its boundary faces, shaded, in an oblique
#          view; 2D: its triangles), the vertices colored by their
#          Voronoi volume at UTM coordinates
#   right: the error of LaGriT's Voronoi volume at UTM against the
#          independent value (Python, origin copy), per vertex, on a log
#          scale; with REF_STOR, that run's errors at UTM in red
RAMP = [(68, 1, 84), (59, 82, 139), (33, 145, 140), (94, 201, 98),
        (253, 231, 37)]                       # viridis-like
PW, PH = 440, 420
TOP = 46


def color(t):
    t = max(0.0, min(1.0, t)) * (len(RAMP) - 1)
    i = min(int(t), len(RAMP) - 2)
    f = t - i
    return "#%02x%02x%02x" % tuple(
        round(RAMP[i][k] + f * (RAMP[i + 1][k] - RAMP[i][k]))
        for k in range(3))


def heading(ox, title, note):
    return ['<text x="%d" y="18" font-size="13">%s</text>' %
            (ox + 10, html.escape(title)),
            '<text x="%d" y="34" fill="#666">%s</text>' %
            (ox + 10, html.escape(note))]


class View:
    """A fixed oblique view (azimuth AZ, elevation EL, degrees), scaled
    to fit PTS in the plot area of a panel."""

    def __init__(self, pts, az=-55.0, el=28.0):
        self.ca, self.sa = math.cos(math.radians(az)), \
            math.sin(math.radians(az))
        self.ce, self.se = math.cos(math.radians(el)), \
            math.sin(math.radians(el))
        sc = [self.raw(p) for p in pts]
        self.x0, x1 = min(s[0] for s in sc), max(s[0] for s in sc)
        self.y0, y1 = min(s[1] for s in sc), max(s[1] for s in sc)
        self.k = min((PW - 40) / ((x1 - self.x0) or 1.0),
                     (PH - TOP - 50) / ((y1 - self.y0) or 1.0))
        self.yh = y1 - self.y0

    def raw(self, p):
        x1 = p[0] * self.ca - p[1] * self.sa
        y1 = p[0] * self.sa + p[1] * self.ca
        return (x1, p[2] * self.ce + y1 * self.se,
                y1 * self.ce - p[2] * self.se)     # larger = farther

    def __call__(self, p, ox=0):
        x, y, d = self.raw(p)
        return (ox + 20 + (x - self.x0) * self.k,
                TOP + 4 + (self.yh - (y - self.y0)) * self.k, d)


def boundary_faces(cells):
    """Triangles of tets that belong to one tet only."""
    seen = {}
    for t in cells:
        for f in ((t[0], t[1], t[2]), (t[0], t[1], t[3]),
                  (t[0], t[2], t[3]), (t[1], t[2], t[3])):
            key = tuple(sorted(f))
            seen[key] = seen.get(key, 0) + 1
    return [f for f, n in seen.items() if n == 1]


def panel_mesh(xyz, cells, vol, two_d, ox, title, note):
    lo, hi = min(vol), max(vol)
    span = (hi - lo) or 1.0
    o = heading(ox, title, note)
    if two_d:
        view = View(xyz, az=0.0, el=90.0)
        for t in cells:
            pts = [view(xyz[i], ox) for i in t]
            o.append('<polygon points="%s" fill="#eee" stroke="#999" '
                     'stroke-width="0.4"/>' %
                     " ".join("%.1f,%.1f" % (p[0], p[1]) for p in pts))
        for i, p in enumerate(xyz):
            s = view(p, ox)
            o.append('<circle cx="%.1f" cy="%.1f" r="2.0" fill="%s"/>' %
                     (s[0], s[1], color((vol[i] - lo) / span)))
    else:
        view = View(xyz)
        items = []
        for f in boundary_faces(cells):
            A, B, C = (view(xyz[i], ox) for i in f)
            n3 = cross(sub(xyz[f[1]], xyz[f[0]]), sub(xyz[f[2]], xyz[f[0]]))
            nz = abs((B[0] - A[0]) * (C[1] - A[1]) -
                     (B[1] - A[1]) * (C[0] - A[0]))
            g = int(170 + 70 * min(1.0, nz / (math.sqrt(dot(n3, n3)) *
                                              view.k ** 2 + 1e-300)))
            items.append((max(A[2], B[2], C[2]),
                          '<polygon points="%.1f,%.1f %.1f,%.1f %.1f,%.1f" '
                          'fill="rgb(%d,%d,%d)" fill-opacity="0.55" '
                          'stroke="#999" stroke-width="0.4"/>' %
                          (A[0], A[1], B[0], B[1], C[0], C[1], g, g, g)))
        for i, p in enumerate(xyz):
            s = view(p, ox)
            items.append((s[2] - 1e-9,
                          '<circle cx="%.1f" cy="%.1f" r="2.2" fill="%s"/>'
                          % (s[0], s[1], color((vol[i] - lo) / span))))
        items.sort(key=lambda t: -t[0])
        o += [s for _, s in items]
    ly = PH - 12
    for k in range(5):
        o.append('<rect x="%d" y="%d" width="22" height="10" fill="%s"/>'
                 % (ox + 14 + 22 * k, ly - 9, color(k / 4)))
    o.append('<text x="%d" y="%d">Voronoi volume %.3g to %.3g</text>' %
             (ox + 128, ly, lo, hi))
    return o


def panel_error(xv, series, ox, title, note):
    """series: [(name, color, yv)]: for each vertex, |y - x| / max |x|
    on a log scale (1e-17 to 10) against x, the independent value."""
    lo, hi = min(xv), max(xv)
    span = (hi - lo) or 1.0
    big = max(abs(v) for v in xv) or 1.0
    e0, e1 = -17, 1
    x0, y0, w, h = ox + 56, TOP + 10, PW - 80, PH - TOP - 76

    def X(v):
        return x0 + (v - lo) / span * w

    def Y(err):
        e = math.log10(max(err, 10.0 ** e0))
        return y0 + h - (min(e, e1) - e0) / (e1 - e0) * h
    o = heading(ox, title, note)
    o.append('<rect x="%d" y="%d" width="%d" height="%d" fill="none" '
             'stroke="#999"/>' % (x0, y0, w, h))
    for e in range(e0, e1 + 1, 3):
        o.append('<line x1="%d" y1="%.1f" x2="%d" y2="%.1f" stroke="#eee"/>'
                 '<text x="%d" y="%.1f" text-anchor="end">1e%d</text>' %
                 (x0, Y(10.0 ** e), x0 + w, Y(10.0 ** e), x0 - 4,
                  Y(10.0 ** e) + 4, e))
    for name, col, ys in series:
        for a, b in zip(xv, ys):
            o.append('<circle cx="%.1f" cy="%.1f" r="2.0" fill="%s" '
                     'fill-opacity="0.7"/>' %
                     (X(a), Y(abs(b - a) / big), col))
    for v, anchor in ((lo, "start"), (hi, "end")):
        o.append('<text x="%.1f" y="%d" text-anchor="%s">%.3g</text>' %
                 (X(v), y0 + h + 14, anchor, v))
    o.append('<text x="%d" y="%d">Voronoi volume, independent (Python, '
             'origin)</text>' % (x0 + w // 2 - 120, y0 + h + 30))
    ly = PH - 12
    x = ox + 14
    for name, col, ys in series:
        o.append('<circle cx="%d" cy="%d" r="3" fill="%s"/><text x="%d" '
                 'y="%d">%s</text>' % (x, ly - 4, col, x + 7, ly,
                                       html.escape(name)))
        x += 200
    return o


def write_svg(path, panels):
    W, H = PW * len(panels), PH
    with open(path, "w") as f:
        f.write('<svg xmlns="http://www.w3.org/2000/svg" viewBox="0 0 %d '
                '%d" width="%d" height="%d" font-family="sans-serif" '
                'font-size="11"><rect width="100%%" height="100%%" '
                'fill="#fff"/>\n' % (W, H, W, H))
        for o in panels:
            f.write("\n".join(o) + "\n")
        f.write("</svg>\n")


# --------------------------------------------------------------- checks
def verdict(ok):
    return "PASS" if ok else "FAIL"


def judge(err, tol, root, check):
    """(counts as passed, word): a KNOWN problem does not fail while
    its error stays below TOL_KNOWN."""
    if (root, check) in KNOWN:
        if err <= tol:
            return True, "FIXED"
        return (True, "KNOWN") if err <= TOL_KNOWN else (False, "FAIL")
    return err <= tol, verdict(err <= tol)


def check_stor(work, root, kind, mesh, lines):
    """One pair of .stor files; returns ok, the UTM copy's volumes."""
    two_d = kind == "stor2d"
    o = read_stor(os.path.join(work, root + "_o.stor"))
    u = read_stor(os.path.join(work, root + "_u.stor"))
    ok = True
    same = o["rows"] == u["rows"] and o["cols"] == u["cols"]
    if same:
        dv, dc = maxrel(o["vol"], u["vol"]), maxrel(o["coef"], u["coef"])
        same = dv <= TOL_SAME and dc <= TOL_SAME
        lines.append("%s %s same: origin vs UTM, volumes %.1e, "
                     "coefficients %.1e (tol %.0e)" %
                     (verdict(same), root, dv, dc, TOL_SAME))
    else:
        lines.append("FAIL %s same: the matrix has other rows or "
                     "columns at UTM" % root)
    ok &= same
    xyz, cells = mesh
    vol, coef, total = (voronoi_2d if two_d else voronoi_3d)(xyz, cells)
    for name, s in (("origin", o), ("UTM", u)):
        d = abs(sum(s["vol"]) - total) / sum(abs(v) for v in s["vol"])
        neg = sum(1 for v in s["vol"] if v < 0)
        good, word = judge(d, TOL_SUM, root, "sum")
        ok &= good
        lines.append("%s %s sum (%s): %.15g, cells %.15g (%.1e of "
                     "sum |vol|); %d "
                     "negative" % (word, root, name,
                                   sum(s["vol"]), total, d, neg))
    if kind != "hybrid":
        dv = maxrel(vol, o["vol"])
        mc = max(abs(c) for c in o["coef"]) or 1.0
        dc = 0.0
        for r in range(o["neq"]):
            for k in range(o["rows"][r], o["rows"][r + 1]):
                c = o["cols"][k]
                if c != r:
                    v = coef.get((min(r, c), max(r, c)), 0.0)
                    dc = max(dc, abs(-v - o["coef"][k]) / mc)
        good = dv <= TOL_INDEP and dc <= TOL_INDEP
        ok &= good
        lines.append("%s %s indep: vs Python, volumes %.1e, "
                     "coefficients %.1e (tol %.0e)" %
                     (verdict(good), root, dv, dc, TOL_INDEP))
    return ok, u["vol"], vol


def check_uge(work, root, mesh, lines):
    co, no = read_uge(os.path.join(work, root + "_o.uge"))
    cu, nu = read_uge(os.path.join(work, root + "_u.uge"))
    if len(co) != len(cu) or [c[:2] for c in no] != [c[:2] for c in nu]:
        lines.append("FAIL %s same: other cells or connections at UTM"
                     % root)
        return False, [c[1] for c in cu], voronoi_3d(*mesh)[0]
    dv = maxrel([c[1] for c in co], [c[1] for c in cu])
    da = maxrel([c[3] for c in no], [c[3] for c in nu])
    dx = max(max(abs(b[0][k] - SHIFT[k] - a[0][k]) for k in range(3))
             for a, b in zip(co + [(c[2], 0) for c in no],
                             cu + [(c[2], 0) for c in nu]))
    ok = dv <= TOL_SAME and da <= TOL_SAME and dx <= 1e-6
    lines.append("%s %s same: origin vs UTM, volumes %.1e, areas %.1e "
                 "(tol %.0e); centers u - SHIFT - o %.1e (tol 1e-6, "
                 "13 digits)" % (verdict(ok), root, dv, da, TOL_SAME, dx))
    xyz, cells = mesh
    vol, coef, total = voronoi_3d(xyz, cells)
    d = abs(sum(c[1] for c in cu) - total) / sum(abs(c[1]) for c in cu)
    good = d <= TOL_SUM
    lines.append("%s %s sum (UTM): %.15g, cells %.15g (%.1e)" %
                 (verdict(good), root, sum(c[1] for c in cu), total, d))
    di = maxrel(vol, [c[1] for c in co])
    good2 = di <= TOL_INDEP
    lines.append("%s %s indep: volumes vs Python %.1e (tol %.0e)" %
                 (verdict(good2), root, di, TOL_INDEP))
    return ok and good and good2, [c[1] for c in cu], vol


# st08: the files each dump must write (root, extensions)
LONG_NAMES = [
    ("st08_stor_26_characters_xx", [".stor"]),
    ("st08_stor_30_characters_xxxxxx", [".stor"]),
    ("st08_stor_32_characters_xxxxxxxx", [".stor", ".uge"]),
    ("st08_fehm_29_characters_xxxxx",
     [".fehmn", ".stor", "_material.zone", "_outside.zone",
      "_outside_vor.area", "_multi_mat.zone", "_interface.zone"]),
    ("st08_tecplot_31_characters_xxxx", [".plt"]),
    ("st08_2d_stor_31_characters_xxxx", [".stor"]),
]


def check_names(work, lines):
    """Every expected file exists (with one material the zone files
    _multi_mat and _interface are empty), and no other st08 file was made (a
    name cut short)."""
    ok = True
    want = set()
    for root, exts in LONG_NAMES:
        for e in exts:
            want.add(root + e)
            good = os.path.isfile(os.path.join(work, root + e))
            ok &= good
            lines.append("%s %s (%d + %d characters)" %
                         (verdict(good), root + e, len(root), len(e)))
    extra = sorted(f for f in os.listdir(work)
                   if f.startswith("st08_") and f not in want and
                   not f.endswith((".lgi", ".out")))
    good = not extra
    ok &= good
    lines.append("%s no other file (a cut name)%s" %
                 (verdict(good), ": " + " ".join(extra) if extra else ""))
    return ok


def check_case(work, logdir, deck, title, pairs):
    lines, pics = [], []
    out = os.path.join(work, deck + ".out")
    if os.path.isfile(out):
        with open(out, errors="replace") as f:
            txt = f.read()
        if "LaGriT successfully completed" not in txt and \
                "successfully completed" not in txt:
            lines.append("FAIL LaGriT did not finish (see %s.stdout)"
                         % deck)
            return False, lines, pics, "crash"
    if pairs[0][1] == "names":                # no mesh, no picture
        ok = check_names(work, lines)
        nfail = sum(1 for l in lines if l.startswith("FAIL"))
        return ok, lines, pics, "%d checks, %d failed" % (len(lines),
                                                          nfail)
    mesh = read_avs(os.path.join(work, deck[:4] + "_mesh.inp"))
    ok, first = True, None
    for root, kind in pairs:
        if kind == "uge":
            good, uvol, ivol = check_uge(work, root, mesh, lines)
        else:
            good, uvol, ivol = check_stor(work, root, kind, mesh, lines)
        ok &= good
        if first is None or kind not in ("hybrid",):
            first = (root, kind, uvol, ivol)
    root, kind, uvol, ivol = first
    two_d = kind == "stor2d"
    xyz = [(p[0], p[1], 0.0) for p in mesh[0]] if two_d else mesh[0]
    series = [("LaGriT at UTM", "#1f77b4", uvol)]
    ref = os.environ.get("REF_STOR")
    if ref:
        try:
            ext = ".uge" if kind == "uge" else ".stor"
            path = os.path.join(ref, root + "_u" + ext)
            rv = ([c[1] for c in read_uge(path)[0]] if kind == "uge"
                  else read_stor(path)["vol"])
            if len(rv) == len(ivol):
                series.append(("reference lagrit at UTM", "#d62728", rv))
        except (OSError, ValueError, IndexError):
            pass
    nm = "%d vertices, %d %s" % (len(xyz), len(mesh[1]),
                                 "triangles" if two_d else "tets")
    panels = [panel_mesh(xyz, mesh[1], uvol, two_d, 0,
                         deck + ": the mesh tested", nm),
              panel_error(ivol, series, PW, "error per vertex at UTM",
                          "|LaGriT - Python| / largest volume; the .stor "
                          "has 13 digits: ~1e-13 is the floor")]
    svg = deck + ".svg"
    write_svg(os.path.join(logdir, svg), panels)
    pics.append(svg)
    nfail = sum(1 for l in lines if l.startswith("FAIL"))
    stat = "%d checks, %d failed" % (len(lines), nfail)
    return ok, lines, pics, stat


def main(work, logdir):
    os.makedirs(logdir, exist_ok=True)
    rows, allok = [], True
    for deck, title, pairs, what in CASES:
        try:
            ok, lines, pics, stat = check_case(work, logdir, deck, title,
                                               pairs)
        except Exception as e:          # a broken output is a failure
            ok, lines, pics, stat = False, ["FAIL checker: %r" % e], [], ""
        allok &= ok
        with open(os.path.join(logdir, deck + ".check"), "w") as f:
            f.write("\n".join(lines) + "\n")
        rows.append((deck, ok, title, what, lines, pics, stat))
        print("%-22s %s  %s" % (deck, verdict(ok), stat))
    with open(os.path.join(logdir, "table.txt"), "w") as f:
        for deck, ok, title, what, lines, pics, stat in rows:
            f.write("%-22s %s  %s\n" % (deck, verdict(ok), stat))
    npass = sum(r[1] for r in rows)
    o = ["<!doctype html><html><head><meta charset='utf-8'>",
         "<title>test_stor_utm</title>",
         "<style>body{font-family:sans-serif;max-width:1150px;margin:20px "
         "auto;padding:0 16px}td,th{padding:4px 8px;border-bottom:1px solid "
         "#ddd;vertical-align:top}.P{color:#080}.F{color:#c00}"
         "pre{font-size:12px;margin:2px 0;white-space:pre-wrap}"
         "details{font-size:13px;max-width:330px}"
         "img{max-width:100%}</style></head><body>",
         "<p style='margin:0 0 8px;font-size:13px'><a href='../../summary"
         ".html'>&larr; all suites (top-level summary)</a></p>",
         "<h1>test_stor_utm: %d of %d decks pass</h1>" % (npass, len(rows)),
         "<p><b>What it does.</b> Each deck writes one mesh twice with "
         "<code>dump / stor</code> (or <code>dump / pflotran</code>): at "
         "the origin and moved to UTM-sized coordinates (7e5, 3.2e6, "
         "1.5e3), with the same geometry to the last bit. The two files "
         "must hold the same Voronoi volumes and coefficients (to 1e-12), "
         "the volumes must add up to the volume of the cells, and the "
         "origin file must agree with an independent computation in plain "
         "Python. Every .stor writer is covered: the 3D default "
         "(anothermatbld3d, compression all and graph, hybrid), the older "
         "3D writer (matbld3d_stor: none, coefs) and 2D "
         "(matbld2d_stor).</p>",
         "<table><tr><th>deck</th><th>result</th><th>what</th>"
         "<th>checks</th></tr>"]
    for deck, ok, title, what, lines, pics, stat in rows:
        o.append("<tr><td><a href='%s.check'>%s</a><details><summary>"
                 "What it does</summary>%s</details></td><td class='%s'>"
                 "%s</td><td>%s</td><td><pre>%s</pre></td></tr>" %
                 (deck, deck, html.escape(what), "P" if ok else "F",
                  verdict(ok), html.escape(title),
                  html.escape("\n".join(lines))))
    o.append("</table>")
    for deck, ok, title, what, lines, pics, stat in rows:
        for p in pics:
            o.append("<h3>%s</h3><p>%s</p><img src='%s'>" %
                     (deck, html.escape(title), p))
    o.append("</body></html>")
    with open(os.path.join(logdir, "summary.html"), "w") as f:
        f.write("\n".join(o) + "\n")
    print("%d of %d decks pass" % (npass, len(rows)))
    return 0 if allok else 1


if __name__ == "__main__":
    sys.exit(main(sys.argv[1], sys.argv[2]))
