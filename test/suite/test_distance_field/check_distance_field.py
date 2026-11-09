#!/usr/bin/env python3
"""check_distance_field.py WORK LOGDIR

Checks test_distance_field against brute force: for every sink vertex,
the exact distance to every facet of the source, written here
independently of LaGriT's Fortran (projection onto the triangle's
plane when it falls inside, else the nearest of the three edges).
Quads are cut into 4 triangles at the mean of their 4 vertices, as
hextotet / 4 does. To keep plain Python fast, the facets are visited
nearest bounding sphere first and the search stops when no sphere can
hold a nearer point; that changes the order, not the answer.

Writes LOGDIR/<deck>.check, LOGDIR/table.txt, LOGDIR/summary.html and
the pictures LOGDIR/*.svg (the source in 3D, lines from sink points
to their closest points; a slice or a scatter plot).
Exit 0 if every deck passes.
"""
import html
import math
import os
import random
import sys
import time

TOL = 1e-10          # relative to the source's size


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


def dot(a, b):
    return a[0] * b[0] + a[1] * b[1] + a[2] * b[2]


def cross(a, b):
    return (a[1] * b[2] - a[2] * b[1], a[2] * b[0] - a[0] * b[2],
            a[0] * b[1] - a[1] * b[0])


def seg_d2(p, a, b):
    ab = sub(b, a)
    s = dot(ab, ab)
    t = 0.0 if s <= 0 else max(0.0, min(1.0, dot(sub(p, a), ab) / s))
    q = (a[0] + t * ab[0], a[1] + t * ab[1], a[2] + t * ab[2])
    d = sub(p, q)
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
        self.rad = max(math.sqrt(dot(sub(v, self.cen), sub(v, self.cen)))
                       for v in (a, b, c))

    def d2(self, p):
        a, b, c = self.a, self.b, self.c
        best = min(seg_d2(p, a, b), seg_d2(p, b, c), seg_d2(p, c, a))
        if not self.flat:
            n = self.n
            h = dot(sub(p, a), n)
            q = (p[0] - h * n[0], p[1] - h * n[1], p[2] - h * n[2])
            if (dot(cross(sub(b, a), sub(q, a)), n) >= 0 and
                    dot(cross(sub(c, b), sub(q, b)), n) >= 0 and
                    dot(cross(sub(a, c), sub(q, c)), n) >= 0):
                best = min(best, h * h)
        return best


def facets(xyz, cells):
    out = []
    for t, v in cells:
        p = [xyz[i] for i in v]
        if t == "tri":
            out.append(Facet(p[0], p[1], p[2]))
        elif t == "line":
            out.append(Facet(p[0], p[1], p[1]))
        elif t == "quad":
            m = tuple(sum(q[i] for q in p) / 4 for i in range(3))
            for k in range(4):
                out.append(Facet(p[k], p[(k + 1) % 4], m))
        else:
            raise ValueError("cell type " + t)
    return out


def facet_dist(p, F):
    """Exact distance from p to the facets F (pruned brute force)."""
    lb = sorted((math.sqrt(dot(sub(p, f.cen), sub(p, f.cen))) - f.rad, k)
                for k, f in enumerate(F))
    best = math.inf
    for low, k in lb:
        if low > 0 and low * low > best:
            break
        best = min(best, F[k].d2(p))
    return math.sqrt(best)


def vertex_dist(p, V):
    return math.sqrt(min(dot(sub(p, v), sub(p, v)) for v in V))


def size_of(V):
    return max(max(v[i] for v in V) - min(v[i] for v in V) for i in range(3))


# ------------------------------------------------------------- pictures
# SVG drawn by hand (standard library only). Each panel has a title, a
# one-line note under it, then the plot:
#   3D:      the source (shaded triangles, line segments), a sample of
#            sink points, and a line from each to its closest point on
#            the source, colored by where that point is
#   slice:   a value on the grid sink points of one plane z = const,
#            with the source's cut by that plane drawn in white
#   scatter: one quantity against another for the sink points
REGION_COLOR = {"face": "#1f77b4", "edge": "#ff7f0e", "vertex": "#d62728"}
RAMP = [(68, 1, 84), (59, 82, 139), (33, 145, 140), (94, 201, 98),
        (253, 231, 37)]                       # viridis-like
PW, PH = 440, 420                             # one panel
TOP = 46                                      # the plot starts here


def color(t):
    t = max(0.0, min(1.0, t)) * (len(RAMP) - 1)
    i = min(int(t), len(RAMP) - 2)
    f = t - i
    c = [round(RAMP[i][k] + f * (RAMP[i + 1][k] - RAMP[i][k]))
         for k in range(3)]
    return "#%02x%02x%02x" % tuple(c)


def heading(ox, title, note):
    return ['<text x="%d" y="18" font-size="13">%s</text>' %
            (ox + 10, html.escape(title)),
            '<text x="%d" y="34" fill="#666">%s</text>' %
            (ox + 10, html.escape(note))]


def closest(p, F):
    """Closest point on the facets F to p, and where it lies:
    face, edge or vertex (a segment's ends are vertices)."""
    best = (math.inf, None, None)
    for f in F:
        a, b, c = f.a, f.b, f.c
        if not f.flat:
            n = f.n
            h = dot(sub(p, a), n)
            q = (p[0] - h * n[0], p[1] - h * n[1], p[2] - h * n[2])
            if (dot(cross(sub(b, a), sub(q, a)), n) >= 0 and
                    dot(cross(sub(c, b), sub(q, b)), n) >= 0 and
                    dot(cross(sub(a, c), sub(q, c)), n) >= 0):
                if h * h < best[0]:
                    best = (h * h, q, "face")
                continue
        for u, v in ((a, b), (b, c), (c, a)):
            uv = sub(v, u)
            s = dot(uv, uv)
            if s <= 0:
                continue
            t = max(0.0, min(1.0, dot(sub(p, u), uv) / s))
            q = (u[0] + t * uv[0], u[1] + t * uv[1], u[2] + t * uv[2])
            d2 = dot(sub(p, q), sub(p, q))
            if d2 < best[0] - 1e-15:
                best = (d2, q, "vertex" if t in (0.0, 1.0) else "edge")
    return best[1], best[2]


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
                     (PH - TOP - 36) / ((y1 - self.y0) or 1.0))
        self.yh = y1 - self.y0

    def raw(self, p):
        x1 = p[0] * self.ca - p[1] * self.sa
        y1 = p[0] * self.sa + p[1] * self.ca
        return (x1, p[2] * self.ce + y1 * self.se,
                y1 * self.ce - p[2] * self.se)     # depth: larger = farther

    def __call__(self, p, ox=0):
        x, y, d = self.raw(p)
        return (ox + 20 + (x - self.x0) * self.k,
                TOP + 4 + (self.yh - (y - self.y0)) * self.k, d)


def panel_3d(F, P, show, ox, title, note):
    """Source F, sink points P[i] for i in show, lines to the closest
    points; painter's algorithm over triangles, lines and dots."""
    near = [(i,) + closest(P[i], F) for i in show]
    view = View([v for f in F for v in (f.a, f.b, f.c)] +
                [P[i] for i in show])
    items = []                                     # (depth, svg)
    for f in F:
        A, B, C = view(f.a, ox), view(f.b, ox), view(f.c, ox)
        if f.flat:                                 # a line segment
            items.append(((A[2] + B[2]) / 2,
                          '<line x1="%.1f" y1="%.1f" x2="%.1f" y2="%.1f" '
                          'stroke="#444" stroke-width="2.4"/>' %
                          (A[0], A[1], B[0], B[1])))
            continue
        nz = abs((B[0] - A[0]) * (C[1] - A[1]) - (B[1] - A[1]) *
                 (C[0] - A[0]))
        n3 = cross(sub(f.b, f.a), sub(f.c, f.a))
        g = int(150 + 90 * min(1.0, nz / (math.sqrt(dot(n3, n3)) *
                                          view.k ** 2 + 1e-300)))
        items.append((max(A[2], B[2], C[2]),
                      '<polygon points="%.1f,%.1f %.1f,%.1f %.1f,%.1f" '
                      'fill="rgb(%d,%d,%d)" fill-opacity="0.85" '
                      'stroke="#888" stroke-width="0.4"/>' %
                      (A[0], A[1], B[0], B[1], C[0], C[1], g, g, g)))
    for i, q, reg in near:
        S, Q = view(P[i], ox), view(q, ox)
        col = REGION_COLOR[reg]
        items.append((min(S[2], Q[2]) - 1e-9,
                      '<line x1="%.1f" y1="%.1f" x2="%.1f" y2="%.1f" '
                      'stroke="%s" stroke-width="1.5"/>' %
                      (S[0], S[1], Q[0], Q[1], col)))
        items.append((S[2] - 2e-9, '<circle cx="%.1f" cy="%.1f" r="2.6" '
                      'fill="#000"/>' % (S[0], S[1])))
        items.append((Q[2] - 2e-9, '<circle cx="%.1f" cy="%.1f" r="2.1" '
                      'fill="%s"/>' % (Q[0], Q[1], col)))
    items.sort(key=lambda t: -t[0])
    o = heading(ox, title, note) + [s for _, s in items]
    ly = PH - 10
    o.append('<circle cx="%d" cy="%d" r="2.6" fill="#000"/>'
             '<text x="%d" y="%d">sink point</text>' %
             (ox + 14, ly - 4, ox + 20, ly))
    x = ox + 92
    for reg in ("face", "edge", "vertex"):
        n = sum(r == reg for _, _, r in near)
        if n:
            o.append('<line x1="%d" y1="%d" x2="%d" y2="%d" stroke="%s" '
                     'stroke-width="2"/><text x="%d" y="%d">to a %s (%d)'
                     '</text>' % (x, ly - 4, x + 16, ly - 4,
                                  REGION_COLOR[reg], x + 20, ly, reg, n))
            x += 112
    return o


def cut_segments(F, z, eps=1e-12):
    """The source cut by the plane z = const, as 2D segments (an edge
    crossing the plane, or a vertex on it)."""
    segs = []
    for f in F:
        vs = [f.a, f.b] if f.flat else [f.a, f.b, f.c]
        pts = [(v[0], v[1]) for v in vs if abs(v[2] - z) <= eps]
        for k in range(len(vs)):
            u, v = vs[k], vs[(k + 1) % len(vs)]
            du, dv = u[2] - z, v[2] - z
            if (du > eps and dv < -eps) or (du < -eps and dv > eps):
                t = du / (du - dv)
                pts.append((u[0] + t * (v[0] - u[0]),
                            u[1] + t * (v[1] - u[1])))
        pts = sorted(set((round(x, 12), round(y, 12)) for x, y in pts))
        if len(pts) >= 2:
            segs.append((pts[0], pts[-1]))
    return segs


def panel_slice(xy, val, segs, ox, title, note):
    """A map of VAL on a grid slice, the source's cut in white."""
    xs = sorted(set(round(p[0], 9) for p in xy))
    ys = sorted(set(round(p[1], 9) for p in xy))
    x0, x1, y0, y1 = xs[0], xs[-1], ys[0], ys[-1]
    S = PH - TOP - 30
    span = max(x1 - x0, y1 - y0) or 1.0
    cell = S / span * (x1 - x0) / max(len(xs) - 1, 1)
    lo, hi = min(val), max(val)
    X = lambda x: ox + 20 + cell / 2 + (x - x0) / span * (S - cell)
    Y = lambda y: TOP + cell / 2 + (y1 - y) / span * (S - cell)
    o = heading(ox, title, note)
    for (x, y), v in zip(xy, val):
        o.append('<rect x="%.1f" y="%.1f" width="%.2f" height="%.2f" '
                 'fill="%s"/>' % (X(x) - cell / 2, Y(y) - cell / 2,
                                  cell + 0.6, cell + 0.6,
                                  color((v - lo) / (hi - lo) if hi > lo
                                        else 0)))
    for (a, b) in segs:
        o.append('<line x1="%.1f" y1="%.1f" x2="%.1f" y2="%.1f" '
                 'stroke="#fff" stroke-width="2" stroke-linecap="round"/>'
                 % (X(a[0]), Y(a[1]), X(b[0]), Y(b[1])))
    bx = ox + S + 36
    for k in range(50):
        o.append('<rect x="%d" y="%.1f" width="12" height="%.2f" '
                 'fill="%s"/>' % (bx, TOP + S * (1 - (k + 1) / 50),
                                  S / 50 + 0.5, color(k / 49)))
    o.append('<text x="%d" y="%d">%.3g</text>' % (bx + 16, TOP + 10, hi))
    o.append('<text x="%d" y="%d">%.3g</text>' % (bx + 16, TOP + S, lo))
    o.append('<text x="%d" y="%d">x across, y up; %d x %d grid points'
             '</text>' % (ox + 20, PH - 10, len(xs), len(ys)))
    return o


def panel_xy(xv, yv, ox, title, note, xlab, ylab, diag):
    """Scatter of yv against xv; with DIAG, the line y = x."""
    S = PH - TOP - 62
    xh = max(xv) or 1.0
    yh = max(yv) or 1.0
    if diag:
        xh = yh = max(xh, yh)
    X = lambda v: ox + 64 + v / xh * S
    Y = lambda v: TOP + S - v / yh * S
    o = heading(ox, title, note)
    o.append('<rect x="%.1f" y="%d" width="%d" height="%d" fill="none" '
             'stroke="#999"/>' % (X(0), TOP, S, S))
    if diag:
        o.append('<line x1="%.1f" y1="%.1f" x2="%.1f" y2="%.1f" '
                 'stroke="#bbb" stroke-dasharray="4,3"/>' %
                 (X(0), Y(0), X(xh), Y(yh)))
    for x, y in zip(xv, yv):
        o.append('<circle cx="%.1f" cy="%.1f" r="1.8" fill="#1f77b4" '
                 'fill-opacity="0.6"/>' % (X(x), Y(y)))
    for v in (0, xh / 2, xh):
        o.append('<text x="%.1f" y="%d" text-anchor="middle">%.2g</text>'
                 % (X(v), TOP + S + 14, v))
    for v in (0, yh / 2, yh):
        o.append('<text x="%.1f" y="%.1f" text-anchor="end">%.2g</text>'
                 % (X(0) - 4, Y(v) + 4, v))
    o.append('<text x="%.1f" y="%d" text-anchor="middle">%s</text>' %
             (X(xh / 2), TOP + S + 32, html.escape(xlab)))
    o.append('<text x="%d" y="%.1f" text-anchor="middle" transform="rotate'
             '(-90 %d %.1f)">%s</text>' % (ox + 18, Y(yh / 2), ox + 18,
                                           Y(yh / 2), html.escape(ylab)))
    return o


def write_svg(path, panels):
    """Panels drawn at x = 0 or PW; two per row."""
    rows = (len(panels) + 1) // 2
    W, H = PW * min(2, len(panels)), PH * rows
    with open(path, "w") as f:
        f.write('<svg xmlns="http://www.w3.org/2000/svg" viewBox="0 0 %d '
                '%d" width="%d" height="%d" font-family="sans-serif" '
                'font-size="11"><rect width="100%%" height="100%%" '
                'fill="#fff"/>\n' % (W, H, W, H))
        for k, o in enumerate(panels):
            f.write('<g transform="translate(0,%d)">\n' % (PH * (k // 2)))
            f.write("\n".join(o) + "\n</g>\n")
        f.write("</svg>\n")


def sample(idx, n, seed):
    """n of the indices idx, the same every run."""
    idx = list(idx)
    return sorted(random.Random(seed).sample(idx, min(n, len(idx))))


SOURCE_NAME = {"df01": "one tilted triangle",
               "df02": "sphere, 320 triangles",
               "df03": "warped surface, 36 quads (144 triangles)",
               "df04": "helix, 80 line segments",
               "df05": "8 triangles (z = 0.5) and 2 segments",
               "df08": "sphere, 5120 triangles"}


def picture(logdir, deck, F, P, d, att, ref):
    """Write LOGDIR/<deck>.svg; return its name."""
    key = deck[:4]
    nsink = len(P) - 7 if key == "df01" else len(P)
    if key == "df01":                  # grid points near the triangle
        show = sample([i for i in range(nsink) if d[i] < 0.45], 45, 1)
    elif key == "df08":                # grid points near the sphere
        show = sample([i for i in range(0, nsink, 7) if d[i] < 0.3], 40, 8)
    else:
        show = sample(range(nsink), 40, 2)
    panels = [panel_3d(F, P, show, 0, "source: " + SOURCE_NAME[key],
                       "%d of the %d sink points, each joined to its "
                       "closest point" % (len(show), nsink))]
    if key in ("df01", "df08"):
        zs = 0.4 if key == "df01" else 0.0
        zl = min(set(P[i][2] for i in range(nsink)),
                 key=lambda z: abs(z - zs))
        keep = [i for i in range(nsink) if abs(P[i][2] - zl) < 1e-9]
        xy = [P[i][:2] for i in keep]
        cut = cut_segments(F, zl)
        panels.append(panel_slice(xy, [d[i] for i in keep], cut, PW,
                                  "facet distance (LaGriT), plane z = "
                                  "%.2f" % zl, "white: the source cut by "
                                  "this plane (0 along it)"))
        if "dvert" in att:
            panels.append(panel_slice(
                xy, [att["dvert"][i] - d[i] for i in keep], cut, 0,
                "vertex minus facet distance, plane z = %.2f" % zl,
                "largest next to the surface, between its vertices"))
    if "dvert" in att:
        step = 10 if key == "df08" else 1
        panels.append(panel_xy(
            d[::step], [a - b for a, b in zip(att["dvert"][::step],
                                              d[::step])],
            PW if len(panels) % 2 else 0,
            "vertex minus facet distance, by distance",
            "%s sink points: the error is largest at the surface"
            % ("every 10th of the" if step > 1 else "all"),
            "facet distance (exact)", "vertex - facet", False))
    if key not in ("df01", "df08") and "dvert" not in att:
        k = sorted(ref)
        panels.append(panel_xy([ref[i] for i in k], [d[i] for i in k], PW,
                               "LaGriT against brute force",
                               "all %d sink points: every one on the line "
                               "y = x" % len(k), "brute force (Python)",
                               "LaGriT facet distance", True))
    write_svg(os.path.join(logdir, deck + ".svg"), panels)
    return [deck + ".svg"]


# ---------------------------------------------------------------- cases
CASES = [
    # deck, source, description
    ("df01_one_triangle", "df01_src.inp",
     "one tilted triangle; sink points in every face, edge and vertex "
     "region, and 7 points on the triangle (distance 0)"),
    ("df02_sphere", "df02_src.inp",
     "triangulated sphere (320 triangles), 1500 random sink points; "
     "the vertex option of the same run for comparison"),
    ("df03_quads", "df03_src.inp",
     "warped quad surface (36 quads, not planar): each quad cut into 4 "
     "triangles at its center by hextotet / 4"),
    ("df04_lines", "df04_src.inp", "helix of 80 line segments"),
    ("df05_tri_and_lines", "df05_src.inp",
     "8 triangles and 2 line segments in one source mesh object"),
    ("df06_vertex", "df02_src.inp",
     "vertex option unchanged: 5 words and 6 words (vertex), against "
     "the brute-force nearest vertex"),
    ("df07_errors", "df02_src.inp",
     "refused: tet source, quads mixed with triangles, no cells, bad "
     "6th word; LaGriT goes on and the next command works"),
    ("df08_big_sphere", "df08_src.inp",
     "5120 triangles, 68921 sink vertices (a 41^3 grid); brute force on "
     "every 10th vertex"),
]


def verdict(ok):
    return "PASS" if ok else "FAIL"


def check_case(work, logdir, deck, src):
    lines, ok, pics, stat = [], True, [], ""
    out = os.path.join(work, deck.split("_")[0] + "_out.inp")
    stdout = open(os.path.join(work, deck + ".out"),
                  errors="replace").read()
    if "Fortran runtime error" in stdout or "Segmentation" in stdout:
        return False, ["FAIL LaGriT crashed"], pics, "crash"
    if not os.path.exists(out):
        return False, ["FAIL no output file " + os.path.basename(out)], \
            pics, ""
    P, _, att = read_avs(out)
    V, cells, _ = read_avs(os.path.join(work, src))
    tol = TOL * max(size_of(V), 1.0)

    if deck.startswith("df06"):
        e5 = max(abs(d - vertex_dist(p, V)) for p, d in zip(P, att["dv5"]))
        same = att["dv5"] == att["dv6"]
        good = e5 <= tol and same
        lines.append("%s 5 words = brute-force nearest vertex (%.1e); "
                     "6 words (vertex) identical: %s" %
                     (verdict(good), e5, same))
        return good, lines, pics, "err %.1e" % e5

    if deck.startswith("df07"):
        want = [("source cells must be", 2), ("source has no cells", 1),
                ("Argument 6 must be vertex or facet", 1)]
        for msg, n in want:
            k = stdout.count(msg)
            ok &= k == n
            lines.append("%s '%s' %d time(s) (want %d)" %
                         (verdict(k == n), msg, k, n))
        bad = [a for a in ("d1", "d2", "d3", "d4") if a in att]
        ok &= not bad
        lines.append("%s no attribute made by a refused command %s" %
                     (verdict(not bad), " ".join(bad)))
        F = facets(V, cells)
        err = max(abs(d - facet_dist(p, F)) for p, d in
                  zip(P, att["d5"])) if "d5" in att else math.inf
        ok &= err <= tol
        lines.append("%s the command after the errors works (%.1e)" %
                     (verdict(err <= tol), err))
        return ok, lines, pics, "4 refused"

    # the facet decks
    F = facets(V, cells)
    step = 10 if deck.startswith("df08") else 1
    t0 = time.time()
    idx = list(range(0, len(P), step))
    ref = {i: facet_dist(P[i], F) for i in idx}
    tb = time.time() - t0
    d = att["dfacet"]
    err = max(abs(d[i] - ref[i]) for i in idx)
    ok &= err <= tol
    lines.append("%s max |LaGriT - brute force| = %.3e (tol %.1e), "
                 "%d sink vertices checked, %d facets" %
                 (verdict(err <= tol), err, tol, len(idx), len(F)))
    lines.append("     brute force took %.1f s" % tb)
    stat = "max err %.1e" % err

    if deck.startswith("df01"):
        z = max(abs(x) for x in d[-7:])
        ok &= z <= tol
        lines.append("%s the 7 points on the triangle: max d = %.2e" %
                     (verdict(z <= tol), z))
        # independent of the closed form: a dense barycentric sample of
        # the triangle is an upper bound close to the answer
        a, b, c = V[cells[0][1][0]], V[cells[0][1][1]], V[cells[0][1][2]]
        n = 200
        S = [(a[0] + u / n * (b[0] - a[0]) + v / n * (c[0] - a[0]),
              a[1] + u / n * (b[1] - a[1]) + v / n * (c[1] - a[1]),
              a[2] + u / n * (b[2] - a[2]) + v / n * (c[2] - a[2]))
             for u in range(n + 1) for v in range(n + 1 - u)]
        gap = [vertex_dist(P[i], S) - d[i] for i in range(0, 400, 4)]
        good = min(gap) >= -tol and max(gap) < 0.01
        ok &= good
        lines.append("%s dense sample (%d points of the triangle, 100 sink "
                     "points): never closer than LaGriT, gap %.1e..%.1e" %
                     (verdict(good), len(S), min(gap), max(gap)))

    if "dvert" in att:
        dv = att["dvert"]
        e2 = max(abs(dv[i] - vertex_dist(P[i], V)) for i in idx)
        below = all(d[i] <= dv[i] + tol for i in range(len(P)))
        good = e2 <= tol and below
        ok &= good
        lines.append("%s vertex option = brute-force nearest vertex "
                     "(%.1e); facet <= vertex everywhere: %s" %
                     (verdict(good), e2, below))
        diff = [dv[i] - d[i] for i in range(len(P))]
        lines.append("     vertex - facet: max %.4f, mean %.4f" %
                     (max(diff), sum(diff) / len(diff)))
        stat += ", vertex - facet up to %.3f" % max(diff)

    pics += picture(logdir, deck, F, P, d, att, ref)
    return ok, lines, pics, stat


def main(work, logdir):
    os.makedirs(logdir, exist_ok=True)
    rows, allok = [], True
    for deck, src, desc in CASES:
        try:
            ok, lines, pics, stat = check_case(work, logdir, deck, src)
        except Exception as e:          # a broken output is a failure
            ok, lines, pics, stat = False, ["FAIL checker: %r" % e], [], ""
        allok &= ok
        with open(os.path.join(logdir, deck + ".check"), "w") as f:
            f.write("\n".join(lines) + "\n")
        rows.append((deck, ok, desc, lines, pics, stat))
        print("%-22s %s  %s" % (deck, verdict(ok), stat))
    with open(os.path.join(logdir, "table.txt"), "w") as f:
        for deck, ok, desc, lines, pics, stat in rows:
            f.write("%-22s %s  %s\n" % (deck, verdict(ok), stat))
    npass = sum(r[1] for r in rows)
    o = ["<!doctype html><html><head><meta charset='utf-8'>",
         "<title>test_distance_field</title>",
         "<style>body{font-family:sans-serif;max-width:1150px;margin:20px "
         "auto;padding:0 16px}td,th{padding:4px 8px;border-bottom:1px solid "
         "#ddd;vertical-align:top}.P{color:#080}.F{color:#c00}"
         "pre{font-size:12px;margin:2px 0;white-space:pre-wrap}"
         "img{max-width:100%}</style></head><body>",
         "<p style='margin:0 0 8px;font-size:13px'><a href='../../summary"
         ".html'>&larr; all suites (top-level summary)</a></p>",
         "<h1>test_distance_field: %d of %d decks pass</h1>" %
         (npass, len(rows)),
         "<p><b>What this tests.</b> <code>compute / distance_field / "
         "mo_sink / mo_src / dfield / facet</code>: the exact distance "
         "from every sink vertex to the nearest point of the source's "
         "triangles, line segments or quads (inside a cell, on an edge or "
         "at a vertex), against a brute-force computation in plain Python. "
         "Also: the vertex option (5 words, the default) is unchanged, and "
         "bad inputs are refused.</p>",
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
