#!/usr/bin/env python3
"""
check_createpts.py - check one createpts deck's output against the
documented meaning of its options, and draw it.

    python3 check_createpts.py DECK.lgi PIC_PREFIX

Reads the deck's header lines (see "DECK HEADERS"), the vertex file the
deck dumped right after createpts, and, if any, the connected mesh it
dumped at the end. Prints one "key value" line per quantity, "-> HARD:"
for each failed check, "NOTE:" for behaviour that is reported but not
judged, then "HARD: pass" or "HARD: FAIL". Writes PIC_PREFIX_pts.svg
(the vertices) and PIC_PREFIX_mesh.svg (the connected mesh).

Exit status 0 = pass, 1 = fail, 2 = usage or unreadable input.
Python 3, standard library only. Nothing here shares code with LaGriT:
the expected points come from the DOCUMENTED meaning of the options
(lagrit.lanl.gov createpts pages), not from a copy of rz.f.

Terms: meshes have VERTICES and CELLS (LaGriT's own words, "nodes",
"elements", stay only where a file format or message uses them).

----------------------------------------------------------------------
DECK HEADERS
----------------------------------------------------------------------
  # spec    : what createpts was asked for, in the deck's own terms:
      xyz|rtz|rtp  ni,nj,nk  min1,min2,min3  max1,max2,max3  iiz,ijz,ikz
                   [ ratio  irat,jrat,krat  r1,r2,r3 ]
      line         n  x0,y0,z0  x1,y1,z1  iz  [ ratio r ]   (iz 0 with
                   ratio r: n cells s, s r, ... fill the line; the points
                   are their centres)
      brick xyz|rtz ...  (as xyz|rtz; the cells are checked too)
      none         (no expected point set: error and odd-input decks)
      vector GEOM n1,n2,n3 V1 V2 V3 [V0 [R1 R2 R3]]   createpts/vector:
                   point (i,j,k), 0 <= i <= n1 ..., is V0 plus i steps
                   along V1, j along V2, k along V3, where step m of Vd
                   is Vd times Rd^m component by component (R default
                   1,1,1); rtz / rtp: the sums are (r,theta,z) or
                   (r,theta,phi), turned into x,y,z at the end
      interp N || SPEC_A || SPEC_B   createpts/interp of point sets A
                   and B (made by SPEC_A, SPEC_B): for each pair
                   A_i, B_i, the points A_i + k (B_i - A_i) / (N + 1),
                   k = 0 ... N+1
      rotz DEG SPEC  the points of SPEC turned DEG about the z axis
      SPEC + SPEC  several createpts commands whose points were merged
                   (addmesh / merge): the points of each, in turn
  # points  : FILE   the vertices, dumped right after createpts
  # mesh    : FILE tet|tri|hex|quad   the connected mesh (optional)
  # filter  : yes    the deck ran filter + rmpoint/compress before
                     connect: the mesh must hold each DISTINCT point once
  # open    : TEXT   the documentation does not say what to expect: the
                     points are reported (NOTE), only the count judged
  # attr    : NAME FIRST LAST VALUE   node attribute NAME of mesh
                     vertices FIRST..LAST must still be VALUE
  # unused_ok : yes  unused mesh vertices are reported, not failed
  # coincident_ok : yes  mesh vertices at the same place are expected
                     (e.g. a brick's axis without filter): no NOTE
  # parents_ok : yes  multi-material mesh (settets): unused vertices
                     with itp1 = 41 are parents of interface vertices,
                     not an error; they are counted, not failed
  # shells  : R0,R1,...,Rn  M1,...,Mn   a spherical shell mesh: every
                     vertex used by a cell lies between R0 and Rn, and
                     every cell of material Mk lies between R(k-1) and Rk
  # volume  : V      the cells must fill exactly V (to 1e-9 relative)
  # vertices : N    the mesh has N vertices;  # cells : M   M cells
  # cellattr : median|voronoi   the mesh's cell attributes xmed,ymed,
                     zmed (the average of the cell's vertices) or xvor,
                     yvor,zvor (the centre of the circumscribed sphere
                     or circle of a tet or tri) are checked cell by cell
  # random  : GEOM SPACING MIN MAX EDGEDIST   createpts/random: every
                     point inside; xyz: the 8 corners, points on each
                     of the 12 edges and 6 faces, interior points at
                     least EDGEDIST from the boundary; rtz (full circle):
                     inside the cylinder, interior points EDGEDIST from
                     its surface; rtp with rmin = rmax: all on the sphere
  # lattice : H      every mesh vertex coordinate is a multiple of H
  # same_as : FILE   the points equal those in FILE, in order
  # same_set : FILE  the same points as FILE, in any order (each once)
  # picture : panels FILE TYPE COLOUR [inside], FILE TYPE COLOUR, ...
                     several meshes the deck wrote, each checked (cell
                     type, no cell of volume <= 0, no unused vertex but
                     settets parents) and drawn side by side, 3 a row;
                     "inside": turned to show an octant's cut faces
  # picture : layers N [tilt DEG]   cells come N to a layer (interp):
                     each layer its own colour, blue (first) to red
                     (last), seen from DEG above the horizon (default 30)
  # picture : cutaway   draw the mesh with the quarter nearest the
                     viewer removed, coloured by material

----------------------------------------------------------------------
EXPECTED POINTS (documented meaning)
----------------------------------------------------------------------
  Along each direction, n values between min and max:
    iz = 1 (vertices):  min and max are the first and last value;
                        equal steps, or, with ratio zoning on (r),
                        steps s, s r, s r^2, ... that end exactly at max.
    iz = 0 (cell centres): n equal cells span [min, max]; the values
                        are their centres. With ratio zoning this is
                        an OPEN question (# open): not judged.
    n = 1:              the middle, (min + max) / 2, whatever iz says.
  xyz: (x, y, z).  rtz: (r cos t, r sin t, z), t in degrees from +x.
  rtp: (r sin t cos p, r sin t sin p, r cos t), t from +z, p from +x.
  line: n points from (x0,y0,z0) to (x1,y1,z1), one direction as above.

----------------------------------------------------------------------
HARD CHECKS
----------------------------------------------------------------------
  point_count        vertices written = ni nj nk (line: n)
  points_matched     every expected point has a vertex within
                     TOL = 1e-9 x (size of the point set), and the
                     reverse; max_point_error is the largest distance
  mesh_vertices      with "# filter : yes": the mesh has exactly one
                     vertex per DISTINCT expected point
  mesh_cell_type     every cell is of the type in "# mesh"
  brick_cells        brick: (ni-1)(nj-1)(nk-1) cells (one n of 1:
                     a quad layer, (a-1)(b-1) cells)
  nonpositive_cells  cells with volume (area in 2D) <= 0
  unused_vertices    mesh vertices used by no cell
  mesh_volume        sum of cell volumes (areas) = volume of the convex
                     hull of the points, for xyz and rtz (a prism of
                     the 2D hull of x,y); for rtp it is reported only.
                     A flat mesh (quads, triangles) may lie in the xy,
                     xz or yz plane: its area is measured in that plane
  orientation        a flat mesh: every cell turns the same way about
                     the plane's normal (in xy: counterclockwise)
  inner_faces        xyz tets: a cell face used by only one cell must lie
                     on a side of the box; one inside means the cells do
                     not match there (e.g. two blocks connected apart)
  attr               "# attr" values kept

REPORTED, NOT JUDGED
  point_order        whether the vertices come out in the order
                     t fastest (rtz), p fastest (rtp), x fastest (xyz)
  duplicate_points   expected points that coincide (axis, poles, seam)
  coincident_mesh_vertices   mesh vertices at the same place (a brick
                     around a full circle has its seam twice)
"""
import math
import re
import sys

TOL_REL = 1e-9


# ---------------------------------------------------------------- input
def headers(deck):
    h = {}
    with open(deck) as f:
        for line in f:
            m = re.match(r"^#\s*([A-Za-z_]+)\s*:\s*(.*?)\s*$", line)
            if m:
                h.setdefault(m.group(1).lower(), m.group(2))
    return h


def read_avs(fn):
    """vertices [(x,y,z)], cells [(type, [ids 0-based])], node attrs"""
    with open(fn) as f:
        tok = f.readline().split()
        nn, nc, nnd = int(tok[0]), int(tok[1]), int(tok[2])
        V = []
        for _ in range(nn):
            w = f.readline().split()
            V.append((float(w[1]), float(w[2]), float(w[3])))
        C, mats = [], []
        for _ in range(nc):
            w = f.readline().split()
            C.append((w[2], [int(v) - 1 for v in w[3:]]))
            mats.append(int(w[1]))
        attrs = {}
        if nnd > 0:
            w = f.readline().split()
            na = int(w[0])
            names = [f.readline().split(",")[0].strip() for _ in range(na)]
            vals = {n: [] for n in names}
            for _ in range(nn):
                w = f.readline().split()
                for n, v in zip(names, w[1:]):
                    vals[n].append(float(v))
            attrs = vals
        ncd = int(tok[3]) if len(tok) > 3 else 0
        if ncd > 0:
            w = f.readline().split()
            na = int(w[0])
            names = [f.readline().split(",")[0].strip() for _ in range(na)]
            cvals = {n: [] for n in names}
            for _ in range(nc):
                w = f.readline().split()
                for n, v in zip(names, w[1:]):
                    cvals[n].append(float(v))
            attrs["__cell"] = cvals
    attrs["__itetclr"] = mats
    return V, C, attrs


# --------------------------------------------------- expected points
def ints(s):
    return [int(float(v)) for v in s.split(",")]


def floats(s):
    return [float(v) for v in s.split(",")]


def dist1d(n, a, b, iz, ratio=None):
    """n documented values from a to b (None if undocumented)"""
    if n <= 1:
        return [0.5 * (a + b)]
    if iz == 1:
        if not ratio or ratio == 1.0:
            return [a + (b - a) * k / (n - 1) for k in range(n)]
        steps = [ratio ** k for k in range(n - 1)]
        s = (b - a) / sum(steps)
        out, x = [a], a
        for st in steps[:-1]:
            x += s * st
            out.append(x)
        out.append(b)
        return out
    if ratio and ratio != 1.0:
        return None                     # open question
    w = (b - a) / n
    return [a + w * (k + 0.5) for k in range(n)]


def vector_points(w):
    """createpts/vector, by its documented meaning (see DECK HEADERS)"""
    geom, n = w[0], ints(w[1])
    V = [floats(w[2]), floats(w[3]), floats(w[4])]
    V0 = floats(w[5]) if len(w) > 5 else [0.0, 0.0, 0.0]
    R = [floats(w[6 + d]) if len(w) > 6 + d else [1.0, 1.0, 1.0]
         for d in range(3)]

    def walk(d, i):            # the sum of the first i steps along V[d]
        return [sum(V[d][c] * R[d][c] ** m for m in range(i)) for c in range(3)]
    pts = []
    for k in range(n[2] + 1):
        for j in range(n[1] + 1):
            for i in range(n[0] + 1):
                a, b, c = walk(0, i), walk(1, j), walk(2, k)
                q = [V0[t] + a[t] + b[t] + c[t] for t in range(3)]
                if geom == "rtz":
                    t = math.radians(q[1])
                    q = [q[0] * math.cos(t), q[0] * math.sin(t), q[2]]
                elif geom == "rtp":
                    t, f = math.radians(q[1]), math.radians(q[2])
                    q = [q[0] * math.sin(t) * math.cos(f),
                         q[0] * math.sin(t) * math.sin(f), q[0] * math.cos(t)]
                pts.append(tuple(q))
    return pts


def expected(spec):
    """(points in documented order, kind, open?) from '# spec'; with
    'SPEC + SPEC' the points of each part in turn"""
    if " || " in spec:
        w = spec.split(" || ")
        n = int(w[0].split()[1])
        A, ka, oa = expected(w[1])
        B, kb, ob = expected(w[2])
        if A is None or B is None or len(A) != len(B):
            return None, "interp", True
        pts = []
        for a, b in zip(A, B):
            for k in range(n + 2):
                u = k / (n + 1.0)
                pts.append(tuple(a[i] + u * (b[i] - a[i]) for i in range(3)))
        return pts, "interp", False
    if spec.split()[0] == "rotz":
        # rotz DEG SPEC: the points of SPEC turned DEG about the z axis
        w = spec.split(None, 2)
        P, k, o = expected(w[2])
        if P is None:
            return None, k, o
        a = math.radians(float(w[1]))
        ca, sa = math.cos(a), math.sin(a)
        return [(ca * p[0] - sa * p[1], sa * p[0] + ca * p[1], p[2])
                for p in P], k, o
    if spec.split()[0] == "vector":
        return vector_points(spec.split()[1:]), "vector", False
    if " + " in spec:
        pts, kind, op = [], None, False
        for part in spec.split(" + "):
            p, k, o = expected(part)
            if p is None:
                return None, k if kind is None else kind, True
            pts += p
            kind = kind or k
            op = op or o
        return pts, kind, op
    w = spec.split()
    if not w or w[0] == "none":
        return None, "none", False
    if w[0] == "brick":
        w = w[1:]
    kind = w[0]
    if kind == "line":
        n = int(w[1])
        p0, p1 = floats(w[2]), floats(w[3])
        iz = int(w[4])
        r = float(w[6]) if len(w) > 6 and w[5] == "ratio" else None
        if iz == 0 and r and r != 1.0:
            # cell centres with ratio zoning: n cells of length s, s r,
            # ... s r^(n-1) fill the line; the points are their centres
            sc = 1.0 / sum(r ** k for k in range(n))
            t, a0 = [], 0.0
            for k in range(n):
                t.append(a0 + 0.5 * sc * r ** k)
                a0 += sc * r ** k
        else:
            t = dist1d(n, 0.0, 1.0, iz, r)
        if t is None:
            return None, kind, True
        return [tuple(p0[i] + (p1[i] - p0[i]) * u for i in range(3))
                for u in t], kind, False
    n = ints(w[1])
    lo, hi, iz = floats(w[2]), floats(w[3]), ints(w[4])
    rat, rv = [0, 0, 0], [1.0, 1.0, 1.0]
    if len(w) > 7 and w[5] == "ratio":
        rat, rv = ints(w[6]), floats(w[7])
    d = []
    for i in range(3):
        d.append(dist1d(max(1, n[i]), lo[i], hi[i], iz[i],
                        rv[i] if rat[i] else None))
    if any(x is None for x in d):
        return None, kind, True
    pts = []
    if kind == "xyz":
        for z in d[2]:
            for y in d[1]:
                for x in d[0]:
                    pts.append((x, y, z))
    elif kind == "rtz":
        for z in d[2]:
            for r in d[0]:
                for t in d[1]:
                    a = math.radians(t)
                    pts.append((r * math.cos(a), r * math.sin(a), z))
    elif kind == "rtp":
        for r in d[0]:
            for t in d[1]:
                for p in d[2]:
                    a, b = math.radians(t), math.radians(p)
                    pts.append((r * math.sin(a) * math.cos(b),
                                r * math.sin(a) * math.sin(b),
                                r * math.cos(a)))
    return pts, kind, False


# ---------------------------------------------------------- geometry
def bbox_size(P):
    if not P:
        return 1.0
    s = math.sqrt(sum((max(p[i] for p in P) - min(p[i] for p in P)) ** 2
                      for i in range(3)))
    return s if s > 0 else 1.0


class Grid:
    def __init__(self, P, w):
        self.P, self.w, self.g = P, w, {}
        for i, p in enumerate(P):
            self.g.setdefault(self.key(p), []).append(i)

    def key(self, p):
        return tuple(int(math.floor(c / self.w)) for c in p)

    def near(self, p, r):
        k = self.key(p)
        out = []
        for a in (-1, 0, 1):
            for b in (-1, 0, 1):
                for c in (-1, 0, 1):
                    for i in self.g.get((k[0] + a, k[1] + b, k[2] + c), ()):
                        if math.dist(self.P[i], p) <= r:
                            out.append(i)
        return out


def distinct(P, tol):
    """number of distinct points (within tol)"""
    g = Grid(P, max(tol * 4, 1e-300))
    seen, n = set(), 0
    for i, p in enumerate(P):
        if i in seen:
            continue
        n += 1
        for j in g.near(p, tol):
            seen.add(j)
    return n


def match(A, B, tol):
    """largest distance from a point of A to its nearest in B, and the
    number of A points with none within tol"""
    if not B:
        return float("inf"), len(A)
    g = Grid(B, max(tol * 50, bbox_size(B) / max(1, len(B)) ** (1 / 3)))
    worst, miss = 0.0, 0
    for p in A:
        c = g.near(p, g.w)
        d = min((math.dist(p, B[j]) for j in c), default=None)
        if d is None:
            d = min(math.dist(p, q) for q in B)
        worst = max(worst, d)
        if d > tol:
            miss += 1
    return worst, miss


def sub(a, b):
    return (a[0] - b[0], a[1] - b[1], a[2] - b[2])


def cross(a, b):
    return (a[1] * b[2] - a[2] * b[1], a[2] * b[0] - a[0] * b[2],
            a[0] * b[1] - a[1] * b[0])


def dot(a, b):
    return a[0] * b[0] + a[1] * b[1] + a[2] * b[2]


def tet_vol(a, b, c, d):
    return dot(sub(b, a), cross(sub(c, a), sub(d, a))) / 6.0


HEX_TETS = [(0, 1, 3, 4), (1, 2, 3, 6), (1, 3, 4, 6), (3, 4, 6, 7),
            (1, 4, 5, 6)]


def cell_measure(typ, X, normal=(0.0, 0.0, 1.0)):
    """signed volume (3D) or area (2D, from the xy normal sign), in
    LaGriT's own orientation convention: a tet (1,2,3,4) is positive
    when 1,2,3 turn CLOCKWISE seen from 4 (the opposite of the usual
    right-hand rule; LaGriT's quality reports these volumes positive),
    a hex likewise; a tri or quad is positive counterclockwise about
    'normal' (the flat mesh's plane: +z for xy)"""
    if typ == "tet":
        return -tet_vol(*X)
    if typ == "hex":
        return -sum(tet_vol(X[a], X[b], X[c], X[d]) for a, b, c, d in HEX_TETS)
    if typ in ("tri", "quad"):
        n = (0.0, 0.0, 0.0)
        for i in range(len(X)):
            n = tuple(n[k] + cross(X[i], X[(i + 1) % len(X)])[k]
                      for k in range(3))
        a = 0.5 * math.sqrt(dot(n, n))
        return a if dot(n, normal) >= 0 else -a
    return 0.0


def hull2d_area(P):
    pts = sorted(set((round(p[0], 12), round(p[1], 12)) for p in P))
    if len(pts) < 3:
        return 0.0

    def cr(o, a, b):
        return (a[0] - o[0]) * (b[1] - o[1]) - (a[1] - o[1]) * (b[0] - o[0])
    lo, up = [], []
    for p in pts:
        while len(lo) >= 2 and cr(lo[-2], lo[-1], p) <= 0:
            lo.pop()
        lo.append(p)
    for p in reversed(pts):
        while len(up) >= 2 and cr(up[-2], up[-1], p) <= 0:
            up.pop()
        up.append(p)
    h = lo[:-1] + up[:-1]
    return 0.5 * abs(sum(h[i][0] * h[(i + 1) % len(h)][1] -
                         h[(i + 1) % len(h)][0] * h[i][1]
                         for i in range(len(h))))


# ---------------------------------------------------------- pictures
VIEW_ROT = math.radians(-35.0)
VIEW_TILT = math.radians(30.0)


def view_axes():
    """a right-handed camera (user, Oct 16: the old view was a mirror
    image, its axis icon left-handed): E points from the scene to the
    eye (azimuth VIEW_ROT from +x, VIEW_TILT above the horizon), R to
    the right of the screen, U up; R x U = E, so x cross y shows as z"""
    ca, sa = math.cos(VIEW_ROT), math.sin(VIEW_ROT)
    ct, st = math.cos(VIEW_TILT), math.sin(VIEW_TILT)
    E = (ct * ca, ct * sa, st)
    R = (-sa, ca, 0.0)
    U = cross(E, R)
    return R, U, E


def project(p, flat):
    """3D view (z up, seen from the +x, -y side, above), or plain x,y
    for flat sets"""
    if flat:
        return p[0], p[1], 0.0
    R, U, E = view_axes()
    return dot(p, R), dot(p, U), dot(p, E)          # depth: larger = nearer


def triad(flat, H, colour="#333"):
    """a small x, y, z axis icon in the lower left corner (user, Oct 13)"""
    ox, oy, L = 30, H - 26, 20
    out = []
    axes = [("x", (1, 0, 0), "#c0392b"), ("y", (0, 1, 0), "#27ae60")]
    if not flat:
        axes.append(("z", (0, 0, 1), "#2e64c8"))
    for name, v, col in axes:
        px, py, _ = project(v, flat)
        n = math.hypot(px, py) or 1
        ex, ey = ox + L * px / n * min(1, n), oy - L * py / n * min(1, n)
        out.append('<line x1="%d" y1="%d" x2="%.1f" y2="%.1f" stroke="%s" '
                   'stroke-width="2"/>' % (ox, oy, ex, ey, col))
        out.append('<text x="%.1f" y="%.1f" font-size="11" fill="%s" '
                   'text-anchor="middle" dominant-baseline="middle">%s</text>'
                   % (ox + (L + 9) * (ex - ox) / L, oy + (L + 9) * (ey - oy) / L,
                      col, name))
    return out


def svg_frame(P2, W=420, H=340):
    """picture coordinates; margins: 20 at the top (title), 52 at the
    bottom and 24 at the sides (the axis icon sits bottom left)"""
    xs, ys = [p[0] for p in P2] or [0], [p[1] for p in P2] or [0]
    x0, x1, y0, y1 = min(xs), max(xs), min(ys), max(ys)
    mt, mb, ms = 22, 52, 24
    sx = (W - 2 * ms) / ((x1 - x0) or 1)
    sy = (H - mt - mb) / ((y1 - y0) or 1)
    s = min(sx, sy)
    ox = ms + ((W - 2 * ms) - (x1 - x0) * s) / 2
    oy = mb + ((H - mt - mb) - (y1 - y0) * s) / 2

    def T(x, y):
        return ox + (x - x0) * s, H - oy - (y - y0) * s
    return T


def write_pts_svg(fn, V, flat, title, dup_tol):
    P2 = [project(p, flat) for p in V]
    W, H = 420, 340
    T = svg_frame(P2, W, H)
    out = ['<svg xmlns="http://www.w3.org/2000/svg" width="%d" height="%d" '
           'viewBox="0 0 %d %d" font-family="sans-serif">' % (W, H, W, H),
           '<rect width="100%" height="100%" fill="#fff"/>']
    # coincident vertices: a red ring with the count
    groups = {}
    for i, p in enumerate(V):
        k = tuple(round(c / dup_tol) for c in p) if dup_tol > 0 else p
        groups.setdefault(k, []).append(i)
    order = sorted(range(len(V)), key=lambda i: P2[i][2])
    dmin = min((d[2] for d in P2), default=0)
    dmax = max((d[2] for d in P2), default=1)
    for i in order:
        x, y = T(P2[i][0], P2[i][1])
        f = 0.0 if dmax == dmin else (P2[i][2] - dmin) / (dmax - dmin)
        g = int(170 - 130 * f)
        if flat:
            g = 60
        out.append('<circle cx="%.1f" cy="%.1f" r="3" fill="rgb(%d,%d,200)"/>'
                   % (x, y, g, g))
    many = sum(1 for ids in groups.values() if len(ids) > 1)
    for k, ids in groups.items():
        if len(ids) > 1:
            x, y = T(P2[ids[0]][0], P2[ids[0]][1])
            if many > 60:          # too many to label: small plain rings
                out.append('<circle cx="%.1f" cy="%.1f" r="4.5" fill="none" '
                           'stroke="#d62828" stroke-width="1"/>' % (x, y))
                continue
            out.append('<circle cx="%.1f" cy="%.1f" r="7" fill="none" '
                       'stroke="#d62828" stroke-width="1.5"/>' % (x, y))
            out.append('<text x="%.1f" y="%.1f" font-size="10" fill="#d62828" '
                       'paint-order="stroke" stroke="#fff" stroke-width="3">'
                       'x%d</text>' % (x + 8, y - 6, len(ids)))
    if many > 60:
        title += "; red rings: %d places with 2 or more" % many
    out.append('<text x="8" y="14" font-size="12" fill="#222">%s</text>'
               % title.replace("&", "&amp;").replace("<", "&lt;"))
    out += triad(flat, H)
    out.append("</svg>")
    with open(fn, "w") as f:
        f.write("\n".join(out) + "\n")


FACES = {"tet": [(0, 2, 1), (0, 1, 3), (1, 2, 3), (0, 3, 2)],
         "hex": [(0, 3, 2, 1), (4, 5, 6, 7), (0, 1, 5, 4), (1, 2, 6, 5),
                 (2, 3, 7, 6), (3, 0, 4, 7)]}


MAT_RGB = {1: (205, 205, 220), 2: (205, 60, 205), 3: (70, 200, 70),
           4: (70, 140, 230), 5: (230, 150, 50)}


def layer_rgb(k, n):
    """layer k of n: blue to red"""
    u = k / max(1, n - 1)
    return (int(60 + 180 * u), int(110 - 40 * abs(2 * u - 1)), int(230 - 190 * u))


def write_mesh_svg(fn, V, C, flat, title, mats=None, cut=False, layer=None):
    """the mesh's outer faces, shaded; with cut, the quarter nearest
    the viewer is removed first; with mats, faces coloured by material"""
    W, H = 420, 340
    out = ['<svg xmlns="http://www.w3.org/2000/svg" width="%d" height="%d" '
           'viewBox="0 0 %d %d" font-family="sans-serif">' % (W, H, W, H),
           '<rect width="100%" height="100%" fill="#fff"/>']
    polys = []
    if flat or all(t in ("tri", "quad") for t, _ in C):
        for t, ids in C:
            polys.append((0.0, [V[i] for i in ids], 0.85, None))
    else:
        cnt, rep, cen, mof = {}, {}, {}, {}
        ex, ey = view_axes()[2][:2]    # the eye's direction in x, y
        for n_, (t, ids) in enumerate(C):
            cc = tuple(sum(V[i][k] for i in ids) / len(ids) for k in range(3))
            if cut and cc[0] * ex > 0 and cc[1] * ey > 0:
                continue           # the quarter facing the viewer
            m_ = mats[n_] if mats else 1
            if layer:
                m_ = n_ // layer       # the layer of this cell
            for fc in FACES.get(t, []):
                f = [ids[i] for i in fc]
                k = tuple(sorted(f))
                cnt[k] = cnt.get(k, 0) + 1
                rep[k] = f
                cen[k] = cc
                mof[k] = m_
        # toward the viewer, in world coordinates (gradient of depth)
        eye = view_axes()[2]
        light = (0.4, 0.5, 0.75)
        for k, n in cnt.items():
            if n != 1:
                continue
            f = rep[k]
            X = [V[i] for i in f]
            nrm = (0.0, 0.0, 0.0)          # Newell: safe for a face with a
            for i in range(len(X)):        # repeated vertex (axis wedges)
                c = cross(X[i], X[(i + 1) % len(X)])
                nrm = (nrm[0] + c[0], nrm[1] + c[1], nrm[2] + c[2])
            ln = math.sqrt(dot(nrm, nrm)) or 1
            fc = tuple(sum(p[k] for p in X) / len(X) for k in range(3))
            if dot(nrm, sub(fc, cen[k])) < 0:          # make it outward
                nrm = (-nrm[0], -nrm[1], -nrm[2])
            if dot(nrm, eye) <= 0:                     # faces away: hidden
                continue
            sh = 0.55 + 0.4 * abs(dot(nrm, light)) / ln
            depth = sum(project(p, False)[2] for p in X) / len(X)
            polys.append((depth, X, sh, mof[k] if (mats or layer) else None))
    P2all = [project(p, flat) for p in V]
    T = svg_frame(P2all, W, H)
    for depth, X, sh, m_ in sorted(polys, key=lambda q: q[0]):
        pts = " ".join("%.1f,%.1f" % T(*project(p, flat)[:2]) for p in X)
        if m_ is None:
            g = int(255 * sh)
            col = (g, g, min(255, g + 12))
        elif layer:
            b = layer_rgb(m_, (len(C) + layer - 1) // layer)
            col = tuple(min(255, int(c * (0.45 + 0.65 * sh))) for c in b)
        else:
            b = MAT_RGB.get(m_, (180, 180, 180))
            col = tuple(min(255, int(c * (0.35 + 0.75 * sh))) for c in b)
        out.append('<polygon points="%s" fill="rgb(%d,%d,%d)" stroke="#334" '
                   'stroke-width="0.4"/>' % ((pts,) + col))
    out.append('<text x="8" y="14" font-size="12" fill="#222">%s</text>'
               % title.replace("&", "&amp;").replace("<", "&lt;"))
    out += triad(flat, H)
    out.append("</svg>")
    with open(fn, "w") as f:
        f.write("\n".join(out) + "\n")


PANEL_RGB = {"green": (60, 200, 60), "yellowgreen": (170, 215, 40),
             "red": (225, 70, 30), "blue": (40, 80, 220),
             "cyan": (40, 220, 230), "orange": (240, 160, 30)}


def outer_faces(V, C):
    """boundary faces [(vertex ids, centre of its cell)]"""
    cnt, rep, cen = {}, {}, {}
    for t, ids in C:
        cc = tuple(sum(V[i][k] for i in ids) / len(ids) for k in range(3))
        for fc in FACES.get(t, []):
            f = [ids[i] for i in fc]
            k = tuple(sorted(f))
            cnt[k] = cnt.get(k, 0) + 1
            rep[k] = f
            cen[k] = cc
    return [(rep[k], cen[k]) for k, n in cnt.items() if n == 1]


def write_panels_svg(fn, panels, cols=3, PW=300, PH=260):
    """panels: [(V, C, colour name, label)], drawn 'cols' to a row, each
    mesh's outer faces facing the viewer, shaded, in its colour"""
    rows = (len(panels) + cols - 1) // cols
    W, H = cols * PW, rows * PH + 30
    out = ['<svg xmlns="http://www.w3.org/2000/svg" width="%d" height="%d" '
           'viewBox="0 0 %d %d" font-family="sans-serif">' % (W, H, W, H),
           '<rect width="100%" height="100%" fill="#fff"/>']
    eye = view_axes()[2]
    light = (0.4, 0.5, 0.75)
    for n, (V, C, colour, label) in enumerate(panels):
        ox, oy = (n % cols) * PW, 30 + (n // cols) * PH
        polys = []
        for f, cc in outer_faces(V, C):
            X = [V[i] for i in f]
            nrm = (0.0, 0.0, 0.0)
            for i in range(len(X)):
                c = cross(X[i], X[(i + 1) % len(X)])
                nrm = (nrm[0] + c[0], nrm[1] + c[1], nrm[2] + c[2])
            ln = math.sqrt(dot(nrm, nrm)) or 1
            fc = tuple(sum(p[k] for p in X) / len(X) for k in range(3))
            if dot(nrm, sub(fc, cc)) < 0:
                nrm = (-nrm[0], -nrm[1], -nrm[2])
            if dot(nrm, eye) <= 0:
                continue
            sh = 0.45 + 0.55 * abs(dot(nrm, light)) / ln
            depth = sum(project(p, False)[2] for p in X) / len(X)
            polys.append((depth, X, sh))
        P2 = [project(p, False) for p in V] or [(0, 0, 0)]
        xs, ys = [p[0] for p in P2], [p[1] for p in P2]
        x0, x1, y0, y1 = min(xs), max(xs), min(ys), max(ys)
        sc = min((PW - 30) / ((x1 - x0) or 1), (PH - 40) / ((y1 - y0) or 1))
        bx = ox + (PW - (x1 - x0) * sc) / 2
        by = oy + 22 + (PH - 40 - (y1 - y0) * sc) / 2
        base = PANEL_RGB.get(colour, (180, 180, 180))
        for depth, X, sh in sorted(polys, key=lambda q: q[0]):
            pts = " ".join("%.1f,%.1f" % (bx + (project(p, False)[0] - x0) * sc,
                                          by + (y1 - project(p, False)[1]) * sc)
                           for p in X)
            col = tuple(min(255, int(c * sh)) for c in base)
            out.append('<polygon points="%s" fill="rgb(%d,%d,%d)" stroke="#111" '
                       'stroke-width="0.35"/>' % ((pts,) + col))
        out.append('<text x="%d" y="%d" font-size="11" fill="#222">%s</text>'
                   % (ox + 8, oy + 14, label.replace("&", "&amp;")))
    out.append('<text x="8" y="18" font-size="13" fill="#222">the six meshes '
               'of the demo, as on its page (top right: the box octant)</text>')
    out.append("</svg>")
    with open(fn, "w") as f:
        f.write("\n".join(out) + "\n")


# --------------------------------------------------------------- main
def main(argv):
    if len(argv) != 3:
        print(__doc__)
        return 2
    deck, prefix = argv[1], argv[2]
    h = headers(deck)
    hard, notes = [], []
    exp, kind, is_open = expected(h.get("spec", "none"))
    if "open" in h:
        is_open = True
    spec_words = h.get("spec", "none").split()
    is_brick = bool(spec_words) and spec_words[0] == "brick"
    ptsfile = h.get("points")
    V = []
    if ptsfile:
        try:
            V, _, _ = read_avs(ptsfile)
        except (OSError, ValueError, IndexError) as e:
            print("  unreadable          %s: %s" % (ptsfile, e))
            print("HARD: FAIL")
            return 1
    size = bbox_size(exp if exp else V)
    tol = TOL_REL * size
    print("  spec                %s" % h.get("spec", "none"))
    print("  vertices_written    %d" % len(V))
    nexp = None
    if kind in ("vector", "interp"):
        nexp = len(exp) if exp is not None else None
        print("  vertices_expected   %s" % nexp)
        if nexp is not None and len(V) != nexp:
            hard.append("%d vertices written, %d expected" % (len(V), nexp))
    elif kind != "none":
        nexp = 0
        for part in h.get("spec", "none").split(" + "):
            w = part.split()
            if w[0] == "brick":
                w = w[1:]
            if w[0] == "line":
                nexp += max(1, int(w[1]))
            else:
                k = 1
                for v in ints(w[1]):
                    k *= max(1, v)
                nexp += k
        print("  vertices_expected   %d" % nexp)
        if len(V) != nexp:
            hard.append("%d vertices written, %d expected" % (len(V), nexp))
    if exp is not None and V:
        e1, m1 = match(exp, V, tol)
        e2, m2 = match(V, exp, tol)
        print("  max_vertex_error    %.3e" % max(e1, e2))
        print("  unmatched           %d expected positions, %d vertices" % (m1, m2))
        if is_open:
            notes.append("not documented: %s (max distance from the "
                         "documented reading %.3g)" % (h.get("open", ""),
                                                       max(e1, e2)))
        elif m1 or m2:
            hard.append("%d expected positions with no vertex, %d vertices at no expected position "
                        "(max error %.3g)" % (m1, m2, max(e1, e2)))
        same = len(V) == len(exp) and all(math.dist(a, b) <= tol
                                          for a, b in zip(V, exp))
        order = {"vector": "V1 fastest, then V2, then V3",
                 "interp": "pair by pair, A to B",
                 "xyz": "x fastest, then y, then z",
                 "rtz": "theta fastest, then r, then z",
                 "rtp": "phi fastest, then theta, then r",
                 "line": "start to end"}.get(kind, "")
        print("  vertex_order        %s" % (order if same else
                                            "NOT " + order))
        nd = distinct(exp, tol)
        print("  coincident_positions %d (distinct %d)" % (len(exp) - nd, nd))
    elif is_open and V:
        notes.append("not documented: %s" % h.get("open", ""))
    if V:
        xs = sorted(set(round(p[0], 10) for p in V))
        print("  x_values            %s" % " ".join("%.6g" % x for x in xs[:12])
              + (" ..." if len(xs) > 12 else ""))

    # ---- the connected mesh
    MV, MC, attrs = [], [], {}
    mesh = h.get("mesh", "").split()
    if mesh:
        try:
            MV, MC, attrs = read_avs(mesh[0])
        except (OSError, ValueError, IndexError) as e:
            hard.append("no mesh %s (%s)" % (mesh[0], e))
    if mesh and MV:
        want = mesh[1] if len(mesh) > 1 else None
        print("  mesh_vertices       %d" % len(MV))
        print("  mesh_cells          %d" % len(MC))
        types = sorted(set(t for t, _ in MC))
        print("  mesh_cell_types     %s" % (",".join(types) or "none"))
        if want and any(t != want for t, _ in MC):
            hard.append("cells of type %s, expected %s" % (",".join(types), want))
        if not MC:
            hard.append("no cells in the mesh")
        if h.get("filter", "").startswith("y") and exp is not None:
            nd = distinct(exp, tol)
            if len(MV) != nd:
                hard.append("mesh has %d vertices, %d distinct positions expected"
                            % (len(MV), nd))
        if is_brick and kind != "none":
            w = spec_words[1:]
            n = [max(1, v) for v in ints(w[1])]
            big = [v for v in n if v > 1]
            ncell = 1
            for v in big:
                ncell *= v - 1
            print("  brick_cells_expected %d" % ncell)
            if len(MC) != ncell:
                hard.append("%d cells, a brick of %s has %d" %
                            (len(MC), "x".join(map(str, n)), ncell))
        msize = bbox_size(MV)
        dim = 2 if all(t in ("tri", "quad") for t, _ in MC) else 3
        # a flat mesh: the plane it lies in (its normal axis)
        ext = [max(p[i] for p in MV) - min(p[i] for p in MV) for i in range(3)]
        flat_axis = 2
        if dim == 2:
            flat_axis = min(range(3), key=lambda i: (ext[i], -i))
        normal = tuple(1.0 if i == flat_axis else 0.0 for i in range(3))
        meas = [cell_measure(t, [MV[i] for i in ids], normal) for t, ids in MC]
        eps = 1e-12 * msize ** dim
        if dim == 2 and flat_axis != 2:
            # xz or yz plane: no documented turn; all cells must agree
            plane = "yz" if flat_axis == 0 else "xz"
            bad = sum(1 for v in meas if abs(v) <= eps)
            npos = sum(1 for v in meas if v > eps)
            nneg = sum(1 for v in meas if v < -eps)
            print("  plane               %s (cells turning +%d, -%d about the"
                  " %s axis)" % (plane, npos, nneg, "xyz"[flat_axis]))
            if npos and nneg:
                hard.append("cells turn both ways in the %s plane: %d one way, "
                            "%d the other" % (plane, npos, nneg))
        else:
            bad = sum(1 for v in meas if v <= eps)
        tot = math.fsum(abs(v) for v in meas)
        print("  nonpositive_cells   %d" % bad)
        print("  mesh_volume         %.12g" % tot)
        if bad:
            hard.append("%d cells with volume (area) <= 0" % bad)
        degen = sum(1 for _, ids in MC if len(set(ids)) < len(ids))
        print("  degenerate_cells    %d (cells listing a vertex twice: e.g."
              " after filter, the hexes on an axis, whose axis face has"
              " area 0)" % degen)
        if kind == "xyz" and dim == 3 and all(t == "tet" for t, _ in MC):
            lo = [min(p[i] for p in MV) for i in range(3)]
            hi = [max(p[i] for p in MV) for i in range(3)]
            ftol = 1e-9 * msize
            once = {}
            for _, ids in MC:
                for fc in FACES["tet"]:
                    k = tuple(sorted(ids[i] for i in fc))
                    once[k] = once.get(k, 0) + 1

            def on_side(k):
                return any(all(abs(MV[j][a] - b) <= ftol for j in k)
                           for a in range(3) for b in (lo[a], hi[a]))
            inner = sum(1 for k, n in once.items() if n == 1 and not on_side(k))
            over = sum(1 for n in once.values() if n > 2)
            print("  inner_faces         %d (faces of one cell inside the box);"
                  " %d faces of 3+ cells" % (inner, over))
            if inner or over:
                hard.append("the cells do not match: %d faces inside the box "
                            "belong to one cell, %d to three or more"
                            % (inner, over))
        used = set(i for _, ids in MC for i in ids)
        unused = len(MV) - len(used)
        if h.get("parents_ok") and "itp1" in attrs:
            npar = sum(1 for i in range(len(MV))
                       if i not in used and attrs["itp1"][i] == 41)
            print("  parent_vertices     %d (itp1 = 41, unused by cells:"
                  " settets interface parents)" % npar)
            unused -= npar
        print("  unused_vertices     %d" % unused)
        coinc = len(MV) - distinct(MV, TOL_REL * msize)
        print("  coincident_mesh_vertices %d" % coinc)
        if unused and not h.get("unused_ok"):
            hard.append("%d mesh vertices used by no cell" % unused)
        if coinc and not h.get("coincident_ok"):
            notes.append("%d mesh vertices sit on another vertex" % coinc)
        ref = exp if exp else MV
        if kind in ("xyz", "rtz") and dim == 3:
            zs = [p[2] for p in ref]
            hv = hull2d_area(ref) * (max(zs) - min(zs))
        elif dim == 2:
            keep = [i for i in range(3) if i != flat_axis]
            hv = hull2d_area([(p[keep[0]], p[keep[1]], 0.0) for p in ref])
        else:
            hv = None
        if hv is not None:
            print("  hull_volume         %.12g" % hv)
            if abs(tot - hv) > 1e-9 * max(hv, 1e-300):
                hard.append("cells fill %.12g, the convex hull is %.12g"
                            % (tot, hv))
    if "vertices" in h and mesh:
        want_n = int(h["vertices"])
        if len(MV) != want_n:
            hard.append("mesh has %d vertices, expected %d" % (len(MV), want_n))
    if "cells" in h and mesh:
        want_c = int(h["cells"])
        if len(MC) != want_c:
            hard.append("mesh has %d cells, expected %d" % (len(MC), want_c))
    if "cellattr" in h and MV and MC:
        which = h["cellattr"].split()[0]
        names = {"median": ("xmed", "ymed", "zmed"),
                 "voronoi": ("xvor", "yvor", "zvor")}[which]
        ca = attrs.get("__cell", {})
        if not all(n in ca for n in names):
            hard.append("no cell attributes %s in the mesh file" % ",".join(names))
        else:
            msz = bbox_size(MV)
            worst, nbad = 0.0, 0
            for c, (t, ids) in enumerate(MC):
                q = (ca[names[0]][c], ca[names[1]][c], ca[names[2]][c])
                X = [MV[i] for i in ids]
                if which == "median":
                    e = math.dist(q, tuple(sum(p[k] for p in X) / len(X)
                                           for k in range(3)))
                else:
                    d = [math.dist(q, p) for p in X]
                    e = max(d) - min(d)       # equidistant from the vertices
                    if t == "tri":            # and in the triangle's plane
                        nrm = cross(sub(X[1], X[0]), sub(X[2], X[0]))
                        ln = math.sqrt(dot(nrm, nrm)) or 1
                        e = max(e, abs(dot(sub(q, X[0]), nrm)) / ln)
                worst = max(worst, e)
                if e > 1e-9 * msz:
                    nbad += 1
            print("  cellattr_%-9s  %d cells, largest error %.3e"
                  % (which, len(MC), worst))
            if nbad:
                hard.append("%d cells whose %s point is wrong (largest error %.3g)"
                            % (nbad, which, worst))
    LV = V if V else MV            # the points (else the mesh vertices)
    if "lattice" in h and LV:
        hl = float(h["lattice"])
        off = max(abs(c / hl - round(c / hl)) for p in LV for c in p)
        print("  lattice             %g: largest offset %.3e steps" % (hl, off))
        if off > 1e-9:
            hard.append("vertices off the lattice of step %g" % hl)
        ncoinc = len(LV) - distinct(LV, 1e-9 * bbox_size(LV))
        print("  lattice_coincident  %d" % ncoinc)
        if ncoinc:
            hard.append("%d vertices at the same place as another" % ncoinc)
    if "same_as" in h and V:
        try:
            V2, _, _ = read_avs(h["same_as"])
        except (OSError, ValueError, IndexError) as e:
            V2 = None
            hard.append("same_as: %s unreadable (%s)" % (h["same_as"], e))
        if V2 is not None:
            same = len(V2) == len(V) and all(a == b for a, b in zip(V, V2))
            print("  same_as             %s: %s" % (h["same_as"], "identical" if same else "DIFFERENT"))
            if not same:
                hard.append("vertices differ from %s (same seeds)" % h["same_as"])
    if "same_set" in h and V:
        try:
            V2, _, _ = read_avs(h["same_set"])
        except (OSError, ValueError, IndexError) as e:
            V2 = None
            hard.append("same_set: %s unreadable (%s)" % (h["same_set"], e))
        if V2 is not None:
            stol = 1e-9 * bbox_size(V2)
            e1, m1 = match(V2, V, stol)
            e2, m2 = match(V, V2, stol)
            dup = len(V) - distinct(V, stol)
            print("  same_set            %s: %d of %d missing, %d extra, %d repeated"
                  % (h["same_set"], m1, len(V2), m2, dup))
            if m1 or m2 or dup or len(V) != len(V2):
                hard.append("vertices are not the set in %s (%d missing, %d extra, %d repeated)"
                            % (h["same_set"], m1, m2, dup))
    if "random" in h and V:
        rw = h["random"].split()
        geom, sp = rw[0], float(rw[1])
        lo_, hi_, ed = floats(rw[2]), floats(rw[3]), float(rw[4])
        tl = 1e-9 * max(1.0, max(abs(c) for c in lo_ + hi_))
        outside = 0
        if geom == "xyz":
            faces_hit = {}
            ncorner = 0
            interior_min = None
            edges_hit = set()
            for p in V:
                on = [(i, b) for i in range(3) for b in (0, 1)
                      if abs(p[i] - (lo_[i], hi_[i])[b]) <= tl]
                if any(p[i] < lo_[i] - tl or p[i] > hi_[i] + tl for i in range(3)):
                    outside += 1
                for f in on:
                    faces_hit[f] = faces_hit.get(f, 0) + 1
                if len(on) == 3:
                    ncorner += 1
                if len(on) == 2:
                    edges_hit.add(tuple(sorted(on)))
                if not on:
                    dmin = min(min(p[i] - lo_[i], hi_[i] - p[i]) for i in range(3))
                    interior_min = dmin if interior_min is None else min(interior_min, dmin)
            print("  random_box          %d corners, %d of 12 edges, %d of 6 faces;"
                  " interior points at least %.4g from the boundary (edgedist %g)"
                  % (ncorner, len(edges_hit), len(faces_hit),
                     interior_min if interior_min is not None else -1, ed))
            if ncorner != 8:
                hard.append("%d corner vertices, 8 expected" % ncorner)
            if len(edges_hit) != 12 or len(faces_hit) != 6:
                hard.append("vertices on %d of 12 edges, %d of 6 faces"
                            % (len(edges_hit), len(faces_hit)))
            if interior_min is not None and interior_min < ed * (1 - 1e-9):
                hard.append("an interior vertex %.4g from the boundary, edgedist %g"
                            % (interior_min, ed))
        elif geom == "rtz":
            c0 = floats(rw[5]) if len(rw) > 5 else [0.0, 0.0, 0.0]
            interior_min = None
            for p in V:
                r = math.hypot(p[0] - c0[0], p[1] - c0[1])
                z = p[2] - c0[2]
                if r > hi_[0] + tl or z < lo_[2] - tl or z > hi_[2] + tl:
                    outside += 1
                    continue
                d = min(hi_[0] - r, z - lo_[2], hi_[2] - z)
                if d > tl:
                    interior_min = d if interior_min is None else min(interior_min, d)
            print("  random_cylinder     interior points at least %.4g from the"
                  " surface (edgedist %g)" % (interior_min or -1, ed))
            if interior_min is not None and interior_min < ed * (1 - 1e-9):
                hard.append("an interior vertex %.4g from the surface, edgedist %g"
                            % (interior_min, ed))
        elif geom == "rtp":
            rs = [math.dist(p, (0, 0, 0)) for p in V]
            outside = sum(1 for r in rs if r < lo_[0] - tl or r > hi_[0] + tl)
            print("  random_sphere       r %.12g to %.12g (expected %g to %g)"
                  % (min(rs), max(rs), lo_[0], hi_[0]))
        print("  random_outside      %d" % outside)
        if outside:
            hard.append("%d random vertices outside the region" % outside)
        if len(V) > 1:
            g = Grid(V, sp)
            nn = []
            for p in V[:min(len(V), 400)]:
                c = [math.dist(p, V[j]) for j in g.near(p, 2 * sp) if V[j] != p]
                if c:
                    nn.append(min(c))
            if nn:
                print("  random_spacing      nearest neighbour mean %.4g, min %.4g"
                      " (target %g; first %d points)" % (sum(nn) / len(nn), min(nn), sp, len(nn)))
    if "volume" in h and MV and MC:
        want_v = float(h["volume"])
        print("  volume_expected     %.12g" % want_v)
        if abs(tot - want_v) > 1e-9 * abs(want_v):
            hard.append("cells fill %.12g, expected %.12g" % (tot, want_v))
    if "shells" in h and MV and MC:
        rs_s, ms_s = h["shells"].split()
        R_ = floats(rs_s)
        M_ = ints(ms_s)
        rtol = 1e-9 * R_[-1]
        mats = attrs.get("__itetclr", [])

        def rad(i):
            return math.sqrt(MV[i][0] ** 2 + MV[i][1] ** 2 + MV[i][2] ** 2)
        used = set(i for _, ids in MC for i in ids)
        rin = min(rad(i) for i in used)
        rout = max(rad(i) for i in used)
        print("  shell_radii         %.12g to %.12g (expected %g to %g)"
              % (rin, rout, R_[0], R_[-1]))
        if rin < R_[0] - rtol or rout > R_[-1] + rtol:
            hard.append("vertices outside the shell %g to %g (%.6g to %.6g)"
                        % (R_[0], R_[-1], rin, rout))
        cnt = {}
        wrong = 0
        for (t, ids), m in zip(MC, mats):
            cnt[m] = cnt.get(m, 0) + 1
            if m not in M_:
                wrong += 1
                continue
            k = M_.index(m)
            lo, hi = R_[k], R_[k + 1]
            if any(rad(i) < lo - rtol or rad(i) > hi + rtol for i in ids):
                wrong += 1
        print("  shell_materials     %s" % ", ".join(
            "%d: %d cells" % (m, n) for m, n in sorted(cnt.items())))
        if wrong:
            hard.append("%d cells not inside the shell of their material"
                        % wrong)
        exact = 4.0 / 3.0 * math.pi * (R_[-1] ** 3 - R_[0] ** 3)
        print("  shell_volume        %.6g of the exact %.6g (%.2f %%;"
              " the faceted surfaces cut a little off)"
              % (tot, exact, 100.0 * tot / exact))
    if "attr" in h:
        name, a, b, val = h["attr"].split()
        a, b, val = int(a), int(b), float(val)
        got = attrs.get(name)
        if got is None:
            hard.append("no attribute %s in the mesh file" % name)
        else:
            wrong = [i for i in range(a - 1, b) if got[i] != val]
            print("  attr_%s_%d_%d      %d of %d changed" %
                  (name, a, b, len(wrong), b - a + 1))
            if wrong:
                hard.append("%s of vertices %d-%d should still be %g; %d were "
                            "changed (vertex %d is %g)" %
                            (name, a, b, val, len(wrong), wrong[0] + 1,
                             got[wrong[0]]))
    panels = []
    if h.get("picture", "").startswith("panels"):
        for spec in h["picture"][len("panels"):].split(","):
            w = spec.split()
            if len(w) < 3:
                continue
            fnp, want, colour = w[0], w[1], w[2]
            inside = len(w) > 3 and w[3] == "inside"
            try:
                PV, PC, PA = read_avs(fnp)
            except (OSError, ValueError, IndexError) as e:
                hard.append("panel %s unreadable (%s)" % (fnp, e))
                continue
            types = sorted(set(t for t, _ in PC))
            meas = [cell_measure(t, [PV[i] for i in ids]) for t, ids in PC]
            veps = 1e-12 * bbox_size(PV) ** 3      # once, not per cell
            bad = sum(1 for v in meas if v <= veps)
            used = set(i for _, ids in PC for i in ids)
            par = sum(1 for i in range(len(PV)) if i not in used and
                      PA.get("itp1") and PA["itp1"][i] == 41)
            unused = len(PV) - len(used) - par
            vol = math.fsum(abs(v) for v in meas)
            print("  panel %-28s %5d vertices %6d %s cells, volume %.6g;"
                  " %d <= 0, %d unused (+%d parents)"
                  % (fnp, len(PV), len(PC), ",".join(types) or "no", vol,
                     bad, unused, par))
            if not PC:
                hard.append("%s: no cells" % fnp)
            if any(t != want for t in types):
                hard.append("%s: cells of type %s, expected %s"
                            % (fnp, ",".join(types), want))
            if bad:
                hard.append("%s: %d cells with volume <= 0" % (fnp, bad))
            if unused:
                hard.append("%s: %d vertices used by no cell" % (fnp, unused))
            if inside:     # turned 180 deg about y: the cut faces of an
                           # octant (normals -x,-y,-z) face the viewer,
                           # who is on the +x, -y side, above
                PV = [(-p[0], p[1], -p[2]) for p in PV]
            panels.append((PV, PC, colour, "%s: %d %s cells"
                           % (fnp.replace("out_cp13_", "").replace(".inp", "")
                              .replace("mesh", "rtp (this deck's mesh)"),
                              len(PC), want)))
    for n in notes:
        print("  NOTE: " + n)
    for x in hard:
        print("  -> HARD: " + x)
    print("HARD: " + ("FAIL" if hard else "pass"))

    # ---- pictures
    flat_pts = bool(V) and (max(p[2] for p in V) - min(p[2] for p in V)
                            <= 1e-12 * bbox_size(V))
    if panels:
        # several meshes: the panel picture only (user, Oct 15)
        try:
            import os
            os.remove(prefix + "_pts.svg")
        except OSError:
            pass
    elif V:
        write_pts_svg(prefix + "_pts.svg", V, flat_pts,
                      "vertices: %d" % len(V), tol * 10)
    if panels:
        write_panels_svg(prefix + "_mesh.svg", panels)
    elif MV and MC and h.get("picture", "").startswith("layers"):
        global VIEW_TILT
        pw = h["picture"].split()
        nl = int(pw[1])
        if "tilt" in pw:
            VIEW_TILT = math.radians(float(pw[pw.index("tilt") + 1]))
        write_mesh_svg(prefix + "_mesh.svg", MV, MC, False,
                       "mesh: %d vertices, %d %s cells in %d layers"
                       " (blue: first, red: last)"
                       % (len(MV), len(MC), "/".join(sorted(set(t for t, _ in MC))),
                          (len(MC) + nl - 1) // nl), layer=nl)
    elif MV and MC:
        flat_m = all(t in ("tri", "quad") for t, _ in MC) and \
            max(p[2] for p in MV) - min(p[2] for p in MV) <= 1e-12 * bbox_size(MV)
        cut = h.get("picture", "").startswith("cutaway")
        mats = attrs.get("__itetclr") if cut else None
        write_mesh_svg(prefix + "_mesh.svg", MV, MC, flat_m,
                       "mesh: %d vertices, %d %s cells%s" %
                       (len(MV), len(MC), "/".join(sorted(set(t for t, _ in MC))),
                        " (cutaway, by material)" if cut else ""),
                       mats, cut)
    return 1 if hard else 0


if __name__ == "__main__":
    sys.exit(main(sys.argv))
