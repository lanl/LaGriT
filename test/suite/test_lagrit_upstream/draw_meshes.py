#!/usr/bin/env python3
"""draw_meshes.py: one SVG picture of an AVS mesh file written by LaGriT.

Used by check_upstream.py (test_lagrit_upstream). Python 3, standard
library only. Right-handed views (x cross y shows as z; user, Oct 16
2026), axis icon bottom left, legend bottom right.

What is drawn
  3D cells (tet, hex, pri, pyr): the outer faces seen from the camera,
      shaded
  2D cells (tri, quad): every cell; lines: the segments; vertices only:
      dots
  Very large files are not drawn (MAX_CELLS, MAX_VERTICES).

The view
  A mesh that lies in a plane (any plane: all vertices within 1e-6 of
  the mesh size of it) is seen straight down the plane's normal (user,
  Oct 17: recon1's fracture mesh, refine_octree_prd); otherwise the 3D
  view of test_createpts (azimuth -60 deg, 25 deg above the horizon).

Colour (the "color" argument; legend bottom right)
  "mat"          the cell material (itetclr, the AVS material column)
  "cell:NAME"    a cell attribute (itetclr = the material column)
  An attribute missing from the file: a neutral mesh and a note, never
  the material instead.
  "node:NAME"    a vertex attribute (imt = imt1): the cells are drawn in
                 a neutral grey and every vertex on a visible face is a
                 dot in its own colour (user, Oct 17: colour a vertex
                 attribute at the vertices, never the cells)
  "attr:NAME"    NAME as a vertex attribute if the file has one, else
                 as a cell attribute
  "aspect"       aspect ratio of each cell, as LaGriT's quality command:
                 tri 2 r / R, tet 3 r / R (r inscribed, R circumscribed
                 radius; 1 = equilateral, 0 = flat)
  An integer attribute (or the material) with at most 12 values: one
  colour per value (the same value always the same colour). A real
  attribute, or more values: a blue-to-yellow scale; vrange, or a range
  in the spec ("aspect[0,1]"), fixes its ends (e.g. one scale for two
  pictures).

view="top": seen straight down the z axis (a source drawn beside a
horizontal sink, so the two patterns line up); box=(xmin, ymin, xmax,
ymax) frames the picture to cover that box too (one scale for both);
mark=(xmin, ymin, xmax, ymax) draws a red dashed box there (where a
small sink lies on its source).

Octree meshes (refine / amr keep every level): only the leaves are
drawn; see octree_parents().

draw(avs_file, svg_file, title, color="mat", vrange=None, view=None)
-> one line
describing the mesh; raises ValueError if it cannot read the file.
attr_range(avs_file, color) -> (min, max) of the values that colour it.
"""
import math

VIEW_ROT = math.radians(-60.0)      # azimuth of the eye from +x
VIEW_TILT = math.radians(25.0)      # above the horizon
MAX_CELLS = 250000
MAX_VERTICES = 400000
W, H = 300, 250

FACES = {"tet": [(0, 2, 1), (0, 1, 3), (1, 2, 3), (0, 3, 2)],
         "hex": [(0, 3, 2, 1), (4, 5, 6, 7), (0, 1, 5, 4), (1, 2, 6, 5),
                 (2, 3, 7, 6), (3, 0, 4, 7)],
         "pri": [(0, 2, 1), (3, 4, 5), (0, 1, 4, 3), (1, 2, 5, 4),
                 (2, 0, 3, 5)],
         "pyr": [(0, 3, 2, 1), (0, 1, 4), (1, 2, 4), (2, 3, 4), (3, 0, 4)]}
SURF = ("tri", "quad")
# AVS names LaGriT writes, and other spellings, for the same cells
ALIAS = {"prism": "pri", "pyramid": "pyr", "tetra": "tet", "hexa": "hex",
         "triangle": "tri", "pt": "pnt", "point": "pnt"}
CAT_RGB = [(200, 200, 215), (215, 80, 200), (80, 195, 80), (80, 140, 230),
           (235, 155, 55), (60, 200, 200), (225, 70, 70), (160, 120, 220),
           (150, 110, 60), (240, 210, 60), (120, 120, 120), (40, 90, 60)]
SCALE = [(0.0, (48, 18, 120)), (0.25, (40, 100, 190)), (0.5, (35, 170, 150)),
         (0.75, (140, 210, 70)), (1.0, (250, 230, 40))]
ATTR_ALIAS = {"imt": "imt1", "itp": "itp1", "icr": "icr1", "isn": "isn1"}


def sub(a, b):
    return (a[0] - b[0], a[1] - b[1], a[2] - b[2])


def cross(a, b):
    return (a[1] * b[2] - a[2] * b[1], a[2] * b[0] - a[0] * b[2],
            a[0] * b[1] - a[1] * b[0])


def dot(a, b):
    return a[0] * b[0] + a[1] * b[1] + a[2] * b[2]


def unit(a):
    n = math.sqrt(dot(a, a)) or 1.0
    return (a[0] / n, a[1] / n, a[2] / n)


# ------------------------------------------------------------ reading
def read_avs(fn):
    """dict: V [(x,y,z)], C [(type, [ids 0-based], material)], node
    {name: [values]}, cell {name: [values]}; or {"too_large": (nn, nc)}"""
    with open(fn, errors="replace") as f:
        head = f.readline().split()
        while head and head[0].startswith("#"):
            head = f.readline().split()
        if len(head) < 2:
            raise ValueError("not an AVS file")
        nn, nc = int(head[0]), int(head[1])
        if nn > MAX_VERTICES or nc > MAX_CELLS:
            return {"too_large": (nn, nc)}
        V = []
        for _ in range(nn):
            w = f.readline().split()
            V.append((float(w[1]), float(w[2]), float(w[3])))
        C = []
        for _ in range(nc):
            w = f.readline().split()
            t = w[2].lower()
            C.append((ALIAS.get(t, t), [int(v) - 1 for v in w[3:]],
                      int(float(w[1]))))
        node, cell, types = {}, {}, {}
        nnd = int(head[2]) if len(head) > 2 else 0
        ncd = int(head[3]) if len(head) > 3 else 0
        for count, n, store in ((nnd, nn, node), (ncd, nc, cell)):
            if count <= 0:
                continue
            try:
                w = f.readline().split()
                na, sizes = int(w[0]), [int(x) for x in w[1:]]
                names = []
                for _ in range(na):
                    nm, _, typ = f.readline().partition(",")
                    names.append(nm.strip().lower())
                    types[nm.strip().lower()] = typ.strip().lower()
                cols = {nm: [] for nm in names}
                for _ in range(n):
                    w = f.readline().split()[1:]
                    k = 0
                    for nm, sz in zip(names, sizes or [1] * na):
                        cols[nm].append(float(w[k].replace("D", "E")))
                        k += sz
                store.update(cols)
            except (ValueError, IndexError):
                break
    return {"V": V, "C": C, "node": node, "cell": cell, "types": types}


def octree_parents(V, C, kids):
    """indices of the cells that are parents in an octree (refine /
    amr keeps every level: a refined hex stays in the mesh with its 8
    children). Only the leaves are drawn (user, Oct 16 2026).
    With the cell attribute itetkid (a parent's first child; 0 for a
    leaf) it is exact. Without it: a hex is a parent when a hex sharing
    one of its vertices is less than half its size and has its centre
    inside the parent's box (a child shares one corner with its parent;
    a neighbour's centre is outside). Both agree on refine_octree."""
    if kids is not None and len(kids) == len(C):
        return {n for n, k in enumerate(kids) if k > 0}
    box, by_vertex = {}, {}
    for n, (t, ids, _) in enumerate(C):
        if t != "hex" or len(ids) < 8:
            continue
        P = [V[i] for i in ids[:8]]
        lo = tuple(min(p[k] for p in P) for k in range(3))
        hi = tuple(max(p[k] for p in P) for k in range(3))
        size = (hi[0] - lo[0]) * (hi[1] - lo[1]) * (hi[2] - lo[2])
        cen = tuple((lo[k] + hi[k]) / 2 for k in range(3))
        box[n] = (lo, hi, size, cen)
        for i in ids[:8]:
            by_vertex.setdefault(i, []).append(n)
    parents = set()
    for n, (lo, hi, size, _) in box.items():
        if size <= 0:
            continue
        eps = [1e-9 * (hi[k] - lo[k]) for k in range(3)]
        for i in C[n][1][:8]:
            for m in by_vertex.get(i, ()):
                if m == n:
                    continue
                _, _, s2, c2 = box[m]
                if s2 < 0.5 * size and all(lo[k] + eps[k] < c2[k] < hi[k] - eps[k]
                                           for k in range(3)):
                    parents.add(n)
                    break
            if n in parents:
                break
    return parents


# ------------------------------------------------------------ the view
def default_camera():
    """E from the scene to the eye, R to the right, U up; R x U = E"""
    ca, sa = math.cos(VIEW_ROT), math.sin(VIEW_ROT)
    ct, st = math.cos(VIEW_TILT), math.sin(VIEW_TILT)
    E = (ct * ca, ct * sa, st)
    R = (-sa, ca, 0.0)
    return R, cross(E, R), E


def plane_normal(V):
    """the unit normal of the plane holding every vertex, or None"""
    if len(V) < 3:
        return None
    c = tuple(sum(p[k] for p in V) / len(V) for k in range(3))
    span = max(max(p[k] for p in V) - min(p[k] for p in V) for k in range(3))
    if span <= 0:
        return None
    a = max(V, key=lambda p: dot(sub(p, c), sub(p, c)))
    ab = sub(a, c)
    b = max(V, key=lambda p: dot(cross(ab, sub(p, c)), cross(ab, sub(p, c))))
    n = cross(ab, sub(b, c))
    if dot(n, n) <= (1e-12 * span ** 2) ** 2:
        return None                         # all on a line
    n = unit(n)
    tol = 1e-6 * span
    if any(abs(dot(sub(p, c), n)) > tol for p in V):
        return None
    return n


def camera_for(V):
    """(R, U, E, planar). A planar mesh is seen down its normal: the
    normal nearest +z is chosen for the eye if the plane is nearly
    horizontal (then x right, y up, as before); otherwise the side the
    default 3D view looks from, with "up" the plane's steepest
    direction, so a vertical plane stands upright."""
    n = plane_normal(V)
    if n is None:
        R, U, E = default_camera()
        return R, U, E, False
    if abs(n[2]) > 0.999:
        E = (0.0, 0.0, 1.0) if n[2] > 0 else (0.0, 0.0, 1.0)
        return (1.0, 0.0, 0.0), (0.0, 1.0, 0.0), E, True
    E = n if dot(n, default_camera()[2]) >= 0 else (-n[0], -n[1], -n[2])
    up = sub((0.0, 0.0, 1.0), tuple(E[k] * E[2] for k in range(3)))
    U = unit(up)
    R = cross(U, E)                         # then R x U = E
    return R, U, E, True


def project(p, cam):
    R, U, E = cam[:3]
    return dot(p, R), dot(p, U), dot(p, E)


def triad(cam):
    ox, oy, L = 26, H - 22, 17
    out = []
    for name, v, col in (("x", (1, 0, 0), "#c0392b"), ("y", (0, 1, 0), "#27ae60"),
                         ("z", (0, 0, 1), "#2e64c8")):
        px, py, pz = project(v, cam)
        n = math.hypot(px, py)
        if n < 0.08:                        # pointing at the viewer
            sym = "&#8857;" if pz > 0 else "&#8855;"
            out.append('<text x="%d" y="%d" font-size="11" fill="%s" '
                       'text-anchor="middle" dominant-baseline="middle">%s%s'
                       '</text>' % (ox, oy, col, sym, name))
            continue
        ex, ey = ox + L * px, oy - L * py
        out.append('<line x1="%d" y1="%d" x2="%.1f" y2="%.1f" stroke="%s" '
                   'stroke-width="2"/>' % (ox, oy, ex, ey, col))
        out.append('<text x="%.1f" y="%.1f" font-size="10" fill="%s" '
                   'text-anchor="middle" dominant-baseline="middle">%s</text>'
                   % (ox + (L + 8) * px / n * max(n, 0.5),
                      oy - (L + 8) * py / n * max(n, 0.5), col, name))
    return out


def frame(P2, nlines=2):
    """picture coordinates; the top margin leaves room for the title
    and nlines lines of description"""
    xs, ys = [p[0] for p in P2] or [0], [p[1] for p in P2] or [0]
    x0, x1, y0, y1 = min(xs), max(xs), min(ys), max(ys)
    mt, mb, ms = 22 + 12 * nlines, 40, 16
    s = min((W - 2 * ms) / ((x1 - x0) or 1), (H - mt - mb) / ((y1 - y0) or 1))
    ox = ms + ((W - 2 * ms) - (x1 - x0) * s) / 2
    oy = mb + ((H - mt - mb) - (y1 - y0) * s) / 2
    return lambda x, y: (ox + (x - x0) * s, H - oy - (y - y0) * s)


# ------------------------------------------------------------ colour
def scale_rgb(u):
    u = min(1.0, max(0.0, u))
    for (a, ca), (b, cb) in zip(SCALE, SCALE[1:]):
        if u <= b:
            t = (u - a) / ((b - a) or 1)
            return tuple(int(ca[k] + t * (cb[k] - ca[k])) for k in range(3))
    return SCALE[-1][1]


def cat_rgb(v):
    return CAT_RGB[(int(round(v)) - 1) % len(CAT_RGB)] if v >= 1 else (150, 150, 150)


def aspect(t, X):
    """LaGriT's aspect ratio: tri 2 r / R, tet 3 r / R; None otherwise"""
    if t == "tri" and len(X) >= 3:
        a, b, c = (math.dist(X[1], X[2]), math.dist(X[0], X[2]),
                   math.dist(X[0], X[1]))
        s = (a + b + c) / 2
        A = math.sqrt(max(s * (s - a) * (s - b) * (s - c), 0.0))
        if A <= 0:
            return 0.0
        return 2 * (A / s) / (a * b * c / (4 * A))
    if t == "tet" and len(X) >= 4:
        p0, p1, p2, p3 = X[:4]
        v = abs(dot(sub(p1, p0), cross(sub(p2, p0), sub(p3, p0)))) / 6
        if v <= 0:
            return 0.0
        area = sum(math.sqrt(dot(cross(sub(q, p), sub(r, p)),
                                 cross(sub(q, p), sub(r, p)))) / 2
                   for p, q, r in ((p0, p1, p2), (p0, p1, p3), (p0, p2, p3),
                                   (p1, p2, p3)))
        rin = 3 * v / area
        a, b, c = sub(p1, p0), sub(p2, p0), sub(p3, p0)
        num = tuple(dot(a, a) * x + dot(b, b) * y + dot(c, c) * z
                    for x, y, z in zip(cross(b, c), cross(c, a), cross(a, b)))
        R = math.sqrt(dot(num, num)) / (12 * v)
        return 3 * rin / R
    return None


def cell_values(m, color):
    """(per-cell values or None, per-vertex values or None, label,
    message) for a colour spec; message "real" means a real-valued
    attribute (always a colour scale), any other text a problem"""
    V, C = m["V"], m["C"]
    if color in (None, "", "mat"):
        return [c[2] for c in C], None, "material (itetclr)", ""
    if color == "aspect":
        vals = [aspect(t, [V[i] for i in ids]) for t, ids, _ in C]
        return vals, None, "aspect ratio", "real"
    kind, _, name = color.partition(":")
    name = ATTR_ALIAS.get(name.lower(), name.lower())
    if kind == "attr":                      # a vertex attribute if any
        kind = "node" if name in m["node"] else "cell"
    if kind == "cell":
        if name == "itetclr" and name not in m["cell"]:
            return [c[2] for c in C], None, "itetclr", ""
        if name in m["cell"]:
            return (list(m["cell"][name]), None, name,
                    "real" if "integer" not in m["types"].get(name, "") else "")
        return None, None, name, "cell attribute %s not in the file" % name
    if kind == "node":
        if name in m["node"]:
            nv = m["node"][name]
            whole = all(float(x).is_integer() for x in nv)
            vals = []
            for _, ids, _ in C:
                xs = [nv[i] for i in ids if i < len(nv)]
                if not xs:
                    vals.append(None)
                elif whole:
                    vals.append(max(set(xs), key=xs.count))
                else:
                    vals.append(sum(xs) / len(xs))
            return (vals, nv, name,
                    "real" if "integer" not in m["types"].get(name, "") else "")
        return None, None, name, "vertex attribute %s not in the file" % name
    return None, None, color, "unknown colour %s" % color


def is_categorical(vals):
    vs = {v for v in vals if v is not None}
    return bool(vs) and len(vs) <= 12 and all(float(v).is_integer() for v in vs)


def attr_range(avs_file, color):
    """(min, max) of the values that colour the picture, or None"""
    m = read_avs(avs_file)
    if "too_large" in m:
        return None
    cv, nv, _, msg = cell_values(m, split_spec(color)[0])
    if msg and msg != "real":
        return None
    vals = [v for v in (nv if nv is not None else cv or []) if v is not None]
    return (min(vals), max(vals)) if vals else None


def legend(label, cat, values, lo, hi):
    out = []
    x0, y0 = W - 128, H - 30
    if cat:
        vs = sorted({v for v in values if v is not None})[:8]
        out.append('<text x="%d" y="%d" font-size="9" fill="#333">%s</text>'
                   % (x0, y0 - 4, esc(label)))
        for k, v in enumerate(vs):
            col = cat_rgb(v)
            x = x0 + (k % 4) * 31
            y = y0 + (k // 4) * 12
            out.append('<rect x="%d" y="%d" width="9" height="9" fill="rgb(%d,%d,%d)" '
                       'stroke="#333" stroke-width="0.4"/>' % ((x, y) + col))
            out.append('<text x="%d" y="%d" font-size="9" fill="#333">%g</text>'
                       % (x + 11, y + 8, v))
        return out
    out.append('<text x="%d" y="%d" font-size="9" fill="#333">%s</text>'
               % (x0, y0 - 4, esc(label)))
    for k in range(40):
        col = scale_rgb(k / 39)
        out.append('<rect x="%.1f" y="%d" width="3.2" height="8" '
                   'fill="rgb(%d,%d,%d)"/>' % ((x0 + k * 3, y0) + col))
    out.append('<text x="%d" y="%d" font-size="9" fill="#333">%.3g</text>'
               % (x0, y0 + 18, lo))
    out.append('<text x="%d" y="%d" font-size="9" fill="#333" '
               'text-anchor="end">%.3g</text>' % (x0 + 120, y0 + 18, hi))
    return out


def esc(t):
    return t.replace("&", "&amp;").replace("<", "&lt;").replace(">", "&gt;")


def wrap(sub_title):
    """the description in lines of at most about 56 characters, broken
    at "; " """
    parts, line = [], ""
    for piece in sub_title.split("; "):
        if line and len(line) + len(piece) + 2 > 56:
            parts.append(line)
            line = piece
        else:
            line = piece if not line else line + "; " + piece
    parts.append(line)
    return parts[:3]


def svg(lines, title, sub_title, cam):
    out = ['<svg xmlns="http://www.w3.org/2000/svg" width="%d" height="%d" '
           'viewBox="0 0 %d %d" font-family="sans-serif">' % (W, H, W, H),
           '<rect width="100%" height="100%" fill="#fff"/>']
    out += lines
    out.append('<text x="6" y="13" font-size="11" fill="#222">%s</text>'
               % esc(title))
    for k, t in enumerate(wrap(sub_title)):
        out.append('<text x="6" y="%d" font-size="10" fill="#555">%s</text>'
                   % (26 + 12 * k, esc(t)))
    out += triad(cam)
    out.append("</svg>")
    return "\n".join(out) + "\n"


# ------------------------------------------------------------ drawing
def split_spec(color):
    """"aspect[0,1]" -> ("aspect", (0.0, 1.0)); "mat" -> ("mat", None)"""
    if color and color.endswith("]") and "[" in color:
        base, _, r = color[:-1].partition("[")
        lo, _, hi = r.partition(",")
        return base, (float(lo), float(hi))
    return color, None


def draw(avs_file, svg_file, title, color="mat", vrange=None, view=None,
         box=None, mark=None, dot_scale=1.0):
    color, fixed = split_spec(color)
    vrange = fixed or vrange
    m = read_avs(avs_file)
    cam0 = default_camera() + (False,)
    if "too_large" in m:
        desc = "%d vertices, %d cells: too large to draw" % m["too_large"]
        open(svg_file, "w").write(svg([], title, desc, cam0))
        return desc
    V = m["V"]
    kids = m["cell"].get("itetkid")
    parents = octree_parents(V, m["C"], kids)
    nall = len(m["C"])
    keep = [n for n in range(nall) if n not in parents]
    types = {}
    for n in keep:
        t = m["C"][n][0]
        types[t] = types.get(t, 0) + 1
    desc = "%d vertices" % len(V)
    if keep:
        desc += ", " + ", ".join("%d %s" % (n, t) for t, n in
                                 sorted(types.items(), key=lambda q: -q[1]))
    if parents:
        desc += "; octree: leaves only, %d parent cells of %d not drawn" % (
            len(parents), nall)
    if not V:
        open(svg_file, "w").write(svg([], title, desc + "; empty", cam0))
        return desc
    cvals, nvals, label, msg = cell_values(m, color)
    real = msg == "real"
    missing = bool(msg) and not real
    if missing:                             # no colour: never the material
        desc += "; " + msg
        cvals, nvals = [None] * len(m["C"]), None
        color = "none"
    C = [m["C"][n] for n in keep]
    cvals = [cvals[n] for n in keep] if cvals is not None else [None] * len(C)
    allv = [v for v in (nvals if nvals is not None else cvals) if v is not None]
    cat = is_categorical(allv) and not real
    lo, hi = (vrange if vrange else
              ((min(allv), max(allv)) if allv else (0.0, 1.0)))
    if color == "aspect" and not vrange:
        # 1 is the best; the scale runs from the worst cell (rounded
        # down to a tenth) to 1, so the poor cells stand out
        lo, hi = (math.floor(min(allv) * 10) / 10 if allv else 0.0), 1.0
    if color not in (None, "", "mat") and not cat and allv:
        desc += "; %s %.4g to %.4g" % (label, min(allv), max(allv))
    if nvals is not None:
        desc += "; coloured at the vertices"
    if color in (None, "", "mat"):
        mats = sorted({v for v in cvals if v is not None})
        if len(mats) > 1:
            desc += "; materials %s" % ",".join("%g" % v for v in mats[:8])

    NEUTRAL = (222, 222, 228)

    def rgb(v):
        if v is None:
            return NEUTRAL
        if cat:
            return cat_rgb(v)
        return scale_rgb((v - lo) / ((hi - lo) or 1))

    R, U, E, planar = camera_for(V)
    if view == "top" and not planar:        # straight down the z axis
        R, U, E, planar = (1.0, 0.0, 0.0), (0.0, 1.0, 0.0), (0.0, 0.0, 1.0), False
        desc += "; seen from above (as its sink)"
    cam = (R, U, E, planar)
    light = (0.4, 0.5, 0.75)
    polys, segs = [], []                  # (depth, points, rgb)
    cnt, rep, own = {}, {}, {}
    if nvals is not None:                   # the vertices carry the colour
        cvals = [None] * len(C)
    for (t, ids, _), v in zip(C, cvals):
        if t in FACES and len(ids) >= max(max(f) for f in FACES[t]) + 1:
            cc = tuple(sum(V[i][k] for i in ids) / len(ids) for k in range(3))
            for fc in FACES[t]:
                f = [ids[i] for i in fc]
                k = tuple(sorted(f))
                cnt[k] = cnt.get(k, 0) + 1
                rep[k] = f
                own[k] = (cc, v)
        elif t in SURF:
            X = [V[i] for i in ids]
            sh = 0.9
            if not planar:
                nrm = (0.0, 0.0, 0.0)
                for i in range(len(X)):
                    c = cross(X[i], X[(i + 1) % len(X)])
                    nrm = (nrm[0] + c[0], nrm[1] + c[1], nrm[2] + c[2])
                ln = math.sqrt(dot(nrm, nrm)) or 1
                sh = 0.5 + 0.45 * abs(dot(nrm, light)) / ln
            depth = sum(project(p, cam)[2] for p in X) / len(X)
            polys.append((depth, X, tuple(min(255, int(c * sh)) for c in rgb(v)),
                          ids))
        elif t == "line" and len(ids) >= 2:
            segs.append(([V[i] for i in ids[:2]], v))
    for k, n in cnt.items():
        if n != 1:
            continue
        f = rep[k]
        cc, v = own[k]
        X = [V[i] for i in f]
        nrm = (0.0, 0.0, 0.0)
        for i in range(len(X)):
            c = cross(X[i], X[(i + 1) % len(X)])
            nrm = (nrm[0] + c[0], nrm[1] + c[1], nrm[2] + c[2])
        ln = math.sqrt(dot(nrm, nrm)) or 1
        fcen = tuple(sum(p[q] for p in X) / len(X) for q in range(3))
        if dot(nrm, sub(fcen, cc)) < 0:
            nrm = (-nrm[0], -nrm[1], -nrm[2])
        if dot(nrm, E) <= 0:
            continue
        sh = 0.5 + 0.45 * abs(dot(nrm, light)) / ln
        depth = sum(project(p, cam)[2] for p in X) / len(X)
        polys.append((depth, X, tuple(min(255, int(c * sh)) for c in rgb(v)), f))
    if planar:
        desc += "; seen down the plane's normal"
    if mark:
        desc += "; red box: where the sink lies"
    P2 = [project(p, cam) for p in V]
    if box:                                 # one frame for two pictures
        P2 = P2 + [(box[0], box[1], 0.0), (box[2], box[3], 0.0)]
    T = frame(P2, len(wrap(desc)))
    sw = "0.35" if len(polys) < 4000 else "0.15"
    lines = []
    polys.sort(key=lambda q: q[0])
    # a vertex attribute: each vertex on a visible face is a dot in its
    # colour, drawn just after the nearest face that holds it, so faces
    # in front of it still hide it
    last = {}
    if nvals is not None:
        for k, q in enumerate(polys):
            for i in q[3]:
                last[i] = k
    nvis = len(last) if last else len(V)
    rdot = max(1.0, min(3.5, 150.0 / math.sqrt(max(nvis, 1)))) * dot_scale
    after = {}
    for i, k in last.items():
        after.setdefault(k, []).append(i)

    def dot_svg(i):
        x, y = T(*project(V[i], cam)[:2])
        col = rgb(nvals[i]) if nvals is not None and i < len(nvals) else (53, 87, 168)
        return ('<circle cx="%.1f" cy="%.1f" r="%.1f" fill="rgb(%d,%d,%d)" '
                'stroke="#222" stroke-width="0.25"/>' % ((x, y, rdot) + col))
    for k, (depth, X, col, ids) in enumerate(polys):
        pts = " ".join("%.1f,%.1f" % T(*project(p, cam)[:2]) for p in X)
        lines.append('<polygon points="%s" fill="rgb(%d,%d,%d)" stroke="#556" '
                     'stroke-width="%s"/>' % ((pts,) + col + (sw,)))
        for i in after.get(k, ()):
            lines.append(dot_svg(i))
    for X, v in segs:
        (x1, y1), (x2, y2) = (T(*project(p, cam)[:2]) for p in X)
        col = rgb(v) if color not in (None, "", "mat") else (42, 75, 141)
        lines.append('<line x1="%.1f" y1="%.1f" x2="%.1f" y2="%.1f" '
                     'stroke="rgb(%d,%d,%d)" stroke-width="1"/>'
                     % ((x1, y1, x2, y2) + col))
    # vertices only (no cells, or only lines): every vertex is a dot
    if not polys and (not C or nvals is not None):
        rdot = 2.4 if len(V) < 2000 else 0.8
        for i in sorted(range(len(V)), key=lambda i: project(V[i], cam)[2]):
            lines.append(dot_svg(i))
    if mark:                                # where the sink lies
        (ax, ay), (bx, by) = T(mark[0], mark[1]), T(mark[2], mark[3])
        x0, x1 = min(ax, bx), max(ax, bx)
        y0, y1 = min(ay, by), max(ay, by)
        if x1 - x0 < 4:
            x0, x1 = (x0 + x1) / 2 - 2, (x0 + x1) / 2 + 2
        if y1 - y0 < 4:
            y0, y1 = (y0 + y1) / 2 - 2, (y0 + y1) / 2 + 2
        lines.append('<rect x="%.1f" y="%.1f" width="%.1f" height="%.1f" '
                     'fill="none" stroke="#d00" stroke-width="1.2" '
                     'stroke-dasharray="3,2"/>' % (x0, y0, x1 - x0, y1 - y0))
        lines.append('<text x="%.1f" y="%.1f" font-size="10" fill="#d00" '
                     'paint-order="stroke" stroke="#fff" stroke-width="3">'
                     'sink</text>' % (x1 + 3, y0 + 9))
    if (color not in (None, "", "mat", "none")
            or (color != "none" and allv and len(set(allv)) > 1)):
        lines += legend(label, cat, allv, lo, hi)
    open(svg_file, "w").write(svg(lines, title, desc, cam))
    return desc
