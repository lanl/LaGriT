#!/usr/bin/env python3
"""check_hextotet.py WORKDIR LOGDIR

Judges hextotet and grid2grid (Python 3, standard library only) from
the meshes the decks write, against answers computed here from the
source mesh alone.

The options (hextotet / N; grid2grid names in brackets)
   2  quad    -> 2 triangles, no new vertex           (quadtotri2)
   4  quad    -> 4 triangles, 1 new: quad centre      (quadtotri4)
   4  pyramid -> 4 tets, 1 new: base centre           (pyrtotet4)
   3  prism   -> 3 tets, no new vertex                (prismtotet3)
   5  hex     -> 5 tets, no new vertex                (hextotet5)
   6  hex     -> 6 tets, no new vertex                (hextotet6)
  14  prism   -> 14 tets, new: centroid + 3 quad face centres
                                                      (prismtotet14)
  18  prism   -> 18 tets, new: centroid + 5 face centres (prismtotet18)
  24  hex     -> 24 tets, new: centroid + 6 face centres (hextotet24)
A centroid is the average of the cell's vertices, a face centre the
average of the face's vertices. A face centre on a face shared by two
cells is ONE vertex (a split that conforms across the face).

Deck header lines (see 00README):
  # split   : OUT.inp SRC.inp OPTION   judge OUT as SRC split by OPTION
  # same    : A.inp B.inp              the two files must be identical
                                        (grid2grid writes what hextotet
                                        writes)
  # refused : TEXT                     TEXT must be in the deck's output

HARD checks of a split (all must hold for PASS)
  cells     every cell is a tet (triangle for 2D options); count =
            factor x source cells
  vertices  count = source vertices + the new vertices expected (each
            shared face centre once); the source vertices first, at
            their own coordinates; every new vertex at a distinct
            expected point (centroid or face centre), every expected
            point present
  volume    every cell has positive volume in LaGriT's sense (tets: the
            AVS order 1 2 4 3 is LaGriT's 1 2 3 4; triangles: the
            orientation of the source quads); the total = the source
            volume (cells split through their centroid and face
            centres; every source here has a flat outside)
  conform   no face (2D: edge) is shared by more than two cells, and
            the faces on one cell only have the area (length) of the
            source boundary: no gaps or overlaps inside
  parent    every cell lies in exactly one source cell (its vertices
            are among that cell's vertices, centroid and face centres)
            and has that cell's material; the volume of the cells in
            each source cell = that cell's volume, if its faces are flat
  field     if the source has the linear vertex field f = 1 + 2x + 3y
            + 5z, every output vertex has f at its own position (to
            1e-9): new vertices are averages of the old ones
MEASURED (reported, not judged): smallest and largest dihedral angle of
the tets (triangles: angle), materials of the new vertices (imt).

Every deck must also finish ("LaGriT successfully completed") with no
MMVERIFY message, no "Program received signal" and no FATAL.

Writes LOGDIR/<deck>.check, LOGDIR/table.txt and LOGDIR/summary.html
with pictures (draw_meshes.py from test_lagrit_upstream, and an
exploded view of the cells made from one source cell).
"""
import glob
import html
import math
import os
import sys

HERE = os.path.dirname(os.path.abspath(__file__))
sys.path.insert(0, os.path.join(HERE, "..", "test_lagrit_upstream"))
try:
    import draw_meshes
except Exception:                       # pictures are optional
    draw_meshes = None

E = html.escape
TOL = 1e-9

# LaGriT local cell faces (vertex positions in the AVS cell line, 0-based)
# Hex and prism AVS lines list the top face first (as dump/avs writes
# LaGriT's brick); the faces only need to be the right vertex sets.
FACES = {"hex": [(0, 1, 2, 3), (4, 5, 6, 7), (0, 1, 5, 4), (1, 2, 6, 5),
                 (2, 3, 7, 6), (3, 0, 4, 7)],
         "prism": [(0, 1, 2), (3, 4, 5), (0, 1, 4, 3), (1, 2, 5, 4),
                   (2, 0, 3, 5)],
         "pyr": [(1, 2, 3, 4), (0, 1, 2), (0, 2, 3), (0, 3, 4), (0, 4, 1)],
         "tet": [(0, 1, 2), (0, 1, 3), (0, 2, 3), (1, 2, 3)],
         "quad": [(0, 1), (1, 2), (2, 3), (3, 0)],
         "tri": [(0, 1), (1, 2), (2, 0)]}
#                 source  out    factor  centroid  which faces get a centre
OPTIONS = {(2, "quad"): ("tri", 2, False, None),
           (4, "quad"): ("tri", 4, True, None),
           (4, "pyr"): ("tet", 4, False, 4),      # the quad base only
           (3, "prism"): ("tet", 3, False, None),
           (5, "hex"): ("tet", 5, False, None),
           (6, "hex"): ("tet", 6, False, None),
           (14, "prism"): ("tet", 14, True, 4),   # the 3 quad faces
           (18, "prism"): ("tet", 18, True, 0),   # all 5 faces
           (24, "hex"): ("tet", 24, True, 0)}     # all 6 faces
ALIAS = {"pri": "prism", "pyramid": "pyr", "tetra": "tet", "hexa": "hex"}


# ------------------------------------------------------------ geometry
def sub(a, b):
    return (a[0] - b[0], a[1] - b[1], a[2] - b[2])


def cross(a, b):
    return (a[1] * b[2] - a[2] * b[1], a[2] * b[0] - a[0] * b[2], a[0] * b[1] - a[1] * b[0])


def dot(a, b):
    return a[0] * b[0] + a[1] * b[1] + a[2] * b[2]


def norm(a):
    return math.sqrt(dot(a, a))


def avg(P):
    n = float(len(P))
    return (sum(p[0] for p in P) / n, sum(p[1] for p in P) / n, sum(p[2] for p in P) / n)


def det6(a, b, c, d):
    return dot(sub(b, a), cross(sub(c, a), sub(d, a))) / 6.0


def f_lin(p):
    return 1.0 + 2.0 * p[0] + 3.0 * p[1] + 5.0 * p[2]


def key(p, scale):
    return tuple(int(round(c / (scale * 1e-9))) for c in p)


def poly_area(P):
    """area of a (possibly warped) polygon as the fan about its centre"""
    c = avg(P)
    return sum(0.5 * norm(cross(sub(P[i], c), sub(P[(i + 1) % len(P)], c))) for i in range(len(P)))


def is_flat(P):
    if len(P) < 4:
        return True
    n = cross(sub(P[1], P[0]), sub(P[2], P[0]))
    s = norm(n) or 1.0
    return all(abs(dot(sub(p, P[0]), n)) / s < 1e-12 for p in P[3:])


def cell_volume(typ, P):
    """volume (3D, through centroid and face centres) or area (2D)"""
    if typ in ("quad", "tri"):
        return poly_area(P)
    c = avg(P)
    v = 0.0
    for f in FACES[typ]:
        Q = [P[i] for i in f]
        fc = avg(Q)
        for i in range(len(Q)):
            v += abs(det6(c, Q[i], Q[(i + 1) % len(Q)], fc))
    return v


def dihedrals(P):
    """the six dihedral angles of a tet, degrees"""
    out = []
    for i, j in ((0, 1), (0, 2), (0, 3), (1, 2), (1, 3), (2, 3)):
        k, l = [m for m in range(4) if m not in (i, j)]
        e = sub(P[j], P[i])
        a = sub(P[k], P[i])
        b = sub(P[l], P[i])
        ee = dot(e, e) or 1.0
        a = sub(a, tuple(e[m] * dot(a, e) / ee for m in range(3)))
        b = sub(b, tuple(e[m] * dot(b, e) / ee for m in range(3)))
        c = dot(a, b) / ((norm(a) * norm(b)) or 1.0)
        out.append(math.degrees(math.acos(max(-1.0, min(1.0, c)))))
    return out


def angles2d(P):
    out = []
    for i in range(3):
        a = sub(P[(i + 1) % 3], P[i])
        b = sub(P[(i + 2) % 3], P[i])
        c = dot(a, b) / ((norm(a) * norm(b)) or 1.0)
        out.append(math.degrees(math.acos(max(-1.0, min(1.0, c)))))
    return out


# ------------------------------------------------------------ reading
def read_avs(fn):
    with open(fn) as f:
        head = f.readline().split()
        nn, nc = int(head[0]), int(head[1])
        nnd = int(head[2]) if len(head) > 2 else 0
        V = []
        for _ in range(nn):
            w = f.readline().split()
            V.append((float(w[1]), float(w[2]), float(w[3])))
        C = []
        for _ in range(nc):
            w = f.readline().split()
            t = w[2].lower()
            C.append((ALIAS.get(t, t), [int(v) - 1 for v in w[3:]], int(w[1])))
        node = {}
        if nnd > 0:
            w = f.readline().split()
            na, sizes = int(w[0]), [int(x) for x in w[1:]]
            names = [f.readline().partition(",")[0].strip().lower() for _ in range(na)]
            cols = {n: [] for n in names}
            for _ in range(nn):
                w = f.readline().split()[1:]
                k = 0
                for n, sz in zip(names, sizes or [1] * na):
                    cols[n].append(float(w[k].replace("D", "E")))
                    k += sz
            node = cols
    return {"V": V, "C": C, "node": node}


def headers(deck):
    out = {"split": [], "same": [], "refused": [], "doc": []}
    for line in open(deck):
        if not line.startswith("#"):
            break
        body = line[1:].strip()
        for k in ("split", "same", "refused"):
            if body.startswith(k) and ":" in body and body.split(":")[0].strip() == k:
                out[k].append(body.split(":", 1)[1].strip())
                break
        else:
            if body and not body.startswith("#") and not body.endswith(".lgi"):
                out["doc"].append(body)
    return out


# ------------------------------------------------------------ one split
def check_split(work, out_name, src_name, opt):
    r = {"out": out_name, "src": src_name, "opt": opt, "fails": [], "rows": {}}
    fails = r["fails"]
    po, ps = os.path.join(work, out_name), os.path.join(work, src_name)
    if not os.path.isfile(ps):
        fails.append("source %s not written" % src_name)
        return r
    if not os.path.isfile(po):
        fails.append("%s not written" % out_name)
        return r
    S, O = read_avs(ps), read_avs(po)
    styp = {c[0] for c in S["C"]}
    if len(styp) != 1:
        fails.append("source has cell types %s" % sorted(styp))
        return r
    styp = styp.pop()
    spec = OPTIONS.get((opt, styp))
    if spec is None:
        fails.append("option %d does not apply to %s cells" % (opt, styp))
        return r
    otyp, factor, want_cen, which_faces = spec
    VS, CS = S["V"], S["C"]
    span = max(max(p[k] for p in VS) - min(p[k] for p in VS) for k in range(3)) or 1.0

    # ---- expected new points: centroids, face centres (shared once)
    pts = {}                        # key -> (kind, set of source cells)
    for i, p in enumerate(VS):
        pts.setdefault(key(p, span), ("vertex", set()))
    for ci, (t, ids, _) in enumerate(CS):
        for v in ids:
            pts[key(VS[v], span)][1].add(ci)
    expected_new = {}
    faces_of = {}
    for ci, (t, ids, _) in enumerate(CS):
        P = [VS[v] for v in ids]
        if want_cen:
            k = key(avg(P), span)
            expected_new[k] = avg(P)
            pts.setdefault(k, ("centroid", set()))[1].add(ci)
        if which_faces is not None and t in FACES:
            for f in FACES[t]:
                if which_faces and len(f) != which_faces:
                    continue
                Q = [VS[ids[i]] for i in f]
                k = key(avg(Q), span)
                expected_new[k] = avg(Q)
                pts.setdefault(k, ("face", set()))[1].add(ci)
    if opt == 4 and styp == "quad":
        pass                        # the quad centre is its centroid
    n_expect_v = len(VS) + len(expected_new)
    n_expect_c = factor * len(CS)

    VO, CO = O["V"], O["C"]
    types = {c[0] for c in CO}
    r["rows"]["cells"] = "%d / %d" % (len(CO), n_expect_c)
    r["rows"]["vertices"] = "%d / %d" % (len(VO), n_expect_v)
    if types != {otyp}:
        fails.append("cell types %s, expected %s" % (sorted(types), otyp))
    if len(CO) != n_expect_c:
        fails.append("%d cells, expected %d" % (len(CO), n_expect_c))
    if len(VO) != n_expect_v:
        fails.append("%d vertices, expected %d" % (len(VO), n_expect_v))
    moved = sum(1 for i in range(min(len(VS), len(VO))) if norm(sub(VS[i], VO[i])) > TOL * span)
    if moved:
        fails.append("%d source vertices moved or reordered" % moved)
    got_new = {}
    stray = 0
    for p in VO[len(VS):]:
        k = key(p, span)
        if k in expected_new:
            got_new[k] = got_new.get(k, 0) + 1
        else:
            stray += 1
    dup = sum(n - 1 for n in got_new.values() if n > 1)
    missing = len(expected_new) - len(got_new)
    r["rows"]["new vertices"] = "%d placed, %d missing, %d stray, %d twice" % (len(got_new), missing, stray, dup)
    if stray or dup or missing:
        fails.append("new vertices: %d not at a centroid or face centre, %d twice, %d missing" % (stray, dup, missing))
    if types != {otyp} or not CO:
        return r

    # ---- orientation and volume
    nneg = 0
    vol = 0.0
    ref_sign = None
    if otyp == "tri":
        q = CS[0][1]
        ref = cross(sub(VS[q[1]], VS[q[0]]), sub(VS[q[2]], VS[q[0]]))
    for t, ids, _ in CO:
        P = [VO[i] for i in ids]
        if otyp == "tet":
            v = -det6(P[0], P[1], P[2], P[3])      # LaGriT's sense
        else:
            v = 0.5 * dot(cross(sub(P[1], P[0]), sub(P[2], P[0])), ref) / (norm(ref) or 1.0)
        if v <= 1e-14 * span ** 3:
            nneg += 1
        vol += abs(v)
    svol = sum(cell_volume(t, [VS[v] for v in ids]) for t, ids, _ in CS)
    r["rows"]["volume"] = "%.12g / %.12g" % (vol, svol)
    r["rows"]["inverted or flat"] = str(nneg)
    if nneg:
        fails.append("%d cells inverted or flat" % nneg)
    if abs(vol - svol) > 1e-9 * max(svol, 1e-30):
        fails.append("volume %.12g, source %.12g" % (vol, svol))

    # ---- conformity: faces (2D: edges) shared by at most 2 cells
    fo = FACES[otyp]
    count = {}
    for t, ids, _ in CO:
        for f in fo:
            k = tuple(sorted(ids[i] for i in f))
            count[k] = count.get(k, 0) + 1
    over = sum(1 for n in count.values() if n > 2)
    bnd = 0.0
    for k, n in count.items():
        if n == 1:
            P = [VO[i] for i in k]
            bnd += poly_area(P) if len(P) > 2 else norm(sub(P[1], P[0]))
    scount = {}
    for t, ids, _ in CS:
        for f in FACES[t]:
            k = tuple(sorted(ids[i] for i in f))
            scount[k] = scount.get(k, 0) + 1
    sb = 0.0
    for k, n in scount.items():
        if n == 1:
            P = [VS[i] for i in k]
            if len(P) > 2:
                # put the face's vertices back in cycle order
                c = avg(P)
                nrm = None
                for t, ids, _ in CS:
                    for f in FACES[t]:
                        if tuple(sorted(ids[i] for i in f)) == k:
                            P = [VS[ids[i]] for i in f]
                sb += poly_area(P)
            else:
                sb += norm(sub(P[1], P[0]))
    r["rows"]["boundary"] = "%.12g / %.12g" % (bnd, sb)
    if over:
        fails.append("%d faces shared by more than two cells" % over)
    if abs(bnd - sb) > 1e-9 * max(sb, 1e-30):
        fails.append("outside %s %.12g, source %.12g (gaps or overlaps inside)"
                     % ("area" if otyp == "tet" else "length", bnd, sb))

    # ---- parent, material, volume per source cell
    nopar = nmat = 0
    per = [0.0] * len(CS)
    kids = [[] for _ in CS]
    for ti, (t, ids, mat) in enumerate(CO):
        cand = None
        for i in ids:
            e = pts.get(key(VO[i], span))
            s = e[1] if e else set()
            cand = set(s) if cand is None else cand & s
        if not cand or len(cand) != 1:
            nopar += 1
            continue
        p = cand.pop()
        kids[p].append(ti)
        if mat != CS[p][2]:
            nmat += 1
        P = [VO[i] for i in ids]
        per[p] += abs(det6(*P)) if otyp == "tet" else poly_area(P)
    badvol = 0
    for ci, (t, ids, _) in enumerate(CS):
        P = [VS[v] for v in ids]
        if all(is_flat([P[i] for i in f]) for f in FACES[t]):
            if abs(per[ci] - cell_volume(t, P)) > 1e-9 * cell_volume(t, P):
                badvol += 1
    r["rows"]["parent / material / volume"] = "%d no parent, %d wrong material, %d cells' volume off" % (nopar, nmat, badvol)
    if nopar:
        fails.append("%d cells not inside exactly one source cell" % nopar)
    if nmat:
        fails.append("%d cells with a material other than their source cell's" % nmat)
    if badvol:
        fails.append("%d source cells whose tets do not fill them" % badvol)
    r["kids"] = kids

    # ---- the linear field
    if "f" in S["node"]:
        if "f" not in O["node"]:
            fails.append("vertex field f lost")
        else:
            err = max(abs(O["node"]["f"][i] - f_lin(VO[i])) for i in range(len(VO)))
            r["rows"]["field f, max error"] = "%.2e" % err
            if err > 1e-9 * 10:
                fails.append("field f off by %.3g at a vertex" % err)

    # ---- measured
    if otyp == "tet":
        ang = [a for t, ids, _ in CO for a in dihedrals([VO[i] for i in ids])]
        r["rows"]["dihedral min / max"] = "%.2f / %.2f deg" % (min(ang), max(ang))
    else:
        ang = [a for t, ids, _ in CO for a in angles2d([VO[i] for i in ids])]
        r["rows"]["angle min / max"] = "%.2f / %.2f deg" % (min(ang), max(ang))
    if "imt1" in O["node"] and len(VO) > len(VS):
        r["rows"]["imt of new vertices"] = ", ".join(str(v) for v in sorted({int(x) for x in O["node"]["imt1"][len(VS):]}))
    return r


# ------------------------------------------------------------ pictures
CAT = ["#8fa8d8", "#e6a35a", "#7cc47c", "#d77fc1", "#e0d36a", "#6ccfcf",
       "#c98f6b", "#a993e0", "#e07a7a", "#9ec2a0", "#d0b0e8", "#b8b8b8"]


def exploded_svg(path, out_avs, kids, title, sub_title):
    """the cells made from one source cell, each shrunk and pushed away
    from the source cell's centre so every one is seen"""
    O = read_avs(out_avs)
    if not kids:
        return False
    V = O["V"]
    cells = [O["C"][i] for i in kids]
    allp = [V[i] for _, ids, _ in cells for i in ids]
    c0 = avg(allp)
    rot, tilt = math.radians(-60.0), math.radians(25.0)
    Ev = (math.cos(tilt) * math.cos(rot), math.cos(tilt) * math.sin(rot), math.sin(tilt))
    R = (-math.sin(rot), math.cos(rot), 0.0)
    U = cross(Ev, R)
    flat = all(abs(p[2] - allp[0][2]) < 1e-12 for p in allp)
    if flat:
        R, U, Ev = (1.0, 0.0, 0.0), (0.0, 1.0, 0.0), (0.0, 0.0, 1.0)
    polys = []
    for n, (t, ids, _) in enumerate(cells):
        P = [V[i] for i in ids]
        c = avg(P)
        shift = tuple(0.45 * (c[k] - c0[k]) for k in range(3))
        P = [tuple(c[k] + 0.82 * (p[k] - c[k]) + shift[k] for k in range(3)) for p in P]
        faces = FACES["tet"] if t == "tet" else [tuple(range(len(P)))]
        for f in faces:
            Q = [P[i] for i in f]
            depth = dot(avg(Q), Ev)
            nrm = cross(sub(Q[1], Q[0]), sub(Q[2], Q[0]))
            shade = abs(dot(nrm, Ev)) / ((norm(nrm)) or 1.0)
            polys.append((depth, [(dot(q, R), dot(q, U)) for q in Q], CAT[n % len(CAT)], shade))
    polys.sort(key=lambda p: p[0])
    xs = [x for _, Q, _, _ in polys for x, _ in Q]
    ys = [y for _, Q, _, _ in polys for _, y in Q]
    W, H = 300, 250
    sc = 0.86 * min(W / ((max(xs) - min(xs)) or 1), (H - 44) / ((max(ys) - min(ys)) or 1))
    ox = (W - sc * (max(xs) + min(xs))) / 2
    oy = (H + 30 + sc * (max(ys) + min(ys))) / 2
    o = ['<svg xmlns="http://www.w3.org/2000/svg" viewBox="0 0 %d %d" width="%d" height="%d" '
         'font-family="sans-serif"><rect width="100%%" height="100%%" fill="#fff"/>' % (W, H, W, H)]
    for _, Q, col, shade in polys:
        r_, g_, b_ = (int(col[i:i + 2], 16) for i in (1, 3, 5))
        k = 0.55 + 0.45 * shade
        o.append('<polygon points="%s" fill="rgb(%d,%d,%d)" stroke="#333" stroke-width="0.6" '
                 'fill-opacity="0.92"/>' % (" ".join("%.1f,%.1f" % (ox + sc * x, oy - sc * y) for x, y in Q),
                                            r_ * k, g_ * k, b_ * k))
    o.append('<text x="6" y="13" font-size="11" fill="#222">%s</text>' % E(title))
    o.append('<text x="6" y="26" font-size="9" fill="#666">%s</text>' % E(sub_title))
    o.append("</svg>")
    open(path, "w").write("\n".join(o) + "\n")
    return True


def picture(work, logdir, avs, title, color="mat"):
    src = os.path.join(work, avs)
    if draw_meshes is None or not os.path.isfile(src):
        return ""
    svg = avs[:-4] + ".svg"
    try:
        draw_meshes.draw(src, os.path.join(logdir, svg), title, color=color)
    except Exception:
        return ""
    return '<figure><a href="%s"><img src="%s" width="300" alt="%s"></a></figure>' % (svg, svg, E(title))


# ------------------------------------------------------------ main
def main(work, logdir):
    decks = sorted(glob.glob(os.path.join(HERE, "ht[0-9]*.lgi")))
    results = []
    allok = True
    for deck in decks:
        name = os.path.basename(deck)[:-4]
        h = headers(deck)
        res = {"deck": name, "doc": h["doc"], "splits": [], "same": [], "refused": [], "fails": []}
        out = os.path.join(work, name + ".out")
        text = open(out, errors="replace").read() if os.path.isfile(out) else ""
        if "LaGriT successfully completed" not in text:
            res["fails"].append("LaGriT did not finish")
        for bad in ("Program received signal", "MMVERIFY:", "FATAL"):
            n = text.count(bad)
            if n:
                res["fails"].append('%d x "%s"' % (n, bad))
        for s in h["split"]:
            o, src, opt = s.split()
            res["splits"].append(check_split(work, o, src, int(opt)))
        for s in h["same"]:
            a, b = s.split()
            pa, pb = os.path.join(work, a), os.path.join(work, b)
            ok = os.path.isfile(pa) and os.path.isfile(pb) and open(pa).read() == open(pb).read()
            res["same"].append((a, b, ok))
            if not ok:
                res["fails"].append("%s and %s differ" % (a, b))
        for t in h["refused"]:
            ok = t in text
            res["refused"].append((t, ok))
            if not ok:
                res["fails"].append('no "%s"' % t)
        for sp in res["splits"]:
            res["fails"] += ["%s: %s" % (sp["out"], f) for f in sp["fails"]]
        res["result"] = "PASS" if not res["fails"] else "FAIL"
        allok = allok and not res["fails"]
        with open(os.path.join(logdir, name + ".check"), "w") as f:
            f.write("result %s\n" % res["result"])
            for sp in res["splits"]:
                for k, v in sp["rows"].items():
                    f.write("%s %s: %s\n" % (sp["out"], k, v))
            for x in res["fails"]:
                f.write("fail %s\n" % x)
        print("  %-4s  %s%s" % (res["result"], name, ("  " + "; ".join(res["fails"][:3])) if res["fails"] else ""))
        results.append(res)
    with open(os.path.join(logdir, "table.txt"), "w") as f:
        for r in results:
            f.write("%s %s\n" % (r["deck"], r["result"]))
    write_page(work, logdir, results)
    npass = sum(r["result"] == "PASS" for r in results)
    print("\npassed %d   failed %d" % (npass, len(results) - npass))
    return 0 if allok else 1


ROWS = ["cells", "vertices", "new vertices", "volume", "inverted or flat", "boundary",
        "parent / material / volume", "field f, max error", "dihedral min / max", "angle min / max",
        "imt of new vertices"]


def write_page(work, logdir, results):
    o = ["""<!doctype html><html><head><meta charset="utf-8">
<meta name="viewport" content="width=device-width, initial-scale=1">
<title>hextotet and grid2grid</title>
<style>body{font-family:-apple-system,Helvetica,Arial,sans-serif;margin:16px;max-width:1350px;color:#1f2328;background:#fff}
h2{font-size:17px;margin:28px 0 6px}p,li{font-size:14px;line-height:1.45;max-width:980px}
table{border-collapse:collapse;font-size:12px;margin:4px 0}td,th{border-bottom:1px solid #d0d7de;padding:2px 6px;text-align:left;vertical-align:top}
th{background:#f6f8fa}.PASS{color:#1a7f37;font-weight:600}.FAIL{color:#cf222e;font-weight:600}
.row{display:flex;flex-wrap:wrap;gap:10px;align-items:flex-start}.pair{display:flex;gap:4px;border:1px solid #d0d7de;border-radius:6px;padding:4px}figure{margin:0}code{font-size:12px}</style></head><body>
<p style="margin:0 0 8px;font-size:13px"><a href="../../summary.html">&larr; all suites (top-level summary)</a></p>
<h1 style="font-size:20px">hextotet and grid2grid: hex, prism, pyramid and quad cells split into tets and triangles</h1>
<p>Each split is judged against the source mesh alone: the number of cells and vertices; every new vertex at a
cell centroid or face centre, a face centre shared by two cells made once; every cell with positive volume and the
total volume of the source; a conforming split (no face shared by more than two cells, and the outside of the result
has exactly the area of the outside of the source, so there is no gap or overlap inside); every cell inside one source
cell, with its material, filling it; a linear vertex field <i>f</i> = 1 + 2x + 3y + 5z carried exactly to the new vertices.
grid2grid must write the same file as hextotet. Options that do not fit the cells must be refused.
Table entries are <i>got / expected</i>. Definitions: <code>check_hextotet.py</code>.</p>
<p><b>What it does:</b> each deck builds a small mesh of one cell type (hex, prism, pyramid, quad, or a mix with two
materials), splits it with <code>hextotet</code> and each of its options (and with <code>grid2grid</code>), and dumps
the source and the results; the checker judges every split from the dumps alone. Under each deck: the source mesh,
then each split coloured by material, with one source cell's new cells pulled apart.</p>
<table><tr><th>deck</th><th>result</th><th>what it does</th><th>what failed</th></tr>"""]
    for r in results:
        o.append('<tr><td><a href="#%s">%s</a></td><td class="%s">%s</td><td>%s</td><td>%s</td></tr>'
                 % (E(r["deck"]), E(r["deck"]), r["result"], r["result"], E(" ".join(r["doc"])),
                    E("; ".join(r["fails"][:4])) or "-"))
    o.append("</table>")
    for r in results:
        o.append('<h2 id="%s">%s <span class="%s">%s</span></h2>' % (E(r["deck"]), E(r["deck"]), r["result"], r["result"]))
        o.append("<p>%s</p>" % E(" ".join(r["doc"])))
        if r["splits"]:
            o.append("<table><tr><th></th>%s</tr>" % "".join(
                "<th>%s<br>option %d</th>" % (E(sp["out"]), sp["opt"]) for sp in r["splits"]))
            for row in ROWS:
                if any(row in sp["rows"] for sp in r["splits"]):
                    o.append("<tr><td>%s</td>%s</tr>" % (E(row), "".join(
                        "<td>%s</td>" % E(sp["rows"].get(row, "")) for sp in r["splits"])))
            o.append("<tr><td>result</td>%s</tr></table>" % "".join(
                '<td class="%s">%s</td>' % ("FAIL" if sp["fails"] else "PASS", "FAIL" if sp["fails"] else "PASS")
                for sp in r["splits"]))
        if r["same"]:
            o.append("<p>%s</p>" % "<br>".join(
                '<span class="%s">%s</span> %s and %s identical' % ("PASS" if ok else "FAIL", "same" if ok else "DIFFER", E(a), E(b))
                for a, b, ok in r["same"]))
        if r["refused"]:
            o.append("<p>Refused, with the message:<br>%s</p>" % "<br>".join(
                '<span class="%s">%s</span> <code>%s</code>' % ("PASS" if ok else "FAIL", "found" if ok else "MISSING", E(t))
                for t, ok in r["refused"]))
        for f in r["fails"]:
            o.append('<p class="FAIL">%s</p>' % E(f))
        pics = []
        seen = set()
        for sp in r["splits"]:
            if sp["src"] not in seen:
                seen.add(sp["src"])
                pics.append(picture(work, logdir, sp["src"], "source %s (material)" % sp["src"]))
        o.append('<div class="row">%s</div>' % "".join(p for p in pics if p))
        pics = []
        for sp in r["splits"]:
            pair = [picture(work, logdir, sp["out"], "%s: option %d (material)" % (sp["out"], sp["opt"]))]
            if sp.get("kids"):
                ci = next((i for i, k in enumerate(sp["kids"]) if k), None)
                svg = sp["out"][:-4] + "_one_cell.svg"
                try:
                    if ci is not None and exploded_svg(
                            os.path.join(logdir, svg), os.path.join(work, sp["out"]), sp["kids"][ci],
                            "option %d: one source cell, exploded" % sp["opt"],
                            "the %d cells made from source cell %d" % (len(sp["kids"][ci]), ci + 1)):
                        pair.append('<figure><a href="%s"><img src="%s" width="300" alt="exploded"></a></figure>' % (svg, svg))
                except Exception:
                    pass
            pics.append('<div class="pair">%s</div>' % "".join(x for x in pair if x))
        o.append('<div class="row">%s</div>' % "".join(p for p in pics if p))
    o.append("</body></html>")
    open(os.path.join(logdir, "summary.html"), "w").write("\n".join(o) + "\n")


if __name__ == "__main__":
    if len(sys.argv) != 3:
        sys.exit(__doc__)
    sys.exit(main(sys.argv[1], sys.argv[2]))
