#!/usr/bin/env python3
"""check_recon.py WORKDIR LOGDIR

Judges the test_recon decks (Python 3, standard library only) from the
meshes each deck dumps before and after each recon (<deck>_<step>.inp)
and LaGriT's output (<deck>.out). Every test is computed here, in exact
rational arithmetic on the coordinates as written in the dump:

  valid            no cell with zero or inverted area / volume; no face
                   in more than two tets
  delaunay         no interior edge (face) whose far vertex is inside the
                   circumcircle (circumsphere) of the cell on the other side
  centres_inside   every cell's circumcentre (its Voronoi vertex) inside
                   the domain, here the box of the vertices: the aim of
                   recon / 1 (positive coupling coefficients for two-point
                   flux codes; Murphy and Gable 2000); fewer_centres_out:
                   fewer outside than before
  no_obtuse_boundary  no triangle with an angle over 90 degrees opposite
                   a boundary edge (its Voronoi coefficient is negative)
  same_vertices    the same vertices; same_boundary: the same boundary
                   edges (faces); same_size: the same area (volume), to
                   1e-12; unchanged: the same vertices and cells
  added_on_boundary  every new vertex lies on the boundary of the mesh
                   before (2D: on a boundary edge; 3D: on a boundary face)
  added_on_boundary_or_interface  2D: on a boundary or interface edge
  same_materials   the area (volume) of each material unchanged, to 1e-9
  interface_kept   every interface edge lies along an interface edge of
                   the mesh before (2D)
  fewer_nondelaunay, fewer_obtuse_boundary   fewer than before
  no_stray_file    recon wrote no file of its own (gmv_refine_edge_3d)
  finished         LaGriT printed "LaGriT successfully completed"
  clean_output     no "zero terminated parent/child chain" message
A deck's "known" checks are expected to fail (a documented problem):
the deck is KNOWN while they fail and FIXED (an error, so the entry is
updated) when one passes. Writes LOGDIR/<deck>.check, table.txt and
summary.html.
"""
import html
import json
import os
import sys
from fractions import Fraction as F

E = html.escape

from fractions import Fraction as F
import sys


def read_avs(fn):
    L = open(fn).read().split("\n")
    nn, ne, nva, nea = (int(x) for x in L[0].split()[:4])
    V, Vf = [], []
    for i in range(nn):
        w = L[1 + i].split()
        V.append(tuple(float(x) for x in w[1:4]))
        Vf.append(tuple(F(x) for x in w[1:4]))
    C, col, typ = [], [], []
    for i in range(ne):
        w = L[1 + nn + i].split()
        col.append(int(w[1]))
        typ.append(w[2])
        C.append([int(x) - 1 for x in w[3:]])
    k = 1 + nn + ne
    A = {}
    if nva:
        nh = int(L[k].split()[0])
        names = [L[k + 1 + j].split(",")[0].strip() for j in range(nh)]
        for n in names:
            A[n] = []
        for i in range(nn):
            w = L[k + 1 + nh + i].split()[1:]
            for n, x in zip(names, w):
                A[n].append(float(x))
    return V, Vf, C, col, typ, A


def sub(a, b):
    return [a[i] - b[i] for i in range(len(a))]


def det3(u, v, w):
    return (u[0] * (v[1] * w[2] - v[2] * w[1]) - u[1] * (v[0] * w[2] - v[2] * w[0])
            + u[2] * (v[0] * w[1] - v[1] * w[0]))


def area2(a, b, c):
    return (b[0] - a[0]) * (c[1] - a[1]) - (b[1] - a[1]) * (c[0] - a[0])


def incircle(a, b, c, d):
    """> 0 when d is inside the circle of the counterclockwise a b c"""
    m = []
    for p in (a, b, c):
        dx, dy = p[0] - d[0], p[1] - d[1]
        m.append((dx, dy, dx * dx + dy * dy))
    return det3(*m)


def insphere(a, b, c, d, e):
    """> 0 when e is inside the sphere and det(a-e, b-e, c-e) ... orientation
    det(b-a, c-a, d-a) < 0 (Shewchuk's positive orientation)"""
    rows = []
    for p in (a, b, c, d):
        q = sub(p, e)
        rows.append(q + [q[0] * q[0] + q[1] * q[1] + q[2] * q[2]])
    # 4x4 determinant
    def d4(M):
        s = 0
        for j in range(4):
            minor = [[M[r][c] for c in range(4) if c != j] for r in range(1, 4)]
            s += (-1) ** j * M[0][j] * det3(*minor)
        return s
    return d4(rows)




def circumcentre(P, dim):
    """exact circumcentre of a triangle (2D) or tet, None if flat"""
    a = P[0]
    rows = [[P[k][i] - a[i] for i in range(dim)] for k in range(1, dim + 1)]
    rhs = [sum(x * x for x in r) / 2 for r in rows]
    if dim == 2:
        d = rows[0][0] * rows[1][1] - rows[0][1] * rows[1][0]
        if d == 0:
            return None
        x = (rhs[0] * rows[1][1] - rows[0][1] * rhs[1]) / d
        y = (rows[0][0] * rhs[1] - rhs[0] * rows[1][0]) / d
        return (a[0] + x, a[1] + y)
    d = det3(*rows)
    if d == 0:
        return None
    out = []
    for j in range(3):
        m = [list(r) for r in rows]
        for i in range(3):
            m[i][j] = rhs[i]
        out.append(a[j] + det3(*m) / d)
    return tuple(out)


def boundary_of(C, dim):
    """{edge or face (sorted vertex tuple): [(cell, far vertex)]}"""
    S = {}
    for t, c in enumerate(C):
        k = len(c)
        for i in range(k):
            if dim == 2:
                f = tuple(sorted((c[i], c[(i + 1) % 3])))
                o = c[(i + 2) % 3]
            else:
                f = tuple(sorted(c[:i] + c[i + 1:]))
                o = c[i]
            S.setdefault(f, []).append((t, o))
    return S


def measure(fn, dim):
    V, Vf, C, col, typ, A = read_avs(fn)
    S = boundary_of(C, dim)
    r = dict(V=V, Vf=Vf, C=C, col=col, S=S)
    sgn = []
    for c in C:
        p = [Vf[k] for k in c]
        if dim == 2:
            sgn.append(area2(p[0], p[1], p[2]))
        else:
            sgn.append(det3(sub(p[1], p[0]), sub(p[2], p[0]), sub(p[3], p[0])))
    good = 1 if dim == 2 else -1          # LaGriT: tri counterclockwise, tet det < 0
    r["inverted"] = sum(1 for s in sgn if s == 0 or (s > 0) != (good > 0))
    r["overfull"] = sum(1 for L in S.values() if len(L) > 2)
    nd, bad_faces, obt, obt_items = 0, [], 0, []
    for f, L in S.items():
        if len(L) == 2:
            (t1, o1), (t2, o2) = L
            p = [Vf[k] for k in C[t1]]
            if dim == 2:
                s = incircle(p[0], p[1], p[2], Vf[o2]) * (1 if sgn[t1] > 0 else -1)
                if s > 0:
                    nd += 1
                    bad_faces.append(f)
            else:
                s = insphere(*p, Vf[o2])
                if s * sgn[t1] < 0:
                    nd += 1
                    bad_faces.append(f)
        elif len(L) == 1:
            if dim == 2:
                a, b, p = Vf[f[0]], Vf[f[1]], Vf[L[0][1]]
                if (a[0] - p[0]) * (b[0] - p[0]) + (a[1] - p[1]) * (b[1] - p[1]) < 0:
                    obt += 1
                    obt_items.append(f)
            else:
                a, b, c = (Vf[k] for k in f)
                for x, y, z in ((a, b, c), (b, c, a), (c, a, b)):
                    u, v = sub(y, x), sub(z, x)
                    if u[0] * v[0] + u[1] * v[1] + u[2] * v[2] < 0:
                        obt += 1
                        obt_items.append(f)
    r["nonDelaunay"], r["bad"], r["obtuse_boundary"], r["obt"] = nd, bad_faces, obt, obt_items
    # Voronoi centres (cell circumcentres) outside the domain: the box the
    # vertices span (every mesh here is a box). recon / 1 is meant to bring
    # them all inside (Murphy and Gable 2000): positive coupling coefficients
    lo = [min(p[i] for p in Vf) for i in range(dim)]
    hi = [max(p[i] for p in Vf) for i in range(dim)]
    outc = []
    for t, c in enumerate(C):
        cc = circumcentre([Vf[k] for k in c], dim)
        if cc is not None and any(cc[i] < lo[i] or cc[i] > hi[i] for i in range(dim)):
            outc.append(t)
    r["centres_out"] = len(outc)
    r["outc"] = outc
    r["bnd"] = [f for f, L in S.items() if len(L) == 1]
    r["iface"] = [f for f, L in S.items() if len(L) == 2 and col[L[0][0]] != col[L[1][0]]]
    size = {}
    for t, s in enumerate(sgn):
        size[col[t]] = size.get(col[t], 0) + abs(s)
    k = 2 if dim == 2 else 6
    r["by_mat"] = {m: v / k for m, v in size.items()}
    r["size"] = sum(size.values()) / k
    return r


def on_segment(p, a, b):
    if area2(a, b, p) != 0:
        # allow the dump's 13 digits: relative to the edge length squared
        L2 = (b[0] - a[0]) ** 2 + (b[1] - a[1]) ** 2
        if abs(area2(a, b, p)) ** 2 > F(1, 10 ** 20) * L2 * L2:
            return False
    d = (p[0] - a[0]) * (b[0] - a[0]) + (p[1] - a[1]) * (b[1] - a[1])
    return 0 <= d <= (b[0] - a[0]) ** 2 + (b[1] - a[1]) ** 2


def on_triangle(p, a, b, c):
    n = [x for x in (lambda u, v: [u[1] * v[2] - u[2] * v[1], u[2] * v[0] - u[0] * v[2],
                                   u[0] * v[1] - u[1] * v[0]])(sub(b, a), sub(c, a))]
    nn = n[0] ** 2 + n[1] ** 2 + n[2] ** 2
    if nn == 0:
        return False
    h = sum(n[i] * (p[i] - a[i]) for i in range(3))
    if h * h > F(1, 10 ** 20) * nn * max(sum((b[i] - a[i]) ** 2 for i in range(3)), 1e-300):
        return False
    # barycentric signs in the plane
    def side(x, y):
        u, v = sub(y, x), sub(p, x)
        w = [u[1] * v[2] - u[2] * v[1], u[2] * v[0] - u[0] * v[2], u[0] * v[1] - u[1] * v[0]]
        return sum(w[i] * n[i] for i in range(3))
    s = [side(a, b), side(b, c), side(c, a)]
    tol = -F(1, 10 ** 10) * nn
    return all(x >= tol for x in s)


def keyset(Vf, items):
    return set(tuple(sorted(Vf[k] for k in it)) for it in items)


def judge(name, e, work):
    """(hard failures, known failures, known passes, steps measured)"""
    dim = e["dim"]
    out = os.path.join(work, name + ".out")
    text = open(out, errors="replace").read() if os.path.isfile(out) else ""
    finished = "LaGriT successfully completed" in text
    hard, kfail, kpass = [], [], []
    known = e.get("known", {})
    kall = [c for v in known.values() for c in v]
    if not finished:
        (kfail if "finished" in kall else hard).append("finished: LaGriT did not finish the deck")
    elif "finished" in kall:
        kpass.append("finished")
    if "clean_output" in kall:
        bad = [l.strip() for l in text.split("\n") if "zero terminated parent/child chain" in l]
        (kfail if bad else kpass).append("clean_output: %d 'zero terminated parent/child chain' messages" % len(bad))
    for t in e.get("has", []):
        if t not in text:
            hard.append('has: no "%s" in the output' % t)
    M = {}
    for s in e["steps"]:
        fn = os.path.join(work, "%s_%s.inp" % (name, s))
        if os.path.isfile(fn):
            M[s] = measure(fn, dim)
    stray = os.path.isfile(os.path.join(work, name + ".gmv_refine_edge_3d"))
    steps = e["steps"]
    for s in steps[1:]:
        checks = e.get("after", {}).get(s, []) + known.get(s, [])
        if s not in M:
            if not finished and "finished" in kall:
                continue
            hard.append("step %s: no dump" % s)
            continue
        a, b = M[steps[steps.index(s) - 1]], M[s]
        for c in checks:
            if c in ("finished", "clean_output"):
                continue
            ok, why = True, ""
            if c == "valid":
                ok = b["inverted"] == 0 and b["overfull"] == 0
                why = "%d inverted or flat, %d faces in 3+ cells" % (b["inverted"], b["overfull"])
            elif c in ("delaunay", "delaunay_in_materials"):
                ok, why = b["nonDelaunay"] == 0, "%d not Delaunay" % b["nonDelaunay"]
            elif c == "centres_inside":
                ok, why = b["centres_out"] == 0, "%d Voronoi centres outside the domain" % b["centres_out"]
            elif c == "fewer_centres_out":
                ok = b["centres_out"] < a["centres_out"]
                why = "%d -> %d Voronoi centres outside the domain" % (a["centres_out"], b["centres_out"])
            elif c == "no_obtuse_boundary":
                ok, why = b["obtuse_boundary"] == 0, "%d obtuse at the boundary" % b["obtuse_boundary"]
            elif c == "same_vertices":
                ok = set(a["Vf"]) == set(b["Vf"])
                why = "%d -> %d vertices" % (len(a["Vf"]), len(b["Vf"]))
            elif c == "same_boundary":
                ok = keyset(a["Vf"], a["bnd"]) == keyset(b["Vf"], b["bnd"])
                why = "%d -> %d boundary %s" % (len(a["bnd"]), len(b["bnd"]), "edges" if dim == 2 else "faces")
            elif c == "same_size":
                ok = abs(a["size"] - b["size"]) <= F(1, 10 ** 12) * a["size"]
                why = "%.15g -> %.15g" % (float(a["size"]), float(b["size"]))
            elif c == "unchanged":
                ok = (set(a["Vf"]) == set(b["Vf"]) and
                      keyset(a["Vf"], a["C"]) == keyset(b["Vf"], b["C"]))
                why = "%d -> %d cells" % (len(a["C"]), len(b["C"]))
            elif c in ("added_on_boundary", "added_on_boundary_or_interface"):
                old = set(a["Vf"])
                new = [p for p in b["Vf"] if p not in old]
                where = a["bnd"] + (a["iface"] if c.endswith("interface") else [])
                off = 0
                for p in new:
                    if dim == 2:
                        hit = any(on_segment(p, a["Vf"][f[0]], a["Vf"][f[1]]) for f in where)
                    else:
                        hit = any(on_triangle(p, *(a["Vf"][k] for k in f)) for f in where)
                    off += not hit
                ok, why = off == 0, "%d added, %d of them elsewhere" % (len(new), off)
            elif c == "same_materials":
                ok = all(abs(a["by_mat"].get(m, 0) - b["by_mat"].get(m, 0)) <= F(1, 10 ** 9) * a["size"]
                         for m in set(a["by_mat"]) | set(b["by_mat"]))
                why = ", ".join("material %d: %.12g -> %.12g" % (m, float(a["by_mat"].get(m, 0)),
                                float(b["by_mat"].get(m, 0))) for m in sorted(set(a["by_mat"]) | set(b["by_mat"])))
            elif c == "interface_kept":
                # every interface edge after lies along an interface edge before
                off = 0
                for f in b["iface"]:
                    p, q = b["Vf"][f[0]], b["Vf"][f[1]]
                    if not any(on_segment(p, a["Vf"][g[0]], a["Vf"][g[1]]) and
                               on_segment(q, a["Vf"][g[0]], a["Vf"][g[1]]) for g in a["iface"]):
                        off += 1
                ok, why = off == 0, "%d -> %d interface edges, %d not along one before" % (len(a["iface"]), len(b["iface"]), off)
            elif c == "fewer_nondelaunay":
                ok = b["nonDelaunay"] < a["nonDelaunay"]
                why = "%d -> %d not Delaunay" % (a["nonDelaunay"], b["nonDelaunay"])
            elif c == "fewer_obtuse_boundary":
                ok = b["obtuse_boundary"] < a["obtuse_boundary"]
                why = "%d -> %d obtuse boundary angles" % (a["obtuse_boundary"], b["obtuse_boundary"])
            elif c == "no_stray_file":
                ok, why = not stray, "recon wrote gmv_refine_edge_3d" if stray else "no file written"
            else:
                ok, why = False, "unknown check"
            line = "step %s %s: %s" % (s, c, why)
            if c in known.get(s, []):
                (kpass if ok else kfail).append(line)
            elif not ok:
                hard.append(line)
    return hard, kfail, kpass, M, text


ABOUT_PIC = ("2D: the mesh after each step; red = an edge that is not Delaunay, orange = a boundary edge whose "
             "opposite angle is obtuse, blue = an interface edge, green dots = vertices recon added. 3D: an oblique "
             "view of the boundary edges (grey), with the vertices recon added (green) and the faces that are not "
             "Delaunay (red edges).")


def picture(name, s, r, prev, dim):
    old = set(prev["Vf"]) if prev else set()
    segs, pts = [], []
    V = r["V"]
    bad = set(r["bad"])
    obt = set(r["obt"])
    ifc = set(r["iface"])
    if dim == 2:
        drawn = set()
        for f in r["S"]:
            a, b = V[f[0]], V[f[1]]
            col, w = "#c8ced6", 0.6
            if f in ifc:
                col, w = "#0969da", 1.4
            if f in obt:
                col, w = "#fb8500", 2.0
            if f in bad:
                col, w = "#cf222e", 1.6
            segs.append((a[0], a[1], b[0], b[1], col, w))
            drawn.add(f)
        P = lambda p: (p[0], p[1])
    else:
        P = lambda p: (p[0] + 0.42 * p[2] + 0.1 * p[1], p[1] + 0.32 * p[2] - 0.06 * p[0])
        for f in r["bnd"]:
            for x, y in ((f[0], f[1]), (f[1], f[2]), (f[0], f[2])):
                A, B = P(V[x]), P(V[y])
                segs.append((A[0], A[1], B[0], B[1], "#c8ced6", 0.5))
        for f in r["bad"]:
            for x, y in ((f[0], f[1]), (f[1], f[2]), (f[0], f[2])):
                A, B = P(V[x]), P(V[y])
                segs.append((A[0], A[1], B[0], B[1], "#cf222e", 0.8))
    segs.sort(key=lambda q: q[5])
    nadd = 0
    if prev:
        for p, pf in zip(V, r["Vf"]):
            if pf not in old:
                nadd += 1
                X = P(p)
                pts.append((X[0], X[1], "#1a7f37", 2.4, ""))
    note = "%d cells, %d not Delaunay, %d centres outside%s" % (
        len(r["C"]), r["nonDelaunay"], r["centres_out"], ", %d added" % nadd if prev else "")
    title = "before" if not prev else "after step %s" % s
    w, hgt = 300, 270 if dim == 2 else 300
    if dim == 2:
        xs, ys = [p[0] for p in V], [p[1] for p in V]
        asp = (max(xs) - min(xs)) / max(max(ys) - min(ys), 1e-300)
        if asp > 3:                          # a long domain: one wide picture per row
            w, hgt = 1150, int(max(150, min(270, 1150 / asp + 70)))
    return svg_plot2d(title, segs, pts, (), w, hgt, pad=0.02, note=note)


# ---- small SVG helpers (standard library only; copied into each checker) ----
def svg_bars(title, labels, series, colors, width=560, height=220, logy=False, unit=""):
    """grouped bar chart: labels on x, series = [(name, [values])]"""
    import math
    E2 = lambda s: str(s).replace("&", "&amp;").replace("<", "&lt;").replace(">", "&gt;")
    L, R, T, B = 48, 12, 28, 58
    W, H = width - L - R, height - T - B
    vals = [v for _, vs in series for v in vs if v is not None]
    vmax = max([v for v in vals] + [1e-30])
    if logy:
        pos = [v for v in vals if v and v > 0]
        lo = 10 ** math.floor(math.log10(min(pos))) if pos else 1.0
        hi = 10 ** math.ceil(math.log10(vmax)) if vmax > 0 else 10.0
        if hi <= lo: hi = lo * 10
        def Y(v):
            if not v or v <= 0: return T + H
            return T + H - H * (math.log10(v) - math.log10(lo)) / (math.log10(hi) - math.log10(lo))
        ticks = [10 ** k for k in range(int(round(math.log10(lo))), int(round(math.log10(hi))) + 1)]
    else:
        hi = vmax * 1.1 if vmax > 0 else 1.0
        def Y(v): return T + H - H * (v or 0) / hi
        step = 10 ** math.floor(math.log10(hi)) if hi > 0 else 1
        while hi / step > 6: step *= 2
        ticks = [k * step for k in range(0, int(hi / step) + 1)]
    o = ['<svg viewBox="0 0 %d %d" width="%d" style="max-width:100%%;height:auto;font:11px sans-serif">' % (width, height, width),
         '<text x="%d" y="16" font-weight="600">%s</text>' % (L, E2(title))]
    for t in ticks:
        y = Y(t)
        o.append('<line x1="%d" x2="%d" y1="%.1f" y2="%.1f" stroke="#d0d7de"/>' % (L, L + W, y, y))
        o.append('<text x="%d" y="%.1f" text-anchor="end" fill="#57606a">%s</text>' % (L - 4, y + 4, ("%g" % t) + unit))
    n, k = len(labels), len(series)
    gw = W / max(n, 1)
    bw = gw * 0.8 / max(k, 1)
    for i, lab in enumerate(labels):
        for j, (name, vs) in enumerate(series):
            v = vs[i]
            x = L + i * gw + gw * 0.1 + j * bw
            y = Y(v)
            o.append('<rect x="%.1f" y="%.1f" width="%.1f" height="%.1f" fill="%s"><title>%s: %s</title></rect>'
                     % (x, y, bw - 1, T + H - y, colors[j], E2(name), E2(v)))
        o.append('<text x="%.1f" y="%d" text-anchor="end" transform="rotate(-30 %.1f %d)">%s</text>'
                 % (L + i * gw + gw / 2, T + H + 14, L + i * gw + gw / 2, T + H + 14, E2(lab)))
    for j, (name, _) in enumerate(series):
        o.append('<rect x="%d" y="%d" width="10" height="10" fill="%s"/><text x="%d" y="%d">%s</text>'
                 % (L + W - 150, 6 + 13 * j, colors[j], L + W - 136, 15 + 13 * j, E2(name)))
    o.append("</svg>")
    return "\n".join(o)


def svg_plot2d(title, segs=(), pts=(), circles=(), width=260, height=260, pad=0.08, note=""):
    """segs: [(x1,y1,x2,y2,color,widthpx)], pts: [(x,y,color,rpx,label)],
    circles: [(cx,cy,r,color)]; equal axes, fitted to everything"""
    E2 = lambda s: str(s).replace("&", "&amp;").replace("<", "&lt;").replace(">", "&gt;")
    xs = [s[0] for s in segs] + [s[2] for s in segs] + [p[0] for p in pts] + \
         [c[0] - c[2] for c in circles] + [c[0] + c[2] for c in circles]
    ys = [s[1] for s in segs] + [s[3] for s in segs] + [p[1] for p in pts] + \
         [c[1] - c[2] for c in circles] + [c[1] + c[2] for c in circles]
    if not xs:
        xs, ys = [0, 1], [0, 1]
    x0, x1, y0, y1 = min(xs), max(xs), min(ys), max(ys)
    cx, cy = (x0 + x1) / 2, (y0 + y1) / 2
    T = 22
    # fit both ways (a long domain fills a wide picture)
    s = min(width * (1 - 2 * pad) / ((x1 - x0) or 1.0), (height - T - 14) * (1 - 2 * pad) / ((y1 - y0) or 1.0))
    def P(x, y):
        return width / 2 + (x - cx) * s, T + (height - T) / 2 - (y - cy) * s
    o = ['<svg viewBox="0 0 %d %d" width="%d" style="max-width:100%%;height:auto;font:10px sans-serif;background:#fff">'
         % (width, height, width), '<text x="4" y="14" font-weight="600">%s</text>' % E2(title)]
    for (x, y, r, col) in circles:
        X, Y = P(x, y)
        o.append('<circle cx="%.1f" cy="%.1f" r="%.1f" fill="none" stroke="%s" stroke-dasharray="3 2"/>' % (X, Y, r * s, col))
    for (xa, ya, xb, yb, col, w) in segs:
        A, B = P(xa, ya), P(xb, yb)
        o.append('<line x1="%.1f" y1="%.1f" x2="%.1f" y2="%.1f" stroke="%s" stroke-width="%s"/>' % (A[0], A[1], B[0], B[1], col, w))
    for p in pts:
        X, Y = P(p[0], p[1])
        o.append('<circle cx="%.1f" cy="%.1f" r="%s" fill="%s"/>' % (X, Y, p[3], p[2]))
        if len(p) > 4 and p[4]:
            o.append('<text x="%.1f" y="%.1f">%s</text>' % (X + 4, Y - 3, E2(p[4])))
    if note:
        o.append('<text x="4" y="%d" fill="#57606a">%s</text>' % (height - 4, E2(note)))
    o.append("</svg>")
    return "\n".join(o)


def main(work, logdir):
    exp = json.load(open(os.path.join(work, "expect.json")))
    rows, nbad = [], 0
    for name, e in exp.items():
        try:
            hard, kfail, kpass, M, text = judge(name, e, work)
        except (OSError, ValueError, IndexError, KeyError, ZeroDivisionError) as x:
            hard, kfail, kpass, M, text = ["output not readable (%s: %s)" % (type(x).__name__, x)], [], [], {}, ""
        if hard:
            res = "FAIL"
        elif kpass:
            res = "FIXED"
        elif kfail:
            res = "KNOWN"
        else:
            res = "PASS"
        nbad += res in ("FAIL", "FIXED")
        with open(os.path.join(logdir, name + ".check"), "w") as f:
            f.write("result %s\n" % res)
            for x in hard:
                f.write("fail %s\n" % x)
            for x in kfail:
                f.write("known %s\n" % x)
            for x in kpass:
                f.write("fixed %s\n" % x)
        print("  %-5s  %s%s" % (res, name, ("  " + "; ".join((hard + kpass)[:3])) if hard or kpass else ""))
        pics, prev = [], None
        for s in e["steps"]:
            if s in M:
                pics.append(picture(name, s, M[s], prev, e["dim"]))
                prev = M[s]
        counts = [(s, M[s]["nonDelaunay"], M[s]["centres_out"], M[s]["obtuse_boundary"], len(M[s]["Vf"]), len(M[s]["C"]))
                  for s in e["steps"] if s in M]
        msgs = [l.rstrip() for l in text.split("\n") if l.startswith(("WARNING", "Refine on", " zero terminated",
                "in fnd2to3i", "ERROR", "Recon2d: iteration", " recon2:", "Edge iteration", "Edge_add"))][:12]
        rows.append((name, res, e, hard, kfail, kpass, pics, counts, msgs))
    with open(os.path.join(logdir, "table.txt"), "w") as f:
        for r in rows:
            f.write("%-20s %s\n" % (r[0], r[1]))
    o = ["""<!doctype html><html><head><meta charset="utf-8"><title>test_recon</title>
<style>body{font-family:-apple-system,Helvetica,Arial,sans-serif;margin:16px;max-width:1250px;color:#1f2328;background:#fff}
p,li{font-size:14px;line-height:1.45;max-width:1050px}table{border-collapse:collapse;font-size:12.5px}
td,th{border-bottom:1px solid #d0d7de;padding:4px 7px;text-align:left;vertical-align:top}th{background:#f6f8fa}
.PASS{color:#1a7f37;font-weight:600}.FAIL,.FIXED{color:#cf222e;font-weight:600}.KNOWN{color:#9a6700;font-weight:600}
pre{font-size:11px;background:#f6f8fa;padding:6px;margin:4px 0;white-space:pre-wrap}.pics{display:flex;flex-wrap:wrap;gap:8px}
h2{font-size:16px;margin:26px 0 4px}</style></head><body>
<p style="margin:0 0 8px;font-size:13px"><a href="../../summary.html">&larr; all suites (top-level summary)</a></p>
<h1 style="font-size:20px">recon: reconnection by flips, and recon / 1</h1>
<p><b>What it does:</b> each deck makes or reads a 2D or 3D mesh, dumps it, runs <code>recon / 0</code> (flip
edges or faces toward Delaunay, add nothing) or <code>recon / 1</code> (also add vertices where the boundary has
an obtuse angle: in 2D, refine_edge_2d after each round of flips until every Voronoi coefficient is positive; in
3D, one pass of refine_edge_3d and flips), and dumps the result. The checker compares each mesh with the one before
it, in exact arithmetic. KNOWN = a documented problem, not yet fixed; FIXED = it now passes, so the entry should
change.</p>
<p>%s</p>
<table><tr><th>deck</th><th>result</th><th>what it does</th><th>per step: not Delaunay / Voronoi centres outside / obtuse at the boundary / vertices / cells</th><th>findings</th></tr>""" % E(ABOUT_PIC)]
    for name, res, e, hard, kfail, kpass, pics, counts, msgs in rows:
        cnt = "<br>".join("%s: %d / %d / %d / %d / %d" % c for c in counts) or "-"
        fnd = "".join('<div class="FAIL">%s</div>' % E(x) for x in hard + kpass) + \
            "".join('<div class="KNOWN">%s</div>' % E(x) for x in kfail)
        o.append('<tr><td><a href="#%s">%s</a></td><td class="%s">%s</td><td style="max-width:420px">%s</td><td>%s</td><td>%s</td></tr>'
                 % (name, name, res, res, E(e["about"]), cnt, fnd or "-"))
    o.append("</table>")
    for name, res, e, hard, kfail, kpass, pics, counts, msgs in rows:
        o.append('<h2 id="%s">%s <span class="%s">%s</span></h2><p>%s</p>' % (name, name, res, res, E(e["about"])))
        if pics:
            o.append('<div class="pics">%s</div>' % "".join(pics))
        if msgs:
            o.append("<pre>%s</pre>" % E("\n".join(msgs)))
    o.append("</body></html>")
    open(os.path.join(logdir, "summary.html"), "w").write("\n".join(o) + "\n")
    n = sum(r[1] in ("PASS", "KNOWN") for r in rows)
    print("\npass or known %d   fail %d" % (n, len(rows) - n))
    return 1 if nbad else 0


if __name__ == "__main__":
    sys.exit(main(sys.argv[1], sys.argv[2]))
