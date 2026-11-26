#!/usr/bin/env python3
"""check_extract_surfmesh.py WORKDIR LOGDIR

Judges extract / surfmesh / 1,0,0 / mo_s / mo / external (Python 3,
standard library only). Each deck dumps its source mesh (NAME_in.inp)
and the extracted boundary (NAME_out.inp); its header holds

  # check : NAME | 2d or 3d | MEASURE

A deck PASSES when LaGriT finished, and
  - every vertex of the boundary is the source vertex its attribute
    idnode0 names, with the same x, y and z (GitHub #4: z was set to
    0 for a 2D mesh not in the x-y plane);
  - the boundary of a 2d mesh is line cells, of a 3d mesh triangles,
    and their total length (or area) is MEASURE, to 1e-12;
  - every source vertex on the boundary of the domain (on the edge of
    its bounding box) is a vertex of the boundary mesh.
Each picture shows the source mesh and, over it, the boundary that was
extracted. Writes LOGDIR/<deck>.check, LOGDIR/table.txt and
LOGDIR/summary.html.
"""
import glob
import html
import math
import os
import sys

HERE = os.path.dirname(os.path.abspath(__file__))
E = html.escape


def read_avs(p):
    """vertices [(x,y,z)], cells [(type, [ids 0-based])], vertex
    attributes {name: [values]}"""
    L = [l for l in open(p).read().split("\n") if not l.startswith("#")]
    nn, nc, nnd = map(int, L[0].split()[:3])
    V = [tuple(map(float, L[1 + i].split()[1:4])) for i in range(nn)]
    C = []
    for i in range(nc):
        w = L[1 + nn + i].split()
        C.append((w[2].lower(), [int(x) - 1 for x in w[3:]]))
    att = {}
    if nnd:
        k = 1 + nn + nc
        w = L[k].split()
        na = int(w[0])
        names = [L[k + 1 + j].split(",")[0].strip() for j in range(na)]
        rows = [L[k + 1 + na + i].split()[1:] for i in range(nn)]
        for j, n in enumerate(names):
            att[n] = [float(r[j]) for r in rows]
    return V, C, att


def measure(V, C):
    s = 0.0
    for t, c in C:
        p = [V[i] for i in c]
        if t == "line":
            s += math.dist(p[0], p[1])
        elif t == "tri":
            u = [p[1][k] - p[0][k] for k in range(3)]
            v = [p[2][k] - p[0][k] for k in range(3)]
            w = (u[1] * v[2] - u[2] * v[1], u[2] * v[0] - u[0] * v[2], u[0] * v[1] - u[1] * v[0])
            s += 0.5 * math.sqrt(sum(x * x for x in w))
    return s


def header(deck):
    for l in open(deck):
        if l.startswith("# check"):
            name, dim, m = [x.strip() for x in l.split(":", 1)[1].split("|")]
            return name, dim, float(m)
    raise SystemExit("%s: no '# check' line" % deck)


def about(deck):
    out = []
    for l in open(deck).read().split("\n")[3:]:
        if l.startswith("# check") or not l.startswith("#"):
            break
        out.append(l[1:].strip())
    return " ".join(x for x in out if x)


def picture(V, C, B, path, title):
    """the source mesh (grey) and the boundary (red, thick), seen down
    the axis along which the source is thinnest; for 3d an oblique
    view"""
    xs = [[v[k] for v in V] for k in range(3)]
    span = [max(a) - min(a) for a in xs]
    if min(span) < 1e-12:                    # a plane: drop that axis
        k0 = span.index(min(span))
        ax = [k for k in range(3) if k != k0]
        proj = lambda p: (p[ax[0]], p[ax[1]])
        labels = "xyz"[ax[0]], "xyz"[ax[1]]
    else:
        proj = lambda p: (p[0] + 0.45 * p[1], p[2] + 0.3 * p[1])
        labels = "x (+0.45 y)", "z (+0.3 y)"
    P = [proj(v) for v in V]
    QB = [proj(v) for v in B[0]]
    x0, x1 = min(p[0] for p in P + QB), max(p[0] for p in P + QB)
    y0, y1 = min(p[1] for p in P + QB), max(p[1] for p in P + QB)
    W, H, m = 380, 260, 30
    s = min((W - 2 * m) / max(x1 - x0, 1e-30), (H - 2 * m - 20) / max(y1 - y0, 1e-30))
    X = lambda p: m + (p[0] - x0) * s
    Y = lambda p: H - m - (p[1] - y0) * s
    o = ['<svg xmlns="http://www.w3.org/2000/svg" width="%d" height="%d">' % (W, H),
         '<rect width="100%" height="100%" fill="white"/>',
         '<text x="8" y="16" font-size="12" font-family="sans-serif">%s</text>' % E(title)]
    for t, c in C:
        pts = [P[i] for i in c]
        edges = [(pts[a], pts[b]) for a in range(len(pts)) for b in range(a + 1, len(pts))] \
            if t == "tet" else list(zip(pts, pts[1:] + pts[:1]))
        for a, b in edges:
            o.append('<line x1="%.1f" y1="%.1f" x2="%.1f" y2="%.1f" stroke="#bbb" stroke-width="0.8"/>'
                     % (X(a), Y(a), X(b), Y(b)))
    for t, c in B[1]:
        pts = [QB[i] for i in c]
        for a, b in (zip(pts, pts[1:] + pts[:1]) if len(pts) > 2 else [(pts[0], pts[1])]):
            o.append('<line x1="%.1f" y1="%.1f" x2="%.1f" y2="%.1f" stroke="#cf222e" stroke-width="%s"/>'
                     % (X(a), Y(a), X(b), Y(b), "2.2" if len(pts) == 2 else "1.0"))
    for q in QB:
        o.append('<circle cx="%.1f" cy="%.1f" r="2.5" fill="#cf222e"/>' % (X(q), Y(q)))
    o.append('<text x="%d" y="%d" font-size="11" font-family="sans-serif">%s right, %s up; grey: '
             'the mesh, red: the boundary extracted</text>' % (8, H - 8, labels[0], labels[1]))
    o.append("</svg>")
    open(path, "w").write("\n".join(o) + "\n")


def main(work, logdir):
    rows, ok_all = [], True
    for deck in sorted(glob.glob(os.path.join(HERE, "es[0-9]*.lgi"))):
        d = os.path.basename(deck)[:-4]
        name, dim, want = header(deck)
        out = os.path.join(work, d + ".out")
        text = open(out, errors="replace").read() if os.path.isfile(out) else ""
        fails, what, pic = [], "", ""
        if "LaGriT successfully completed" not in text:
            fails.append("LaGriT did not finish")
        try:
            V, C, _ = read_avs(os.path.join(work, name + "_in.inp"))
            BV, BC, batt = read_avs(os.path.join(work, name + "_out.inp"))
        except Exception as e:
            fails.append("cannot read %s_in.inp or %s_out.inp: %s" % (name, name, e))
            V = C = BV = BC = []
            batt = {}
        if BV:
            ids = batt.get("idnode0")
            if ids is None:
                fails.append("no attribute idnode0 in the boundary mesh")
            else:
                bad = sum(1 for v, i in zip(BV, ids) if V[int(i) - 1] != v)
                if bad:
                    fails.append("%d boundary vertices not at their source vertex" % bad)
            ctype = "line" if dim == "2d" else "tri"
            other = [t for t, c in BC if t != ctype]
            if other:
                fails.append("%d cells are not %s" % (len(other), ctype))
            m = measure(BV, BC)
            if abs(m - want) > 1e-12 * max(1.0, want):
                fails.append("total %s %.15g, expected %g" % ("length" if dim == "2d" else "area", m, want))
            lo = [min(v[k] for v in V) for k in range(3)]
            hi = [max(v[k] for v in V) for k in range(3)]
            onbox = [v for v in V if any((hi[k] > lo[k]) and (v[k] in (lo[k], hi[k])) for k in range(3))]
            miss = sum(1 for v in onbox if v not in set(BV))
            if miss:
                fails.append("%d vertices on the domain boundary are not in the boundary mesh" % miss)
            what = "%d %s cells, %d vertices; total %s %.12g" % (
                len(BC), ctype, len(BV), "length" if dim == "2d" else "area", m)
            try:
                svg = d + ".svg"
                picture(V, C, (BV, BC), os.path.join(logdir, svg), "%s: the mesh and its boundary" % d)
                pic = '<a href="%s"><img src="%s" width="300"></a>' % (svg, svg)
            except Exception as e:
                pic = "(no picture: %s)" % E(str(e))
        res = "PASS" if not fails else "FAIL"
        ok_all &= not fails
        with open(os.path.join(logdir, d + ".check"), "w") as f:
            f.write("result %s\n" % res)
            f.writelines("fail %s\n" % x for x in fails)
        rows.append((d, res, fails, what, pic, about(deck)))
        print("  %-4s  %s%s" % (res, d, ("  " + "; ".join(fails[:3])) if fails else ""))
    with open(os.path.join(logdir, "table.txt"), "w") as f:
        f.writelines("%s %s\n" % (r[0], r[1]) for r in rows)
    o = ["""<!doctype html><html><head><meta charset="utf-8">
<meta name="viewport" content="width=device-width, initial-scale=1"><title>extract / surfmesh</title>
<style>body{font-family:-apple-system,Helvetica,Arial,sans-serif;margin:16px;max-width:1100px;color:#1f2328;background:#fff}
p,li{font-size:14px;line-height:1.45}table{border-collapse:collapse;font-size:12px}td,th{border-bottom:1px solid #d0d7de;
padding:3px 6px;text-align:left;vertical-align:top}th{background:#f6f8fa}.PASS{color:#1a7f37;font-weight:600}
.FAIL{color:#cf222e;font-weight:600}</style></head><body>
<p style="margin:0 0 8px;font-size:13px"><a href="../../summary.html">&larr; all suites (top-level summary)</a></p>
<h1 style="font-size:20px">extract / surfmesh: the boundary of a mesh</h1>
<p>extract / surfmesh / 1,0,0 / mo_s / mo / external makes the boundary of mo: lines for a triangle mesh, triangles for a
tet mesh. A triangle mesh may lie in any coordinate plane; in the x-z and y-z planes the boundary got z = 0 (GitHub #4).</p>
<p><b>What it does:</b> each deck dumps the mesh and its boundary; the checker finds every boundary vertex at the source
vertex its attribute idnode0 names, adds up the boundary's length (or area) and compares it with the domain's, and checks
that every vertex on the edge of the domain is on the boundary. Each picture shows the mesh (grey) and the boundary (red).</p>
<table><tr><th>deck</th><th>result</th><th>boundary</th><th>picture</th></tr>"""]
    for d, r, fails, what, pic, ab in rows:
        o.append('<tr><td>%s<details><summary>What it does</summary>%s</details></td><td class="%s">%s</td><td>%s%s</td><td>%s</td></tr>'
                 % (E(d), E(ab), r, r, E(what), "".join('<div class="FAIL">%s</div>' % E(x) for x in fails), pic))
    o.append("</table></body></html>")
    open(os.path.join(logdir, "summary.html"), "w").write("\n".join(o) + "\n")
    n = sum(r[1] == "PASS" for r in rows)
    print("\npassed %d   failed %d" % (n, len(rows) - n))
    return 0 if ok_all else 1


if __name__ == "__main__":
    if len(sys.argv) != 3:
        sys.exit(__doc__)
    sys.exit(main(sys.argv[1], sys.argv[2]))
