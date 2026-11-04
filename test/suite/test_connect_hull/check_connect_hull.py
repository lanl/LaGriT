#!/usr/bin/env python3
"""check_connect_hull.py WORKDIR LOGDIR

Judges test_connect_hull (Python 3, standard library only). For each
mesh, against the input coordinates (exact rationals where floating
point cannot decide):
  valid     every tet positively oriented (exactly), no face in more
            than two tets
  missing   the share of the convex hull's volume (computed here with
            an incremental hull) not covered by the tets: 0 when the
            mesh fills the hull; about 1e-7 when only pockets flatter
            than 1e-6 of their size are left open
  open      boundary faces with a point outside (pockets left)
  nonDel    (connect / exact) interior faces not locally Delaunay
A deck PASSES when the mesh is valid and, with the fill (plain connect
and exact), missing <= 1e-6; with exact also nonDel = 0. nohull decks are
the baseline (valid only).
Writes LOGDIR/<deck>.check, LOGDIR/table.txt and LOGDIR/summary.html.
"""
import html
import json
import math
import os
import sys
from fractions import Fraction as Fr

E = html.escape

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
    span = max(x1 - x0, y1 - y0) or 1.0
    cx, cy = (x0 + x1) / 2, (y0 + y1) / 2
    T = 22
    s = (min(width, height - T) * (1 - 2 * pad)) / span
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



def read_avs(fn):
    with open(fn) as f:
        h = f.readline().split()
        n, ne = int(h[0]), int(h[1])
        X = [tuple(float(v) for v in f.readline().split()[1:4]) for _ in range(n)]
        T = [tuple(int(v) - 1 for v in f.readline().split()[3:7]) for _ in range(ne)]
    return X, T


def det3(m):
    return (m[0][0] * (m[1][1] * m[2][2] - m[1][2] * m[2][1]) - m[0][1] * (m[1][0] * m[2][2] - m[1][2] * m[2][0])
            + m[0][2] * (m[1][0] * m[2][1] - m[1][1] * m[2][0]))


def orient(a, b, c, d):
    return det3([[b[i] - a[i] for i in range(3)], [c[i] - a[i] for i in range(3)], [d[i] - a[i] for i in range(3)]])


def insphere(a, b, c, d, e):
    rows = []
    for p in (a, b, c, d):
        w = [p[i] - e[i] for i in range(3)]
        rows.append(w + [w[0] * w[0] + w[1] * w[1] + w[2] * w[2]])
    s = 0
    for j in range(4):
        s += (-1) ** j * rows[0][j] * det3([[rows[r][k] for k in range(4) if k != j] for r in range(1, 4)])
    o = orient(a, b, c, d)
    return 0 if o == 0 else (-s if o > 0 else s)



def hull_volume(P):
    """volume of the convex hull of the points P (incremental hull; the
    visibility tests exact, in rationals, when floating point is close)"""
    PF = {}

    def pf(i):
        if i not in PF:
            PF[i] = tuple(Fr(v) for v in P[i])
        return PF[i]

    def side(f, q):
        a, b, c = f
        o = orient(P[a], P[b], P[c], P[q])
        h = max(abs(P[x][k] - P[q][k]) for x in f for k in range(3))
        if abs(o) > 1e-9 * h ** 3:
            return 1 if o > 0 else -1
        o = orient(pf(a), pf(b), pf(c), pf(q))
        return (o > 0) - (o < 0)

    n = len(P)
    i0 = 0
    i1 = max(range(n), key=lambda i: math.dist(P[i], P[i0]))
    best, i2 = -1.0, None
    for i in range(n):
        ab = [P[i1][k] - P[i0][k] for k in range(3)]
        ac = [P[i][k] - P[i0][k] for k in range(3)]
        cr = [ab[1] * ac[2] - ab[2] * ac[1], ab[2] * ac[0] - ab[0] * ac[2], ab[0] * ac[1] - ab[1] * ac[0]]
        v = sum(x * x for x in cr)
        if v > best:
            best, i2 = v, i
    best, i3 = -1.0, None
    for i in range(n):
        v = abs(orient(P[i0], P[i1], P[i2], P[i]))
        if v > best:
            best, i3 = v, i
    faces = [(i0, i1, i2), (i0, i1, i3), (i0, i2, i3), (i1, i2, i3)]
    cen = [sum(P[i][k] for i in (i0, i1, i2, i3)) / 4 for k in range(3)]
    fixed = []
    for f in faces:
        a, b, c = f
        if orient(P[a], P[b], P[c], cen) > 0:   # make the centre inside: orient < 0
            f = (a, c, b)
        fixed.append(f)
    faces = set(fixed)
    for q in range(n):
        if q in (i0, i1, i2, i3):
            continue
        vis = [f for f in faces if side(f, q) > 0]
        if not vis:
            continue
        edges = {}
        for f in vis:
            for e in ((f[0], f[1]), (f[1], f[2]), (f[2], f[0])):
                edges[e] = edges.get(e, 0) + 1
        for f in vis:
            faces.discard(f)
        for (a, b), _ in list(edges.items()):
            if (b, a) not in edges:
                faces.add((a, b, q))
    vol = 0.0
    for a, b, c in faces:
        vol += -orient(P[a], P[b], P[c], cen) / 6.0
    return abs(vol)

def check(X, T, exact):
    XF = {}

    def xf(i):
        if i not in XF:
            XF[i] = tuple(Fr(v) for v in X[i])
        return XF[i]

    vol = [orient(*(X[i] for i in t)) for t in T]
    scale = max(abs(v) for v in vol)
    pos = sum(v > 0 for v in vol)
    sgn0 = 1 if pos >= len(vol) - pos else -1      # LaGriT's orientation
    bad = 0
    for t, v in zip(T, vol):
        if v * sgn0 > 1e-9 * scale:
            continue
        if orient(*(xf(i) for i in t)) * sgn0 <= 0:
            bad += 1
    faces = {}
    for k, t in enumerate(T):
        for j in range(4):
            faces.setdefault(frozenset(t[:j] + t[j + 1:]), []).append((k, t[j]))
    over = sum(1 for L in faces.values() if len(L) > 2)
    # gap: points outside boundary faces
    gap, nopen = 0.0, 0
    openf = []
    used = sorted(set(i for t in T for i in t))
    diag = math.dist([min(X[i][k] for i in used) for k in range(3)],
                     [max(X[i][k] for i in used) for k in range(3)])
    for f, L in faces.items():
        if len(L) != 1:
            continue
        k, w = L[0]
        a, b, c = [X[i] for i in f]
        nrm = [(b[1] - a[1]) * (c[2] - a[2]) - (b[2] - a[2]) * (c[1] - a[1]),
               (b[2] - a[2]) * (c[0] - a[0]) - (b[0] - a[0]) * (c[2] - a[2]),
               (b[0] - a[0]) * (c[1] - a[1]) - (b[1] - a[1]) * (c[0] - a[0])]
        nl = math.sqrt(sum(x * x for x in nrm))
        if nl == 0:
            continue
        sgn = -1.0 if sum(nrm[i] * (X[w][i] - a[i]) for i in range(3)) > 0 else 1.0
        L2 = max(math.dist(a, b), math.dist(b, c), math.dist(c, a))
        worst = 0.0
        for q in used:
            d = sgn * sum(nrm[i] * (X[q][i] - a[i]) for i in range(3)) / nl
            if d > worst:
                worst = d
        if worst > 1e-12 * L2:
            nopen += 1
            openf.append(tuple(f))
            gap = max(gap, worst / diag)
    nondel = 0
    if exact:
        for f, L in faces.items():
            if len(L) != 2:
                continue
            (k, _), (_, z) = L
            P = [X[i] for i in T[k]] + [X[z]]
            r = insphere(*P)
            h = max(abs(P[i][j] - P[4][j]) for i in range(4) for j in range(3))
            if r < -1e-8 * h ** 5:
                continue
            if insphere(*(xf(i) for i in list(T[k]) + [z])) > 0:
                nondel += 1
    hv = hull_volume([X[i] for i in used])
    mv = sum(abs(v) for v in vol) / 6.0
    LAST[0] = (X, T, openf)
    return dict(tets=len(T), invalid=bad + over, gap=gap, open=nopen, nonDel=nondel,
                missing=(hv - mv) / hv)


LAST = [None]


def picture(title, Tbase):
    """oblique view: the boundary edges of the mesh (grey), the faces of the
    tets the fill added (green; those not in the nohull mesh) and the
    boundary faces still open, with a point outside them (red)"""
    X, T, openf = LAST[0]
    def pr(p):
        return (p[0] + 0.35 * p[2], p[1] + 0.22 * p[2])
    lo = [min(p[k] for p in X) for k in range(3)]
    hi = [max(p[k] for p in X) for k in range(3)]
    sc = [max(hi[k] - lo[k], 1e-300) for k in range(3)]
    def nz(p):   # each axis to [0, 1] so long thin boxes still show
        return [(p[k] - lo[k]) / sc[k] for k in range(3)]
    faces = {}
    for t in T:
        for j in range(4):
            f = tuple(sorted(t[:j] + t[j + 1:]))
            faces[f] = faces.get(f, 0) + 1
    segs = []
    seen = set()
    for f, c in faces.items():
        if c != 1:
            continue
        for a, b in ((f[0], f[1]), (f[1], f[2]), (f[0], f[2])):
            if (a, b) in seen:
                continue
            seen.add((a, b))
            A, B = pr(nz(X[a])), pr(nz(X[b]))
            segs.append((A[0], A[1], B[0], B[1], "#d0d7de", 0.5))
    added = [t for t in T if tuple(sorted(t)) not in Tbase] if Tbase is not None else []
    for t in added:
        for a, b in ((0, 1), (0, 2), (0, 3), (1, 2), (1, 3), (2, 3)):
            A, B = pr(nz(X[t[a]])), pr(nz(X[t[b]]))
            segs.append((A[0], A[1], B[0], B[1], "#1a7f37", 0.7))
    for f in openf:
        for a, b in ((f[0], f[1]), (f[1], f[2]), (f[0], f[2])):
            A, B = pr(nz(X[a])), pr(nz(X[b]))
            segs.append((A[0], A[1], B[0], B[1], "#cf222e", 1.0))
    note = "green: %d tets added; red: %d faces open" % (len(added), len(openf))
    return svg_plot2d(title, segs, (), (), 250, 250, pad=0.04, note=note)


def main(work, logdir):
    exp = json.load(open(os.path.join(work, "expect.json")))
    rows, ok_all = [], True
    base = {}
    for deck, e in exp.items():
        fn = os.path.join(work, "out_%s.inp" % deck)
        if e["mode"] == "nohull" and os.path.isfile(fn):
            base[e["set"]] = set(tuple(sorted(t)) for t in read_avs(fn)[1])
    for deck in sorted(exp):
        e = exp[deck]
        fails, r, added = [], None, None
        outp = os.path.join(work, deck + ".out")
        text = open(outp, errors="replace").read() if os.path.isfile(outp) else ""
        if "LaGriT successfully completed" not in text:
            fails.append("LaGriT did not finish")
        for l in text.splitlines():
            if "tets to reach the convex hull" in l:
                added = int(l.split()[2])
        mesh = os.path.join(work, "out_%s.inp" % deck)
        if os.path.isfile(mesh):
            X0, _ = read_avs(os.path.join(work, "pts_%s.inp" % e["set"]))
            X, T = read_avs(mesh)
            X = list(X0) + list(X[len(X0):])
            r = check(X, T, e["mode"] == "exact")
            r["pic"] = picture("%s, %s" % (e["set"], {"tol": "connect", "exact": "connect / exact",
                                                       "nohull": "connect / nohull"}[e["mode"]]),
                               base.get(e["set"]) if e["mode"] != "nohull" else None)
            if r["invalid"]:
                fails.append("%d tets inverted or overlapping" % r["invalid"])
            if e["mode"] != "nohull" and r["missing"] > 1e-6:
                fails.append("%.2g of the hull volume missing" % r["missing"])
            if e["mode"] == "exact" and r["nonDel"]:
                fails.append("%d faces not Delaunay" % r["nonDel"])
            if e["mode"] == "nohull" and added is not None:
                fails.append("nohull still filled")
        else:
            fails.append("no mesh written")
        res = "PASS" if not fails else "FAIL"
        ok_all &= not fails
        with open(os.path.join(logdir, deck + ".check"), "w") as f:
            f.write("result %s\n" % res)
            if r:
                f.write("".join("%s %s\n" % kv for kv in sorted(r.items()) if kv[0] != "pic"))
            f.writelines("fail %s\n" % x for x in fails)
        rows.append((deck, res, fails, e, r, added))
        print("  %-4s  %-22s %s%s" % (res, deck, ("tets=%d added=%s missing=%.2g open=%d nonDel=%d" %
              (r["tets"], added, r["missing"], r["open"], r["nonDel"])) if r else "", ("  " + "; ".join(fails)) if fails else ""))
    with open(os.path.join(logdir, "table.txt"), "w") as f:
        f.writelines("%s %s\n" % (d, s) for d, s, _, _, _, _ in rows)
    o = ["""<!doctype html><html><head><meta charset="utf-8">
<meta name="viewport" content="width=device-width, initial-scale=1"><title>connect hull</title>
<style>body{font-family:-apple-system,Helvetica,Arial,sans-serif;margin:16px;max-width:1150px;color:#1f2328;background:#fff}
p{font-size:14px;line-height:1.45}table{border-collapse:collapse;font-size:12px}td,th{border-bottom:1px solid #d0d7de;
padding:3px 6px;text-align:right;vertical-align:top}td:first-child,th:first-child,td.l{text-align:left}th{background:#f6f8fa}
.PASS{color:#1a7f37;font-weight:600}.FAIL{color:#cf222e;font-weight:600}
details summary{cursor:pointer;color:#0969da;font-size:12px}</style></head><body>
<p style="margin:0 0 8px;font-size:13px"><a href="../../summary.html">&larr; all suites (top-level summary)</a></p>
<h1 style="font-size:20px">3D connect: fill to the convex hull</h1>
<p>After the enclosing tetrahedron is removed, connect adds the Delaunay tets between the mesh and the convex hull
(src/lg_connect3d_hull.c). <i>added</i>: tets added; <i>missing</i>: the share of the convex hull's volume not covered (pockets flatter than 1e-6 of their size are left open by design);
<i>open</i>: boundary faces with a point outside; <i>nonDel</i>: faces not Delaunay (exact rationals, connect / exact).
<b>nohull</b> keeps the mesh as the enclosing tet left it: the baseline.</p>
<p>The pictures show each mesh in an oblique view, every axis scaled to the same length (so the 100 x 1 x 1 box
shows): grey the edges of its boundary, green the tets the fill added (those not in the nohull mesh), red the boundary
faces still open (a point outside them). The chart shows the share of the hull's volume not covered.</p>"""]
    sets = []
    for row in rows:
        if row[3]["set"] not in sets:
            sets.append(row[3]["set"])
    def val(st, mode):
        for d, s_, f_, e, r, a in rows:
            if e["set"] == st and e["mode"] == mode and r:
                return max(r["missing"], 0.0)
        return 0.0
    o.append(svg_bars("Share of the convex hull's volume missing (log scale; 0 drawn as no bar)", sets,
                      [("nohull (before)", [val(t, "nohull") for t in sets]),
                       ("with the fill", [val(t, "tol") for t in sets])],
                      ["#cf222e", "#1a7f37"], width=640, height=250, logy=True))
    o.append("""<table><tr><th>deck</th><th>result</th><th>points</th><th>tets</th><th>added</th><th>missing</th><th>open</th><th>nonDel</th>
<th class="l">notes</th><th>picture</th></tr>""")
    for d, s, fails, e, r, added in rows:
        o.append('<tr><td>%s<details><summary>What it does</summary>%s</details></td><td class="%s">%s</td>'
                 '<td>%d</td><td>%s</td><td>%s</td><td>%s</td><td>%s</td><td>%s</td><td class="l">%s</td><td style="min-width:250px">%s</td></tr>'
                 % (E(d), E(e["about"]), s, s, e["n"], r["tets"] if r else "-", "-" if added is None else added,
                    "%.2g" % r["missing"] if r else "-", r["open"] if r else "-",
                    r["nonDel"] if r and e["mode"] == "exact" else "", "".join('<div class="FAIL">%s</div>' % E(x) for x in fails),
                    r.get("pic", "") if r else ""))
    o.append("</table></body></html>")
    open(os.path.join(logdir, "summary.html"), "w").write("\n".join(o) + "\n")
    n = sum(s == "PASS" for _, s, _, _, _, _ in rows)
    print("\npassed %d   failed %d" % (n, len(rows) - n))
    return 0 if ok_all else 1


if __name__ == "__main__":
    if len(sys.argv) != 3:
        sys.exit(__doc__)
    sys.exit(main(sys.argv[1], sys.argv[2]))
