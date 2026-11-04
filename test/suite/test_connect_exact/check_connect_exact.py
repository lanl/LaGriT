#!/usr/bin/env python3
"""check_connect_exact.py WORKDIR LOGDIR

Judges test_connect_exact (Python 3, standard library only). For each
deck it reads the input points (WORKDIR/pts_<set>.inp, 17 digits, the
coordinates LaGriT read) and the mesh LaGriT wrote (out_<deck>.inp),
and computes with exact rational arithmetic where floating point
cannot decide:

  verts      vertices in the mesh (points added by connect: verts - n)
  missing    input points in no tet (dudded)
  nonDel     interior faces that are not locally Delaunay: the vertex
             across the face is strictly inside the other tet's
             circumsphere
  inverted   tets whose orientation is opposite to the majority, or
             exactly flat
  flat_fp    tets whose volume is zero or negative in floating point
             but positive exactly (flat to round-off)

A connect / exact deck PASSES when nothing was added, nothing is
missing, nonDel = 0, inverted = 0 and flat_fp = 0 (the sphere: flat_fp
is reported, a known property of its exact Delaunay tetrahedralization,
not a failure). A plain connect deck PASSES when LaGriT finished and
inverted = 0; its other numbers are shown as the baseline the exact
tests are compared with.
Writes LOGDIR/<deck>.check, LOGDIR/table.txt and LOGDIR/summary.html.
"""
import html
import json
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
    """det(b - a, c - a, d - a): positive for a positively oriented tet"""
    return det3([[b[i] - a[i] for i in range(3)], [c[i] - a[i] for i in range(3)], [d[i] - a[i] for i in range(3)]])


def insphere(a, b, c, d, e):
    """> 0 when e is strictly inside the sphere through a, b, c, d (any order)"""
    rows = []
    for p in (a, b, c, d):
        w = [p[i] - e[i] for i in range(3)]
        rows.append(w + [w[0] * w[0] + w[1] * w[1] + w[2] * w[2]])
    s = 0
    for j in range(4):
        s += (-1) ** j * rows[0][j] * det3([[rows[r][k] for k in range(4) if k != j] for r in range(1, 4)])
    o = orient(a, b, c, d)
    return 0 if o == 0 else (-s if o > 0 else s)


def check(X0, X, T):
    X = list(X0) + list(X[len(X0):])
    XF = {}

    def xf(i):
        if i not in XF:
            XF[i] = tuple(Fr(v) for v in X[i])
        return XF[i]

    vol = [orient(*(X[i] for i in t)) for t in T]
    scale = max(abs(v) for v in vol) if vol else 1.0
    pos = sum(v > 0 for v in vol)
    sgn = 1 if pos >= len(vol) - pos else -1
    inverted = flat_fp = 0
    for t, v in zip(T, vol):
        if v * sgn > 1e-10 * scale:
            continue
        o = orient(*(xf(i) for i in t)) * sgn
        if o <= 0:
            inverted += 1
        elif v * sgn <= 0:
            flat_fp += 1
    faces = {}
    for k, t in enumerate(T):
        for j in range(4):
            faces.setdefault(frozenset(t[:j] + t[j + 1:]), []).append((k, t[j]))
    nondel = 0
    badf = []
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
            badf.append(tuple(f))
    used = set(i for t in T for i in t)
    missing = sum(1 for i in range(len(X0)) if i not in used)
    PIC[0] = (X, [i for i in range(len(X0)) if i not in used], list(range(len(X0), len(X))), badf)
    return dict(tets=len(T), verts=len(X), missing=missing, nonDel=nondel, inverted=inverted, flat_fp=flat_fp)


PIC = [None]


def picture(name):
    """oblique view of the vertices: red the faces that are not Delaunay,
    black crosses the input points left out, orange the points added"""
    if PIC[0] is None:
        return ""
    X, miss, add, badf = PIC[0]
    def pr(p):
        return (p[0] + 0.35 * p[2], p[1] + 0.22 * p[2])
    pts = [pr(p) + ("#d0d7de", 1.2, "") for p in X]
    segs = []
    for f in badf[:3000]:
        a, b, c = [pr(X[i]) for i in f]
        segs += [(a[0], a[1], b[0], b[1], "#cf222e", 0.8), (b[0], b[1], c[0], c[1], "#cf222e", 0.8),
                 (c[0], c[1], a[0], a[1], "#cf222e", 0.8)]
    pts += [pr(X[i]) + ("#bf8700", 2.5, "") for i in add]
    pts += [pr(X[i]) + ("#1f2328", 3.5, "") for i in miss]
    note = "red %d not Delaunay, orange %d added, black %d out" % (len(badf), len(add), len(miss))
    return svg_plot2d(name, segs, pts, (), 250, 250, pad=0.04, note=note)


def main(work, logdir):
    exp = json.load(open(os.path.join(work, "expect.json")))
    rows, ok_all = [], True
    for deck in sorted(exp):
        e = exp[deck]
        fails, r = [], None
        text = open(os.path.join(work, deck + ".out"), errors="replace").read() \
            if os.path.isfile(os.path.join(work, deck + ".out")) else ""
        if "LaGriT successfully completed" not in text:
            fails.append("LaGriT did not finish")
        mesh = os.path.join(work, "out_%s.inp" % deck)
        if os.path.isfile(mesh):
            X0, _ = read_avs(os.path.join(work, "pts_%s.inp" % e["set"]))
            X, T = read_avs(mesh)
            r = check(X0, X, T) if T else dict(tets=0, verts=len(X), missing=len(X0), nonDel=0, inverted=0, flat_fp=0)
            r["added"] = r["verts"] - e["n"]
            r["pic"] = picture("%s, %s" % (e["set"], "connect / exact" if e["mode"] == "exact" else "plain connect"))
            if r["inverted"]:
                fails.append("%d tets inverted or flat (exactly)" % r["inverted"])
            if e["mode"] == "exact":
                for k, what in (("added", "points added"), ("missing", "points missing"),
                                ("nonDel", "faces not Delaunay")):
                    if r[k]:
                        fails.append("%d %s" % (r[k], what))
                if r["flat_fp"] and not e["flat_known"]:
                    fails.append("%d tets flat to round-off" % r["flat_fp"])
                if r["tets"] == 0:
                    fails.append("no tets")
        else:
            fails.append("no mesh written")
        res = "PASS" if not fails else "FAIL"
        ok_all &= not fails
        with open(os.path.join(logdir, deck + ".check"), "w") as f:
            f.write("result %s\n" % res)
            if r:
                f.write("".join("%s %s\n" % kv for kv in sorted(r.items()) if kv[0] != "pic"))
            f.writelines("fail %s\n" % x for x in fails)
        rows.append((deck, res, fails, e, r))
        print("  %-4s  %-24s %s%s" % (res, deck, " ".join("%s=%s" % (k, r[k]) for k in
              ("tets", "added", "missing", "nonDel", "inverted", "flat_fp")) if r else "",
              ("  " + "; ".join(fails)) if fails else ""))
    with open(os.path.join(logdir, "table.txt"), "w") as f:
        f.writelines("%s %s\n" % (d, s) for d, s, _, _, _ in rows)
    o = ["""<!doctype html><html><head><meta charset="utf-8">
<meta name="viewport" content="width=device-width, initial-scale=1"><title>connect exact</title>
<style>body{font-family:-apple-system,Helvetica,Arial,sans-serif;margin:16px;max-width:1150px;color:#1f2328;background:#fff}
p{font-size:14px;line-height:1.45}table{border-collapse:collapse;font-size:12px}td,th{border-bottom:1px solid #d0d7de;
padding:3px 6px;text-align:right;vertical-align:top}td:first-child,th:first-child,td.l{text-align:left}th{background:#f6f8fa}
.PASS{color:#1a7f37;font-weight:600}.FAIL{color:#cf222e;font-weight:600}.bad{color:#cf222e}
details summary{cursor:pointer;color:#0969da;font-size:12px}</style></head><body>
<p style="margin:0 0 8px;font-size:13px"><a href="../../summary.html">&larr; all suites (top-level summary)</a></p>
<h1 style="font-size:20px">3D connect: tolerance tests and connect / exact</h1>
<p>Each point set is connected with plain <b>connect</b> (tolerance in-sphere tests, plus points added in small tets)
and with <b>connect / exact</b> (exact in-sphere tests, no points added). Every mesh is checked against the input
coordinates with exact rational arithmetic. <i>added</i>: points connect added; <i>missing</i>: input points in no tet
(dudded); <i>nonDel</i>: interior faces that are not Delaunay; <i>inverted</i>: tets inverted or flat, exactly;
<i>flat fp</i>: tets valid exactly but of zero or negative volume in floating point. connect / exact must add and
miss nothing and be exactly Delaunay; plain connect must give valid tets, and its other numbers are the baseline.</p>
"""]
    sets = []
    for d, s_, f_, e, r in rows:
        if e["set"] not in sets:
            sets.append(e["set"])
    def val(st, mode, key):
        for d, s_, f_, e, r in rows:
            if e["set"] == st and e["mode"] == mode and r:
                return r[key]
        return 0
    o.append(svg_bars("Faces not Delaunay (log scale; 0 drawn as no bar)", sets,
                      [("plain connect", [val(t, "tol", "nonDel") for t in sets]),
                       ("connect / exact", [val(t, "exact", "nonDel") for t in sets])],
                      ["#cf222e", "#1a7f37"], width=760, height=260, logy=True))
    o.append(svg_bars("Points added by connect, and input points left out (plain connect)", sets,
                      [("added", [val(t, "tol", "added") for t in sets]),
                       ("left out", [val(t, "tol", "missing") for t in sets])],
                      ["#bf8700", "#1f2328"], width=760, height=240))
    o.append("""<p>The pictures show each mesh's vertices in an oblique view: in red the faces that are not Delaunay (the
vertex across them is strictly inside the circumsphere), in orange the points connect added, in black the input
points it left out. connect / exact must show none of them.</p>
<table><tr><th>deck</th><th>result</th><th>points</th><th>tets</th><th>added</th><th>missing</th><th>nonDel</th>
<th>inverted</th><th>flat fp</th><th class="l">notes</th><th>picture</th></tr>""")
    for d, s, fails, e, r in rows:
        cells = "".join('<td%s>%s</td>' % (' class="bad"' if r and k != "tets" and r[k] and (e["mode"] == "exact" or k == "inverted")
                        and not (k == "flat_fp" and e["flat_known"]) else "", r[k] if r else "-")
                        for k in ("tets", "added", "missing", "nonDel", "inverted", "flat_fp"))
        o.append('<tr><td>%s<details><summary>What it does</summary>%s</details></td><td class="%s">%s</td>'
                 '<td>%d</td>%s<td class="l">%s</td><td style="min-width:250px">%s</td></tr>'
                 % (E(d), E(e["about"]), s, s, e["n"], cells,
                    "".join('<div class="FAIL">%s</div>' % E(x) for x in fails) or
                    ("known: flat to round-off" if e["flat_known"] else ""), r.get("pic", "") if r else ""))
    o.append("</table></body></html>")
    open(os.path.join(logdir, "summary.html"), "w").write("\n".join(o) + "\n")
    n = sum(s == "PASS" for _, s, _, _, _ in rows)
    print("\npassed %d   failed %d" % (n, len(rows) - n))
    return 0 if ok_all else 1


if __name__ == "__main__":
    if len(sys.argv) != 3:
        sys.exit(__doc__)
    sys.exit(main(sys.argv[1], sys.argv[2]))
