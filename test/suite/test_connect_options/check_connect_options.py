#!/usr/bin/env python3
"""check_connect_options.py WORKDIR LOGDIR

Judges test_connect_options (Python 3, standard library only) from each
deck's output (WORKDIR/<deck>.out) against WORKDIR/expect.json, which
make_connect_options.py writes with the decks.

A deck PASSES when
  - LaGriT finished ("LaGriT successfully completed"), with no signal
    and no MMVERIFY message;
  - the node and element counts of mesh object mo (from the
    cmo / status / brief at the end) meet "nodes" and "elements":
    ("eq", n) exactly n, ("gt", n) more than n, ("same", deck) the
    same as that deck's;
  - every text in "has" appears in the output.
Writes LOGDIR/<deck>.check, LOGDIR/table.txt and LOGDIR/summary.html.
"""
import html
import json
import os
import re
import sys

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
    """points, cells and vertex imt of an AVS file (dump / avs / ... / 1 1 1 0)"""
    with open(fn) as f:
        h = f.readline().split()
        n, ne, nna = int(h[0]), int(h[1]), int(h[2])
        X = [tuple(float(v) for v in f.readline().split()[1:4]) for _ in range(n)]
        C = []
        for _ in range(ne):
            w = f.readline().split()
            C.append((int(w[1]), [int(v) - 1 for v in w[3:]]))
        imt = [1] * n
        rest = f.read().split("\n")
        # node attribute block: first line counts, then names, then rows
        try:
            k = 0
            while k < len(rest) and not rest[k].strip():
                k += 1
            na = [int(v) for v in rest[k].split()]
            names = [rest[k + 1 + i].split(",")[0].strip() for i in range(na[0])]
            rows = rest[k + 1 + na[0]:k + 1 + na[0] + n]
            if "imt1" in names:
                j = names.index("imt1")
                imt = [int(float(r.split()[1 + j])) for r in rows]
        except (ValueError, IndexError):
            pass
    return X, C, imt


MATCOL = {1: "#0969da", 2: "#bf8700", 3: "#8250df"}


def picture(deck, e, work):
    fn = os.path.join(work, "out_%s.inp" % deck)
    if not os.path.isfile(fn):
        return ""
    X, C, imt = read_avs(fn)
    n0 = {"A": 216, "B": 125, "C": 49}[e["setup"]]
    used = set(i for _, c in C for i in c)
    if e["setup"] == "C":     # 2D: the triangles
        segs = []
        for _, c in C:
            for a, b in ((c[0], c[1]), (c[1], c[2]), (c[2], c[0])):
                segs.append((X[a][0], X[a][1], X[b][0], X[b][1], "#8c959f", 0.6))
        pts = [(X[i][0], X[i][1], "#cf222e" if i >= n0 else MATCOL.get(imt[i], "#1f2328"), 3 if i >= n0 else 2, "")
               for i in range(len(X))]
        segs.append((0.45, 0.0, 0.55, 1.0, "#cf222e", 1.2))
        return svg_plot2d("2D mesh (red line: the interface)", segs, pts, (), 230, 230,
                          note="red points: added by connect (%d)" % max(0, len(X) - n0))
    if e["setup"] == "A":     # 3D: side view (y, z) of the vertices
        pts = [(X[i][1], X[i][2], "#cf222e" if i >= n0 else MATCOL.get(imt[i], "#1f2328"), 3 if i >= n0 else 2, "")
               for i in range(len(X))]
        segs = [(0.0, 0.55, 1.0, 0.45, "#cf222e", 1.0)]
        return svg_plot2d("3D vertices, side view (y, z); red line: interface", segs, pts, (), 230, 230,
                          note="red: added by connect (%d)" % max(0, len(X) - n0))
    pts = [(X[i][0] + 0.18 * X[i][2], X[i][1] + 0.12 * X[i][2], "#1a7f37" if i in used else "#afb8c1",
            3 if i in used else 2, "") for i in range(len(X))]
    return svg_plot2d("3D vertices (oblique view)", (), pts, (), 230, 230,
                      note="green: in a tet (%d); grey: not connected" % len(used))


def counts(text):
    n = re.findall(r"number of nodes\s*=\s*(\d+)", text)
    e = re.findall(r"number of elements\s*=\s*(\d+)", text)
    return (int(n[-1]) if n else None, int(e[-1]) if e else None)


def main(work, logdir):
    exp = json.load(open(os.path.join(work, "expect.json")))
    got = {}
    texts = {}
    for deck in exp:
        p = os.path.join(work, deck + ".out")
        texts[deck] = open(p, errors="replace").read() if os.path.isfile(p) else ""
        got[deck] = counts(texts[deck])
    rows = []
    ok_all = True
    for deck in sorted(exp):
        e, text = exp[deck], texts[deck]
        fails = []
        if "LaGriT successfully completed" not in text:
            fails.append("LaGriT did not finish")
        for bad in ("Program received signal", "MMVERIFY"):
            if bad in text:
                fails.append('"%s" in the output' % bad)
        for k, i in (("nodes", 0), ("elements", 1)):
            if k not in e:
                continue
            op, v = e[k]
            have = got[deck][i]
            want = got[v][i] if op == "same" else v
            if have is None:
                fails.append("no %s count" % k)
            elif op in ("eq", "same") and have != want:
                fails.append("%s %d, expected %d%s" % (k, have, want, " (as %s)" % v if op == "same" else ""))
            elif op == "gt" and not have > want:
                fails.append("%s %d, expected more than %d" % (k, have, want))
        for t in e.get("has", []):
            if t not in text:
                fails.append('no "%s"' % t)
        msgs = [l.rstrip() for l in text.splitlines() if "WARNING: connect" in l or l.startswith("   (") or
                l.startswith("   tetrahedron needs") or l.startswith("   triangle needs")]
        res = "PASS" if not fails else "FAIL"
        ok_all &= not fails
        with open(os.path.join(logdir, deck + ".check"), "w") as f:
            f.write("result %s\n" % res)
            f.writelines("fail %s\n" % x for x in fails)
        rows.append((deck, res, fails, msgs, e, got[deck]))
        print("  %-4s  %s%s" % (res, deck, ("  " + "; ".join(fails[:3])) if fails else ""))
    with open(os.path.join(logdir, "table.txt"), "w") as f:
        f.writelines("%s %s\n" % (r[0], r[1]) for r in rows)
    o = ["""<!doctype html><html><head><meta charset="utf-8">
<meta name="viewport" content="width=device-width, initial-scale=1"><title>connect options</title>
<style>body{font-family:-apple-system,Helvetica,Arial,sans-serif;margin:16px;max-width:1150px;color:#1f2328;background:#fff}
p,li{font-size:14px;line-height:1.45}table{border-collapse:collapse;font-size:12px}td,th{border-bottom:1px solid #d0d7de;
padding:3px 6px;text-align:left;vertical-align:top}th{background:#f6f8fa}.PASS{color:#1a7f37;font-weight:600}
.FAIL{color:#cf222e;font-weight:600}code,pre{font-size:11px;background:#f6f8fa}pre{padding:6px;margin:2px 0;white-space:pre-wrap;max-width:300px}
details summary{cursor:pointer;color:#0969da;font-size:12px}</style></head><body>
<p style="margin:0 0 8px;font-size:13px"><a href="../../summary.html">&larr; all suites (top-level summary)</a></p>
<h1 style="font-size:20px">connect options: noadd, check_interface, old_order, point range, enclosing tet</h1>
<p>The keywords <b>noadd</b>, <b>check_interface</b> and <b>old_order</b> may be anywhere in the command, in 2D and 3D
(<b>check_interface</b> is 3D only: a WARNING in 2D). When <b>delaunay</b> is left out of token 2, a point range or a pset
there is still read as the point range. The coordinates of the enclosing tetrahedron (12 numbers) or triangle (9) are
used with a WARNING that they are deprecated; fewer numbers give a WARNING and are ignored. Mesh A: 3D, two materials
split by a tilted plane, 216 points. Mesh B: 3D, 125 points, points 1 to 60 chosen. Mesh C: 2D, two materials, 49
points. The column V3.3.4 says what LaGriT V3.3.4 did with the same command.</p>
<p>The pictures show each deck's mesh: for the two-material meshes (A, C) the vertices coloured by material (blue,
yellow) with the points connect added in red, so <b>noadd</b> shows as no red points; for mesh B the vertices that are in a
tet (green) and those left out by the point range or pset (grey).</p>
<table><tr><th>deck</th><th>result</th><th>command</th><th>nodes / elements</th><th>V3.3.4</th><th>LaGriT's warnings</th><th style="min-width:240px">mesh</th></tr>"""]
    for d, r, fails, msgs, e, (nn, ne) in rows:
        o.append('<tr><td>%s<details><summary>What it does</summary>%s</details></td><td class="%s">%s</td>'
                 '<td><code>%s</code></td><td>%s / %s</td><td>%s</td><td><pre>%s</pre>%s</td><td style="min-width:240px">%s</td></tr>'
                 % (E(d), E(e["about"]), r, r, E(e["cmd"]), nn, ne, E(e["old"]),
                    E("\n".join(msgs) or "(none)"),
                    "".join('<div class="FAIL">%s</div>' % E(x) for x in fails), picture(d, e, work)))
    o.append("</table></body></html>")
    open(os.path.join(logdir, "summary.html"), "w").write("\n".join(o) + "\n")
    n = sum(r[1] == "PASS" for r in rows)
    print("\npassed %d   failed %d" % (n, len(rows) - n))
    return 0 if ok_all else 1


if __name__ == "__main__":
    if len(sys.argv) != 3:
        sys.exit(__doc__)
    sys.exit(main(sys.argv[1], sys.argv[2]))
