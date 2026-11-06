#!/usr/bin/env python3
"""make_page.py LOGDIR: logs/summary.html from table.txt and the
*.check files of test_avs_io. Python 3, standard library only."""
import glob
import html
import os
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


EDGES = {"pt": [], "line": [(0, 1)], "tri": [(0, 1), (1, 2), (2, 0)],
         "quad": [(0, 1), (1, 2), (2, 3), (3, 0)],
         "tet": [(0, 1), (0, 2), (0, 3), (1, 2), (1, 3), (2, 3)],
         "pyr": [(1, 2), (2, 3), (3, 4), (4, 1), (0, 1), (0, 2), (0, 3), (0, 4)],  # apex first
         "pri": [(0, 1), (1, 2), (2, 0), (3, 4), (4, 5), (5, 3), (0, 3), (1, 4), (2, 5)],
         "hex": [(0, 1), (1, 2), (2, 3), (3, 0), (4, 5), (5, 6), (6, 7), (7, 4),
                 (0, 4), (1, 5), (2, 6), (3, 7)]}
COL = {"pt": "#8250df", "line": "#57606a", "tri": "#0969da", "quad": "#1a7f37",
       "tet": "#cf222e", "pyr": "#bf8700", "pri": "#8250df", "hex": "#0969da"}


def read_cells(fn):
    """vertices and (type, vertex list) cells of an AVS file, or None"""
    try:
        L = [l for l in open(fn).read().split("\n") if l.strip() and not l.startswith("#")]
        nv, nc = int(L[0].split()[0]), int(L[0].split()[1])
        X, C = {}, []
        for l in L[1:1 + nv]:
            w = l.split()
            X[int(w[0])] = (float(w[1]), float(w[2]), float(w[3]))
        for l in L[1 + nv:1 + nv + nc]:
            w = l.split()
            t = w[2][:3] if w[2].startswith("pri") else w[2]
            C.append((t, [X[int(k)] for k in w[3:] if int(k) in X]))
        return list(X.values()), C
    except Exception:
        return None


def picture(fn):
    """oblique view of the mesh in fn: the cell edges coloured by cell type,
    the vertices as dots"""
    r = read_cells(fn)
    if not r or not r[0]:
        return ""
    X, C = r
    def pr(p):
        return (p[0] + 0.45 * p[2] + 0.15 * p[1], p[1] + 0.3 * p[2])
    segs, pts, kinds = [], [], []
    for t, v in C:
        if t not in kinds:
            kinds.append(t)
        for a, b in EDGES.get(t, []):
            if a < len(v) and b < len(v):
                A, B = pr(v[a]), pr(v[b])
                segs.append((A[0], A[1], B[0], B[1], COL.get(t, "#57606a"), 1.2))
        if t == "pt" and v:
            A = pr(v[0])
            pts.append((A[0], A[1], COL["pt"], 4, ""))
    for p in X:
        A = pr(p)
        pts.append((A[0], A[1], "#24292f", 1.6, ""))
    note = "%d vertices, %d cells: %s" % (len(X), len(C), ", ".join(kinds) or "none")
    return svg_plot2d(os.path.basename(fn), segs, pts, (), 210, 170, pad=0.06, note=note)
logs = sys.argv[1]
rows = open(os.path.join(logs, "table.txt")).read().split("\n")[1:]
here = os.path.dirname(os.path.abspath(__file__))
P = ['<!doctype html><html><head><meta charset="utf-8"><title>test_avs_io</title>',
     "<style>body{font-family:sans-serif;margin:16px;max-width:1100px}"
     "td{padding:2px 10px 2px 0;vertical-align:top}"
     ".PASS{color:#070;font-weight:bold}.KNOWN{color:#a60;font-weight:bold}"
     ".FAIL,.FIXED{color:#c00;font-weight:bold}"
     "pre{background:#f4f4f4;padding:6px;font-size:12px;margin:2px 0 10px}"
     "</style></head><body>",
     "<h1>test_avs_io: read / avs and dump / avs</h1>",
     "<p><b>What it does:</b> each deck reads a small AVS file (one kind of cell, or a "
     "file with an unusual layout), writes it back, and reads its own file again. "
     "The picture shows the mesh LaGriT wrote, seen at an angle: cell edges coloured by "
     "cell type, vertices as black dots. Decks that stop LaGriT have no picture.</p>",
     "<p>Every mesh LaGriT reads is compared with the file it came from "
     "(the checker reads AVS itself); every file LaGriT writes is compared "
     "byte for byte with <code>golden/</code>. KNOWN = a documented problem "
     "not yet fixed; FIXED = it now passes, so its "
     "<code># known:</code> line should go. See 00README.</p>",
     "<table><tr><th>deck</th><th>result</th><th>what it tests / detail</th><th>the mesh LaGriT wrote (first dump / avs)</th></tr>"]
for r in rows:
    w = r.split(None, 2)
    if len(w) < 2:
        continue
    name, res = w[0], w[1]
    det = w[2] if len(w) > 2 else ""
    deck = os.path.join(here, name + ".lgi")
    title = ""
    if os.path.exists(deck):
        t = [l[2:] for l in open(deck).read().split("\n")
             if l.startswith("# ") and not l.startswith(("# check:", "# known:"))]
        title = " ".join(t)
    pic = ""
    if os.path.exists(deck):
        for l in open(deck).read().split("\n"):
            w = [x.strip() for x in l.split("/")]
            if len(w) >= 3 and w[0] == "dump" and w[1] == "avs":
                pic = picture(os.path.join(here, "work", w[2]))
                break
    chk = os.path.join(logs, name + ".check")
    body = open(chk).read() if os.path.exists(chk) else ""
    P.append('<tr><td><a href="%s.stdout">%s</a></td><td class="%s">%s</td>'
             '<td>%s<br><small>%s</small><details><summary>checks</summary>'
             '<pre>%s</pre></details></td><td>%s</td></tr>'
             % (E(name), E(name), E(res), E(res), E(title), E(det), E(body), pic))
P.append("</table></body></html>")
open(os.path.join(logs, "summary.html"), "w").write("\n".join(P))
