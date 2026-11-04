#!/usr/bin/env python3
"""make_page.py LOGDIR: LOGDIR/summary.html for test_poisson_command_errors,
from LOGDIR/table.txt (written by run_error_tests.sh), the EXPECT lines in
each deck's header, the first ERROR or WARNING block on each deck's stdout,
and, for the warn decks that write a mesh, a picture of it with its
polygon. Python 3, standard library only."""
import html
import os
import sys

E = html.escape
here = os.path.dirname(os.path.abspath(__file__))
logs = sys.argv[1] if len(sys.argv) > 1 else "logs"


def expect(deck):
    """the EXPECT: lines of the deck header, as one sentence"""
    out, on = [], False
    for l in open(deck, errors="replace"):
        if not l.startswith("#"):
            break
        t = l[1:].strip()
        if t.startswith("EXPECT"):
            on = True
            continue
        if on and (not t or t.endswith(":") or t.startswith("#")):
            if out:
                break
            continue
        if on:
            out.append(t)
    return " ".join(out)


def block(stdout):
    """the first ERROR or WARNING POISSON DISK block, banner to banner"""
    if not os.path.isfile(stdout):
        return ""
    L, out, on = open(stdout, errors="replace").read().split("\n"), [], False
    for l in L:
        if "POISSON DISK:" in l and not on:
            on = True
        if on:
            out.append(l.rstrip())
            if "POISSON DISK" in l and "POISSON DISK:" not in l:
                break
    return "\n".join(out)


def read_avs(fn):
    L = [l.split() for l in open(fn).read().split("\n") if l.strip()]
    nv, nc = int(L[0][0]), int(L[0][1])
    X = {int(w[0]): (float(w[1]), float(w[2])) for w in L[1:1 + nv]}
    C = [[int(k) for k in w[3:]] for w in L[1 + nv:1 + nv + nc]]
    return X, C


def picture(deck):
    """the mesh the deck dumped (grey triangles, blue vertices) on its
    polygon (red)"""
    lines = open(deck, errors="replace").read().split("\n")
    poly = out = None
    for l in lines:
        w = [x.strip() for x in l.split("/")]
        if len(w) >= 3 and w[0] == "read" and w[1] == "avs":
            poly = w[2]
        if len(w) >= 3 and w[0] == "dump" and w[1] == "avs":
            out = w[2]
    if not out or not os.path.isfile(os.path.join(here, out)):
        return ""
    X, C = read_avs(os.path.join(here, out))
    P = []
    if poly and os.path.isfile(os.path.join(here, poly)):
        PX, _ = read_avs(os.path.join(here, poly))
        P = [PX[k] for k in sorted(PX)]
    xs = [p[0] for p in X.values()] + [p[0] for p in P]
    ys = [p[1] for p in X.values()] + [p[1] for p in P]
    x0, x1, y0, y1 = min(xs), max(xs), min(ys), max(ys)
    S, pad = 200.0, 8
    sc = S / max(x1 - x0, y1 - y0, 1e-300)
    def T(p):
        return pad + (p[0] - x0) * sc, pad + (y1 - p[1]) * sc
    W, H = (x1 - x0) * sc + 2 * pad, (y1 - y0) * sc + 2 * pad
    o = ['<svg xmlns="http://www.w3.org/2000/svg" width="%.0f" height="%.0f" role="img">' % (W, H)]
    seen = set()
    for c in C:
        for a, b in zip(c, c[1:] + c[:1]):
            e = (min(a, b), max(a, b))
            if e in seen or a not in X or b not in X:
                continue
            seen.add(e)
            (ax, ay), (bx, by) = T(X[a]), T(X[b])
            o.append('<line x1="%.1f" y1="%.1f" x2="%.1f" y2="%.1f" stroke="#8c959f" stroke-width="0.5"/>'
                     % (ax, ay, bx, by))
    if P:
        o.append('<polygon points="%s" fill="none" stroke="#cf222e" stroke-width="1.5"/>'
                 % " ".join("%.1f,%.1f" % T(p) for p in P))
    for p in X.values():
        x, y = T(p)
        o.append('<circle cx="%.1f" cy="%.1f" r="1.2" fill="#0969da"/>' % (x, y))
    o.append("</svg>")
    return ('%s<div style="font-size:11px;color:#57606a">%s: %d vertices, %d triangles; red: the polygon</div>'
            % ("".join(o), E(out), len(X), len(C)))


rows = []
for l in open(os.path.join(logs, "table.txt")):
    w = l.split(None, 2)
    if len(w) < 2:
        continue
    res, rest = w[0], " ".join(w[1:]).strip()
    name = rest.split()[0]
    deck = os.path.join(here, name + ".lgi")
    if os.path.isfile(deck):
        about = expect(deck)
        msg = block(os.path.join(logs, name + ".stdout"))
        pic = picture(deck) if name.startswith("warn") else ""
    else:
        about, msg, pic = ("every ERROR or WARNING banner that opens a block also closes it, "
                           "over all the decks", "", "")
    if os.path.isfile(deck):
        rows.append((name, res, rest[len(name):].strip(), about, msg, pic))
    else:
        rows.append(("block balance", res, rest, about, msg, pic))

o = ["""<!doctype html><html><head><meta charset="utf-8"><title>test_poisson_command_errors</title>
<style>body{font-family:-apple-system,Helvetica,Arial,sans-serif;margin:16px;max-width:1200px;color:#1f2328;background:#fff}
p{font-size:14px;line-height:1.45;max-width:1000px}table{border-collapse:collapse;font-size:12.5px}
td,th{border-bottom:1px solid #d0d7de;padding:4px 7px;text-align:left;vertical-align:top}th{background:#f6f8fa}
.PASS{color:#1a7f37;font-weight:600}.FAIL{color:#cf222e;font-weight:600}
pre{font-size:11px;background:#f6f8fa;padding:6px;margin:0;white-space:pre-wrap;max-width:560px}</style></head><body>
<p style="margin:0 0 8px;font-size:13px"><a href="../../summary.html">&larr; all suites (top-level summary)</a></p>
<h1 style="font-size:20px">createpts / poisson_disk: error and warning messages</h1>
<p><b>What it does:</b> each err deck gives <code>createpts / poisson_disk</code> (2D polygon or 3D box) a command it
must refuse: a bad token, a missing value, a bad polygon, an h field below its floor, an output mesh object that already
exists, a flat or inverted box. It PASSES when LaGriT prints an ERROR block naming the fault (all faults in one block
where there are several) and does not crash. Each warn deck gives a command LaGriT can still carry out (a nonconvex
polygon, a large h, no edge buffer): a WARNING block, no ERROR block, and the mesh is written; its picture shows that
mesh. The last row checks that every block LaGriT opened it also closed. The message column is LaGriT's own first
block, from the deck's stdout (all three output streams are in <code>logs/</code>).</p>
<table><tr><th>deck</th><th>result</th><th>what it does (the deck's EXPECT)</th><th>LaGriT's message</th><th>mesh written</th></tr>"""]
for name, res, note, about, msg, pic in rows:
    link = '<a href="%s.all">%s</a>' % (E(name), E(name)) if os.path.isfile(os.path.join(logs, name + ".all")) else E(name)
    o.append('<tr><td>%s<br><small>%s</small></td><td class="%s">%s</td><td style="max-width:300px">%s</td>'
             '<td>%s</td><td>%s</td></tr>'
             % (link, E(note), E(res), E(res), E(about), "<pre>%s</pre>" % E(msg) if msg else "", pic))
o.append("</table></body></html>")
open(os.path.join(logs, "summary.html"), "w").write("\n".join(o) + "\n")
