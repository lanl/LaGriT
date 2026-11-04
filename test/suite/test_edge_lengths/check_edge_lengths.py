#!/usr/bin/env python3
"""check_edge_lengths.py WORKDIR LOGDIR

Judges  cmo / addatt / mo / edge_lengths_min | _max | _mean / name
(Python 3, standard library only). Each deck dumps its mesh with the
three attributes el_min, el_max, el_mean; the checker recomputes them
from the vertices and cells in that same file:

  the edges of a cell are its sides (triangle 3, quad 4: no diagonals)
  or, for a tet, its 6 vertex pairs; an edge in several cells counts
  once; at each vertex, the shortest, longest and mean length of its
  edges; 0 for a vertex in no cell.

A deck PASSES when LaGriT finished, the three attributes are real
vertex attributes, every value agrees with the reference to 1e-10
(relative), and the deck's header lines hold:
  # check   : OUT.inp   the file to judge
  # exact   : V         every value of all three is V
  # lone    : N         vertex N is in no cell and has 0
  # refused : TEXT      TEXT is in LaGriT's output
Writes LOGDIR/<deck>.check, LOGDIR/table.txt and LOGDIR/summary.html
(meshes coloured by el_mean, with draw_meshes.py from
test_lagrit_upstream).
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
except Exception:
    draw_meshes = None
E = html.escape
EDGES = {"tri": [(0, 1), (1, 2), (2, 0)], "quad": [(0, 1), (1, 2), (2, 3), (3, 0)],
         "tet": [(0, 1), (0, 2), (0, 3), (1, 2), (1, 3), (2, 3)]}


def read_avs(p):
    L = open(p).read().split("\n")
    nn, nc, nnd = map(int, L[0].split()[:3])
    V = [tuple(map(float, L[1 + i].split()[1:4])) for i in range(nn)]
    C = []
    for i in range(nc):
        w = L[1 + nn + i].split()
        C.append((w[2].lower(), [int(x) - 1 for x in w[3:]]))
    att, types = {}, {}
    if nnd:
        k = 1 + nn + nc
        w = L[k].split()
        na, sizes = int(w[0]), [int(x) for x in w[1:]] or [1] * int(w[0])
        names = []
        for j in range(na):
            n, _, t = L[k + 1 + j].partition(",")
            names.append(n.strip())
            types[n.strip()] = t.strip().lower()
        for i in range(nn):
            w = L[k + 1 + na + i].split()[1:]
            col = 0
            for n, sz in zip(names, sizes):
                if sz == 1:
                    att.setdefault(n, []).append(float(w[col]))
                col += sz
    return V, C, att, types


def reference(V, C):
    nb = [set() for _ in V]
    for t, ids in C:
        for a, b in EDGES[t]:
            nb[ids[a]].add(ids[b])
            nb[ids[b]].add(ids[a])
    out = []
    for i, p in enumerate(V):
        d = [math.dist(p, V[j]) for j in nb[i]]
        out.append((min(d), max(d), sum(d) / len(d)) if d else (0.0, 0.0, 0.0))
    return out


def headers(deck):
    h = {"check": [], "exact": [], "lone": [], "refused": [], "doc": []}
    for line in open(deck):
        if not line.startswith("#"):
            break
        b = line[1:].strip()
        k = b.split(":")[0].strip()
        if k in h and ":" in b:
            h[k].append(b.split(":", 1)[1].strip())
        elif b and not b.startswith("#") and not b.endswith(".lgi"):
            h["doc"].append(b)
    return h


def main(work, logdir):
    rows = []
    allok = True
    for deck in sorted(glob.glob(os.path.join(HERE, "el[0-9]*.lgi"))):
        name = os.path.basename(deck)[:-4]
        h = headers(deck)
        txt = open(os.path.join(work, name + ".out"), errors="replace").read() \
            if os.path.isfile(os.path.join(work, name + ".out")) else ""
        fails, info = [], {}
        if "LaGriT successfully completed" not in txt:
            fails.append("LaGriT did not finish")
        for t in h["refused"]:
            if t not in txt:
                fails.append('no "%s"' % t)
        pic = ""
        for out in h["check"]:
            p = os.path.join(work, out)
            if not os.path.isfile(p):
                fails.append("%s not written" % out)
                continue
            try:
                V, C, att, types = read_avs(p)
            except (ValueError, IndexError) as ex:
                fails.append("cannot read %s: %s" % (out, ex))
                continue
            ref = reference(V, C)
            for k, n in enumerate(("el_min", "el_max", "el_mean")):
                if n not in att:
                    fails.append("no attribute %s" % n)
                    continue
                if types.get(n) != "real":
                    fails.append("%s is %s, not real" % (n, types.get(n)))
                err = max(abs(att[n][i] - ref[i][k]) / max(abs(ref[i][k]), 1e-30) if ref[i][k] else abs(att[n][i])
                          for i in range(len(V)))
                info[n] = "%.3g to %.3g, largest relative difference %.1e" % (min(att[n]), max(att[n]), err)
                if err > 1e-10:
                    fails.append("%s differs by %.2e (relative)" % (n, err))
                for x in h["exact"]:
                    off = max(abs(v - float(x)) for v in att[n])
                    if off > 1e-12:
                        fails.append("%s is not %s everywhere (off by %.2e)" % (n, x, off))
                for x in h["lone"]:
                    if att[n][int(x) - 1] != 0.0:
                        fails.append("%s at lone vertex %s is %g, not 0" % (n, x, att[n][int(x) - 1]))
            info["mesh"] = "%d vertices, %d cells (%s)" % (len(V), len(C), ", ".join(sorted({t for t, _ in C})))
            if draw_meshes is not None:
                svg = name + ".svg"
                try:
                    draw_meshes.draw(p, os.path.join(logdir, svg), "%s: el_mean" % out, color="node:el_mean")
                    pic = '<a href="%s"><img src="%s" width="300"></a>' % (svg, svg)
                except Exception:
                    pass
        res = "PASS" if not fails else "FAIL"
        allok &= not fails
        open(os.path.join(logdir, name + ".check"), "w").write(
            "result %s\n" % res + "".join("fail %s\n" % f for f in fails))
        print("  %-4s  %s%s" % (res, name, ("  " + "; ".join(fails[:3])) if fails else ""))
        rows.append((name, res, h["doc"], info, fails, pic))
    open(os.path.join(logdir, "table.txt"), "w").write("".join("%s %s\n" % (r[0], r[1]) for r in rows))
    o = ["""<!doctype html><html><head><meta charset="utf-8"><title>edge_lengths</title>
<style>body{font-family:-apple-system,Helvetica,Arial,sans-serif;margin:16px;max-width:1150px;color:#1f2328}
table{border-collapse:collapse;font-size:12.5px}td,th{border-bottom:1px solid #d0d7de;padding:4px 7px;text-align:left;vertical-align:top}
th{background:#f6f8fa}.PASS{color:#1a7f37;font-weight:600}.FAIL{color:#cf222e;font-weight:600}</style></head><body>
<p style="font-size:13px"><a href="../../summary.html">&larr; all suites (top-level summary)</a></p>
<h1 style="font-size:20px">cmo / addatt / edge_lengths_min, _max, _mean</h1>
<p style="font-size:14px;max-width:950px">Each attribute is checked at every vertex against the shortest, longest and mean
length of the distinct edges at that vertex, computed here from the mesh in the same file (cell sides; a tet's 6 vertex
pairs; no quad diagonals; 0 for a vertex in no cell). Pictures: the mesh coloured by <code>el_mean</code>.</p>
<p style="font-size:14px;max-width:950px"><b>What it does:</b> each deck builds a small mesh (equilateral and right
triangles, quads, a mix of cell types, a jittered grid, a mesh with a vertex in no cell, tets), adds the three attributes
with <code>cmo / addatt</code>, and dumps it; el08 gives commands LaGriT must refuse. The checker reads the dump and
recomputes all three at every vertex.</p>
<table><tr><th>deck</th><th>result</th><th>what it does</th><th>values</th><th>picture</th></tr>"""]
    for name, res, doc, info, fails, pic in rows:
        o.append('<tr><td>%s</td><td class="%s">%s</td><td>%s</td><td>%s%s</td><td>%s</td></tr>' % (
            E(name), res, res, E(" ".join(doc)),
            "<br>".join("%s: %s" % (E(k), E(v)) for k, v in info.items()),
            "".join('<div class="FAIL">%s</div>' % E(f) for f in fails), pic))
    o.append("</table></body></html>")
    open(os.path.join(logdir, "summary.html"), "w").write("\n".join(o) + "\n")
    n = sum(r[1] == "PASS" for r in rows)
    print("\npassed %d   failed %d" % (n, len(rows) - n))
    return 0 if allok else 1


if __name__ == "__main__":
    if len(sys.argv) != 3:
        sys.exit(__doc__)
    sys.exit(main(sys.argv[1], sys.argv[2]))
