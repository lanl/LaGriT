#!/usr/bin/env python3
"""make_page.py LOGDIR [WORKDIR]: LOGDIR/summary.html from table.txt
and the .check files (one row per deck, its checker lines in a
fold-out), with a picture of each deck's output mesh coloured by the
attribute it checks (drawn with test_lagrit_upstream/draw_meshes.py).
Tet meshes are cut open (only the tets with centre y > 0.5) so the
inside of the cube shows; a vertex attribute is shown on each tet as
the mean over its vertices. A picture is never a failure."""
import glob
import html
import os
import sys


def cut_open(src, dst):
    """the tets with centre y > 0.5, vertex and cell data kept (AVS UCD:
    header, vertices, cells, vertex data block, cell data block)"""
    L = open(src).read().split("\n")
    h = L[0].split()
    nn, nc, nnd, ncd = int(h[0]), int(h[1]), int(h[2]), int(h[3])
    V = [[float(x) for x in l.split()[1:4]] for l in L[1:1 + nn]]
    keep = []
    for c, l in enumerate(L[1 + nn:1 + nn + nc]):
        ids = [int(x) - 1 for x in l.split()[3:]]
        if sum(V[i][1] for i in ids) / len(ids) > 0.5:
            keep.append(c)
    k = 1 + nn + nc
    vdata, cdata = [], []
    if nnd:
        na = int(L[k].split()[0])
        vdata = L[k:k + 1 + na + nn]
        k += 1 + na + nn
    if ncd:
        na = int(L[k].split()[0])
        rows = L[k + 1 + na:k + 1 + na + nc]
        cdata = L[k:k + 1 + na] + [rows[c] for c in keep]
    cells = [L[1 + nn + c] for c in keep]
    head = "%d %d %d %d 0" % (nn, len(keep), nnd, ncd)
    open(dst, "w").write("\n".join([head] + L[1:1 + nn] + cells + vdata + cdata) + "\n")


def cut_with_cell_mean(src, dst, attr):
    """cut open as cut_open, and colour each kept tet by the mean of a
    VERTEX attribute over its vertices (dudded vertices, itp1 = 21, left
    out of the mean: interpolate skips them and leaves a flag value), so
    the field shows on the faces; the only cell attribute written is
    <attr>_mean"""
    L = open(src).read().split("\n")
    h = L[0].split()
    nn, nc, nnd = int(h[0]), int(h[1]), int(h[2])
    V = [[float(x) for x in l.split()[1:4]] for l in L[1:1 + nn]]
    k = 1 + nn + nc
    na = int(L[k].split()[0])
    names = [L[k + 1 + i].split(",")[0].strip() for i in range(na)]
    rows = [l.split()[1:] for l in L[k + 1 + na:k + 1 + na + nn]]
    val = [float(r[names.index(attr)]) for r in rows]
    itp = [float(r[names.index("itp1")]) for r in rows] if "itp1" in names else [0] * nn
    cells, means = [], []
    for l in L[1 + nn:1 + nn + nc]:
        ids = [int(x) - 1 for x in l.split()[3:]]
        if sum(V[i][1] for i in ids) / len(ids) <= 0.5:
            continue
        good = [val[i] for i in ids if itp[i] != 21] or [val[i] for i in ids]
        cells.append(l)
        means.append(sum(good) / len(good))
    out = ["%d %d 0 1 0" % (nn, len(cells))] + L[1:1 + nn] + cells
    out += ["1 1", "%s_mean, real" % attr]
    out += ["%d %.10e" % (i + 1, m) for i, m in enumerate(means)]
    open(dst, "w").write("\n".join(out) + "\n")


def picture(name, deck, logdir, workdir):
    """logdir/<name>.svg from the deck's output, or '' """
    spec = None
    for l in open(deck):
        if l.startswith("# check:"):
            spec = [x.strip() for x in l.split(":", 1)[1].split("|")]
    if not spec or not workdir:
        return ""
    kind, out, attr = spec[0], spec[1], spec[2].split(",")[0]
    src = os.path.join(workdir, out)
    if not os.path.isfile(src):
        return ""
    here = os.path.dirname(os.path.abspath(__file__))
    sys.path.insert(0, os.path.join(here, "..", "test_lagrit_upstream"))
    try:
        import draw_meshes
    except ImportError:
        return ""
    f = src
    L = open(src).read().split("\n")
    ncell = int(L[0].split()[1])
    nn = int(L[0].split()[0])
    if ncell and L[1 + nn].split()[2] in ("tri", "quad"):
        ncell = 0                # a 2D mesh: drawn whole, not cut
    color = ("cell:" if kind == "map" else "node:") + attr
    if ncell:
        f = os.path.join(workdir, name + "_cut.inp")
        if kind == "map":
            cut_open(src, f)
        elif False:
            pass
        else:
            cut_with_cell_mean(src, f, attr)
            color = "cell:%s_mean" % attr
    svg = name + ".svg"
    try:
        draw_meshes.draw(f, os.path.join(logdir, svg), "%s: %s" % (name, attr),
                         color=color, dot_scale=0.6)
    except Exception:
        return ""
    return '<a href="%s"><img src="%s" width="300" alt="%s"></a>' % (svg, svg, svg)


def main(logdir, workdir=None):
    rows = [l.rstrip("\n") for l in open(os.path.join(logdir, "table.txt"))][1:]
    here = os.path.dirname(os.path.abspath(__file__))
    o = ["""<!doctype html><html><head><meta charset="utf-8">
<meta name="viewport" content="width=device-width, initial-scale=1">
<title>interpolate and compute</title>
<style>body{font-family:-apple-system,Helvetica,Arial,sans-serif;margin:16px;max-width:1300px;color:#1f2328;background:#fff}
table{border-collapse:collapse;font-size:13px}td,th{border-bottom:1px solid #d0d7de;padding:3px 8px;text-align:left;vertical-align:top}
.PASS{color:#1a7f37;font-weight:600}.KNOWN{color:#9a6700;font-weight:600}.FAIL{color:#cf222e;font-weight:600}pre{font-size:12px;margin:4px 0;white-space:pre-wrap}pre.cmd{background:#f6f8fa;padding:4px 6px;border-radius:4px}</style></head><body>
<p style="margin:0 0 8px;font-size:13px"><a href="../../summary.html">&larr; all suites (top-level summary)</a></p>
<h1 style="font-size:20px">interpolate and compute</h1>
<p style="font-size:14px"><b><a href="demo.html">What each method does: the demonstrations &rarr;</a></b></p>
<p style="font-size:14px">Each picture is the deck's output coloured by the attribute it checks; tet meshes are cut open (only the tets with centre y &gt; 0.5) so the inside of the cube shows, and a vertex value is shown on each tet as the mean over its vertices (a dudded vertex left out).</p>
<p style="font-size:14px">The input meshes a deck reads (src_*.inp, sink_*.inp) are written by make_inputs.py into <code>work/</code> at the start of each run, and every deck is run there; to run one yourself after a run: <code>cd work; lagrit &lt; ic01_voronoi.lgi</code>. (work/ is emptied by the next run; <code>python3 make_inputs.py DIR</code> writes the inputs anywhere.)</p>
<table><tr><th>deck</th><th>result</th><th>detail</th><th>what it tests, and the command</th><th>picture</th></tr>"""]
    for r in rows:
        name, res, det = r[:36].strip(), r[37:43].strip(), r[44:].strip()
        doc = []
        for l in open(os.path.join(here, name + ".lgi")):
            if l.startswith(("# check", "# known")) or not l.startswith("#"):
                break
            if not l.startswith("# " + name):
                doc.append(l[2:].rstrip())
        deck_text = open(os.path.join(here, name + ".lgi")).read()
        # the commands that make the test (interpolate / compute and
        # what is done to the sink before them: dudding, rmpoint,
        # delatt pt_gtg), shown in the row; reading inputs, addatt,
        # dump and finish only in the whole deck, in a fold-out
        # (user, Oct 20 2026)
        cmds = []
        for l in deck_text.splitlines():
            w = [t.strip().lower() for t in l.split("/")]
            if l.startswith("#") or not l.strip() or w[0] in ("read", "dump", "finish"):
                continue
            if w[0] == "cmo" and len(w) > 1 and w[1] in ("addatt", "select"):
                continue
            cmds.append(l)
        chk = open(os.path.join(logdir, name + ".check")).read()
        pic = picture(name, os.path.join(here, name + ".lgi"), logdir, workdir)
        o.append('<tr><td>%s</td><td class="%s">%s</td><td>%s<details><summary>checker</summary><pre>%s</pre></details></td>'
                 '<td>%s<pre class="cmd">%s</pre><details><summary>whole deck (%s.lgi)</summary><pre>%s</pre></details></td><td>%s</td></tr>'
                 % (html.escape(name), res, res, html.escape(det), html.escape(chk), html.escape(" ".join(doc)),
                    html.escape("\n".join(cmds)), html.escape(name), html.escape(deck_text), pic))
    o.append("</table></body></html>")
    open(os.path.join(logdir, "summary.html"), "w").write("\n".join(o) + "\n")


if __name__ == "__main__":
    main(sys.argv[1], sys.argv[2] if len(sys.argv) > 2 else None)
