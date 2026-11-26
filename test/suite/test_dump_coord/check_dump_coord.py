#!/usr/bin/env python3
"""check_dump_coord.py WORKDIR LOGDIR

Judges the coor and elem lists of FEHM's geometry file (NAME.fehmn,
written by dump / coord and by dump / fehm) against the same mesh
dumped by dump / avs (NAME.inp) in the same deck (Python 3, standard
library only). Each deck's header holds

  # check : NAME | CELL | NS

CELL is the cell type the elem list must hold (tri, quad, tet, hex),
or none: no elem list, and LaGriT's WARNING that says so; NS is the
number of vertices per cell in the list.

A deck PASSES when LaGriT finished, and
  - coor lists every vertex of the AVS file, in its order, with the
    same coordinates (both are written with 17 digits: equal to 1e-15,
    relative);
  - elem has NS vertices a cell and one line per cell of the AVS file,
    each with the vertices of the AVS file's cell, in the same order
    (dump / avs writes cells in the order FEHM takes: for a tet
    LaGriT's 1 2 4 3, for a hex 5 6 7 8 1 2 3 4);
  - and, from the coordinates alone: a triangle or quad is
    counterclockwise in the x-y plane (positive signed area), as FEHM
    wants in 2D; a tet has a negative signed volume (the order 1 2 4
    3 of LaGriT's positive one); a hex lists its top face first (its
    first four vertices higher than its last four);
  - for none: the elem list is empty, and the output has "WARNING: no
    elem list written".
The picture of each deck is the mesh as FEHM gets it: the vertices of
coor and the cells of elem (drawn with draw_meshes.py from
test_lagrit_upstream; a mesh with no elem list shows its vertices).
Writes LOGDIR/<deck>.check, LOGDIR/table.txt and LOGDIR/summary.html.
"""
import glob
import html
import os
import sys

HERE = os.path.dirname(os.path.abspath(__file__))
sys.path.insert(0, os.path.join(HERE, "..", "test_lagrit_upstream"))
try:
    import draw_meshes
except Exception:
    draw_meshes = None
E = html.escape


def read_avs(p):
    """vertices [(x,y,z)], cells [(type, [ids 1-based])]"""
    L = open(p).read().split("\n")
    nn, nc = map(int, L[0].split()[:2])
    V = [tuple(map(float, L[1 + i].split()[1:4])) for i in range(nn)]
    C = []
    for i in range(nc):
        w = L[1 + nn + i].split()
        C.append((w[2].lower(), [int(x) for x in w[3:]]))
    return V, C


def read_fehmn(p):
    """vertices [(x,y,z)], ns, cells [[ids 1-based]]; raises on a bad
    file"""
    L = [l.rstrip("\n") for l in open(p)]
    k = L.index("coor")
    nn = int(L[k + 1].split()[0])
    V = []
    for i in range(nn):
        w = L[k + 2 + i].split()
        assert int(w[0]) == i + 1, "coor line %d numbered %s" % (i + 1, w[0])
        V.append(tuple(map(float, w[1:4])))
    k = L.index("elem")
    w = L[k + 1].split()
    if not w:                                   # no elem list
        return V, 0, []
    ns, ne = int(w[0]), int(w[1])
    C = []
    for i in range(ne):
        w = L[k + 2 + i].split()
        assert int(w[0]) == i + 1, "elem line %d numbered %s" % (i + 1, w[0])
        C.append([int(x) for x in w[1:]])
    return V, ns, C


def area2(V, c):
    """twice the signed area of polygon c (1-based) in the x-y plane"""
    s = 0.0
    for a, b in zip(c, c[1:] + c[:1]):
        s += V[a - 1][0] * V[b - 1][1] - V[b - 1][0] * V[a - 1][1]
    return s


def vol6(V, c):
    """six times the signed volume of tet c (1-based)"""
    a, b, d, e = [V[i - 1] for i in c]
    u = [b[i] - a[i] for i in range(3)]
    v = [d[i] - a[i] for i in range(3)]
    w = [e[i] - a[i] for i in range(3)]
    return (u[0] * (v[1] * w[2] - v[2] * w[1]) - u[1] * (v[0] * w[2] - v[2] * w[0])
            + u[2] * (v[0] * w[1] - v[1] * w[0]))


def oriented(V, cell, f):
    """True when cell f (1-based, as in elem) is in FEHM's orientation"""
    if cell in ("tri", "quad"):
        return area2(V, f) > 0
    if cell == "tet":
        return vol6(V, f) < 0
    if cell == "hex":
        z = [V[i - 1][2] for i in f]
        return min(z[:4]) > max(z[4:])
    return True


def write_avs(p, V, cell, C):
    """the mesh FEHM gets, as an AVS file for the picture"""
    with open(p, "w") as f:
        f.write("%d %d 0 0 0\n" % (len(V), len(C)))
        for i, v in enumerate(V):
            f.write("%d %r %r %r\n" % ((i + 1,) + v))
        for i, c in enumerate(C):
            f.write("%d 1 %s %s\n" % (i + 1, cell, " ".join(map(str, c))))


def header(deck):
    for l in open(deck):
        if l.startswith("# check"):
            name, cell, ns = [x.strip() for x in l.split(":", 1)[1].split("|")]
            return name, cell, int(ns)
    raise SystemExit("%s: no '# check' line" % deck)


def about(deck):
    """the deck's description: its header lines between the title and
    the check line"""
    out = []
    for l in open(deck).read().split("\n")[3:]:
        if l.startswith("# check") or not l.startswith("#"):
            break
        out.append(l[1:].strip())
    return " ".join(x for x in out if x)


def main(work, logdir):
    rows, ok_all = [], True
    for deck in sorted(glob.glob(os.path.join(HERE, "dc[0-9]*.lgi"))):
        d = os.path.basename(deck)[:-4]
        name, cell, ns = header(deck)
        out = os.path.join(work, d + ".out")
        text = open(out, errors="replace").read() if os.path.isfile(out) else ""
        fails, what = [], ""
        if "LaGriT successfully completed" not in text:
            fails.append("LaGriT did not finish")
        fe, av = os.path.join(work, name + ".fehmn"), os.path.join(work, name + ".inp")
        try:
            VF, nsf, CF = read_fehmn(fe)
            VA, CA = read_avs(av)
        except Exception as e:
            fails.append("cannot read %s or %s: %s" % (name + ".fehmn", name + ".inp", e))
            VF, nsf, CF, VA, CA = [], 0, [], [], []
        if VA:
            if len(VF) != len(VA):
                fails.append("coor has %d vertices, the mesh %d" % (len(VF), len(VA)))
            else:
                bad = sum(1 for a, b in zip(VF, VA)
                          if any(abs(p - q) > 1e-15 * max(1.0, abs(q)) for p, q in zip(a, b)))
                if bad:
                    fails.append("%d vertices differ from the mesh" % bad)
            if cell == "none":
                if CF:
                    fails.append("elem has %d cells, expected none" % len(CF))
                if "WARNING: no elem list written" not in text:
                    fails.append('no "WARNING: no elem list written"')
                what = "no elem list; %d vertices" % len(VF)
            else:
                if nsf != ns:
                    fails.append("elem has %d vertices a cell, expected %d" % (nsf, ns))
                if len(CF) != len(CA):
                    fails.append("elem has %d cells, the mesh %d" % (len(CF), len(CA)))
                wrong = sum(1 for (t, c), f in zip(CA, CF) if t != cell or f != c)
                if wrong:
                    fails.append("%d cells differ from the mesh's" % wrong)
                bad = sum(1 for f in CF if len(f) != ns or not oriented(VF, cell, f))
                if bad:
                    fails.append("%d cells not in FEHM's orientation" % bad)
                what = "%d %s, %d vertices a cell; %d vertices" % (len(CF), cell, nsf, len(VF))
        pic = ""
        if draw_meshes is not None and VF:
            tmp = os.path.join(logdir, d + "_fehm.inp")
            write_avs(tmp, VF, cell if cell != "none" else "tri", CF)
            svg = d + ".svg"
            try:
                draw_meshes.draw(tmp, os.path.join(logdir, svg),
                                 "%s: the mesh in %s.fehmn" % (d, name), color="none")
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
<meta name="viewport" content="width=device-width, initial-scale=1"><title>dump / coord</title>
<style>body{font-family:-apple-system,Helvetica,Arial,sans-serif;margin:16px;max-width:1100px;color:#1f2328;background:#fff}
p,li{font-size:14px;line-height:1.45}table{border-collapse:collapse;font-size:12px}td,th{border-bottom:1px solid #d0d7de;
padding:3px 6px;text-align:left;vertical-align:top}th{background:#f6f8fa}.PASS{color:#1a7f37;font-weight:600}
.FAIL{color:#cf222e;font-weight:600}</style></head><body>
<p style="margin:0 0 8px;font-size:13px"><a href="../../summary.html">&larr; all suites (top-level summary)</a></p>
<h1 style="font-size:20px">dump / coord and dump / fehm: FEHM's coor and elem lists</h1>
<p>FEHM's geometry file (NAME.fehmn) lists the vertices (coor) and the cells (elem). Each deck writes it with
dump / coord (dc06: dump / fehm) and the same mesh with dump / avs; the checker compares the two: every vertex, every
cell in FEHM's order of LaGriT's, and 2D cells counterclockwise. Quads had an empty elem list (GitHub #289); cells FEHM's
list does not take (prisms) now get a WARNING instead of an empty list without a word.</p>
<p><b>What it does:</b> each picture is the mesh rebuilt from the .fehmn file alone: the vertices of coor joined by the
cells of elem, so a missing or scrambled elem list shows.</p>
<table><tr><th>deck</th><th>result</th><th>elem list</th><th>the mesh in the .fehmn</th></tr>"""]
    for d, r, fails, what, pic, ab in rows:
        o.append('<tr><td>%s<details><summary>What it does</summary>%s</details></td><td class="%s">%s</td><td>%s%s</td><td>%s</td></tr>'
                 % (E(d), E(ab), r, r, E(what),
                    "".join('<div class="FAIL">%s</div>' % E(x) for x in fails), pic))
    o.append("</table></body></html>")
    open(os.path.join(logdir, "summary.html"), "w").write("\n".join(o) + "\n")
    n = sum(r[1] == "PASS" for r in rows)
    print("\npassed %d   failed %d" % (n, len(rows) - n))
    return 0 if ok_all else 1


if __name__ == "__main__":
    if len(sys.argv) != 3:
        sys.exit(__doc__)
    sys.exit(main(sys.argv[1], sys.argv[2]))
