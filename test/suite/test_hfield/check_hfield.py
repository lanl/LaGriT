#!/usr/bin/env python3
"""check_hfield.py WORKDIR LOGDIR

Judges test_hfield (Python 3, standard library only). Every deck must
finish ("LaGriT successfully completed") with no signal and no MMVERIFY
message. Then, per deck:

  hf01 to hf08  h at every query vertex equals the rule (hflib.hrule)
                with the distances computed here by brute force, to
                1e-9 (relative to h_ceiling)
  hf09, hf10    the control-file mesh and the hfield mesh have the same
                vertices and cells; h on the hfield mesh is the rule
  hf11          each ERROR text is printed, and hf_ok is unchanged
                (list: no sources)
  hf12          the WARNING, and the mesh is made
  hf13          the lattices: n^3 and n^2 vertices over the bounding
                box, h the rule; hf_2d deleted

Writes LOGDIR/<deck>.check, LOGDIR/table.txt, LOGDIR/summary.html and
the pictures LOGDIR/*.svg.
"""
import html
import math
import os
import re
import sys

HERE = os.path.dirname(os.path.abspath(__file__))
sys.path.insert(0, HERE)
sys.path.insert(0, os.path.join(HERE, "..", "cv_dual_tools"))
import hflib  # noqa: E402
import make_hfield as mk  # noqa: E402
from svgdraw import Svg, ramp, Projector  # noqa: E402

E = html.escape
TOL = 1e-9


def const(v):
    return lambda x: v


def compare(fails, V, got, want, ceil, label):
    worst, wi = 0.0, -1
    for i, (g, w) in enumerate(zip(got, want)):
        e = abs(g - w)
        if e > worst:
            worst, wi = e, i
    if worst > TOL * ceil:
        fails.append("%s: h at %s is %.9g, the rule gives %.9g"
                     % (label, tuple(round(c, 4) for c in V[wi]),
                        got[wi], want[wi]))
    return worst


def ck_hf01(work, fails, notes):
    p = mk.P["hf01"]
    V, _, a = hflib.read_avs(os.path.join(work, "hf01_q.inp"))
    src = [(const(p["H"]), lambda x: math.dist(x, (0, 0, 0)), p["band"],
            p["growth"])]
    want = [hflib.hrule(src, p["floor"], p["ceil"], v) for v in V]
    compare(fails, V, a["h"], want, p["ceil"], "near point")
    notes["plot"] = ([v[0] for v in V], a["h"], want)


def ck_hf02(work, fails, notes):
    p = mk.P["hf02"]
    V, _, a = hflib.read_avs(os.path.join(work, "hf02_q.inp"))
    S = [((0, 0, 0), (1, 0, 0)), ((1, 0, 0), (1, 1, 0))]
    src = [(const(p["floor"]),
            lambda x: min(hflib.d_seg(x, s, t) for s, t in S), 0.0,
            p["growth"])]
    want = [hflib.hrule(src, p["floor"], p["ceil"], v) for v in V]
    compare(fails, V, a["h"], want, p["ceil"], "near lines")


def ck_hf03(work, fails, notes):
    p = mk.P["hf03"]
    V, _, a = hflib.read_avs(os.path.join(work, "hf03_q.inp"))
    sq = [(0, 0, 0), (1, 0, 0), (1, 1, 0), (0, 1, 0)]
    src = [(const(p["H"]),
            lambda x: min(hflib.d_tri(x, sq[0], sq[1], sq[2]),
                          hflib.d_tri(x, sq[0], sq[2], sq[3])), 0.0,
            p["growth"])]
    want = [hflib.hrule(src, p["floor"], p["ceil"], v) for v in V]
    compare(fails, V, a["ht"], want, p["ceil"], "near triangles")
    compare(fails, V, a["hq"], want, p["ceil"], "near a quad")


def ck_hf04(work, fails, notes):
    p = mk.P["hf04"]
    xs = p["xs"]
    V, _, a = hflib.read_avs(os.path.join(work, "hf04_q.inp"))
    Vs, _, asrc = hflib.read_avs(os.path.join(work, "hf04_src_out.inp"))
    mean = []
    for i, x in enumerate(xs):
        e = [abs(x - xs[j]) for j in (i - 1, i + 1) if 0 <= j < len(xs)]
        mean.append(sum(e) / len(e))
    got_mean = asrc["h_src"]
    if any(abs(g - m) > 1e-12 for g, m in zip(got_mean, mean)):
        fails.append("edge_lengths_mean of the source: %s, expected %s"
                     % (got_mean, mean))
    P3 = [(x, 0.0, 0.0) for x in xs]

    def H(x):
        j = min(range(len(P3)), key=lambda k: math.dist(x, P3[k]))
        return p["fac"] * mean[j]
    src = [(H, lambda x: min(hflib.d_seg(x, P3[i], P3[i + 1])
                             for i in range(len(P3) - 1)),
            p["band"], p["sgrowth"])]
    want = [hflib.hrule(src, p["floor"], p["ceil"], v) for v in V]
    compare(fails, V, a["h"], want, p["ceil"], "h_attribute")


def ck_hf05(work, fails, notes):
    p = mk.P["hf05"]
    V, _, a = hflib.read_avs(os.path.join(work, "hf05_q.inp"))
    src = [(const(p["H1"]), lambda x: math.dist(x, (0, 0, 0)), 0.0,
            p["growth"]),
           (const(p["H2"]), lambda x: math.dist(x, (1, 0, 0)), 0.0,
            p["growth"])]
    want = [hflib.hrule(src, p["floor"], p["ceil"], v) for v in V]
    compare(fails, V, a["h"], want, p["ceil"], "two sources")
    notes["plot"] = ([v[0] for v in V], a["h"], want)


def ck_hf06(work, fails, notes):
    p = mk.P["hf06"]
    V, _, a = hflib.read_avs(os.path.join(work, "hf06_q.inp"))
    db = lambda x: hflib.d_box(x, (0, 0, 0), (1, 1, 1))  # noqa
    for att, band in (("hq", 0.0), ("ht", p["band"])):
        src = [(const(p["H"]), db, band, p["growth"])]
        want = [hflib.hrule(src, p["floor"], p["ceil"], v) for v in V]
        compare(fails, V, a[att], want, p["ceil"],
                "cube of %s" % ("quads" if att == "hq" else "triangles"))
    # facing in: inside (lt) is the outside of the cube
    def dbin(x):
        if db(x) > 0:
            return 0.0
        return min(min(x[i], 1 - x[i]) for i in range(3))
    src = [(const(p["H"]), dbin, 0.0, p["growth"])]
    want = [hflib.hrule(src, p["floor"], p["ceil"], v) for v in V]
    compare(fails, V, a["hi"], want, p["ceil"], "cube facing in")
    nin = sum(1 for v in V if db(v) == 0)
    notes["count"] = "%d of %d query vertices inside the cube" % (nin,
                                                                  len(V))


def ck_hf07(work, fails, notes):
    p = mk.P["hf07"]
    V, _, a = hflib.read_avs(os.path.join(work, "hf07_q_out.inp"))
    Vs, T, _ = mk.shell_mesh()
    m, _ = mk.shell_inradius()

    def d(x):
        r = math.sqrt(sum(c * c for c in x))
        if 0.5 < r < m:
            return 0.0
        return min(hflib.d_tri(x, Vs[i], Vs[j], Vs[k]) for i, j, k in T)
    src = [(const(p["H"]), d, 0.0, p["growth"])]
    want = [hflib.hrule(src, p["floor"], p["ceil"], v) for v in V]
    compare(fails, V, a["h"], want, p["ceil"], "hollow ball")
    nin = sum(1 for v in V if 0.5 < math.sqrt(sum(c * c for c in v)) < m)
    nhole = sum(1 for v in V if math.sqrt(sum(c * c for c in v)) < 0.5)
    notes["count"] = ("%d query vertices: %d in the shell, %d in the hole"
                      % (len(V), nin, nhole))
    notes["shell"] = (V, a["h"])


def ck_hf08(work, fails, notes):
    p = mk.P["hf08"]
    V, _, a = hflib.read_avs(os.path.join(work, "hf08_q.inp"))
    P = [(x, y, 0.0) for x, y in p["poly"]]

    def d(x):
        return 0.0 if hflib.in_polygon(x, P) else hflib.d_polygon(x, P)
    src = [(const(p["H"]), d, 0.0, p["growth"])]
    want = [hflib.hrule(src, p["floor"], p["ceil"], v) for v in V]
    compare(fails, V, a["hp"], want, p["ceil"], "polygon of vertices")
    compare(fails, V, a["hl"], want, p["ceil"], "loop of lines")
    notes["poly"] = (V, a["hp"], P)


def same_mesh(fails, work, f1, f2, label):
    V1, C1, _ = hflib.read_avs(os.path.join(work, f1))
    V2, C2, _ = hflib.read_avs(os.path.join(work, f2))
    if V1 != V2 or C1 != C2:
        fails.append("%s: the control file and hfield meshes differ "
                     "(%d and %d vertices)" % (label, len(V1), len(V2)))
    if len(V1) < 50:
        fails.append("%s: only %d vertices" % (label, len(V1)))
    return len(V1), len(C1)


def h_on(fails, work, f, floor, ceil, growth, label):
    V, C, a = hflib.read_avs(os.path.join(work, f))
    src = [(const(floor), lambda x: math.dist(x, (0, 0, 0)), 0.0, growth)]
    want = [hflib.hrule(src, floor, ceil, v) for v in V]
    compare(fails, V, a["h"], want, ceil, label)
    return V, C, a["h"]


def ck_hf09(work, fails, notes):
    n = same_mesh(fails, work, "hf09_a.inp", "hf09_b.inp", "2D")
    notes["count"] = "%d vertices, %d triangles in both" % n
    notes["mesh2d"] = h_on(fails, work, "hf09_b_h.inp", 0.02, 0.1, 0.25,
                           "2D mesh")


def ck_hf10(work, fails, notes):
    n1 = same_mesh(fails, work, "hf10_a.inp", "hf10_b.inp", "octree")
    n2 = same_mesh(fails, work, "hf10_c.inp", "hf10_d.inp", "hex_grid")
    notes["count"] = ("octree: %d vertices, %d tets; hex_grid: %d "
                      "vertices, %d tets" % (n1 + n2))
    notes["mesh3d"] = h_on(fails, work, "hf10_b_h.inp", 0.06, 0.2, 0.3,
                           "3D mesh")


def ck_hf11(work, fails, notes, text):
    for _, t in mk.ERRORS:
        if t not in text:
            fails.append('no ERROR "%s"' % t)
    if "(no sources: h is h_ceiling everywhere)" not in text:
        fails.append("hf_ok changed by a refused command")


def ck_hf12(work, fails, notes, text):
    for t in ("hfield hf_low: h_floor 1.000E-02",
              "is below h_spacing_scalar 2.000E-02",
              "h_spacing_scalar is used as the floor"):
        if t not in text:
            fails.append('no "%s"' % t)
    V, C, _ = hflib.read_avs(os.path.join(work, "hf12_b.inp"))
    if len(V) < 50:
        fails.append("only %d vertices" % len(V))
    notes["count"] = "%d vertices" % len(V)


def ck_hf13(work, fails, notes, text):
    p = mk.P["hf13"]
    for f, pts, nexp in (("hf13_3d.inp", [(0, 0, 0), (1, 1, 1)],
                          p["n"] ** 3),
                         ("hf13_2d.inp", [(0, 0, 0), (1, 1, 0)],
                          p["n"] ** 2)):
        V, C, a = hflib.read_avs(os.path.join(work, f))
        if len(V) != nexp:
            fails.append("%s: %d vertices, expected %d" % (f, len(V),
                                                           nexp))
        lo = [min(v[i] for v in V) for i in range(3)]
        hi = [max(v[i] for v in V) for i in range(3)]
        blo = [min(q[i] for q in pts) for i in range(3)]
        bhi = [max(q[i] for q in pts) for i in range(3)]
        if any(abs(x - y) > 1e-12 for x, y in zip(lo + hi, blo + bhi)):
            fails.append("%s: the box is %s to %s" % (f, lo, hi))
        src = [(const(p["H"]), (lambda q: lambda x: math.dist(x, q))(q),
                0.0, p["growth"]) for q in pts]
        want = [hflib.hrule(src, p["floor"], p["ceil"], v) for v in V]
        compare(fails, V, a["h_field"], want, p["ceil"], f)
    if "hfield hf_2d deleted" not in text or \
            "no hfield named hf_2d" not in text:
        fails.append("hfield / delete: hf_2d still listed")


CHECKS = {"hf01": ck_hf01, "hf02": ck_hf02, "hf03": ck_hf03,
          "hf04": ck_hf04, "hf05": ck_hf05, "hf06": ck_hf06,
          "hf07": ck_hf07, "hf08": ck_hf08, "hf09": ck_hf09,
          "hf10": ck_hf10}
TEXT_CHECKS = {"hf11": ck_hf11, "hf12": ck_hf12, "hf13": ck_hf13}


def about():
    out, cur = {}, None
    src = open(os.path.join(HERE, "make_hfield.py")).read()
    for line in src.split('"""')[1].split("\n"):
        m = re.match(r"^  (hf\d\d_\w+)\s+(.*)$", line)
        if m:
            cur = m.group(1)
            out[cur] = m.group(2).strip()
        elif cur and line.startswith("      ") and line.strip():
            out[cur] += " " + line.strip()
        else:
            cur = None
    return out


# ---------------------------------------------------------------- pictures
def pic_plot(path, xs, got, want, title):
    s = Svg(460, 250)
    x0, y0, w, h = 50, 20, 390, 190
    lo, hi = min(xs), max(xs)
    hmax = max(max(got), max(want)) * 1.1
    X = lambda x: x0 + w * (x - lo) / (hi - lo)  # noqa
    Y = lambda v: y0 + h - h * v / hmax  # noqa
    s.line(x0, y0 + h, x0 + w, y0 + h, color="#888")
    s.line(x0, y0, x0, y0 + h, color="#888")
    s.polyline([(X(x), Y(v)) for x, v in zip(xs, want)], color="#999",
               width=4)
    for x, v in zip(xs, got):
        s.circle(X(x), Y(v), 2.5, fill="#1f6feb")
    s.text(x0, 14, title, size=12)
    s.text(x0 + w, y0 + h + 16, "x", size=11, anchor="end")
    s.text(x0 - 6, y0 + 4, "%.2f" % hmax, size=10, anchor="end")
    s.text(x0 - 6, y0 + h, "0", size=10, anchor="end")
    s.text(x0 + w, 14, "grey: the rule   dots: LaGriT", size=10,
           anchor="end")
    s.save(path)


def pic_points2d(path, V, hv, P, title):
    s = Svg(320, 330)
    lo, hi = -0.3, 1.3
    X = lambda x: 20 + 280 * (x - lo) / (hi - lo)  # noqa
    Y = lambda y: 310 - 280 * (y - lo) / (hi - lo)  # noqa
    a, b = min(hv), max(hv)
    if P:
        s.polygon([(X(q[0]), Y(q[1])) for q in P], fill="none",
                  stroke="#333", width=1.5)
    for v, h in zip(V, hv):
        s.circle(X(v[0]), Y(v[1]), 6, fill=ramp((h - a) / (b - a or 1)))
    s.text(20, 16, title, size=12)
    s.save(path)


def pic_mesh2d(path, V, C, hv, title):
    s = Svg(340, 360)
    X = lambda x: 20 + 300 * x  # noqa
    Y = lambda y: 330 - 300 * y  # noqa
    a, b = min(hv), max(hv)
    for t, ids in C:
        hm = sum(hv[i] for i in ids) / len(ids)
        s.polygon([(X(V[i][0]), Y(V[i][1])) for i in ids],
                  fill=ramp((hm - a) / (b - a or 1)), stroke="#333",
                  width=0.5)
    s.text(20, 16, title, size=12)
    s.legend(200, 6, 120, 10, "%.3f" % a, "%.3f" % b, "h")
    s.save(path)


def pic_points3d(path, V, hv, title):
    s = Svg(360, 340)
    pr = Projector(V, 20, 30, 320, 290)
    a, b = min(hv), max(hv)
    order = sorted(range(len(V)), key=lambda i: -V[i][1])
    for i in order:
        x, y = pr(V[i])
        s.circle(x, y, 2.5, fill=ramp((hv[i] - a) / (b - a or 1)))
    s.text(20, 16, title, size=12)
    s.legend(220, 6, 120, 10, "%.3f" % a, "%.3f" % b, "h")
    s.save(path)


def pictures(deck, notes, logdir):
    out = []
    if "plot" in notes:
        f = deck + ".svg"
        pic_plot(os.path.join(logdir, f), *notes["plot"],
                 title="h along the line of query vertices")
        out.append(f)
    if "poly" in notes:
        f = deck + ".svg"
        pic_points2d(os.path.join(logdir, f), *notes["poly"],
                     title="h at the query vertices (dark: small)")
        out.append(f)
    if "shell" in notes:
        V, hv = notes["shell"]
        sel = [i for i, v in enumerate(V) if abs(v[2]) < 0.35]
        f = deck + ".svg"
        pic_points2d(os.path.join(logdir, f),
                     [((V[i][0] + 1.5) / 3 * 1.6 - 0.3,
                       (V[i][1] + 1.5) / 3 * 1.6 - 0.3) for i in sel],
                     [hv[i] for i in sel], None,
                     title="|z| < 0.35, h (dark: the shell)")
        out.append(f)
    if "mesh2d" in notes:
        f = deck + ".svg"
        pic_mesh2d(os.path.join(logdir, f), *notes["mesh2d"],
                   title="2D mesh from hfield, colored by h")
        out.append(f)
    if "mesh3d" in notes:
        f = deck + ".svg"
        V, C, hv = notes["mesh3d"]
        pic_points3d(os.path.join(logdir, f), V, hv,
                     title="3D vertices from hfield, colored by h")
        out.append(f)
    return out


def main(work, logdir):
    ABOUT = about()
    decks = sorted(f[:-4] for f in os.listdir(work)
                   if re.match(r"hf\d\d_\w+\.lgi$", f))
    rows, ok_all = [], True
    for deck in decks:
        key = deck[:4]
        p = os.path.join(work, deck + ".out")
        text = open(p, errors="replace").read() if os.path.isfile(p) else ""
        fails, notes = [], {}
        if "LaGriT successfully completed" not in text:
            fails.append("LaGriT did not finish")
        for bad in ("Program received signal", "MMVERIFY"):
            if bad in text:
                fails.append('"%s" in the output' % bad)
        try:
            if key in CHECKS:
                CHECKS[key](work, fails, notes)
            elif key in TEXT_CHECKS:
                TEXT_CHECKS[key](work, fails, notes, text)
        except Exception as e:  # a missing or broken output file
            fails.append("could not check: %s: %s" % (type(e).__name__, e))
        if key not in ("hf11", "hf13") and "ERROR" in text:
            fails.append("an ERROR in the output")
        msgs = [l.rstrip() for l in text.splitlines()
                if l.startswith(("ERROR hfield", "hfield ", "  h",
                                 "   h_i", "WARNING POISSON", "  1 ",
                                 "  2 ", "  hfield", "  is below",
                                 "  give a", "  h_spacing"))]
        pics = []
        try:
            pics = pictures(deck, notes, logdir)
        except Exception as e:
            fails.append("picture: %s" % e)
        res = "PASS" if not fails else "FAIL"
        ok_all &= not fails
        with open(os.path.join(logdir, deck + ".check"), "w") as f:
            f.write("result %s\n" % res)
            f.writelines("fail %s\n" % x for x in fails)
        rows.append((deck, res, fails, msgs, notes.get("count", ""), pics))
        print("  %-4s  %s%s" % (res, deck, ("  " + "; ".join(fails[:3]))
                                if fails else ""))
    with open(os.path.join(logdir, "table.txt"), "w") as f:
        f.writelines("%s %s\n" % (d, r) for d, r, *_ in rows)
    o = ["""<!doctype html><html><head><meta charset="utf-8">
<meta name="viewport" content="width=device-width, initial-scale=1"><title>hfield</title>
<style>body{font-family:-apple-system,Helvetica,Arial,sans-serif;margin:16px;max-width:1100px;color:#1f2328;background:#fff}
p,li{font-size:14px;line-height:1.45}table{border-collapse:collapse;font-size:12px}td,th{border-bottom:1px solid #d0d7de;
padding:3px 6px;text-align:left;vertical-align:top}th{background:#f6f8fa}.PASS{color:#1a7f37;font-weight:600}
.FAIL{color:#cf222e;font-weight:600}pre{font-size:11px;background:#f6f8fa;padding:6px;margin:2px 0;white-space:pre-wrap}
img{max-width:360px;border:1px solid #d0d7de;margin:2px}</style></head><body>
<p style="margin:0 0 8px;font-size:13px"><a href="../../summary.html">&larr; all suites (top-level summary)</a></p>
<h1 style="font-size:20px">hfield: a spacing h from a few sources, for createpts / poisson_disk</h1>
<p>Each source i gives h_i = H + h_growth &times; max(0, d &minus; h_band), with d the distance to the source
(0 inside a surface for <code>inside, surface</code>); h = min(h_ceiling, max(h_floor, the smallest h_i)).</p>
<p><b>What it does:</b> each deck builds an hfield and evaluates it on query vertices (hfield / eval); the checker
computes the same rule with its own distances (brute force over every cell; inside by radius, box or ray crossing)
and compares. Two decks compare createpts / poisson_disk with a control file and with hfield, the same seed:
the same mesh. The rest check the ERRORs, the WARNING and hfield / dump.</p>
<table><tr><th>deck</th><th>result</th><th>LaGriT's lines</th><th>picture</th></tr>"""]
    for d, r, fails, msgs, count, pics in rows:
        o.append('<tr><td>%s<details><summary>What it does</summary>%s'
                 '</details>%s</td><td class="%s">%s</td><td><pre>%s</pre>'
                 '%s</td><td>%s</td></tr>'
                 % (E(d), E(ABOUT.get(d, "")),
                    ("<div>%s</div>" % E(count)) if count else "", r, r,
                    E("\n".join(msgs[:40]) or "(none)"),
                    "".join('<div class="FAIL">%s</div>' % E(x)
                            for x in fails),
                    "".join('<img src="%s">' % E(f) for f in pics)))
    o.append("</table></body></html>")
    open(os.path.join(logdir, "summary.html"), "w").write("\n".join(o) + "\n")
    n = sum(r == "PASS" for _, r, *_ in rows)
    print("\npassed %d   failed %d" % (n, len(rows) - n))
    return 0 if ok_all else 1


if __name__ == "__main__":
    if len(sys.argv) != 3:
        sys.exit(__doc__)
    sys.exit(main(sys.argv[1], sys.argv[2]))
