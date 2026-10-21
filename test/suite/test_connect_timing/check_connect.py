#!/usr/bin/env python3
"""check_connect.py LOGDIR WORKDIR

Judges the connect timing runs (time_connect.py) and writes
LOGDIR/table.txt and LOGDIR/summary.html. Python 3, standard library.

Checks of each case (dimension, number of vertices, order)
  HARD  LaGriT exited 0 and finished; connect made cells; no cell has a
        volume (area) below 0 (quality / volume / lt 0.)
  HARD  for the meshes dumped (up to 32000 vertices in 2D, 8000 in 3D):
        every vertex is in a cell; Delaunay: for every pair of cells
        sharing an edge (2D) or face (3D), the other cell's far vertex
        is not inside the circumcircle (circumsphere): the in-circle
        (in-sphere) determinant, divided by L^4 (L^5), L the longest
        edge from the cell's first vertex, is at most 1e-5. Exactly
        cocircular (cospherical) points make either choice Delaunay;
        LaGriT's tolerance accepts tiny negatives, so values near 1e-7
        are reported, not failed.
  REPORTED  the connect time; the coverage of the box (the total
        area / volume of the cells; the corners make the convex hull
        the unit box, so 1 would be full); the cell counts of the
        orders (Delaunay of points in general position is unique, so
        they differ only where points are nearly cocircular).
Growth
  For each dimension and order, the exponent p of t = a N^p, fitted by
  least squares on log t against log N over the sizes whose connect
  time is at least 0.05 s (shorter times are mostly noise); with fewer
  than three such sizes, the three largest. p = 1 is linear, p < 1
  better, p > 1 worse; p = 2 doubles the time... four-fold per doubling.
Exit status: 1 on any FAIL.
"""
import html
import math
import os
import sys

TOL_DELAUNAY = 1e-5
MIN_T = 0.05
ORDERS = ("random", "xyz", "xyz_bins", "morton")
COL = {"random": "#c0392b", "xyz": "#2e64c8", "xyz_bins": "#b8650a",
       "morton": "#1a8a3a"}


def read_results(path):
    rows = []
    for line in open(path):
        r = {}
        for kv in line.strip().split("; "):
            k, _, v = kv.partition("=")
            r[k] = v
        for k in ("dim", "n", "reps", "rc"):
            r[k] = int(r[k])
        for k in ("t_connect", "t_timed", "t_base"):
            r[k] = float(r[k])
        for k in ("cells", "negative"):
            r[k] = int(r[k]) if r[k] not in ("None", "") else None
        r["volume"] = float(r["volume"]) if r["volume"] not in ("None", "") else None
        r["finished"] = r["finished"] == "True"
        r["dumped"] = r["dumped"] == "True"
        rows.append(r)
    return rows


def read_mesh(path):
    L = open(path).read().split("\n")
    nn, nc = int(L[0].split()[0]), int(L[0].split()[1])
    V = [tuple(float(x) for x in l.split()[1:4]) for l in L[1:1 + nn]]
    C = [[int(x) - 1 for x in l.split()[3:]] for l in L[1 + nn:1 + nn + nc]]
    return V, C


def det3(a, b, c):
    return (a[0] * (b[1] * c[2] - b[2] * c[1]) - a[1] * (b[0] * c[2] - b[2] * c[0])
            + a[2] * (b[0] * c[1] - b[1] * c[0]))


def incircle(a, b, c, d):
    """> 0 when d is inside the circle through a, b, c (counterclockwise)"""
    rows = []
    for p in (a, b, c):
        x, y = p[0] - d[0], p[1] - d[1]
        rows.append((x, y, x * x + y * y))
    return det3(*rows)


def insphere(a, b, c, d, e):
    """sign follows the orientation of a, b, c, d: > 0 inside when
    orient(a, b, c, d) > 0"""
    R = []
    for p in (a, b, c, d):
        x, y, z = p[0] - e[0], p[1] - e[1], p[2] - e[2]
        R.append((x, y, z, x * x + y * y + z * z))
    s = 0.0
    for k in range(4):
        sub = [R[i] for i in range(4) if i != k]
        s += ((-1) ** (k + 3)) * R[k][3] * det3(sub[0][:3], sub[1][:3], sub[2][:3])
    return s


def mesh_checks(dim, path):
    """(unused vertices, faces tested, violations > tol, worst value)"""
    V, C = read_mesh(path)
    used = set(i for c in C for i in c)
    unused = len(V) - len(used)
    k = 3 if dim == 2 else 4
    shared = {}
    for t, c in enumerate(C):
        for skip in range(k):
            f = tuple(sorted(c[i] for i in range(k) if i != skip))
            shared.setdefault(f, []).append(t)
    tested, bad, worst = 0, 0, 0.0
    for f, ts in shared.items():
        if len(ts) != 2:
            continue
        a, b = ts
        far = [v for v in C[b] if v not in C[a]]
        if not far:
            continue
        P = [V[i] for i in C[a]]
        e = V[far[0]]
        L = max(math.dist(P[0], P[i]) for i in range(1, k)) or 1.0
        if dim == 2:
            o = ((P[1][0] - P[0][0]) * (P[2][1] - P[0][1])
                 - (P[1][1] - P[0][1]) * (P[2][0] - P[0][0]))
            if o == 0:
                continue
            val = incircle(*P, e) * (1 if o > 0 else -1) / L ** 4
        else:
            o = det3(*(tuple(P[j][q] - P[3][q] for q in range(3)) for j in range(3)))
            if o == 0:
                continue
            val = insphere(*P, e) * (1 if o > 0 else -1) / L ** 5
        tested += 1
        worst = max(worst, val)
        if val > TOL_DELAUNAY:
            bad += 1
    return unused, tested, bad, worst


def fit(rows):
    pts = [(r["n"], r["t_connect"]) for r in rows if r["t_connect"] >= MIN_T]
    if len(pts) < 3:
        pts = sorted([(r["n"], r["t_connect"]) for r in rows
                      if r["t_connect"] > 0])[-3:]
    if len(pts) < 2:
        return None, pts
    xs = [math.log(n) for n, _ in pts]
    ys = [math.log(t) for _, t in pts]
    mx, my = sum(xs) / len(xs), sum(ys) / len(ys)
    sxx = sum((x - mx) ** 2 for x in xs)
    if sxx == 0:
        return None, pts
    p = sum((x - mx) * (y - my) for x, y in zip(xs, ys)) / sxx
    return p, pts


def plot(rows, dim, fits, W=520, H=360):
    data = [r for r in rows if r["dim"] == dim and r["t_connect"] > 0]
    if not data:
        return ""
    ns = [r["n"] for r in data]
    ts = [r["t_connect"] for r in data]
    x0, x1 = math.log10(min(ns)), math.log10(max(ns))
    y0, y1 = math.floor(math.log10(min(ts))), math.ceil(math.log10(max(ts)))
    if x1 == x0:
        x1 = x0 + 1
    ml, mr, mt, mb = 56, 120, 30, 44
    X = lambda n: ml + (math.log10(n) - x0) / (x1 - x0) * (W - ml - mr)
    Y = lambda t: H - mb - (math.log10(t) - y0) / ((y1 - y0) or 1) * (H - mt - mb)
    o = ['<svg xmlns="http://www.w3.org/2000/svg" width="%d" height="%d" '
         'viewBox="0 0 %d %d" font-family="sans-serif" font-size="11">' % (W, H, W, H),
         '<rect width="100%" height="100%" fill="#fff"/>',
         '<text x="%d" y="18" font-size="13" fill="#222">connect, %dD: time '
         'against vertices (log-log)</text>' % (ml, dim)]
    for e in range(int(y0), int(y1) + 1):
        y = Y(10 ** e)
        o.append('<line x1="%d" y1="%.1f" x2="%d" y2="%.1f" stroke="#e3e3e3"/>'
                 % (ml, y, W - mr, y))
        o.append('<text x="%d" y="%.1f" text-anchor="end" fill="#555">%g s</text>'
                 % (ml - 4, y + 4, 10 ** e))
    for n in sorted(set(ns)):
        x = X(n)
        o.append('<line x1="%.1f" y1="%d" x2="%.1f" y2="%d" stroke="#eee"/>'
                 % (x, mt, x, H - mb))
        o.append('<text x="%.1f" y="%d" text-anchor="middle" fill="#555">%s</text>'
                 % (x, H - mb + 14, ("%dk" % (n // 1000)) if n >= 1000 else n))
    o.append('<text x="%d" y="%d" text-anchor="middle" fill="#333">vertices</text>'
             % ((ml + W - mr) / 2, H - 8))
    # reference slopes N^1 and N^2 through the first random point
    ref = sorted([r for r in data if r["order"] == "random"], key=lambda r: r["n"])
    if ref:
        nA, tA = ref[0]["n"], ref[0]["t_connect"]
        nB = max(ns)
        for p, dash in ((1, "4,3"), (2, "1,3")):
            tB = tA * (nB / nA) ** p
            o.append('<line x1="%.1f" y1="%.1f" x2="%.1f" y2="%.1f" stroke="#999" '
                     'stroke-dasharray="%s"/>' % (X(nA), Y(tA), X(nB),
                                                  Y(min(tB, 10 ** y1)), dash))
        o.append('<text x="%d" y="%d" fill="#777">gray: N^1 dashed,</text>'
                 % (W - mr + 6, H - mb - 14))
        o.append('<text x="%d" y="%d" fill="#777">N^2 dotted</text>'
                 % (W - mr + 6, H - mb))
    for k, order in enumerate(ORDERS):
        pts = sorted([(r["n"], r["t_connect"]) for r in data if r["order"] == order])
        if not pts:
            continue
        c = COL[order]
        o.append('<polyline fill="none" stroke="%s" stroke-width="1.8" points="%s"/>'
                 % (c, " ".join("%.1f,%.1f" % (X(n), Y(t)) for n, t in pts)))
        for n, t in pts:
            o.append('<circle cx="%.1f" cy="%.1f" r="3" fill="%s"/>' % (X(n), Y(t), c))
        p = fits.get((dim, order))
        o.append('<text x="%d" y="%d" fill="%s">%s %s</text>'
                 % (W - mr + 6, mt + 16 + 16 * k, c, order,
                    ("N^%.2f" % p) if p is not None else ""))
    o.append("</svg>")
    return "\n".join(o)


COMMENT_JS = None


def comment_js():
    """the comment-box script of test_lagrit_upstream (one copy of the
    code: read from its checker)"""
    here = os.path.dirname(os.path.abspath(__file__))
    src = open(os.path.join(here, "..", "test_lagrit_upstream",
                            "check_upstream.py")).read()
    a = src.index("<script>\n// Per-test comments")
    b = src.index("</script>", a) + len("</script>")
    return src[a:b].replace("var CM_SUITE = 'test_lagrit_upstream';",
                            "var CM_SUITE = 'test_connect_timing';")


def main(argv):
    logdir, work = argv[1:3]
    rows = read_results(os.path.join(logdir, "results.txt"))
    for r in rows:
        r["unused"] = r["tested"] = r["bad"] = None
        r["worst"] = None
        mpath = os.path.join(work, r["dir"], "mesh.inp")
        if r["dumped"] and os.path.isfile(mpath):
            r["unused"], r["tested"], r["bad"], r["worst"] = mesh_checks(r["dim"], mpath)
            os.remove(mpath)                  # large; keep the logs only
        why = []
        if r["rc"] != 0:
            why.append("exit status %d" % r["rc"])
        if not r["finished"]:
            why.append("did not finish")
        if not r["cells"]:
            why.append("no cells")
        if r["negative"]:
            why.append("%d inverted cells" % r["negative"])
        if r["unused"]:
            why.append("%d vertices not in any cell" % r["unused"])
        if r["bad"]:
            why.append("%d faces not Delaunay (worst %.2g)" % (r["bad"], r["worst"]))
        r["why"] = "; ".join(why)
        r["verdict"] = "FAIL" if why else "PASS"
    fits, fitpts = {}, {}
    for dim in (2, 3):
        for order in ORDERS:
            sel = [r for r in rows if r["dim"] == dim and r["order"] == order]
            p, pts = fit(sel)
            fits[(dim, order)] = p
            fitpts[(dim, order)] = pts
    # cell counts across orders
    counts = {}
    for r in rows:
        counts.setdefault((r["dim"], r["n"]), {})[r["order"]] = r["cells"]

    fmt = "%-3s %8s %-8s %10s %9s %9s %10s %s"
    table = [fmt % ("dim", "vertices", "order", "connect_s", "cells", "coverage",
                    "delaunay", "result")]
    for r in rows:
        dl = ("-" if r["worst"] is None else "%.1e" % r["worst"])
        table.append(fmt % ("%dD" % r["dim"], r["n"], r["order"],
                            "%.3f" % r["t_connect"], r["cells"],
                            "%.5f" % r["volume"] if r["volume"] is not None else "-",
                            dl, r["verdict"] + ("  " + r["why"] if r["why"] else "")))
    table.append("")
    for dim in (2, 3):
        table.append("%dD growth: " % dim + ", ".join(
            "%s N^%.2f" % (o, fits[(dim, o)]) if fits[(dim, o)] is not None else
            "%s -" % o for o in ORDERS))
    nf = sum(1 for r in rows if r["verdict"] == "FAIL")
    summary = "pass %d   fail %d   (of %d cases)" % (len(rows) - nf, nf, len(rows))
    open(os.path.join(logdir, "table.txt"), "w").write("\n".join(table) + "\n")
    print("\n".join(table))
    print()
    print("summary page: logs/summary.html")
    print(summary)
    write_page(os.path.join(logdir, "summary.html"), rows, fits, counts, summary)
    return 1 if nf else 0


def cut_half(src, dst):
    """a tet mesh cut open: only the tets whose centre has y > 0.5 (the cut faces the eye), so
    the picture shows the inside (the box faces alone hold only a few
    vertices). AVS UCD: header (nn nc nnd ncd nmd), vertices, cells,
    the vertex data block (nnd > 0: a count line, one name line per
    attribute, nn value lines), the cell data block (dropped here)."""
    L = open(src).read().split("\n")
    h = L[0].split()
    nn, nc, nnd = int(h[0]), int(h[1]), int(h[2])
    V = [[float(x) for x in l.split()[1:4]] for l in L[1:1 + nn]]
    keep = []
    for l in L[1 + nn:1 + nn + nc]:
        ids = [int(x) - 1 for x in l.split()[3:]]
        if sum(V[i][1] for i in ids) / len(ids) > 0.5:
            keep.append(l)
    k = 1 + nn + nc
    vdata = []
    if nnd:
        nat = int(L[k].split()[0])
        vdata = L[k:k + 1 + nat + nn]
    head = "%d %d %d 0 0" % (nn, len(keep), nnd)
    open(dst, "w").write("\n".join([head] + L[1:1 + nn] + keep + vdata) + "\n")


def path_svg(src, title, W=260):
    """the path through the vertices in their order in the mesh object
    (2D), coloured from the first (dark blue) to the last (yellow) on
    the same scale as the mesh pictures"""
    L = open(src).read().split("\n")
    nn = int(L[0].split()[0])
    P = [tuple(float(x) for x in l.split()[1:3]) for l in L[1:1 + nn]]
    sys.path.insert(0, os.path.join(os.path.dirname(os.path.abspath(__file__)),
                                    "..", "test_lagrit_upstream"))
    import draw_meshes
    m, top = 12, 22
    S = W - 2 * m
    o = ['<svg xmlns="http://www.w3.org/2000/svg" width="%d" height="%d" '
         'viewBox="0 0 %d %d" font-family="sans-serif">' % (W, W + top, W, W + top),
         '<rect width="100%" height="100%" fill="#fff"/>',
         '<text x="6" y="14" font-size="11" fill="#222">%s</text>' % html.escape(title),
         '<rect x="%d" y="%d" width="%d" height="%d" fill="none" stroke="#bbb"/>'
         % (m, top, S, S)]
    for i in range(len(P) - 1):
        (x1, y1), (x2, y2) = P[i], P[i + 1]
        r, g, b = draw_meshes.scale_rgb(i / max(len(P) - 2, 1))
        o.append('<line x1="%.1f" y1="%.1f" x2="%.1f" y2="%.1f" '
                 'stroke="rgb(%d,%d,%d)" stroke-width="0.9"/>'
                 % (m + x1 * S, top + (1 - y1) * S, m + x2 * S, top + (1 - y2) * S,
                    r, g, b))
    o.append("</svg>")
    return "\n".join(o)


def pictures(logdir):
    """the small meshes of time_connect.py's pictures(), drawn with
    test_lagrit_upstream/draw_meshes.py: the vertices coloured by their
    number in the mesh object (vnum: blue first, yellow last)"""
    here = os.path.dirname(os.path.abspath(__file__))
    work = os.path.join(here, "work")
    sys.path.insert(0, os.path.join(here, "..", "test_lagrit_upstream"))
    try:
        import draw_meshes
    except ImportError:
        return "<p>(pictures: draw_meshes.py not found)</p>"
    o = ["<h2>The meshes</h2>",
         "<p>Small meshes from the same points (2D: 1000 vertices, 3D: 3000, "
         "cut open: only the tets with centre y &gt; 0.5). Each vertex is "
         "coloured by its number in the mesh object, the order connect "
         "receives the vertices in: dark blue first, yellow last. random: "
         "speckled; xyz: a sweep from x = 0 to 1; xyz_bins: slab after slab "
         "(column after column in 3D); morton: quadrant by quadrant "
         "(octant by octant). The mesh is the same Delaunay mesh in each "
         "(up to near-cocircular ties); only the colours differ.</p>"]
    o.append("<p>The order itself, 2D: a line from each vertex to the next "
             "one in the mesh object (the same 1000 vertices).</p>")
    o.append('<div class="plots">')
    for order in ORDERS:
        src = os.path.join(work, "pic_d2_%s" % order, "pic.inp")
        if os.path.isfile(src):
            name = "path_2d_%s.svg" % order
            open(os.path.join(logdir, name), "w").write(
                path_svg(src, "2D, %s order: the path" % order))
            o.append('<a href="%s"><img src="%s" width="260" alt="%s"></a>'
                     % (name, name, name))
    o.append("</div>")
    for dim in (2, 3):
        o.append('<div class="plots">')
        for order in ORDERS:
            src = os.path.join(work, "pic_d%d_%s" % (dim, order), "pic.inp")
            if not os.path.isfile(src):
                continue
            name = "pic_%dd_%s.svg" % (dim, order)
            f = src
            if dim == 3:
                f = src[:-4] + "_cut.inp"
                cut_half(src, f)
            try:
                draw_meshes.draw(f, os.path.join(logdir, name),
                                 "%dD, %s order" % (dim, order),
                                 color="node:vnum", view="top" if dim == 2 else None,
                                 dot_scale=0.45)
            except Exception as ex:      # a picture is never a failure
                o.append("<p>%s: not drawn (%s)</p>" % (name, html.escape(str(ex))))
                continue
            o.append('<a href="%s"><img src="%s" width="540" alt="%s"></a>'
                     % (name, name, name))
        o.append("</div>")
    o.append('<textarea class="cm" data-test="pictures" data-result="" '
             'placeholder="comment on the pictures (empty = no change)"></textarea>')
    return "\n".join(o)


def write_page(path, rows, fits, counts, summary):
    e = html.escape
    big = max(r["n"] for r in rows) > 16000
    o = ["""<!doctype html><html><head><meta charset="utf-8">
<meta name="viewport" content="width=device-width, initial-scale=1">
<title>connect timing</title>
<style>
body{font-family:-apple-system,Helvetica,Arial,sans-serif;margin:16px;background:#fff;color:#1f2328;max-width:1150px}
h1{font-size:20px;margin:0 0 6px} h2{font-size:16px;margin:18px 0 6px}
p,li{font-size:14px;line-height:1.45}
table{border-collapse:collapse;font-size:13px}
th,td{border-bottom:1px solid #d0d7de;padding:3px 8px;text-align:right}
th{background:#f6f8fa} td.l,th.l{text-align:left}
.PASS{color:#1a7f37;font-weight:600}.FAIL{color:#cf222e;font-weight:600}
.plots{display:flex;flex-wrap:wrap;gap:12px}
.cm{width:100%;box-sizing:border-box;font:13px sans-serif;padding:6px;margin:4px 0 6px;min-height:2.4em;border:1px solid #bbb;border-radius:4px;background:#fffef5}
.cm.has{border-color:#e8a200;background:#fff6dd}
#cbar{position:sticky;top:0;z-index:5;background:#223;color:#fff;padding:8px 12px;margin:0 -16px 12px;display:flex;gap:12px;align-items:center;flex-wrap:wrap}
#cbar button{font:13px sans-serif;padding:4px 10px;cursor:pointer}
#cbar .msg{font-size:12px;opacity:.85}
</style></head><body>
<p style="margin:0 0 8px;font-size:13px"><a href="../../summary.html">&larr; all suites (top-level summary)</a></p>
<div id="cbar"><b>Comments: <span id="ccount">0</span></b>
<button onclick="cmCopy()">Copy all comments</button>
<button onclick="cmSave()">Save as file</button>
<button onclick="cmClear()">Clear all</button>
<span class="msg" id="cmsg">Type in any box; an empty box means no change. Comments are kept in this browser until you clear them.</span></div>
<h1>connect timing: how the time grows with the number of vertices</h1>"""]
    o.append('<p id="runinfo"><b>%s</b> &mdash; %s sizes (%s)</p>'
             % (e(summary), "BIG" if big else "quick",
                "1000 to %d vertices" % max(r["n"] for r in rows)))
    o.append("<p>Uniformly random points in the unit square (2D, triangles) or "
             "cube (3D, tets), with the box corners, the same points in four "
             "orders: <b>random</b>; <b>xyz</b>, LaGriT's multi-key sort / "
             "index on xic yic zic, then reorder (y only breaks ties in x, "
             "and random x never tie, so this is a sweep along x); "
             "<b>xyz_bins</b>, the same multi-key sort on binned coordinates: "
             "x cut into slabs (2D: xbin yic zic) or x and y into columns "
             "(3D: xbin ybin zic), all in LaGriT commands; <b>morton</b>, a "
             "Z-order curve (Python, time_connect.py) that keeps nearby "
             "points together. Only connect is timed (the same "
             "deck without connect is subtracted). The growth exponent p of "
             "t = a N<sup>p</sup>: 1 linear, below 1 better, above 1 worse "
             "(2: each doubling of N takes four times as long).</p>")
    o.append('<table><tr><th class="l">growth</th>%s</tr>' % "".join(
                 "<th>%s</th>" % k for k in ORDERS))
    for dim in (2, 3):
        o.append('<tr><td class="l">%dD</td>%s</tr>' % (dim, "".join(
            "<td>N<sup>%.2f</sup></td>" % fits[(dim, k)] if fits[(dim, k)] is not None
            else "<td>-</td>" for k in ORDERS)))
    o.append("</table>")
    if not big:
        o.append("<p>Quick sizes only (to 16000 vertices): the exponents are "
                 "rough; run with BIG=1 for 1000 to 256000.</p>")
    o.append('<textarea class="cm" data-test="growth" data-result="%s" '
             'placeholder="comment on the growth (empty = no change)"></textarea>'
             % e(summary))
    o.append('<div class="plots">%s%s</div>' % (plot(rows, 2, fits), plot(rows, 3, fits)))
    o.append('<textarea class="cm" data-test="plots" data-result="" '
             'placeholder="comment on the plots (empty = no change)"></textarea>')
    o.append(pictures(os.path.dirname(path)))
    o.append("<h2>Every case</h2>")
    o.append('<table><tr><th class="l">dim</th><th>vertices</th><th class="l">order</th>'
             '<th>connect s</th><th>slope</th><th>runs</th><th>cells</th><th>coverage</th>'
             '<th>inverted</th><th>unused</th><th>Delaunay worst</th>'
             '<th class="l">result</th></tr>')
    prev = {}
    for r in rows:
        k = (r["dim"], r["order"])
        p = prev.get(k)
        r["slope"] = (math.log(r["t_connect"] / p[1]) / math.log(r["n"] / p[0])
                      if p and p[1] > 0 and r["t_connect"] > 0 else None)
        prev[k] = (r["n"], r["t_connect"])
    for r in rows:
        cc = counts[(r["dim"], r["n"])]
        diff = len(set(v for v in cc.values() if v is not None)) > 1
        o.append('<tr><td class="l">%dD</td><td>%d</td><td class="l" style="color:%s">%s</td>'
                 '<td>%.3f</td><td>%s</td><td>%d</td><td>%s%s</td><td>%s</td><td>%s</td><td>%s</td>'
                 '<td>%s</td><td class="l"><span class="%s">%s</span> %s</td></tr>'
                 % (r["dim"], r["n"], COL[r["order"]], r["order"], r["t_connect"],
                    "%.2f" % r["slope"] if r["slope"] is not None else "-",
                    r["reps"], r["cells"], "*" if diff else "",
                    "%.5f" % r["volume"] if r["volume"] is not None else "-",
                    r["negative"] if r["negative"] is not None else "-",
                    r["unused"] if r["unused"] is not None else "-",
                    "%.1e" % r["worst"] if r["worst"] is not None else "-",
                    r["verdict"], r["verdict"], e(r["why"])))
    o.append("</table>")
    o.append("<p>slope: the growth from the size before, log(t2/t1) / "
             "log(N2/N1): 1 = the time doubles with N, 2 = it quadruples "
             "(below about 0.05 s the times are mostly noise).</p>")
    o.append("<p>cells*: the orders give slightly different cell counts "
             "for this size. Points in general position have one Delaunay "
             "mesh, but near-cocircular (cospherical) groups of points can be "
             "split either way within LaGriT's tolerance; the Delaunay column "
             "shows how close (the largest in-circle / in-sphere value found, "
             "scaled by the cell size; 0 or negative = exactly Delaunay, "
             "failure above 1e-5). Coverage: the total area (volume) of the "
             "cells; the box corners make the convex hull the unit box, so "
             "full coverage would be 1. connect does not always reach the hull "
             "(LaGriT's documentation: &quot;A convex geometry is not "
             "guaranteed&quot;): cells along the box faces are lost with the "
             "temporary enclosing tet, fewer as N grows.</p>")
    o.append('<textarea class="cm" data-test="cases" data-result="" '
             'placeholder="comment on the cases (empty = no change)"></textarea>')
    o.append(comment_js())
    o.append("</body></html>")
    open(path, "w").write("\n".join(o) + "\n")


if __name__ == "__main__":
    sys.exit(main(sys.argv))
