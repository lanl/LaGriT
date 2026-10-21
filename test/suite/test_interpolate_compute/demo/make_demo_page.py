#!/usr/bin/env python3
"""make_demo_page.py WORKDIR OUTDIR

Draws the interpolate / compute demonstrations (demo.lgi) as one page,
OUTDIR/demo.html: each result as a picture made of its pixels (a PNG
written with the standard library) with the source drawn over it in
SVG. Python 3, standard library only. Nothing here is judged.
"""
import base64
import colorsys
import html
import math
import os
import struct
import sys
import zlib

PX = 4          # screen pixels per grid pixel


def read_avs(path):
    L = open(path).read().split("\n")
    h = L[0].split()
    nn, nc, nnd, ncd = int(h[0]), int(h[1]), int(h[2]), int(h[3])
    V = [tuple(float(x) for x in l.split()[1:4]) for l in L[1:1 + nn]]
    C = [[int(x) - 1 for x in l.split()[3:]] for l in L[1 + nn:1 + nn + nc]]
    k = 1 + nn + nc
    va, ca = {}, {}
    for count, store, n in ((nnd, va, nn), (ncd, ca, nc)):
        if not count:
            continue
        na = int(L[k].split()[0])
        names = [L[k + 1 + i].split(",")[0].strip() for i in range(na)]
        rows = [l.split()[1:] for l in L[k + 1 + na:k + 1 + na + n]]
        for j, name in enumerate(names):
            store[name] = [float(r[j]) for r in rows]
        k += 1 + na + n
    return V, C, va, ca


def png(rows):
    """rows of (r, g, b) -> PNG bytes (no compression tricks)"""
    w, h = len(rows[0]), len(rows)
    raw = b"".join(b"\x00" + bytes(c for px in row for c in px) for row in rows)
    def chunk(t, data):
        return (struct.pack(">I", len(data)) + t + data
                + struct.pack(">I", zlib.crc32(t + data) & 0xffffffff))
    return (b"\x89PNG\r\n\x1a\n" + chunk(b"IHDR", struct.pack(">IIBBBBB", w, h, 8, 2, 0, 0, 0))
            + chunk(b"IDAT", zlib.compress(raw, 9)) + chunk(b"IEND", b""))


SEQ = [(0.0, (48, 18, 120)), (0.25, (40, 100, 190)), (0.5, (35, 170, 150)),
       (0.75, (140, 205, 80)), (1.0, (250, 230, 60))]


def seq(u, bands=0):
    u = max(0.0, min(1.0, u))
    if bands:
        u = min(bands - 1, int(u * bands)) / (bands - 1)
    for (a, ca), (b, cb) in zip(SEQ, SEQ[1:]):
        if u <= b:
            t = (u - a) / (b - a)
            return tuple(int(ca[i] + t * (cb[i] - ca[i])) for i in range(3))
    return SEQ[-1][1]


def div(v, m, bands=0):
    """diverging: blue below 0, white at 0, red above"""
    u = max(-1.0, min(1.0, v / m))
    if bands:
        u = (math.floor(u * bands) + .5) / bands
    if u < 0:
        return tuple(int(255 + (-u) * (c - 255)) for c in (40, 90, 180))
    return tuple(int(255 + u * (c - 255)) for c in (190, 50, 40))


def cat(v):
    h = (v * 0.618034) % 1.0
    return tuple(int(255 * c) for c in colorsys.hsv_to_rgb(h, .55, .95))


def raster(V, vals, colour, n):
    """node values on an n x n grid of the unit square -> PNG rows, top
    row = largest y"""
    grid = [[(255, 255, 255)] * n for _ in range(n)]
    for p, v in zip(V, vals):
        i = int(round(p[0] * (n - 1)))
        j = int(round(p[1] * (n - 1)))
        grid[n - 1 - j][i] = colour(v)
    return [[c for c in row for _ in range(PX)] for row in grid for _ in range(PX)]


def panel(rows, title, overlay, size, note=""):
    data = base64.b64encode(png(rows)).decode()
    W = size + 20
    o = ['<svg xmlns="http://www.w3.org/2000/svg" width="%d" height="%d" viewBox="0 0 %d %d" '
         'font-family="sans-serif">' % (W, size + 56, W, size + 56),
         '<text x="10" y="16" font-size="13" fill="#222">%s</text>' % html.escape(title),
         '<text x="10" y="32" font-size="11" fill="#666">%s</text>' % html.escape(note),
         '<g transform="translate(10,40)"><image width="%d" height="%d" '
         'image-rendering="pixelated" href="data:image/png;base64,%s"/>' % (size, size, data),
         '<svg width="%d" height="%d" viewBox="0 0 1 1" preserveAspectRatio="none" overflow="hidden">'
         '<g transform="translate(0,1) scale(1,-1)">%s</g></svg>' % (size, size, overlay),
         '<rect width="%d" height="%d" fill="none" stroke="#999"/></g></svg>' % (size, size)]
    return "\n".join(o)


def edges(V, C, colour="#222", width=0.004):
    seen, o = set(), []
    for c in C:
        for a, b in zip(c, c[1:] + c[:1]):
            k = (min(a, b), max(a, b))
            if k in seen:
                continue
            seen.add(k)
            o.append('<line x1="%.4f" y1="%.4f" x2="%.4f" y2="%.4f" stroke="%s" stroke-width="%s"/>'
                     % (V[a][0], V[a][1], V[b][0], V[b][1], colour, width))
    return "".join(o)


def dots(V, r=0.012, fill="#111", colours=None):
    return "".join('<circle cx="%.4f" cy="%.4f" r="%s" fill="%s" stroke="#000" stroke-width="0.004"/>'
                   % (p[0], p[1], r, colours[i] if colours else fill) for i, p in enumerate(V))


def hexc(c):
    return "#%02x%02x%02x" % c


def main(work, out):
    S = 101 * PX
    sec = []
    # 1 voronoi
    V, _, va, _ = read_avs(os.path.join(work, "demo_voronoi.inp"))
    P, _, pa, _ = read_avs(os.path.join(work, "vor_src.inp"))
    sec.append(("interpolate / voronoi",
                "Every pixel takes the value of the <b>nearest source point</b>. With 25 "
                "points, each carrying its own value (colour), the pixels form the "
                "Voronoi cells of the points: the region closer to that point than to any other.",
                [panel(raster(V, va["vid"], cat, 101), "25 points; each pixel = nearest point's value",
                       dots(P, colours=[hexc(cat(v)) for v in pa["vid"]]), S,
                       "pixels: 101 x 101 points; dots: the source points")]))
    # 2 continuous
    V, _, va, _ = read_avs(os.path.join(work, "demo_continuous.inp"))
    T, TC, ta, _ = read_avs(os.path.join(work, "tri_src.inp"))
    sys.path.insert(0, os.path.dirname(os.path.abspath(__file__)))
    from make_demo_inputs import bump
    exact = [bump(p[0], p[1]) for p in V]
    lo, hi = min(exact), max(exact)
    col = lambda v: seq((v - lo) / (hi - lo), 12)
    ov = edges(T, TC, "#fff", 0.003) + dots(T, 0.008, "#fff")
    flagged = sum(1 for v in va["fbump"] if v > hi + 0.5)
    sec.append(("interpolate / continuous",
                "Each pixel lies in one triangle of a <b>coarse mesh</b> (7 x 7 vertices) "
                "and gets the <b>linear blend</b> of the field at that triangle's three corners. "
                "Left: the field itself. Right: what the coarse mesh can carry. The peak, which falls "
                "between vertices, is cut down to flat facets; the bands (12 equal steps) show the "
                "straight-line contours inside each triangle. "
                "<b>Found by this picture (Oct 20 2026):</b> the bright dotted streaks are "
                "pixels that lie exactly on an edge between two triangles and were found "
                "in neither, so they got the flag value (the largest value + 1) instead of "
                "the interpolated one; %d of 10,201 pixels. Test deck ic22." % flagged,
                [panel(raster(V, exact, col, 101), "the field (exact)", ov, S, "white: the source triangles"),
                 panel(raster(V, va["fbump"], col, 101), "interpolated from the triangles", ov, S,
                       "linear in each triangle")]))
    # 3 map
    M, MC, _, mca = read_avs(os.path.join(work, "demo_map.inp"))
    Q, QC, _, qca = read_avs(os.path.join(work, "quad_src.inp"))
    lo, hi = min(qca["check"]), max(qca["check"])
    col = lambda v: seq((v - lo) / (hi - lo))
    src = "".join('<polygon points="%s" fill="%s" stroke="#222" stroke-width="0.004"/>'
                  % (" ".join("%.4f,%.4f" % (Q[i][0], Q[i][1]) for i in c), hexc(col(v)))
                  for c, v in zip(QC, qca["check"]))
    n = 60
    grid = [[(255, 255, 255)] * n for _ in range(n)]
    for c, v in zip(MC, mca["check"]):
        cx = sum(M[i][0] for i in c) / 4
        cy = sum(M[i][1] for i in c) / 4
        grid[n - 1 - int(round(cy * n - .5))][int(round(cx * n - .5))] = col(v)
    rows = [[c for c in row for _ in range(S // n)] for row in grid for _ in range(S // n)]
    blank = [[(255, 255, 255)] * (S // n * n)] * (S // n * n)
    sec.append(("interpolate / map",
                "A value that belongs to whole <b>cells</b>: each fine cell takes the value "
                "of the coarse source cell that contains its centre. The source is a 5 x 5 grid "
                "turned 25 degrees; the fine 60 x 60 cells inherit its pattern as a staircase "
                "along the turned edges.",
                [panel(blank, "source: turned 5 x 5 cells", src, S // n * n, "one value per cell"),
                 panel(rows, "each fine cell = the source cell at its centre", edges(Q, QC, "#fff", 0.002),
                       S // n * n, "60 x 60 cells; thin white lines: the source cells")]))
    # 4 distance
    V, _, va, _ = read_avs(os.path.join(work, "demo_distance.inp"))
    Cv, _, _, _ = read_avs(os.path.join(work, "curve_src.inp"))
    hi = max(va["dist"])
    sec.append(("compute / distance_field",
                "Each pixel gets its <b>distance to the nearest source point</b>; the points lie "
                "along a sine curve and a circle. Equal-distance bands (12 steps) run parallel to "
                "the curves; dark = close.",
                [panel(raster(V, va["dist"], lambda v: seq(v / hi, 12), 101), "distance to the nearest point",
                       dots(Cv, 0.005, "#fff"), S, "white dots: the 120 source points")]))
    # 5 signed distance
    V, _, va, _ = read_avs(os.path.join(work, "demo_signed.inp"))
    m = max(abs(v) for v in va["sdist"])
    circ = '<circle cx="0.5" cy="0.5" r="0.3" fill="none" stroke="#000" stroke-width="0.004" stroke-dasharray="0.02,0.012"/>'
    inside = va["sdist"][50 * 101 + 50]
    sec.append(("compute / signed_distance_field",
                "A <b>closed surface</b> (a sphere of radius 0.3, made of triangles) and a slice "
                "through its centre. Each pixel gets its distance to the nearest surface vertex, "
                "with a sign for the side it is on: %s inside, %s outside (white = on the surface). "
                "The distance is to the nearest vertex, not to the triangles, so a coarse surface "
                "gives slightly bumpy bands." % ("negative (blue)" if inside < 0 else "positive (red)",
                                                 "positive (red)" if inside < 0 else "negative (blue)"),
                [panel(raster(V, va["sdist"], lambda v: div(v, m, 8), 101),
                       "signed distance on the slice z = 0.5", circ, S, "dashed: where the sphere cuts the slice")]))
    o = ["""<!doctype html><html><head><meta charset="utf-8">
<meta name="viewport" content="width=device-width, initial-scale=1">
<title>interpolate and compute: what they do</title>
<style>body{font-family:-apple-system,Helvetica,Arial,sans-serif;margin:16px;max-width:1000px;color:#1f2328;background:#fff}
h2{font-size:17px;margin:26px 0 4px}p{font-size:14px;line-height:1.45;max-width:850px}.row{display:flex;flex-wrap:wrap;gap:10px}pre.cmd{font-size:12px;background:#f6f8fa;padding:6px 8px;border-radius:4px;white-space:pre-wrap;max-width:850px}</style></head><body>
<p style="margin:0 0 8px;font-size:13px"><a href="summary.html">&larr; interpolate and compute tests</a> &middot; <a href="../../summary.html">all suites</a></p>
<h1 style="font-size:20px">interpolate and compute: what they do</h1>
<p>Demonstrations, not tests: small 2D cases (and one slice through 3D) chosen so the picture shows the method. Run by run_interpolate_tests.sh from demo/demo.lgi in <code>work/</code>, where demo/make_demo_inputs.py writes its input meshes (to run it yourself after a run: <code>cd work; lagrit &lt; demo.lgi</code>); nothing on this page is judged.</p>"""]
    # the LaGriT commands of each case, from demo.lgi (blocks start at
    # "# 1 ", "# 2 ", ... in the same order as the sections)
    blocks, cur = [], None
    for l in open(os.path.join(os.path.dirname(os.path.abspath(__file__)), "demo.lgi")):
        l = l.rstrip("\n")
        if l[:2] == "# " and l[2:3].isdigit():
            cur = []
            blocks.append(cur)
        elif cur is not None and not l.startswith("#") and l.strip() != "finish":
            cur.append(l)
    for i, (title, text, panels) in enumerate(sec):
        cmd = "\n".join(blocks[i]) if i < len(blocks) else ""
        o.append("<h2>%s</h2><p>%s</p><pre class=\"cmd\">%s</pre><div class=\"row\">%s</div>"
                 % (html.escape(title), text, html.escape(cmd), "".join(panels)))
    o.append("</body></html>")
    open(os.path.join(out, "demo.html"), "w").write("\n".join(o) + "\n")


if __name__ == "__main__":
    main(sys.argv[1], sys.argv[2])
