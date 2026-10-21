#!/usr/bin/env python3
"""
render_test_poisson_3d.py - picture of all the test_Poisson_3D.lgi meshes
side by side, each cut open to show its interior tets. Python 3 standard
library only (a small z-buffer rasteriser and PNG writer).

    render_test_poisson_3d.py CASES OUTDIR [--width W] [--height H]
    render_test_poisson_3d.py CASES OUTDIR --cut z [--at Z] [--color ATT]
                                          [--width W]

The second form (used by test_poisson_3d_refine_letters) draws ONE case cut
normal to z, seen from +z, like the user's ParaView view: every tet
whose centroid has z > Z (default: mid box) is removed, and each drawn
triangle takes the colour of its tet's cell attribute ATT (default
edgemax, written by quality/edge_max/y) on ParaView's cool-to-warm
scale, linear from the attribute's min (blue) to max (red), shaded by
the facet's normal so the jagged cut shows relief. Writes
mesh_cut_z.png and mesh_cut_z.svg (with a colour bar) in OUTDIR.

Writes, in OUTDIR:
    mesh_overview.png           grey, like a ParaView crinkle clip
    mesh_overview_quality.png   same view, tets coloured by quality
    mesh_overview.svg           both PNGs embedded, with case labels
                                (inlined into the summary page)

HOW THE PICTURE IS MADE
    Crinkle clip: every tet whose centroid has x > the box's mid x is
    removed, so the cut surface is made of whole tets, not sliced ones.
    What is drawn is the boundary of the remaining tets: the box faces
    (where the face lattice shows as dense mesh lines) and the jagged
    cut surface (where the interior tets show).
    Each case is shifted along z by its ztrans from the .cases file, as
    the deck does for the merged dump, and the view is orthographic from
    the +x, +y, -z side, so case 01 is front left and the rest recede
    to the right.
    Quality colouring: each drawn triangle takes the colour of the tet
    it belongs to, by that tet's smallest dihedral angle:
        red    < 5 deg (sliver)    orange  5 - 10 deg    grey  >= 10 deg
"""

import base64
import math
import os
import struct
import sys
import zlib
from array import array

sys.path.insert(0, os.path.dirname(os.path.abspath(__file__)))
import check_test_poisson_3d as C          # noqa: E402  (shared readers)

BG = (82, 87, 112)
EDGE = (26, 26, 140)
FACE = (232, 232, 226)
RED = (220, 50, 40)
ORANGE = (240, 150, 40)
VIEW = (1.0, 0.55, -0.42)       # direction from the scene to the camera
LIGHT = (0.8, 1.0, -0.2)


def norm(v):
    l = math.sqrt(v[0] * v[0] + v[1] * v[1] + v[2] * v[2])
    return (v[0] / l, v[1] / l, v[2] / l)


def cross(a, b):
    return (a[1] * b[2] - a[2] * b[1], a[2] * b[0] - a[0] * b[2],
            a[0] * b[1] - a[1] * b[0])


def dot(a, b):
    return a[0] * b[0] + a[1] * b[1] + a[2] * b[2]


def min_dihedral(P, t):
    """smallest dihedral angle (deg) of tet t = 4 node indices."""
    a, b, c, d = (P[i] for i in t)
    u = (b[0] - a[0], b[1] - a[1], b[2] - a[2])
    v = (c[0] - a[0], c[1] - a[1], c[2] - a[2])
    w = (d[0] - a[0], d[1] - a[1], d[2] - a[2])
    nd = cross(v, u); nc = cross(u, w); nb = cross(w, v)
    na = (-(nb[0] + nc[0] + nd[0]), -(nb[1] + nc[1] + nd[1]),
          -(nb[2] + nc[2] + nd[2]))
    ns = [na, nb, nc, nd]
    ls = [math.sqrt(dot(n, n)) for n in ns]
    if min(ls) == 0.0:
        return 0.0
    cmax = -1.0
    for i in range(4):
        for j in range(i + 1, 4):
            cmax = max(cmax, -dot(ns[i], ns[j]) / (ls[i] * ls[j]))
    return math.degrees(math.acos(min(1.0, cmax)))


def clipped_surface(m, xcut, zoff, axis=0):
    """Boundary triangles of the tets whose centroid coordinate `axis`
    (0 = x, 2 = z) is < xcut.
    Returns list of (p0, p1, p2, outward normal, owner tet)."""
    x, y, z, t = m["x"], m["y"], m["z"], m["tets"]
    n = m["nn"]
    cc = (x, y, z)[axis]
    open_faces = {}
    for e in range(len(t) // 4):
        q = t[4 * e:4 * e + 4]
        if (cc[q[0]] + cc[q[1]] + cc[q[2]] + cc[q[3]]) * 0.25 >= xcut:
            continue
        for k in range(4):
            f = sorted(q[:k] + q[k + 1:])
            key = (f[0] * n + f[1]) * n + f[2]
            if key in open_faces:
                del open_faces[key]          # shared: interior face
            else:
                open_faces[key] = (e, k)
    out = []
    for key, (e, k) in open_faces.items():
        q = t[4 * e:4 * e + 4]
        opp = q[k]
        f = q[:k] + q[k + 1:]
        p = [(x[i], y[i], z[i] + zoff) for i in f]
        o = (x[opp], y[opp], z[opp] + zoff)
        nrm = cross((p[1][0] - p[0][0], p[1][1] - p[0][1], p[1][2] - p[0][2]),
                    (p[2][0] - p[0][0], p[2][1] - p[0][1], p[2][2] - p[0][2]))
        if dot(nrm, (o[0] - p[0][0], o[1] - p[0][1], o[2] - p[0][2])) > 0:
            nrm = (-nrm[0], -nrm[1], -nrm[2])
        out.append((p[0], p[1], p[2], nrm, e))
    return out


def write_png(path, W, H, rgb):
    raw = bytearray()
    for r in range(H):
        raw.append(0)
        raw += rgb[3 * W * r:3 * W * (r + 1)]

    def chunk(tag, data):
        c = struct.pack(">I", len(data)) + tag + data
        return c + struct.pack(">I", zlib.crc32(tag + data) & 0xffffffff)
    png = (b"\x89PNG\r\n\x1a\n" +
           chunk(b"IHDR", struct.pack(">IIBBBBB", W, H, 8, 2, 0, 0, 0)) +
           chunk(b"IDAT", zlib.compress(bytes(raw), 6)) +
           chunk(b"IEND", b""))
    with open(path, "wb") as f:
        f.write(png)
    return png


# ParaView "Cool to Warm" (Moreland), sampled
COOLWARM = ((0.0, (59, 76, 192)), (0.125, (88, 118, 226)),
            (0.25, (124, 159, 249)), (0.375, (162, 192, 254)),
            (0.5, (221, 221, 221)), (0.625, (243, 190, 165)),
            (0.75, (240, 145, 112)), (0.875, (217, 88, 71)),
            (1.0, (180, 4, 38)))


def coolwarm(f):
    f = 0.0 if f < 0.0 else (1.0 if f > 1.0 else f)
    for (f0, c0), (f1, c1) in zip(COOLWARM, COOLWARM[1:]):
        if f <= f1:
            u = (f - f0) / (f1 - f0)
            return tuple(c0[k] + (c1[k] - c0[k]) * u for k in range(3))
    return COOLWARM[-1][1]


def main_cut_z(argv, cases, outdir):
    W = int(argv[argv.index("--width") + 1]) if "--width" in argv else 1600
    att = argv[argv.index("--color") + 1] if "--color" in argv else "edgemax"
    box = cases["box"]
    zcut = (float(argv[argv.index("--at") + 1]) if "--at" in argv
            else 0.5 * (box[4] + box[5]))
    c = cases["cases"][0]
    if not os.path.exists(c["inp"]):
        sys.exit("render: no %s" % c["inp"])
    m = C.read_avs(c["inp"])
    val = m["cellatt"].get(att)
    if val is None:
        sys.exit("render: no cell attribute %s in %s" % (att, c["inp"]))
    surf = clipped_surface(m, zcut, 0.0, axis=2)
    vlo, vhi = min(val), max(val)
    # top view: screen x = x, screen y = -y, depth = z
    margin, bar = 20, 150
    scale = (W - 2 * margin - bar) / (box[1] - box[0])
    H = int(scale * (box[3] - box[2]) + 2 * margin)
    NP = W * H
    zbuf = array("d", [-1e300]) * NP
    fid = array("i", [-1]) * NP
    lt = norm((-0.35, 0.45, 1.0))
    cols = []
    for k, (p0, p1, p2, nrm, e) in enumerate(surf):
        if nrm[2] <= 0.0:
            cols.append(None)
            continue
        n = norm(nrm)
        sh = 0.35 + 0.65 * max(0.0, dot(n, lt))
        base = coolwarm((val[e] - vlo) / (vhi - vlo) if vhi > vlo else 0.5)
        cols.append(tuple(min(255, int(b * sh)) for b in base))
        pts = [(margin + scale * (p[0] - box[0]),
                margin + scale * (box[3] - p[1]), p[2]) for p in (p0, p1, p2)]
        (x0, y0, z0), (x1, y1, z1), (x2, y2, z2) = pts
        area = (x1 - x0) * (y2 - y0) - (x2 - x0) * (y1 - y0)
        if area == 0.0:
            continue
        ia = 1.0 / area
        ixa = max(0, int(min(x0, x1, x2))); ixb = min(W - 1, int(max(x0, x1, x2)) + 1)
        iya = max(0, int(min(y0, y1, y2))); iyb = min(H - 1, int(max(y0, y1, y2)) + 1)
        for iy in range(iya, iyb + 1):
            py = iy + 0.5
            row = iy * W
            for ix in range(ixa, ixb + 1):
                px = ix + 0.5
                w0 = ((x1 - px) * (y2 - py) - (x2 - px) * (y1 - py)) * ia
                w1 = ((x2 - px) * (y0 - py) - (x0 - px) * (y2 - py)) * ia
                w2 = 1.0 - w0 - w1
                if w0 < 0 or w1 < 0 or w2 < 0:
                    continue
                zz = w0 * z0 + w1 * z1 + w2 * z2
                if zz > zbuf[row + ix]:
                    zbuf[row + ix] = zz
                    fid[row + ix] = k
    rgb = bytearray(3 * NP)
    for i in range(NP):
        k = fid[i]
        rgb[3 * i:3 * i + 3] = bytes(BG if k < 0 else cols[k])
    # colour bar, drawn into the image
    bx0, bx1 = W - bar + 40, W - bar + 64
    by0, by1 = margin + 10, H - margin - 10
    for iy in range(by0, by1 + 1):
        cc = coolwarm((by1 - iy) / float(by1 - by0))
        for ix in range(bx0, bx1 + 1):
            i = iy * W + ix
            rgb[3 * i:3 * i + 3] = bytes(int(v) for v in cc)
    os.makedirs(outdir, exist_ok=True)
    png = write_png(os.path.join(outdir, "mesh_cut_z.png"), W, H, rgb)
    ticks = []
    for k in range(6):
        f = k / 5.0
        yy = by1 - f * (by1 - by0)
        ticks.append('<line x1="%d" x2="%d" y1="%.1f" y2="%.1f" '
                     'stroke="#fff"/><text x="%d" y="%.1f" font-size="18" '
                     'fill="#fff">%.3g</text>'
                     % (bx1, bx1 + 6, yy, yy, bx1 + 10, yy + 6,
                        vlo + f * (vhi - vlo)))
    title = ("%s cut normal to z: tets with centroid z > %g removed, seen "
             "from +z, coloured by %s" % (c["inp"], zcut, att))
    svg = ('<svg xmlns="http://www.w3.org/2000/svg" width="100%%" '
           'viewBox="0 0 %d %d" font-family="sans-serif">'
           '<text x="10" y="24" font-size="22" fill="#222">%s</text>'
           '<image x="0" y="34" width="%d" height="%d" '
           'href="data:image/png;base64,%s"/>'
           '<g transform="translate(0,34)">%s'
           '<text x="%d" y="%d" font-size="18" fill="#fff" '
           'transform="rotate(-90 %d %d)" text-anchor="middle">%s</text>'
           '</g></svg>\n'
           % (W, H + 34, C.esc(title), W, H, base64.b64encode(png).decode(),
              "".join(ticks), bx0 - 12, (by0 + by1) // 2, bx0 - 12,
              (by0 + by1) // 2, C.esc(att)))
    with open(os.path.join(outdir, "mesh_cut_z.svg"), "w") as f:
        f.write(svg)
    print("render: cut z = %g, %d surface triangles, %s %.4g .. %.4g, "
          "%dx%d" % (zcut, len(surf), att, vlo, vhi, W, H))


def main(argv):
    if len(argv) < 3:
        sys.exit(__doc__)
    if "--cut" in argv:
        if argv[argv.index("--cut") + 1] != "z":
            sys.exit("render: only --cut z is supported")
        return main_cut_z(argv, C.read_cases(argv[1]), argv[2])
    cases = C.read_cases(argv[1])
    outdir = argv[2]
    W = int(argv[argv.index("--width") + 1]) if "--width" in argv else 1600
    H = int(argv[argv.index("--height") + 1]) if "--height" in argv else 1000
    box = cases["box"]
    xcut = 0.5 * (box[0] + box[1])

    # ---- collect clipped surfaces --------------------------------------
    tris, qual, labels = [], [], []
    for c in cases["cases"]:
        if not os.path.exists(c["inp"]):
            print("render: skip %s (no %s)" % (c["id"], c["inp"]))
            continue
        m = C.read_avs(c["inp"])
        zoff = c.get("ztrans", 0.0)
        surf = clipped_surface(m, xcut, zoff)
        P = list(zip(m["x"], m["y"], m["z"]))
        t = m["tets"]
        cache = {}
        for (p0, p1, p2, nrm, e) in surf:
            if e not in cache:
                cache[e] = min_dihedral(P, t[4 * e:4 * e + 4])
            tris.append((p0, p1, p2, nrm))
            qual.append(cache[e])
        labels.append((c, zoff))
        print("render: case %s  %d surface triangles" % (c["id"], len(surf)))
    if not tris:
        sys.exit("render: nothing to draw")

    # ---- camera ----------------------------------------------------------
    cv = norm(VIEW)
    right = norm(cross((0.0, 1.0, 0.0), cv))
    if right[2] < 0:
        right = (-right[0], -right[1], -right[2])
    up = norm(cross(cv, right))
    if up[1] < 0:
        up = (-up[0], -up[1], -up[2])
    lt = norm(LIGHT)

    def proj(p):
        return dot(p, right), dot(p, up), dot(p, cv)

    xs, ys = [], []
    for c, zoff in labels:
        for X in (box[0], xcut):
            for Y in (box[2], box[3]):
                for Z in (box[4], box[5]):
                    sx, sy, _ = proj((X, Y, Z + zoff))
                    xs.append(sx); ys.append(sy)
    margin_b = 60
    sxmin, sxmax, symin, symax = min(xs), max(xs), min(ys), max(ys)
    scale = min((W - 60) / (sxmax - sxmin), (H - 40 - margin_b) / (symax - symin))
    ox = (W - scale * (sxmax - sxmin)) / 2 - scale * sxmin
    oy = 20 + scale * symax

    def screen(p):
        sx, sy, sz = proj(p)
        return ox + scale * sx, oy - scale * sy, sz

    # ---- rasterise -------------------------------------------------------
    NP = W * H
    zbuf = array("d", [-1e300]) * NP
    fid = array("i", [-1]) * NP
    front = []
    for k, (p0, p1, p2, nrm) in enumerate(tris):
        if dot(nrm, cv) <= 0.0:
            continue                      # back face
        front.append(k)
        (x0, y0, z0), (x1, y1, z1), (x2, y2, z2) = screen(p0), screen(p1), screen(p2)
        area = (x1 - x0) * (y2 - y0) - (x2 - x0) * (y1 - y0)
        if area == 0.0:
            continue
        ia = 1.0 / area
        ixa = max(0, int(min(x0, x1, x2))); ixb = min(W - 1, int(max(x0, x1, x2)) + 1)
        iya = max(0, int(min(y0, y1, y2))); iyb = min(H - 1, int(max(y0, y1, y2)) + 1)
        for iy in range(iya, iyb + 1):
            py = iy + 0.5
            row = iy * W
            for ix in range(ixa, ixb + 1):
                px = ix + 0.5
                w0 = ((x1 - px) * (y2 - py) - (x2 - px) * (y1 - py)) * ia
                w1 = ((x2 - px) * (y0 - py) - (x0 - px) * (y2 - py)) * ia
                w2 = 1.0 - w0 - w1
                if w0 < 0 or w1 < 0 or w2 < 0:
                    continue
                zz = w0 * z0 + w1 * z1 + w2 * z2
                if zz > zbuf[row + ix]:
                    zbuf[row + ix] = zz
                    fid[row + ix] = k

    # ---- edges, depth tested against the surface ------------------------
    zr = [zbuf[i] for i in range(0, NP, 97) if zbuf[i] > -1e299]
    bias = 2e-3 * ((max(zr) - min(zr)) if zr else 1.0)
    edge = bytearray(NP)
    for k in front:
        sp = [screen(p) for p in tris[k][:3]]
        for a, b in ((0, 1), (1, 2), (2, 0)):
            (xa, ya, za), (xb, yb, zb) = sp[a], sp[b]
            nstep = int(max(abs(xb - xa), abs(yb - ya))) + 1
            for s in range(nstep + 1):
                f = s / nstep
                ix = int(xa + (xb - xa) * f); iy = int(ya + (yb - ya) * f)
                if 0 <= ix < W and 0 <= iy < H:
                    i = iy * W + ix
                    if za + (zb - za) * f >= zbuf[i] - bias:
                        edge[i] = 1

    # ---- compose the two images ------------------------------------------
    shade = {}
    grey, qcol = bytearray(3 * NP), bytearray(3 * NP)
    for i in range(NP):
        k = fid[i]
        if k < 0:
            cg = cq = BG
        elif edge[i]:
            cg = cq = EDGE
        else:
            s = shade.get(k)
            if s is None:
                n = norm(tris[k][3])
                s = shade[k] = 0.45 + 0.55 * max(0.0, dot(n, lt))
            cg = (int(FACE[0] * s), int(FACE[1] * s), int(FACE[2] * s))
            d = qual[k]
            base = RED if d < 5.0 else (ORANGE if d < 10.0 else FACE)
            cq = (int(base[0] * s), int(base[1] * s), int(base[2] * s))
        grey[3 * i:3 * i + 3] = bytes(cg)
        qcol[3 * i:3 * i + 3] = bytes(cq)

    os.makedirs(outdir, exist_ok=True)
    png_g = write_png(os.path.join(outdir, "mesh_overview.png"), W, H, grey)
    png_q = write_png(os.path.join(outdir, "mesh_overview_quality.png"),
                      W, H, qcol)

    # ---- SVG wrapper with labels (for the summary page) ------------------
    def label_svg():
        out = []
        for c, zoff in labels:
            sx, sy, _ = screen((xcut, box[2], 0.5 * (box[4] + box[5]) + zoff))
            buf = ("%g h" % c["buf"]) if c.get("ifbuf") else "off"
            for row, txt in enumerate(("case %s" % c["id"],
                                       "h %g %s" % (c["h"], c.get("field")),
                                       "buffer %s" % buf)):
                out.append('<text x="%.0f" y="%.0f" font-size="%d" '
                           'text-anchor="middle" fill="#fff">%s</text>'
                           % (sx, sy + 30 + 21 * row, 21 if row == 0 else 17,
                              txt))
        return "".join(out)

    def panel(png, y0, title):
        return ('<text x="10" y="%d" font-size="24" fill="#222">%s</text>'
                '<image x="0" y="%d" width="%d" height="%d" '
                'href="data:image/png;base64,%s"/><g transform="translate(0,%d)">'
                '%s</g>' % (y0 - 8, C.esc(title), y0, W, H,
                            base64.b64encode(png).decode(), y0, label_svg()))
    title_g = ("All cases, crinkle-clipped at x = %g: tets with centroid "
               "x > %g removed" % (xcut, xcut))
    title_q = ("Same view, coloured by each tet's smallest dihedral angle: "
               "red < 5 deg (sliver), orange 5-10 deg")
    # width 100%: scales to the summary page; labels sized for that
    svg = ('<svg xmlns="http://www.w3.org/2000/svg" width="100%%" '
           'viewBox="0 0 %d %d" font-family="sans-serif">%s%s</svg>\n'
           % (W, 2 * H + 80, panel(png_g, 34, title_g),
              panel(png_q, H + 74, title_q)))
    with open(os.path.join(outdir, "mesh_overview.svg"), "w") as f:
        f.write(svg)
    print("render: %d triangles drawn, %dx%d" % (len(front), W, H))


if __name__ == "__main__":
    main(sys.argv)
