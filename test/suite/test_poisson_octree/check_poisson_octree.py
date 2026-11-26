#!/usr/bin/env python3
"""check_poisson_octree.py WORK LOGS
check_poisson_octree.py run LAGRIT WORK DECK


test_poisson_octree checker (see 00README). Reads the vertex files, the
octree file and the LaGriT output the decks left in WORK, checks them,
and writes LOGS/summary.html: what is tested, every check by deck, and
figures: the octree, spacing against h, octree against hex_grid (time,
memory, vertices, tet angles), slices at constant h, and the maximality
report. Exit 0 if every check passes. "run" runs one deck in WORK and
records its time and peak memory (WORK/<deck>.rusage). Python 3,
standard library only.

No reference meshes: random numbers differ between C++ libraries, so the
checks are rules and ranges, not exact coordinates.
"""
import html
import math
import os
import re
import subprocess
import sys
import time

SOURCE = (0.3, 0.6, 0.45)  # the point source in h_point.mlgi

DECKS = [
    ("po01_point_source",
     "The unit box, h_spacing_scalar 0.02, h = 0.02 + 0.2 &times; distance to (0.3, 0.6, 0.45), at most 0.2 "
     "(h_point.mlgi). Writes the octree leaves (octree_dump)."),
    ("po02_small_batch",
     "po01 with octree_batch 50: the h field file runs on many small batches of octree vertices. "
     "h at each vertex must not depend on the batch, so the vertices must be exactly po01's."),
    ("po03_other_seed", "po01 with poisson_seed 2: other vertices, about as many."),
    ("po04_k_factor",
     "po01 with octree_k_factor 1.5: leaves at most h / 1.5 instead of h / 0.75, so a finer, more "
     "accurate octree; about the same vertices."),
    ("po05_read_octree",
     "Reads po01's octree file back into LaGriT as a hex mesh, with h_field_att and the level itetlev."),
    ("po06_constant_h_octree", "h = 0.1 everywhere (no h field file), h_lookup octree."),
    ("po06_constant_h_hex_grid", "The same with h_lookup hex_grid, the original path, for comparison."),
    ("po07_point_hex_grid",
     "po01 with h_lookup hex_grid, for comparison: the same h field, the original lattice and neighbour grid."),
    ("po08_default_graded",
     "po01 without h_lookup: with a control file the default lookup is the octree, so the vertices "
     "must be exactly po01's."),
    ("po09_default_uniform",
     "po06 without h_lookup: for a uniform h the default lookup is hex_grid (the mesh of LaGriT "
     "before V3.4.0), so the vertices must be exactly po06_constant_h_hex_grid's."),
    ("po10_face_lattice",
     "The edge buffer on (the default), a 10 &times; 10 &times; 5 box, h_spacing_scalar 0.05, h = 1 "
     "(h_one.mlgi). The six faces get the outside vertices of a 200 &times; 200 &times; 100 lattice, "
     "158,008 of them. LaGriT V3.4.0 built a hex brick of all 4,000,000 lattice vertices to find them "
     "(about 1.5 GB of memory); now only the face vertices are made. face_lattice uniform: with a "
     "control file the default is now the adaptive lattice (po11)."),
    ("po11_face_adaptive",
     "The edge buffer on, the 10 &times; 10 &times; 5 box, h_spacing_scalar 0.05, h = 0.1 + 0.2 &times; "
     "distance to (2, 5, 0) on the bottom face, at most 1 (h_face.mlgi): face_lattice adaptive, the default "
     "with a control file. Each face is a quad mesh refined with refine (a 2:1 balanced quadtree) until no "
     "quad is longer than face_lattice_factor &times; h at its corners (1.5 by default: face edges about as "
     "long as the tet edges inside); the sampler fills the whole box and keeps POI_EDGE_BUFFER &times; "
     "face_lattice_factor &times; h away from the face vertices."),
    ("po14_face_factor_1",
     "po11 with face_lattice_factor 1: face quads up to h instead of 1.5 h (the default), so the faces are "
     "about twice as fine as the interior, and the gap to the sampled vertices is h."),
    ("po12_face_uniform",
     "po11 with face_lattice uniform, for comparison: the lattice at h_spacing_scalar on the faces and the "
     "sampler in the box moved in by h_spacing_scalar, whatever h is."),
    ("po13_face_large_box",
     "The edge buffer on, a 24 &times; 24 &times; 14 box, h_spacing_scalar 0.05, h = 1 (h_one.mlgi), "
     "face_lattice adaptive by default. A uniform face lattice at 0.05 would be about 1,000,000 vertices "
     "here (it was killed at 6 GB with V3.4.0's brick)."),
]

# po11 to po13: face_lattice adaptive
FACE_SRC = (2.0, 5.0, 0.0)  # the point source of h_face.mlgi
BOX11 = ((0.0, 10.0), (0.0, 10.0), (0.0, 5.0))
BOX13 = ((0.0, 24.0), (0.0, 24.0), (0.0, 14.0))

# po10's box and lattice (po10_face_lattice.lgi)
BOX10 = ((0.0, 10.0), (0.0, 10.0), (0.0, 5.0))
H10 = 0.05

BLUES = ["#dbe9f6", "#b3d1ea", "#7fb0d8", "#4a8cc4", "#2166ac", "#0b3c7a", "#061f45"]
OCT_COLOR, HEX_COLOR, INK, INK2, GRID = "#2166ac", "#e07b00", "#1f2328", "#57606a", "#d0d7de"


def h_point(p):
    return min(0.2, 0.02 + 0.2 * math.dist(p, SOURCE))


def h_const(p):
    return 0.1


def read_points(path):
    with open(path) as f:
        n = int(f.readline().split()[0])
        return [tuple(float(v) for v in f.readline().split()[1:4]) for _ in range(n)]


def read_octree(path):
    """Vertices, hex cells (vertex numbers) and itetlev of an octree
    file written by octree_dump."""
    with open(path) as f:
        nv, nc, nva, nca = [int(v) for v in f.readline().split()[:4]]
        xyz = [tuple(float(v) for v in f.readline().split()[1:4]) for _ in range(nv)]
        cells = [[int(v) - 1 for v in f.readline().split()[3:11]] for _ in range(nc)]
        if nva:
            k = int(f.readline().split()[0])
            for _ in range(k):
                f.readline()
            for _ in range(nv):
                f.readline()
        lev = [1] * nc
        if nca:
            k = int(f.readline().split()[0])
            for _ in range(k):
                f.readline()
            lev = [int(float(f.readline().split()[1])) for _ in range(nc)]
    return xyz, cells, lev


def read_text(path):
    try:
        with open(path, errors="replace") as f:
            return f.read()
    except OSError:
        return ""


def on_face(p):
    return any(abs(c) < 1e-9 or abs(c - 1.0) < 1e-9 for c in p)


def neighbours(points, hfun):
    """For every interior vertex: its nearest interior neighbour's
    distance / max(h) of the two. Also the smallest such ratio over
    pairs of interior vertices, and over pairs with a vertex on a box
    face."""
    cell = 0.2
    grid = {}
    for i, p in enumerate(points):
        grid.setdefault(tuple(int(c // cell) for c in p), []).append(i)
    hs = [hfun(p) for p in points]
    nn = [math.inf] * len(points)
    best_in, best_face = math.inf, math.inf
    for i, p in enumerate(points):
        k = tuple(int(c // cell) for c in p)
        for dx in (-1, 0, 1):
            for dy in (-1, 0, 1):
                for dz in (-1, 0, 1):
                    for j in grid.get((k[0] + dx, k[1] + dy, k[2] + dz), ()):
                        if j == i:
                            continue
                        r = math.dist(p, points[j]) / max(hs[i], hs[j])
                        face = on_face(p) or on_face(points[j])
                        if not face:
                            nn[i] = min(nn[i], r)
                        if j < i:
                            continue
                        if face:
                            best_face = min(best_face, r)
                        else:
                            best_in = min(best_in, r)
    interior_nn = [nn[i] for i, p in enumerate(points) if not on_face(p)]
    return best_in, best_face, interior_nn


def median(v):
    s = sorted(v)
    return s[len(s) // 2] if s else float("nan")


def last_float(pattern, text):
    m = re.findall(pattern, text)
    return float(m[-1]) if m else None


# ------------------------------------------------------------------ figures
def svg_slice(xyz, cells, lev, points, zcut=0.45, slab=0.03):
    """The octree leaves cut by the plane z = zcut, coloured by level,
    with the vertices within slab of the plane."""
    W, H, M = 470, 470, 40
    rects = []
    for c, L in zip(cells, lev):
        zs = [xyz[v][2] for v in c]
        if min(zs) <= zcut < max(zs):
            xs = [xyz[v][0] for v in c]
            ys = [xyz[v][1] for v in c]
            rects.append((min(xs), max(xs), min(ys), max(ys), L))
    if not rects:
        return ""
    x0 = min(r[0] for r in rects); x1 = max(r[1] for r in rects)
    y0 = min(r[2] for r in rects); y1 = max(r[3] for r in rects)
    s = (W - 2 * M) / max(x1 - x0, y1 - y0)

    def X(x):
        return M + (x - x0) * s

    def Y(y):
        return H - M - (y - y0) * s

    lmin, lmax = min(r[4] for r in rects), max(r[4] for r in rects)
    out = ['<svg xmlns="http://www.w3.org/2000/svg" viewBox="0 0 %d %d" width="%d" role="img" '
           'aria-label="octree slice">' % (W + 150, H, W + 150)]
    for (a, b, c, d, L) in rects:
        col = BLUES[min(len(BLUES) - 1, max(0, L - lmin))]
        out.append('<rect x="%.1f" y="%.1f" width="%.1f" height="%.1f" fill="%s" stroke="#fff" stroke-width="0.6"/>'
                   % (X(a), Y(d), (b - a) * s, (d - c) * s, col))
    for p in points:
        if abs(p[2] - zcut) < slab:
            out.append('<circle cx="%.1f" cy="%.1f" r="1.8" fill="%s"/>' % (X(p[0]), Y(p[1]), INK))
    sx, sy = X(SOURCE[0]), Y(SOURCE[1])
    out.append('<path d="M%.1f %.1fh12M%.1f %.1fv12" stroke="#d00" stroke-width="2" transform="translate(-6 -6)"/>'
               % (sx, sy, sx, sy))
    out.append('<rect x="%.1f" y="%.1f" width="%.1f" height="%.1f" fill="none" stroke="%s"/>'
               % (X(0), Y(1), s, s, INK2))
    for v in (0, 0.5, 1):
        out.append('<text x="%.1f" y="%d" font-size="11" fill="%s" text-anchor="middle">x %g</text>'
                   % (X(v), H - M + 16, INK2, v))
        out.append('<text x="%d" y="%.1f" font-size="11" fill="%s" text-anchor="end">y %g</text>'
                   % (M - 4, Y(v) + 4, INK2, v))
    lx = W + 10
    out.append('<text x="%d" y="%d" font-size="12" fill="%s">leaf level</text>' % (lx, M + 4, INK))
    for i, L in enumerate(range(lmin, lmax + 1)):
        col = BLUES[min(len(BLUES) - 1, L - lmin)]
        out.append('<rect x="%d" y="%d" width="14" height="14" fill="%s"/>' % (lx, M + 14 + 20 * i, col))
        out.append('<text x="%d" y="%d" font-size="11" fill="%s">%d</text>' % (lx + 20, M + 25 + 20 * i, INK2, L))
    yy = M + 30 + 20 * (lmax - lmin + 1)
    out.append('<circle cx="%d" cy="%d" r="2.5" fill="%s"/>' % (lx + 7, yy, INK))
    out.append('<text x="%d" y="%d" font-size="11" fill="%s">vertex within</text>' % (lx + 20, yy + 4, INK2))
    out.append('<text x="%d" y="%d" font-size="11" fill="%s">%.2f of the plane</text>' % (lx + 20, yy + 18, INK2, slab))
    out.append('<path d="M%d %dh12M%d %dv12" stroke="#d00" stroke-width="2"/>' % (lx + 1, yy + 34, lx + 7, yy + 28))
    out.append('<text x="%d" y="%d" font-size="11" fill="%s">point source</text>' % (lx + 20, yy + 38, INK2))
    out.append('</svg>')
    return "\n".join(out)


def svg_hist(series, lo=0.9, hi=1.6, width=0.02, title="", xlabel="nearest-neighbour distance / max(h of the two)",
             ref=(1.0, "1 = the closest the rule allows"), what="vertices", tick=0.1, tickfmt="%.1f",
             legend_left=False):
    """Step histograms (fraction of the values in each bin), one line
    per (label, colour, values)."""
    W, H, L, R, T, B = 600, 260, 50, 20, 20, 40
    nb = int(round((hi - lo) / width))
    hists = []
    ymax = 0.0
    for lab, col, vals in series:
        c = [0] * nb
        for v in vals:
            k = int((v - lo) / width)
            if 0 <= k < nb:
                c[k] += 1
        f = [x / max(1, len(vals)) for x in c]
        ymax = max(ymax, max(f) if f else 0)
        hists.append((lab, col, f, vals))
    ymax = ymax * 1.1 or 1

    def X(v):
        return L + (v - lo) / (hi - lo) * (W - L - R)

    def Y(f):
        return H - B - f / ymax * (H - T - B)

    out = ['<svg xmlns="http://www.w3.org/2000/svg" viewBox="0 0 %d %d" width="%d" role="img" aria-label="%s">'
           % (W, H, W, html.escape(title))]
    for t in [lo + i * tick for i in range(int(round((hi - lo) / tick)) + 1)]:
        out.append('<line x1="%.1f" y1="%d" x2="%.1f" y2="%d" stroke="%s" stroke-width="0.5"/>'
                   % (X(t), T, X(t), H - B, GRID))
        out.append(('<text x="%.1f" y="%d" font-size="11" fill="%s" text-anchor="middle">' + tickfmt + '</text>')
                   % (X(t), H - B + 15, INK2, t))
    if ref:
        out.append('<line x1="%.1f" y1="%d" x2="%.1f" y2="%d" stroke="#d00" stroke-dasharray="4 3"/>'
                   % (X(ref[0]), T, X(ref[0]), H - B))
        out.append('<text x="%.1f" y="%d" font-size="11" fill="#d00">%s</text>'
                   % (X(ref[0]) + 4, T + 10, html.escape(ref[1])))
    out.append('<text x="%.1f" y="%d" font-size="11" fill="%s" text-anchor="middle">%s</text>'
               % ((L + W - R) / 2, H - 6, INK2, html.escape(xlabel)))
    out.append('<line x1="%d" y1="%d" x2="%d" y2="%d" stroke="%s"/>' % (L, H - B, W - R, H - B, INK2))
    for k, (lab, col, f, vals) in enumerate(hists):
        d = "M%.1f %.1f" % (X(lo), Y(0))
        for i, v in enumerate(f):
            d += "V%.1fH%.1f" % (Y(v), X(lo + (i + 1) * width))
        d += "V%.1f" % Y(0)
        out.append('<path d="%s" fill="none" stroke="%s" stroke-width="2"/>' % (d, col))
        lx = L + 10 if legend_left else W - R - 250
        out.append('<rect x="%d" y="%d" width="12" height="3" fill="%s"/>' % (lx, T + 22 + 18 * k, col))
        out.append('<text x="%d" y="%d" font-size="11" fill="%s">%s: %d %s, median %.3g</text>'
                   % (lx + 17, T + 27 + 18 * k, INK, html.escape(lab), len(vals), what, median(vals)))
    out.append('</svg>')
    return "\n".join(out)


def svg_tracking(points):
    """Each interior vertex: the distance to its nearest interior
    neighbour against its distance from the point source, with h."""
    W, H, L, R, T, B = 600, 300, 55, 20, 20, 40
    inner = [p for p in points if not on_face(p)]
    cell = 0.2
    grid = {}
    for i, p in enumerate(inner):
        grid.setdefault(tuple(int(c // cell) for c in p), []).append(i)
    xy = []
    for i, p in enumerate(inner):
        k = tuple(int(c // cell) for c in p)
        best = math.inf
        for dx in (-1, 0, 1):
            for dy in (-1, 0, 1):
                for dz in (-1, 0, 1):
                    for j in grid.get((k[0] + dx, k[1] + dy, k[2] + dz), ()):
                        if j != i:
                            best = min(best, math.dist(p, inner[j]))
        if best < math.inf:
            xy.append((math.dist(p, SOURCE), best))
    dmax, ymax = 1.0, 0.3

    def X(d):
        return L + d / dmax * (W - L - R)

    def Y(v):
        return H - B - v / ymax * (H - T - B)

    out = ['<svg xmlns="http://www.w3.org/2000/svg" viewBox="0 0 %d %d" width="%d" role="img" '
           'aria-label="spacing against distance">' % (W, H, W)]
    for t in (0, 0.25, 0.5, 0.75, 1.0):
        out.append('<line x1="%.1f" y1="%d" x2="%.1f" y2="%d" stroke="%s" stroke-width="0.5"/>' % (X(t), T, X(t), H - B, GRID))
        out.append('<text x="%.1f" y="%d" font-size="11" fill="%s" text-anchor="middle">%g</text>' % (X(t), H - B + 15, INK2, t))
    for v in (0, 0.1, 0.2, 0.3):
        out.append('<line x1="%d" y1="%.1f" x2="%d" y2="%.1f" stroke="%s" stroke-width="0.5"/>' % (L, Y(v), W - R, Y(v), GRID))
        out.append('<text x="%d" y="%.1f" font-size="11" fill="%s" text-anchor="end">%g</text>' % (L - 5, Y(v) + 4, INK2, v))
    for d, v in xy:
        out.append('<circle cx="%.1f" cy="%.1f" r="1.8" fill="%s" fill-opacity="0.55"/>' % (X(d), Y(min(v, ymax)), OCT_COLOR))
    path = "M" + " L".join("%.1f %.1f" % (X(d / 100.0), Y(min(0.2, 0.02 + 0.2 * d / 100.0))) for d in range(0, 101))
    out.append('<path d="%s" fill="none" stroke="#d00" stroke-width="2"/>' % path)
    out.append('<text x="%.1f" y="%.1f" font-size="11" fill="#d00">h = 0.02 + 0.2 d, at most 0.2</text>'
               % (X(0.55), Y(0.2) - 6))
    out.append('<text x="%.1f" y="%d" font-size="11" fill="%s" text-anchor="middle">distance d from the point '
               'source</text>' % ((L + W - R) / 2, H - 6, INK2))
    out.append('<text x="14" y="%.1f" font-size="11" fill="%s" transform="rotate(-90 14 %.1f)" '
               'text-anchor="middle">nearest-neighbour distance</text>' % ((T + H - B) / 2, INK2, (T + H - B) / 2))
    out.append('</svg>')
    return "\n".join(out)


def svg_points_pair(pa, pb, la, lb, zc=0.5, slab=0.05):
    """Two square panels: the vertices of pa and pb within slab of z = zc."""
    S, M, G = 300, 26, 30
    W, H = 2 * S + G, S + 2 * M
    out = ['<svg xmlns="http://www.w3.org/2000/svg" viewBox="0 0 %d %d" width="%d" role="img" '
           'aria-label="vertices at constant h">' % (W, H, W)]
    for k, (pp, lab, col) in enumerate(((pa, la, OCT_COLOR), (pb, lb, HEX_COLOR))):
        x0 = k * (S + G)
        sel = [p for p in pp if abs(p[2] - zc) < slab]
        out.append('<rect x="%d" y="%d" width="%d" height="%d" fill="none" stroke="%s"/>' % (x0, M, S, S, INK2))
        for p in sel:
            out.append('<circle cx="%.1f" cy="%.1f" r="3" fill="%s"/>' % (x0 + p[0] * S, M + (1 - p[1]) * S, col))
        out.append('<text x="%d" y="%d" font-size="12" fill="%s">%s: %d vertices in the slab</text>'
                   % (x0, M - 8, INK, html.escape(lab), len(sel)))
    out.append('</svg>')
    return "\n".join(out)


def svg_levels(counts):
    """Leaves per level, as bars."""
    W, H, L, B, T = 420, 170, 40, 30, 14
    n = len(counts)
    cmax = max(counts) or 1
    bw = (W - L - 10) / n
    out = ['<svg xmlns="http://www.w3.org/2000/svg" viewBox="0 0 %d %d" width="%d" role="img" '
           'aria-label="leaves per level">' % (W, H, W)]
    for i, c in enumerate(counts):
        h = c / cmax * (H - B - T)
        out.append('<rect x="%.1f" y="%.1f" width="%.1f" height="%.1f" fill="%s"/>'
                   % (L + i * bw + 4, H - B - h, bw - 8, h, BLUES[min(len(BLUES) - 1, i + 1)]))
        out.append('<text x="%.1f" y="%.1f" font-size="11" fill="%s" text-anchor="middle">%s</text>'
                   % (L + (i + 0.5) * bw, H - B - h - 3, INK, "{:,}".format(c)))
        out.append('<text x="%.1f" y="%d" font-size="11" fill="%s" text-anchor="middle">%d</text>'
                   % (L + (i + 0.5) * bw, H - B + 14, INK2, i))
    out.append('<text x="%d" y="%d" font-size="11" fill="%s">level</text>' % (2, H - B + 14, INK2))
    out.append('</svg>')
    return "\n".join(out)


def read_angles(path):
    """Smallest and largest dihedral angle of every tet (degrees), from
    an attribute-only AVS table with ang_min and ang_max."""
    mins, maxs = [], []
    try:
        with open(path) as f:
            f.readline()
            k = int(f.readline().split()[0])
            for _ in range(k):
                f.readline()
            for line in f:
                w = line.split()
                if len(w) == 3:
                    mins.append(float(w[1]))
                    maxs.append(float(w[2]))
    except (OSError, ValueError, IndexError):
        pass
    return mins, maxs


def face_lattice(box, h):
    """The lattice the edge buffer puts on the faces: n = ceil(L / h)
    vertices along each side, from min to max."""
    n = [int(math.ceil((b[1] - b[0]) / h)) for b in box]
    axes = [[b[0] + i * (b[1] - b[0]) / (k - 1) for i in range(k)] for b, k in zip(box, n)]
    count = n[0] * n[1] * n[2] - max(n[0] - 2, 0) * max(n[1] - 2, 0) * max(n[2] - 2, 0)
    return n, axes, count


def on_box_face(p, box, tol):
    return any(abs(p[a] - box[a][0]) <= tol or abs(p[a] - box[a][1]) <= tol for a in range(3))


def check_face_lattice(points, box, h):
    """(face vertices, how many are off the lattice, smallest distance
    of the other vertices to a face)"""
    n, axes, _ = face_lattice(box, h)
    tol = 1e-9 * max(b[1] - b[0] for b in box)
    face, off, gap = 0, 0, float("inf")
    for p in points:
        if on_box_face(p, box, tol):
            face += 1
            for a in range(3):
                b = box[a]
                i = int(round((p[a] - b[0]) / (b[1] - b[0]) * (n[a] - 1)))
                if not (0 <= i < n[a] and abs(p[a] - axes[a][i]) <= tol):
                    off += 1
                    break
        else:
            gap = min(gap, min(min(p[a] - box[a][0], box[a][1] - p[a]) for a in range(3)))
    return face, off, gap


def svg_face_lattice(points, box):
    """Two panels of po10: the corner of the face z = 0 seen from above,
    and the slice y = 5 seen from the side: the face lattice and the
    sampled vertices."""
    tol = 1e-9 * 10
    face = [p for p in points if on_box_face(p, box, tol)]
    inner = [p for p in points if not on_box_face(p, box, tol)]
    W, M = 640, 28
    out = ['<svg xmlns="http://www.w3.org/2000/svg" viewBox="0 0 %d 360" width="%d" role="img" '
           'aria-label="face lattice of po10">' % (W, W)]
    # panel 1: x, y in [0, 2], the face z = 0 and the vertices with z < 1.5
    S = 280
    out.append('<rect x="0" y="%d" width="%d" height="%d" fill="none" stroke="%s"/>' % (M, S, S, INK2))
    for p in face:
        if p[2] <= tol and p[0] <= 2 and p[1] <= 2:
            out.append('<circle cx="%.1f" cy="%.1f" r="1.2" fill="%s"/>' % (p[0] / 2 * S, M + (1 - p[1] / 2) * S,
                                                                          OCT_COLOR))
    for p in inner:
        if p[2] < 1.5 and p[0] <= 2 and p[1] <= 2:
            out.append('<circle cx="%.1f" cy="%.1f" r="4" fill="%s"/>' % (p[0] / 2 * S, M + (1 - p[1] / 2) * S,
                                                                        HEX_COLOR))
    out.append('<text x="0" y="%d" font-size="12" fill="%s">corner of the face z = 0 (x, y from 0 to 2)</text>'
               % (M - 8, INK))
    # panel 2: the slice |y - 5| < 0.5, x in [0, 10], z in [0, 5]
    x0, S2 = S + 40, W - S - 40
    sc = S2 / 10.0
    out.append('<rect x="%d" y="%d" width="%.0f" height="%.0f" fill="none" stroke="%s"/>'
               % (x0, M, S2, 5 * sc, INK2))
    for p in face:
        if abs(p[1] - 5) < 0.5 and (p[1] > tol and p[1] < 10 - tol):
            out.append('<circle cx="%.1f" cy="%.1f" r="1" fill="%s"/>' % (x0 + p[0] * sc, M + (5 - p[2]) * sc,
                                                                        OCT_COLOR))
    for p in inner:
        if abs(p[1] - 5) < 0.5:
            out.append('<circle cx="%.1f" cy="%.1f" r="3" fill="%s"/>' % (x0 + p[0] * sc, M + (5 - p[2]) * sc,
                                                                        HEX_COLOR))
    out.append('<text x="%d" y="%d" font-size="12" fill="%s">the slice y = 5 &plusmn; 0.5, from the side '
               '(x 0 to 10, z 0 to 5)</text>' % (x0, M - 8, INK))
    ly = M + S + 24
    out.append('<circle cx="6" cy="%d" r="3" fill="%s"/><text x="16" y="%d" font-size="12" fill="%s">face '
               'lattice (%s vertices, 0.05 apart)</text>' % (ly - 4, OCT_COLOR, ly, INK, "{:,}".format(len(face))))
    out.append('<circle cx="%d" cy="%d" r="4" fill="%s"/><text x="%d" y="%d" font-size="12" fill="%s">sampled '
               'vertices (%s, h = 1)</text>' % (x0 + 6, ly - 4, HEX_COLOR, x0 + 16, ly, INK,
                                                 "{:,}".format(len(inner))))
    out.append('</svg>')
    return "\n".join(out)


def h_face(p):
    return min(1.0, 0.1 + 0.2 * math.dist(p, FACE_SRC))


def h_one(p):
    return 1.0


def read_mesh(path):
    """Vertices and tets of an AVS file (dump / avs ... / 1 1 0 0)."""
    with open(path) as f:
        nn, ne = (int(v) for v in f.readline().split()[:2])
        pts = [tuple(float(v) for v in f.readline().split()[1:4]) for _ in range(nn)]
        tets = []
        for _ in range(ne):
            w = f.readline().split()
            if len(w) >= 7 and w[2] == "tet":
                tets.append(tuple(int(v) - 1 for v in w[3:7]))
    return pts, tets


def circumcenter(a, b, c, d):
    r = []
    for p in (b, c, d):
        u = [p[k] - a[k] for k in range(3)]
        r.append(u + [0.5 * (u[0] * u[0] + u[1] * u[1] + u[2] * u[2])])

    def det3(m):
        return (m[0][0] * (m[1][1] * m[2][2] - m[1][2] * m[2][1])
                - m[0][1] * (m[1][0] * m[2][2] - m[1][2] * m[2][0])
                + m[0][2] * (m[1][0] * m[2][1] - m[1][1] * m[2][0]))
    D = det3([row[:3] for row in r])
    if D == 0.0:
        return None
    out = []
    for k in range(3):
        m = [row[:3] for row in r]
        for i in range(3):
            m[i][k] = r[i][3]
        out.append(a[k] + det3(m) / D)
    return out


def grid_pairs(pts, cell):
    """Spatial hash of pts, cell size cell."""
    g = {}
    for i, p in enumerate(pts):
        g.setdefault((int(p[0] // cell), int(p[1] // cell), int(p[2] // cell)), []).append(i)
    return g


def near(g, cell, p, r):
    """Indices in grid g within the cells r around p."""
    n = int(math.ceil(r / cell))
    c = (int(p[0] // cell), int(p[1] // cell), int(p[2] // cell))
    for a in range(c[0] - n, c[0] + n + 1):
        for b in range(c[1] - n, c[1] + n + 1):
            for cc in range(c[2] - n, c[2] + n + 1):
                for j in g.get((a, b, cc), ()):
                    yield j


def face_checks(pts, tets, box, hfun, spacing=True):
    """For face_lattice adaptive: on the faces, the spacing of the face
    vertices against h; the interior vertices' distance to the face
    vertices against h; the tets with a facet on the box, and how many
    of them have their circumcenter (Voronoi point) outside the box."""
    L = max(b[1] - b[0] for b in box)
    tol = 1e-9 * L
    onf = [on_box_face(p, box, tol) for p in pts]
    face = [i for i in range(len(pts)) if onf[i]]
    inner = [i for i in range(len(pts)) if not onf[i]]
    hv = [hfun(p) for p in pts]
    hmax = max(hv)
    cell = hmax
    gf = grid_pairs([pts[i] for i in face], cell) if spacing else {}
    # face spacing: nearest other face vertex on the same box face / h
    sp_max, sp = 0.0, []
    for k, i in enumerate(face if spacing else []):
        p = pts[i]
        best = float("inf")
        for jj in near(gf, cell, p, 1.5 * hv[i]):
            j = face[jj]
            if j == i:
                continue
            q = pts[j]
            same = any(abs(p[a] - b) <= tol and abs(q[a] - b) <= tol for a in range(3) for b in box[a])
            if same:
                best = min(best, math.dist(p, q))
        if best < float("inf"):
            sp.append(best / hv[i])
    sp_max = max(sp) if sp else 0.0
    # interior to face: d / max(h)
    fi_min = float("inf")
    for i in (inner if spacing else []):
        p = pts[i]
        for jj in near(gf, cell, p, 1.5 * hmax):
            j = face[jj]
            fi_min = min(fi_min, math.dist(p, pts[j]) / max(hv[i], hv[j]))
    # boundary tets
    nb = nout = 0
    for t in tets:
        q = [pts[i] for i in t]
        if not any(sum(1 for v in q if abs(v[a] - b) <= tol) >= 3 for a in range(3) for b in box[a]):
            continue
        nb += 1
        c = circumcenter(*q)
        if c is None or any(c[a] < box[a][0] - tol or c[a] > box[a][1] + tol for a in range(3)):
            nout += 1
    return {"face": len(face), "inner": len(inner), "sp": sp, "sp_max": sp_max, "fi_min": fi_min,
            "nb": nb, "nout": nout}


def neg_aij(text):
    m = re.findall(r"negative A\(ij\)\s+(\d+)", text)
    return int(m[-1]) if m else None


def svg_face_pair(ma, mb, la, lb, box):
    """Two columns, po11 and po12: the bottom face z = 0 near the
    source (its triangles: the facets of the tets on the face), and the
    slice y = 5 from the side, face vertices blue, sampled orange."""
    W, M, S = 640, 26, 290
    G = W - 2 * S
    out = ['<svg xmlns="http://www.w3.org/2000/svg" viewBox="0 0 %d %d" width="%d" role="img" '
           'aria-label="face lattice adaptive and uniform">' % (W, S + 2 * M + 14 + S // 2 + 52, W)]
    tol = 1e-9 * 10
    for k, (m, lab) in enumerate(((ma, la), (mb, lb))):
        pts, tets = m
        x0 = k * (S + G)
        # panel 1: bottom face, x 0..4, y 3..7
        X0, X1, Y0, Y1 = 0.0, 4.0, 3.0, 7.0
        sc = S / (X1 - X0)
        out.append('<text x="%d" y="%d" font-size="12" fill="%s">%s: bottom face near (2, 5, 0)</text>'
                   % (x0, M - 8, INK, html.escape(lab)))
        out.append('<rect x="%d" y="%d" width="%d" height="%d" fill="none" stroke="%s"/>' % (x0, M, S, S, INK2))
        segs = set()
        for t in tets:
            b = [i for i in t if abs(pts[i][2]) <= tol]
            if len(b) == 3:
                for a2 in range(3):
                    e = tuple(sorted((b[a2], b[(a2 + 1) % 3])))
                    segs.add(e)
        lines = []
        for i, j in segs:
            p, q = pts[i], pts[j]
            if X0 <= p[0] <= X1 and Y0 <= p[1] <= Y1 and X0 <= q[0] <= X1 and Y0 <= q[1] <= Y1:
                lines.append('M%.1f %.1fL%.1f %.1f' % (x0 + (p[0] - X0) * sc, M + (Y1 - p[1]) * sc,
                                                     x0 + (q[0] - X0) * sc, M + (Y1 - q[1]) * sc))
        out.append('<path d="%s" stroke="%s" stroke-width="0.6" fill="none"/>' % (" ".join(lines), OCT_COLOR))
        sx, sy = x0 + (FACE_SRC[0] - X0) * sc, M + (Y1 - FACE_SRC[1]) * sc
        out.append('<circle cx="%.1f" cy="%.1f" r="4" fill="#c00"/>' % (sx, sy))
        # panel 2: slice y = 5, x 0..10, z 0..5
        top = M + S + M + 14
        sc2 = S / 10.0
        out.append('<text x="%d" y="%d" font-size="12" fill="%s">slice y = 5 &plusmn; 0.25, from the side</text>'
                   % (x0, top - 8, INK))
        out.append('<rect x="%d" y="%d" width="%d" height="%.0f" fill="none" stroke="%s"/>'
                   % (x0, top, S, 5 * sc2, INK2))
        for p in pts:
            if abs(p[1] - 5) < 0.25:
                f = on_box_face(p, box, tol)
                out.append('<circle cx="%.1f" cy="%.1f" r="%s" fill="%s"/>'
                           % (x0 + p[0] * sc2, top + (5 - p[2]) * sc2, "1.2" if f else "2.2",
                              OCT_COLOR if f else HEX_COLOR))
    yc = S + 2 * M + 14 + S // 2 + 22
    out.append('<text x="0" y="%d" font-size="12" fill="%s">top: the triangles of the tets on the bottom '
               'face (red: the source, h = 0.1)</text>' % (yc, INK2))
    out.append('<text x="0" y="%d" font-size="12" fill="%s">bottom: blue face vertices, orange sampled '
               'vertices</text>' % (yc + 16, INK2))
    out.append('</svg>')
    return "\n".join(out)


def read_rusage(work, deck):
    try:
        with open(os.path.join(work, deck + ".rusage")) as f:
            t, m = f.read().split()
            return float(t), float(m)
    except (OSError, ValueError):
        return None, None


def run_deck(lagrit, work, deck):
    """Run one deck in work; write its output to <deck>.out and its time
    (s) and peak memory (bytes) to <deck>.rusage."""
    name = deck[:-4]
    t0 = time.time()
    with open(os.path.join(work, deck)) as fin, open(os.path.join(work, name + ".out"), "w") as fout:
        p = subprocess.Popen([lagrit], stdin=fin, stdout=fout, stderr=subprocess.STDOUT, cwd=work)
        _, status, ru = os.wait4(p.pid, 0)
    peak = ru.ru_maxrss * (1 if sys.platform == "darwin" else 1024)
    with open(os.path.join(work, name + ".rusage"), "w") as f:
        f.write("%.3f %d\n" % (time.time() - t0, peak))
    return 0


# ------------------------------------------------------------------ main
def main():
    if len(sys.argv) == 5 and sys.argv[1] == "run":
        return run_deck(sys.argv[2], sys.argv[3], sys.argv[4])
    work, logs = sys.argv[1], sys.argv[2]
    rows = []

    def check(deck, what, ok, detail):
        rows.append((deck, what, bool(ok), detail))

    out = {}
    for name in sorted(os.listdir(work)):
        if name.endswith(".out") and name != "lagrit.out":
            out[name[:-4]] = read_text(os.path.join(work, name))
    for deck, text in out.items():
        check(deck, "LaGriT finished, no ERROR",
              "successfully completed" in text and "ERROR" not in text,
              "ERROR in output" if "ERROR" in text else "")

    def pts(fname):
        p = os.path.join(work, fname)
        return read_points(p) if os.path.isfile(p) else []

    p1, p2, p3, p4 = pts("po01_points.inp"), pts("po02_points.inp"), pts("po03_points.inp"), pts("po04_points.inp")
    t1, t4 = out.get("po01_point_source", ""), out.get("po04_k_factor", "")
    nn1 = []

    # po01: count, spacing, maximality, the octree file
    check("po01_point_source", "vertices 700 to 1000", 700 <= len(p1) <= 1000, "%d" % len(p1))
    if p1:
        si, sf, nn1 = neighbours(p1, h_point)
        check("po01_point_source", "interior vertices: every pair at least max(h) apart", si >= 0.999,
              "smallest distance / max(h) %.4f" % si)
        check("po01_point_source", "vertices on the box faces: at least 0.6 max(h) from any other "
              "(faces are sampled with 0.9 h, box edges with 0.81 h)", sf >= 0.6, "smallest %.4f" % sf)
    vf = last_float(r"volume fraction ([0-9.eE+-]+)", t1)
    check("po01_point_source", "maximality: where a vertex could still go, under 1e-3 of the volume",
          vf is not None and vf < 1e-3, "%s" % vf)
    m = re.search(r"cells (\d+), leaves (\d+), vertices (\d+)", t1)
    oct_file = os.path.join(work, "po01_octree.inp")
    octree = None
    if m and os.path.isfile(oct_file):
        octree = read_octree(oct_file)
        nv, nc = len(octree[0]), len(octree[1])
        check("po01_point_source", "octree file: as many cells and vertices as the run reports",
              (nc, nv) == (int(m.group(2)), int(m.group(3))), "%d cells, %d vertices" % (nc, nv))
    else:
        check("po01_point_source", "octree file written and reported", False, "missing")

    # po02: batching must not change anything
    nb = re.search(r"(\d+) batches of h_field_att", out.get("po02_small_batch", ""))
    check("po02_small_batch", "the h field ran on more than 50 batches", nb is not None and int(nb.group(1)) > 50,
          nb.group(1) if nb else "not reported")
    check("po02_small_batch", "the same vertices as po01", p2 == p1 and len(p2) > 0, "%d vertices" % len(p2))

    # po03: another seed
    check("po03_other_seed", "different vertices from po01", p3 != p1, "")
    check("po03_other_seed", "vertex count within 5% of po01", p1 and abs(len(p3) - len(p1)) <= 0.05 * len(p1),
          "%d against %d" % (len(p3), len(p1)))

    # po04: K 1.5
    l1 = re.search(r"leaves (\d+)", t1)
    l4 = re.search(r"leaves (\d+)", t4)
    check("po04_k_factor", "more leaves than po01",
          l1 and l4 and int(l4.group(1)) > int(l1.group(1)),
          "%s against %s" % (l4.group(1) if l4 else "?", l1.group(1) if l1 else "?"))
    check("po04_k_factor", "vertex count within 5% of po01", p1 and abs(len(p4) - len(p1)) <= 0.05 * len(p1),
          "%d against %d" % (len(p4), len(p1)))
    if p4:
        si, _, _ = neighbours(p4, h_point)
        check("po04_k_factor", "interior vertices: every pair at least max(h) apart", si >= 0.999,
              "smallest %.4f" % si)

    # po05: the octree file read back
    t5 = out.get("po05_read_octree", "")
    vmin = last_float(r"min volume =\s+([0-9.eE+-]+)", t5)
    check("po05_read_octree", "every cell has a positive volume", vmin is not None and vmin > 0,
          "smallest volume %s" % vmin)
    hm = last_float(r"h_field_att\s+([0-9.eE+-]+)", t5)
    check("po05_read_octree", "h_field_att is at least 0.02 everywhere", hm is not None and hm >= 0.02 * 0.999,
          "smallest %s" % hm)

    # po06: constant h, octree against hex grid
    po, ph = pts("po06_points_octree.inp"), pts("po06_points_hex_grid.inp")
    nn6 = {}
    for nm, pp in (("octree", po), ("hex_grid", ph)):
        if pp:
            si, _, nn6[nm] = neighbours(pp, h_const)
            check("po06_constant_h_" + nm, "interior vertices: every pair at least h apart", si >= 0.999,
                  "smallest %.4f" % si)
    check("po06_constant_h_octree", "1.0 to 1.45 times hex_grid's vertex count",
          ph and 1.0 <= len(po) / len(ph) <= 1.45, "%d against %d" % (len(po), len(ph)))

    # po08, po09: the default lookup (h_lookup not given)
    p8, p9 = pts("po08_points.inp"), pts("po09_points.inp")
    check("po08_default_graded", "says h_lookup octree is the default with a control file",
          "h_lookup octree (the default with a control file)" in out.get("po08_default_graded", ""), "")
    check("po08_default_graded", "the same vertices as po01", p8 == p1 and len(p8) > 0, "%d vertices" % len(p8))
    check("po09_default_uniform", "says h_lookup hex_grid is the default for a uniform h",
          "h_lookup hex_grid (the default for a uniform h)" in out.get("po09_default_uniform", ""), "")
    check("po09_default_uniform", "the same vertices as po06_constant_h_hex_grid", p9 == ph and len(p9) > 0,
          "%d vertices" % len(p9))

    # po07: the same field with hex_grid
    p7 = pts("po07_points.inp")
    if p7:
        si, _, _ = neighbours(p7, h_point)
        check("po07_point_hex_grid", "interior vertices: every pair at least max(h) apart", si >= 0.999,
              "smallest %.4f" % si)
    check("po01_point_source", "at least 0.95 times hex_grid's vertex count (po07)",
          p7 and len(p1) >= 0.95 * len(p7), "%d against %d" % (len(p1), len(p7)))
    m1, m7 = read_rusage(work, "po01_point_source")[1], read_rusage(work, "po07_point_hex_grid")[1]
    check("po01_point_source", "less peak memory than hex_grid (po07)", m1 and m7 and m1 < m7,
          "%.0f MB against %.0f MB" % ((m1 or 0) / 1e6, (m7 or 0) / 1e6))

    # po10: the edge buffer's face lattice, made without the brick
    p10 = pts("po10_points.inp")
    t10 = out.get("po10_face_lattice", "")
    _, _, nexp = face_lattice(BOX10, H10)
    face10, off10, gap10 = check_face_lattice(p10, BOX10, H10) if p10 else (0, 0, 0.0)
    check("po10_face_lattice", "vertices on the box faces: the lattice count", face10 == nexp,
          "%d, expected %d" % (face10, nexp))
    check("po10_face_lattice", "every face vertex on the lattice (to 1e-9 of the box)", p10 and off10 == 0,
          "%d off" % off10)
    nitp = re.search(r"THE PSET\s+p_face\s+HAS\s+(\d+) POINTS", t10)
    check("po10_face_lattice", "itp1 10 on every face vertex", nitp is not None and int(nitp.group(1)) == nexp,
          nitp.group(1) if nitp else "not reported")
    check("po10_face_lattice", "the other vertices at least POI_EDGE_BUFFER x h_spacing_scalar (0.05) "
          "inside the box", p10 and gap10 >= 0.05 * (1 - 1e-9), "smallest %.4f" % gap10)
    r10 = read_rusage(work, "po10_face_lattice")
    check("po10_face_lattice", "peak memory under 400 MB (the brick needed about 1.5 GB)",
          r10[1] is not None and r10[1] < 400e6, "%.0f MB" % ((r10[1] or 0) / 1e6))

    # po11 to po13: face_lattice adaptive
    m11 = m12 = None
    fc = {}
    for deck, mesh, box, hfun in (("po11_face_adaptive", "po11_mesh.inp", BOX11, h_face),
                                  ("po14_face_factor_1", "po14_mesh.inp", BOX11, h_face),
                                  ("po12_face_uniform", "po12_mesh.inp", BOX11, h_face),
                                  ("po13_face_large_box", "po13_mesh.inp", BOX13, h_one)):
        txt = out.get(deck, "")
        path = os.path.join(work, mesh)
        if not os.path.isfile(path):
            check(deck, "mesh written", False, "missing")
            continue
        m = read_mesh(path)
        if deck.startswith("po11"):
            m11 = m
        if deck.startswith("po12"):
            m12 = m
        r = face_checks(m[0], m[1], box, hfun, spacing=not deck.startswith("po12"))
        fc[deck] = r
        na = neg_aij(txt)
        rr = read_rusage(work, deck)
        if deck == "po12_face_uniform":
            check(deck, "says face_lattice uniform", "face_lattice uniform" in txt, "")
            check(deck, "reported: negative coupling coefficients in the .stor", na is not None,
                  "%s negative A(ij); %d vertices; %d of %d tets on the box with the Voronoi point outside"
                  % (na, len(m[0]), r["nout"], r["nb"]))
            continue
        check(deck, "says face_lattice adaptive (the default with a control file)",
              "face_lattice adaptive (the default with a control file)" in txt, "")
        r["factor_txt"] = txt
        nface = re.search(r"face_lattice adaptive:\s+(\d+) vertices", txt)
        check(deck, "the face vertices it reports are those on the box faces",
              nface is not None and int(nface.group(1)) == r["face"],
              "%s reported, %d found" % (nface.group(1) if nface else "?", r["face"]))
        fq = re.search(r"quads up to\s+([0-9.]+) h", txt)
        F = float(fq.group(1)) if fq else 0.0
        check(deck, "face_lattice_factor reported: 1.5 by default, 1 in po14",
              abs(F - (1.0 if deck.startswith("po14") else 1.5)) < 1e-9, "%s" % (fq.group(1) if fq else "?"))
        check(deck, "face spacing: no face vertex farther than face_lattice_factor x h from its nearest face "
              "neighbor", F > 0 and r["sp_max"] <= F + 1e-6, "largest nearest-neighbor distance / h %.4f" % r["sp_max"])
        check(deck, "the sampled vertices at least 0.9 max(h) from the face vertices (POI_EDGE_BUFFER 1)",
              r["fi_min"] >= 0.9, "smallest %.4f" % r["fi_min"])
        check(deck, "every tet with a facet on the box has its Voronoi point inside the box",
              r["nb"] > 0 and r["nout"] == 0, "%d of %d outside" % (r["nout"], r["nb"]))
        check(deck, "no negative coupling coefficients in the .stor (cv_dual)", na == 0, "%s" % na)
        if deck == "po13_face_large_box":
            check(deck, "under 20,000 vertices (a uniform face lattice: about 1,000,000)", len(m[0]) < 20000,
                  "%d" % len(m[0]))
            check(deck, "peak memory under 200 MB", rr[1] is not None and rr[1] < 200e6,
                  "%.0f MB" % ((rr[1] or 0) / 1e6))
    if m11 and m12:
        check("po11_face_adaptive", "fewer vertices than po12 (face_lattice uniform)", len(m11[0]) < len(m12[0]),
              "%d against %d" % (len(m11[0]), len(m12[0])))

    npass = sum(r[2] for r in rows)
    nfail = len(rows) - npass
    width = max(len(r[0]) for r in rows)
    print("\n".join("%-4s  %-*s  %s  %s" % ("PASS" if r[2] else "FAIL", width, r[0], r[1], r[3]) for r in rows))
    print("\npassed %d   failed %d" % (npass, nfail))
    extra = {"work": work, "p7": p7, "po": po, "ph": ph, "t7": out.get("po07_point_hex_grid", ""), "p10": p10, "m11": m11, "m12": m12, "fc": fc,
             "t2": out.get("po02_small_batch", "")}
    write_page(logs, rows, npass, nfail, octree, p1, nn1, nn6, t1, len(po), len(ph), extra)
    return 0 if nfail == 0 else 1


def write_page(logs, rows, npass, nfail, octree, p1, nn1, nn6, t1, n_oct, n_hex, extra):
    E = html.escape
    maxi = re.findall(r"maximality: (\d+) boxes looked at.*?still be added is in the (\d+) boxes left.*?"
                      r"volume fraction ([0-9.eE+-]+)", t1, re.S)
    o = ['<!doctype html><html><head><meta charset="utf-8"><title>test_poisson_octree summary</title>',
         '<meta name="viewport" content="width=device-width, initial-scale=1">',
         '<style>body{font-family:system-ui,sans-serif;margin:16px;max-width:980px;color:%s;background:#fff}'
         'h2{margin-top:28px}td,th{padding:3px 10px 3px 0;vertical-align:top;text-align:left}'
         '.PASS{color:#070}.FAIL{color:#c00;font-weight:bold}.deck td{padding-top:12px;font-weight:600}'
         '.note{color:%s;font-size:14px}svg{max-width:100%%;height:auto}code{background:#f4f4f4;padding:0 3px}'
         '</style></head><body>' % (INK, INK2),
         '<p style="margin:0 0 8px;font-size:13px"><a href="../../summary.html">&larr; all suites '
         '(top-level summary)</a></p>',
         '<h1>test_poisson_octree</h1>',
         '<p class="note">%s</p>' % E(time.strftime("%a %b %d %H:%M:%S %Y")),
         '<p><b class="%s">%s</b>: %d checks passed, %d failed.</p>'
         % ("PASS" if nfail == 0 else "FAIL", "PASS" if nfail == 0 else "FAIL", npass, nfail),
         '<p><code>createpts / poisson_disk / 3d_box ... / h_lookup octree</code> looks h up in an octree '
         'adapted to h, instead of a uniform lattice over the box, and keeps track of the vertices on the '
         'octree\'s leaves. A candidate vertex is rejected when it is closer than max(h(x), h(q)) to an '
         'accepted vertex q, and the fill of empty space ends with a bound on where a vertex could still go. '
         'This suite checks those rules with h computed exactly from its formula, and the octree itself. '
         'There are no reference meshes: random numbers differ between C++ libraries (Linux, macOS), so the '
         'checks are rules and ranges. The error cases of the keywords are in '
         '<code>test_poisson_command_errors</code> (err26&ndash;31, warn06).</p>']

    # checks, by deck
    o.append('<h2>Checks</h2><table>')
    desc = dict(DECKS)
    for deck, _ in DECKS:
        o.append('<tr class="deck"><td colspan="3">%s</td></tr><tr><td colspan="3" class="note">%s</td></tr>'
                 % (E(deck), desc[deck]))
        for d, w, ok, det in rows:
            if d == deck:
                s = "PASS" if ok else "FAIL"
                o.append('<tr><td class="%s">%s</td><td>%s</td><td class="note">%s</td></tr>'
                         % (s, s, E(w), E(det)))
    o.append('</table>')

    # figures
    if octree and p1:
        o.append('<h2>The octree of po01</h2>')
        o.append('<p class="note">The leaves cut by the plane z = 0.45 through the point source, coloured by '
                 'level (1 is the coarsest). A leaf is split while the smallest h at its corners is less than '
                 '0.75 &times; its size, so leaves are small near the source, where h = 0.02, and large where '
                 'h reaches 0.2. The dots are the vertices within 0.03 of the plane: their spacing follows h.</p>')
        o.append(svg_slice(*octree, p1))
    if nn1:
        o.append('<h2>Spacing in po01</h2>')
        o.append('<p class="note">For every interior vertex: the distance to its nearest interior neighbour '
                 'divided by the larger h of the two, with h from the formula (vertices on the box faces are '
                 'left out: they are placed with 0.9 h). The rule allows nothing below 1. A maximal fill puts '
                 'most vertices between 1 and about 1.1: they sit as close as the rule allows.</p>')
        o.append(svg_hist([("po01, h_lookup octree", OCT_COLOR, nn1)], title="po01 spacing"))
        o.append('<p class="note">The same vertices against their distance from the point source: the '
                 'distance to the nearest interior neighbour, with h asked for in red. Every dot is on or above '
                 'the line, and near it: the spacing follows h across its tenfold range.</p>')
        o.append(svg_tracking(p1))

    # octree against hex grid on the same graded field
    work, p7, t7 = extra["work"], extra["p7"], extra["t7"]
    if p1 and p7:
        r1, r7 = read_rusage(work, "po01_point_source"), read_rusage(work, "po07_point_hex_grid")
        a1 = read_angles(os.path.join(work, "po01_angles.table"))
        a7 = read_angles(os.path.join(work, "po07_angles.table"))
        dfn = re.search(r"Number of nodes in the distance field: (\d+)", t7)

        def fmt_t(v):
            return "%.1f s" % v if v is not None else "?"

        def fmt_m(v):
            return "%.0f MB" % (v / 1e6) if v is not None else "?"

        def sliv(a):
            return "%.1f%%" % (100.0 * sum(1 for x in a if x < 10) / len(a)) if a else "?"

        o.append('<h2>Octree against hex_grid on the same graded field (po01, po07)</h2>')
        o.append('<p class="note">The same deck and h field, h_lookup octree and hex_grid. Time and peak memory '
                 'are of the whole LaGriT run, on this machine. The angles are those of the Delaunay tets '
                 'connect makes, before any improvement (improve_tets would raise the smallest): thin tets are '
                 'normal here, on both paths.</p><table>'
                 '<tr><th></th><th>octree (po01)</th><th>hex_grid (po07)</th></tr>')
        rowsc = [("LaGriT time", fmt_t(r1[0]), fmt_t(r7[0])),
                 ("peak memory", fmt_m(r1[1]), fmt_m(r7[1])),
                 ("h looked up at", "%s octree vertices" % "{:,}".format(len(octree[0])) if octree else "?",
                  "%s lattice points" % "{:,}".format(int(dfn.group(1))) if dfn else "?"),
                 ("vertices made", "{:,}".format(len(p1)), "{:,}".format(len(p7))),
                 ("tets", "{:,}".format(len(a1[0])), "{:,}".format(len(a7[0]))),
                 ("smallest dihedral angle, median", "%.1f&deg;" % median(a1[0]) if a1[0] else "?",
                  "%.1f&deg;" % median(a7[0]) if a7[0] else "?"),
                 ("tets with a dihedral angle under 10&deg;", sliv(a1[0]), sliv(a7[0])),
                 ("largest dihedral angle, median", "%.1f&deg;" % median(a1[1]) if a1[1] else "?",
                  "%.1f&deg;" % median(a7[1]) if a7[1] else "?")]
        for lab, a, b in rowsc:
            o.append('<tr><td>%s</td><td>%s</td><td>%s</td></tr>' % (lab, a, b))
        o.append('</table>')
        if a1[0] and a7[0]:
            o.append(svg_hist([("octree", OCT_COLOR, a1[0]), ("hex_grid", HEX_COLOR, a7[0])], lo=0, hi=75,
                              width=2.5, title="smallest dihedral angle", xlabel="smallest dihedral angle of a "
                              "tet (degrees)", ref=None, what="tets", tick=10, tickfmt="%g",
                              legend_left=True))
            o.append(svg_hist([("octree", OCT_COLOR, a1[1]), ("hex_grid", HEX_COLOR, a7[1])], lo=70, hi=180,
                              width=2.5, title="largest dihedral angle", xlabel="largest dihedral angle of a "
                              "tet (degrees)", ref=None, what="tets", tick=10, tickfmt="%g"))
    if nn6:
        o.append('<h2>Constant h: octree against hex_grid (po06)</h2>')
        o.append('<p class="note">The same measure at h = 0.1 everywhere. hex_grid rejects a candidate also '
                 'when its grid cell (h_spacing_scalar / &radic;3) has its corner inside a disk, so it keeps '
                 'vertices further apart than the rule asks. The octree tests the distance exactly and fills '
                 'until no room is left: %d vertices against %d.</p>' % (n_oct, n_hex))
        o.append(svg_hist([("h_lookup octree", OCT_COLOR, nn6.get("octree", [])),
                           ("h_lookup hex_grid", HEX_COLOR, nn6.get("hex_grid", []))],
                          title="po06 spacing, octree and hex grid"))
        o.append('<p class="note">The vertices within 0.05 of the plane z = 0.5, seen from above.</p>')
        o.append(svg_points_pair(extra["po"], extra["ph"], "h_lookup octree", "h_lookup hex_grid"))
    lev = re.search(r"leaves per level:([\d ]+)", t1)
    oc = re.search(r"h_lookup octree: K ([0-9.]+), finest cell ([0-9.eE+-]+), (\d+) passes, (\d+) batches", t1)
    mb = re.search(r"tree arrays ([0-9.eE+-]+) MB", t1)
    nb2 = re.search(r"(\d+) batches of h_field_att", extra["t2"])
    if lev:
        counts = [int(x) for x in lev.group(1).split()]
        o.append('<h2>The octree of po01 in numbers</h2>')
        o.append('<p class="note">Leaves per level: level 0 is the 4 &times; 4 &times; 4 roots (size 0.26), '
                 'and each level halves the size. A leaf is split while the smallest h at its corners is below '
                 '0.75 &times; its size, so every root is split; the finest leaves, level 4, are around the point '
                 'source, where h is 0.02 to 0.03.</p>')
        o.append(svg_levels(counts))
        o.append('<table>')
        if octree:
            o.append('<tr><td>leaves, octree vertices</td><td>%s, %s</td></tr>'
                     % ("{:,}".format(len(octree[1])), "{:,}".format(len(octree[0]))))
        if oc:
            o.append('<tr><td>octree_k_factor, finest leaf</td><td>%s, %s (h_spacing_scalar / 1.5)</td></tr>'
                     % (oc.group(1), oc.group(2)))
            o.append('<tr><td>refinement passes, runs of the h field file</td><td>%s, %s</td></tr>'
                     % (oc.group(3), oc.group(4)))
        if nb2:
            o.append('<tr><td>runs of the h field file with octree_batch 50 (po02)</td><td>%s</td></tr>' % nb2.group(1))
        if mb:
            o.append('<tr><td>the octree\'s arrays</td><td>%.2f MB</td></tr>' % float(mb.group(1)))
        o.append('</table>')
    if maxi:
        o.append('<h2>Maximality report of po01</h2>')
        o.append('<p class="note">Each fill pass ends by printing where a vertex could still be added: '
                 'the boxes of at most h/16 that are not proven covered by one vertex\'s disk. Most of them are '
                 'covered by two disks together, so this is an upper bound.</p><table>'
                 '<tr><th>fill pass</th><th>boxes looked at</th><th>boxes left</th>'
                 '<th>their share of the volume</th></tr>')
        for i, (a, b, c) in enumerate(maxi):
            o.append('<tr><td>%s</td><td>%s</td><td>%s</td><td>%s</td></tr>'
                     % ("first" if i == 0 else "resample %d" % i, "{:,}".format(int(a)), "{:,}".format(int(b)), c))
        o.append('</table>')
    if extra.get("p10"):
        r10 = read_rusage(extra["work"], "po10_face_lattice")
        o.append('<h2>The edge buffer\'s face lattice (po10)</h2>')
        o.append('<p class="note">With the edge buffer on, the sampler fills the box moved in by '
                 'POI_EDGE_BUFFER &times; h_spacing_scalar, and a lattice at h_spacing_scalar is put on the six '
                 'faces. Blue: the face lattice; orange: the sampled vertices (h = 1). LaGriT V3.4.0 found the face '
                 'vertices by building a hex brick over the whole box (4,000,000 vertices here); now only the '
                 'face vertices are made, the same ones in the same order. This run: %.1f s, %.0f MB.</p>'
                 % (r10[0] or 0, (r10[1] or 0) / 1e6))
        o.append(svg_face_lattice(extra["p10"], BOX10))
    if extra.get("m11") and extra.get("m12"):
        fc = extra["fc"]
        r11, r12 = read_rusage(extra["work"], "po11_face_adaptive"), read_rusage(extra["work"], "po12_face_uniform")
        a, b = fc.get("po11_face_adaptive", {}), fc.get("po12_face_uniform", {})
        o.append('<h2>face_lattice adaptive against uniform (po11, po12)</h2>')
        o.append('<p class="note">The same box and h field (h = 0.1 at the red dot on the bottom face, growing '
                 'to 1). Adaptive: each face is a quad mesh refined with refine until no quad is longer than '
                 'face_lattice_factor &times; h at its corners (1.5 h by default), and the sampler keeps 1.5 h from '
                 'the face vertices, so the tets on the faces reach as far inward as along the face; their Voronoi '
                 'points stay inside the box and the coupling coefficients positive. Uniform: a lattice at h_spacing_scalar (0.05) on '
                 'every face, whatever h is, and the sampled vertices from h_spacing_scalar inward.</p>'
                 '<table><tr><th></th><th>adaptive (po11)</th><th>uniform (po12)</th></tr>')
        rowsf = [("vertices", "{:,}".format(len(extra["m11"][0])), "{:,}".format(len(extra["m12"][0]))),
                 ("vertices on the faces", "{:,}".format(a.get("face", 0)), "{:,}".format(b.get("face", 0))),
                 ("tets", "{:,}".format(len(extra["m11"][1])), "{:,}".format(len(extra["m12"][1]))),
                 ("tets on the box with the Voronoi point outside", "%d of %d" % (a.get("nout", 0), a.get("nb", 0)),
                  "%d of %d" % (b.get("nout", 0), b.get("nb", 0))),
                 ("negative coupling coefficients (.stor)",
                  "%s" % neg_aij(read_text(os.path.join(extra["work"], "po11_face_adaptive.out"))),
                  "%s" % neg_aij(read_text(os.path.join(extra["work"], "po12_face_uniform.out")))),
                 ("LaGriT time, peak memory", "%.1f s, %.0f MB" % (r11[0] or 0, (r11[1] or 0) / 1e6),
                  "%.1f s, %.0f MB" % (r12[0] or 0, (r12[1] or 0) / 1e6))]
        for lab, x1, x2 in rowsf:
            o.append('<tr><td>%s</td><td>%s</td><td>%s</td></tr>' % (lab, x1, x2))
        o.append('</table>')
        o.append(svg_face_pair(extra["m11"], extra["m12"], "adaptive (po11)", "uniform (po12)", BOX11))
    o.append('</body></html>')
    os.makedirs(logs, exist_ok=True)
    with open(os.path.join(logs, "summary.html"), "w") as f:
        f.write("\n".join(o) + "\n")


if __name__ == "__main__":
    sys.exit(main())
