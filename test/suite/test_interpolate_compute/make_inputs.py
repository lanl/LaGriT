#!/usr/bin/env python3
"""make_inputs.py DIR

Writes the input meshes of test_interpolate_compute (deterministic,
Python's random with fixed seeds). Python 3, standard library only.

  src_points.inp  300 random points in [0.02, 0.98]^3 (seed 11), no
                  cells; vertex attribute fsrc = 1 + 2x - 3y + 0.5z
  sink_points.inp 400 random points in [0.05, 0.95]^3 (seed 12), no
                  cells (points inside the tet mesh below)
  src_tet.inp     tet mesh of the unit cube, 6 x 6 x 6 vertices, each
                  cube cut into 6 tets; vertex attribute flin = the same
                  linear function
  sink_tet.inp    the same tets, shrunk into [0.015, 0.985]^3
  src_hex.inp     hex mesh of the unit cube, 4 x 4 x 4 cells; cell
                  attribute cval = 1 + i + 10 j + 100 k (0-based cell
                  indices)
  src_hex.inp also carries the vertex attribute flin.
  src_hex_rot.inp the same hexes numbered differently (turned 180
                  degrees about a horizontal axis): same elements
  src_tri.inp, src_quad.inp   7 x 7 vertices on the unit square (z = 0):
                  72 triangles, 36 quads; vertex flin, cell cval = cell
                  number
  src_prism.inp   a 5 x 5 triangle grid extruded through 4 layers: 128
                  prisms; vertex flin, cell cval
  src_tet_cval.inp the tet mesh with cell cval
  sink2d_points.inp 300 random points in [0.05, 0.95]^2, z = 0
  sink2d_grid.inp 101 x 101 points on the unit square, z = 0 (many lie
                  exactly on edges of src_tri.inp)
  sink2d_boundary.inp 92 points on and just outside the exterior edges
                  and corners of the unit square (offsets 0, 1e-15,
                  1e-14: must get the value; 1e-9 and more: must get
                  the flag); vertex attribute expect = 1 or 0
  sink2d_tri.inp, sink_tet_sq.inp  sink meshes for map, squeezed into
                  the box by a different amount on each axis so no
                  centroid lies on a source face
  src_hex_warp.inp the hexes sheared, interior vertices moved (faces not
                  planar) and turned over (stored edge 1-2 along y);
                  vertex flin. sink_warp.inp: sink_points.inp sheared
  src_hex_warp_g.inp the same hexes in LaGriT's numbering with a curved
                  field g = xy + z^2 + sin 2x
  src_prism_flip.inp the prisms turned over (other vertex numbering)
  src_pyr.inp     3 x 3 x 3 cubes, each cut into 6 pyramids; vertex flin
  plane_tri.inp   triangles covering the square [-1, 2]^2 at z = 0.4
                  (8 x 8 vertices), for the signed distance field
"""
import math
import os
import random
import sys


def flin(x, y, z):
    return 1.0 + 2.0 * x - 3.0 * y + 0.5 * z


def write(path, V, cells, ctype, vatt=None, catt=None):
    with open(path, "w") as f:
        f.write("%d %d %d %d 0\n" % (len(V), len(cells), 1 if vatt else 0,
                                    1 if catt else 0))
        for i, p in enumerate(V):
            f.write("%d %.15e %.15e %.15e\n" % ((i + 1,) + tuple(p)))
        for i, c in enumerate(cells):
            f.write("%d 1 %s %s\n" % (i + 1, ctype, " ".join(str(k + 1) for k in c)))
        for att in (vatt, catt):
            if att:
                name, vals = att
                f.write("1 1\n%s, real\n" % name)
                for i, v in enumerate(vals):
                    f.write("%d %.15e\n" % (i + 1, v))


def main(d):
    r = random.Random(11)
    P = [(r.uniform(.02, .98), r.uniform(.02, .98), r.uniform(.02, .98)) for _ in range(300)]
    write(os.path.join(d, "src_points.inp"), P, [], "", ("fsrc", [flin(*p) for p in P]))
    r = random.Random(12)
    Q = [(r.uniform(.05, .95), r.uniform(.05, .95), r.uniform(.05, .95)) for _ in range(400)]
    write(os.path.join(d, "sink_points.inp"), Q, [], "")
    # tet mesh: 6 tets per cube (Kuhn split along the main diagonal)
    n = 6
    h = 1.0 / (n - 1)
    idx = lambda i, j, k: i + n * (j + n * k)
    V = [(i * h, j * h, k * h) for k in range(n) for j in range(n) for i in range(n)]
    T = []
    for k in range(n - 1):
        for j in range(n - 1):
            for i in range(n - 1):
                c = [idx(i + a, j + b, k + e) for e in (0, 1) for b in (0, 1) for a in (0, 1)]
                v0, v7 = c[0], c[7]
                for p1, p2 in ((1, 3), (3, 2), (2, 6), (6, 4), (4, 5), (5, 1)):
                    T.append([v0, c[p1], c[p2], v7])
    # orient every tet as LaGriT counts positive volume (face 1-2-3
    # seen from vertex 4 is clockwise)
    def vol(t):
        a, b, cc, dd = (V[q] for q in t)
        u = [b[m] - a[m] for m in range(3)]
        v = [cc[m] - a[m] for m in range(3)]
        w = [dd[m] - a[m] for m in range(3)]
        return (u[0] * (v[1] * w[2] - v[2] * w[1]) - u[1] * (v[0] * w[2] - v[2] * w[0])
                + u[2] * (v[0] * w[1] - v[1] * w[0]))
    T = [t if vol(t) < 0 else [t[0], t[2], t[1], t[3]] for t in T]
    write(os.path.join(d, "src_tet.inp"), V, T, "tet", ("flin", [flin(*p) for p in V]))
    # the same tets shrunk to [0.015, 0.985]^3, so no tet centroid lies
    # on a face of the 4 x 4 x 4 hex mesh (for interpolate / map)
    write(os.path.join(d, "sink_tet.inp"), [tuple(.015 + .97 * c for c in p) for p in V],
          T, "tet")
    # hex mesh 4 x 4 x 4 cells
    m = 5
    g = 1.0 / (m - 1)
    hid = lambda i, j, k: i + m * (j + m * k)
    HV = [(i * g, j * g, k * g) for k in range(m) for j in range(m) for i in range(m)]
    H, cv = [], []
    for k in range(m - 1):
        for j in range(m - 1):
            for i in range(m - 1):
                # in the AVS file: the top face counterclockwise seen
                # from above, then the bottom face in the same order;
                # read / avs stores them swapped (AVS 1..8 -> 5 6 7 8
                # 1 2 3 4), giving LaGriT's internal order, bottom face
                # first, as createpts / brick makes it
                H.append([hid(i, j, k + 1), hid(i + 1, j, k + 1), hid(i + 1, j + 1, k + 1),
                          hid(i, j + 1, k + 1), hid(i, j, k), hid(i + 1, j, k),
                          hid(i + 1, j + 1, k), hid(i, j + 1, k)])
                cv.append(1 + i + 10 * j + 100 * k)
    write(os.path.join(d, "src_hex.inp"), HV, H, "hex", ("flin", [flin(*p) for p in HV]),
          ("cval", cv))
    # the same hexes numbered differently: the same element turned 180
    # degrees about a horizontal axis, so stored edge 1-2 runs along y;
    # a valid right-handed numbering, positive volumes
    Hrot = [[h[4], h[7], h[6], h[5], h[0], h[3], h[2], h[1]] for h in H]
    write(os.path.join(d, "src_hex_rot.inp"), HV, Hrot, "hex",
          ("flin", [flin(*p) for p in HV]), ("cval", cv))
    # plane of triangles at z = 0.4 over [-1, 2]^2
    q = 8
    s = 3.0 / (q - 1)
    pid = lambda i, j: i + q * j
    PV = [(-1 + i * s, -1 + j * s, 0.4) for j in range(q) for i in range(q)]
    PT = []
    for j in range(q - 1):
        for i in range(q - 1):
            PT.append([pid(i, j), pid(i + 1, j), pid(i + 1, j + 1)])
            PT.append([pid(i, j), pid(i + 1, j + 1), pid(i, j + 1)])
    write(os.path.join(d, "plane_tri.inp"), PV, PT, "tri")
    more(d, V, T)


def squeeze(p):
    """into the box, a different shrink per axis, so no sink centroid
    lands on a source cell face or edge by symmetry"""
    return (.011 + .97 * p[0], .017 + .96 * p[1], (.023 + .95 * p[2]) if p[2] else 0.0)


def grid2(n):
    """n x n vertices on the unit square (z = 0), triangles (2 per
    square) and quads, counterclockwise seen from +z"""
    h = 1.0 / (n - 1)
    gid = lambda i, j: i + n * j
    V = [(i * h, j * h, 0.0) for j in range(n) for i in range(n)]
    tri, quad = [], []
    for j in range(n - 1):
        for i in range(n - 1):
            a, b, c, e = gid(i, j), gid(i + 1, j), gid(i + 1, j + 1), gid(i, j + 1)
            if (i + j) % 2:                 # alternate the diagonal
                tri += [[a, b, c], [a, c, e]]
            else:
                tri += [[a, b, e], [b, c, e]]
            quad.append([a, b, c, e])
    return V, tri, quad


def more(d, Vtet, Ttet):
    """the 2D sources and sinks, the prism source, a cell value on every
    source for interpolate / map (cval = the cell number), and the
    sinks for map (centroids inside, off every source face)"""
    num = lambda cells: [float(i + 1) for i in range(len(cells))]
    V2, tri, quad = grid2(7)
    write(os.path.join(d, "src_tri.inp"), V2, tri, "tri", ("flin", [flin(*p) for p in V2]),
          ("cval", num(tri)))
    write(os.path.join(d, "src_quad.inp"), V2, quad, "quad", ("flin", [flin(*p) for p in V2]),
          ("cval", num(quad)))
    r = random.Random(13)
    Q2 = [(r.uniform(.05, .95), r.uniform(.05, .95), 0.0) for _ in range(300)]
    write(os.path.join(d, "sink2d_points.inp"), Q2, [], "")
    write(os.path.join(d, "sink2d_grid.inp"),
          [(i / 100, j / 100, 0.0) for j in range(101) for i in range(101)], [], "")
    # points on and just outside the exterior edges and corners of the
    # unit square: expect = 1 must get the linear value (on the edge
    # within rounding), expect = 0 must get the flag value
    B, ex = [], []
    for t in (0.23, 0.5, 0.77):
        for off, e in ((0.0, 1), (1e-15, 1), (1e-14, 1), (1e-9, 0), (1e-6, 0), (1e-3, 0)):
            B += [(t, -off, 0.0), (1 + off, t, 0.0), (t, 1 + off, 0.0), (-off, t, 0.0)]
            ex += [e] * 4
    for off, e in ((0.0, 1), (1e-15, 1), (1e-14, 1), (1e-9, 0), (1e-6, 0)):
        B += [(-off, -off, 0.0), (1 + off, -off, 0.0), (1 + off, 1 + off, 0.0), (-off, 1 + off, 0.0)]
        ex += [e] * 4
    write(os.path.join(d, "sink2d_boundary.inp"), B, [], "", ("expect", [float(e) for e in ex]))
    S2, stri, _ = grid2(9)
    write(os.path.join(d, "sink2d_tri.inp"), [squeeze(p) for p in S2], stri, "tri")
    # prisms: a 5 x 5 triangle grid extruded through 4 layers
    P2, ptri, _ = grid2(5)
    nl = 5
    PV = [(x, y, k / (nl - 1)) for k in range(nl) for (x, y, _) in P2]
    m = len(P2)
    # in the AVS file the top triangle counterclockwise seen from
    # above, then the bottom one; read / avs stores them swapped (AVS
    # 1..6 -> 4 5 6 1 2 3), as stack / fill makes it
    PR = [[t[0] + (k + 1) * m, t[1] + (k + 1) * m, t[2] + (k + 1) * m,
           t[0] + k * m, t[1] + k * m, t[2] + k * m]
          for k in range(nl - 1) for t in ptri]
    write(os.path.join(d, "src_prism.inp"), PV, PR, "prism", ("flin", [flin(*p) for p in PV]),
          ("cval", num(PR)))
    # the tet mesh with a cell value, and a sink of squeezed tets for it
    write(os.path.join(d, "src_tet_cval.inp"), Vtet, Ttet, "tet", None, ("cval", num(Ttet)))
    write(os.path.join(d, "sink_tet_sq.inp"), [squeeze(p) for p in Vtet], Ttet, "tet")
    more_cells(d)


def read_cells(path):
    """vertices and cell vertex lists (0-based, AVS file order)"""
    L = open(path).read().split("\n")
    nn, nc = int(L[0].split()[0]), int(L[0].split()[1])
    V = [tuple(map(float, l.split()[1:4])) for l in L[1:1 + nn]]
    C = [[int(k) - 1 for k in l.split()[3:]] for l in L[1 + nn:1 + nn + nc]]
    return V, C


def gcurve(x, y, z):
    """a curved field: interpolation is not exact, but interpolating
    from hexes must equal hextotet / 24 then interpolating from tets"""
    return x * y + z * z + math.sin(2.0 * x)


def more_cells(d):
    """(Oct 2026, the median split in cinterpolate_elem) warped and
    renumbered hexes, renumbered prisms, pyramids, a curved field"""
    shear = lambda p: (p[0] + 0.3 * p[1] + 0.2 * p[2], p[1] + 0.25 * p[2], p[2] + 0.1 * p[0])
    on = lambda t: abs(t) < 1e-9 or abs(t - 1.0) < 1e-9
    HV, H = read_cells(os.path.join(d, "src_hex.inp"))
    r = random.Random(14)
    W = []
    for v in HV:
        q = v
        if not any(on(t) for t in v):        # interior vertices: faces no longer planar
            q = tuple(t + r.uniform(-0.05, 0.05) for t in v)
        W.append(shear(q))
    # the warped hexes turned over (stored edge 1-2 along y, not x)
    Hrot = [[h[i] for i in (4, 7, 6, 5, 0, 3, 2, 1)] for h in H]
    write(os.path.join(d, "src_hex_warp.inp"), W, Hrot, "hex", ("flin", [flin(*p) for p in W]))
    write(os.path.join(d, "src_hex_warp_g.inp"), W, H, "hex", ("g", [gcurve(*p) for p in W]))
    S, _ = read_cells(os.path.join(d, "sink_points.inp"))
    write(os.path.join(d, "sink_warp.inp"), [shear(p) for p in S], [], "")
    # the prisms turned over (top and bottom triangles swapped, reversed)
    PV, PR = read_cells(os.path.join(d, "src_prism.inp"))
    write(os.path.join(d, "src_prism_flip.inp"), PV, [[c[i] for i in (3, 5, 4, 0, 2, 1)] for c in PR],
          "prism", ("flin", [flin(*p) for p in PV]))
    # pyramids: each of 3 x 3 x 3 cubes cut into 6, apex at its centre
    # (AVS order: apex first, then the base)
    n, h = 3, 1.0 / 3
    YV, idx, Y = [], {}, []

    def vid(p):
        k = tuple(round(t, 12) for t in p)
        if k not in idx:
            idx[k] = len(YV)
            YV.append(p)
        return idx[k]
    faces = [[(0, 0, 0), (1, 0, 0), (1, 1, 0), (0, 1, 0)], [(0, 0, 1), (0, 1, 1), (1, 1, 1), (1, 0, 1)],
             [(0, 0, 0), (0, 0, 1), (1, 0, 1), (1, 0, 0)], [(1, 0, 0), (1, 0, 1), (1, 1, 1), (1, 1, 0)],
             [(1, 1, 0), (1, 1, 1), (0, 1, 1), (0, 1, 0)], [(0, 1, 0), (0, 1, 1), (0, 0, 1), (0, 0, 0)]]
    for i in range(n):
        for j in range(n):
            for k in range(n):
                o = (i * h, j * h, k * h)
                c = vid((o[0] + h / 2, o[1] + h / 2, o[2] + h / 2))
                for fc in faces:
                    Y.append([c] + [vid((o[0] + a * h, o[1] + b * h, o[2] + e * h)) for a, b, e in fc])
    write(os.path.join(d, "src_pyr.inp"), YV, Y, "pyr", ("flin", [flin(*p) for p in YV]))


if __name__ == "__main__":
    main(sys.argv[1] if len(sys.argv) > 1 else ".")
