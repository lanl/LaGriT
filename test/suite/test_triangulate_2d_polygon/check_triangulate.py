#!/usr/bin/env python3
"""
check_triangulate.py - is the mesh a valid triangulation of the polygon(s)?

    python3 check_triangulate.py [--refined] [--min-angle DEG]
                                 [--boundary-angle DEG]
                                 POLY[,POLY...] MESH.inp [PIC.svg]

POLY is a polygon given to triangulate (an AVS vertex set, in order);
several, comma separated, when one mesh holds several polygons (the
LaGriT letters). MESH is what LaGriT wrote. Nothing here shares code
or tolerances with triangulate_lg.f.

Terms: polygons and meshes have VERTICES; the mesh has triangle CELLS.
LaGriT's own names (nnodes, nelements, "tri") appear only where the
file format uses them.

----------------------------------------------------------------------
POLYGONS WITH HOLES (keyholes)
----------------------------------------------------------------------
triangulate takes one vertex list per polygon. A hole is joined to the
outer boundary by a zero-width slit travelled both ways, so the slit's
two end vertices appear twice, and the slit edge is listed twice. After
triangulate, LaGriT's filter merges each repeated vertex, and the slit
becomes an ordinary interior edge. So vertices are compared by
COORDINATES, and an edge listed twice in the polygon must be used by
exactly TWO cells.

TOPOLOGY (both modes, HARD), on the mesh's OWN vertex numbers, so
that two unmerged copies of a slit vertex can never pass as one:
  duplicate_vertices     mesh vertices at the same place as another
                         (the slit's repeated vertices not merged)
  boundary_loops         closed loops of edges used by one cell; must
                         be 1 + (number of holes) per polygon. A hole
                         is a group of connected edges listed twice in
                         its polygon (its slit). A slit left doubled in
                         the mesh joins the hole to the outside: one
                         loop instead of two.
  euler                  V - E + F of the mesh (vertices used by cells);
                         must be the sum over polygons of 1 - holes
                         (1 for a disk, 0 for an annulus)

----------------------------------------------------------------------
DEFAULT MODE: straight out of triangulate
----------------------------------------------------------------------
The mesh must use exactly the polygon vertices. Each mesh vertex is
matched to its NEAREST polygon vertex (dump/avs prints 12 significant
digits), within MATCH_REL x (the larger of the polygon size and its
largest coordinate); geometry then uses the polygon's own coordinates.
A last vertex that repeats the first exactly is dropped, as
triangulate drops it.

HARD checks
  unmatched_vertices     mesh vertices that are not polygon vertices
  missing_vertices       polygon vertices absent from the mesh
  non_triangle_cells     cells that are not "tri"
  cell_count             must be the sum over polygons of n - 2, with n
                         counting every listed vertex (repeats included)
  zero_area_cells        height over longest side <= ZERO_REL x size
  inverted_cells         orientation differs from the majority
  area_mismatch          | sum of cell areas - polygon area | / area
                         > AREA_REL: overlapping cells or a gap
  boundary_edges_wrong   polygon edges not used by exactly one cell
                         (exactly two for a slit edge listed twice)
  extra_open_edges       edges used by one cell that are not polygon
                         edges
  overused_edges         edges used by more than two cells

----------------------------------------------------------------------
--refined MODE: after refine / smooth / recon
----------------------------------------------------------------------
New vertices are expected (on the boundary and inside), so the mesh is
judged as a mesh OF the polygons:

HARD checks
  outside_vertices       vertices neither inside a polygon nor on its
                         boundary (within MATCH_REL x size)
  missing_corners        polygon vertices with no mesh vertex there
  zero_area_cells, inverted_cells, non_triangle_cells, overused_edges
                         as above
  area_mismatch          as above
  open_edges_off_boundary  edges used by one cell that do not lie along
                         a polygon edge (a slit is not a boundary)
  boundary_length_mismatch  total length of those edges differs from the
                         polygon perimeter (slits excluded) by more than
                         AREA_REL: part of the boundary is not covered

  min_angle (only with --min-angle DEG, used on refined and smoothed
                         meshes): the smallest angle of any cell must be
                         at least DEG degrees
  boundary_angle (only with --boundary-angle DEG, normally 90): in
                         every cell on the boundary, the angle FACING
                         the boundary edge (opposite it) must be at
                         most DEG. At 90 this is the control volume
                         condition: the cell's circumcentre is not
                         outside the domain, so the Voronoi cells of
                         the boundary vertices are closed by the
                         boundary (Delaunay two point flux).
  unused_vertices (refined mode): vertices used by no cell (after
                         recon / 1 and no rmpoint / compress, there
                         should be none)

MEASURED, NOT JUDGED (both modes)
  cells, vertices, cell_orientation (clockwise / counterclockwise),
  min_angle_deg, max_angle_deg, min_height_rel (smallest cell height
  over its longest side, / polygon size), max_edge, min_edge,
  cells_below_20deg, cells_below_30deg (cells whose SMALLEST angle is
  under 20 / 30 degrees: "poor" cells; 0 is ideal),
  boundary_angle_max_deg (largest angle facing a boundary edge),
  boundary_angles_over_90 (boundary edges whose facing angle is over
  90 deg), unused_vertices.

OUTPUT: one "key value" line per quantity, "-> HARD: ..." for each
failure, then "HARD: pass" or "HARD: FAIL".
Exit status 0 = pass, 1 = fail, 2 = usage or unreadable input.
"""
import math
import sys

MATCH_REL = 1.0e-9     # vertex match / on-boundary distance, x size
ZERO_REL = 1.0e-13     # zero-area cell: height <= this x polygon size
AREA_REL = 1.0e-9      # area and boundary length, relative
DRAW_MAX_CELLS = 6000  # above this, the picture shows only bad cells


def read_poly(fn):
    xs, ys = [], []
    with open(fn) as f:
        n = int(f.readline().split()[0])
        for _ in range(n):
            w = f.readline().split()
            xs.append(float(w[1]))
            ys.append(float(w[2]))
    return xs, ys


def read_mesh(fn):
    with open(fn) as f:
        hdr = f.readline().split()
        nv, nc = int(hdr[0]), int(hdr[1])
        vx, vy, ids = [], [], {}
        for k in range(nv):
            w = f.readline().split()
            ids[int(w[0])] = k
            vx.append(float(w[1]))
            vy.append(float(w[2]))
        cells, other = [], 0
        for _ in range(nc):
            w = f.readline().split()
            if w[2].lower() == "tri" and len(w) >= 6:
                cells.append(tuple(ids[int(v)] for v in w[3:6]))
            else:
                other += 1
    return vx, vy, cells, other


def area2(ax, ay, bx, by, cx, cy):
    return (bx - ax) * (cy - ay) - (cx - ax) * (by - ay)


def seg_dist(px, py, ax, ay, bx, by):
    dx, dy = bx - ax, by - ay
    L2 = dx * dx + dy * dy
    t = 0.0 if L2 == 0 else max(0.0, min(1.0,
                                         ((px - ax) * dx + (py - ay) * dy) / L2))
    return math.hypot(ax + t * dx - px, ay + t * dy - py)


class Grid:
    """bucket points for nearest-point queries"""
    def __init__(self, X, Y, cellw):
        self.X, self.Y, self.w = X, Y, cellw
        self.g = {}
        for i, (x, y) in enumerate(zip(X, Y)):
            self.g.setdefault(self.key(x, y), []).append(i)

    def key(self, x, y):
        return (int(math.floor(x / self.w)), int(math.floor(y / self.w)))

    def nearest(self, x, y):
        gx, gy = self.key(x, y)
        best, bi = float("inf"), -1
        for a in (gx - 1, gx, gx + 1):
            for b in (gy - 1, gy, gy + 1):
                for i in self.g.get((a, b), ()):
                    d = math.hypot(self.X[i] - x, self.Y[i] - y)
                    if d < best:
                        best, bi = d, i
        return best, bi

    def near(self, x, y, r):
        """indices of the points within r of (x, y); r <= cell width"""
        gx, gy = self.key(x, y)
        return [i for a in (gx - 1, gx, gx + 1) for b in (gy - 1, gy, gy + 1)
                for i in self.g.get((a, b), ())
                if math.hypot(self.X[i] - x, self.Y[i] - y) <= r]


def main():
    args = sys.argv[1:]
    refined = False
    min_angle_req = None
    bnd_angle_req = None
    while args and args[0].startswith("--"):
        if args[0] == "--refined":
            refined = True
            args = args[1:]
        elif args[0] in ("--min-angle", "--boundary-angle") and len(args) > 1:
            try:
                v = float(args[1])
            except ValueError:
                print(__doc__)
                return 2
            if args[0] == "--min-angle":
                min_angle_req = v
            else:
                bnd_angle_req = v
            args = args[2:]
        else:
            print(__doc__)
            return 2
    if len(args) not in (2, 3):
        print(__doc__)
        return 2
    try:
        rings = []
        for fn in args[0].split(","):
            xs, ys = read_poly(fn)
            if len(xs) >= 2 and xs[0] == xs[-1] and ys[0] == ys[-1]:
                xs, ys = xs[:-1], ys[:-1]
                print("  repeated_last_vertex dropped (%s)" % fn)
            rings.append(list(zip(xs, ys)))
        mx, my, cells, other = read_mesh(args[1])
    except (OSError, ValueError, IndexError, KeyError) as e:
        print("  unreadable          %s" % e)
        print("HARD: FAIL")
        return 2

    # ---- distinct polygon points; rings as point ids -----------------
    pid, PX, PY = {}, [], []
    ring_ids = []
    for r in rings:
        ids = []
        for p in r:
            if p not in pid:
                pid[p] = len(PX)
                PX.append(p[0])
                PY.append(p[1])
            ids.append(pid[p])
        ring_ids.append(ids)
    allx, ally = [p[0] for r in rings for p in r], [p[1] for r in rings for p in r]
    size = math.hypot(max(allx) - min(allx), max(ally) - min(ally))
    big = max([size] + [abs(v) for v in allx + ally])
    tol = MATCH_REL * big
    cx0, cy0 = sum(PX) / len(PX), sum(PY) / len(PY)       # local origin
    X = [x - cx0 for x in PX]
    Y = [y - cy0 for y in PY]
    MX = [x - cx0 for x in mx]
    MY = [y - cy0 for y in my]

    # polygon edges with multiplicity (a slit is listed twice)
    pedge = {}
    for ids in ring_ids:
        n = len(ids)
        for i in range(n):
            a, b = ids[i], ids[(i + 1) % n]
            k = (a, b) if a < b else (b, a)
            pedge[k] = pedge.get(k, 0) + 1
    poly_area2 = math.fsum(abs(math.fsum(
        X[ids[i]] * Y[ids[(i + 1) % len(ids)]] -
        X[ids[(i + 1) % len(ids)]] * Y[ids[i]] for i in range(len(ids))))
        for ids in ring_ids)
    expected_cells = sum(len(ids) - 2 for ids in ring_ids)
    hard = []
    print("  mode                %s" % ("refined" if refined else "triangulated"))
    print("  polygons            %d" % len(rings))
    print("  polygon_vertices    %d (listed %d)" % (len(PX),
                                                   sum(len(r) for r in rings)))
    print("  mesh_vertices       %d" % len(mx))
    print("  cells               %d" % len(cells))
    print("  non_triangle_cells  %d" % other)
    if other:
        hard.append("%d cells are not triangles" % other)

    # ---- vertices ----------------------------------------------------
    pgrid = Grid(X, Y, max(size / max(1.0, math.sqrt(len(X))), tol * 4))
    if not refined:
        vmap, unmatched = [], 0
        for x, y in zip(MX, MY):
            d, i = pgrid.nearest(x, y)
            if d > tol:
                unmatched += 1
                i = -1
            vmap.append(i)
        missing = len(PX) - len(set(v for v in vmap if v >= 0))
        print("  unmatched_vertices  %d" % unmatched)
        print("  missing_vertices    %d" % missing)
        if unmatched:
            hard.append("%d mesh vertices are not polygon vertices"
                        % unmatched)
        if missing:
            hard.append("%d polygon vertices missing from the mesh" % missing)
        print("  expected_cells      %d" % expected_cells)
        if len(cells) != expected_cells:
            hard.append("%d cells, a triangulation of these polygons has %d"
                        % (len(cells), expected_cells))
        VX, VY = X, Y
        cl = [tuple(vmap[v] for v in c) for c in cells]
        cl = [c for c in cl if min(c) >= 0]
    else:
        VX, VY = MX, MY
        cl = list(cells)
        segs = [(a, b) for (a, b) in pedge]
        outside = 0
        for x, y in zip(MX, MY):
            if any(seg_dist(x, y, X[a], Y[a], X[b], Y[b]) <= tol
                   for a, b in segs):
                continue
            if not any(inside_ring(x, y, ids, X, Y) for ids in ring_ids):
                outside += 1
        mgrid = Grid(MX, MY, max(size / max(1.0, math.sqrt(len(MX))), tol * 4))
        miss = sum(1 for i in range(len(PX))
                   if mgrid.nearest(X[i], Y[i])[0] > tol)
        print("  outside_vertices    %d" % outside)
        print("  missing_corners     %d" % miss)
        if outside:
            hard.append("%d vertices outside the polygons" % outside)
        if miss:
            hard.append("%d polygon vertices with no mesh vertex" % miss)

    # ---- topology, on the mesh's own vertex numbers --------------------
    holes = 0
    ring_segs = []                 # (x1, y1, x2, y2, slit?) for the picture
    for ids in ring_ids:
        n = len(ids)
        cnt = {}
        for i in range(n):
            a, b = ids[i], ids[(i + 1) % n]
            k = (a, b) if a < b else (b, a)
            cnt[k] = cnt.get(k, 0) + 1
        par = {}

        def find(u):
            while par.setdefault(u, u) != u:
                par[u] = par[par[u]]
                u = par[u]
            return u
        for (a, b), m in cnt.items():
            if m >= 2:
                par[find(a)] = find(b)
        holes += len({find(u) for u in par})
        for i in range(n):
            a, b = ids[i], ids[(i + 1) % n]
            k = (a, b) if a < b else (b, a)
            ring_segs.append((PX[a], PY[a], PX[b], PY[b], cnt[k] >= 2))
    used = sorted({v for c in cells for v in c})
    rgrid = Grid(MX, MY, max(size / max(1.0, math.sqrt(len(MX))), tol * 4))
    used_s = set(used)
    dup = sum(1 for v in used
              if any(w != v and w in used_s
                     for w in rgrid.near(MX[v], MY[v], tol)))
    redges = {}
    for c in cells:
        for p, q in ((c[0], c[1]), (c[1], c[2]), (c[2], c[0])):
            k = (p, q) if p < q else (q, p)
            redges[k] = redges.get(k, 0) + 1
    bpar = {}

    def bfind(u):
        while bpar.setdefault(u, u) != u:
            bpar[u] = bpar[bpar[u]]
            u = bpar[u]
        return u
    for (p, q), m in redges.items():
        if m == 1:
            bpar[bfind(p)] = bfind(q)
    loops = len({bfind(u) for u in bpar})
    exp_loops = len(rings) + holes
    euler = len(used) - len(redges) + len(cells)
    exp_euler = len(rings) - holes
    print("  holes_in_polygons   %d" % holes)
    print("  duplicate_vertices  %d" % dup)
    print("  boundary_loops      %d (expected %d)" % (loops, exp_loops))
    print("  euler               %d (expected %d)" % (euler, exp_euler))
    if dup:
        hard.append("%d mesh vertices sit on another vertex (slit vertices "
                    "not merged?)" % dup)
    if cells and loops != exp_loops:
        hard.append("%d boundary loops, expected %d (a hole is not a real "
                    "hole, or a slit is still open)" % (loops, exp_loops))
    if cells and euler != exp_euler:
        hard.append("Euler number %d, expected %d" % (euler, exp_euler))

    # ---- cells -------------------------------------------------------
    signs, areas = [], []
    zero = 0
    min_ang, max_ang, min_h = 180.0, 0.0, float("inf")
    emin, emax = float("inf"), 0.0
    cell_min = []                  # each cell's smallest angle, degrees
    for (a, b, d) in cl:
        A = area2(VX[a], VY[a], VX[b], VY[b], VX[d], VY[d])
        l2 = [(VX[b] - VX[a]) ** 2 + (VY[b] - VY[a]) ** 2,
              (VX[d] - VX[b]) ** 2 + (VY[d] - VY[b]) ** 2,
              (VX[a] - VX[d]) ** 2 + (VY[a] - VY[d]) ** 2]
        la = [math.sqrt(v) for v in l2]
        emin, emax = min(emin, min(la)), max(emax, max(la))
        h = abs(A) / max(la) if max(la) > 0 else 0.0
        min_h = min(min_h, h)
        areas.append(abs(A))
        if h <= ZERO_REL * size:
            zero += 1
            signs.append(0)
            cell_min.append(0.0)
            continue
        signs.append(1 if A > 0 else -1)
        cmin = 180.0
        for k in range(3):
            o1, o2 = la[(k + 1) % 3], la[(k + 2) % 3]
            cosv = (o1 * o1 + o2 * o2 - la[k] * la[k]) / (2 * o1 * o2)
            ang = math.degrees(math.acos(max(-1.0, min(1.0, cosv))))
            min_ang, max_ang = min(min_ang, ang), max(max_ang, ang)
            cmin = min(cmin, ang)
        cell_min.append(cmin)
    npos, nneg = signs.count(1), signs.count(-1)
    inverted = min(npos, nneg)
    print("  zero_area_cells     %d" % zero)
    print("  inverted_cells      %d" % inverted)
    print("  cell_orientation    %s" % ("counterclockwise" if npos > nneg
                                       else "clockwise" if nneg > npos
                                       else "none"))
    if zero:
        hard.append("%d zero-area cells (three collinear vertices)" % zero)
    if inverted:
        hard.append("%d inverted cells" % inverted)
    ma = math.fsum(areas)
    rel = abs(ma - poly_area2) / poly_area2 if poly_area2 > 0 else float("inf")
    print("  polygon_area        %.12g" % (poly_area2 / 2))
    print("  mesh_area           %.12g" % (ma / 2))
    print("  area_mismatch_rel   %.3e" % rel)
    if rel > AREA_REL:
        hard.append("cell areas sum to %.6g x the polygon area "
                    "(overlap or gap)" % (ma / poly_area2 if poly_area2 else 0))

    # ---- edges -------------------------------------------------------
    edges = {}
    for c in cl:
        for p, q in ((c[0], c[1]), (c[1], c[2]), (c[2], c[0])):
            k = (p, q) if p < q else (q, p)
            edges[k] = edges.get(k, 0) + 1
    over = sum(1 for k in edges.values() if k > 2)
    print("  overused_edges      %d" % over)
    if over:
        hard.append("%d edges used by more than two cells" % over)
    bad_b = 0
    if not refined:
        bad_b = sum(1 for e, m in pedge.items() if edges.get(e, 0) != m)
        extra = sum(1 for e, k in edges.items() if k == 1 and e not in pedge)
        print("  boundary_edges_wrong %d" % bad_b)
        print("  extra_open_edges    %d" % extra)
        if bad_b:
            hard.append("%d polygon edges not used by the right number of "
                        "cells" % bad_b)
        if extra:
            hard.append("%d interior edges used by only one cell" % extra)
    else:
        bsegs = [(a, b) for (a, b), m in pedge.items() if m == 1]
        perim = math.fsum(math.hypot(X[b] - X[a], Y[b] - Y[a])
                          for a, b in bsegs)
        off, blen = 0, []
        for (p, q), k in edges.items():
            if k != 1:
                continue
            blen.append(math.hypot(MX[q] - MX[p], MY[q] - MY[p]))
            if not any(seg_dist(MX[p], MY[p], X[a], Y[a], X[b], Y[b]) <= tol
                       and seg_dist(MX[q], MY[q], X[a], Y[a], X[b], Y[b]) <= tol
                       for a, b in bsegs):
                off += 1
        brel = abs(math.fsum(blen) - perim) / perim if perim else 0.0
        print("  open_edges_off_boundary %d" % off)
        print("  boundary_length_rel_err %.3e" % brel)
        if off:
            hard.append("%d open edges not on the polygon boundary" % off)
        if brel > AREA_REL:
            hard.append("mesh boundary length differs from the polygon "
                        "perimeter (%.3e)" % brel)

    print("  min_angle_deg       %.4g" % (min_ang if cl else 0.0))
    print("  max_angle_deg       %.4g" % (max_ang if cl else 0.0))
    print("  min_height_rel      %.3e" % ((min_h / size) if cl else 0.0))
    print("  min_edge            %.4g" % (emin if cl else 0.0))
    print("  max_edge            %.4g" % (emax if cl else 0.0))
    print("  cells_below_20deg   %d" % sum(1 for a in cell_min if a < 20.0))
    print("  cells_below_30deg   %d" % sum(1 for a in cell_min if a < 30.0))
    # the angle facing each boundary edge (edge used by one cell)
    bmax, bover, bad_bnd = 0.0, 0, []
    for c in cl:
        for k in range(3):
            p, q, o = c[k], c[(k + 1) % 3], c[(k + 2) % 3]
            e = (p, q) if p < q else (q, p)
            if edges.get(e, 0) != 1:
                continue
            ux, uy = VX[p] - VX[o], VY[p] - VY[o]
            wx, wy = VX[q] - VX[o], VY[q] - VY[o]
            nu, nw = math.hypot(ux, uy), math.hypot(wx, wy)
            if nu == 0 or nw == 0:
                continue
            cosv = (ux * wx + uy * wy) / (nu * nw)
            ang = math.degrees(math.acos(max(-1.0, min(1.0, cosv))))
            bmax = max(bmax, ang)
            if cosv < -1e-9:              # over 90 degrees
                bover += 1
                bad_bnd.append((p, q))
    print("  boundary_angle_max_deg %.4g" % bmax)
    print("  boundary_angles_over_90 %d" % bover)
    nused = len({v for c in cl for v in c})
    unused = len(VX) - nused if refined else 0
    if refined:
        print("  unused_vertices     %d" % unused)
        if unused:
            hard.append("%d vertices used by no cell" % unused)
    if bnd_angle_req is not None:
        print("  boundary_angle_required %.4g" % bnd_angle_req)
        if bmax > bnd_angle_req + 1e-7:
            hard.append("%d boundary cells face the boundary with an angle "
                        "over %.3g deg (max %.4g): circumcentre outside"
                        % (bover, bnd_angle_req, bmax))
    if min_angle_req is not None:
        print("  min_angle_required  %.4g" % min_angle_req)
        if cl and min_ang < min_angle_req:
            hard.append("smallest angle %.3g deg, below the %.3g deg asked "
                        "for (# min_angle)" % (min_ang, min_angle_req))
    for h in hard:
        print("  -> HARD: " + h)
    print("HARD: " + ("FAIL" if hard else "pass"))

    if len(args) == 3:
        bnd = set(pedge) if not refined else set()
        write_svg(args[2], VX, VY, cl, signs, edges, bnd, pedge, hard,
                  rings_xy=[[(X[i], Y[i]) for i in ids] for ids in ring_ids],
                  cell_min=cell_min,
                  bad_bnd=bad_bnd if bnd_angle_req is not None else (),
                  ring_segs=[(x1 - cx0, y1 - cy0, x2 - cx0, y2 - cy0, s)
                             for x1, y1, x2, y2, s in ring_segs])
    return 1 if hard else 0


def inside_ring(x, y, ids, X, Y):
    c = False
    n = len(ids)
    for i in range(n):
        xi, yi = X[ids[i]], Y[ids[i]]
        xj, yj = X[ids[i - 1]], Y[ids[i - 1]]
        if (yi > y) != (yj > y) and \
           x < (xj - xi) * (y - yi) / (yj - yi) + xi:
            c = not c
    return c


def write_svg(fn, X, Y, cl, signs, edges, bnd, pedge, hard, rings_xy,
              cell_min=None, ring_segs=None, bad_bnd=()):
    """Outline (black), cells (grey; light amber if the cell's smallest
    angle is under 20 degrees), zero-area cells (red), inverted cells
    (black fill), polygon edges not used by the right number of cells
    (orange). Above DRAW_MAX_CELLS only the bad cells are drawn."""
    allx = [p[0] for r in rings_xy for p in r]
    ally = [p[1] for r in rings_xy for p in r]
    x0, x1, y0, y1 = min(allx), max(allx), min(ally), max(ally)
    m = 16
    span_x, span_y = (x1 - x0) or 1, (y1 - y0) or 1
    W = 900 if span_x / span_y > 1.8 else 520
    s = min((W - 2 * m) / span_x, (520 - 2 * m) / span_y)
    H = int(span_y * s + 2 * m + 4)

    def P(x, y):
        return "%.2f,%.2f" % (m + (x - x0) * s, H - m - (y - y0) * s)
    maj = 1 if signs.count(1) >= signs.count(-1) else -1
    out = ['<svg xmlns="http://www.w3.org/2000/svg" width="%d" height="%d" '
           'viewBox="0 0 %d %d" font-family="sans-serif">' % (W, H, W, H),
           '<rect width="100%" height="100%" fill="#fff"/>']
    many = len(cl) > DRAW_MAX_CELLS
    for i, (c, sg) in enumerate(zip(cl, signs)):
        poor = cell_min is not None and cell_min[i] < 20.0
        if sg == 0:
            sty = 'fill="none" stroke="#d62828" stroke-width="3"'
        elif sg != maj:
            sty = 'fill="#000" stroke="#000" stroke-width="0.5"'
        elif many:
            continue
        elif poor:
            sty = 'fill="#f6c768" stroke="#555" stroke-width="0.4"'
        else:
            sty = 'fill="#e6e6e6" stroke="#555" stroke-width="0.4"'
        out.append('<polygon points="%s %s %s" %s/>' % (
            P(X[c[0]], Y[c[0]]), P(X[c[1]], Y[c[1]]), P(X[c[2]], Y[c[2]]), sty))
    # the polygon boundary in black; a slit (an edge listed twice, which
    # becomes an INTERIOR edge once triangulate merges its vertices) as
    # a dashed blue line, so it is not mistaken for boundary
    for x1, y1, x2, y2, slit in (ring_segs or []):
        sty = ('stroke="#1971c2" stroke-width="1.5" stroke-dasharray="5 3"'
               if slit else 'stroke="#000" stroke-width="1"')
        out.append('<line x1="%s" y1="%s" x2="%s" y2="%s" %s/>'
                   % tuple(P(x1, y1).split(",") + P(x2, y2).split(",") + [sty]))
    for (a, b) in bnd:
        if edges.get((a, b), 0) != pedge[(a, b)]:
            out.append('<line x1="%s" y1="%s" x2="%s" y2="%s" '
                       'stroke="#f08c00" stroke-width="3"/>'
                       % tuple(P(X[a], Y[a]).split(",") +
                               P(X[b], Y[b]).split(",")))
    # boundary edges whose facing angle is over 90 deg: thick magenta
    # (only drawn when that is judged, --boundary-angle: after recon/1)
    for p, q in bad_bnd:
        out.append('<line x1="%s" y1="%s" x2="%s" y2="%s" stroke="#c2255c" '
                   'stroke-width="3"/>' % tuple(P(X[p], Y[p]).split(",") +
                                                P(X[q], Y[q]).split(",")))
    note = ("only bad cells drawn (%d cells)" % len(cl)) if many else ""
    if hard:
        note = (note + "  " if note else "") + hard[0]
    if note:
        out.append('<text x="%d" y="%d" font-size="12" fill="#222" '
                   'paint-order="stroke" stroke="#fff" stroke-width="3">'
                   '%s</text>' % (m, 13, note.replace("&", "&amp;")
                                  .replace("<", "&lt;")))
    out.append("</svg>")
    with open(fn, "w") as f:
        f.write("\n".join(out) + "\n")


if __name__ == "__main__":
    sys.exit(main())
