#!/usr/bin/env python3
"""make_hfield.py WORKDIR

Writes the decks of test_hfield (Python 3, standard library only) and
the meshes they read. check_hfield.py computes what each deck must
give, from the numbers in P below and the shapes in hflib.py.

  hf01_near_point    near a point: h_value, h_band, h_growth, h_floor,
                     h_ceiling, on a line of vertices; list
  hf02_near_line     near a line mesh (an L of two lines): the
                     distance to the lines; no h_value, so H is h_floor
  hf03_near_surface  near a surface: two triangles, and the same square
                     as one quad; the distance to the facets
  hf04_attribute     h_attribute: H from the source's edge_lengths_mean
                     (cmo / addatt) at its nearest vertex, times
                     h_factor, with h_band and a per-source h_growth
  hf05_two_sources   two points with different h_value: the smaller
                     h wins everywhere
  hf06_inside_cube   inside, surface: a cube as quads (extract /
                     surfmesh) and as triangles (hextotet), with h_band;
                     and a cube whose triangles face in: inside is lt,
                     against the normals, so there inside is the
                     outside of the cube
  hf07_inside_shell  inside, surface: a hollow ball (a sphere of radius
                     1 facing out around one of radius 0.5 facing in,
                     320 triangles each): the hole is outside
  hf08_inside_poly   inside, surface in 2D: an L-shaped polygon, as
                     vertices only (as for 2d_polygon) and as a loop of
                     lines with one line turned over
  hf09_poisson_2d    createpts / poisson_disk / 2d_polygon with a
                     control file and with hfield, NAME giving the same
                     h: the same seed makes the same mesh
  hf10_poisson_3d    the same in 3D, 3d_box, with the octree (the
                     default) and with h_lookup, hex_grid
  hf11_errors        the ERRORs: each refused, nothing changed
  hf12_floor_warning h_floor below h_spacing_scalar: a WARNING, and
                     h_spacing_scalar is the floor
  hf13_dump          hfield / dump: an n x n x n lattice over the
                     sources' bounding box, and n x n when they are
                     flat in z
"""
import os
import shutil
import sys

HERE = os.path.dirname(os.path.abspath(__file__))
sys.path.insert(0, HERE)
import hflib  # noqa: E402

# The numbers of each deck, shared with check_hfield.py.
P = {
    "hf01": dict(floor=0.05, ceil=0.3, growth=0.25, H=0.1, band=0.4),
    "hf02": dict(floor=0.02, ceil=0.5, growth=0.3),
    "hf03": dict(floor=0.05, ceil=1.0, growth=0.5, H=0.1),
    "hf04": dict(floor=0.01, ceil=1.0, growth=0.4, fac=0.74, band=0.05,
                 sgrowth=0.2, xs=[0.0, 0.1, 0.3, 0.6, 1.0]),
    "hf05": dict(floor=0.02, ceil=0.4, growth=0.5, H1=0.05, H2=0.1),
    "hf06": dict(floor=0.05, ceil=1.0, growth=0.5, H=0.1, band=0.1),
    "hf07": dict(floor=0.02, ceil=0.5, growth=0.4, H=0.05),
    "hf08": dict(floor=0.02, ceil=0.4, growth=0.3, H=0.05,
                 poly=[(0, 0), (1, 0), (1, 0.4), (0.4, 0.4), (0.4, 1),
                       (0, 1)]),
    "hf13": dict(floor=0.05, ceil=0.5, growth=0.3, H=0.05, n=5),
}

ERRORS = [
    ("hfield / create / hf_e / h_floor, 0 / h_ceiling, 1",
     "create: h_floor must be greater than 0"),
    ("hfield / create / hf_e / h_floor, 1 / h_ceiling, 0.5",
     "create: h_ceiling must not be less than h_floor"),
    ("hfield / create / hf_e / h_floor, 0.1 / h_ceiling, 1 / "
     "h_growth, 0", "create: h_growth must be greater than 0"),
    ("hfield / create / hf_e / h_ceiling, 1",
     "create: h_floor and h_ceiling are required"),
    ("hfield / create / hf_ok / h_floor, 0.1 / h_ceiling, 1",
     "already exists"),
    ("hfield / add / hf_none / near, mo_pt", "no hfield named hf_none"),
    ("hfield / add / hf_ok / near, mo_none",
     "mesh object mo_none does not exist"),
    ("hfield / add / hf_ok / near, mo_empty", "has no vertices"),
    ("hfield / add / hf_ok / near, mo_pt / h_attribute, imt",
     "h_attribute imt is not a VDOUBLE scalar vertex attribute of mo_pt"),
    ("hfield / add / hf_ok / near, mo_pt / h_value, 0.1 / "
     "h_attribute, xic", "add: give h_value or h_attribute, not both"),
    ("hfield / add / hf_ok / near, mo_pt / h_factor, 2",
     "add: h_factor goes with h_attribute"),
    ("hfield / add / hf_ok / near, mo_pt / h_band, -1",
     "add: h_band must not be negative"),
    ("hfield / add / hf_ok / near, mo_tet",
     "near: mo_tet must have no cells,"),
    ("hfield / add / hf_ok / inside, surface / mo_open",
     "is not a closed surface:"),
    ("hfield / add / hf_ok / inside, surface / mo_mixed",
     "is not oriented the same way everywhere"),
    ("hfield / add / hf_ok / inside, surface / mo_pt",
     "fewer than 3 vertices"),
    ("hfield / eval / hf_none / mo_pt / h", "no hfield named hf_none"),
    ("hfield / frob / hf_ok", "unknown action frob"),
    ("createpts / poisson_disk / 2d_polygon / mo_p / mo_poly / 0.1 / "
     "connect / hfield, hf_none", "hfield hf_none does not exist"),
    ("createpts / poisson_disk / 2d_polygon / mo_p / mo_poly / 0.1 / "
     "connect / ctl.mlgi / hfield, hf_ok",
     "give a control file or hfield, not both"),
]


def write(work, name, lines):
    with open(os.path.join(work, name), "w", newline="\n") as f:
        f.write("\n".join(lines) + "\n")


def lcg(n, seed=12345):
    """n pseudo-random numbers in [0, 1), the same on every machine."""
    out, s = [], seed
    for _ in range(n):
        s = (1103515245 * s + 12345) % 2147483648
        out.append(s / 2147483648.0)
    return out


def shell_points():
    """Query points for hf07: not between a sphere and its polyhedron,
    where inside would depend on the triangles, not the radius."""
    pts = [(-1.5 + 0.1 * k, 0.013, 0.007) for k in range(31)]
    r = lcg(900)
    pts += [(3 * r[3 * i] - 1.5, 3 * r[3 * i + 1] - 1.5,
             3 * r[3 * i + 2] - 1.5) for i in range(300)]
    mo, mi = shell_inradius()
    keep = []
    for p in pts:
        rr = sum(c * c for c in p) ** 0.5
        if mo - 0.01 <= rr <= 1.01 or mi * 0.5 - 0.01 <= rr <= 0.51:
            continue
        keep.append(p)
    return keep


def shell_inradius():
    """The smallest distance from the center to a face, of the unit
    icosphere (levels 2): inside the polyhedron below it for sure."""
    V, F = hflib.icosphere(1.0, (0, 0, 0), 2)
    m = 1.0
    for a, b, c in F:
        m = min(m, hflib.d_tri((0, 0, 0), V[a], V[b], V[c]))
    return m, m


def shell_mesh():
    Vo, Fo = hflib.icosphere(1.0, (0, 0, 0), 2)
    Vi, Fi = hflib.icosphere(0.5, (0, 0, 0), 2)
    V = Vo + Vi
    n = len(Vo)
    T = [(a, b, c) for a, b, c in Fo] + [(a + n, c + n, b + n)
                                         for a, b, c in Fi]
    cells = [("tri", [a + 1, b + 1, c + 1]) for a, b, c in T]
    return V, T, cells


def cube_tris(inward):
    """The unit cube's surface, 12 triangles, normals out (or in)."""
    V = [(x, y, z) for z in (0, 1) for y in (0, 1) for x in (0, 1)]
    Q = [(0, 2, 3, 1), (4, 5, 7, 6), (0, 1, 5, 4), (2, 6, 7, 3),
         (0, 4, 6, 2), (1, 3, 7, 5)]
    T = []
    for a, b, c, d in Q:
        T += [(a, b, c), (a, c, d)]
    if inward:
        T = [(a, c, b) for a, b, c in T]
    return V, [("tri", [a + 1, b + 1, c + 1]) for a, b, c in T]


def main(work):
    os.makedirs(work, exist_ok=True)
    w = lambda name, lines: write(work, name + ".lgi", lines)  # noqa

    p = P["hf01"]
    w("hf01_near_point", [
        "cmo / create / mo_pt",
        "createpts / xyz / 1,1,1 / 0. 0. 0. / 0. 0. 0. / 1,1,1",
        "cmo / create / mo_q",
        "createpts / xyz / 41,1,1 / 0. 0.1 0. / 2. 0.1 0. / 1,1,1",
        "hfield / create / hf_a / h_floor, %g / h_ceiling, %g / "
        "h_growth, %g" % (p["floor"], p["ceil"], p["growth"]),
        "hfield / add / hf_a / near, mo_pt / h_value, %g / h_band, %g"
        % (p["H"], p["band"]),
        "hfield / list / hf_a",
        "hfield / eval / hf_a / mo_q / h",
        "dump / avs / hf01_q.inp / mo_q",
        "finish"])

    p = P["hf02"]
    hflib.write_avs(os.path.join(work, "hf02_src.inp"),
                    [(0, 0, 0), (1, 0, 0), (1, 1, 0)],
                    [("line", [1, 2]), ("line", [2, 3])])
    w("hf02_near_line", [
        "read / avs / hf02_src.inp / mo_src",
        "cmo / create / mo_q",
        "createpts / xyz / 9,9,1 / -0.5 -0.5 0.2 / 1.5 1.5 0.2 / 1,1,1",
        "hfield / create / hf_l / h_floor, %g / h_ceiling, %g / "
        "h_growth, %g" % (p["floor"], p["ceil"], p["growth"]),
        "hfield / add / hf_l / near, mo_src",
        "hfield / eval / hf_l / mo_q / h",
        "dump / avs / hf02_q.inp / mo_q",
        "finish"])

    p = P["hf03"]
    sq = [(0, 0, 0), (1, 0, 0), (1, 1, 0), (0, 1, 0)]
    hflib.write_avs(os.path.join(work, "hf03_tri.inp"), sq,
                    [("tri", [1, 2, 3]), ("tri", [1, 3, 4])])
    hflib.write_avs(os.path.join(work, "hf03_quad.inp"), sq,
                    [("quad", [1, 2, 3, 4])])
    mk = ("hfield / create / %s / h_floor, %g / h_ceiling, %g / "
          "h_growth, %g")
    w("hf03_near_surface", [
        "read / avs / hf03_tri.inp / mo_tri",
        "read / avs / hf03_quad.inp / mo_quad",
        "cmo / create / mo_q",
        "createpts / xyz / 7,7,3 / -0.5 -0.5 -0.4 / 1.5 1.5 0.4 / 1,1,1",
        mk % ("hf_t", p["floor"], p["ceil"], p["growth"]),
        "hfield / add / hf_t / near, mo_tri / h_value, %g" % p["H"],
        mk % ("hf_q", p["floor"], p["ceil"], p["growth"]),
        "hfield / add / hf_q / near, mo_quad / h_value, %g" % p["H"],
        "hfield / eval / hf_t / mo_q / ht",
        "hfield / eval / hf_q / mo_q / hq",
        "dump / avs / hf03_q.inp / mo_q",
        "finish"])

    p = P["hf04"]
    xs = p["xs"]
    hflib.write_avs(os.path.join(work, "hf04_src.inp"),
                    [(x, 0, 0) for x in xs],
                    [("line", [i + 1, i + 2]) for i in range(len(xs) - 1)])
    w("hf04_attribute", [
        "read / avs / hf04_src.inp / mo_src",
        "cmo / addatt / mo_src / edge_lengths_mean / h_src",
        "cmo / create / mo_q",
        "createpts / xyz / 15,1,1 / -0.17 0.1 0. / 1.23 0.1 0. / 1,1,1",
        mk % ("hf_e", p["floor"], p["ceil"], p["growth"]),
        "hfield / add / hf_e / near, mo_src / h_attribute, h_src / "
        "h_factor, %g / h_band, %g / h_growth, %g"
        % (p["fac"], p["band"], p["sgrowth"]),
        "hfield / list / hf_e",
        "hfield / eval / hf_e / mo_q / h",
        "dump / avs / hf04_q.inp / mo_q",
        "dump / avs / hf04_src_out.inp / mo_src",
        "finish"])

    p = P["hf05"]
    w("hf05_two_sources", [
        "cmo / create / mo_p1",
        "createpts / xyz / 1,1,1 / 0. 0. 0. / 0. 0. 0. / 1,1,1",
        "cmo / create / mo_p2",
        "createpts / xyz / 1,1,1 / 1. 0. 0. / 1. 0. 0. / 1,1,1",
        "cmo / create / mo_q",
        "createpts / xyz / 21,1,1 / -0.5 0. 0. / 1.5 0. 0. / 1,1,1",
        mk % ("hf_2", p["floor"], p["ceil"], p["growth"]),
        "hfield / add / hf_2 / near, mo_p1 / h_value, %g" % p["H1"],
        "hfield / add / hf_2 / near, mo_p2 / h_value, %g" % p["H2"],
        "hfield / eval / hf_2 / mo_q / h",
        "dump / avs / hf05_q.inp / mo_q",
        "finish"])

    p = P["hf06"]
    hflib.write_avs(os.path.join(work, "hf06_cube_in.inp"),
                    *cube_tris(True))
    w("hf06_inside_cube", [
        "cmo / create / mo_hex",
        "createpts / brick / xyz / 3,3,3 / 0. 0. 0. / 1. 1. 1. / 1,1,1",
        "extract / surfmesh / 1,0,0 / mo_sq / mo_hex",
        "hextotet / 2 / mo_st / mo_sq",
        "cmo / create / mo_q",
        "createpts / xyz / 6,6,6 / -0.5 -0.5 -0.5 / 1.5 1.5 1.5 / 1,1,1",
        mk % ("hf_cq", p["floor"], p["ceil"], p["growth"]),
        "hfield / add / hf_cq / inside, surface / mo_sq / h_value, %g"
        % p["H"],
        mk % ("hf_ct", p["floor"], p["ceil"], p["growth"]),
        "hfield / add / hf_ct / inside, surface / mo_st / h_value, %g / "
        "h_band, %g" % (p["H"], p["band"]),
        "read / avs / hf06_cube_in.inp / mo_in",
        mk % ("hf_ci", p["floor"], p["ceil"], p["growth"]),
        "hfield / add / hf_ci / inside, surface / mo_in / h_value, %g"
        % p["H"],
        "hfield / eval / hf_cq / mo_q / hq",
        "hfield / eval / hf_ct / mo_q / ht",
        "hfield / eval / hf_ci / mo_q / hi",
        "dump / avs / hf06_q.inp / mo_q",
        "finish"])

    p = P["hf07"]
    V, T, cells = shell_mesh()
    hflib.write_avs(os.path.join(work, "hf07_shell.inp"), V, cells)
    Q = shell_points()
    hflib.write_avs(os.path.join(work, "hf07_q.inp"), Q, [])
    w("hf07_inside_shell", [
        "read / avs / hf07_shell.inp / mo_shell",
        "read / avs / hf07_q.inp / mo_q",
        mk % ("hf_s", p["floor"], p["ceil"], p["growth"]),
        "hfield / add / hf_s / inside, surface / mo_shell / h_value, %g"
        % p["H"],
        "hfield / eval / hf_s / mo_q / h",
        "dump / avs / hf07_q_out.inp / mo_q",
        "finish"])

    p = P["hf08"]
    poly = [(x, y, 0.0) for x, y in p["poly"]]
    hflib.write_avs(os.path.join(work, "hf08_pts.inp"), poly, [])
    n = len(poly)
    lines = [("line", [i + 1, i % n + 2 if i < n - 1 else 1])
             for i in range(n)]
    lines[2] = ("line", [lines[2][1][1], lines[2][1][0]])
    hflib.write_avs(os.path.join(work, "hf08_lines.inp"), poly, lines)
    w("hf08_inside_poly", [
        "read / avs / hf08_pts.inp / mo_ppts",
        "read / avs / hf08_lines.inp / mo_plin",
        "cmo / create / mo_q",
        "createpts / xyz / 12,12,1 / -0.25 -0.25 0. / 1.25 1.25 0. / "
        "1,1,1",
        mk % ("hf_pp", p["floor"], p["ceil"], p["growth"]),
        "hfield / add / hf_pp / inside, surface / mo_ppts / h_value, %g"
        % p["H"],
        mk % ("hf_pl", p["floor"], p["ceil"], p["growth"]),
        "hfield / add / hf_pl / inside, surface / mo_plin / h_value, %g"
        % p["H"],
        "hfield / eval / hf_pp / mo_q / hp",
        "hfield / eval / hf_pl / mo_q / hl",
        "dump / avs / hf08_q.inp / mo_q",
        "finish"])

    shutil.copy(os.path.join(HERE, "..", "test_poisson_2d",
                             "unit_square.poly"), work)
    write(work, "ctl2.mlgi", [
        "compute / distance_field / mo_poi_h_field / mo_pt / dfld",
        "math / multiply / mo_poi_h_field / h_field_att / 1,0,0 / "
        "mo_poi_h_field / dfld / 0.25",
        "math / add / mo_poi_h_field / h_field_att / 1,0,0 / "
        "mo_poi_h_field / h_field_att / 0.02",
        "math / min / mo_poi_h_field / h_field_att / 1,0,0 / "
        "mo_poi_h_field / h_field_att / 0.1",
        "cmo / DELATT / mo_poi_h_field / dfld",
        "finish"])
    w("hf09_poisson_2d", [
        "read / avs / unit_square.poly / mo_polygon",
        "cmo / create / mo_pt",
        "createpts / xyz / 1,1,1 / 0. 0. 0. / 0. 0. 0. / 1,1,1",
        "hfield / create / hf_pt / h_floor, 0.02 / h_ceiling, 0.1 / "
        "h_growth, 0.25",
        "hfield / add / hf_pt / near, mo_pt",
        "createpts / poisson_disk / 2d_polygon / mo_a / mo_polygon / "
        "0.02 / connect / ctl2.mlgi / poisson_seed, 7",
        "createpts / poisson_disk / 2d_polygon / mo_b / mo_polygon / "
        "0.02 / connect / hfield, hf_pt / poisson_seed, 7",
        "dump / avs / hf09_a.inp / mo_a",
        "dump / avs / hf09_b.inp / mo_b",
        "hfield / eval / hf_pt / mo_b / h",
        "dump / avs / hf09_b_h.inp / mo_b",
        "finish"])

    write(work, "ctl3.mlgi", [
        "compute / distance_field / mo_poi_h_field / mo_pt / dfld",
        "math / multiply / mo_poi_h_field / h_field_att / 1,0,0 / "
        "mo_poi_h_field / dfld / 0.3",
        "math / add / mo_poi_h_field / h_field_att / 1,0,0 / "
        "mo_poi_h_field / h_field_att / 0.06",
        "math / min / mo_poi_h_field / h_field_att / 1,0,0 / "
        "mo_poi_h_field / h_field_att / 0.2",
        "cmo / DELATT / mo_poi_h_field / dfld",
        "finish"])
    box = ["define / POI_%s / %s" % (k, v) for k, v in
           (("XMIN", "0."), ("YMIN", "0."), ("ZMIN", "0."),
            ("XMAX", "1."), ("YMAX", "1."), ("ZMAX", "1."))]
    w("hf10_poisson_3d", [
        "cmo / create / mo_pt",
        "createpts / xyz / 1,1,1 / 0. 0. 0. / 0. 0. 0. / 1,1,1",
        "hfield / create / hf_pt / h_floor, 0.06 / h_ceiling, 0.2 / "
        "h_growth, 0.3",
        "hfield / add / hf_pt / near, mo_pt"] + box + [
        "createpts / poisson_disk / 3d_box / mo_a / 0.06 / connect / "
        "ctl3.mlgi / poisson_seed, 7",
        "createpts / poisson_disk / 3d_box / mo_b / 0.06 / connect / "
        "hfield, hf_pt / poisson_seed, 7",
        "createpts / poisson_disk / 3d_box / mo_c / 0.06 / connect / "
        "ctl3.mlgi / poisson_seed, 7 / h_lookup, hex_grid",
        "createpts / poisson_disk / 3d_box / mo_d / 0.06 / connect / "
        "hfield, hf_pt / poisson_seed, 7 / h_lookup, hex_grid",
        "dump / avs / hf10_a.inp / mo_a",
        "dump / avs / hf10_b.inp / mo_b",
        "dump / avs / hf10_c.inp / mo_c",
        "dump / avs / hf10_d.inp / mo_d",
        "hfield / eval / hf_pt / mo_b / h",
        "dump / avs / hf10_b_h.inp / mo_b",
        "finish"])

    hflib.write_avs(os.path.join(work, "hf11_open.inp"),
                    [(0, 0, 0), (1, 0, 0), (0, 1, 0), (0, 0, 1)],
                    [("tri", [1, 2, 3]), ("tri", [1, 2, 4]),
                     ("tri", [1, 3, 4])])
    V, T, cells = shell_mesh()
    cells = [(t, [ids[1], ids[0], ids[2]] if k % 3 == 1 else ids)
             for k, (t, ids) in enumerate(cells)]
    hflib.write_avs(os.path.join(work, "hf11_mixed.inp"), V, cells)
    lines = [
        "read / avs / hf11_mixed.inp / mo_mixed",
        "read / avs / unit_square.poly / mo_poly",
        "read / avs / hf11_open.inp / mo_open",
        "cmo / create / mo_pt",
        "createpts / xyz / 1,1,1 / 0. 0. 0. / 0. 0. 0. / 1,1,1",
        "cmo / create / mo_empty",
        "cmo / create / mo_tet",
        "createpts / brick / xyz / 2,2,2 / 0. 0. 0. / 1. 1. 1. / 1,1,1",
        "hfield / create / hf_ok / h_floor, 0.1 / h_ceiling, 1"]
    lines += [c for c, _ in ERRORS]
    lines += ["hfield / list / hf_ok", "finish"]
    w("hf11_errors", lines)

    w("hf12_floor_warning", [
        "read / avs / unit_square.poly / mo_polygon",
        "cmo / create / mo_pt",
        "createpts / xyz / 1,1,1 / 0. 0. 0. / 0. 0. 0. / 1,1,1",
        "hfield / create / hf_low / h_floor, 0.01 / h_ceiling, 0.1 / "
        "h_growth, 0.25",
        "hfield / add / hf_low / near, mo_pt",
        "createpts / poisson_disk / 2d_polygon / mo_b / mo_polygon / "
        "0.02 / connect / hfield, hf_low / poisson_seed, 7",
        "dump / avs / hf12_b.inp / mo_b",
        "finish"])

    p = P["hf13"]
    w("hf13_dump", [
        "cmo / create / mo_p1",
        "createpts / xyz / 1,1,1 / 0. 0. 0. / 0. 0. 0. / 1,1,1",
        "cmo / create / mo_p2",
        "createpts / xyz / 1,1,1 / 1. 1. 1. / 1. 1. 1. / 1,1,1",
        "cmo / create / mo_p3",
        "createpts / xyz / 1,1,1 / 1. 1. 0. / 1. 1. 0. / 1,1,1",
        mk % ("hf_3d", p["floor"], p["ceil"], p["growth"]),
        "hfield / add / hf_3d / near, mo_p1 / h_value, %g" % p["H"],
        "hfield / add / hf_3d / near, mo_p2 / h_value, %g" % p["H"],
        mk % ("hf_2d", p["floor"], p["ceil"], p["growth"]),
        "hfield / add / hf_2d / near, mo_p1 / h_value, %g" % p["H"],
        "hfield / add / hf_2d / near, mo_p3 / h_value, %g" % p["H"],
        "hfield / dump / hf_3d / hf13_3d.inp / %d" % p["n"],
        "hfield / dump / hf_2d / hf13_2d.inp / %d" % p["n"],
        "hfield / delete / hf_2d",
        "hfield / list / hf_2d",
        "finish"])


if __name__ == "__main__":
    if len(sys.argv) != 2:
        sys.exit(__doc__)
    main(sys.argv[1])
