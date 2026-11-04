#!/usr/bin/env python3
"""make_recon.py WORKDIR

Writes the input meshes and decks of test_recon (Python 3, standard
library only) and WORKDIR/expect.json. Each deck dumps the mesh before
and after each recon (<deck>_<step>.inp); check_recon.py judges them.

Inputs written here (fixed seeds), coordinates with 17 digits:
  grid2.inp   a 12 x 10 grid of the rectangle 1 x 0.8, interior vertices
              moved by up to 0.35 of the spacing, each square cut by a
              FIXED diagonal: valid, 54 edges not Delaunay, no obtuse
              angle at the boundary
  grid3.inp   a 6 x 6 x 6 grid of the unit cube, interior vertices moved
              by up to 0.2 of the spacing, each cube cut into 6 tets
              along its main diagonal: valid, 621 faces not Delaunay
  grid3m.inp  grid3 with the cells at x > 0.5 given material 2, read as
              it is (no parent / child vertices): rc15
  unit_square.poly, h_field_x.mlgi  the square and the graded h field of
              test_poisson_2d, for the Poisson disk meshes of rc06, rc07
Other meshes are made in the decks (random points, connect).
"""
import json
import os
import random
import sys

DECKS = {}   # name: (what it does, deck text, expect)


def tri_grid(n, m, jit, seed):
    rng = random.Random(seed)
    V = []
    for j in range(m + 1):
        for i in range(n + 1):
            x, y = i / n, j / m * 0.8
            if 0 < i < n and 0 < j < m:
                x += rng.uniform(-jit, jit) / n
                y += rng.uniform(-jit, jit) * 0.8 / m
            V.append((x, y, 0.0))
    T = []
    for j in range(m):
        for i in range(n):
            a = j * (n + 1) + i
            b, c, d = a + 1, a + n + 2, a + n + 1
            if (i + j) % 2 == 0:
                T += [(a, b, c), (a, c, d)]
            else:
                T += [(a, b, d), (b, c, d)]
    return V, T


def tet_grid(n, jit, seed):
    rng = random.Random(seed)
    V, idx = [], {}
    for k in range(n + 1):
        for j in range(n + 1):
            for i in range(n + 1):
                p = [i / n, j / n, k / n]
                for a, q in enumerate((i, j, k)):
                    if 0 < q < n:
                        p[a] += rng.uniform(-jit, jit) / n
                idx[(i, j, k)] = len(V)
                V.append(tuple(p))
    T, col = [], []
    perms = [(0, 1, 2), (0, 2, 1), (1, 0, 2), (1, 2, 0), (2, 0, 1), (2, 1, 0)]
    for k in range(n):
        for j in range(n):
            for i in range(n):
                for pm in perms:
                    c = [i, j, k]
                    ps = [idx[tuple(c)]]
                    for ax in pm:
                        c[ax] += 1
                        ps.append(idx[tuple(c)])
                    T.append(tuple(ps))
                    col.append(1 if i < n // 2 else 2)
    return V, T, col


def det3(u, v, w):
    return (u[0] * (v[1] * w[2] - v[2] * w[1]) - u[1] * (v[0] * w[2] - v[2] * w[0])
            + u[2] * (v[0] * w[1] - v[1] * w[0]))


def write_avs(fn, V, T, kind, col=None):
    with open(fn, "w") as f:
        f.write("%d %d 0 0 0\n" % (len(V), len(T)))
        for i, p in enumerate(V):
            f.write("%d %.17g %.17g %.17g\n" % (i + 1, p[0], p[1], p[2]))
        for e, t in enumerate(T):
            vs = list(t)
            P = [V[x] for x in vs]
            if kind == "tet":
                # LaGriT's order: det(b-a, c-a, d-a) < 0
                d = det3(*[[P[q][r] - P[0][r] for r in range(3)] for q in (1, 2, 3)])
                if d > 0:
                    vs[1], vs[2] = vs[2], vs[1]
            else:
                a, b, c = P
                if (b[0] - a[0]) * (c[1] - a[1]) - (b[1] - a[1]) * (c[0] - a[0]) < 0:
                    vs[1], vs[2] = vs[2], vs[1]
            f.write("%d %d %s %s\n" % (e + 1, col[e] if col else 1, kind, " ".join(str(x + 1) for x in vs)))


RANDOM2 = """cmo / create / mo / / / triplane
createpts / random / xyz / 0.08 / 0. 0. 0. / 1. 0.6 0. / 0. 0. 0. / 0 0 0 / 7
cmo / setatt / mo / imt / 1 0 0 / 1
connect
"""
RANDOM3 = """cmo / create / mo / / / tet
createpts / random / xyz / 0.2 / 0. 0. 0. / 1. 1. 1. / 0. 0. 0. / 0 0 0 / 7
createpts / xyz / 3 3 3 / 0. 0. 0. / 1. 1. 1. / 1 1 1
filter / 1 0 0
rmpoint / compress
cmo / setatt / mo / imt / 1 0 0 / 1
"""


def deck(name, about, body, steps, expect):
    """steps: the dump names in order (the first is the mesh before)"""
    DECKS[name] = (about, body, dict(expect, steps=steps))


def make():
    deck("rc01_2d_flip",
         "recon / 0 on a valid 2D mesh with 54 edges that are not Delaunay (a grid moved and cut by fixed "
         "diagonals). recon / 0 must flip them all and add no vertex: the result is Delaunay, with the same "
         "vertices, the same boundary and the same area.",
         """read / avs / grid2.inp / mo
cmo / setatt / mo / imt / 1 0 0 / 1
resetpts / itp
dump / avs / rc01_2d_flip_0.inp / mo
recon / 0
dump / avs / rc01_2d_flip_1.inp / mo
""", ["0", "1"], {"dim": 2, "after": {"1": ["delaunay", "same_vertices", "same_boundary", "valid", "same_size"]}})
    deck("rc02_2d_add",
         "A Delaunay mesh of random points whose boundary has 15 obtuse angles (a triangle on the boundary whose "
         "angle opposite the boundary edge is over 90 degrees: its Voronoi coefficient is negative). recon / 0 "
         "must leave it as it is; recon / 1 must add vertices on the boundary until no boundary angle is obtuse, "
         "keeping the mesh Delaunay, valid and the same shape.",
         RANDOM2 + """dump / avs / rc02_2d_add_0.inp / mo
recon / 0
dump / avs / rc02_2d_add_1.inp / mo
recon / 1
dump / avs / rc02_2d_add_2.inp / mo
""", ["0", "1", "2"], {"dim": 2, "after": {
            "1": ["delaunay", "same_vertices", "same_boundary", "valid", "same_size", "unchanged"],
            "2": ["delaunay", "centres_inside", "no_obtuse_boundary", "added_on_boundary", "valid", "same_size"]}})
    deck("rc03_2d_flip_add",
         "recon / 1 straight on the 2D grid of rc01 (not Delaunay, no obtuse boundary angle): it must flip to "
         "Delaunay and, with no obtuse boundary angle to fix, add nothing.",
         """read / avs / grid2.inp / mo
cmo / setatt / mo / imt / 1 0 0 / 1
resetpts / itp
dump / avs / rc03_2d_flip_add_0.inp / mo
recon / 1
dump / avs / rc03_2d_flip_add_1.inp / mo
""", ["0", "1"], {"dim": 2, "after": {"1": ["delaunay", "centres_inside", "no_obtuse_boundary", "same_vertices", "valid", "same_size"]}})
    deck("rc04_2d_materials",
         "Two materials in 2D (random points, material 2 at x > 0.5, a column of points on x = 0.5; connect). "
         "recon / 0 and then recon / 1 must keep the area of each material and the interface (every interface "
         "edge along one before); recon / 1 may add vertices on the boundary and on the interface. (2D connect "
         "does not put the interface on x = 0.5 here: it prints 'Cannot locate interface between points', a "
         "connect matter, not recon's.)",
         """cmo / create / mo / / / triplane
createpts / random / xyz / 0.08 / 0. 0. 0. / 1. 0.6 0. / 0. 0. 0. / 0 0 0 / 7
createpts / xyz / 2 7 1 / 0.5 0. 0. / 0.5 0.6 0. / 1 1 1
filter / 1 0 0
cmo / setatt / mo / imt / 1 0 0 / 1
pset / right / attribute / xic / 1 0 0 / gt / 0.5
cmo / setatt / mo / imt / pset get right / 2
connect
dump / avs / rc04_2d_materials_0.inp / mo
recon / 0
dump / avs / rc04_2d_materials_1.inp / mo
recon / 1
dump / avs / rc04_2d_materials_2.inp / mo
""", ["0", "1", "2"], {"dim": 2, "iface_x": 0.5, "after": {
            "1": ["delaunay_in_materials", "valid", "same_size", "same_materials", "interface_kept"],
            "2": ["valid", "same_size", "same_materials", "interface_kept", "added_on_boundary_or_interface", "centres_inside"]}})
    deck("rc05_2d_off",
         "With the mesh object's ivoronoi set to 5, recon is turned off: the grid of rc01 must come back "
         "unchanged.",
         """read / avs / grid2.inp / mo
cmo / setatt / mo / imt / 1 0 0 / 1
resetpts / itp
cmo / setatt / mo / ivoronoi / 5
dump / avs / rc05_2d_off_0.inp / mo
recon / 0
dump / avs / rc05_2d_off_1.inp / mo
""", ["0", "1"], {"dim": 2, "after": {"1": ["unchanged"]}})
    # Poisson disk meshes (Carl: real-world meshes), and the same points
    # stretched in x (Carl: x 1.01, 1.1, 2, 10) before connect, so that
    # the boundary triangles become obtuse and recon / 1 has work to do
    for base, h, extra, what in (
            ("rc06_2d_poisson", "0.05", "", "uniform h = 0.05"),
            ("rc07_2d_graded", "0.01", " / h_field_x.mlgi",
             "h graded from 0.01 at x = 0 to 0.05 at the sides (h = max(0.01, 0.1 |x|))")):
        for sx, tag in (("1.0", ""), ("1.01", "_x1p01"), ("1.1", "_x1p1"), ("2.0", "_x2"), ("10.0", "_x10")):
            name = base + tag
            stretch = "" if sx == "1.0" else "scale / 1 0 0 / relative / xyz / %s 1. 1. / 0. 0. 0.\n" % sx
            how = "" if sx == "1.0" else ", then x multiplied by %s (the square becomes a %g x 1 rectangle)" % (sx, float(sx))
            deck(name,
                 "A mesh one would compute on: Poisson disk points in the unit square, %s, made with no_connect "
                 "(with its own connect, createpts runs smooth, recon / 0, smooth and recon / 1 itself)%s, then "
                 "connect. recon / 0 must leave the Delaunay mesh as it is; recon / 1 must bring every Voronoi "
                 "centre inside the rectangle, adding vertices on the boundary only, keeping the mesh Delaunay, "
                 "valid and the same area." % (what, how),
                 """define / WIDTH_FACTOR / 10.0
read / avs / unit_square.poly / mo_polygon
createpts / poisson_disk / 2d_polygon / mo / mo_polygon &
      / %s / no_connect%s
cmo / select / mo
%scmo / setatt / mo / imt / 1 0 0 / 1
connect
dump / avs / %s_0.inp / mo
recon / 0
dump / avs / %s_1.inp / mo
recon / 1
dump / avs / %s_2.inp / mo
""" % (h, extra, stretch, name, name, name), ["0", "1", "2"], {"dim": 2, "after": {
                     "1": ["delaunay", "same_vertices", "same_boundary", "valid", "same_size", "unchanged"],
                     "2": ["delaunay", "centres_inside", "no_obtuse_boundary", "added_on_boundary", "valid",
                           "same_size"]}})
    deck("rc11_3d_flip",
         "recon / 0 on a valid 3D mesh with 621 faces that are not Delaunay (a grid moved and cut into 6 tets "
         "per cube). recon / 0 must add no vertex and keep the boundary and the volume. The faces left not "
         "Delaunay are counted (recon / 0 restores Delaunay only as far as its flips reach).",
         """read / avs / grid3.inp / mo
cmo / setatt / mo / imt / 1 0 0 / 1
resetpts / itp
dump / avs / rc11_3d_flip_0.inp / mo
recon / 0
dump / avs / rc11_3d_flip_1.inp / mo
""", ["0", "1"], {"dim": 3, "after": {"1": ["valid", "same_vertices", "same_boundary", "same_size", "fewer_nondelaunay"]},
                  "measure": ["nonDelaunay"]})
    deck("rc12_3d_add",
         "recon / 1 on a Delaunay tet mesh of random points (connect). The documentation recommends recon / 1 "
         "for 2D only. It must keep the mesh valid, the volume and the boundary surface (new vertices on "
         "boundary edges only); whether it keeps the mesh Delaunay and reduces the obtuse boundary angles is "
         "measured.",
         RANDOM3 + """connect
dump / avs / rc12_3d_add_0.inp / mo
recon / 1
dump / avs / rc12_3d_add_1.inp / mo
""", ["0", "1"], {"dim": 3, "after": {"1": ["valid", "same_size", "added_on_boundary"]},
                  "measure": ["nonDelaunay", "obtuse_boundary"],
                  "known": {"1": ["delaunay", "fewer_centres_out", "fewer_obtuse_boundary", "no_stray_file"]}})
    deck("rc13_3d_materials",
         "Two materials in 3D (random points, material 2 at x > 0.5; connect). recon / 0 and then recon / 1 "
         "must keep the mesh valid and the volume of each material; recon / 0 on this Delaunay mesh must change "
         "nothing. recon / 1 here also prints 'zero terminated parent/child chain' messages (its new interface "
         "vertices are not linked into the parent / child chains).",
         RANDOM3.replace("cmo / setatt / mo / imt / 1 0 0 / 1\n", """cmo / setatt / mo / imt / 1 0 0 / 1
pset / right / attribute / xic / 1 0 0 / gt / 0.5
cmo / setatt / mo / imt / pset get right / 2
""") + """connect
dump / avs / rc13_3d_materials_0.inp / mo
recon / 0
dump / avs / rc13_3d_materials_1.inp / mo
recon / 1
dump / avs / rc13_3d_materials_2.inp / mo
""", ["0", "1", "2"], {"dim": 3, "after": {"1": ["valid", "same_size", "same_materials", "unchanged"],
                                           "2": ["valid", "same_size", "same_materials"]},
                       "measure": ["nonDelaunay", "obtuse_boundary"], "known": {"2": ["clean_output"]}})
    deck("rc14_errors",
         "recon where it cannot work: a mesh object with no cells (a WARNING, no crash) and a hex mesh (a "
         "message, the mesh unchanged). LaGriT must finish the deck.",
         """cmo / create / mpts / / / tet
createpts / xyz / 3 3 3 / 0. 0. 0. / 1. 1. 1. / 1 1 1
recon / 0
cmo / create / mhex / / / hex
createpts / brick / xyz / 3 3 3 / 0. 0. 0. / 1. 1. 1. / 1 1 1
recon / 0
""", [], {"dim": 0, "has": ["WARNING Recon Early Exit: 0 elements", "is not implemented"]})
    deck("rc15_no_parents",
         "A 3D mesh with two materials read from an AVS file as it is, without the parent / child vertices "
         "that connect makes on an interface. recon / 0 must either work or say what is wrong; LaGriT must "
         "finish the deck.",
         """read / avs / grid3m.inp / mo
resetpts / itp
dump / avs / rc15_no_parents_0.inp / mo
recon / 0
dump / avs / rc15_no_parents_1.inp / mo
""", ["0", "1"], {"dim": 3, "after": {"1": ["valid", "same_size"]}, "known": {"0": ["finished"]}})


def main(work):
    os.makedirs(work, exist_ok=True)
    V, T = tri_grid(12, 10, 0.35, 3)
    write_avs(os.path.join(work, "grid2.inp"), V, T, "tri")
    V, T, col = tet_grid(6, 0.2, 3)
    write_avs(os.path.join(work, "grid3.inp"), V, T, "tet")
    write_avs(os.path.join(work, "grid3m.inp"), V, T, "tet", col)
    with open(os.path.join(work, "unit_square.poly"), "w") as f:
        f.write("4 0 0 0 0\n1 -0.5 -0.5 0\n2 0.5 -0.5 0\n3 0.5 0.5 0\n4 -0.5 0.5 0\n")
    # the h field of test_poisson_2d (user_h_field_att.mlgi): h = max(h0, h0 WIDTH_FACTOR |x|)
    with open(os.path.join(work, "h_field_x.mlgi"), "w") as f:
        f.write("""define / MO_H_FIELD / mo_poi_h_field
math / power / MO_H_FIELD / h_field_att / 1,0,0 / MO_H_FIELD / xic / 2.0
math / power / MO_H_FIELD / h_field_att / 1,0,0 / MO_H_FIELD / h_field_att / 0.5
math / multiply / MO_H_FIELD / h_field_att / 1,0,0 / MO_H_FIELD / h_field_att / POI_H_FACTOR
math / multiply / MO_H_FIELD / h_field_att / 1,0,0 / MO_H_FIELD / h_field_att / WIDTH_FACTOR
math / floor / MO_H_FIELD / h_field_att / 1,0,0 / MO_H_FIELD / h_field_att / POI_H_FACTOR
finish
""")
    make()
    exp = {}
    for name, (about, body, e) in DECKS.items():
        with open(os.path.join(work, name + ".lgi"), "w") as f:
            f.write("* %s\n* %s\n" % (name, about[:70]))
            f.write(body)
            f.write("finish\n")
        exp[name] = dict(e, about=about)
    json.dump(exp, open(os.path.join(work, "expect.json"), "w"), indent=1)
    print("make_recon.py: %d decks in %s" % (len(DECKS), work))


if __name__ == "__main__":
    main(sys.argv[1] if len(sys.argv) > 1 else "work")
