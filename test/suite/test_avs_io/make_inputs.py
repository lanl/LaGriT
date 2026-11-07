#!/usr/bin/env python3
"""make_inputs.py WORKDIR

Writes the AVS UCD input files of test_avs_io into WORKDIR. Every file
is written here, by hand-made rules, not by LaGriT, so read / avs is
tested against files LaGriT did not write. The same files every time.

The meshes (small; the coordinates have 13 significant digits, which
dump / avs writes exactly):

  cells_pt      4 vertices, 4 pt cells
  cells_line    4 vertices, 3 line cells
  cells_tri     5 vertices, 4 triangles (a square around its centre)
  cells_quad    6 vertices, 2 quads
  cells_tet     5 vertices, 2 tets sharing a face
  cells_pyr     6 vertices, 2 pyramids sharing their base
  cells_prism   9 vertices, 2 prisms, one on the other
  cells_hex    12 vertices, 2 hexes, one on the other
  hybrid       one tet, one pyramid, one prism and one hex, apart

Each has two vertex attributes, va (integer) and vr (real), and two
cell attributes, ca (integer) and cr (real); materials 1 and 2.

Variants of cells_hex (the same mesh, written differently):
  hex_zero_based     vertices and cells numbered from 0
  hex_comments       "#" lines before and inside every section
  hex_no_attributes  header "12 2 0 0 0", no attribute sections
  hex_vertex_atts    vertex attributes only ("2 0")
  hex_cell_atts      cell attributes only ("0 2"): the vertex-attribute
                     section is the single line "0"
  hex_wide           12 cell attributes: more cell than vertex
                     attributes (dump / att_elem's header)
  extremes           values at the edges of the number formats: three
                     digit exponents, the smallest and largest doubles,
                     -0.0, values that round up to the next power of 10,
                     13-digit integers
Files for known problems (see the decks with a "# known:" line):
  hex_first_two      the first vertex is numbered 2
  hex_gap            vertex numbers 1..6, 8..13 (7 missing), the cells
                     numbered to match
  prism_pri          cells_prism with the cell name "pri"
  hex_no_space       "va,integer" (no space after the comma)
  hex_short          cut off in the middle of the cells
  hex_bad_type       a cell named "polyhedron" between the two hexes,
                     with its attribute row
  hex_far_z          one z coordinate is -1e31
  vec_rank3          one vertex attribute of rank 3
  hex_same_names     a cell attribute named va, like a vertex attribute

Python 3, standard library only.
"""
import os
import sys


def fmt(x):
    return "%.12E" % x


# ------------------------------------------------------------ meshes
# Cells are in AVS vertex order, 1-based.

def mesh_pt():
    V = [(0.0, 0.0, 0.0), (1.0, 0.0, 0.0), (0.0, 1.0, 0.0), (0.0, 0.0, 1.0)]
    C = [("pt", [i + 1]) for i in range(4)]
    return V, C


def mesh_line():
    V = [(0.0, 0.0, 0.0), (1.0, 0.0, 0.0), (2.0, 0.5, 0.0), (3.0, 0.5, 0.25)]
    C = [("line", [1, 2]), ("line", [2, 3]), ("line", [3, 4])]
    return V, C


def mesh_tri():
    V = [(0.0, 0.0, 0.0), (1.0, 0.0, 0.0), (1.0, 1.0, 0.0), (0.0, 1.0, 0.0),
         (0.5, 0.5, 0.0)]
    C = [("tri", [1, 2, 5]), ("tri", [2, 3, 5]), ("tri", [3, 4, 5]),
         ("tri", [4, 1, 5])]
    return V, C


def mesh_quad():
    V = [(0.0, 0.0, 0.0), (1.0, 0.0, 0.0), (2.0, 0.0, 0.0),
         (0.0, 1.0, 0.0), (1.0, 1.0, 0.0), (2.0, 1.0, 0.0)]
    C = [("quad", [1, 2, 5, 4]), ("quad", [2, 3, 6, 5])]
    return V, C


def mesh_tet():
    V = [(0.0, 0.0, 0.0), (1.0, 0.0, 0.0), (0.0, 1.0, 0.0), (0.0, 0.0, 1.0),
         (1.0, 1.0, 1.0)]
    C = [("tet", [1, 2, 3, 4]), ("tet", [2, 3, 4, 5])]
    return V, C


def mesh_pyr():
    V = [(0.0, 0.0, 0.0), (1.0, 0.0, 0.0), (1.0, 1.0, 0.0), (0.0, 1.0, 0.0),
         (0.5, 0.5, 1.0), (0.5, 0.5, -1.0)]
    C = [("pyr", [5, 1, 2, 3, 4]), ("pyr", [6, 4, 3, 2, 1])]
    return V, C


def mesh_prism():
    V = []
    for z in (0.0, 1.0, 2.0):
        V += [(0.0, 0.0, z), (1.0, 0.0, z), (0.0, 1.0, z)]
    C = [("prism", [1, 2, 3, 4, 5, 6]), ("prism", [4, 5, 6, 7, 8, 9])]
    return V, C


def mesh_hex():
    V = []
    for z in (0.0, 1.0, 2.0):
        V += [(0.0, 0.0, z), (1.0, 0.0, z), (1.0, 1.0, z), (0.0, 1.0, z)]
    C = [("hex", [1, 2, 3, 4, 5, 6, 7, 8]), ("hex", [5, 6, 7, 8, 9, 10, 11, 12])]
    return V, C


def mesh_hybrid():
    parts = [mesh_tet, mesh_pyr, mesh_prism, mesh_hex]
    V, C = [], []
    for k, f in enumerate(parts):
        v, c = f()
        n0 = len(V)
        V += [(x + 3.0 * k, y, z) for (x, y, z) in v]
        C.append((c[0][0], [i + n0 for i in c[0][1]]))
    return V, C


MESHES = {"pt": mesh_pt, "line": mesh_line, "tri": mesh_tri,
          "quad": mesh_quad, "tet": mesh_tet, "pyr": mesh_pyr,
          "prism": mesh_prism, "hex": mesh_hex}


def jiggle(V):
    """13 significant digits in every coordinate, so a dump that keeps
    13 digits must reproduce them exactly"""
    out = []
    for i, (x, y, z) in enumerate(V):
        d = 1.234567890123e-3 * (i + 1)
        out.append((float(fmt(x + d)), float(fmt(y - 0.5 * d)),
                    float(fmt(z + 0.25 * d))))
    return out


def vatts(n):
    va = [(-1) ** i * (7 * i + 3) for i in range(n)]
    vr = [float(fmt((i + 1) * 1.0471975511966 * 10 ** (i % 5 - 2)
                    * (-1) ** (i // 2))) for i in range(n)]
    return va, vr


def catts(n):
    ca = [100 * (i + 1) - 7 for i in range(n)]
    cr = [float(fmt(-2.718281828459 * (i + 1) ** 3 * 10 ** (3 - i % 7)))
          for i in range(n)]
    return ca, cr


# ------------------------------------------------------------ writing

def write(path, V, C, mats, vnames=(), vcols=(), cnames=(), ccols=(),
          base=1, comments=False, cut_after_cells=None, no_space=False,
          header=None, vranks=None):
    L = []
    if comments:
        L.append("# a comment before the header")
    nv, nc = len(V), len(C)
    if header is None:
        header = "%d %d %d %d 0" % (nv, nc, len(vnames) if vnames else 0,
                                     len(cnames) if cnames else 0)
    L.append(header)
    if comments:
        L.append("# vertices")
    for i, (x, y, z) in enumerate(V):
        L.append("%d %s %s %s" % (i + base, fmt(x), fmt(y), fmt(z)))
        if comments and i == 2:
            L.append("# inside the vertices")
    if comments:
        L.append("# cells")
    for k, (t, idx) in enumerate(C):
        if cut_after_cells is not None and k == cut_after_cells:
            break
        L.append("%d %d %s %s" % (k + base, mats[k], t,
                                  " ".join(str(i - 1 + base) for i in idx)))
        if comments and k == 0:
            L.append("# inside the cells")
    if cut_after_cells is not None:
        open(path, "w").write("\n".join(L) + "\n")
        return

    def section(names, cols, n, ranks=None):
        if comments:
            L.append("# attribute section")
        ranks = ranks or [1] * len(names)
        L.append("%d %s" % (len(names), " ".join(str(r) for r in ranks)))
        for name, col in zip(names, cols):
            kind = "integer" if isinstance(col[0], int) else "real"
            sep = "," if no_space and kind == "integer" else ", "
            L.append("%s%s%s" % (name, sep, kind))
        for i in range(n):
            vals = []
            for col, r in zip(cols, ranks):
                v = col[i]
                vals += [str(v) if isinstance(v, int) else fmt(v)] * r
            L.append("%d %s" % (i + base, " ".join(vals)))

    if vnames:
        section(vnames, vcols, nv, vranks)
    elif cnames:
        L.append("0")
    if cnames:
        section(cnames, ccols, nc)
    open(path, "w").write("\n".join(L) + "\n")


def main():
    out = sys.argv[1]
    os.makedirs(out, exist_ok=True)

    def std(path, V, C, **kw):
        va, vr = vatts(len(V))
        ca, cr = catts(len(C))
        mats = [1 + k % 2 for k in range(len(C))]
        args = dict(vnames=("va", "vr"), vcols=(va, vr),
                    cnames=("ca", "cr"), ccols=(ca, cr))
        args.update(kw)
        write(os.path.join(out, path), V, C, mats, **args)

    for name, f in MESHES.items():
        V, C = f()
        std("cells_%s.inp" % name, jiggle(V), C)
    V, C = mesh_hybrid()
    std("hybrid.inp", jiggle(V), C)

    V, C = mesh_hex()
    V = jiggle(V)
    std("hex_zero_based.inp", V, C, base=0)
    std("hex_comments.inp", V, C, comments=True)
    std("hex_no_attributes.inp", V, C, vnames=(), vcols=(), cnames=(),
        ccols=())
    std("hex_vertex_atts.inp", V, C, cnames=(), ccols=())
    std("hex_cell_atts.inp", V, C, vnames=(), vcols=())
    ca, cr = catts(len(C))
    wide_names = tuple("c%02d" % k for k in range(12))
    wide_cols = tuple((ca if k % 2 == 0 else cr) for k in range(12))
    std("hex_wide.inp", V, C, cnames=wide_names, ccols=wide_cols)
    # known problems
    std("hex_first_two.inp", V, C, base=2)
    # a folder with a copy of cells_tet.inp, for a path in read / avs
    os.makedirs(os.path.join(out, "sub"), exist_ok=True)
    open(os.path.join(out, "sub", "cells_tet.inp"), "w").write(
        open(os.path.join(out, "cells_tet.inp")).read())
    std("hex_no_space.inp", V, C, no_space=True)
    std("hex_short.inp", V, C, cut_after_cells=1)
    Vf = list(V)
    Vf[5] = (Vf[5][0], Vf[5][1], -1.0e31)
    std("hex_far_z.inp", Vf, C)
    write_extremes(os.path.join(out, "extremes.inp"))
    ca, cr = catts(len(C))
    std("hex_same_names.inp", V, C, cnames=("va", "cr"), ccols=(ca, cr))
    std("vec_rank3.inp", V, C, vranks=[3, 1])
    Vp, Cp = mesh_prism()
    std("prism_pri.inp", jiggle(Vp), [("pri", c) for (_, c) in Cp])
    # vertex 7 missing: number the vertices 1..6, 8..13
    p = os.path.join(out, "hex_gap.inp")
    std("hex_gap.inp", V, C)
    L = open(p).read().split("\n")
    for i in range(1 + 6, 1 + 12):
        w = L[i].split(" ", 1)
        L[i] = "%d %s" % (int(w[0]) + 1, w[1])
    for i in range(1 + 12, 1 + 12 + 2):
        w = L[i].split()
        L[i] = " ".join(w[:3] + [str(int(v) + (int(v) >= 7)) for v in w[3:]])
    open(p, "w").write("\n".join(L))
    # a third cell of an unknown type, with its attribute row
    p = os.path.join(out, "hex_bad_type.inp")
    std("hex_bad_type.inp", V, C)
    L = open(p).read().split("\n")
    L[0] = "12 3 2 2 0"
    L[1 + 12 + 1] = "3" + L[1 + 12 + 1][1:]
    L.insert(1 + 12 + 1, "2 1 polyhedron 1 2 3 4 5 6")
    i = L.index("cr, real") + 1
    L[i + 1] = "3" + L[i + 1][1:]
    L.insert(i + 1, "2 999 %s" % fmt(9.99))
    open(p, "w").write("\n".join(L))


def write_extremes(path):
    """extremes.inp: the 12-vertex hex mesh with values at the edges of
    what dump / avs formats: exponents of three digits, the smallest
    and largest doubles, -0.0, values that round up to the next power
    of ten, and integers of 13 digits"""
    V, C = mesh_hex()
    V = [list(v) for v in jiggle(V)]
    V[0][0] = 9.99999999999995e29
    V[1][1] = -1.0e-29
    V[2][2] = 123456789.0123456789
    vi = [0, 1, -1, 99, -100, 2 ** 40, -(2 ** 40), 999999999999, 7, -7, 10 ** 12, 5]
    vr = [1e-300, -1e300, 1.0e100, -9.99999999999995e99, 5e-324, -0.0,
          1.0000000000005, 123456789012.345678, -1e-100, 0.5, 2.5e-15,
          1.7976931348623157e308]
    ca = [2 ** 40, -(2 ** 40)]
    cr = [1e-99, -9.999999999995e-100]
    L = ["12 2 2 2 0"]
    for i, (x, y, z) in enumerate(V):
        L.append("%d %r %r %r" % (i + 1, x, y, z))
    for k, (t, idx) in enumerate(C):
        L.append("%d %d %s %s" % (k + 1, k + 1, t, " ".join(map(str, idx))))
    L += ["2 1 1", "vi, integer", "vr, real"]
    L += ["%d %d %r" % (i + 1, vi[i], vr[i]) for i in range(12)]
    L += ["2 1 1", "ca, integer", "cr, real"]
    L += ["%d %d %r" % (k + 1, ca[k], cr[k]) for k in range(2)]
    open(path, "w").write("\n".join(L) + "\n")


if __name__ == "__main__":
    main()
