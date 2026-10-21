#!/usr/bin/env python3
"""make_decks.py - writes every cpNN_*.lgi deck of this suite (numbered
in group order). Edit a case() below, then run: python3 make_decks.py
The decks are generated: change them here, not by hand."""
import os, textwrap
D = os.path.dirname(os.path.abspath(__file__))
os.makedirs(D, exist_ok=True)
cases = []
def case(name, group, spec, create, what, mesh="tet", cmo="tet", pre="", known=None,
         open_=None, attr=None, expect="valid", reason=None, post="", unused_ok=False, filt=True, doc=None, found=None,
         warns=None, brick_filter=False, coincident_ok=False, tail=None,
         parents_ok=False, shells=None, picture=None, volume=None,
         extra=None):
    cases.append(dict(name=name.split("_", 1)[1], group=group, spec=spec, create=create, what=what, mesh=mesh,
                      cmo=cmo, pre=pre, known=known, open_=open_, attr=attr, expect=expect,
                      reason=reason, post=post, unused_ok=unused_ok, filt=filt, doc=doc, found=found,
                      warns=warns, brick_filter=brick_filter,
                      coincident_ok=coincident_ok, tail=tail,
                      parents_ok=parents_ok, shells=shells, picture=picture,
                      volume=volume, extra=extra))

# ------------------------------------------------------------- xyz
case("cp_xyz_vertices", "xyz", "xyz 9,5,7 0.,2.,0. 5.,6.,2. 1,1,1",
     "createpts / xyz / 9,5,7 / 0.,2.,0. / 5.,6.,2. / 1,1,1",
     "9 x 5 x 7 points; min and max are the first and last points (1,1,1).",
     doc="the doc's first example (box 5 x 4 x 2), finer")
case("cp_xyz_centres", "xyz", "xyz 10,6,8 0.,2.,0. 5.,6.,2. 0,0,0",
     "createpts / xyz / 10,6,8 / 0.,2.,0. / 5.,6.,2. / 0,0,0",
     "the same box with 0,0,0: the points are the centres of 10 x 6 x 8\n"
     "equal cells (x = 0.25, 0.75, ... 4.75).")
case("cp_xyz_mixed", "xyz", "xyz 9,6,7 0.,2.,0. 5.,6.,2. 1,0,1",
     "createpts / xyz / 9,6,7 / 0.,2.,0. / 5.,6.,2. / 1,0,1",
     "vertices in x and z, cell centres in y (1,0,1).")
case("cp_xyz_plane", "xyz", "xyz 7,5,1 0.,0.,0. 3.,2.,0. 1,1,1",
     "createpts / xyz / 7,5,1 / 0.,0.,0. / 3.,2.,0. / 1,1,1",
     "one layer (nk = 1, zmin = zmax): a 7 x 5 grid in the xy plane,\n"
     "connected into triangles in a triplane mesh object.",
     mesh="tri", cmo="triplane")
case("cp_xyz_ratio_vertices", "xyz",
     "xyz 11,5,5 0.,0.,0. 4.,2.,2. 1,1,1 ratio 1,0,0 1.2,1.,1.",
     "createpts / xyz / 11,5,5 / 0.,0.,0. / 4.,2.,2. / 1,1,1 / 1,0,0 / 1.2,1.,1.",
     "ratio zoning in x (1.2): each x step is 1.2 times the one before,\n"
     "first point at x = 0, last at x = 4.")
case("cp_xyz_ratio_xyz", "xyz",
     "xyz 7,7,7 0.,0.,0. 1.,1.,1. 1,1,1 ratio 1,1,1 1.5,1.5,1.5",
     "createpts / xyz / 7,7,7 / 0.,0.,0. / 1.,1.,1. / 1,1,1 / 1,1,1 / 1.5,1.5,1.5",
     "ratio zoning 1.5 in x, y AND z in the unit cube, 7 x 7 x 7 (user,\n"
     "Oct 13): each step is 1.5 x the one before, so the cells grow from\n"
     "the corner (0,0,0) to the corner (1,1,1); last step / first =\n"
     "1.5^5 = 7.6.")
case("cp_xyz_ratio_mirrored", "xyz",
     "xyz 11,5,5 0.,0.,0. 4.,2.,2. 1,1,1 ratio 1,0,0 1.2,1.,1. + "
     "xyz 11,5,5 -4.,0.,0. 0.,2.,2. 1,1,1 ratio 1,0,0 0.8333333333333334,1.,1.",
     "define / RATIO1 / 1.2\n"
     "define / RATIO2 / 0.8333333333333334\n"
     "cmo / create / mo1 / / / tet\n"
     "createpts / xyz / 11,5,5 / 0.,0.,0. / 4.,2.,2. / 1,1,1 / 1,0,0 / RATIO1,1.,1.\n"
     "cmo / create / mo2 / / / tet\n"
     "createpts / xyz / 11,5,5 / -4.,0.,0. / 0.,2.,2. / 1,1,1 / 1,0,0 / RATIO2,1.,1.\n"
     "addmesh / merge / mo / mo1 / mo2\n"
     "cmo / select / mo",
     "two blocks mirrored about x = 0 (user, Oct 13): x = 0 to 4 with\n"
     "ratio 1.2, and x = -4 to 0 with ratio 1/1.2, so the cells are\n"
     "smallest at x = 0 and the two steps next to it are equal. The\n"
     "points are merged (addmesh / merge), the 25 shared at x = 0\n"
     "removed (filter, rmpoint / compress), and connect makes ONE mesh.\n"
     "The ratios are defined first: the second createpts line would\n"
     "be over 80 characters, and LaGriT reads only the first 80 of a line.\n"
     "Connecting each block first and merging the meshes does NOT work:\n"
     "each block picks its own diagonals on the x = 0 face, and 40 faces\n"
     "there do not match (sandbox, Oct 13).",
     cmo=None)
# ------------------------------------------------------------- rtz
case("cp_rtz_doc_example", "rtz", "rtz 4,6,11 0.,0.,0. 3.,360.,10. 1,0,1",
     "createpts / rtz / 4,6,11 / 0.,0.,0. / 3.,360.,10. / 1,0,1",
     "the doc's example: radii 0, 1, 2, 3; 6 angles at cell centres\n"
     "(30, 90, ... 330 deg); z = 0 to 10. 264 points, 6 on the axis at\n"
     "each of 11 heights: 55 duplicates, 209 distinct.",
     doc="CRTPTSRZ.md, second example")
case("cp_rtz_full_vertices", "rtz", "rtz 5,17,5 0.,0.,0. 2.,360.,2. 1,1,1",
     "createpts / rtz / 5,17,5 / 0.,0.,0. / 2.,360.,2. / 1,1,1",
     "a full circle with angles as vertices: 0, 22.5, ... 360 deg, so the\n"
     "0 and 360 deg points coincide (a seam), and r = 0 repeats on the axis.")
case("cp_rtz_arc", "rtz", "rtz 5,7,3 1.,0.,0. 2.,90.,1. 1,1,1",
     "createpts / rtz / 5,7,3 / 1.,0.,0. / 2.,90.,1. / 1,1,1",
     "a quarter annulus, r = 1 to 2, 0 to 90 deg. No duplicates. connect\n"
     "fills the CONVEX HULL, so the cells also cover the dent at r < 1.")
case("cp_rtz_over_360", "rtz", "rtz 3,5,2 0.,0.,0. 1.,720.,1. 1,1,1",
     "createpts / rtz / 3,5,2 / 0.,0.,0. / 1.,720.,1. / 1,1,1",
     "an arc of 720 deg (two turns) with 5 angles: 0, 180, 360, 540, 720,\n"
     "so the second turn lands on the first (red rings in the picture).\n"
     "The points are where the documentation puts them: not a failure of\n"
     "createpts, but a point set that needs filter before connect. It\n"
     "must WARN (user, Oct 13).",
     mesh=None, warns="360",
     found="before FIX 2: no message. FIX 2 (createpts_lg.f, Oct 13):\n"
           "WARNING: createpts / rtz: theta range 720.0 deg is over 360.")
case("cp_rtz_over_360_spiral", "rtz", "rtz 5,30,5 0.,0.,0. 1.,720.,1. 1,1,1",
     "createpts / rtz / 5,30,5 / 0.,0.,0. / 1.,720.,1. / 1,1,1",
     "the user's question (Oct 13): 30 angles over 720 deg, a step of\n"
     "720/29 = 24.8 deg. The second turn falls exactly halfway between the\n"
     "first turn's angles, and only 720 lands on 0: 29 distinct angles,\n"
     "evenly spaced (360/29 deg), in a scrambled order. After filter the\n"
     "mesh is a valid cylinder. Still WARNS: the user may not mean it.",
     warns="360")
# ------------------------------------------------------------- rtp
# the LANL sphere demo, assembled from demo_sphere/*.body (see its
# README.txt): the decks as given, in lagrit.input_driver's order, with
# only the dumps changed
DEMO_ORDER = ["geom", "icos_162", "icos_642", "cube", "icos_642_tet", "rtp",
              "sphere_in_box"]


def sphere_demo_body():
    out = []
    for n in DEMO_ORDER:
        fn = os.path.join(D, "demo_sphere", "lagrit.input_%s.body" % n)
        lines = open(fn).read().rstrip("\n").split("\n")
        out.append("*" + "-" * 60)
        out.append("* lagrit.input_%s" % n)
        out.append("*" + "-" * 60)
        gmv = [k for k, l in enumerate(lines)
               if l.replace(" ", "").lower().startswith("dump/gmv/")]
        last_cp = max([k for k, l in enumerate(lines)
                       if l.replace(" ", "").lower().startswith("createpts/")],
                      default=-1)
        for k, l in enumerate(lines):
            l = l.rstrip()
            for a in ("merged1.inp", "merged2.inp", "cmo_box.inp"):
                l = l.replace(a, "out_cp13_" + a)
            if k in gmv:
                w = l.replace(" ", "").split("/")
                mo = w[3] if len(w) > 3 and w[3] else ""
                # the deck's final GMV dump becomes an AVS dump the
                # checker reads; other GMV dumps are left out
                if n not in ("sphere_in_box", "geom") and k == gmv[-1]:
                    f = "{msh}" if n == "rtp" else "out_cp13_%s.inp" % n
                    out.append("dump / avs / %s / %s" % (f, mo))
                continue
            out.append(l)
            if n == "rtp" and k == last_cp:
                out.append("dump / avs / {pts} / cmo1")
        if n == "sphere_in_box":
            out.append("dump / avs / out_cp13_sphere_in_box.inp / cmo1_3daaa")
    return "\n".join(out)


case("cp_rtp_sphere_demo", "rtp",
     "rtp 3,19,10 0.,0.,0. 1.,360.,180. 1,1,1 + "
     "rtp 3,19,10 1.,0.,0. 2.,360.,180. 1,1,1 + "
     "rtp 3,37,19 2.,0.,0. 3.,360.,180. 1,1,1",
     "",
     "the user's LANL sphere demo (meshing.lanl.gov/proj/DEMO_sphere_\n"
     "mesh_generation, Oct 15), every deck its driver runs, as given\n"
     "(demo_sphere/): the shells geometry, hex icosahedral spheres with\n"
     "162 and 642 vertices per shell (createpts/sphere type 8), a tet\n"
     "sphere from a projected cube (type 1), a tet icosahedral sphere\n"
     "(type 2), the tet rtp shells (this deck's points and mesh), and a\n"
     "hex sphere set into a hex box, cut to one octant. Only the dumps\n"
     "are changed (AVS for the checker). The picture is the six meshes\n"
     "side by side, as on the demo page (the missing lagrit.input_merge\n"
     "made that layout); top right, the box octant (the cutaway).\n"
     "The rtp mesh is checked as a shell r 1-3 in materials 2 and 3;\n"
     "every mesh: cell type, no cell of volume <= 0, no unused vertex\n"
     "(settets' interface parents, itp1 = 41, excepted).",
     cmo=None, mesh="tet", filt=False, coincident_ok=True,
     parents_ok=True, shells="1.,2.,3. 2,3",
     picture="panels out_cp13_icos_642_tet.inp tet green, "
             "out_cp13_cube.inp tet yellowgreen, "
             "out_cp13_sphere_in_box.inp hex red inside, "
             "out_cp13_icos_162.inp hex blue, "
             "out_cp13_icos_642.inp hex cyan, "
             "{msh} tet orange",
     doc="meshing.lanl.gov/proj/DEMO_sphere_mesh_generation",
     tail=None)
cases[-1]["tail"] = sphere_demo_body()
case("cp_rtp_sphere", "rtp", "rtp 5,17,33 0.,0.,0. 2.,180.,360. 1,1,1",
     "createpts / rtp / 5,17,33 / 0.,0.,0. / 2.,180.,360. / 1,1,1",
     "a full sphere: r = 0, 0.5, ... 2; theta (from +z) 0, 11.25, ... 180;\n"
     "phi (from +x) 0, 11.25, ... 360 (theta and phi doubled, user Oct 13).\n"
     "2805 points: the centre, the poles and the phi = 0 / 360 seam\n"
     "repeat.")
case("cp_rtp_octant", "rtp", "rtp 5,7,7 1.,0.,0. 2.,90.,90. 1,1,1",
     "createpts / rtp / 5,7,7 / 1.,0.,0. / 2.,90.,90. / 1,1,1",
     "an octant of a spherical shell, r = 1 to 2, theta and phi 0 to 90.\n"
     "The theta = 0 points of each radius coincide (the pole).")
case("cp_rtp_double_cover", "rtp", "rtp 3,9,9 0.,0.,0. 1.,360.,360. 1,1,1",
     "createpts / rtp / 3,9,9 / 0.,0.,0. / 1.,360.,360. / 1,1,1",
     "theta AND phi both 0 to 360 deg: every point of the sphere is made\n"
     "at least twice. Must WARN. (theta 0-360 with phi 0-180, as in the\n"
     "user's sphere demo, covers the sphere once and must NOT warn.)",
     mesh=None, warns="more than once",
     found="first FIX 2 (Oct 13) warned for theta over 180 alone, which\n"
           "also caught the demo's valid theta 0-360 / phi 0-180 (Oct 15).")
# ------------------------------------------------------------- line
case("cp_line_vertices", "line", "line 21 0.,0.,0. 1.,2.,3. 1",
     "createpts / line / 21 / / / 0.,0.,0. / 1.,2.,3. / 1,1,1",
     "21 points from (0,0,0) to (1,2,3), ends included.", mesh=None)
case("cp_line_centres", "line", "line 10 0.,0.,0. 1.,0.,0. 0",
     "createpts / line / 10 / / / 0.,0.,0. / 1.,0.,0. / 0,0,0",
     "10 cell centres between x = 0 and 1: 0.05, 0.15, ... 0.95.", mesh=None)
case("cp_line_ratio", "line", "line 31 0.,0.,0. 2.,0.,0. 1 ratio 1.2",
     "createpts / line / 31 / / / 0.,0.,0. / 2.,0.,0. / 1,1,1 / 1,1,1 / 1.2,1.2,1.2",
     "31 points from x = 0 to 2 (3 x the points, 2 x the length, user\n"
     "Oct 13), ends included, each step 1.2 x the one before.", mesh=None)
case("cp_line_centres_ratio", "line", "line 10 0.,0.,0. 1.,0.,0. 0 ratio 1.2",
     "createpts / line / 10 / / / 0.,0.,0. / 1.,0.,0. / 0,0,0 / 1,1,1 / 1.2,1.2,1.2",
     "cell centres with ratio zoning 1.2: 10 cells of length s, 1.2 s,\n"
     "... fill x = 0 to 1; the points are their centres.", mesh=None)
case("cp_line_centres_ratio1", "line", "line 10 0.,0.,0. 1.,0.,0. 0 ratio 1.0",
     "createpts / line / 10 / / / 0.,0.,0. / 1.,0.,0. / 0,0,0 / 1,1,1 / 1.,1.,1.",
     "cell centres with ratio zoning ON but ratio 1.0: must be the same\n"
     "points as the cell-centre line deck (0.05, 0.15, ... 0.95).", mesh=None,
     found="FIXED Oct 15 2026 (createpts fixes); before: (5 points) 0.0909, 0.2727, ... 0.8182: the step is 1/5.5, the first gap\n"
           "half a step, the last gap a FULL step. rz.f line branch: rsum =\n"
           "0.5 + (1 + sum r^i) + r^(n-1) should end with 0.5 r^(n-1).")
# ------------------------------------------------------------- brick
case("cp_brick_hex", "brick", "brick xyz 9,5,9 0.,0.,0. 1.,1.,1. 1,1,1",
     "createpts / brick / xyz / 9,5,9 / 0.,0.,0. / 1.,1.,1. / 1,1,1",
     "the doc's example (3,2,3: 4 hexes) at 4 times the resolution:\n"
     "405 vertices, 8 x 4 x 8 = 256 hexes, volume 1.",
     mesh="brick hex", cmo="hex", doc="CRTPTBRICK.md, first example, 4 x finer")
case("cp_brick_hex_ratio", "brick",
     "brick xyz 9,5,5 0.,0.,0. 4.,2.,2. 1,1,1 ratio 1,0,0 1.15,1.,1.",
     "createpts / brick / xyz / 9,5,5 / 0.,0.,0. / 4.,2.,2. / 1,1,1 / 1,0,0 / &\n"
     "  1.15,1.,1.",
     "hexes with ratio zoning 1.15 in x. (The command is continued with &:\n"
     "on one line it was 82 characters, and LaGriT reads only 80.)",
     mesh="brick hex", cmo="hex",
     found="Oct 13: before the line was split, LaGriT read it as ending\n"
           "\"1.15,1.\": the third ratio was lost (it defaulted, so the deck\n"
           "passed by luck).")
case("cp_brick_quad_xy", "brick", "brick xyz 7,5,1 0.,0.,0. 3.,2.,0. 1,1,0",
     "createpts / brick / xyz / 7,5,1 / 0.,0.,0. / 3.,2.,0. / 1,1,0",
     "one layer of quads in the xy plane (nk = 1), with ikz = 0: 35\n"
     "vertices, 24 quads. rzbrick3.f has a branch for each plane (nk = 1,\n"
     "nj = 1, ni = 1). The other way to the same quads: quadxy, then\n"
     "createpts / brick / xyz / 7,5,1 / 1,0,0 / connect (see the\n"
     "connect deck).",
     mesh="brick quad", cmo="quad")
case("cp_brick_quad_xy_vertex", "brick", "brick xyz 7,5,1 0.,0.,0. 3.,2.,0. 1,1,1",
     "createpts / brick / xyz / 7,5,1 / 0.,0.,0. / 3.,2.,0. / 1,1,1",
     "the same quads with ikz = 1 (the doc's usual 1,1,1).",
     mesh="brick quad", cmo="quad",
     found="before FIX 3: abort (Linux), heap corruption: rzbrickg.f, vertex\n"
           "mode, if(nz1.lt.2) nz1=2 wrote 7 x 5 x 2 vertices into room for\n"
           "7 x 5 x 1. FIX 3 (rzbrickg.f, Oct 13): one vertex in a direction\n"
           "is the middle of min and max, as in rz.f.")
case("cp_brick_quad_xz", "brick", "brick xyz 7,1,5 0.,0.,0. 3.,0.,2. 1,0,1",
     "createpts / brick / xyz / 7,1,5 / 0.,0.,0. / 3.,0.,2. / 1,0,1",
     "quads in the xz plane (nj = 1), ijz = 0 (user, Oct 13: all three\n"
     "planes). The checker measures each quad in the xz plane and\n"
     "requires all of them to turn the same way.",
     mesh="brick quad", cmo="quad")
case("cp_brick_quad_xz_vertex", "brick", "brick xyz 7,1,5 0.,0.,0. 3.,0.,2. 1,1,1",
     "createpts / brick / xyz / 7,1,5 / 0.,0.,0. / 3.,0.,2. / 1,1,1",
     "the xz quads with ijz = 1. Before FIX 3 this overran the arrays\n"
     "(the same cause as the xy deck with ikz = 1).",
     mesh="brick quad", cmo="quad")
case("cp_brick_quad_yz", "brick", "brick xyz 1,5,7 0.,0.,0. 0.,2.,3. 0,1,1",
     "createpts / brick / xyz / 1,5,7 / 0.,0.,0. / 0.,2.,3. / 0,1,1",
     "quads in the yz plane (ni = 1), iiz = 0.",
     mesh="brick quad", cmo="quad")
case("cp_brick_quad_yz_vertex", "brick", "brick xyz 1,5,7 0.,0.,0. 0.,2.,3. 1,1,1",
     "createpts / brick / xyz / 1,5,7 / 0.,0.,0. / 0.,2.,3. / 1,1,1",
     "the yz quads with iiz = 1. Before FIX 3 this overran the arrays.",
     mesh="brick quad", cmo="quad")
case("cp_brick_cylinder", "brick", "brick rtz 5,17,5 0.,0.,0. 2.,360.,2. 1,1,1",
     "createpts / brick / rtz / 5,17,5 / 0.,0.,0. / 2.,360.,2. / 1,1,1",
     "a hex cylinder (as the doc's example, smaller): radii 0, 0.5 ... 2;\n"
     "angles 0, 22.5, ... 360; z = 0, 0.5, ... 2. The 0 / 360 deg seam and\n"
     "the axis are vertices at the same place, so the brick is NOT closed\n"
     "there: filter / rmpoint / compress / resetpts / itp (user, Oct 13)\n"
     "merge them. The result is one connected mesh with valid topology;\n"
     "the 64 hexes on the axis then list the axis vertices twice (their\n"
     "axis face has area 0): reported as degenerate_cells (user, Oct 13).",
     mesh="brick hex", cmo="hex", doc="CRTPTBRICK.md, last example",
     brick_filter=True)
case("cp_brick_cylinder_jsym", "brick", "brick rtz 5,17,5 0.,0.,0. 2.,360.,2. 1,1,1",
     "createpts/brick/rtz/5,17,5/0.,0.,0./2.,360.,2./1,1,1/0,0,0/1.,1.,1./0,1,0",
     "the same cylinder with jsym = 1, the 22nd token (\"symmetry flags\" in\n"
     "the code; documented Oct 13, user). jsym = 1 makes the j direction (theta)\n"
     "PERIODIC: the last row of cells (from 337.5 to 360 deg) is joined to\n"
     "the FIRST row of vertices (0 deg) instead of the last (360 deg), so\n"
     "the seam closes without filter. The 25 vertices at 360 deg (5 radii\n"
     "x 5 heights) are made but no cell uses them. isym / ksym do the same in i / k.\n"
     "The axis is not merged (that needs filter). Written without spaces:\n"
     "with them the line is 91 characters, and LaGriT reads only 80.",
     mesh="brick hex", cmo="hex", unused_ok=True, coincident_ok=True,
     doc="CRTPTBRICK.md (jsym, Oct 13 2026)",
     found="Oct 13: until now this line had spaces, was 91 characters, and\n"
           "LaGriT never saw the jsym flag.")
case("cp_brick_hex_default_mo", "brick", "brick xyz 9,5,9 0.,0.,0. 1.,1.,1. 1,1,1",
     "cmo / create / mo\n"
     "createpts / brick / xyz / 9,5,9 / 0.,0.,0. / 1.,1.,1. / 1,1,1",
     "the hex brick in a mesh object made with NO cell type (tet by\n"
     "default), as LaGriT's own tests do (level01 filter, quality,\n"
     "surface): brick sets the cell type itself (rzbrick3.f), so this is\n"
     "the same mesh as in a hex mesh object: 405 vertices, 256 hexes.\n"
     "Refused by FIX 2 (Oct 13); allowed again by FIX 3 (user, Oct 16).",
     mesh="brick hex", cmo=None,
     found="FIX 2 (Oct 13) to FIX 3 (Oct 16): ERROR 'the mesh object must\n"
           "be hex'. Original code: the same AVS file as a hex mesh object.")
case("cp_brick_connect_quadxyz", "brick", "none",
     "quadxyz / 5,7,5 / 0.,0.,0. / 1.,0.,0. / 1.5,0.5,2.0 / .5,.2,2.5 / &\n"
     "  -1.,1.5,0. / 2.0,0.,0.0 / 2.1,1.9,2.4 / -0.2,1.8,2.3\n"
     "createpts / brick / xyz / 5,7,5 / 1,0,0 / connect",
     "the doc's second example: vertices from quadxyz, hexes from\n"
     "createpts / brick / ... / connect. 175 vertices, 96 hexes.",
     mesh="brick hex", cmo="hex", doc="CRTPTBRICK.md, second example")
case("cp_brick_append", "brick", "none",
     "createpts / xyz / 2,2,2 / 5.,5.,5. / 6.,6.,6. / 1,1,1\n"
     "cmo / setatt / mo / itp1 / 1,0,0 / 12\n"
     "createpts / brick / xyz / 5,5,5 / 0.,0.,0. / 1.,1.,1. / 1,1,1",
     "8 vertices already in the mesh object (itp1 set to 12), then a brick\n"
     "of 125 more. The first 8 must keep itp1 = 12.",
     mesh="brick hex", cmo="hex", unused_ok=True, attr="itp1 1 8 12",
     found="FIXED Oct 15 2026 (createpts fixes); before: itp1 of vertices 1-8 is 0 after the brick. rzbrick3.f:\n"
           "do i1=1,nptsv: itp1(i1)=0 should run from ifpt1.")
# ------------------------------------------------------------- interp
case("cp_interp_brick_two_surfaces", "interp", "none",
     "cmo / create / mo / / / quad\n"
     "createpts / brick / xyz / 4,3,1 / 0.,0.,0. / 3.,2.,0. / 1,1,0\n"
     "pset / p1 / seq / 1,0,0\n"
     "copypts / mo / mo / 0,0 / 1,0,0\n"
     "zq / isetwd / 0,0,0 / 0\n"
     "pset / p2 / seq / 0,0,0\n"
     "trans / pset,get,p2 / 0.,0.,0. / 0.,0.,2.\n"
     "createpts / brick / interp / 5 / pset,get,p1 / pset,get,p2 / mo_hex",
     "hexes between two copies of a quad surface (the method of the\n"
     "user's sphere_in_box demo): a 4 x 3 quad layer at z = 0, a copy\n"
     "moved to z = 2, then createpts / brick / interp fills the space\n"
     "between them into a NEW mesh object, mo_hex. Two bugs found by\n"
     "this (Oct 15): FIX 2 refused brick / interp ('interp is not a\n"
     "geometry'); and zq / isetwd did not reliably clear the point-set\n"
     "bits copypts had copied (zq.f tested two error codes it never\n"
     "set), so p1 held both surfaces: 'Number of nodes in FROM not\n"
     "equal TO'. The cells must fill the 3 x 2 x 2 box exactly.",
     cmo=None, mesh="hex", volume="12.",
     found="original code: ERROR:rzinterp, FROM 24 not equal TO 12 (zq).\n"
           "FIX 2 as first committed: ERROR, interp is not a geometry.\n"
           "With both fixes: 84 vertices, 36 hexes, volume 12.",
     tail="cmo / select / mo_hex\n"
          "dump / avs / {pts} / mo_hex\n"
          "quality\n"
          "dump / avs / {msh} / mo_hex")
case("cp_interp_twist_45",  "interp",
     "interp 6 || xyz 5,5,1 -10.,-10.,0. 10.,10.,0. 1,1,1 || "
     "rotz 45 xyz 5,5,1 -10.,-10.,20. 10.,10.,20. 1,1,1",
     "define / R45 / 14.142135623730951\n"
     "define / R45N / -14.142135623730951\n"
     "cmo / create / mo_quad1 / / / quad\n"
     "quadxy / 5 5 / -10. -10. 0. / 10. -10. 0. / 10. 10. 0. / -10. 10. 0.\n"
     "createpts / brick / xyz / 5,5,1 / 1,0,0 / connect\n"
     "cmo / create / mo_quad2 / / / quad\n"
     "quadxy / 5 5 / 0. R45N 20. / R45 0. 20. / 0. R45 20. / R45N 0. 20.\n"
     "createpts / brick / xyz / 5,5,1 / 1,0,0 / connect\n"
     "addmesh / merge / mo_quads / mo_quad1 / mo_quad2\n"
     "cmo / select / mo_quads\n"
     "createpts / interp / 6 / 1 0 0 / 1 0 0 / mo_hex",
     "the doc's quads -> hexes, as a twist (user, Oct 16): two 20 x 20\n"
     "quad surfaces of 4 x 4 cells in xy planes, z = 0 and z = 20, the\n"
     "upper one turned 45 deg about the z axis (its corners are the lower\n"
     "one's, rotated). 6 layers of vertices between vertex i below and\n"
     "vertex i above: (6 + 2) x 25 = 200 vertices, 7 x 16 = 112 hexes,\n"
     "each layer turned a little more. Every vertex must lie on the\n"
     "straight line between its pair, at k/7 of the way. Picture: the 7\n"
     "layers coloured blue (bottom) to red (top), seen from 45 deg above.",
     cmo=None, mesh="hex", picture="layers 16 tilt 45",
     doc="createpts_interp.md, quads to hex (as a twist)",
     extra=[("vertices", "200"), ("cells", "112")],
     tail="cmo / select / mo_hex\n"
          "dump / avs / {pts} / mo_hex\n"
          "quality\n"
          "dump / avs / {msh} / mo_hex")
case("cp_interp_points_triplane", "interp",
     "interp 10 || line 10 -1.,0.,0. 1.,0.,0. 1 || line 10 0.,-1.,1. 0.,1.,1. 1",
     "createpts/line/10///-1.,0.,0.,1.,0.,0./1,1,1\n"
     "createpts/line/10///0.,-1.,1.,0.,1.,1./1,1,1\n"
     "createpts/ interp / 10 / 1 0 0 / 1 0 0 / cmo_pts",
     "the doc's point example (two lines of 10 points, no cells; the\n"
     "doc left out the required switches: 1,1,1 added), TRIPLANE mesh\n"
     "object: 10 points between each pair, 12 x 10 = 120 points.",
     cmo="triplane", mesh=None, doc="createpts_interp.md, first example",
     found="FIXED Oct 15 2026 (createpts fixes); before: 96 of 120 points off by up to 4.7e-8: rzinterp computes the\n"
           "fraction as real(l-1)/real(ilayers+1) (single precision).",
     tail="cmo / select / cmo_pts\n"
          "dump / avs / {pts} / cmo_pts")
case("cp_interp_points_tet_mo", "interp",
     "interp 10 || line 10 -1.,0.,0. 1.,0.,0. 1 || line 10 0.,-1.,1. 0.,1.,1. 1",
     "createpts/line/10///-1.,0.,0.,1.,0.,0./1,1,1\n"
     "createpts/line/10///0.,-1.,1.,0.,1.,1./1,1,1\n"
     "createpts/ interp / 10 / 1 0 0 / 1 0 0 / cmo_pts",
     "the same in a TET mesh object (the usual default): only points,\n"
     "no cells, so the same 120 points are expected.",
     cmo="tet", mesh=None,
     found="FIXED Oct 15 2026 (createpts fixes); before: 'Grid topology too high: 3' (not an ERROR); cmo_pts empty.\n"
           "rzbrick3.f rzinterp tests ndimensions_topo, not the cells.",
     tail="cmo / select / cmo_pts\n"
          "dump / avs / {pts} / cmo_pts")
case("cp_interp_lines_to_quads", "interp", "none",
     "cmo / create / mo1 / / / line\n"
     "createpts / brick / xyz / 5,1,1 / 0.,0.,0. / 4.,0.,0. / 1,1,1\n"
     "cmo / create / mo2 / / / line\n"
     "createpts / brick / xyz / 5,1,1 / 0.,2.,0. / 4.,2.,0. / 1,1,1\n"
     "addmesh / merge / mo3 / mo1 / mo2\n"
     "cmo / select / mo3\n"
     "createpts / interp / 3 / 1 0 0 / 1 0 0 / mo_quad",
     "two lines of 4 line cells, merged; 3 layers between: the doc's\n"
     "'line -> quad'. 5 x 5 = 25 vertices, 4 x 4 = 16 quads.",
     cmo=None, mesh="quad", extra=[("vertices", "25"), ("cells", "16")],
     tail="cmo / select / mo_quad\n"
          "dump / avs / {pts} / mo_quad\n"
          "quality\n"
          "dump / avs / {msh} / mo_quad")
case("cp_interp_points_line_mo", "interp",
     "interp 10 || line 10 -1.,0.,0. 1.,0.,0. 1 || line 10 0.,-1.,1. 0.,1.,1. 1",
     "createpts/line/10///-1.,0.,0.,1.,0.,0./1,1,1\n"
     "createpts/line/10///0.,-1.,1.,0.,1.,1./1,1,1\n"
     "createpts/ interp / 10 / 1 0 0 / 1 0 0 / cmo_pts",
     "the doc's point example in a LINE mesh object (points, no cells):\n"
     "the same 120 points expected.",
     cmo="line", mesh=None,
     found="FIXED Oct 15 2026 (createpts fixes); before: ERROR: Illegal Mesh Type: qud (rzbrick3.f rzinterp sets\n"
           "cmesh_type = 'qud' for a 1D source); cmo_pts empty.",
     tail="cmo / select / cmo_pts\n"
          "dump / avs / {pts} / cmo_pts")
# ------------------------------------------------------------- vector
case("cp_vector_simple_cubic", "vector", "vector xyz 3,3,3 1,0,0 0,1,0 0,0,1",
     "createpts/vector/xyz/3,3,3/1,0,0/0,1,0/0,0,1",
     "the doc's simple cubic example: 4 x 4 x 4 points 0..3 (n + 1 per\n"
     "direction, as rz/xyz/n1+1,n2+1,n3+1/...).", mesh="tet",
     doc="CRTPTRZV_LG.md, simple cubic")
case("cp_vector_bcc", "vector", "vector xyz 2,2,2 .5,.5,.5 .5,.5,-.5 .5,-.5,-.5",
     "createpts/vector/xyz/2,2,2/.5,.5,.5/.5,.5,-.5/.5,-.5,-.5/",
     "the doc's body centred cubic example: 3 x 3 x 3 = 27 points along\n"
     "three vectors to cube centres.", mesh="tet",
     doc="CRTPTRZV_LG.md, body centred cubic")
case("cp_vector_hex_lattice", "vector", "vector xyz 4,4,1 1,0,0 .5,0.866,0 0,0,1",
     "createpts/vector/xyz/4,4,1/1,0,0/.5,0.866,0/0,0,1",
     "the doc's hexagonal lattice in the xy plane, two layers in z.",
     mesh="tet", doc="CRTPTRZV_LG.md, hexagonal lattice")
case("cp_vector_spiral_rtz", "vector",
     "vector rtz 20,0,0 .1,10.,1 0,0,0 0,0,0 0,0,0 1.1,1,.9",
     "createpts/vector/rtz/20,0,0/.1,10.,1/ , , / , , / , , /1.1,1,.9",
     "the doc's spiral: 21 points, each step r x 1.1, theta + 10 deg, z x\n"
     "0.9 of the one before.", mesh=None, doc="CRTPTRZV_LG.md, spiral")
case("cp_vector_doc_fcc_as_written", "vector", "xyz 2,2,2 .5,.5,0 0,.5,.5 0,0,0",
     "createpts/xyz/2,2,2/.5,.5,0/0,.5,.5/.5,0,.5/",
     "the doc's face centred cubic example AS WRITTEN: it says\n"
     "createpts/xyz/..., not createpts/vector/xyz/..., so LaGriT reads an\n"
     "xyz box from (.5,.5,0) to (0,.5,.5) with switches .5,0,.5 (-> 0).\n"
     "The same slip is in the doc's hexagonal, diamond and hcp examples.",
     mesh=None, open_="the doc's fcc example is createpts/xyz, not createpts/vector")
# ------------------------------------------------------------- random
case("cp_random_xyz_box", "random", "none",
     "createpts/random/xyz/0.1/0,0,0/1,1,1/0,0,0/0.05/98765,4321",
     "random points, target spacing 0.1, in the unit cube, edge\n"
     "protection 0.05 (spacing/2), fixed seeds. The doc: points on the\n"
     "corners, edges and faces, interior points at least edgedist from\n"
     "the boundary. Then filter and connect.", mesh="tet",
     extra=[("random", "xyz 0.1 0,0,0 1,1,1 0.05")],
     doc="CRTPTRZRAN.md, first example (with seeds)")
case("cp_random_seeds_repeat", "random", "none",
     "",
     "the same command twice with the same seeds, in two mesh objects:\n"
     "the doc says the points must be identical. The first set is then\n"
     "connected (user, Oct 16).", cmo=None, mesh="tet",
     extra=[("same_as", "out_{pts}")],
     tail="cmo / create / mo1 / / / tet\n"
          "createpts/random/xyz/0.25/0,0,0/1,1,1/0,0,0/0.125/98765,4321\n"
          "dump / avs / {pts} / mo1\n"
          "cmo / create / mo2 / / / tet\n"
          "createpts/random/xyz/0.25/0,0,0/1,1,1/0,0,0/0.125/98765,4321\n"
          "dump / avs / out_{pts} / mo2\n"
          "cmo / select / mo1\n"
          "connect\n"
          "cmo / setatt / mo1 / itetclr / 1,0,0 / 1\n"
          "resetpts / itp\n"
          "quality\n"
          "dump / avs / {msh} / mo1")
case("cp_random_rtz_cylinder", "random", "none",
     "createpts/random/rtz/0.1/0,0,0/1,360,1/2,3,4/0.05/98765,4321",
     "random points in a cylinder r 1, theta 0-360, z 0-1, centred at\n"
     "(2,3,4), edge protection 0.05 (the doc's second example with the\n"
     "angle and height in their places, and seeds). Then connect.",
     mesh="tet", extra=[("random", "rtz 0.1 0,0,0 1,360,1 0.05 2,3,4")])
case("cp_random_rtp_sphere_surface", "random", "none",
     "createpts/random/rtp/0.5/5,0,0/5,180,360/0,0,0/0.25/98765,4321",
     "random points on the surface of a sphere of radius 5 (rmin = rmax\n"
     "= 5, theta 0-180, phi 0-360), target spacing 0.5: about\n"
     "4 pi 25 / 0.25 = 1257 points (the doc's third example, corrected).\n"
     "Then connected: tets filling the sphere (user, Oct 16).",
     mesh="tet", extra=[("random", "rtp 0.5 5,0,0 5,180,360 0.25")])
case("cp_random_doc_rtz_as_written", "random", "none",
     "createpts / random / rtz / .1 /  0,0,0 / 1,,360 / 2,3,4 / 0.2",
     "the doc's second example AS WRITTEN ('a cylinder of radius 1'):\n"
     "rmax = 1,,360 is r = 1, theta = (default) 0, z = 360, so the\n"
     "points fill a half-disc strip 360 high, not a cylinder.",
     mesh=None, open_="the doc's rtz example puts 360 in z: a 360 high strip, 36000 points")
case("cp_random_doc_rtp_as_written", "random", "none",
     "createpts / random / rtp / .5 /  5,0,0 / 5,,360 /  , ,  /  / 98765 4321/",
     "the doc's third example AS WRITTEN ('the surface of a sphere of\n"
     "radius 5'): rmax = 5,,360 leaves theta at 0, so it makes 1 point.",
     mesh=None, open_="the doc's rtp example leaves theta at 0: 1 point, not a sphere")
# ------------------------------------------------------------- amr
case("cp_amr_region_refine", "amr", "none",
     "",
     "amr in a box region (user, Oct 16): a 10 x 10 x 2 hex brick (11 x 11\n"
     "x 3 vertices, 1 apart); the hexes with a vertex in the box\n"
     "0-5.1 x 0-5.1 x 0-2.1 refined twice. The points are copied into a\n"
     "tet mesh object and connected. Every point on the 1/4 lattice,\n"
     "once; the tets fill the 10 x 10 x 2 box (volume 200). amr refines\n"
     "hex CELLS, so the vertices come from createpts / brick (createpts\n"
     "/ xyz makes points only, which amr has nothing to refine).",
     cmo=None, mesh="tet", volume="200.",
     extra=[("lattice", "0.25")],
     tail="cmo / create / cmo / / / hex\n"
          "createpts / brick / xyz / 11 11 3 / 0. 0. 0. / 10. 10. 2. / 1 1 1\n"
          "surface/s_refine/reflect/box/0.,0.,0./5.1,5.1,2.1\n"
          "region/r_refine/le s_refine\n"
          "createpts/amr/r_refine/2\n"
          "dump / avs / {pts} / cmo\n"
          "cmo / create / mo_tet\n"
          "copypts / mo_tet / cmo\n"
          "cmo / select / mo_tet\n"
          "connect\n"
          "cmo / setatt / mo_tet / itetclr / 1,0,0 / 1\n"
          "resetpts / itp\n"
          "quality\n"
          "dump / avs / {msh} / mo_tet")
case("cp_amr_doc_demo_connect", "amr", "none",
     "",
     "the doc's whole demo: the amr points copied into a tet mesh object,\n"
     "setpts, connect (two materials, split by the plane x = z), settets.\n"
     "The tets must fill the unit cube (volume 1).",
     cmo=None, mesh="tet", parents_ok=True, coincident_ok=True,
     volume="1.", doc="CREATEPTSAMR.md, demo",
     known="the doc's amr demo crashes in connect (multi_material)",
     found="segmentation fault in connect, in multi_material (valgrind:\n"
           "invalid write); also on the original code. Not createpts.\n"
           "Oct 16: valgrind (Debug build): invalid write at\n"
           "multi_material.f:982 icontab(1,nconbnd)=n, called from\n"
           "connect.f:886, as icontab gets a new interface-type entry.\n"
           "Also crashes with imt 1, with connect before setpts, with or\n"
           "without cmo/release. PARKED (user, Oct 16: do not modify\n"
           "multi_material.f / connect.f for now).\n"
           "The user's way round it: imt 1 for every vertex, connect,\n"
           "then setpts and settets. It runs, but connect then sees one\n"
           "material: settets reports 249 multimaterial tets (cross the\n"
           "plane x = z). Not a conforming two-material mesh.",
     tail="cmo/create/cmo///hex\n"
          "surface/inside/reflect/box/0,0,0/1,1,1\n"
          "surface/diag/intrface/plane/0,0,0/1,0,1/1,1,1\n"
          "region/lin/ le inside and ge diag /\n"
          "region/rin/ le inside and lt diag /\n"
          "mregion/mlin/ le inside and gt diag /\n"
          "mregion/mrin/ le inside and lt diag /\n"
          "quadxyz/2,2,2/0.,0.,0./1.,0.,0./1.,1.,0./0.,1.,0./ &\n"
          "       0.,0.,1./1.,0.,1./1.,1.,1./0.,1.,1./\n"
          "setpts\n"
          "createpts/brick/xyz/2,2,2/1,0,0/connect\n"
          "createpts/amr/lin/1\n"
          "createpts/amr/rin/3\n"
          "dump / avs / {pts} / cmo\n"
          "cmo/create/cmot///tet\n"
          "surface/inside/reflect/box/0,0,0/1,1,1\n"
          "surface/diag/intrface/plane/0,0,0/1,0,1/1,1,1\n"
          "region/lin/ le inside and ge diag /\n"
          "region/rin/ le inside and lt diag /\n"
          "mregion/mlin/ le inside and gt diag /\n"
          "mregion/mrin/ le inside and lt diag /\n"
          "copypts/cmot/cmo\n"
          "cmo/select/cmot\n"
          "cmo/release/cmo\n"
          "cmo/setatt/cmot/itp/1,0,0/0\n"
          "cmo/setatt/cmot/imt/1,0,0/0\n"
          "setpts\n"
          "connect\n"
          "settets\n"
          "quality\n"
          "dump / avs / {msh} / cmot")
case("cp_amr_region_brick", "amr", "none",
     "",
     "amr with a region on a 2 x 2 x 2 hex brick (8 hexes, all in the\n"
     "region): every hex split once: the 5 x 5 x 5 points of the 1/4\n"
     "lattice, 125 vertices.", cmo=None, mesh=None,
     extra=[("lattice", "0.25"), ("same_set", "out_{pts}")],
     found="FIXED Oct 15 2026 (createpts fixes); before: MMVERIFY: PANIC! FATAL ERROR Array storage was corrupted;\n"
           "PROGRAM STOP (exit status 0). rzamr_lg.f checks room for\n"
           "nelements+1 but writes 8 hexes per refined hex.",
     tail="cmo/create/mo///hex\n"
          "surface/s/reflect/box/-1,-1,-1/2,2,2\n"
          "region/r/ le s /\n"
          "mregion/m/ le s /\n"
          "createpts/brick/xyz/3,3,3/0.,0.,0./1.,1.,1./1,1,1\n"
          "setpts\n"
          "createpts/amr/r/1\n"
          "dump / avs / {pts} / mo\n"
          "cmo / create / ref / / / tet\n"
          "createpts/xyz/5,5,5/0.,0.,0./1.,1.,1./1,1,1\n"
          "dump / avs / out_{pts} / ref")
case("cp_amr_all", "amr", "none",
     "",
     "amr with no region (the doc's 'refine the entire mesh') on the same\n"
     "brick: the same 125 points expected.", cmo=None, mesh=None,
     extra=[("lattice", "0.25"), ("same_set", "out_{pts}")],
     found="FIXED Oct 15 2026 (createpts fixes); before: segmentation fault in rzamr_lg: with no region it writes\n"
           "ireg(1) = 1 through a pointer never allocated (and only ever\n"
           "element 1); the region test later reads the same array.",
     tail="cmo/create/mo///hex\n"
          "createpts/brick/xyz/3,3,3/0.,0.,0./1.,1.,1./1,1,1\n"
          "setpts\n"
          "createpts/amr\n"
          "dump / avs / {pts} / mo\n"
          "cmo / create / ref / / / tet\n"
          "createpts/xyz/5,5,5/0.,0.,0./1.,1.,1./1,1,1\n"
          "dump / avs / out_{pts} / ref")
# ------------------------------------------------------------- median
case("cp_median_quad_doc", "median", "none",
     "",
     "the doc's example: a 3 x 3 quad mesh; createpts / median makes\n"
     "cell attributes xmed, ymed, zmed: the average of each cell's\n"
     "vertices.", cmo=None, mesh="quad", extra=[("cellattr", "median")],
     doc="createpts_median.md",
     tail="cmo/create/cmo_quad///quad\n"
          "quadxy 3 3/0. 0. 0./1. 0. 0./1. 1. 0./0. 1. 0.\n"
          "rzbrick/xyz/3,3,1/1,0,0/connect\n"
          "cmo / select / cmo_quad\n"
          "createpts / median\n"
          "dump / avs / {pts} / cmo_quad\n"
          "dump / avs / {msh} / cmo_quad")
case("cp_median_hex_ratio", "median", "none",
     "",
     "hexes of unequal size (ratio zoning 1.3 in x): the median point\n"
     "of each is the average of its 8 vertices.", cmo=None, mesh="hex",
     extra=[("cellattr", "median")],
     tail="cmo / create / mo / / / hex\n"
          "createpts/brick/xyz/6,4,3/0.,0.,0./3.,2.,1./1,1,1/1,0,0/1.3,1.,1.\n"
          "createpts / median\n"
          "dump / avs / {pts} / mo\n"
          "dump / avs / {msh} / mo")
case("cp_median_tet_random", "median", "none",
     "",
     "tets from random points (seeded), connected: the median point of\n"
     "each tet is the average of its 4 vertices.", cmo=None, mesh="tet",
     extra=[("cellattr", "median")],
     tail="cmo / create / mo / / / tet\n"
          "createpts/random/xyz/0.25/0,0,0/1,1,1/0,0,0/0.125/98765,4321\n"
          "filter / 1,0,0\n"
          "rmpoint / compress\n"
          "connect\n"
          "createpts / median\n"
          "dump / avs / {pts} / mo\n"
          "dump / avs / {msh} / mo")
case("cp_median_no_cells", "median", "none",
     "createpts/xyz/3,3,3/0.,0.,0./1.,1.,1./1,1,1\n"
     "createpts / median",
     "points but no cells: there is nothing to do; it should say so\n"
     "(a WARNING).", mesh=None, warns="WARNING",
     found="FIXED Oct 15 2026 (createpts fixes); before: no message; xmed, ymed, zmed made with length 0.")
# ------------------------------------------------------------- voronoi
case("cp_voronoi_tet_doc", "voronoi", "none",
     "",
     "the doc's example: a 3 x 3 x 3 hex brick cut into tets\n"
     "(hextotet / 6); createpts / voronoi makes xvor, yvor, zvor: the\n"
     "centre of each tet's circumscribed sphere.", cmo=None, mesh="tet",
     extra=[("cellattr", "voronoi")], doc="createpts_voronoi.md",
     tail="cmo/create/cmo_hex ///hex\n"
          "createpts/brick/xyz/3,3,3/0.,0.,0./1.,1.,1./1,1,1\n"
          "hextotet / 6 / cmo_tet / cmo_hex\n"
          "cmo / select / cmo_tet\n"
          "createpts / voronoi\n"
          "dump / avs / {pts} / cmo_tet\n"
          "dump / avs / {msh} / cmo_tet")
case("cp_voronoi_tet_random", "voronoi", "none",
     "",
     "tets from random points (seeded): each voronoi point must be\n"
     "equally far from the tet's 4 vertices.", cmo=None, mesh="tet",
     extra=[("cellattr", "voronoi")],
     tail="cmo / create / mo / / / tet\n"
          "createpts/random/xyz/0.25/0,0,0/1,1,1/0,0,0/0.125/98765,4321\n"
          "filter / 1,0,0\n"
          "rmpoint / compress\n"
          "connect\n"
          "createpts / voronoi\n"
          "dump / avs / {pts} / mo\n"
          "dump / avs / {msh} / mo")
case("cp_voronoi_tri", "voronoi", "none",
     "",
     "triangles in the xy plane: each voronoi point is the centre of\n"
     "the triangle's circumscribed circle, in its plane.", cmo=None,
     mesh="tri", extra=[("cellattr", "voronoi")],
     tail="cmo / create / mo / / / triplane\n"
          "createpts/xyz/6,4,1/0.,0.,0./3.,1.5,0./1,1,1\n"
          "connect\n"
          "createpts / voronoi\n"
          "dump / avs / {pts} / mo\n"
          "dump / avs / {msh} / mo")
case("cp_voronoi_hex", "voronoi", "none",
     "createpts/brick/xyz/3,3,3/0.,0.,0./1.,1.,1./1,1,1\n"
     "createpts / voronoi",
     "a hex mesh: voronoi points are defined only for triangles and\n"
     "tets, so it must be REFUSED with an ERROR that says why.",
     cmo="hex", mesh=None, expect="error", reason="ERROR: createpts / voronoi",
     found="FIXED Oct 15 2026 (createpts fixes); before: 'element not triangle or tetrahedron' then '  VORPTS ERROR -';\n"
           "the command is not marked as failed (vorpts_lg.f).")
case("cp_voronoi_no_cells", "voronoi", "none",
     "createpts/xyz/3,3,3/0.,0.,0./1.,1.,1./1,1,1\n"
     "createpts / voronoi",
     "points but no cells: REFUSED with an ERROR that says why.",
     mesh=None, expect="error", reason="ERROR: createpts / voronoi",
     found="FIXED Oct 15 2026 (createpts fixes); before: '  VORPTS ERROR -' only.")
# ------------------------------------------------------------- syntax
case("cp_syntax_negative_count", "syntax", "none",
     "createpts / xyz / -3,3,1 / 0.,0.,0. / 2.,2.,0. / 1,1,1",
     "a NEGATIVE count (-3). rz.f's comments promise a STAGGERED pattern:\n"
     "every other row shifted half a step, with one point fewer, so the\n"
     "points make triangles (a triangular, close-packed lattice) instead\n"
     "of squares. Since Jan 2000 (rz.f rev 1.22, which set n = max(1, n)\n"
     "to stop a count of 0) that branch can never run, and -3 silently\n"
     "gave 1 point. Now an ERROR.",
     mesh=None, expect="error", reason="must be 1 or more",
     found="before FIX 2: 3 points (x = 1, the middle), no message.\n"
           "FIX 2 (Oct 13): ERROR: createpts / xyz: a count is -3 (token 3):\n"
           "it must be 1 or more.")
case("cp_syntax_zero_count", "syntax", "none",
     "createpts / xyz / 0,2,1 / 0.,0.,0. / 2.,2.,0. / 1,1,1",
     "a count of 0: an ERROR (user, Oct 13: not a feature).",
     mesh=None, expect="error", reason="must be 1 or more",
     found="before FIX 2: taken as 1, 2 points, no message.")
case("cp_syntax_line_one_point", "syntax", "none",
     "createpts / line / 1 / / / 0.,0.,0. / 1.,0.,0. / 1,1,1",
     "1 point on a line: an ERROR (user, Oct 13: something is wrong with\n"
     "the input; it should not be a feature).",
     mesh=None, expect="error", reason="a line needs 2 or more points",
     found="before FIX 2: (0,0,0), the start (the middle if ratio is on).")
case("cp_syntax_missing_tokens", "syntax", "none",
     "createpts / xyz / 3,3",
     "the command stops after the counts: no box, no flags.",
     mesh=None, expect="error", reason="incomplete command",
     found="before FIX 2: 9 points from whatever was left in memory, no\n"
           "message. FIX 2: ERROR: createpts / xyz: incomplete command, 4\n"
           "tokens of 14, and the full form of the command.")
case("cp_syntax_no_switches", "syntax", "none",
     "createpts / xyz / 3,3,3 / 0.,0.,0. / 1.,1.,1.",
     "the switches iiz,ijz,ikz left out (11 tokens): they are required.",
     mesh=None, expect="error", reason="incomplete command",
     found="before: cell centres without a message (the parser leaves 0\n"
           "past the end of a command). Oct 15-16: optional, then taken\n"
           "back (user: required). Now: ERROR: incomplete command, 11\n"
           "tokens of 14.")
case("cp_syntax_ratio_incomplete", "syntax", "none",
     "createpts / xyz / 3,3,3 / 0.,0.,0. / 1.,1.,1. / 1,1,1 / 1,0,0",
     "ratio switches without the ratios.",
     mesh=None, expect="error", reason="ratio zoning needs",
     found="before FIX 2: the ratios were read past the end of the command.")
case("cp_syntax_illegal_option", "syntax", "none",
     "createpts / xzy / 3,3,3 / 0.,0.,0. / 1.,1.,1. / 1,1,1",
     "an option that does not exist (xzy).", mesh=None, expect="error",
     reason="unknown option: xzy",
     found="before FIX 2: \"Illegal createpts option xzy\", not flagged as\n"
           "an ERROR. Now: ERROR: createpts: unknown option: xzy, and the\n"
           "list of options.")
case("cp_syntax_no_option", "syntax", "none", "createpts",
     "createpts with nothing after it.", mesh=None, expect="error",
     reason="no option given",
     found="before FIX 2: \"Illegal createpts option create\" (a garbage\n"
           "word read past the end of the command).")
# ------------------------------------------------------------- errors
case("cp_errors_brick_into_tets", "errors", "none",
     "createpts / xyz / 3,3,3 / 0.,0.,0. / 1.,1.,1. / 1,1,1\n"
     "connect\n"
     "createpts / brick / xyz / 5,5,5 / 0.,0.,0. / 1.,1.,1. / 1,1,1",
     "a brick added to a mesh object that already HAS tets: hexes cannot\n"
     "join tets in one mesh object. REFUSED (user, Oct 16; FIX 3). An\n"
     "EMPTY mesh object of any type is re-typed by brick instead (deck\n"
     "brick_hex_default_mo).",
     mesh=None, cmo="tet", expect="error", reason="already has",
     found="original code: accepted. The 48 tets and 64 hexes end up in one mesh\n"
           "object whose cell type brick set to hex: 112 cells, the tets read\n"
           "as 8-vertex cells. FIX 2 to FIX 3: refused for any tet mesh object.")
case("cp_errors_quad_brick_into_hexes", "errors", "none",
     "createpts / brick / xyz / 3,3,3 / 0.,0.,0. / 1.,1.,1. / 1,1,1\n"
     "createpts / brick / xyz / 7,5,1 / 0.,0.,0. / 3.,2.,0. / 1,1,0",
     "a brick of one layer (quads) added to a mesh object that already\n"
     "has hexes: REFUSED, by the same rule.",
     mesh=None, cmo="hex", expect="error", reason="already has",
     found="original code: accepted: 8 hexes + 24 quads = 32 cells in one\n"
           "mesh object, then 'ERROR IN ROUTine geniee_cmo: ABORTING'.")
case("cp_errors_brick_row_in_y", "errors", "none",
     "createpts / brick / xyz / 1,5,1 / 0.,0.,0. / 0.,2.,0. / 1,1,1",
     "a brick of one row along y (1,5,1). rzbrick3.f makes line cells\n"
     "only along x (ni,1,1); along y it made 4 broken quads (a vertex\n"
     "listed twice) and LaGriT then aborted in geniee. REFUSED.",
     mesh=None, cmo="quad", expect="error", reason="must run in x",
     found="before FIX 2: quads 1 3 2 2 ...; MMGETBLK error, ABORTING.")
case("cp_errors_no_mesh_object", "errors", "none",
     "createpts / xyz / 3,3,3 / 0.,0.,0. / 1.,1.,1. / 1,1,1",
     "createpts before any cmo / create: must stop with an ERROR.",
     mesh=None, cmo=None, expect="error", reason="no current mesh object",
     found="original code: segmentation fault (rz.f writes into arrays that\n"
           "were never allocated). FIXED Oct 13 2026 (FIX 1, createpts_lg.f;\n"
           "poisson_disk exempt): stops with\n"
           "'ERROR: createpts: there is no current mesh object'.\n"
           "Without FIX 1 this deck FAILS (crash).")

# Mesh object names (user, Oct 16): never "cmo" (it means the current
# mesh object); mo_ and a description. Applied to every generated line
# (not the user's own demo decks), token by token, never to the command
# word at the start of a line.
import re as _re
MO_TYPE = {"tet": "mo_tet", "hex": "mo_hex", "quad": "mo_quad",
           "triplane": "mo_tri", "line": "mo_line"}
MO_GLOBAL = {"cmo_pts": "mo_pts", "cmo_hex": "mo_hex", "cmo_tet": "mo_tet",
             "cmo_quad": "mo_quad", "cmot": "mo_tet", "ref": "mo_ref",
             "cmo1": "mo_quad1", "cmo2": "mo_quad2", "cmo3": "mo_quads"}
MO_CASE = {   # names that depend on the case
    "brick_hex_default_mo": {"mo": "mo_default"},
    "xyz_ratio_mirrored": {"mo1": "mo_right", "mo2": "mo_left", "mo": "mo_both"},
    "random_seeds_repeat": {"mo1": "mo_seed1", "mo2": "mo_seed2"},
    "interp_lines_to_quads": {"mo1": "mo_line1", "mo2": "mo_line2",
                              "mo3": "mo_lines"},
    "amr_region_refine": {"cmo": "mo_hex"},
    "amr_doc_demo_connect": {"cmo": "mo_hex"},
}


def mo_names(line, c):
    if line.lstrip().startswith(("*", "#")):
        return line
    base = c["name"].split("_", 1)[1]
    m = dict(MO_GLOBAL)
    t = c["cmo"]
    if t is None:          # the case makes its own: the type of its "mo"
        mt = _re.search(r"cmo\s*/\s*create\s*/\s*mo\s*/\s*/\s*/\s*(\w+)",
                        "\n".join([c["create"], c["tail"] or ""]))
        t = mt.group(1) if mt else "tet"
    m["mo"] = MO_TYPE.get(t, "mo_" + t)
    m.update(MO_CASE.get(base, {}))
    parts = _re.split(r"([/,\s]+)", line)
    first = next((k for k, w in enumerate(parts) if w.strip() and
                  not _re.fullmatch(r"[/,\s]+", w)), None)
    return "".join(m.get(w, w) if k != first else w for k, w in enumerate(parts))


ORDER = ["xyz", "rtz", "rtp", "line", "brick", "interp", "vector", "random",
         "amr", "median", "voronoi", "syntax", "errors"]
cases.sort(key=lambda c: ORDER.index(c["group"]))       # stable within a group
for k, c in enumerate(cases, 1):
    c["name"] = "cp%02d_%s" % (k, c["name"])
for c in cases:
    n = c["name"]; short = n.split("_")[0]
    pts = "out_%s_pts.inp" % short
    msh = "out_%s_mesh.inp" % short
    H = ["#" * 69, "# %s.lgi" % n, "#", "# group    : %s" % c["group"],
         "# expect   : %s" % c["expect"]]
    if c["reason"]: H.append("# reason   : %s" % c["reason"])
    if c["known"]: H.append("# known    : %s" % c["known"])
    if c["warns"]: H.append("# warns    : %s" % c["warns"])
    if c["expect"] == "valid":
        H.append("# spec     : %s" % c["spec"])
        H.append("# points   : %s" % pts)
        m = c["mesh"]
        if m and m.startswith("brick") and c["brick_filter"]:
            H.append("# mesh     : %s %s" % (msh, m.split()[1]))
            H.append("# filter   : yes")
        elif m and m.startswith("brick"):
            H.append("# mesh     : %s %s" % (pts, m.split()[1]))
        elif m:
            H.append("# mesh     : %s %s" % (msh, m))
            if c["filt"]: H.append("# filter   : yes")
        if c["open_"]: H.append("# open     : %s" % c["open_"])
        if c["attr"]: H.append("# attr     : %s" % c["attr"])
        if c["unused_ok"]: H.append("# unused_ok : yes")
        if c["coincident_ok"]: H.append("# coincident_ok : yes")
        if c["parents_ok"]: H.append("# parents_ok : yes")
        if c["shells"]: H.append("# shells   : %s" % c["shells"])
        if c["picture"]: H.append("# picture  : %s" % c["picture"].format(pts=pts, msh=msh))
        if c["volume"]: H.append("# volume   : %s" % c["volume"])
        for k_, v_ in (c["extra"] or []):
            H.append("# %-8s : %s" % (k_, v_.format(pts=pts, msh=msh)))
    H += ["#", "# WHAT:"] + ["#   " + l for l in c["what"].split("\n")]
    if c["doc"]: H += ["# FROM THE DOCUMENTATION: %s" % c["doc"]]
    if c["found"]:
        H += ["#", "# FOUND (sandbox build; FIX 1-3 = createpts_lg.f, rzbrickg.f):"] + \
             ["#   " + l for l in c["found"].split("\n")]
    H.append("#" * 69)
    B = []
    if c["cmo"]:
        B.append("cmo / create / mo / / / %s" % c["cmo"])
    B += c["create"].split("\n")
    m = c["mesh"]
    if c["tail"]:
        # the deck's own steps after createpts ({pts}, {msh}: the files)
        B += c["tail"].format(pts=pts, msh=msh).split("\n")
    elif c["expect"] == "valid":
        B.append("dump / avs / %s / mo" % pts)
        if m and not m.startswith("brick"):
            B += ["cmo / setatt / mo / imt / 1,0,0 / 1"]
            if c["filt"]: B += ["filter / 1,0,0", "rmpoint / compress"]
            B += ["connect", "cmo / setatt / mo / itetclr / 1,0,0 / 1",
                  "resetpts / itp", "quality", "dump / avs / %s / mo" % msh]
        elif m and c["brick_filter"]:
            B += ["filter / 1,0,0", "rmpoint / compress", "resetpts / itp",
                  "quality", "dump / avs / %s / mo" % msh]
        elif m:
            B.append("quality")
    B.append("finish")
    if "sphere_demo" not in n:                  # the user's decks: as given
        B = [mo_names(l, c) for l in B]
    for l in B:
        assert len(l) <= 80, (n, len(l), l)     # LaGriT reads 80 columns
    open(os.path.join(D, n + ".lgi"), "w").write("\n".join(H + B) + "\n")
# the list of decks this generator wrote: the runner ignores (and
# names) any other cp*.lgi, e.g. an old deck left behind when a newer
# archive was unpacked over an older one
with open(os.path.join(D, "DECKS.txt"), "w") as f:
    f.write("# written by make_decks.py: the decks of this suite\n")
    for c in cases:
        f.write(c["name"] + ".lgi\n")
print(len(cases), "decks")
