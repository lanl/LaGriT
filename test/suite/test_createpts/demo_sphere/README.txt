The LANL sphere demo (Carl Gable), meshing.lanl.gov/proj/
DEMO_sphere_mesh_generation, as run by its lagrit.input_driver:

  input lagrit.input_geom          the shells r1 = 1, r2 = 2, r3 = 3
  input lagrit.input_icos_162      hex, createpts/sphere type 8, 162
  input lagrit.input_icos_642      hex, createpts/sphere type 8, 642
  input lagrit.input_cube          tet, createpts/sphere type 1 (cube)
  input lagrit.input_icos_642_tet  tet, createpts/sphere type 2
  input lagrit.input_rtp           tet, createpts/rtp shells
  input lagrit.input_sphere_in_box hex sphere in a hex box, octant
  input lagrit.input_merge         (missing from the web page)

These .body files are the decks as given (Oct 15 2026), without their
"finish". make_decks.py joins them, in the driver's order, into deck
cp13_rtp_sphere_demo (see its header), changing only the
dumps: each deck's final GMV dump becomes an AVS dump the checker
reads (out_cp13_<deck>.inp), other GMV dumps are left out, and
the files sphere_in_box writes and reads back get out_cp13_ names.
The six-panel picture (lagrit.input_merge's job) is drawn by the
checker from the six AVS files.
