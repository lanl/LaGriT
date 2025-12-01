---
title: "Example 1: dump files in various formats"
---

## Example: stl format

The **stl** format is often used by applications using surfaces representing boundaries and interfaces of 3D geometries. These include ShaPo, VoroCrust, for voronoi meshing and HOSS for simulations. The surfaces must be air-tight and of good quality.

The surfaces sometimes have issues that can be detected and fixed using LaGriT commands. Here is an example input file used after surfaces have been extraced from a 3D mesh wih materials. See Demo at [extract/surfmesh](https://lanl.github.io/LaGriT/pages/docs/commands/dump/EXTRACT_SURFMESH.html)

<pre>
# Check Quality of input_surface.inp for VoroCrust and ShaPo
# check for watertight surface and jtet consistency
# orientation implied by node order should be counter-clockwise
# Note this is viewed from outside looking at elements
# The .obj file format expects counter-clockwise ordering
# so the AVS tri node order should be same as obj f order

# add normal and orientation attributes, no change

define INFILE cube.inp
define CKAVS cube_tri_att.inp
define CKGMV cube_tri_att.gmv
define FIXSTL cube_tri.fix.stl
define FIXAVS cube_tri.fix.inp
define FIXGMV cube_tri.fix.gmv

# Remove extra attributes created by the extract/surfmesh command
# nothing will happen if the attributes do not exist
read INFILE mos
cmo DELATT mos itetclr0
cmo DELATT mos itetclr1
cmo DELATT mos idnode0
cmo DELATT mos idelem0
cmo DELATT mos idelem1
cmo DELATT mos facecol
cmo DELATT mos idface0
cmo DELATT mos idface1
cmo DELATT mos iign
cmo/printatt/mos/ -all- minmax
cmo/copy/ mos2 / mos

cmo/select/mos

# CHECK for 0 duplicate points
filter 1,0,0
  rmpoint compress
  resetpts itp

# CHECK all areas are positive
quality

# CHECK for 0 boundary shell, 1 boundary open surface
#           0  different exterior boundary components
boundary_components

# CHECK face orientations are all in outward directions
# visual inspection of materials should result with
# 1=down facing 2=up facing, 3=right, 4=back, 5=left, 6=front
settets/normal

# CHECK topology, should have 0 inconsistent elements
# Note this checks that all are inward or outward in same direction
# Note that internal boundaries will result in 3 elems sharing an edge
# For instance
#  geniee: mesh has 32  jtet loops, max cycle length=  3
#  means there are  32 edges with more than 2 faces sharing an edge
#  and there is no single solution for checking or fixing this
#
#  Reference Element                   1
#  Inconsistent Elements               0
# ---NO ACTION, Only Check---
# check direction of normal so it is up
# geniee can be used to change face directions
# geniee/mo_top/2dnormal/-1 = reverse
# geniee/mo_top/2dnormal/1 = fix wrong faces
# geniee/mo_top/2dnormal/0/addatt = report
#
geniee mos 2dnormal 0 addatt
cmo/printatt/mos/ ifflip minmax
  eltset/el_nochange/ ifflip eq 0
  eltset/el_flip/ not el_nochange

dump gmv CKGMV mos
dump avs CKAVS mos


##################################################################
# IF NEEDED fix surface
cmo/delete/mos
cmo/select/mos2

# flip or make same as  element 1 to correct counter-clockwise node order
# geniee mos2 2dnormal 1
geniee mos2 2dnormal -1

settets/normal

dump gmv FIXGMV mos2

cmo/setatt/mos2 itetclr 1
dump stl FIXSTL mos2
dump avs FIXAVS mos2 1 1 0 0 0
cmo printatt mos2 -all- minmax

finish

</pre>



## Example: dump files in various common formats

  The objective is to **dump** a simple geometric object into avs,
  fehm, LaGriT, and gmv formats.
  The output consists of one gmv file, one avs file, one LaGriT file,
  and seven fehm related files.

 Input

  [lagrit_input_dump](input/lagrit_input_dump.txt)

Images of GMV input (the output consists of several file formats
 including binary)

<img  width="300" src="https://lanl.github.io/LaGriT/assets/images/output_tn.gif">
