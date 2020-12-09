---
title: 'LaGriT Release Notes'
---

# LaGriT V1.1.0 Release Notes Notes 2002 


This text is converted from old pdf files and may have translation errors.
See original pdf for clarification.


<a href="/assets/images/release_notes11.pdf" download> LaGriT V1.1.0 Nov 2002 </a> PDF  Release YMP QA/QA STN: 10212-1.1-00 

<a href="/assets/images/release_notes10.pdf" download> LaGriT V1.0.4 2002 </a> PDF 

A summary of the major changes found in this release are listed below. A complete list of
changes is included at the end of this document. Refer to the user‘s manual for a complete
description of the new, enhanced and revised commands.


## New Commands


create_graph

Create a node or dual (element) adjacency graph. If node option is selected, the
graph of node adjacency is created, if dual option is selected, the graph of
element adjacency (dual graph) is created —— see manual for details
```
create_graph/metis/node|dual/ v1,v2/ create|delete
```

metis

Interface METIS graph partition and reorder package with LAGriT.
http://www-users.cs.umn.edu/~karypis/metis
For details of METIS algorithms see:
The standard libraries, Iiblagrit.a and Iibutil.a do not contain METIS. In order to
utilize the METIS functions, one must download the METIS package, build the
METIS libraries on your local system and link them with the LAGriT libraries.
```
For example: f90 -o lagrit driver.f liblagrit.a libmetis.a libutil.a
```


stack

stack\fill stack\reorder read tri or quad surfaces combine into one cmo and fill with prisms 0r hexes


mode

mode/discrete/surface_cmo/tolldamage - all refinement smoothing operations on this surface must result in nodes that are members of
surface_cmo

mode/recon/[delaunaylgeometryladaption] choose reconnection mode - delaunay flip to restore delaunay, geometry will flip to
create 'plump' elements, adaption will flip to reduce solution error

mode/adaption_field/field_name/scale_factor/4d_refine_length/4d_merge_length/4d_damage/percent_to_refine/ error_cut_off_refine 
if this mode is on adaptive massage will be in effect


sethessian

sethessian/user - make the 2nd derivative matrix from the routine supplied by the user

sethessian/erradpt make the 2nd derivative matrix from the user supplied edge errors

sethessian/field_name/[algorithm choice] make the 2nd derivative matrix base on the supplied field and algorithm (default is the mega
algorithm)


loop
loops may be max 10 deep, max 250 tokens in the lforeach/ mode.
Implementation involves a define and a dotask issued for each loop command

```
loop/do variable lp_start lp_stop lp_stride loop_end/
loop/foreach variable item1 item2 itemN loop_end/

Examples:
loop foreach MO cmo1 cmo2 cmo3 loop_end &
   cmo / delete / MO

loop do NX 2 31 loopiend &
loop do NY 4 51 loopiend &
loop do NZ 6 7 1 |oop_end &
loop foreach X0 0 5.5 10.2345678 |oop_end &
   createpts/xyz/NX,NY,NZ/XO O. O. / 100. 100. 100.

loop foreach FILE file1 file2 file3 loop_end &
loop foreach CMO cmo1 cmo2 cmo3 loop_end &
   infile lagrit_control_file
```


## Enhanced Commands



refine

refine allow eltset,get,eltsetname wherever pset get psetname was previously allowed.


math

allow more operand types
add math/sum and math/integrate
math/abs is absolute value option

createpts

createpts/median creates new mesh object attributes called xmed,ymed and zmed of length nelements and rank scalar. They
contain the x,y,z coordinates of the median point of each element in the mesh. All element types are supported


sort
reorder  

sort, reorder  now sort and reorder elements in addition to nodes



## Bug Fixes

pset_nosb fix problem with geom/xyz

writedump ﬁx problem with delatt/keepatt and dmllp/zone

read_geometries fix problem if number of surfaces is zero

rotatept fix typo

surfpts fix problems with ‘surface‘ option

cmo_delatt fix error in retrieving cmo name 

mega3d skip calls involving hessian for smooth/position/geometry option

closed_surfaces ﬁx undeﬁned variable

dump_pt_by_value, put mega3d_nosb, getiedge, polyfun_nosb.f corrected the prob1em involving the
nondistinction of differently colored but coincident edges

hextotet_hybrid add error checking, avoid cmo_interpolate bug 

refine_tet_add get correct cmo name

intersect_elements fix incorrect declaration

lower_d_lg ﬁxed test for increasing attribute space

reﬁne_nosb ﬁx errors in faceedge and tetedge options

cmo_addatt_nosb fix type dec1arations

mergepts_simplex use epsilon for inversion tests

rz ﬁx line mode

dump_geometries_lg,geometry_release_lg,read_geometries_lg_nosb allow for mesh object with no geometry

refine_edge_2d reset ipointj at end

control_command_lg reuse space if names are reused in deﬁne commands



## Code Changes

eset_nosb Fixed multiple calls to eltset so that it behaves like pset(i.e. contents ofa named e1tset reset at each call)

cmo_set_mesh_type add error checking

cmo_addatt allow permanent or temporary persistence

cmo_copyatt allow element attributes to be copied to nodes that are element vertices

cmo_set_mesh_type changed subroutine name from cmo_mesh_type

cmo_status added 'brief‘ option

cmoiinlerpolate add error checking

unpackpc simplify the logic

freemove pass in info that determines move

gctiedge distinguish between coincident but different matcria1 edges

intrp_gtg add tiemat option

isosurface set edges per element

sethessian, dampiptihyivalue, eva1uateisobolevnorm,hess3d ,interpo1ate7hessi 5111- use sethessian
celichain, refineispawnilg, reﬁneicouplingcoeLpopconeng, (:51, cer—chain, eel, popcomponenlsilg use
return ﬂag from reﬁneiedgeiadditet to terminate iterations if reﬁne does nothing

flip2to3b_nosb, ﬂip2to3, hmemadjb_nosb remove warning message

connect_nosb spe11ing error

math_sum, math_integrate new options

rotatlenln_nosb, rotatept_nosb rotate only coordinates — skip other attributes

table_element clean -up

distance_to_sheet, testdamage, edgefun_lt, point_to_plane freemove_nosb, mega3d_inner_loop,
mega3d_nosb, massage, reﬁne_edge_add, sgd, cee_chain, polyfun_nosb, adg3d, mode_lg, msgtty_nosb
changes for adaptive massage and for discrete mode


perturb_lg ﬁx calling sequence

recon2_nosb sb ﬁx debug 0utput

quadxyz,quadxy, read_sheetij format changes

extract_surfmesh better error checking

partition,reﬁne_edge_3d, addmesh_amr, voronoi_stor, grid_to_orid, addmesh_overlap, addmesh_merge pull routines out of temp.f

pset better setting of epsilons for geom option




