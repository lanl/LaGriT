---
title: 'LaGriT Release Notes'
---

# LaGriT V1.1 Release Notes Notes 2003 to 2004 


This text is converted from old pdf files and may have translation errors.
See original pdf for clarification.

<a href="/assets/images/release_notes13.pdf" download> LaGriT V1.1.2 2004 </a> PDF  

<a href="/assets/images/release_notes12.pdf" download> LaGriT V1.1.1 2003 </a> PDF 


A summary of the major changes found in this release are listed below. A complete list of
changes is included at the end of this document. Refer to the user‘s manual for a complete
description of the new, enhanced and revised commands.



## Enhanced Commands


read

read/gocad/file_name/mesh_object_name. Read an ascii GoCad TSURF file (2D).

```
read/gocad/file_name/mesh_object_name
```


refine/...amr/iprd 

Use amr refinement with an optional direction.
iprd = 123 refine all hexes. iprd=1 refine along the X direction,
iprd=2, refine along the y direction, iprd=12,13,23 refine along x,y;
x,z; y,z respectively. For example:
```
refine/constant/imt1/linear/element/1,0,0/ -1.,O.,O./inclusive/amr/12 

will refine all hexs in the x and y directions only.
```

geniee/cmoname/2dnormal/reference_element_number [addatt] 

For 2d meshes only, this option will attempt to make
the topological orientation of a triangle, quad or hybrid-triangle-
quad mesh consistent so that shared edges are traversed on
opposite directions. This will not be possible if more that 2
elements share an edge. reference_element_number is the
element that all other elements will be compared to.


dump/elem_adj_node/file_name/cmoname 

Write a list of all elements adjacent to each node to the file fileiname.


massage

new option semiexclusive. Refinement will be triggered only by
edges both of whose endpoints are in the pset. However edges
with only one or no nodes in the pset might be refined as a result of
‘Rivara’ refinement Chain.


dump/zone_imt

output zone list file associted with imt values but do not output outside lists and multi— material connection lists.


createpts/itp

Add nodes to a mesh object by linear interpolation between two point sets



## Code Changes


ung2avs Changed coordinate arrays to double precision arrays. Modiﬁed formats [or larger numbers.

refine_nosb, filter, geniee,refine_get_add Changes related to refine/amr option

delaunay2d get rid of n**2 loop (zeroing out ioff)

ntrp_gtg  increase number of signiﬁcant figures for output.

addmesh_append_nosb Added support of maintaining all attribute types (VINT, VDOUBLE, VCAR) for both
element based vectors and node based vectors


matbld3d_stor Add output of xyz miu/max bounding box of each Voronoi cell. To activate set if_vor_io/2/

mm2000 Added output at the end of mmprint to print out total amount of memory associated With
memory managed arrays. Changed format in mmprint to allow larger integers.


readgmv_ascii changed format so lhe aseii output can exceed lmﬂlion nodes

dumpgmv_hybrid, readgmv_binary recognize amr variables itetpar, itetkid, itetlev as VINT.

recon, mode add option to turn off recon

rzbrick3 dded options, fixed bugs documented functionality in rzinterp sections of the code.
This piece of code is nearly the same as the point interpolation code used in extrude, but
since it required a lot of changes to get it working, 1 decided not to touch the code that extrude uses.


rz Fixed error that occured when createpts/line was a line aligned With the y or z axis. Error
resulted in only cell centered point distributions. Now one can get correct results When ijz=1 and ikz=1.


sortbins allow attributes of arbitrary length notjust nnodes or nclemcnts

cmo_setatt_nosb use a longer format When printing a real acalar attribute

dump_material_list fix format statement

radapt initialie ctrl to zero

fillholes Skip holes whose number is greater than lhe current nelements value

extract_interface do not release cmo if it exists - this lost added attributes

recon2_nosb pass iopt2t02 to try4to4xv so as to be able to skip interface recons if desired

filter In cases where the specified pset was empty the code would crash due to asking for zero
length array. Put in error check to return if pset is empty set.

cel_chain initialize flag to zero



## Bug Fixes


extrude Set, ndimensions_geom of new mesh object.

pset change format to a32 to print entire pset names.

read_sheetij 1ntialize hdrlen to zero so ﬁles With no header are read correctly

connect2d got rid 1n**2 loop on resetting ioff

shttstv_nosb added explicit lengths to character comparisons (ickin, ickout)

cel_chain Fix truncated option

refine_edge_list_lg ityp=itettyp(it) was not being set correctly

addmesh_nosb Fixed bug in VCHAR type element attribute

cmo_copyatt_nosb Allow copy ot'node attribute to element attribute and Vice versa only if have the same length.


readgmv_ascii fix problem With type ‘lin’

refine_tet_add Return gracefully if nadd=0.

eset Bug in exclusive/pset_get_name  Bug resulted inincorrect result except in the case of tet's
or quads. Fixed for all element types now.


matbld1 changed order in Which work arrays are allocated and released so that they only exist
when they are really needed. Changed the way row/column index pointers are sorted. 01d
method used a trick, but the tliek fails when the number of nodes get larger than O(10**7).
Change to use of a multi—key sort of the isortr and isortc arrays. Added some output to log ﬁles.

