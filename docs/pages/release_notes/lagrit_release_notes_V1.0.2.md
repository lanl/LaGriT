---
title: 'LaGriT Release Notes'
---

# LaGriT V1.0.1 - V1.0.2 Release Notes 1999 - 2000


This text is converted from old pdf files and may have translation errors.
See original pdf for clarification.


<a href="/assets/images/release_notes8.pdf" download> LaGriT V1.0.2 January 2000 </a> PDF Version   


<a href="/assets/images/release_notes7.pdf" download> LaGriT V1.0.1 November 1999 </a> PDF Version


## New Commands

intersect_element 

intersect two mesh objects and create an element based attribute
(xsect_cm) that holds the number of times the sink element was
intersected by a source element
```
intersect_elements/sink_mo/source_mo/[attribute_name]
```

lower_d 

extract lower dimensional meshes from the current mesh object,
create data structure for the lower dimension objects that parallel
the top d default structure: [d1_nnodes, d1_e|ements, d1_itet,
d1_jtet, di_itetoff, d1_jtetoff, d1_itetc|r, d1_itettyp] [d2_nnodes, ..... ]


rzv

distribute nodes as linear combinations of 3 supplied vectors.
rzvlgeom/n1,n2,n3/v11,v12,v13/v21,v22,v23/v31,v32,v33
geom can be xyz, rtp or rtz. n1,n2,n3 specify the max number
applied to each vector. The v‘s are the 3 basis vectors.


rzamr

use octree refinement to a specified level on a existing hex mesh
to distribute nodes within a region- intended a a node distribution
algorithm only - no hex information is preserved.
```
rzamr/region_name/number_of_levels
```


createpts

wrapper for all the ‘rz‘ type commands:
```
createpts/xyz|rtz|rtp|/n1,n2,n3/x1,y1,z1/x2,y2,22/[ix,iy,iz/]/ [irx,iry,irz/rx,ry,rz]

createpts/sphere/1|2|8|diamond /nr,npt,xirad,xorad /xcen,ycen,zcen/iz/irat,rz/

createpts/brick/xyz|rtz|rtplni,nj,nk/xmin,ymin,zmin/ xmax,ymax,zmax/iiz,ijz,ikz/[iirat,ijrat,ikrat/xrz,yrz,zrz/isym,jsym,ksym

createpts/brick/xyz|rtz|rtp/ni,nj,nk/pstatus,get,name/connect/

createpts/amr/region_name/number_of_levels

createpts/random/ xyz|rtz|rtp /spacing/x1,y1,z1/x2,y2,z2/ [xcen.ycen,zcen/ edgedist/ ranseed1,ranseed2]

createpts/vector/ xyz|rtz|rtp/n1,n2,n3 /V11,V12,V13/V21,V22,V23/V31,V32,V33
```


## Command Enhancements


read/gmvfreeformat

read in an ascii gmv file that is to be read with read (unit,*) statements.


boundary

add option to establish or to reset icr, icontab relationship for a set
of surfaces. This is normally done by the setpts command, but if
surfaces are defined after setpts, the surface command will cause
an entry in the icontab table for the nodes on the surface, but no
entry will exist for intersections of surfaces.
```
boundary/dirichlet/icr//surface list

e.g. boundary/diriclet/icr//top left front will identify the nodes
on the intersection of the surfaces top left and front, and will set
the icr value of these nodes to point into the correct icontab entry.
```


quality

add pcc option which will identify negative coupling coefficients
and create an element based attribute to store the value (1 means
okay, anything less than 1 is a negative coupling) 
```
quality/pcc
```


boundary_components

boundary_components Now prints out number of connected boundary components and a representative vertex from
each component.


rmpoint/sparse

For expert users only. Requires reconnection when done.



## Bug fixes

06/10/99 pntlimc ﬁx missing argument in call to writloga - return empty set if error

06/13/99 refine_face_add, cer_chain,cel_chain  ﬁx incorrect icr and itp values assigned using reﬁne/addpts to triangular 2d mesh

16/16/99 copypts remove extra comma from write statement

06/1 7/ 99 readavs read pyramid data correctly

06/23/99 control_connnaud_lg ﬁx errors in nesting of inﬁles and in invoking infile from dotask

16/25/99 get_elements_on_edge update len,len1 1f need to increment memory

17/02/99 readgmv_ascii check for end of ﬁle -- skip blank lines

07/07/99 regupts this is really a compiler bug - change indirect indexing so optimizer does not generate bad code

17/13/99 control_command_lg skip comments When entered as interactive or as dotasks 

07/23/99 refine_face_add make arguments in call match those in subroutine statement

07/23/99 readavs declare pointers — use pointer_arrays.h for integer*8/integer*4 distinction

08/ 04/99 initlagrit initialize pi

18/05/99 cmo_readdump_cmo allow for zero length attributes

08/05/99 read_lagritgeom ﬁx memory problem

08/10/99 multiimaterial calculate length of matlst correctly

18/11/99 reﬁne_interface_elements_lg use nnodes for length of iseedtet

08/11/99 regnpts use max not min for length of name comparisons

08/1 8/99 readdump use ierror_return

08/27/99 rmpoint get correct values in isetwd

18/30/99 multi_material2d_lg refresh pointers

08/3 0/99 multi_magterial refresh pointers

08/31/99 control_command_lg check for an empty loop 

18/31/99 matbld3d_stor, anothermatbld3d_wrapper unformatted IO bugs

18/31/99 writedump restore dump_recolor command

09/01/99 connect2d_lg remove references to if4

09/01/99 initlagrit remove test on uninitialized variable

19/(11/99 read_lagrit_geom use lengths with concatenation operator

09/01/99 refine_face_add initialize isnl for all new nodes

09/01/99 surfset change dotaskgen to dotask

09/01/99 make bigtri get mbndry from mesh object

19/02/99 pset ﬁx test on mregion names

09/03/99 surface restore tabular surface implementation

09/07/99 recon2 allow for nodes on external boundaries with no icr values

19/17/99 cer_chain ﬁx typo (mpary should have been mpary1) effecting pset type refinements

10/03/99 intradd use nefcmo which is required for hybrid grids

10/14/99 region, mregion Check for size of stbout array

10/19/99 rzbrick3 check value of nwds before setting isym, jsym and ksym.

12/13/99 rzbrick3 check nwds before setting end point and ratio ﬂags


## Code Improvements

boundary  new option (see above)

ﬁlter cleaned—up and added comments

pset use Information from call

get_materials_on_edge_lg returns materials around edge

cmo_dump_cmo skip cmo attributes with 'L’ in ioﬂag ﬁeld when writing lagrit dump ﬁles.

matbld0tri, matbld3d assigned idebug, reduced screen output, removed print *

read_trilayers postproccss by column option, preserve buffers, create buffers at interfaces, read gmv or avs

temptam  new subroutine beads_ona_ring to distribute vertically for read_trilayers

rand_lg new random number routine (fancy)

ran2_lg renamed random number routine (simple)

temp,edit clean up unused code

neighbor_recolor improved algorithm

cmo_interpolate  accept 'constant' as interpolation type - implement linear,log, asinh for integer attributes

recon2 test for existence of icontab

readgmv_binary allow line type elements to be read

connect release imt1 when no longer needed

cmo_modatt_cmo look for special name icr,imt,itp, isn and change icr1,imt1,itp1,isn1 

zq get rid unused options

rz change i6 formats to i10

cel_chain check for infinite isnl chain loops

dumpavs check if no nodes, elements then return without crashing

delaunay replace expensive inner loop with more efﬁcient code

connect settets, check if mbndry is big enough

checkmbndry

inv3x3, inv2x2, inv_schmidt_hilbert_lg.f better solution to MX=Y for 3X3 and 2X3 cases

volume_qud.f contains 2 algorithms for calculating volume of a quad

volume_element.f call volume_qud(elcment) for old method using centroid, call volume_qud(element)_alt_lg for method equalizing 4 contributing triangle areas

12/03/99 lower_d_lg, cmo_copyatt_mpno_lg copy all user attributes from one mesh object to another — used by lower_d routines

12/13/99 pset a11ow either compare/value or va1ue/compare in syntax of command.

12/14/99 readdump, readgmv_ascii new option read/gmvfreeformat uses read(unit,*) statements.



## Code Changes


reﬁne reﬁne..edge for 2d now works like reﬁne..face (in 2d edges are facets and nodes are edges - but people don't think that way hence the change) 

rz initiahze itp,imt,icr  to zero for new nodes

blockcom, local_element.h set up relationships for edges to faces for all element types

readgmv ascii call geniee at end —— skip blank lines, test for eof'
readgmvibinary call geniee at end

popcones_lg,massage issue rmpoint/compress from within loop (needed for big problems)

lineline, xsectelementscmo, calc_rdist, lineseglineseg, xsectelm, insideielement, linesegtri, kdtreeselect, tritri new subroutines

dump_material_list replace * formats to speed up code

hpsortip, hpsorti, matbldl replace all calls to ssort with calls to appropriate hpsort routine

agd3d don’t print ‘curve neighbor' warning to standard out.

popcomponents new topo change routine

rmppoint zero out imt,itp,isn,icr for removed nodes in rmpoint/compress option

cmo_interpolate handle ‘and‘ and isetwd as special case

delaunay, refine_edge_add, ﬁndface, matbld0tri, matbld2d_stor replace calls to ssort with hpsort

agd, sgd use inscribed radius not aspect ratio

mergepts_simplex use epsilonv not epsilonl for volume tests

 
