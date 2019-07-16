---
title: 'LaGriT V1.100 Release Notes 1999'
---

# LaGriT V1.100 Release Notes 1999

This text is converted from old pdf files and may have translation errors.
See original pdf for clarification.

<a href="/assets/images/release_notes6.pdf" download> LaGriT V1.0 May 1999 </a>  PDF Version



A summary of the major Changes found in this release are listed below.


triangulate

triangulate a 2D mesh assuming the ordered nodes in the 2D mesh
define the perimeter of a polygon

ung2avs

convert Arclnfo (GIS) Ungenerate files to AVS
ung2avs/avs_file_out/ung_file_in/[z_va|ue]


deﬁne

allows a number to be associated with a character string, such that
the character string can be used in input decks in place of the
number.

```
define/nx/3
define/ny/4
define/nz/5
define/bottom/O.1/
define/top/4.6
define/left/ -4.7
define/right/9.8
surface/s1/reflect/box/0.0,left.bottom/1.0,right,top
```

colormap

This command builds the colormap. In reality it only builds the
material adjacency graph, from which the colormap can be quickly
generated when needed. Three actions are possible:
```
colormap/[add|create|delete]/[cmo_name]
```

- add - The material adjacency characteristics of the specified mesh
object is added to the existing material adjacency graph, which is
created if it didn‘t exist. This is the default action.

- create - The existing material adjacency graph is deleted and a
new one created from the specified mesh object.

- delete - The material adjacency graph is deleted if it exists. Any
specified mesh object is ignored.

Examples:
```
colormap/create/mesh1
colormapl/mesh2
colormap/delete
```

massage

added a smoothing operation to the optimization which can be
turned off with the nosmooth option
```
massage/creation/annihilation/toldamage//[ifirst,ilast,istride]/[nosmooth]
```


smooth

new option **aspect** will smooth to improve aspect ratio by moving a
node toward the neighbor that provides the greatest improvement.
New option lpfilter will smooth surface networks (i.e 2D mesh
objects or the interface network of a 3D mesh) using a polynomial
filter. (filtdeg default 30; k_pb default 0.1)
```
smooth/position/aspectl[ifirst,ilast,istride/toldamage]
smooth/position//pfilter/[ifirst,ilast,istride/filtdeg/k_pb]
```


pset


new option **surface** will identify nodes on the specified surface.

Keyword surface names have the following meaning:
```
-all- will identify nodes on any surface.
-interface- will identify nodes on any interface surface.
-boundary- will identify nodes on exterior surfaces.

pset/psetname/surface/surface_name/[ifirst,ilast,istride]
```


refine

new option **roughness** will refine based on the distance of the
endpoint of an edge to the plane determined by the synthetic
normal with respect to a specified surface at the other endpoint of
the edge.

```
refine/roughness///edge/ifirst,ilast,istride/
distancelsurface_namelexclusivelinclusive
refinelroughnessllledge/1,0,0l.28lptoplinclusive
```

new option **edge_list** will bisect a set of edges specified by the
node numbers of the endpoints of the edges.
```
refine/edge_list///edge/edge_listl

refine/edgeilist///edge/1 2 23 47/ will refine the edge with
endpoints 1 and 2 also the edge with endpoints 23 and 47.
```

new option **interface** will bisect a set of non-interface edges of tets
all of whose vertices are interface nodes.
```
refine/interface///edge/pset,get,psetname//// [inclusivelexclusive]l
```

extract

new option **network** will extract the network of interfaces (consisting of parent nodes) from a mesh.
```
extractlnetworklifirst,ilast,istride/cmoout/cmoin
```

dump

dump/recolor/file_name
This command writes the existing colormap to the specified file.  (See colormap command.)

```
dump/fehm/file_name / [cmo_name] / [binary/ ascii | asciic | binaryc] /[scalar, vector, both] / [delatt, keepatt]
```

The [delatt, keepatt] option gives the user the ability to delete or
keep the boundary attributes, top, bottom, left_w, right_e, back_n,
front_s, which are created by dump/fehm. The default is delatt.
dump/fehm/file_name / [cmo_name] / [binaryc | asciic] produces
compressed matrices

```
dump/gmv/file_name/[cmo_name]/[binary, ascii] 
```
specify binary or ascii format of GMV file on command line

```
dump/lagrit/file,name/[cmo_name]/ 
```
will write an ascii restart file
that contains geometry and mesh object information. cmo_name
can be ‘-all-‘ in which case all mesh objects are written to the file or
it can specify a list of mesh objects to be written.


read

```
read/lagrit/file_name/[cmo_name]/ 
```
will read an ascii restart file written by dump/lagrit.
All mesh object data is preserved in the file including the cmo_name.


connect

connect will triangulate a 2d planar set of nodes generating a triangular Delaunay grid.


## Bug fixes Jan 98 to May 99

multi_material -  ﬁxed error for node added that was on both an interface and an exterior boundary might
get the wrong itp1 value.

connect -  refresh pointers alter call to remove bigtet

ceL_chain - fix bug with memory allocation for mpary array.

massage,getmpary - correctly access pset for massage

try2tob -  get pointer to icontab correctly

cel_chain - Check for psetnames = blank

gctbit,sctbit Change declaration of ISHFT to intrinsic

ﬂip2t03,ﬂp2t03b, ﬂp2to3i - update itettyp for new element

recon2d - use cmo.h (icmoget) to pass to testdamage so it knows If it must refresh pointers

dumpavs - close file always before leaving subroutine

reﬁne_edge_list_lg - correct pointer statement

tangent_plane, cer_chain - ﬁx reﬁne on roughness

reﬁne_ﬁx_add - correctly set ier values for added nodes on constrained interfaces

sheet - explicitly specify -def— for mesh object name

rzbrick - ﬁx ratio ﬁag

control_command_lg - correctly remove unnecessary blanks from command lines

cmo_create -  make interpolation type be ‘and’ for isetwd and xtetwd

cmo_interpolate - ﬁx interpolation for isetwd and xtetwd

pset - idebug delared as integer

rmmat - ﬁx error return ﬂag

resetpts - ﬁx error return ﬂag

surfset - ﬁx memory management error

gctmpary - sct defaults correctly by testing nwds

closed_surfaces - ﬁx arguments to getregv2 call

reﬁne_edge_add - modify pset membership for new nodes.

cmo_select, cmo_get_name - remove null character from end of name

recon2d - set itetoff

lpﬁlter,LowPassFilterModule - avoid overwiting data
 


## Code Improvements Jan 98 to Mar 99


smooth - new option smooth/position/aspect will smooth to improve aspect ratios.

smooth, extract - new option smooth/position/lpfilter will smooth surface networks.
New extract option extract/network will an interface network from a 3D mesh.

pset - New options for surface (surface names: -all-, -interface-, -boundary- have the obvious special meanings)

delaunay - Insert nodes in mesh in random order. Replace n**2 a1gorithm to ﬁnd matching faces with a linked list approach

reeon2d - changed test to use consistent volume calculation.

reﬁne, tangent_plane, cer_chain, reﬁne_edge_list_lg, lpﬁlter, LowPassFilterModule, GmphModule - new command options

triangulate_lg, msgtty - add triangulate command

pntlimc - check for pset named ‘-def—‘ or empty string

corrected warning that showed up on the DEC compile in the following routines:

addmesh, addmesh delete, addlnesh pyramid, boundary components, chkreg, chkregv, closed surfaces,
cmo_delatt_def, cmo_interpolate, cmo_setatt, cmo_release, connect, correctpc, dereﬁne, dopmat,
dumpchad, ﬁlholes, geniee, get_mregions, get_regions, get_surfaces, getreg, getregv, grid_to_grid,
hextotet_att, l1n1en1adjb, hsb2seta, ifacept, initx3d, math, occonv, pstatus, readgmv_binary,
reﬁne_coupling_coef, reﬁne_edge_add, reﬁne_face, reﬁne_face_add, rmregmn, rmsurf, rwdpmw,
r2, search2d, sortbins, taylor_error, translate, volume_tet, voron2d, writedump, refine_edge_list_lg


recon2, mega_error - restrict existence of ‘mega’ related attributes to recon loop. change IO disposition to not write to GMV ﬁles

refine, refine_interface_elements_lg - new reﬁne option to reﬁne non—intcrface edges of tets, all of whose vertices are interface nodes.

dump/fehm, writedump, matbld3d_stor -  generate compressed matrix for geometric coefficients .stor file

cel_chain, cer_chain, refine_edge_add -  set pset membership of child nodes in reﬁne_edge_add_tet

reﬁne_edge_add - pset is inherited from ‘anding’ the pset of the endpoints of reﬁned edgeh



## Code Changes Nov 98 to Apr 99


agd3d massage - add smoothing operation to optimization loop in massage.

sgd, primestep - smoothing now automatic in massage, turn it off with ‘nosmooth’ 

cel_chain - remove call to recon from inside reﬁne/rivara loop.

agd3d - allow more merges of nodes that do not have unique successors and predecessors

dumpavs -  allow for ranksﬂ and limit coordinate range to (—1 16—30, 11e+30)

aratio_tet - handle extreme aspect ratio tets correctly

agd3d, aratio_tet, aratio_tri - remove assumption that fp errors would not be trapped 

massage -  set ipointi to 1 and ipointj to nnodes

intradd - use a more memory efficient al gorithm to create child nodes

agd3d - change error to warning when material match in question (skip merge)

all common blocks -  moved common statements after declarations added ‘save’ statement

dump_recolor_lg, neighbor_recolor_lg, writedump - add dump/recolor command (see above)

dump - fehm option to keep/delete boundary attributes on fehm ﬁles dump_outside_list

ung2avs - option to convert Ungenerate ﬁles to AVS ﬁles

dumpgmv_hybrid - read binary/ascii from command line

llip3t02, llip4to4, llip2t00, llip3t021 [lip4to4i llip2t00b In[lip recon IceonZ
ﬁiplt00, ﬂip2t02, - remove calls to ﬂuxing routines and clean up associated memory usage

control_eommand_lg  - new method of command processing

writloga, writinit, dotask, dotaskx3d, initlagrit, msgtty, control lg.h, lagrith
dumpgmvihybrid cmo attribute 7def- is modified so that it Will not be written to gmv files.
writcdump,rcaddump dump/lagrit and rcad/lagrit - now write and read ascii geometry files
dumpilagrit, eventually this command will also dump the mesh objects dump_lagrit_geom,

read_lagrit - read_lagrit_geom

cmo_dump_cmo dump/lagrit and read/lagrit - now write and read ascii re start ﬁles

cmo_read_dump_cmo - that contain geometry and mesh object intbnnation

matbld2dstor - add max connections to output,  make consistent with matbld3d_stor

eset - don’t print element number of member of set

quality - print if idebug set to 1

connect2d_lg -  new code to connect 2d planar node distributions into 2d grids

delaunay2d_lg, delaunay2d_connect_lg, multi_material2d_lg, fix_small_triangles_lg, make_big_triangle_lg
scale_lg, msgtty change subroutine name ‘scale’ to ‘scale_lg' to avoid conﬂicts with other libraries

