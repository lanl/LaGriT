---
Title: Demo Write Face and Node Sets
Tags: extract surfmesh facesets boundary interface
---

# Demo Write Face and Node Sets

------------------

This example shows how extract/surfmesh can be used to write boundary face information that can be used by ExodusII files to define facesets (side sets). 
For syntax and description see command [**extract/surfmesh**](../commands/dump/EXTRACT_SURFMESH.md). 
The **extract/surfmesh** command will create a surface mesh object extracted from the boundary and or interfaces of the mesh.
The commands add some useful attributes that relate the surface faces to the source elements.

Write the mesh element number **idelem1** and element face number **idface1** to define a boundary or surface in order to set model conditions.

Write the node sets as an FEHM zone file or as an AVS format file.

*Note it is very important that you use the model source mesh to get the intended node and element numbers. If you subset or change the source mesh, the node and element numbers might change so your faceset list will not be valid.*

```
# LaGriT Example for face sets using extract/surfmesh
# Write face to element correlation based on surface
#
# Also write node zones associated with these sets
# Note surfmesh nodes do not remember master node numbers
# Use set_id to carry these to surfmesh

# ----------------------------------------------------
# CREATE master mesh used by the model 
# These are the final node and element numbers
# First create a hex mesh and color by 2 regions
# Convert to tets so we have triangle faces 

define MO_MESH mo_hex
define/MIN/  0.0
define/MAX/  10.0
define/ZMAX/   6.0
define/NXY/ 11
define/NZ/  7
cmo / create / MO_MESH / / / hex
createpts/brick/xyz/NXY NXY NZ/ MIN MIN MIN/ MAX MAX ZMAX/ 1 1 1
surface/lay1/intrface/plane/ 0. 0. 4./ 1. 1. 4./ 0. 1. 4.
region/ r_mat1/ lt lay1
region/ r_mat2/ ge lay1
mregion/ mat1 / lt lay1
mregion/ mat2 / gt lay1
setpts

eltset/ e_mat2/ region / r_mat2
cmo/setatt/mo_hex/ itetclr 1
cmo/setatt/mo_hex/ itetclr/eltset,get,e_mat2/ 2
resetpts/itp
dump/ hex.gmv/ mo_hex
cmo/printatt/ mo_hex / itetclr minmax

# convert hex mesh to tet mesh
grid2grid / hextotet5 / mo_master / mo_hex

# remove duplicates and set boundary tags
cmo/select/ mo_master
rmpoint / compress
resetpts / itp

# create attribute to save node id numbers
cmo/set_id/ mo_master/ node / id_node
cmo/printatt/ mo_master / id_node minmax

cmo/delete/mo_hex
dump/ tet.gmv / mo_master
cmo/printatt/ mo_master / -all- minmax

# ----------------------------------------------------
# EXTRACT ALL BOUNDARIES AND INTERFACES
# The surface mesh object will have attributes we need

extract/surfmesh/ 1,0,0 / mo_surf / mo_master 
cmo/printatt/ mo_surf / -all- minmax

# color itetclr according to the two facesets to write
# 1 = internal interface
# 2 = top

cmo/select/mo_surf
cmo/setatt/mo_surf/itetclr/ 3

# select faces on interface below material 2
eltset/ e_set1/ itetclr0 eq 2
cmo/setatt/ mo_surf / itetclr/eltset,get,e_set1 1

# select faces at top of mesh 
pset/ptop/ attribute zic/1,0,0/ ge ZMAX
eltset/ etop / exclusive / pset,get,ptop
cmo/setatt/ mo_surf / itetclr/ eltset,gt,etop 2

# check faces for correct selection
cmo/printatt/ mo_surf / itetclr minmax
dump/ surfmesh.gmv / mo_surf

# ----------------------------------------------------
# WRITE node zone files the easy way
# write zonn and vertexsets for selected psets

# define the psets
cmo/select/mo_master
pset/TOP_NODES/attribute zic/1,0,0/ ge ZMAX
pset/play_in/attribute itp/1,0,0/ eq 2 
pset/play_bnd/attribute itp/1,0,0/ eq 12 
pset/INTER_NODES/ union play_in play_bnd

# write the psets
pset/TOP_NODES/ zonn/ top_nodes.zonn / ascii
pset/TOP_NODES/ write/ top_nodes.vertexset/ ascii
pset/INTER_NODES/ zonn/ inter_nodes.zonn / ascii
pset/INTER_NODES/ write/ inter.vertexset/ ascii

# ----------------------------------------------------
# SUBSET and WRITE faceset files
# prepare surfmesh to write only idelem1 and idface1 pairs
# these will be the master mesh element to element face number 
# copy surfmesh so we can subset multiple times 
# use AVS format for writing element-face attributes 
# use AVS format for writing node-id_node attributes 

cmo / DELATT / mo_surf / itetclr0
cmo / DELATT / mo_surf / idnode0
cmo / DELATT / mo_surf / idelem0
cmo / DELATT / mo_surf / facecol
cmo / DELATT / mo_surf / itetclr1
cmo / DELATT / mo_surf / idface0

# instead of delete we can limit the ioflag 
cmo / modatt / mo_surf / imt / ioflag/ l
cmo / modatt / mo_surf / itp / ioflag/ l
cmo / modatt / mo_surf / isn / ioflag/ l
cmo / modatt / mo_surf / icr / ioflag/ l

# TOP NODES AND FACES
define / FILENAME / top.faceset
define / SS_ID / 1
cmo / copy / mo_tmp / mo_surf
cmo / select / mo_tmp
eltset / e_keep / itetclr / eq / SS_ID
eltset / e_delete / not / e_keep
rmpoint / element / eltset get e_delete
rmpoint / compress
  dump / avs / top_nodes.dat / mo_tmp / 0 0 1 0
  cmo / DELATT / mo_tmp / id_node
  dump / avs / FILENAME / mo_tmp / 0 0 1 2
  cmo / delete / mo_tmp

# INTERFACE NODES AND FACES
define / FILENAME / interface.faceset
define / SS_ID / 2
cmo / copy / mo_tmp / mo_surf
cmo / select / mo_tmp
eltset / e_keep / itetclr / eq / SS_ID
eltset / e_delete / not / e_keep
rmpoint / element / eltset get e_delete
rmpoint / compress
  dump / avs / interface_nodes.dat / mo_tmp / 0 0 1 0
  cmo / DELATT / mo_tmp / id_node
  dump / avs / FILENAME / mo_tmp / 0 0 0 2
  cmo / delete / mo_tmp

# ----------------------------------------------------
# WRITE MASTER MESH with FACES for viewing
# ExodusII and GMV files can have face sets defined

dump/exo/ mesh_facesets.exo / mo_master / / / facesets &
             interface.faceset top.faceset 

dump/gmv/ mesh_facesets.gmv / mo_master

finish
```
