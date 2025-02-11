---
title: Tutorial LaGriT Introduction Step 04
---

# Step 4. Write FEHM setup files (In Progress) 

<p>
<a href="step_04/04_tet_nodes_imt_well_pts.png"> <img width="500" src="step_04/04_tet_nodes_imt_well_pts.png" /> </a>
</p>
<br>
<!-- End image -->


#### LaGriT command file: [04_fehm_files.lgi](step_04/04_fehm_files.lgi.txt)
#### LaGriT  output file: [lagrit.out](step_04/04_fehm_files.out.txt)

This example will explain FEHM setup files and how to check and view results.
 
- Use tet mesh with materials
- Use [**`dump/fehm`**](https://lanl.github.io/LaGriT/pages/docs/commands/DUMP2.html#fehm)
- Set well zone
- View mesh and attributes 


# Read tet mesh with materials from Example 3

Check there are no negative volumes
Check node materials are defined as expected

```
read / avs / tet_interp_materials.inp / mo_tet
quality
cmo/printatt/mo_tet/imt minmax
```

FEHM does not use element materials
Set to 1 avoids algorithms using multi-material elements
Remove double-defined nodes and duplicates

```
cmo/select/mo_tet
cmo/setatt/mo_tet itetclr 1

resetpts/parent
rmpoint compress
filter/1,0,0
```

# Write default FEHM files

Write AVS file with attributes created for FEHM files

```
dump/fehm/ tet /mo_tet/ keepatt
dump/avs/tet_fehm.inp/mo_tet
```

# CHECK for neg ccoefs in the interior mesh

```
cmo/addatt/mo_tet/ccoef/VDOUBLE/scalar/nnodes/linear/
cmo select mo_tet
pset pin attribute itp   1,0,0 lt 10
pset pneg attribute ccoef 1,0,0 lt -.0001
pset pBAD inter pin pneg
cmo printatt mo_tet ccoef minmax
cmo printatt mo_tet -all- minmax
```

# Create zone file for vertical well

nodes located with center column at known location

Write FEHM style node list for well zone
Assign a zone number larger than material values

```
cmo select mo_tet
pset/px/attribute xic/1,0,0/ eq 50.
pset/py/attribute yic/1,0,0/ eq 20.
pset/pz/attribute zic/1,0,0/ ge 30.
pset/pwell/ inter / px, py, pz

pset / pwell / zone / well_center.zone / 11
```

# Add mesh object attributes for mesh views

Add elevation attribute for mesh views
save node id to node attributes
Write the final tet mesh with all attributes

```
cmo/addatt/mo_tet iwell/VINT/scalar/nnodes/linear/permanent//0
cmo/setatt/mo_tet/ iwell /pset,get,pwell/ 11
cmo/printatt/mo_tet/ iwell/ minmax

cmo/addatt/mo_tet elev/VDOUBLE/scalar/nnodes/
cmo/copyatt/ mo_tet mo_tet / elev zic

cmo/set_id/mo_tet/node/ id_node

dump/avs/tet_attributes.inp/ mo_tet
cmo/printatt/mo_tet/-all- minmax
cmo/status/mo_tet
```

# Create a mesh object with just nodes

 Remove all nodes not in the well zone
 Write well nodes with all attributes
 Use AVS UCD pnt format for paraview

```
cmo/create/motmp
copypts/motmp/mo_tet
cmo/select/motmp
pset/pduds/attribute iwell/1,0,0/ ne 11
rmpoint/pset,get,pduds
rmpoint/compress

dump/avs/tet_well_pnts.inp/ motmp / 1 3 1 0 0
cmo/printatt/motmp/-all- minmax
```


## finish

Always end a session or a file with the **finish** command.

```
finish
```

