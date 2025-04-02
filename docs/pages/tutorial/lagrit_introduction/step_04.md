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

LaGriT's meshing tools are tailored for geologic applications and Voronoi control volume solvers.  Though LaGriT can write files that are of general use, some are specifically designed for the FEHM porous flow and transport code. This example writes model setup files used by FEHM but can be modified for use in other simulators using Voronoi control volumes (FEHM, PFLOTRAN, TOUGH2). 

- Use the tetrahedral mesh with materials created in Step 3.
- Use [**`dump/fehm`**](https://lanl.github.io/LaGriT/pages/docs/commands/dump/DUMP3.html) to write 7 model setup files.
    .fehm - mesh coordinates and geometry in FEHM grid format. Same as [**`dump/coord`**](https://lanl.github.io/LaGriT/pages/docs/commands/DUMP2.html#coord)
    _material.zone - node zone lists for each material. Same as [**`dump/zone_imt`**](https://lanl.github.io/LaGriT/pages/docs/commands/DUMP2.html#zone)
    _outside.zone - node external boundary zone lists. Same as [**`dump/zone_outside`**](https://lanl.github.io/LaGriT/pages/docs/commands/DUMP2.html#zone)
    _outside_vor.area - node external boundary areas
    _interface.zone - zone lists for nodes along material interfaces
    _multi_mat.zone - lists of node pairs connected across material interfaces
    .stor - file with voronoi control volumes associated with each node and the sparce matrix structure. Same as **`dump/stor`**](https://lanl.github.io/LaGriT/pages/docs/commands/DUMP2.html#stor) and described at (https://lanl.github.io/LaGriT/pages/docs/STOR_Form.html)
- Create a vertical well zone
- View and check the mesh quality and defined zones.



## Read tet mesh with materials from Example 3

Assume this is a valid mesh with material IDs assigned to the mesh nodes.
For this example we read the mesh created in Step 3 with assigned materials.

Check there are no negative volumes
Check node materials are defined as expected

```
read / avs / tet_interp_materials.inp / mo_tet
quality
cmo/printatt/mo_tet/imt minmax
```

<pre class="lg-output">
---------------------------------------
All elements have volume  8.3333333E+01
---------------------------------------
      4800 total elements evaluated.

ATTRIBUTE NAME         MIN               MAX         DIFFERENCE    LENGTH
 imt1                        1                4               3      1122
</pre>

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

## Write default FEHM files

Write AVS file with attributes created for FEHM files

```
dump/fehm/ tet /mo_tet/ keepatt
dump/avs/tet_fehm.inp/mo_tet
```

The output from the `dump/fehm` command generates output that is useful for reports and descpriptions of this mesh that can be useful for modelers.

<pre class="lg-output">
*********dump_material_lists********
Minimum material ID value =      1
Maximum material ID value =      4
Total possible materials  =      4
Material      1 has       468 nodes. #nodes/nnodes is   0.417112290859
Material      2 has       270 nodes. #nodes/nnodes is   0.240641713142
Material      3 has       204 nodes. #nodes/nnodes is   0.181818187237
Material      4 has       180 nodes. #nodes/nnodes is   0.160427808762

AMatbld3d_stor: *****Zero Negative Coefficients ******
AMatbld3d_stor: Number of 'zero' (< compress_eps) coefs  0
AMatbld3d_stor: npoints =     1122  ncoefs =       7144
AMatbld3d_stor: Number of unique coefs =              6
AMatbld3d_stor: Maximum num. connections to a node =  7
AMatbld3d_stor: Volume min =   6.2500000E+01
AMatbld3d_stor: Volume max =   5.0000000E+02
</pre>

## CHECK for neg ccoefs in the interior mesh

```
cmo/addatt/mo_tet/ccoef/VDOUBLE/scalar/nnodes/linear/
cmo select mo_tet
pset pin attribute itp   1,0,0 lt 10
pset pneg attribute ccoef 1,0,0 lt -.0001
pset pBAD inter pin pneg
cmo printatt mo_tet ccoef minmax
cmo printatt mo_tet -all- minmax
```

## Create zone file for vertical well

nodes located with center column at known location

Write FEHM style node list for well zone
Assign a zone number larger than material values

Create zone file for vertical well
Use region defined by box surface
vertical column at 50x 20y
Write FEHM style node list for well zone
Assign a zone number larger than material values

```
cmo select mo_tet
surface / s_box / reflect / box / 49.5 19.5 -1. / 50.5 20.5 100.
region/ r_box / le s_box
pset/pwell/ region / r_box

# check extents of the well nodes
cmo/printatt/mo_tet/-xyz/ minmax/ pset,get,pwell

pset / pwell / zone / well_center.zone / 11
```

There should be 13 nodes found within the region. Check the xyz extents to see that one column is selected at the intended elevations.

<pre class="lg-output">
 THE PSET  pwell  HAS         13 POINTS

cmo/printatt/mo_tet/-xyz-/minmax/pset,get,pwell
ATTRIBUTE NAME         MIN               MAX         DIFFERENCE    LENGTH
 xic           5.000000000E+01  5.000000000E+01 0.000000000E+00      1122
 yic           2.000000000E+01  2.000000000E+01 0.000000000E+00      1122
 zic           2.000000000E+01  8.000000000E+01 6.000000000E+01      1122
</pre>

## Add mesh object attributes for mesh views

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

<p> Paraview showing mesh attributes <b>w_left</b> mesh boundary (left) and node <b>imt</b> materials (right) <br>
<a href="step_04/04_tet_nodes_left_w.png"> <img width="400" src="step_04/04_tet_nodes_left_w.png" /> </a>
<a href="step_04/04_tet_nodes_imt.png"> <img width="400" src="step_04/04_tet_nodes_imt.png" /> </a>
</p>
<br>

## Check Well Zone

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
Paraview image showing mesh clipped at well location and the well zone nodes. Check the well points are located correctly with respect to mesh materials and connectivity. Query a well node to check attributes such as **imt** material and **elev**. Note when the well points are subset from the full mesh, the node ID changes but the attribute **node_id** has the original mesh node ID saved.


<p> 
<a href="step_04/04_tet_nodes_imt_well_pts.png"> <img width="400" src="step_04/04_tet_nodes_imt_well_pts.png" /> </a>
 <a href="step_04/04_well_nodes_paraview.png"> <img width="450" src="step_04/04_well_nodes_paraview.png" /> </a>
</p>

## finish

Always end a session or a file with the **finish** command.

```
finish
```

## Snapshots of Paraview Sessions

Snapshot Paraview session shows clipped mesh and well points.

<p> 
 <a href="step_04/04_mesh_clip_well_paraview.png"> <img width="400" src="step_04/04_mesh_clip_well_paraview.png" /> </a>
</p>
<br>

## finish

Always end a session or a file with the **finish** command and a line return after the finish command. The command line parser will not parse a command without a line return.

```
finish

```
