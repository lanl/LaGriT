---
title: Tutorial LaGriT Introduction Step 02
---

# Step 2. Create Tet Mesh from the Hex Mesh 

These examples will use 2 methods for creating a tet mesh from the hex mesh created in Step 1. The first method will use the mesh points to [connect](https://lanl.github.io/LaGriT/pages/docs/commands/CONNECT1.html) into a Delaunay tet mesh. The second will use [grid2grid](https://lanl.github.io/LaGriT/pages/docs/commands/GRID2GRID.html) to convert hex elements into tetrahedrals.

<!-- Begin image -->
<p><a href="step_01/01_hex_mesh.png"> <img width="500" src="step_01/01_hex_mesh.png" /> </a></p>
<br>
<!-- End image -->


#### LaGriT command file: [01_create_hex.lgi](step_01/01_create_hex.lgi.txt)
#### LaGriT  output file: [lagrit.out](step_01/01_create_hex.out.txt)

# Start with hex mesh from Step 1.
# Create hex mesh from Example 1
infile 01_create_hex.lgi

# Check that mesh was created
cmo/status/ 3dmesh / brief


# Method using connect

## Create the Hex Mesh from Step 1

# Copy the hex points into a new mesh object
cmo / create / mo_tet
copypts / mo_tet / 3dmesh

# Make mo_tet the current mesh object for commands
cmo /list
cmo/ select / mo_tet
cmo/ status / mo_tet /brief

# Mark duplicate points as dudded points
# Remove dudded points from the mesh object
# Nothing will happen if there are no duplicate points
filter/1,0,0 ; rmpoint/compress

# Set some defaults for the connect routine
cmo / setatt / mo_tet / imt / 1 0 0 / 1
cmo / setatt / mo_tet / itp / 1 0 0 / 0

# Create Delaunay tet connectivity of all nodes in the mesh
connect

# Check for 0 or negative elements
quality

# set default materials and boundary tags
cmo / setatt / mo_tet / itetclr / 1
resetpts / itp

# Method using grid2grid convert

# Convert each hex into 5 tets
grid2grid / hextotet5 / mo_hex2tet / 3dmesh

# The new mesh object should be current
cmo / list

# Check for 0 or negative elements
quality

# set default materials and boundary tags
cmo / setatt / mo_hex2tet / itetclr / 1
resetpts / itp

# Add attributes and write files for viewing

cmo/addatt / mo_tet / volume / tet_vol
cmo/addatt / mo_tet / voronoi_volume / vor_vol

cmo/addatt / mo_hex2tet / volume / tet_vol
cmo/addatt / mo_hex2tet / voronoi_volume / vor_vol

cmo/printatt/ mo_tet / tet_vol / minmax
cmo/printatt/ mo_tet / vor_vol / minmax
cmo/printatt/ mo_hex2tet / tet_vol / minmax
cmo/printatt/ mo_hex2tet / vor_vol / minmax

dump/avs/02_tet_connect.inp/mo_tet
dump/avs/02_hex2tet5.inp/mo_hex2tet




## finish

Always end a session or a file with the **finish** command.

```
finish
```
