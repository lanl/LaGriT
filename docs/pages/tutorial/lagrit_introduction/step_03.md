---
title: Tutorial LaGriT Introduction Step 02
---

# Step 3. Assign materials to the Mesh 

<!-- Begin image -->
<p><a href="step_01/01_hex_mesh.png"> <img width="500" src="step_01/01_hex_mesh.png" /> </a></p>
<br>
<!-- End image -->


#### LaGriT command file: [01_create_hex.lgi](step_01/01_create_hex.lgi.txt)
#### LaGriT  output file: [lagrit.out](step_01/01_create_hex.out.txt)


## Read the Hex Mesh  



## Assign Materials by Selected Sets (pset and eltset) 



## Assign Materials by Surfaces



## Assign Materials by Interpolation




## Write the Mesh and View


Write an AVS format mesh file for viewing the mesh.
This file can be rendered in certain scientific 3D visualization applications such as Paraview.

Viewing this mesh you should see something similar to the image at the top of the page.
By default, paraview will color the mesh by imt values which are all equal to 1. 

View the mesh using the node attribute itp to see the boundary nodes. The outside nodes will have value of 10 and internal nodes will have value 0. Note when viewing a mesh colored by a node, the colors will "bleed" from one node to the next. Views colored by cell or element will be more distinct.

Image shows hex mesh with node itp colors. The mesh is clipped in half to see the inside nodes of the mesh.
<p><a href="step_01/01_hex_mesh_itp.png"> <img width="300" src="step_01/01_hex_mesh_itp.png" /> </a></p>

```
dump/ avs / 01_hex_mesh.inp / 3dmesh
```

## finish

Always end a session or a file with the **finish** command.

```
finish
```

