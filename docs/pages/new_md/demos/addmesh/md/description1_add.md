---
GENERATOR: 'Mozilla/4.7 [en] (X11; I; IRIX 6.5 IP32) [Netscape]'
title: '
*arguments:'
---

 Example 1: addmesh / add

  The objective is to join two meshes to create a third using the
  **addmesh / add** command.
  Two cubes, one smaller than the other, each having a different grid
  resolution are combined. The **add** option finds the intersection
  and refines one mesh to interface with the boundary of the other
  mesh using cell volume ration criterion.

  [](../lagrit_input_add) 

  Images of GMV output

   
    ---------------------------------------------------------------------------------------------------------------------------------------------------------- ---------------------------------------------------------------------------------------------------------------------------------------------------------- -------------------------------------------------------------------------------------------------------------------------------------------------------
    [<img height="300" width="300" src="https://lanl.github.io/LaGriT/docs/assets/images/addmesh_add/addmesh_mesh1_tn.gif">"112" "87"](image/addmesh_add/addmesh_mesh1.gif">[Input1](image/addmesh_add/addmesh_mesh1.gif">   [<img height="300" width="300" src="https://lanl.github.io/LaGriT/docs/assets/images/addmesh_add/addmesh_mesh2_tn.gif">"114" "89"](image/addmesh_add/addmesh_mesh2.gif">[Input2](image/addmesh_add/addmesh_mesh2.gif">   [<img height="300" width="300" src="https://lanl.github.io/LaGriT/docs/assets/images/addmesh_add/addmesh_out2_tn.gif">"114" "89"](image/addmesh_add/addmesh_out2.gif">[Output](image/addmesh_add/addmesh_out2.gif">
    ---------------------------------------------------------------------------------------------------------------------------------------------------------- ---------------------------------------------------------------------------------------------------------------------------------------------------------- -------------------------------------------------------------------------------------------------------------------------------------------------------
 
  

   
  Input file

  
*TEST addmesh/add (lagrit\_input\_add)

  read / gmv / input\_mesh1.gmv / cmo\_mesh1

  read / gmv / input\_mesh2.gmv / cmo\_mesh2

  
*

  addmesh / add / cmo3 / cmo\_mesh1 / cmo\_mesh2 / 3 / edge

  
*

  resetpts / itp

  dump / gmv / output\_add.gmv/ cmo3

  
* begin compare here

  cmo/status

  cmo/printatt//-all-/minmax

  quality

  finish
