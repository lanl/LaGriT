---
GENERATOR: 'Mozilla/4.7 [en] (X11; I; IRIX 6.5 IP32) [Netscape]'
Generator: 'Microsoft Word 97/98'
Template: 'Macintosh HD:Microsoft Office 98:Templates:Web Pages:Blank Web Page'
title: '
*arguments:'
---

 Example 7: addmesh/merge

  The objective is to join two meshes using the
  **addmesh/merge**command.
  Two rectangular meshes are read and combined. The **merge** command
  concatenates the meshes.

   
 
  Images of GMV output

   
 
    ---------------------------------------------------------------------------------------------------------------------------------------------------------------- ---------------------------------------------------------------------------------------------------------------------------------------------------------------- ----------------------------------------------------------------------------------------------------------------------------------------------------------------
    [<img height="300" width="300" src="https://lanl.github.io/LaGriT/assets/images/addmesh_merge/addmesh_mesh1_tn.gif">"114" "89"](image/addmesh_merge/addmesh_mesh1.gif">[Input1](image/addmesh_merge/addmesh_mesh1.gif">   [<img height="300" width="300" src="https://lanl.github.io/LaGriT/assets/images/addmesh_merge/addmesh_mesh2_tn.gif">"114" "89"](image/addmesh_merge/addmesh_mesh2.gif">[Input2](image/addmesh_merge/addmesh_mesh2.gif">   [<img height="300" width="300" src="https://lanl.github.io/LaGriT/assets/images/addmesh_merge/addmesh_mesh3_tn.gif">"114" "89"](image/addmesh_merge/addmesh_mesh3.gif">[Merged](image/addmesh_merge/addmesh_mesh3.gif">
    ---------------------------------------------------------------------------------------------------------------------------------------------------------------- ---------------------------------------------------------------------------------------------------------------------------------------------------------------- ----------------------------------------------------------------------------------------------------------------------------------------------------------------
 
  

   
 
  Input file

  
* TEST addmesh/merge (lagrit\_input\_merge)

  read   gmv   input\_mesh3.gmv cmo1

  read   gmv   input\_mesh4.gmv   cmo2

  
*

  addmesh   merge   cmo3   cmo1   cmo2

  
*

  filter 1 0 0

  dump   gmv   output\_merge.gmv  cmo3

  
* begin compare here

  cmo/status

  cmo/printatt//-all-/minmax

  quality

  finish
