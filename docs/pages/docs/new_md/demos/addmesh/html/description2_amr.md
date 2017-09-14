---
GENERATOR: 'Mozilla/4.7 [en] (X11; I; IRIX 6.5 IP32) [Netscape]'
Generator: 'Microsoft Word 97/98'
Template: 'Macintosh HD:Microsoft Office 98:Templates:Web Pages:Blank Web Page'
title: '
*arguments:'
---

 Example 2: addmesh / amr

  The objective is to join two meshes to create a third using the
  **addmesh / amr** command.
  Two cubes, one smaller than the other, each having a different grid
  resolution are combined. The **amr** option finds the intersection
  and refines one mesh to interface with the boundary of the other
  mesh using adaptive mesh refinement.

   
 
  Output Images

   
 
    ------------------------------------------------------------------------------------------------------------------------------------------------------------------- ------------------------------------------------------------------------------------------------------------------------------------------------------------------- ----------------------------------------------------------------------------------------------------------------------------
    [mesh 1](image/addmesh_amr/addmesh_amr1.gif">[<img height="300" width="300" src="https://lanl.github.io/LaGriT/assets/images/addmesh_amr/addmesh_amr1_tn.gif">"114" "89"](image/addmesh_amr/addmesh_amr1.gif">               [mesh 2](image/addmesh_amr/addmesh_amr2.gif">[<img height="300" width="300" src="https://lanl.github.io/LaGriT/assets/images/addmesh_amr/addmesh_amr2_tn.gif">"114" "89"](image/addmesh_amr/addmesh_amr2.gif">               [combined (view 1)](image/addmesh_amr/addmesh_amr3.gif"><img height="300" width="300" src="https://lanl.github.io/LaGriT/assets/images/addmesh_amr/addmesh_amr3_tn.gif">"114" "89"
    [combined (view 2) ](image/addmesh_amr/addmesh_amr4.gif">[<img height="300" width="300" src="https://lanl.github.io/LaGriT/assets/images/addmesh_amr/addmesh_amr4_tn.gif">"114" "89"](image/addmesh_amr/addmesh_amr4.gif">   [combined (view 3) ](image/addmesh_amr/addmesh_amr5.gif">[<img height="300" width="300" src="https://lanl.github.io/LaGriT/assets/images/addmesh_amr/addmesh_amr5_tn.gif">"114" "89"](image/addmesh_amr/addmesh_amr5.gif">   
    ------------------------------------------------------------------------------------------------------------------------------------------------------------------- ------------------------------------------------------------------------------------------------------------------------------------------------------------------- ----------------------------------------------------------------------------------------------------------------------------
 
  

   
 
  Input file

  
*TEST addmesh/amr (lagrit\_input\_amr)

  read gmv input\_mesh1.gmv cmo1

  
*dump/ gmv / out\_mesh1.gmv

  read gmv input\_mesh2.gmv cmo2

  
*dump/ gmv / out\_mesh2.gmv

  addmesh amr cmo3 cmo1 cmo2

  dump/ gmv / output\_amr.gmv

  
* begin compare here

  cmo/status

  cmo/printatt//-all-/minmax

  quality

  finish
