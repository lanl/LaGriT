---
GENERATOR: 'Mozilla/4.7 [en] (X11; I; IRIX 6.5 IP32) [Netscape]'
Generator: 'Microsoft Word 97/98'
Template: 'Macintosh HD:Microsoft Office 98:Templates:Web Pages:Blank Web Page'
title: '
*arguments:'
---

 Example 4: addmesh / delete

  The objective is to create a mesh using the **addmesh/delete**
  command by

  deleting one mesh from another.
  Two cubes, one smaller than the other, each having a different grid
  resolution are combined. The **delete** option finds the elements of
  mesh1 that intersect

  mesh2 and removes them from mesh1, creating the resultant mesh
  (mesh3).

  

 Images of GMV output

   ------------------------------------------------------------------------------------------------------------------------------------------------------------------ ------------------------------------------------------------------------------------------------------------------------------------------------------------------ ----------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------
   [<img height="300" width="300" src="https://lanl.github.io/LaGriT/docsassets/images/addmesh_delete/addmesh_mesh1_tn.gif">"114" "89"](image/addmesh_delete/addmesh_mesh1.gif">[mesh1](image/addmesh_delete/addmesh_mesh1.gif">   [<img height="300" width="300" src="https://lanl.github.io/LaGriT/docsassets/images/addmesh_delete/addmesh_mesh2_tn.gif">"114" "89"](image/addmesh_delete/addmesh_mesh2.gif">[mesh2](image/addmesh_delete/addmesh_mesh2.gif">   [<img height="300" width="300" src="https://lanl.github.io/LaGriT/docsassets/images/addmesh_delete/addmesh_delete_tn.gif">"114" "89"](image/addmesh_delete/addmesh_delete.gif">[mesh2 deleted from mesh1](image/addmesh_delete/addmesh_delete.gif">
   ------------------------------------------------------------------------------------------------------------------------------------------------------------------ ------------------------------------------------------------------------------------------------------------------------------------------------------------------ ----------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------

 Input file

 
* TEST addmesh/delete (lagrit\_input\_delete)

 read / gmv / input\_mesh1.gmv / cmo1

 read / gmv / input\_mesh2.gmv / cmo2

 
*

 addmesh / delete / cmo3 / cmo1 / cmo2

 
*

 resetpts / itp

 dump / gmv / output\_delete.gmv / cmo3

 
* begin compare here

 cmo/status

 cmo/printatt//-all-/minmax

 quality

 finish
