---
GENERATOR: 'Mozilla/4.7 [en] (X11; I; IRIX 6.5 IP32) [Netscape]'
Generator: 'Microsoft Word 97/98'
Template: 'Macintosh HD:Microsoft Office 98:Templates:Web Pages:Blank Web Page'
title: '
*arguments:'
---

 Example 3: addmesh/append

 The objective is to join two meshes to create a third using the
 **addmesh / append** command.

 Two cubes, one smaller than the other, each having a different grid
 resolution are combined. The **append** option concatenates two meshes
 where imt, icr, and itetclr

 of mesh2 are given the values max(imt(mesh1)).

  

 Images of GMV output
   -------------------------------------------------------------------------------------------------------------------------------------------------------------------------------- ------------------------------------------------------------------------------------------------------------------------------------------------------------------------------
   [<img height="300" width="300" src="https://lanl.github.io/LaGriT/docs/assets/images/addmesh_append/addmesh_append1_tn.gif">"114" "89"](image/addmesh_append/addmesh_append1.gif">[input mesh1](image/addmesh_append/addmesh_append1.gif">     [<img height="300" width="300" src="https://lanl.github.io/LaGriT/docs/assets/images/addmesh_append/addmesh_append2_tn.gif">"114" "89"](image/addmesh_append/addmesh_append2.gif">[input mesh2](image/addmesh_append/addmesh_append2.gif">
   [<img height="300" width="300" src="https://lanl.github.io/LaGriT/docs/assets/images/addmesh_append/addmesh_append3_tn.gif">"114" "89"](image/addmesh_append/addmesh_append3.gif">[combined mesh](image/addmesh_append/addmesh_append3.gif">   
   -------------------------------------------------------------------------------------------------------------------------------------------------------------------------------- ------------------------------------------------------------------------------------------------------------------------------------------------------------------------------

 

  
 Input file

 
* TEST addmesh/append (lagrit\_input\_append)

 read / gmv / input\_mesh3.gmv/  cmo1

 cmo status cmo1

 read / gmv / input\_mesh4.gmv/  cmo2

 cmo status cmo2

 addmesh / append / cmo3 / cmo1 / cmo2

 
*

 
* run filter to get rid of duplicate nodes

 
*

 filter 1 0 0

 resetpts / itp

 cmo status cmo3

 dump / gmv / output\_append.gmv/ cmo3

 
* begin compare here

 cmo/status

 cmo/printatt//-all-/minmax

 quality

 finish

  


