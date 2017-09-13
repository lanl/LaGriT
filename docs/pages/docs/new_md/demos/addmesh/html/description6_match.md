---
GENERATOR: 'Mozilla/4.7 [en] (X11; I; IRIX 6.5 IP32) [Netscape]'
Generator: 'Microsoft Word 97/98'
Template: 'Macintosh HD:Microsoft Office 98:Templates:Web Pages:Blank Web Page'
title: '
*arguments:'
---

 Example 6: addmesh / match

 The objective is to join two meshes to create a third using the
 **addmesh/match** command. Two rectangular meshes are combined. The
 match option concatenates two meshes but allows the second to be
 translated, rotated, and scaled.

   

 Images of GMV output

  

   ---------------------------------------------------------------------------------------------------------------------------------------------------------------- ---------------------------------------------------------------------------------------------------------------------------------------------------------------- ---------------------------------------------------------------------------------------------------------------------------------------------------------------------------
   [<img height="300" width="300" src="/assets/images/addmesh_match/addmesh_mesh1_tn.gif">"114" "89"](image/addmesh_match/addmesh_mesh1.gif"> [mesh1](image/addmesh_match/addmesh_mesh1.gif">   [<img height="300" width="300" src="/assets/images/addmesh_match/addmesh_mesh2_tn.gif">"114" "89"](image/addmesh_match/addmesh_mesh2.gif"> [mesh2](image/addmesh_match/addmesh_mesh2.gif">   [<img height="300" width="300" src="/assets/images/addmesh_match/addmesh_mesh3_tn.gif">"114" "89"](image/addmesh_match/addmesh_mesh3.gif"> [mesh3](/test/md/image/addmesh_match/addmesh_mesh3.gif">
   ---------------------------------------------------------------------------------------------------------------------------------------------------------------- ---------------------------------------------------------------------------------------------------------------------------------------------------------------- ---------------------------------------------------------------------------------------------------------------------------------------------------------------------------

  

 Input file

 
* TEST addmesh/match (lagrit\_input\_match)

 read   gmv   input\_mesh3.gmv  cmo1

 read   gmv   input\_mesh4.gmv  cmo2

 addmesh   match   cmo3   cmo1   cmo2 / &

       2.0 0.0 0.0 / 3.0 0.0 0.0 / 2.0 0.0 2.0 &

       2.0 0.0 0.0 / 2.0 1.0 0.0 / 2.0 0.0 2.0

 filter 1 0 0

 dump   gmv   output\_match.gmv  cmo3

 
* begin compare here

 cmo/status

 cmo/printatt//-all-/minmax

 quality

 finish
