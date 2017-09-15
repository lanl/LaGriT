---
GENERATOR: 'Mozilla/4.7 [en] (X11; I; IRIX 6.5 IP32) [Netscape]'
Generator: 'Microsoft Word /98'
Template: 'Macintosh HD:Microsoft Office 98:Templates:Web Pages:Blank Web Page'
title: '
*arguments:'
---

 

 Example 5: addmesh / intersect

  The objective is to create a point set that are the nodes in mesh1
  that intersect elements of mesh2.
  Two cubes defined by mesh1 and mesh2 are read. The perimeter of
  mesh2 lies within that of mesh1. The **addmesh / intersect** command
  is used to determine the nodes in mesh1 that intersect elements of
  mesh2.

 Input

     [lagrit\_input\_inter](../lagrit_input_inter)
 Images of GMV output

  ------------------------------------------------------------------------------------------ ------------------------------------------------------------------------------------------ ------------------------------------------------------------------------------------------
  [mesh1](image/addmesh_intersect/addmesh_mesh1.gif"><img height="300" width="300" src="addmesh_mesh1_tn.gif">   [mesh2](image/addmesh_intersect/addmesh_mesh2.gif"><img height="300" width="300" src="addmesh_mesh2_tn.gif">"122"   [mesh3](image/addmesh_intersect/add_inter.gif"><img height="300" width="300" src="add_inter_tn.gif">"151"
  "93"](image/addmesh_intersect/addmesh_mesh1.gif">
                                  "102"](image/addmesh_intersect/addmesh_mesh2.gif">
                                 "107"](image/addmesh_intersect/add_inter.gif">

   
                                                                                          
                                                                                          

   
                                                                                          
                                                                                          

   
                                                                                          
                                                                                          

   
                                                                                          
                                                                                          

   
                                                                                          
                                                                                          

   
                                                                                          
                                                                                          

   
                                                                                                                                                                                     

                                                                                                                                                                                         
                                                                                                                                                                                        Nodes of mesh1(red) that intersect elements of mesh2
  ------------------------------------------------------------------------------------------ ------------------------------------------------------------------------------------------ ------------------------------------------------------------------------------------------

 Input File
* TEST
 addmesh/intersect (lagrit\_input\_inter)

 read / gmv / input\_mesh1.gmv / cmo1

 read / gmv / input\_mesh2.gmv / cmo2

 
*

 
* get nodes of cmo1 that intersect elements of mesh cmo2

 
* and put them into a pset called pset\_overlap

 addmesh / intersect / pset\_overlap / cmo1 / cmo2

 cmo/setatt/cmo1/imt/1 0 0/1

 cmo/setatt/cmo1/imt/pset get pset\_overlap/2

 dump/gmv/output\_inter.gmv/cmo1

 
* begin compare here

 cmo/status

 cmo/printatt//-all-/minmax

 quality

 finish

