
Example 5: addmesh / intersect

The objective is to create a point set that are the nodes in mesh1
that intersect elements of mesh2.
Two cubes defined by mesh1 and mesh2 are read. The perimeter of
mesh2 lies within that of mesh1. The **addmesh / intersect** command
is used to determine the nodes in mesh1 that intersect elements of
mesh2.

Input

[lagrit_input_inter](input/lagrit_input_inter.txt)
 
Images of GMV output

<img  width="300" src="https://lanl.github.io/LaGriT/assets/images/addmesh_mesh1_tn.gif">

<img  width="300" src="https://lanl.github.io/LaGriT/assets/images/addmesh_mesh2_tn.gif">

<img  width="300" src="https://lanl.github.io/LaGriT/assets/images/add_inter_tn.gif">
  

Input file:

	* TEST
	
	addmesh/intersect (lagrit\_input\_inter)
	read / gmv / input\_mesh1.gmv / cmo1
	read / gmv / input\_mesh2.gmv / cmo2
	
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

