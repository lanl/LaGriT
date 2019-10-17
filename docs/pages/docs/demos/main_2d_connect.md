
## Examples:  connect 2D

The objective is to connect a set of random points into a triangle mesh.

| Input Random Points | Output Triangle Mesh (boundary nodes red) | 
| :---: | :---: | 
|  <img  width="350" src="https://lanl.github.io/LaGriT/assets/images/2d_connect_pts.png"> |  <img  width="350" src="https://lanl.github.io/LaGriT/assets/images/2d_connect_tri.png"> | 


1. Create a mesh object named mo_tri and create a set of random points within a box.
2. Connect and Set node itp to mark boundary nodes.
3. Check mesh quality and write mesh file.
 
### Input Commands

```
# create a 2D mesh object
# triplane has ndimensions_topo=2 and ndimensions_geom=2 
cmo / create / mo_tri / / / triplane

# Make some points at the four corners
createpts / xyz / 5 5 1 / 0. 0. 0. / 1. 1. 0. / 1 1 1

# Add some random points and delete duplicate points
createpts / random / xyz / 0.4 / 0.1 0.1 0. / 0.9 0.9 0.
filter / 1 0 0
rmpoint / compress

# set some defaults for the connect routine
# imt material set to 1 and itp boundaries set to 0
cmo / setatt / mo_tri / imt / 1 0 0 / 1
cmo / setatt / mo_tri / itp / 1 0 0 / 0

connect

# set default materials and boundary tags
cmo / setatt / mo_tri / itetclr / 1 0 0 / 1
resetpts / itp

# check areas and write an AVS format file of the mesh
quality
dump / avs / tri.inp / mo_tri
```

## Output from quality command (see screen output and lagrit.out)

```
epsilonl, epsilonaspect:   3.1401849E-13  3.0964614E-38                         
--------------------------------------------                                    
elements with aspect ratio < .01:                    0                          
elements with aspect ratio b/w .01 and .02:          0                          
elements with aspect ratio b/w .02 and .05:          0                          
elements with aspect ratio b/w .05 and .1 :          0                          
elements with aspect ratio b/w .1  and .2 :          0                          
elements with aspect ratio b/w .2  and .5 :          1                          
elements with aspect ratio b/w .5  and 1. :         51                          
min aspect ratio =  0.4160E+00  max aspect ratio =  0.9885E+00                  
 
epsilonvol:   4.4408921E-13                                                     
---------------------------------------                                         
element volumes b/w  0.6647E-02 and  0.9188E-02:         2                      
element volumes b/w  0.9188E-02 and  0.1270E-01:        18                      
element volumes b/w  0.1270E-01 and  0.1756E-01:         3                      
element volumes b/w  0.1756E-01 and  0.2427E-01:        16                      
element volumes b/w  0.2427E-01 and  0.3354E-01:        13                      
min volume =   6.6465893E-03  max volume =   3.3543824E-02                      
-----------------------------------------------------------                     
        52 total elements evaluated.                                         
```




[Old Example Antler Ridge](description_2dconn.md)
*Note this old example is non-convex and has long tets outside the boundary that need to be removed.*


