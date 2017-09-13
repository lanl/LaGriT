 **PERTURB**

This command moves node coordinates in the following manner.

Three pairs of random numbers between 0 and 1 are generated. These pairs
refer to the x, y and z coordinates of the nodes respectively. The first
random number of each pair is multiplied by the factor given in the
command. The second random number is used to determine if the calculated
offset is to be added or subtracted from the coordinate.

WARNING: No checking is done to see if elements are inverted by this
perturbation. If one uses too large a value for the perturbation, one
can easily cause element inversions that will flip the normal vector of
triangles and cause 3D cells to have negative volumes.

FORMAT:

**perturb/pset,get,psetnam**e/xfactor,yfactor,zfactor

 

EXAMPLES

    perturb/1,0,0/0.5,0,0   

add offsets to only the xcoordinates of all nodes

    perturb/pset,get,mypset/0.001,0.001,0.001  
    
add small offsets to all coordinates of the nodes in the **pset** named *mypset.*
