    createpts/brick/xyz/3,2,3/0.,0.,0./1.,1.,1./1,1,1

Creates a hex grid 2x1x2 cells in the unit cube. 

Argument | Description
-------|------------------------------------------------------------------------------------------------------------
xyz | specifies cartesian coordinates
3,2,3 | create 3 points in the x direction, create 2 points in the y direction, create 3 points in the z direction
0,0,0 | xmin= 0, ymin= 0, zmin= 0
1,1,1 | xmax= 1, ymax= 1, zmax= 1
1,1,1 | xmin and xmax are cell vertices, ymin and ymax are cell vertices, zmin and zmax are cell vertices

Input file:

    # create a hexahedral grid
    cmo create abc///hex
    quadxyz/5 7 5/0. 0. 0. /1. 0. 0. /1.5 0.5 2.0 /0.5 0.2 2.5 /-1. 1.5 0. /2. 2. 0. &
             2.1 1.9 2.4 /-.2 1.8 2.3 /
    setpts
    creatpts/brick/xyz/5 7 5/1,0,0/connect/
    settets
    dump/gmv/gmv.hex
    finish


 

