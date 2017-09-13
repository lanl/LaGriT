create a 3-D tetrahedral mesh object and name it 3dmesh

 cmo/create/3dmesh

unit cube

 surface/cube/reflect/box/0.0,0.0,0.0/1.0,1.0,1.0

define z plane as tilted interface

 surface/cutplane/intrface/plane/0.,0.,.5/1.,0.,1./1.,1.,1.

define geometric regions

 region/top/ le cube and gt cutplane /
 region/bottom/ le cube and le cutplane 

define material regions

 mregion/mattop/ le cube and gt cutplane /
 mregion/matbot le cube and lt cutplane 

create 25 points (5x5x1) in a plane above the unit cube
place points on the boundaries in the x and y directions (1,1,0)

 rz/xyz/7,6,1/0.,0.,1.1/1.,1.,1.1/1,1,0

give the points defined by the rz command the name, rayend

 pset/rayend/seq/1,0,0

create rays between points in rayend and the plane below the cube
distribute 3 points along these rays in the region top
add one point at the upper external boundary for each ray

 regnpts/top/3/pset,get,rayend/xyz/0.,0.,-1./1.,0.,-1./1.,1.,-1./0,0

distribute 4 points along these rays in the region bottom
add one point at the lower external boundary for each ray
add one point at the material interface for each ray since
bottom contains the interface - a total of 5 points for each ray.
points will be distributed such that the ratio of distances between
any two consecutive pairs of points is 0.6 traveling from the source
of the ray (the plane) to the ray end.

 regnpts/bottom/4/pset,get,rayend/xyz/0.,0.,-1./1.,0.,-1./1.,1.,-1./1,.6

eliminate coincident or nearly coincident points

1,0,0 means consider all points

 filter/1,0,0

give all points a default material type

 zq/imt/1,0,0/1

rayend points are set to invisible (dud is the code for invisible)
they were used as end points of the rays in the regnpts command

 zq/itp/pset,get,rayend/dud

assign material colors to the points
identify points that are on material interfaces
identify constrained points

 setpts

connect the points into a Delaunay tetrahedral mesh
do not connect across material interfaces -
add points if necessary to resolve material interfaces

 search

set element material type (color)

 settets

dump mesh to some output form

 dump/gmv/gmv.regnpts

terminate processing

 [Return to LaGriT Home Page](index.md)




