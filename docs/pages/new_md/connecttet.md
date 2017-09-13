---
GENERATOR: 'Mozilla/4.7 [en] (X11; I; IRIX 6.5 IP32) [Netscape]'
Generator: Microsoft Word 98
title: 7
---

 

 **7. Connect the points into tetrahedra**

The mesh designer may use the following set of command to connect the
points into a tetrahedral mesh:

 


* eliminate coincident or nearly coincident points


* 1,0,0 means consider all points

**[filter](FILTER.md)/1,0,0**/


* rayend points are set to invisible (21 is the code for invisible)


* they were used as end points of the rays in the regnpts command

**[cmo](cmo_setatt.md)/setatt//itp/pset,get,rayend/21**/


* assign material colors to the points


* identify points that are on material interfaces


* identify constrained points

**[setpts](SETPTS.md)**


* connect the points into a Delaunay tetrahedral mesh


* do not connect across material interfaces - add points if necessary
to resolve material interfaces

**[connect](CONNECT1.md)**


* set element (tetrahedral) color


* spawn child points at material interfaces

**[settets](SETTETS.md)**


* dump mesh to some output form

**dump****/gmv**/filename


* terminate processing

**[finish](FINISH.md)**

<img height="300" width="300" src="Image229.gif">"452" "411"

The complete input for the tutorial is:

 


* create a 3D tetrahedral mesh object and name it *3dmesh*

**cmo/create**/3dmesh/


* unit cube

**surface**/cube**/reflect****/box**/0.0,0.0,0.0/1.0,1.0,1.0/


* define z=.5 plane as interface

**surface** /cutplane**/intrface/plane/0.,0.,.5/1.,0.,.5/1.,1.,.5**/


*define geometric regions

**region**/top**/ le** cube **and gt** cutplane **** /

**region**/bottom**/ le** cube **and le** cutplane /

**
* define material regions**

**mregion**/mattop**/ le** cub**e and gt** cutplane /

**mregion**/matbot**/ le** cube **and lt** cutplane /


* create 25 points (5x5x1) in a plane above the unit cube


* place points on the boundaries in the x and y directions (1,1,0)

**createpts/xyz**/5,5,1/0.,0.,1.1/1.,1.,1.1/1,1,0/


* give the points defined by the createpts command the name, rayend

**pset**/rayend**/seq**/1,0,0/


* create rays between points in rayend and the plane below the cube


* distribute 3 points along these rays in the region top


* add one point at the upper external boundary for each ray

**regnpts**/top/3**/pset**,**get**,rayend**/xyz**/0.,0.,-.1/0.,1.,-.1/1.,1.,-.1/0,0/


* distribute 4 points along these rays in the region *bottom*


* add one point at the lower external boundary for each ray


* add one point at the material interface for each ray since


* *bottom* contains the interface - a total of 5 points for each ray.


* points will be distributed such that the ratio of distances between


* any two consecutive pairs of points is 0.6 traveling from the source


* of the ray (the plane) to the ray end.

**regnpts**/bottom/4**/pset**,**get**,rayend**/xyz**/0.,0.,-.1/0.,1.,-.1/1.,1.,-.1/1,.6/


* eliminate coincident or nearly coincident points


* 1,0,0 means consider all points

**filter/1,0,0**/


* rayend points are set to invisible (21 is the code for invisible)


* they were used as end points of the rays in the regnpts command

**cmo/setall//itp/pset,get,**rayend/21****/


* assign material colors to the points


* identify points that are on material interfaces


* identify constrained points

**setpts**


* connect the points into a Delaunay tetrahedral mesh


* do not connect across material interfaces -


* add points if necessary to resolve material interfaces

**connect**


* set element (tetrahedral) type

**settets**


* dump mesh to some output form

**dump/gmv**/filename


* terminate processing

**finish**
