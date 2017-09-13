---
GENERATOR: 'Mozilla/4.7 [en] (X11; I; IRIX 6.5 IP32) [Netscape]'
Generator: Microsoft Word 98
---

 

 **6. Distribute points within the volume**

There are many methods of distributing points within a volume.  For
simple geometries refer to the [createpts](createpts.md) command. 
This example uses the regnpts command which, although more complicated,
provides greate flexibility.  Points are distributed within regions
using Cartesian, cylindrical or spherical coordinates by constructing
rays that travel through regions and distributing points along these
rays. For this example, points are distributed using Cartesian
coordinates. The rays are specified by defining a set of points and a
plane. For each point in the set, a ray is constructed normal to the
plane passing through the point. In general rays are constructed in
sets, each set is specified by a single plane and a set of points. The
**createpts** command is used to create the points. The[regnpts](REGNPTS.md) command is used to specify the plane, to specify
the region, and to specify the number of points to be distributed along
the rays. The points and the plane should lie outside the enclosing
volume and on opposite sides. The normal to the plane should point
toward the point. As rays are created, if they do not pass through the
specified region, no points are distributed. Points may be spaced evenly
along the ray or they may be spaced according to a ratio. The following
commands will place points in the unit cube.

 


* create 25 points (5x5x1) in a plane above the unit cube


* place points on the boundaries in the x and y directions (1,1,0)

**createpts****/xyz**/5,5,1/0.,0.,1.1/1.,1.,1.1/1,1,0


* give the points defined by the **createpts** command the name,
*rayend*

**pset/rayend/seq/1,0,0**/

<img height="300" width="300" src="Image226.gif">"283" "208"


* create rays between points in *rayend* and the plane below the cube


* distribute 3 points along these rays in the region *top*


* add one point at the upper external boundary for each ray


* will get 4 points total along each ray in region *top*


* "**pset**,**get**,rayend" refers to all the points named *rayend*


* the three points: (0.,0.,-.1), (0.,1.,-.1), (1.,1.,-.1)


* define a plane whose normal points toward the *rayend* points

**regnpts**/top/3**/pset**,**get**,rayend**/xyz**/0.,0.,-.1/0.,1.,-.1/1.,1.,-.1/0,0

<img height="300" width="300" src="Image227.gif">"342" "270"


* distribute 4 points along these rays in the region *bottom*


* add one point at the lower external boundary for each ray


* add one point at the material interface for each ray since


* *bottom* contains the interface - a total of 6 points for each ray.


* points will be distributed such that the ratio of distances between


* any two consecutive pairs of points is 0.6, traveling from the source


* of the ray (the plane) to the ray end.

**regnpts**/bottom/4**/pset**,**get**,rayend**/xyz**/0.,0.,-.1/0.,1.,-.1/1.,1.,-.1/1,.6

<img height="300" width="300" src="Image228.gif">"347" "308"

Other versions of the **regnpts** are appropriate for cylindrical and
spherical geometries. For cylindrical geometries the **createpts**
command specifies points in a cylindrical shell outside the volume. The
**regnpts** command specifies a line (usually the cylinder axis), and
the rays are constructed normal to this line and containing one of the
**createpts** points. For spherical geometries the **createpts** command
specifies points in a spherical shell outside the volume. The
**regnpts** command specifies a point (usually the center of the sphere)
from which rays are constructed to the **createpts** points.
