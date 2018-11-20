---
title: "LaGriT Tutorial"
---

# Tutorial
-----------------------
### **Generating Initial Grids Using the LaGriT Command Language** ###

The steps involved in generating three dimensional grids in the LaGriT command language are:
 
1. [Define mesh objects.](definemo.md)
2. [Define an enclosing volume](defineev.md).
3. [Define interior interfaces.](DEFINEII.md)
4. [Divide the enclosing volume into regions.](dividereg.md)
5. [Assign material types to the regions.](assignmt.md)
6. [Distribute points within the volume.](distributep.md)
7. [Connect the points into tetrahedra](connecttet.md)

Detailed descriptions of the LaGriT commands are given in Section II.
This tutorial covers just the commands needed to generate a simple
grid. The tutorial will explain how to generate a grid in a unit cube
containing two materials separated by a plane. Lines that begin with
an asterisk (*) are comments; keywords are in **bold**.

---------------------------------


## 1. Define mesh objects ##

Define all Mesh Objects to be used in this problem using the [**`cmo/create`**](commands/cmo/cmo_create.md) command. The [**`cmo/create`**](commands/cmo/cmo_create.md) command establishes an
empty Mesh Object data structure ([see Section III.a](meshobject.md) for a description).

For this example we will need only a single 3D Mesh Object:

Create a 3D tetrahedral mesh object and name it `3dmesh`:

    cmo/create/3dmesh/

--------------------------------------

## 2. Define an enclosing volume ##

Define an enclosing volume using the [**`surface`**](commands/SURFACE.md)
 command. Since we are defining an exterior boundary, the boundary type
is `reflect`. The next item of information needed is the geometry of
the volume; some common geometry types are `box`, `cylinder`,
and `sphere`. Geometry types `box` and `sphere` define closed
volumes; whereas a `cylinder` is open on both ends and must be capped
by planes.

Along with the geometry type, the extent of the volume is
defined by specifying for the box its corners, or for the cylinder its
radius and end point of its axis of rotation. The enclosing volume must
be convex. Complicated enclosing volumes can be described by their
bounding surfaces including planes and sheets.

Some simple examples of enclosing volumes are:

### Unit cube: ###

<pre>
<b>surface/cube/reflect</b>/box/0.0,0.0,0.0/1.0,1.0,1.0
</pre>

<img height="300" width="300" src="https://lanl.github.io/LaGriT/assets/images/Image219.gif">

### Cylinder whose axis is the x axis with radius 1 and height 1: ###

<pre>
<b>surface/cyl_vol/reflect/cylinder</b>/0.,0.,0./1.,0.,0./1.
<b>surface/end1/reflect/plane</b>/0.,0.,0./0.,0.,1./0.,1.,1.
<b>surface/end2/reflect/plane</b>/1.,0.,0./1.,0.,1./1.,1.,1.
</pre>

<img height="300" width="300" src="https://lanl.github.io/LaGriT/assets/images/Image220.gif">
<img height="300" width="300" src="https://lanl.github.io/LaGriT/assets/images/Image221.gif">

--------------------------------------


## 3. Define interior interfaces

Interfaces are defined with the [`surface`](commands/SURFACE.md)
command. In this case the boundary type is `intrface`. If the command
defines a volume (e.g. box, cylinder) then the interface is the surface
of the volume defined. If the command defines a plane or sheet then the
interface is the plane or sheet. It is important to remember that planes
are infinite and that the order of points specifying the plane
determines a normal to the plane in the usual right-hand-rule sense
([see Section II.a.9](conventions.md)). This direction is important in
determining regions. In order to divide the unit cube defined above in
half vertically, define a plane by:

<pre>
<b>surface/cutplane/intrface</b>/plane/0.,0.,.5/1.,0.,.5/1.,1.,.5
</pre>

The normal to this plane points in the positive z direction.

<img height="300" width="300" src="https://lanl.github.io/LaGriT/assets/images/Image222.gif">

Interfaces must not be coincident with reflective boundaries. For
example to embed a rectangle inside a cube, it is necessary to extend
the ends of the rectangle beyond the cube to avoid coincident reflective
and interface surfaces:

<pre>
<b>surface/cube/reflect</b>/box/0.0,0.0,0.0/1.0,1.0,1.0
<b>surface/rect/intrface</b>/box/-0.1,0.5,0.2/1.1,0.6,0.5
</pre>

<img height="300" width="300" src="https://lanl.github.io/LaGriT/assets/images/Image223.gif">

--------------------------------------

## 4. Divide the enclosing volumes into regions

The [**`region`**](commands/REGION.md) command is used to divide the
enclosing volume into regions. The directional operators `lt`, `le`,
`gt`, and `ge` are applied to previously defined surfaces
according to the following rules.


* `lt` -- if the surface following is a volume then `lt` means inside
not including the surface of the volume. If the surface is a plane or a
sheet `lt` means the space on the side of the plane or sheet opposite
to the normal not including the plane or sheet itself.

* `le` -- if the surface following is a volume then `le` means inside
including the surface of the volume. If the surface is a plane or a
sheet `le` means the space on the side of the plane or sheet opposite
to the normal including the plane or sheet itself.

* `gt` -- if the surface following is a volume then `gt` means outside
not including the surface of the volume. If the surface is a plane or a
sheet **gt** means the space on the same side of the plane or sheet as
the normal not including the plane or sheet itself.

* `ge` -- if the surface following is a volume then `ge` means outside
including the surface of the volume. If the surface is a plane or a
sheet `ge` means the space on the same side of the plane or sheet as
the normal including the plane or sheet itself.

In region comands, surface names must be preceeded by a directional
operator. The logical operators `or`, `and`, and `not` mean union,
intersection and complement respectively. Parentheses are operators and
are used for nesting. Spaces are required as delimiters to separate
operators and operands. To define the two regions created by the plane
bisecting the unit cube:

<pre>
<b>region/top</b>/ <b>le</b> cube <b>and gt</b> cutplane
<b>region/bottom</b>/ <b>le</b> cube <b>and le</b> cutplane
</pre>

The region `bottom` contains the interface *cutplane*; top contains none
of the interface. Interior interfaces must be included in one and only
one region.

If a region touches an external boundary, include the surface that
defines the enclosing volume in region and mregion commands. For
example, the regions `top` and `bottom` are enclosed by the surface
*cube*

<img height="300" width="300" src="https://lanl.github.io/LaGriT/assets/images/Image224.gif">

-------------------

### 5. Assign material types to the regions

Assign materials to regions using
the [**`mregion`**](commands/MREGION.md) command. This command has
similar syntax to the [**`region`**](commands/REGION.md) command except
that the interface should not be assigned to any material region. To
assign two materials, `mattop` and `matbot`, to the regions `top` and
*bottom:*

<pre>
<b>mregion/ mattop/ le</b> cube <b>and gt</b> cutplane /
<b>mregion/ matbot/ le</b> cube <b>and lt</b> cutplane /
</pre>

-----------------------


### 6. Distribute points within the volume

There are many methods of distributing points within a volume.  For
simple geometries refer to the [`createpts`](createpts.md) command. 

This example uses the regnpts command which, although more complicated,
provides greate flexibility.  Points are distributed within regions
using Cartesian, cylindrical or spherical coordinates by constructing
rays that travel through regions and distributing points along these
rays. For this example, points are distributed using Cartesian
coordinates. The rays are specified by defining a set of points and a
plane. For each point in the set, a ray is constructed normal to the
plane passing through the point. In general rays are constructed in
sets, each set is specified by a single plane and a set of points. The
**`createpts`** command is used to create the points. The
[`regnpts`](REGNPTS.md) command is used to specify the plane, to specify
the region, and to specify the number of points to be distributed along
the rays.

The points and the plane should lie outside the enclosing
volume and on opposite sides. The normal to the plane should point
toward the point. As rays are created, if they do not pass through the
specified region, no points are distributed. Points may be spaced evenly
along the ray or they may be spaced according to a ratio.

The following commands will place points in the unit cube.

    # create 25 points (5x5x1) in a plane above the unit cube
    # place points on the boundaries in the x and y directions (1,1,0)
    createpts/xyz/5,5,1/0.,0.,1.1/1.,1.,1.1/1,1,0/
    
    # give the points defined by the createpts command the name, rayend
    pset/rayend/seq/1,0,0/

<img height="300" width="300" src="https://lagrit.lanl.gov/docs/new_html/Image226.gif">

    # create rays between points in rayend and the plane below the cube
    # distribute 3 points along these rays in the region top
    # add one point at the upper external boundary for each ray
    regnpts/top/3/pset,get,rayend/xyz/0.,0.,-.1/0.,1.,-.1/1.,1.,-.1/0,0/

<img height="300" width="300" src="https://lagrit.lanl.gov/docs/new_html/Image227.gif">

    # distribute 4 points along these rays in the region *bottom*
    # add one point at the lower external boundary for each ray
    # add one point at the material interface for each ray since
    # *bottom* contains the interface - a total of 5 points for each ray.
    # points will be distributed such that the ratio of distances between
    # any two consecutive pairs of points is 0.6 traveling from the source
    # of the ray (the plane) to the ray end.
    
    regnpts/bottom/4/pset,get,rayend/xyz/0.,0.,-.1/0.,1.,-.1/1.,1.,-.1/1,.6/

<img height="300" width="300" src="https://lagrit.lanl.gov/docs/new_html/Image228.gif">

Other versions of the **`regnpts`** are appropriate for cylindrical and
spherical geometries. For cylindrical geometries the **`createpts`**
command specifies points in a cylindrical shell outside the volume.

The **`regnpts`** command specifies a line (usually the cylinder axis), and
the rays are constructed normal to this line and containing one of the
**createpts** points. For spherical geometries the **`createpts`** command
specifies points in a spherical shell outside the volume. The **regnpts**
command specifies a point (usually the center of the sphere)
from which rays are constructed to the **`createpts`** points.

----------------------------

### 7. Connect the points into tetrahedra

The mesh designer may use the following set of command to connect the
points into a tetrahedral mesh:

    # eliminate coincident or nearly coincident points
    # 1,0,0 means consider all points
    filter/1,0,0/
    
    # rayend points are set to invisible (21 is the code for invisible)
    # they were used as end points of the rays in the regnpts command
    cmo/setall//itp/pset,get,rayend/21/
    
    # assign material colors to the points
    # identify points that are on material interfaces
    # identify constrained points
    setpts
    
    # connect the points into a Delaunay tetrahedral mesh
    # do not connect across material interfaces -
    # add points if necessary to resolve material interfaces
    connect
    
    # set element (tetrahedral) type
    settets
    
    # dump mesh to some output form
    dump/gmv/filename
    
    # terminate processing
    finish

<img height="300" width="300" src="https://lanl.github.io/LaGriT/assets/images/Image229.gif">

---------------------------------------

### Full Tutorial Code:

    # create a 3D tetrahedral mesh object and name it *3dmesh*
    cmo/create/3dmesh/
    
    # unit cube
    surface/cube/reflect/box/0.0,0.0,0.0/1.0,1.0,1.0/
    
    # define z=.5 plane as interface
    surface/cutplane/intrface/plane/0.,0.,.5/1.,0.,.5/1.,1.,.5/
    
    # define geometric regions
    region/top/ le cube and gt cutplane/
    region/bottom/ le cube and le cutplane /
    
    # define material regions
    mregion/mattop/ le cube and gt cutplane /
    mregion/matbot/ le cube and lt cutplane /
    
    # create 25 points (5x5x1) in a plane above the unit cube
    # place points on the boundaries in the x and y directions (1,1,0)
    createpts/xyz/5,5,1/0.,0.,1.1/1.,1.,1.1/1,1,0/
    
    # give the points defined by the createpts command the name, rayend
    pset/rayend/seq/1,0,0/
    
    # create rays between points in rayend and the plane below the cube
    # distribute 3 points along these rays in the region top
    # add one point at the upper external boundary for each ray
    regnpts/top/3/pset,get,rayend/xyz/0.,0.,-.1/0.,1.,-.1/1.,1.,-.1/0,0/
    
    # distribute 4 points along these rays in the region *bottom*
    # add one point at the lower external boundary for each ray
    # add one point at the material interface for each ray since
    # *bottom* contains the interface - a total of 5 points for each ray.
    # points will be distributed such that the ratio of distances between
    # any two consecutive pairs of points is 0.6 traveling from the source
    # of the ray (the plane) to the ray end.
    
    regnpts/bottom/4/pset,get,rayend/xyz/0.,0.,-.1/0.,1.,-.1/1.,1.,-.1/1,.6/
    
    # eliminate coincident or nearly coincident points
    # 1,0,0 means consider all points
    filter/1,0,0/
    
    # rayend points are set to invisible (21 is the code for invisible)
    # they were used as end points of the rays in the regnpts command
    cmo/setall//itp/pset,get,rayend/21/
    
    # assign material colors to the points
    # identify points that are on material interfaces
    # identify constrained points
    setpts
    
    # connect the points into a Delaunay tetrahedral mesh
    # do not connect across material interfaces -
    # add points if necessary to resolve material interfaces
    connect
    
    # set element (tetrahedral) type
    settets
    
    # dump mesh to some output form
    dump/gmv/filename
    
    # terminate processing
    finish

<img height="300" width="300" src="https://lanl.github.io/LaGriT/assets/images/Image225.gif">
