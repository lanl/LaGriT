
**2. Define an enclosing volume**

Define an enclosing volume using the [**surface**](commands/SURFACE.md)
 command. Since we are defining an exterior boundary, the boundary type
is **reflect**. The next item of information needed is the geometry of
the volume; some common geometry types are **box**, **cylinder**,
**sphere.** Geometry types, **box** and **sphere,** define closed
volumes; whereas a **cylinder** is open on both ends and must be capped
by planes**.** Along with the geometry type, the extent of the volume is
defined by specifying for the box its corners, or for the cylinder its
radius and end point of its axis of rotation. The enclosing volume must
be convex. Complicated enclosing volumes can be described by their
bounding surfaces including planes and sheets . Some simple examples of
enclosing volumes are:

**Unit cube**:

**surface**/cube**/reflect** **/box**/0.0,0.0,0.0/1.0,1.0,1.0

<img height="300" width="300" src="https://lanl.github.io/LaGriT/assets/images/Image219.gif">"237" "212"

**Cylinder whose axis is the x axis with radius 1 and height 1**:

**surface/cyl\_vol/reflect/cylinder**/0.,0.,0./1.,0.,0./1.

**surface/end1/reflect/plane**/0.,0.,0./0.,0.,1./0.,1.,1.

**surface/end2/reflect/plane**/1.,0.,0./1.,0.,1./1.,1.,1.

<img height="300" width="300" src="https://lanl.github.io/LaGriT/assets/images/Image220.gif">
<img height="300" width="300" src="https://lanl.github.io/LaGriT/assets/images/Image221.gif">
