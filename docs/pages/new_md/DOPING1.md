---
GENERATOR: 'Mozilla/4.51C-SGI [en] (X11; I; IRIX 6.5 IP32) [Netscape]'
Generator: Microsoft Word 98
title: DOPING
---

 

 **DOPING**

  Interpolates between mesh object attributes or assigns values to a
  mesh object attribute.

  Options **constant** and **gaussian** assign values to a mesh object
  attribute.  Options **table**, **integer1**, and **integer2**
  interpolate from a reference mesh object.

  The **constant** option assigns a constant value to all specified
  nodes.

  The **gaussian** option creates a very special gaussian distribution
  around a line or point.  The bounding box (x1,y1,z1) to (x2,y2,z2)
  specifies where the peak concentration will be, Note: y2 is ignored;
  if z1=z2 then the distribution will be around a point.  All
  coordinates are assumed to be given as Cartesian, **xyz** is
  required.  The value assigned to the attribute is determined by the
  Gaussian distribution:
 
   value = concentration 
* exp(-(L/std\_dev)
*
*2)
 
  where L is the effective distance and can be represented as:
 
   L = sqrt( dy
*
*2 + (1/lateral\_diffusion)
*(dx
*
*2 + dz
*
*2) )
 
  and where
 
   dy = y-y1 (y2 ignored)

   dx = x-x1 if x &lt; x1 &lt; x2

   = 0 if x1 &lt; x &lt; x2

   = x-x2 if x1 &lt; x2 &lt; x

   dz similar to dx.
 
  The **table** option interpolates an attribute from a reference mesh
  object and reference attribute onto the current mesh object using
  **linear**, **log** or **asinh** interpolation (the default is
  linear).  In the case of 2D tabular interpolation, additional
  arguments specify the planar correspondence for the interpolation:
  geom\_out and geom\_ref refer to the output and reference
  orientation of a 2D axial distribution and may take the values,
  **xy, yz, xz**,  .
 
  In all cases, field\_out specifies the name of the attribute,
  ifirst, ilast, istride specify a point set restriction, and **set
  add or subtract** indicate if the calculated or input-value is added
  to, subtracted from or used to set the existing node attribute
  value.
 
  If the values to be doped (interpolated) are integers (options
  **integer1** and **integer2**), doping works in two ways.  For
  integer doping, only the **set** option is implemented.

  If the second field is **integer1**, the new nodal attributes are
  based on element material types. Set field\_out and attr\_ref to
  **imt1** in this case. The **integer1** option is implemented only
  for setting node material (**imt1**). The **imt1** values of the
  active mesh object nodes will be set by determining which element in
  the reference mesh object the node falls in. This element's material
  (**itetclr**) value is then assigned to the node **imt1**.
 
  The **integer2** option sets node based attributes in the active
  mesh object by determining which voronoi cell in the reference mesh
  object the node falls in. Then the value for the node corresponding
  to this voronoi cell is copied to the active node.

  If the second field is **integer2**, the new nodal attributes are
  based on the table attribute types using the Voronoi cells around
  the table nodes.

  For integer doping, function can be **min** or **max** to choose
  what happens if a cmo\_out node falls on a boundary between two
  elements or Voronoi cells. For 3d, Voronoi cell based doping,
  function can also be **minp** or **maxp** which makes any cmo\_out
  nodes that fall outside the cmo\_table geometry set to the maximum
  number of materials plus one. Mapset can be set to **create**,
  **use**, or left blank. If **create** is used then an idop attribute
  is formed that maps the cmo\_table nodes to the cmo\_out nodes. If
  **use** is used, doping will read and use this previously formed and
  saved mapping. Note that doping of integers should be done without
  child/parent relationships. If parents exist, the doping results are
  unpredictable at interface boundaries because the value of parent
  nodes are unpredictable there.
 
  **FORMAT:**
 
   **doping****/constant**/field\_out**/setaddsub**/
   ifirst,ilast,istride/value

   **doping/gaussian**/field\_out**/setaddsub**/
   ifirst,ilast,istride/

   **xyz**/x1,y1,z1/x2,y2,z2/lateral\_diffusion/
   concentration/standard\_deviation/

   **doping/table**/field\_out**/setaddsub**/cmo\_ref/attr\_ref/[**linearlog****asinh**]

   **doping/table**/field\_out**/setaddsub**/cmo\_ref/attr\_ref/[**linearlog****asinh**]/
   [geom\_out/geom\_ref]

   **doping/integer1****/imt1****/set**/ifirst,ilast,istride/cmo\_ref
   **/imt1****/min****max**

   **doping/integer2**/field\_out2**/set**/ifirst,ilast,istride/cmo\_ref/attr\_ref/

   **min****max****minp****maxp**/[**create****use**]
 
  EXAMPLE:
 
   **doping****/constant**/density**/set****/pset,get**,mypset/9.73

   For the current mesh object, the value of the attribute density
   will be set to 9.73

   for all nodes in the point set mypset.

   **doping/gaussian**/density**/add****/pset**,mypset**/xyz**/

   0.0,0.5,0.1/0.5,0.5,0.4/0.5/5.0e+18/0.225

   For the current mesh object, for nodes in mypset, the value of
   the

   attribute density will be augmented by

   the value of the distribution as defined above.

   **doping****/table**/my\_field**/set**/1,0,0/cmo\_ref/attr\_ref**/log**

   For the current mesh object, the value of the attribute my\_field
   will be set by

   interpolating from the reference mesh object and attribute.

   **doping****/table**/Saturation
   **/set**/1,0,0/cmo\_course/saturation\_course**/linear****/zx****/yx**/

   In this case the yx plane from the reference cmo is interpolated
   onto the zx plane of the

   current mesh object

   **doping/integer1**/imt1**/set**/1,0,0/cmo\_old/imt1**/min**

   See which element of cmo\_old each node of the current mesh object
   falls in, and set the imt1 attribute value to the itetclr of the
   element in cmo\_old.  If the node falls in more than one element
   use the smallest itetclr.

   **doping/integer2**/rad2**/set**/1,0,0/cmo\_old/rad1**/min/create**

   Create the voronoi cells around the nodes in cmo\_old.  See which
   voronoi cell the nodes in the current mesh object fall in and set
   the value of the attribute rad2 from the value of the attribute
   rad1 in the reference mesh object.  If there is a conflict use the
   smallest value.  Create a new attribute called idop as explained
   above.
