---
GENERATOR: 'Mozilla/4.79C-SGI [en] (X11; U; IRIX64 6.5 IP30) [Netscape]'
Generator: Microsoft Word 98
title: 7
---

**Miscellaneous**

 setsize

  The subroutine **setsize** sets the mesh object attriburtes: xmin,
  xmax, ymin, ymax, zmin, zmax, epsilona, epsilonv
 
   **setsize**( )

   xmin, xmax, ymin, ymax, zmin, zmax are set from the minimum and
   maximum xic,yic,zic values of all 'real' points (dudded and merged
   points will be ignored).

   epsilona is set to :
   ((xmax-xmin)
*
*2+(ymax-ymin)
*
*2+(zmax-zmin)
*
*2)
   
*epsilonr
*1000.

   epsilonv is set to :
   abs(xmax-xmin)
*abs(ymax-ymin)
*abs(zmax-zmin) 
*epsilonr
*1000.

 getsize

  The subroutine **getsize** returns the mesh object attributes:  
  xmin, xmax, ymin, ymax, zmin, zmax, epsilona, epsilonv

  **getsize**(xmin,xmax,ymin,ymax,zmin,zmax,epsilona,epsilonv)

 set\_user\_bounds

  This routine allows the user to set boundary values. [See the
  boundary command.](commands/BOUNDAR1.md)
  **set\_user\_bounds**(**nubndpts,ubndpts,cmo,ipattr, idfield**)

  **nubndpts** - number of boundary nodes

  **ubndpt** - integer array of boundary node indices

  **cmo** - a mesh object name

  **ipattr** - pointer to mesh object attribute to contain boundary
  values

  **idfield** - identifier used to identify the set of boundary nodes.

 inside routines

  The **inside** set of subroutines test whether a query point is
  strictly inside, strictly outside or on the surface of the specified
  element.  The value returned in iflag is 0 if the query point is
  inside the element, -1 if outside, or is set to the local face
  number containing the query point.  Coordinates of the query point
  are in xq, yq, zq.  Coordinates of the vertices of the element are
  in x1, y1, z1, x2.....  The coordinates of these vertices of the
  element must be specified in the correct order ([see the section on
  Mesh Object Connectivity)](meshobjcon.md).  For triangular
  elements both **inside\_tri** and **inside\_tri2d ** must be called;
  the first call will determine if the query point is in the plane of
  the triangle;  the second, if it is in the interior of the
  triangle.  Similar calls must be made for planar quad meshes.
 
  **inside\_pyr**(x1,y1,z1,x2,y2,z2,x3,y3,z3,x4,y4,z4,x5,y5,z5,xq,yq,zq,iflag)

  **inside\_pri**(x1,y1,z1,x2,y2,z2,x3,y3,z3,x4,y4,z4,x5,y5,z5,x6,y6,z6,xq,yq,zq,iflag)

  **inside\_hex**(x1,y1,z1,x2,y2,z2,x3,y3,z3,x4,y4,z4,x5,y5,z5,x6,y6,z6,x7,y7,z7,x8,y8,z8,

  xq,yq,zq,iflag)

  **inside\_tet**(x1,y1,z1,x2,y2,z2,x3,y3,z3,x4,y4,z4,xq,yq,zq,iflag)

  **inside\_quad**2d(x1,y1,z1,x2,y2,z2,x3,y3,z3,x4,y4,z4,xq,yq,zq,iflag)

  **inside\_tri2**d(x1,y1,z1,x2,y2,z2,x3,y3,z3,xq,yq,zq,iflag)
 
  These routines should not be confused with

  **inside\_quad**(x1,y1,z1,x2,y2,z2,x3,y3,z3,x4,y4,z4,xq,yq,zq,iflag)

  **inside\_tri**(x1,y1,z1,x2,y2,z2,x3,y3,z3,xq,yq,zq,iflag)

  which return in iflag: 1 if the query point is on the plane of the
  element, 0 if below the plane and -1 if above.
 
  The generic subroutine **inside\_element** (element\_type, xcoords,
  ycoords, zcoords, xq,yq,zq,iflag) which returns iflag = -1 if the
  query point (xq,yq,zq) is outside the element, 0 if it is inside, 1
  &lt; n &lt; 6 where n is the face of the element on which the query
  point sits.  element\_type is an integer, 1 = point, 2 = line, 3=
  triangle, 4= quad, 5=tet, 6= pyramid, 7= prism, 8=hex).  The x,y,z
  coordinates of the vertices of the element are given in xcoords,
  ycoords, zcoords.  The order of the vertices is important.

   

 volume\_element

  return the volume of an element.
 
  **volume\_element** (ielmtyp,xicvol,yicvol,zicvol,volelm)
 
  **ilemtyp** is the element type (input, usually extracted from the
  ittettyp attribute)

  **xicvol, yicvol, zicvol** are arrays of the coordinates of the
  vertices of the element (input)

  **volelm** is the volume of the element (output)

 user\_interpolate

 **       
 user\_interpolate**(cmo\_sink,cmo\_src,cmolength,cname,nlist,list,ierror\_return)

  Supplying this subroutine and linking it in when building the
  executable allows the user to supply the interpolation formula for
  an attribute whose interpolation has been set to **user**.  By
  default several mesh object attributes have interpolation type
  **user,** for example, isn1, itetoff ( there is no obvious way to
  assign a correct value to new nodes or elements based on the values
  of old elements and other parts of the LaGriT code take care of
  assigning correct values).  Therefore if a user attribute is created
  with interpolation type **user** and the subroutine
  user\_interpolate is supplied, this routine must check the **cname**
  argument to make sure it matches the name of the user attribute.
  (e.g.  if the user attribute is called my\_attribute then the
  routine must contain a statement such as:

           if(cname.eq.'my\_attribute') then

                  **cmo\_sink       ** is the SINK mesh object

                  **cmo\_src         ** is the SOURCE mesh object

                  **cmolength**        is "nnodes" or "nelements"

                  **cname **              is the name of the attribute

                  **nlist **                 number of items to
 interpolate

                  **list(nlist) **         the list of items for which
 new interpolated values are to be produced

                  **ierror\_return**    0 = no error

                                                &gt;1 = error

 
