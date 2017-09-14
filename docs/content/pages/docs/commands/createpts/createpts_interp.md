---
GENERATOR: 'Mozilla/4.7 [en] (X11; I; IRIX 6.5 IP32) [Netscape]'
Generator: Microsoft Word 98
title: 'createpts/interp'
---

 

 **CREATEPTS/interp/npoint/i1 i2 i3/j1 j2 j3/cmo\_new**

  This command adds points to a mesh object. It creates points by
  linear interpolation of coordinates between two point sets. For some
  special cases, it will also produce element connectivity between the
  point sets.

  

  The combinations of input and output are:

  

    ------------------------ --------------------------
    **Source Element Type
   **Result Element Type

    **                       **
 
    point
                   point (no connectivity)

 
    line
                    quad

 
    tri
                     prism

 
    quad
                    hex

 
    hybrid (quad,tri)
       hybrid (hex, prism)

    ------------------------ --------------------------
 
 **FORMAT:**

 reatepts** **/interp/npoint/i1,i2,i3/j1,j2,j3/cmo\_new**

 


 **npoint ** specifies the number of points to create between the
 specified point sets.**

 i1,i2,i3** specifies the point set coordinate interpolation starts
 FROM.

 **j1,j2,j3** specifies the point set coordinate interpolation goes
 TO.

 mo\_new**  is the name of the mesh object the result goes into. The
 source mesh object is the current mesh object.


 i1,i2,i3 and j1,j2,j3 can specify either a)start, stop, stride or b) 1
 0 0  for everything or c)pset get pset\_name

 

 It is up to the user to insure that the number of nodes specified by
 i1,i2,i3 is equal to the number of nodes specified by

 j1,j2,j3 .

 

 If the 1, 0, 0 format is used the code will use:

 i1 = 1, i2 = nnodes/2, i3 = 1, j1 = (nnodes/2)+1, j2 = nnodes, j3 = 1

 

   **EXAMPLES:**

  Example 1

  Create a point distribution between the two point sets created by
  the createpts/line commands

  

  createpts/line/10///-1.,0.,0.,1.,0.,0.

  createpts/line/10///0.,-1.,1.,0.,1.,1.

  createpts/ interp / 10 / 1 0 0 / 1 0 0 / cmo\_pts

  

  Example 2

  Create a hex mesh by linear interpolation between two quad surfaces.
  It is up to the user to be sure that the ordering of the quads are
  as intended. The first node of first quad surface (cmo1) is
  interpolated to the first node of the second quad surface (cmo2)

  

  cmo create cmo1///quad

  quadxy 5 5/ 0. 0. 0. / 20. 0. 0./20. 20. 0. / 0. 20. 0.

  createpts/brick/xyz/5,5,1/1,0,0/connect

  cmo create cmo2///quad

  quadxy 5 5/ 0. 0. 10. / 20. 0. 25./20. 20. 15. / 0. 20. 35.

  createpts/brick/xyz/5,5,1/1,0,0/connect

  addmesh / merge / cmo3 / cmo1 / cmo2

  cmo / select / cmo3

  createpts / interp / 6 / 1 0 0 / 1 0 0 / cmo\_hex

  

  Example 3

  Starting with cmo3 from Example 2, this will interpolate from node 1
  of cmo3 to node 49 of cmo3 and interpolate from node 2 of cmo3 to
  node 50 of cmo3.

  

  createpts / interp / 20 / 1 2 1 / 49 50 1 / cmo\_pts

  


  



