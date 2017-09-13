Notes on connect algorithm:

Connect uses the standard point insertion method of tetrahedralization
which consists of the following steps:

 First an enclosing tetrahedra is constructed which contains the nodes
 to be tetrahedralized.

 Then the nodes are processed in a random order and inserted one at a
 time.

 To insert node n+1:

  Find a tetrahedra whose circumsphere contains node n+1.  Place this
  tetrahedron on the stack.  Pop a tetrahedron off the stack.  Look at
  all face neighbors of this containing tetrahedron to determine if
  their circumspheres contain node n+1.  Place the containing neighbor
  tetrahedra onto the stack.  Continue until the stack is empty.  Keep
  track of the set of containing tetrahedra as the stack is depleted. 
  The union of these tetrahedra is the insertion cavity into which the
  new node is placed; so we remove the containing tetrahedra and
  construct new tetrahedra by connecting the new node to the faces of
  the cavity.

  If for some reason a node cannot be inserted, it is placed in a fail
  list and subsequent passes will attempt to insert the node by using
  looser 'inside' tests.

 When all nodes are inserted, we inspect all edges to see if there are
 any edges that cross material interfaces. If so we construct a node at
 the intersection of the edge and interface.  These new nodes (if any)
 are then inserted into the mesh using the algorithm described above. 
 This process is repeated until no more 'multi-material' edges are
 encountered or until the maximum number of iterations is reached. 
 This step is skipped if the **noadd** option has been specified.

 Finally the initial enclosing tetrahedron vertices and the attached
 tetrahedra are removed.

 To determine if a node is 'inside' a tetrahedron's circumsphere we
 compare the distance from the tetrahedron's voronoi point to a vertex
 of the tetrahedron (i.e. the circumsphere radius) with the distance
 from the voronoi point to the node in question.  If the second
 distance is less than the first, the node is 'inside' the
 circumsphere.  Obviously the comparison of these distances should
 involve an epsilon which is dependent upon both the machine accuracy
 and the problem scale.  For computational efficiency we compare the
 distances squared.

 Currently the 'inside' test is as follows:

  Since we are comparing distances squared the appropriate test is an
  area test.  We construct a variable called **smalarea** =

  [ ((machine
  precision)
*10,000)
*
*(boxsizex
*boxsizey
*boxsizez)
*
*(2.d0/3.d0)]

  where boxsize is the (max coordinate value - min coordinate value)
  in each of the 3 directions,

  machine precision is usually around 2
*10-16 for double precision.

  So for a unit cube the test value is about 7
*10-11.

  For a 1000x1000x1000 cube the test value is about .07.

  For the first pass we use smalarea
*100.

  We loop until no more nodes can be added using this value.

  For the second pass we use smalarea

  For the third pass smalarea/100.

  For the fourth pass we use zero

  For the fifth pass we use -100
*smalarea

 The inside test requires that the coordinates of the circumcenter of
 the new tetrahedra be calculated.  As this calculation uses the
 squares of the coordinates of the vertices, we first translate the
 tetrahedron so that the coordinates one of its vertices lie at zero. 
 This simplifies the calculation and avoids loss of precision when the
 values of the coordinates are very large.  After calculating the
 coordinates of the circumcenter, the tetrahedron and its circumcenter
 are translated back to their original location.

 As the insertion cavity is filled with new tetrahedra, the new
 tetrahedra are subjected to two tests.  The first test checks that the
 volume of each new tetrahedron is positive.  This test uses a volume
 epsilon that represents the smallest volume that can be handled
 computationally.  We use the mesh object attribute **epsilonv** which
 is set to [ ((machine
 precision)
*1000)
*
*(boxsizex
*boxsizey
*boxsizez)].  The second
 test compares the sum of the volumes of the new tetrahedra to the sum
 of the volumes of the removed tetrahedra.  This test fails if: [ 
 (volnewt-vololdt)/vololdt  &gt; machine precision
*10
*
*8].

 The user can change the 'tightness' of the circumsphere test by adding
 an attribute to the mesh object called **circumsphere\_factor** and
 setting this factor, e.g.:

 **cmo****/addatt**/**/circumsphere\_factor****/REAL****/scalar****/scalar**

 **cmo****/setatt**/**/circumsphere\_factor**/1,0,0/.125

 This factor will only be required in extreme circumstances.  If a node
 distribution has an extremely high aspect ratio, the user might see
 warning messages about circumsphere problems and connect may fail to
 connect all nodes.  In this case one might try adjusting the
 circumsphere\_factor.

The requirement for a delaunay tessellation is that the circumcircle or
circumsphere of each element enclose no other nodes in the
tessellation.  If nodes on the boundary of the mesh are nearly coplanar
or nearly collinear, it is possible that the big tetrahedron or big
triangle constructed automatically will not be large enough.  For
illustration purposes consider the 2D case in which there are 3 nodes on
the boundary that are nearly collinear say (0,0,0) (.5,.05,0.) and
(1,0,0), then the circumcircle determined by these three nodes is very
large and may contain node(s) of the big triangle, and the triangle
consisting of these three nodes will not be formed.   In this case it is
necessary for the user to supply the coordinates of the 'big' triangle
in the connect command.
