
 **BOUNDARY**

  The boundary routine operates on the current mesh object. For the
  nodes lying on the specified surface(s), it sets the specified node
  based attribute to the specified value. Optionally boundary will
  call the user supplied subroutine set\_user\_bounds [(see IV.
  e.8)](../miscell.md)

 FORMAT:

  **boundary/dirichlet**/ attr\_name / [valueidentifier] /surface\_list

  where:

Argument | Description
--------------- | ---------------------------------------------------------------
dirichlet |  is currently unused but must be specified
attr\_name   |   is the name of the attribute to be set
value      |    is a constant, and is the value to which the attribute is set
identifier |    is a character string that will be passed to subroutine set\_user\_bounds

   surface\_list is one of:
  
**-all-** (all boundary nodes)

surface\_name**/inclusive** (all bndry nodes on surface)

surface\_name**/exclusive** (all bndry nodes ONLY on
surface)

surface\_name/ (same as exclusive)

surface\_name1/surface\_name2**/inclusive**

(all bndry nodes on the union of the surfaces)

surface\_name1/surface\_name2**/exclusive** (default)

(all bndry nodes ONLY on the intersection of the
surfaces)

surface\_name1/surface\_name2/ surface\_name3/.... (same
as exclusive)

 

 EXAMPLES:

  **boundary** **/dirichlet**/vd\_v/7.0**/-all-**/

  sets the attribute vd\_v for all boundary nodes to be 7.0

  **boundary** **/dirichlet**/vi\_s/8.0/pbot/

  **boundary** **/dirichlet**/vd\_v/9.0/pbot**/inclusive**/

  sets the attribute vd\_v for the nodes that are on the surface pbot
  to be 9.0

  **boundary** **/dirichlet**/vd\_s/13.0/pfrt

  sets the attribute vd\_s for the nodes that are on the union of the
  surfaces pfrt and prgt to 13.0

  **boundary** **/dirichlet**/vi\_t/12.0/prgt/

  **boundary** **/dirichlet**/bconds/top\_plane/s1,s2,s3/

  will pass the set of nodes on the intersection of surfaces s1,s2 and
  s3 along with the string top-plane to subroutine set\_user\_bounds.
