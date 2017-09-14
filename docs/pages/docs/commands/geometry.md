---
Author: Jan Wills
GENERATOR: 'Mozilla/4.7 [en] (X11; I; IRIX 6.5 IP32) [Netscape]'
---

 **GEOMETRY**

  **geometry/create** /geom\_name
 
  Initialize a geometry called geom\_name. Change the name of the
  current geometry to geom\_name. Save all values associated with the
  previous geometry.  To associate this geometry with a mesh object
  use the [cmo/geometry](cmo/cmo_geom.md) command.  See III.E for a
  discussion of geometry.
 
  **geometry/release** /geom\_name
 
  Release all data structures related to geometry, geom\_name and
  remove geom\_name from the list of geometries.

 **EXAMPLES:**

  **geometry/create**/new\_geom/

  **geometry/releas**e/old\_geom/

   

   

 
