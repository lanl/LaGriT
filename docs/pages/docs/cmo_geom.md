---
Author: Jan Wills
GENERATOR: 'Mozilla/4.7 
[en
] (X11; I; IRIX 6.5 IP32) 
[Netscape
]'
---

 **cmo/geometry**

  **cmo/geometry** /cmo\_name/geometry\_name
  Associate the geometry named geometry\_name with the mesh object
  named cmo\_name. The value of the mesh object attribute geom\_name
  will be set to geometry\_name. All geometry information will be
  updated to the geometry information of geometry\_name.  This
  includes number of surfaces, regions, material regions, current
  geometry name, and definitions of active surfaces, regions and
  material regions. cmo\_name and geometry\_name must have been
  previously created.  [See III. E](geometries.md) for a discussion
  of geometry.  The **[cmo/constraint](cmo_constraint.md)**command
  might also be required.

 **EXAMPLES:**

  **geometry/create**/blobgeom/

  **cmo/create**/cmo1

  **cmo/geometry**/cmo1/blobgeom/

  **cmo/constraints**/cmo\_sink/cmo\_src
