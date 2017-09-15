---
Author: Jan Wills
GENERATOR: 'Mozilla/4. [en] (X11; U; IRIX 6.5 IP32) [Netscape]'
---

 
**create**/mo\_name/[npoints/nelements/
**tethexpripyrtriquahybtriplane**/]
Creates a new Mesh Object 'mo\_name', which becomes the Current Mesh
Object.

If a Mesh is created using the first (mesh\_type) format, then values
are supplied for the other parameters as follows:

 
  ---------------- ------------ ------------- ------------ ------------ ------------
  mesh             ndimension   ndimension    nodes\_per   faces\_per   edges\_per
  name             geom         topo          element      element      element

  **tet**          3            3             4            4            6
  **hex**          3            3             8            6            12
  **pri**(sm)      3            3             6            5            9
  **pyr**(amid)    3            3             5            5            8
  **tri**(angle)   3            2             3            3            3
  **qua**(d)       3            2             4            4            4
  **hyb**(rid)     3            3             10           10           12
  **lin(e)**       3            1             2            2            1
  **triplane**     2            2             3            3            3
  ---------------- ------------ ------------- ------------ ------------ ------------

If mo\_name exists nothing happens.

mo\_name required.
Note:  If values are supplied for npoints and/or nelements space is
allocated, but values are not entered for the mesh object attributes:
**nnodes** and **nelements**

 
**EXAMPLES:**
**cmo/create**/mo\_tet2

**cmo/create**/mo\_tet2/0/0**/hex**
