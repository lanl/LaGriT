
HEXTOTET
--------

IMPORTANT NOTE: This command, while still recognized, is officially unsupported. Please see [grid2grid](GRID2GRID.md) instead.

 The **hextotet** command creates a tetrahedral grid from 3D grids. In
 2D the elements are converted to triangles. The first parameter
 ioption determines how the conversion is performed.

**FORMAT:**

 **hextotet**/ [ ioption ] / cmo\_snk / cmo\_src / [**rmvolume**]

ioption: is a numerical number indicating the number of tets or
triangles to break each element into. If this parameter is missing then
default settings are used. The defaults are underlined and will be
detirmined by reading the mesh\_type of the mesh\_object. If mesh\_type
is quad, **2** is used. If mesh\_type is prism, **3** is used. If
mesh\_type is hex, **6** is used. Otherwise **24** is the default value
for ioption.

The selections include:

 **2**   quad to 2 triangles, no new points.

 **3**   prism to 3 tets, no new points.

 **4**   quad to 4 triangles, with one new point.

 **4**   pyramid to 4 tets, with one new point.

 **5**   hex to 5 tets, no new points.

 **6**   hex to 6 tets, no new points.

 **14**   prism to 14 tets, four new points (1 + 3 faces).

 **18**   prism to 18 tets, six new points (1 + 5 faces).

 **24**   hex to 24 tets, seven new points (1 + 6 faces).




cmo\_snk / cmo\_src : are the mesh\_object names. cmo\_src is the
original grid. cmo\_snk is the name for the new tet or triangle grid.



**rmvolume** : keyword is optional and will assign
hextotet\_remove\_volume and hextotet\_remove\_duplicates to 'yes'. This
will enable hextotet to use its own algorithm for removing elements with
zero volume and duplicate points. It may be prone to epsilon errors for
grids over large areas. By default, zero volumes and duplicate points
are not removed from the new mesh object cmo\_snk.









**EXAMPLES:**

 **hextotet** / **24** / cmo\_tet / cmo\_hex
 Convert each hex element in cmo\_hex to 24 tets and name the new grid
 cmo\_tet.
 

 **hextotet** / / cmo\_tri / cmo\_quad
 No value is given for ioption, so the default settings are used. The
 mesh\_type of cmo\_quad is quad, so each element is converted to two
 triangles. The new mesh\_object is named cmo\_tri.
 

 **hextotet** / **3** / cmo\_tet / cmo\_pri / **rmvolume**
 Each prism element in cmo\_pri is converted to three tet elements.
 Zero volume elements and duplicate points are removed. The new tet
 mesh object is called cmo\_tet.

LINKS:

 [Click here for demos](../demos/main_hextet.md)
