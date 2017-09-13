**e. Geometries**

A geometry consists of a set of surfaces and regions that are defined in
terms of existing surfaces.  A name is associated with the geometry data
and the default name is  -defaultgeom-.  A geometry may be named using
the **[geometry/create](geom_create.md)** command which must precede
the **surface**, **region**, and **mregion** commands that define this
geometry.  A geometry may be associated with more than one mesh object. 
**[cmo/geometry](commands/cmo/cmo_geom.md)** and
**[cmo/constraint](commands/cmo/cmo_constraint.md)** command are used
to associate geometries with mesh objects.

The data for a geometry are contained in a set of memory managed arrays
whose partition name is the geometry name and in common blocks geom and
cgeom defined in the geom\_lg.h file.  This file also contains the
pointer statements that can be used to access the data arrays directly.

At present access routines to the geometry information have not been
written except for getregv which accepts as input the coordinates of a
set of query points and which returns a list of region numbers into
which each query point falls.
