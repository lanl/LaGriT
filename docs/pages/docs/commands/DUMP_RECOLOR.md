**DUMP\_RECOLOR**

This command is similar to the regular [dump](DUMP2.md) command except
that the mesh object is recolored before being dumped.  There are two
options specific to this version.  If **restore** is specified  (the
default), the original itetclr and imt1 values are restored, leaving the
mesh object unaltered.  If **norestore** is specified the mesh object is
left recolored (and the original values of itetclr and imt1 lost).  If
**create** is specified (the default) then a new colormap is created and
used to recolor.  Otherwise if **existing** is specified, the existing
colormap is used to recolor the mesh object.  Three dump types are
available:  "**gmv**", "**LaGriT**" and "**avs**". iomode can be
**ascii** or **binary**; **binary** is the default.

**FORMAT:**

 **dump\_recolor/ype/file/mo/
[**restore
 norestore**
]/
[reateexisting**
] /imode

EXAMPLE:

 **dump\_recolor/gmv**/mesh.gmv**/ascii**

 Writes an ascii gmv dump to the fine mesh.gmv.  The mesh object that
 is dumped is the current mesh object recolored according to its own
 material adjacency.
 
 **dump\_recolor/gmv**/mesh.gmv**/norestore/existing**

 Recolors the current mesh object using the existing colormap and then
 writes a binary gmv dump to the fine mesh.gmv.
