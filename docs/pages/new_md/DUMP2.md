---
GENERATOR: 'Mozilla/4.79C-SGI [en] (X11; U; IRIX64 6.5 IP30) [Netscape]'
Generator: Microsoft Word 98
title: DUMP
---

 **DUMP**

This command produces an output file from a Mesh Object. If the option
is **lagrit**, a restart dump is made; a subsequent **read/lagrit**
command will restart the code at the state at which the dump command was
issued.  The default file type is binary.  To create an ASCII formatted
gmv dump precede the dump command with this command:

**cmo****/setatt****/dumptype**=**ascii**/ or include '**ascii'** in the
command.

 NOTE:  For LaGriT versions dated after October 1999, use
**cmo****/setatt**/**/ipolydat****/no** to prevent polygon data to be
written to GMV files.

**FORMAT:**

**dump**/file\_type/file\_name/[cmo\_name]

valid file\_types are: **coord**, **zone**, **gmv**, **avs**, **chad**,
**fehm**, **datex ,std, geom** and **lagrit**

**dump** / **coord** /file\_name/[cmo\_name]

Will output a file with node list x,y,z values and element connectivity
list in FEHM format.

**dump** / **zone** /file\_name/[cmo\_name]/[**delattkeepatt**]

Will output a set of 4 files.

file\_name\_**material**.**zone** is node list for each material (imt)
value.

file\_name\_**outside**.**zone** is a node list for each of 6 (top ,
bottom, left, right, front, back) possible external boundaries for
rectangular geometries.  If **keepatt** is specified, then 6 node based
attributes are added to the mesh object.  If **delatt** is specified
mesh object is not modified.  The attribute names are **top**,
**bottom**, **left\_w**, **right\_e**, **back\_n**, **front\_s**.

Note that a node can belong to more than 1 list. For example in an
orthogonal cube aligned with the coordinate axes, a corner node can
belong to 3 lists (e.g. front\_s, top and left\_w lists).

file\_name\_**outside**.**area** is a list of the outside area
associated with each node in the file\_name\_**outside**.**zone** list. 
Areas are vector areas such that the magnitude of the vector is the
scalar area associated with the node and the direction of the vector is
the area weighted normal vector of the node.

file\_name\_**multi**\_**mat**.**zone** is a list of list of nodes for
each material that are connected by an edge to a node of a different
material. Each list consists of a header followed by list entries. The
header consists of the material number followed by "multi-material
connections" on the same line, followed by "nnum" on the next line,
followed by the number of entries in the list. (The notation nnum is a
holdback from the earlier version of this command, and really should be
nedges.)  The list entries consist of two nodes. The first node is the
one inside the material in question, the second is the node in the other
material. The lists are sorted by the first node, but are not sorted by
the second. Each list entry is on its own separate line, and the nodes
in each entry are separated by a comma.

file\_name\_**outside**.**area** is a list of 2D areas associated with
each node.

**dump** / **zone\_imt** /file\_name/[cmo\_name]

Will output only one file:

file\_name\_**material**.**zone** is node list for each material (imt)
value.

**dump** / **zone\_outside** / file\_name/[cmo\_name] /
[**delatt****keepatt**]

Will output only two files:

file\_name\_**outside**.**zone** is a node list for each of 6 possible
external boundaries.

file\_name\_**outside**.**area** is a list of 2D areas associated with
each node.

**dump** / **avs** /file\_name/[cmo\_name] / iopt\_points /
iopt\_elements / iopt\_node\_attributes/iopt\_element\_attributes

Output in AVS UCD (Unstructured Cell Data) format. One can turn on or
off the output of node coordinates (iopt\_points), element connectivity
(iopt\_elements), node attributes (iopt\_node\_attributes) and element
attributes (iopt\_element\_attributes). 1 (default) is on, 0 is off.

For file format specification see
<http://help.avs.com/Express/doc/help/reference/dvmac/UCD_Form.htm

**dump / avs2 **/
file\_name/[cmo\_name]/[iopt\_points]/iopt\_elements]/[iopt\_node\_attributes]/[iopt\_element\_attributes]**

**

This option will output integers as integers, the other avs option
converts integers to reals on output. The /avs/ option above outputs all
attributes as real numbers. This option is slower but the files are
smaller if there are integers in the node or element attributes.

**dump** / **stl** /file\_name/[cmo\_name]

Output in STL, stereo lithography format. This is only valid for
triangular sheets.

**dump/geom**/file

will write an ascii file containing the geometry information for the
current run. This information includes the region and mregion
definitions and surface, names, types and definitions.

**dump/lagrit**/file\_name/[cmo\_name]/ [**ascii**  **binary**]

will write an restart file that contains geometry and mesh object
information.  cmo\_name can be '**-all-**' in which case all mesh
objects are written to the file or it can specify a list of mesh objects
to be written.  The default is **ascii**.

**dump/recolor**/file\_name

This command writes the existing **colormap** to the specified file. 
(See **[colormap](http://lagrit.lanl.gov/COLORMAP.md)**
command)

**dump/elem\_adj\_node**/file\_name/mo\_name

Write adjacency information to an ascii file. Write list of all elements
adjacent to each node.

File format:

node\_number number\_of\_adjacent\_elem e1 e2 ... en  

**dump/elem\_adj\_elem**/file\_name/mo\_name

Write adjacency information to an ascii file. Write list of all elements
adjacent to each element.

File format:

elem\_number number\_of\_adjacent\_elem e1 e2 ... en  

**dump/gmv**/file-name/[mesh-object]/[**ascii**  **binary**]

Write a file to be read by the graphics program[GMV](http://laws.lanl.gov/XCM/gmv/GMVHome.md).  The defaults are
binary and current mesh object.
[**dump/fehm**/file\_name/[cmo\_name[**scalarvectorbotharea\_scalararea\_vectorarea\_both**]/[**delatt,keepatt**]](http://lagrit.lanl.gov/DUMP3.md)


Write out a series of files for the FEHM flow and transport code. The
file\_name is the file root name for:

file\_name.fehm ( see dump/cord/... command)

file\_name\_material.zone ( see dump/zone\_imt/... command)

file\_name\_outside.zone (see dump/zone\_outside/... command)

file\_name\_outside.area (see dump/zone\_outside/... command)

file\_name\_interface.zone ( output of FEHM zone format files of nodes
along an interface )

file\_name-multi\_mat.zone ( output of FEHM zone format files of
multi-material connections )

file\_name.stor ( output of FEHM format file with geometric coefficient
matrix, these are the Voronoi (control volume) area and volume
associated with each node and the sparce matrix structure.

**dump/geofest**/file-name/[mesh-object] Write a file to be read by
the [GeoFEST, Geophysical Finite Element Simulation
Tool](http://www.openchannelfoundation.org/projects/GeoFEST/) .  The
output file is ascii.




EXAMPLE:

**dump**/gmv/file\_name.gmv/cmo\_name/

**dump**/gmv/file\_name.gmv/cmo\_name/ascii

**dump**/lagrit/file\_name.lg/-all-/binary

**dump**/avs/file\_name.inp/cmo\_name

**dump**/avs/file\_name.inp/cmo\_name/1 0 0 0 (output only node
coordinates)

**dump**/avs/file\_name.inp/cmo\_name/1 1 0 0 (output node coordinates
and element connectivity)

**dump**/avs/file\_name.inp/cmo\_name/1 1 1 0 (output node coordinates,
element connectivity and node attributes)

**dump**/avs/file\_name.inp/cmo\_name/0 0 0 1 (output element
attributes)





[Click here for
demos](http://lagrit.lanl.gov/demos/dump/test/md/main_dump.md)









