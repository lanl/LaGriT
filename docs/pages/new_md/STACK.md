---
GENERATOR: 'Mozilla/4. [en] (X11; U; IRIX 6.5 IP32) [Netscape]'
Generator: Microsoft Word 98
title: STACK
---

 

STACK
-----

 The **stack/layers** command is used to read surfaces and merge into a
 single stacked layers cmo. The surfaces must have the same number of
 nodes, and the x,y coordinates of each layer must be the same. (i.e.
 x\_n,y\_n of surface M must equal x\_n,y\_n of surface N), and the
 surfaces must be single valued functions of z. The layers are stacked
 with the first file on the bottom, last file is the top surface. The
 lower elevation of each layer truncates the upper. The second from
 last surface can be made to truncate all lower surfaces, commonly done
 with a topographic surface.

 

 The **stack/layers** command adds three scalar attributes that are
 used in the **stack/fill** command. See syntax below. nlayers are the
 total number of layers in the stacked cmo, this includes layers added
 for refinement. nnperlayer are the number of nodes in each of the
 layers. neperlayer are the number of elements in each of the layers.

  The node attribute VINT layertyp is created to indicate what type of
  layer each node is in.

  -1 bottom surface

  -2 top surface

  0 original input surfaces (usually interfaces)

  1 derived surface to buffer interfaces

  2 derived surface added as refinement layer

 

 The **layers** option is followed by a list of surface files and their
 material numbers. Further options include the file\_type either AVS or
 GMV, layer\_refinement and pinch\_options.

 

 **stack/layers**/ file\_type / [xy\_subset] / file\_list /
 [buffer\_opt] [truncate\_opt] [pinchout\_opt] [flip\_opt]


file\_type The third parameter indicates the type of files to read as
surfaces.

 **avs** will read AVS UCD files as a surfaces.

 **gmv** will read GMV files as a surfaces.

xy\_subset This option allows a subset of the surfaces to be used. The
syntax is minx, miny, maxx, maxy


file\_list This is the list of files to read from bottom surface to top.
Each surface can be followed by an integer value to indicate a material
number, and an integer value to indicate the number of layers to add as
refinement between input surfaces. The file list has this syntax: file1
mat\_num, file2 mat\_num [ref\_num], file3 mat\_num [ref\_num] ...

 file1 thru filen can be quad or tri surface files.

 mat\_num is the material number for the unit defined by upper and
 lower surface. These values will detirmine the element colors when the
 layers are filled with element volumes.

 ref\_num is the number of refinement layers to add between two
 surfaces. Refinement is done proportionallly, creating new layers
 between the choosen surfaces. The first filename can not have a
 refinement number, units start at second file name. See examples
 below.


buffer\_option This optional parameter creates buffer layers around
interfaces at a distance equal to xvalue. It derives layers above and
below each surface that is read in to the stack routine. Buffers are not
created around refinement layers or on the top and bottom surfaces. The
buffer option has this syntax: **buffer** xvalue


truncate\_option This optional parameter causes all layers below the
choosen surface to be truncated. The truncating surface is indicated by
the integer number. For instance 5 will truncate all layers below the
5th surface by the 5th surface. **trunc** nth\_file


pinchout\_options These optional parameters control how layers are
pinched out where they cross. They will also help to control the minimum
thickness in a unit between layers.

 **pinch** xthick real value xthick is mininum thickness allowed in a
 layer. This allows upper surface elevation to be equal to ower surface
 elevation if upper surface dips below lower surface. By default,
 **pinch** is set to 0.

 **dpinch** dvalue **dmin** mvalue These options are used along with
 buffers to help elements to follow the interface boundarys. These
 options differ from the simple **pinch** option. This option uses the
 beads\_ona\_ring algrithm to move points vertically after all the
 layers are stacked.

 If layer thickness &lt;= dvalue then thickness is set to zero.

 If layer thickness is &lt; dvalue &lt; mvalue, set thickness to
 mvalue.

 (default dvalue = mvalue = 0.0, no post processing)

**flip** optional parameter will flip elements so the normals point
positive Z direction.


**stack/fill** / cmo\_3D / cmo\_stack

is a command used after the files are stacked into a single cmo. The
units are filled between the layers with 3D elements. For triangulated
surfaces, the elements will be prisms, and for quad sheets the filler
elements will be hex.



**FORMAT:**

 **stack/layers**

 **[avs  gmv ]**

 [minx,miny, maxx,maxy]

 filename(1) [matno] filename(i) [matnum, addnum] filename(n)
 [matnum, addnum] **[ flip ]**

 **[ buffer** [xdistance] ]

 **[ pinch** font face="Courier New,Courier"&gt;[xthick] ]

 **[ trunc** [ifile]

 **[ dpinch** xvalue /

 **dmin** xvalue ]

  

  


**EXAMPLES:**

 cmo create cmo\_stack

 **stack/layers/avs**/ fsrf5.inp  1/ fsrf09.inp  2/ fsrf44.inp  2
 **/flip/pinch** 1.0

 This command will read 3 triangulated surface files, flip the normal
 from down to up, and pinch layers less than 1.0 meter apart. When
 converted to a 3D grid, this will be two elements thick in the z
 direction.

 A surface is assigned the material value that occurs with it on the
 command line. When the surfaces are filled with volumes, the nodes on
 the bottom surface will detirmine the material of volume elements on
 and above that surface. So nodes on fsrf5.inp and above will all
 have imt values of 1. Nodes on fsrf09.inp and above will have imt
 equal to 2.

 

 cmo create cmo\_stack

 **stack/layers/avs** / fsrf5.inp  1/  fsrf09.inp  2/fsrf44.inp  2/
 **flip** / **buffer** 3.0 / **dpinch** 1.0 / **dmin** 3.0

 Same surfaces are used but buffer layers are added at 3 meters below
 and 3 meters above the unit interface fsrf09.inp. The units are
 pinched at anything less than 1 meter and the mininum distance to next
 layer is 3 meters.

 

 cmo create cmo\_stack

 **stack/layers/avs** / surf-12.inp  1/ surf-5.inp  2 3 /
 surf5.inp  3  / surf2\_slope.inp  4/ surf25.inp  4 1 / **truncate** 4
 / **pinch** 0.

 **stack/fill**/cmohex/cmo\_stack

 **hextotet/**/cmotet/cmohex

 

 This command reads a list of quad surfaces and assigns material values
 1 through 4. The first unit (between surf-12.inp and surf-5.inp) is
 refined  by 3, so that 3 layers are added between these surfaces. All
 materials will be 1 in this refined unit. The next two units, material
 2 and 3, will have no refinement layers added. The last unit is
 refined once, with a layer between the surfaces surf2\_slope.inp and
 surf25.inp.

 The fill option will fill the space between quad surfaces with hex
 elements. This hex grid will have 4 units and 10 layers.

 The hextotet command can be used to convert the hex grid to a tet
 grid. Note that the second option to hextotet is defaulted. This
 allows hextotet to check on the grid's mesh type and use the
 appropriate tet conversion. There will be 6 tet from each hex and
 there are 3 tets from each prism.


LINKS:

 [Simple Examples for stack](stack_demo.md)

 

 [Advanced Examples for stack](stack_demo2.md)
