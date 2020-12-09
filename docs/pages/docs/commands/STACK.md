---
title: STACK
tags: stack layers
---

# STACK


-------


The **stack/layers** command is used to read surfaces and merge into a
 single stacked layers cmo. The surfaces must have the same number of
 nodes, and the x,y coordinates of each layer must be the same. (i.e.
 x_n,y_n of surface M must equal x_n,y_n of surface N), and the
 surfaces must be single valued functions of z. 

The first file listed is bottom, last file is the top surface.

The lower elevation of each layer truncates the upper. The second from
 last surface can be made to truncate all lower surfaces, commonly done
 with a topographic surface.

 

Three scalar attributes are added and used later by the **`stack/fill`** command.
- **nlayers** are the total number of layers in the stacked cmo, this includes layers added
 for refinement. nnperlayer are the number of nodes in each of the
 layers. 
- **neperlayer** are the number of elements in each of the layers.
- **layertyp** is created to indicate what type of layer each node is in.
  Value -1 is bottom,  -2 is top, 0 is the surfaces read at input, 1 is a derived interface buffer, 2 is a derived refinement layer.


## SYNTAX

<pre>
cmo/create/ cmo_stack
<b>stack/layers</b> / [<b>avs</b> or <b>gmv</b>] [minx,miny, maxx,maxy] / & 
  file_bot(1) [matnum] &
  file_lay(i) [matnum, refnum] &
  file_top(n) [matnum, refnum] &
  [ <b>flip</b> ] [ <b>buffer</b> [xdistance] ] [ <b>pinch</b> xthick ] <b>trunc</b> [ifile_no] [ <b>dpinch</b> xvalue / <b>dmin</b> xvalue ] 

<b>stack/fill</b> / cmo_3D / cmo_stack
</pre>

### Required Options

The stacked layers are read into the current mesh object. Create and name the stacked mesh object with **cmo/create**/cmo_stack before command **`stack/layers`**.


**`stack/fill`** command creates a new mesh object `cmo_3D` from the stacked mesh `cmo_stack`.
The stacked 2D layers are filled to create 3D elements. For triangulated surfaces, the elements will be prisms, and for quad sheets the filler elements will be hex. 


**avs** or **gmv** are the allowable file types that can be read. These must be surfaces with element type **quad** or **tri**.


`minx, miny, maxx, maxy` is optional argument that allows a subset of the surfaces to be used. 

`file(1) ... file(n)`  is the list of files to read from bottom surface to top.
Each surface can be followed by an integer value to indicate a material
number, and an integer value to indicate the number of layers to add as
refinement between input surfaces. 


`mat_num` and `ref_num` material number and refinement count are optional arguments for each file so the file list can look like one of the following 3 syntax lines:
<pre>
    file_bottom, file2, ... file_top 
    file_bottom mat_num, file2 mat_num, ... file_top mat_num 
    file_bottom mat_num, file2 mat_num [ref_num], ... file_top mat_num [ref_num]
</pre>


`mat_num` is the material number for the unit defined by upper and lower surface. These values will detirmine the element colors when the
 layers are filled with element volumes.

`ref_num` is the number of refinement layers to add between two surfaces. Refinement is done proportionallly, creating new layers
 between the choosen surfaces. The first filename can not have a refinement number, units start at second file name. See examples
 below.


### Additional Options


**`flip`**  will flip elements so the normals point positive Z direction, no change if normals are already up direction.


**`buffer`** creates buffer layers around interfaces at a distance equal to `xvalue`. It derives layers above and
below each surface that is read in to the stack routine. Buffers are not created around refinement layers or on the top and bottom surfaces.


**`trunc`**  causes all layers below the choosen surface to be truncated. The truncating surface is indicated by
the integer `ifile_no`. For instance, 5 will truncate all layers below the 5th surface by the 5th surface. 


**`pinch`** `xthick` controls how layers are made coincident  where they cross and will also help to control the minimum thickness between layers.
The real value `xthick` is mininum thickness allowed between layers. This allows upper surface elevations to be equal to lower surface
 elevation if the upper surface dips below lower surface. (default 0)


**`dpinch`** `dvalue` **dmin** `mvalue` These options are used along with buffers to help elements to follow the interface boundarys. These
 options differ from the simple **pinch** option and uses the beads_ona_ring algrithm to move points vertically after all the layers are stacked.
<br>
The algorithm follows these rules:
```
 If layer thickness <= dvalue then thickness is set to zero.

 If layer thickness is < dvalue < mvalue, set thickness to mvalue.

 (default dvalue = mvalue = 0.0, no post processing)
```

<hr>

## EXAMPLES

```
cmo/create/cmo_stack
stack/layers/avs/ fsrf5.inp 1/ fsrf09.inp 2/ fsrf44.inp 2 /flip/pinch 1.0
stack/fill/ mo_prism / cmo_stack
```
This command will read 3 triangulated surface files, flip the normal from down to up, and pinch layers less than 1.0 meter apart. When
 converted to a 3D grid, this mesh  will be two prism elements thick in the z direction.
<br>
 A surface is assigned the material value that occurs with it on the command line. When the surfaces are filled with volumes, the nodes on
 the bottom surface will detirmine the material of volume elements on and above that surface. So nodes on fsrf5.inp and above will all
 have imt values of 1. Nodes on fsrf09.inp and above will have imt equal to 2.
<br>
The prism mesh will have a bottom layer of material 1 and a top layer of material 2.

 
```
cmo/create/cmo_stack
stack/layers/avs / fsrf5.inp 1/ fsrf09.inp 2/fsrf44.inp 2/ &
     flip / buffer 3.0 / dpinch 1.0 / dmin 3.0
```

Three surfaces are read and buffer layers are added at 3 meters below and 3 meters above the unit interface fsrf09.inp. The units are
 pinched at anything less than 1 meter and the mininum distance to next layer is 3 meters.

 
```
cmo create cmo_stack

stack/layers/avs/ &
  surf-12.inp 1   &
  surf-5.inp  2 3 &
  surf5.inp   3   &
  surf2_slope.inp 4 &
  surf25.inp  4 1 / trunc 4 / pinch 0.

stack/fill/mohex/cmo_stack

hextotet//motet/cmohex
```
This command reads a list of quad surfaces and assigns material values 1 through 4. The first thickness (between surf-12.inp and surf-5.inp) is
refined by 3, so that 3 layers are added between these file surfaces. All materials will be 1 in this refined unit. 
The next two units, material 2 and 3, will have no refinement layers added. The last unit is
 refined once, with a layer between the file surfaces surf2_slope.inp and surf25.inp.
<br>
 The fill option will fill the space between quad surfaces with hex elements. This hex grid will have 4 units and 10 layers.
<br>
 The hextotet command can be used to convert the hex grid to a tet grid. Note that the second option to hextotet is defaulted. This
 allows hextotet to check on the grid's mesh type and use the appropriate tet conversion. There will be 6 tet from each hex and
 there are 3 tets from each prism.


### LINKS

 [Simple Examples for stack](../stack_demo.md)

 [Advanced Examples for stack](../stack_demo2.md)

