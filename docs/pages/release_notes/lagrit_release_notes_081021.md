---
title: 'LaGriT V2 Release Notes'
---

## Version 2.2 Release November 2010

This is the last release before work to support 64 bit.

```
*               *    Program:  LaGriT V2.200   Linux m32      *                 
*               *    date_compile: 2010/11/22                 *      
```
### Enhancements:

- **interpolate** Changed interpolate to "find" more points on edges
this will permit nodes to find a nearest edge or point and be
"inside" the triangle for extreme small or large numbers where
epsilon values are difficult to evaluate correctly.
Note, this changed test results for interpolate, test/level01
results were updated for these improvements.

- **extract/surfmesh** Now creates attributes to hold element local face numbers of 3D input 
mesh that occur on either side of output mesh face, idface0 and idface1.
Now copies user-created node-based attributes from source.

- **dump/fehm dump/zone_outside** Changed FEHM outside area to Voronoi instead of Median
FEHM file file_name_outside.area changed to file_name_outside_vor.area
For dump/zone, added keywords keepatt_area or keepatt_voronoi which will 
compute and keep voronoi vector areas xn_varea, yn_varea, zn_varea
and keepatt_median will compute area/num nodes on face and 
keep attributes xn_area, yn_area, zn_area
The written file file_name_outside_vor.area or file_name_outside_med.area 
is a list of 2D area vectors (Ax_i,Ay_i,Az_i) associated with each node.

- **cmo/addatt/voronoi_varea** will do the same voronoi calculation on triangles
as is done with the outside area for FEHM modeling files. The call will
create node attributes xn_varea, yn_varea, zn_varea indicating face directions.

- **dump/ filename** 2 token dump for common files added to writedump.f



### These issues were fixed:

- intrp_gtg.f A bug was fixed in interpolation that would sometimes save
a node id in pt_gtg or el_gtg attributes that was not related
to the found candidate and value. This could occur where there are
multiple candidates for the source and if epsilon values are
reaching near machine limits. The test in level01/intrp_2D_sizes
was changed to capture and evaluate these issues.

- epsilon errors in *intrp_gtg.f, inside_lg.f* There are changes to interpolate using tests for finding points
that are inside or on edges or vertices of an element. The epsilon
tests have been relaxed to allow points that are "near" to be
found on edge - if within the chosen epsilon. The interpolation
has been changed to evaluate candidate points based on the
confidence of being inside the associated triangle. A result
indicating the point is inside will "win" over a candidate result
that is on edge or vertice. If idebug attribute is set to a
number of 5 or greater, there will be many more statements
written that are related to the inside triangle and epsilon
tests.

## Version 2.106 6/29/2010

This version contains the work done over the summer by Aaron Gable.
Better handling of errors and segmentation faults were added to
various pieces of the code having to do with actions involving
more than one mesh object and their user defined attributes.

### Enhancements

- **read** for 3 tokens for .inp .gmv .avs

- **cmo/attribute_union** Change two meshes so they both share the same set of attributes

- **compute / linear_transform** extrapolation from an attribute value in a surface onto every node of
a 3D mesh

- **compute/signed_distance_field** to calculate signed distance relative to above and below.

- **grid2grid** wrapper for hextotet. can also convert from octree mesh to lagrit with **tree_to_fe**

- *anothermatbld3d_wrapper.f* Create two new node vectors, ccoef, ij_ccoef
Put the negative ij coefficient value into the two nodes connected to the ij edge.
The vector ij_coef will assign the j index value to node i so that one can determine 
which edge is associated with the neative coefficient that is assigned to nodes.

- Add option to **pset/ / zone** for user specified zone id number


### These issues were fixed:

- Modified epsilons in tri2d, fixed bug in foreach part of loop

- *anothermatbld3d.c* possible accuracy improvement using Carl's changes to include TranslateTetToZero for geometric calculations


## Version 2.1 Release August 2009

This is a major update to LaGriT and the lg_util library. 
Major changes have been made in the core memory management routines to allow 
development for a 64 bit release.  These changes will be invisible to most users 
but allows better reporting of errors and memory usage for useful diagnostic information.

```         
*               *    Program:  LaGriT V2.100   Linux m32      *                 
*               *    date_compile: 2009/08/03                 *                   
```

== initlagrit.f
add call to mmverify
the new util version checks for correct pointer sizes 

== writinit.f
add m32 and m64 to Program line in lagrit header 
add intel to Program line for Mac

== cmo_addatt.f
addatt keyword vor_volume which calls
anothermatbld3d_wrapper to fill voronoi volumes

== sparseMatrix.c 
initialize list pointers to null
assign null to pointers after free
add warning messages for failure to free 

== writedump.f
declare implicit none and initialize variables
add comments to clarify the case switches
add more error checking and messages
change syntax for dump/ fehm and dump/ stor
old keywords not needed include alternate_scalar, binaryc, asciic
compression keywords are now none, coefs, graph, or all 
old syntax still works, but now code checks
for keywords after filename and cmo and sets
options for the fehm and stor routine calls 
The man pages are updated and corrected 

- dumpfehm.f
Add compress_opt to dumpfehm arguments
add comments and error checking to clarify code logic
check options and set for 2D or 3D calls to matbld
use matbld3d_stor for compress options none and coefs
use anothermatbld3d_wrapper for compress options all and graph
Note anothermatbld3d_wrapper can write only scalar coef values

- anothermatbld3d_wrapper.f
Extensive chages to error handling and messages, but not to the logic of program
This code has same logic as matbld3d - but uses linked lists instead of mmgetblk calls
Use io_type to toggle creation of attribute for voronoi volumes or to write to stor file
added extensive error checking to eliminate segmentation faults
added error check and message for every mmgetblk and mmrelblk
added calls to mmprint when mm calls fail
cleaned up variable declarations and added comments
added istatus to check for errors and completion of matrix
changed all routine messages to start with AMatbld3d_stor to distinguish from matrix built with Matbld3d_stor
added idebug options
added status report at end of routine

- matbld3d_stor.f
Extensive chages to error handling and messages, but not to the logic of program
This code uses many mmgetblk calls and about 40 percent more memory than linked list version
added extensive error checking to eliminate segmentation faults
added error check and message for every mmgetblk and mmrelblk
added calls to mmprint when mm calls fail
cleaned up variable declarations and added comments
added istatus to check for errors and completion of matrix
added idebug options
added status report at end of routine

- matbld1.f
This routine is called  by matbld3d_stor
added error check and message for every mmgetblk and mmrelblk
added calls to mmprint when mm calls fail

###These issues were fixed:

- readatt not working correctly for psets

- *blockcom.h* fix for compile on MAC OS X Absoft on intel comment out ntetmax/0/ and let compiler initialize because we have 2 instances of pointer sizes. See kfix and xfix in neibor.h 


-------------------------------------
## Version 2.004 10/21/2008


### Enhancements:

-  resetpts bug fix, boundary_components and extract_surfmesh added features
-  boundary_components: added id_numb boundary index number
-  extract_surfmesh: added attributes idelem0, idelem1, idnode0 and removed attribute map- Made changes so that filter and rmmat return with no action rather than crash when passed an empty mesh object.
- Add options dump/att_node and dump/att_elem
- Add options dump/att_node and dump/att_elem to output tables of either node attributes or element attributes to ascii files with header lines beginning with character \#. 
These are similar to *dump/avs/file/mo/0 0 2 0* or *dump/avs/file/mo/0 0 0 2* except the addition of \# character to start lines. Now these files can be read in without editing using *cmo / readatt /* workflow.
- Added character # for comment lines

### These issues were fixed:
- resetpts had a bug for some element types
- Corrected bug: cmo_exist returned error flag but check of error flag was to wrong error flag variable.
- Made changes so that filter and rmmat return with no action rather than crash when passed an empty mesh object.

---------

## Version 2.003 05/20/08

Compile and test V2.003 for platforms SGI-32, SUN, MAC, LINUX

### These issues were fixed:

- These include fixes to SGI compile errors in dumpavs.f filter.f refine_tet_add.f lagrit*.h writinit.f and the Makefile in src
- Correct minor bugs. Test case now works. sphere1.f had an incorrect attempt to use MO name before the name had been obtained. Code would crash.
- offsetsurf.f did not handle problems with non-triangle or line type MO. Instead of kicking out, an attempt was made to compute sythetic normal for a mesh object (such as quad) and this caused code to crash.
- Fixed error that occured when all output attributes were turned off.
- Fixed error that occured when all output attributes were turned off.
- Code tried to allocate a zero length array for xvalues( ).


## Version 2.002 Release April 2008

Improved check_test.py to compare numbers as numerical values instead of text string. The new results are saved in result_files

Generalized version Makefile and dependencies

-     Uses wildcards for .f .f90 and .c
-     maintains object files in seperate directories
-     for each platform and for debug and optimized
-     use make help for list of options
-     added options opt and debug as build choices


Initialize nremtet and npoints2 to 0, initialize number_elements and number_nodes to zero

Modify output for Dudded points to indicate when there are no elements (for removal)

Changed name from 'program adrivgen' to 'program lagrit_main'

Modified header correcting spelling, changed X3D to LaGriT. 

### Enhancements

-  Add capability to read FEHM zone/zonn files.

### These issues were fixed:

- corrected change to printatt minmax output so that name has 18 characters, with total line of 80 chars
- for cmo_setatt.f added error check for existing cmo and expanded name string size for 17 character names
- for cmo/printatt/ minmax limited format to 80 characters
- Corrected typo in screen output. Changed 'nnelements' to 'nelements'
- Changed some memory allocation from real(2) to int(1) for integer work arrays. Running a large problem (&gt;10,000,000 nodes) was
    crashing at rmpoint due to MALLOC failure.
    
 
 *changesets tracked with Mecurial/Trac 0.10.4 on ancho.lanl.gov/lagrit/hg/lagrit*

