---
title: 'LaGriT V2 Release Notes'
---


-------------------------------------
## Version 2.004 Release 09/12/08


### Enhancements:

-  resetpts bug fix, boundary_components and extract_surfmesh added features
-  boundary_components: added id_numb boundary index number
-  extract_surfmesh: added attributes idelem0, idelem1, idnode0 and removed attribute map- Made changes so that filter and rmmat return with no action rather than crash when passed an empty mesh object.
- Add options dump/att_node and dump/att_elem
- Add options dump/att_node and dump/att_elem to output tables of either node attributes or element attributes to ascii files with header lines beginning with character \#. 
These are similar to *dump/avs/file/mo/0 0 2 0* or *dump/avs/file/mo/0 0 0 2* except the addition of \# character to start lines. Now these files can be read in without editing using *cmo / readatt /* workflow.


### These issues were fixed:
-  resetpts had a bug for some element types
- Corrected bug: cmo_exist returned error flag but check of error flag was to wrong error flag variable.
- Added character # for comment lines

---------

## LaGriT V2.003 Release 05/20/08

Compile and test V2.003 for platforms SGI-32, SUN, MAC, LINUX

### These issues were fixed:

- These include fixes to SGI compile errors in dumpavs.f filter.f refine_tet_add.f lagrit*.h writinit.f and the Makefile in src
- Correct minor bugs. Test case now works. sphere1.f had an incorrect attempt to use MO name before the name had been obtained. Code would crash.
- offsetsurf.f did not handle problems with non-triangle or line type MO. Instead of kicking out, an attempt was made to compute sythetic normal for a mesh object (such as quad) and this caused code to crash.
- Fixed error that occured when all output attributes were turned off.
- Fixed error that occured when all output attributes were turned off.
- Code tried to allocate a zero length array for xvalues( ).


## V2.002 apr 2008

Added 00README with instructions to run tests Saved old version V2.001

Updated all reference/outx3dgen to new printatt output

improved check_test.py to compare numbers as numerical values instead of text string. The new results are saved in result_files

Generalized version Makefile and dependencies

-     Uses wildcards for .f .f90 and .c

-     maintains object files in seperate directories

-     for each platform and for debug and optimized

-     use make help for list of options

-     added options opt and debug as build choices


initialize nremtet and npoints2 to 0, initialize number_elements and number_nodes to zero

modify output for Dudded points to indicate when there are no elements (for removal)

Changed name from 'program adrivgen' to 'program lagrit_main'

- Modified header correcting spelling, changed X3D to LaGriT. 

### Enhancements

-  Add capability to read FEHM zone/zonn files.

### These issues were fixed:

- corrected change to printatt minmax output so that name has 18 characters, with total line of 80 chars
- for cmo_setatt.f added error check for existing cmo and expanded name string size for 17 character names
- for cmo/printatt/ minmax limited format to 80 characters
- Corrected typo in screen output. Changed 'nnelements' to 'nelements'
- Changed some memory allocation from real(2) to int(1) for integer work arrays. Running a large problem (&gt;10,000,000 nodes) was
    crashing at rmpoint/pset get pset_name due to MALLOC failure.

