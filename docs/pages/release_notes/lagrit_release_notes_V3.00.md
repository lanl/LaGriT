---
title: 'LaGriT Release Notes V3.0'
---

LaGriT Release Notes
====================


##LaGriT V3.001 August 2011


Major changes incoporating work from Andrew Kuprat (64bit work) and
summer student Adam Cozzette to add more capability. Changes to make
64bit code more consistent and easier to modify for various platforms.
The code for stack routines are combined into stack\_options.f from
temptam.f and read_trilayers.f. The beads algorithm and routines are
now all in their own file beads_ona_ring.f

LaGriT assumes that the size of an integer is the same size as a
pointer. Use the preprocessor and configure settings to select the
integer type so that it matches the size of a pointer.

```
 #if SIZEOF_INT == SIZEOF_VOIDP
 #define int_ptrsize int
 #elif SIZEOF_LONG == SIZEOF_VOIDP
 #define int_ptrsize long
 
 Makefile changes for 64 bit compile:
    -fcray-pointer
 Enables the Cray pointer extension, which provides a C-like pointer

   -falign-commons (will try to reorder commons so this is not needed)
 ```
 
 By default, gfortran enforces proper alignment of all variables in a COMMON block by padding them as needed. On certain platforms this is mandatory, on others it increases performance. If a COMMON block is not declared with consistent data types everywhere, this padding can cause trouble, and -fno-align-commons can be used to disable automatic alignment. The same form of this option should be used for all files that share a COMMON block. To avoid potential alignment issues in COMMON blocks, it is recommended to order objects from largests to smallest.
 


This includes Work from Andrew Kuprat and summer student Adam Cozzette.

###Enhancements:

- **filterkd** - new filter command uses kd-tree for filter. Uses reverseform.f

- *anothermatbld3d.c* added several functions for computing the hybrid point of a control volume: tetisOnBoundary, intersectSegmentWithFace, getHybridPoint
- added helper functions for computing dot products and distances.
- changed the areaOf3dTriangle function to compute a vector area rather than a scalar.
- a change computes a unit vector in the direction of an edge between
              two points in the tetrahedral mesh. We dot this with the facet of the Voronoi
              cell in order to consider just the component of the area that is in the
              direction of the edge.
- changed the parameter list for initialize3ddiffusionmat_ so that the
              function also takes arrays for jtet, pmbndry, ifhybrid, and hybridfactor.
              ifhybrid indicates whether to use hybrid volumes and hybrid_factor is an
              attribute that the function will fill in order to indicate the extent to which
              each cell is hybridized
- added function prototypes so that gcc can perform type checking
- added #ifdef to match format string to size of integers being used

- *anothermatbld3d_wrapper.f* changed subroutine call to add hybrid_factor
```
                     subroutine anothermatbld3d_wrapper
        -     x           (ifile,io_type,num_area_coef,ifcompress)
        +     x           (ifile,io_type,num_area_coef,ifcompress, ifhybrid)
```

- Added support for quad metrics in quality / quad

- Added support for writing element sets out to a file based on pset logic so that eltset will write each element set to a separate file if it is given the -all- option filenames now end with .cellset


- intersect added sort line_graph after performing the intersection


- **pset** changed pset to verify a point before writing to file. Added the -all- option that writes each pset to seperate files with the new file extension .vertexset

- **sort/ line_graph** in file line_graph_sort.cpp. Create the sort keys correctly based on whether it is actually sorting nodes or elements.

- *writedump.f* changes to facilitate the *hybrid* option and Rao's new **dump / exo** code.


###These issues were fixed:

- fix bug in 2D delaunay connect connect2d_lg.f where the code doubled the coordinates of the first Voronoi point.

- fixed a bug where attempting to redefine an element set, the set is now zeroed out and written afresh

- *cr_copy.f* fixed an off-by-one error by making an array one element longer in a call to mmgetblk with length + 1 that was causing segfault errors
              
- *geniee.f* changed loop to check condition before starting, this avoids writing to memory is invalid
              
- *pcc_test.f* fixed Warning so it is given once instead of once for every element
          
- **reorder** fixed so that it doesn't rely on the numerical values to determine whether to sort nodes or elements and added messages to indicate possible WARNINGS for reorder
                      
-  *rotatelo.f* Fixed a bug whereby rotateln would rotate some points in one direction and some points in the opposite direction.
              
- *sparseMatrix.c* removed unused variables in order to get rid of compiler warnings. Added #if for printf to use string according to integer size

- *readgmv_binary.f* Changed gmv routines readgmv_binary.f and dumpgmv_hybrid.f so read/write gmv works on 64-bit
              
------------------------------------------------------------------------


*Changesets tracked in Mercurial/Trac on ancho.lanl.gov/lagrit*
