LaGriT Release Notes
====================

-   [**LaGriT V3.203 July 2017 Release**](#Release_V3.203)

-   [**LaGriT V3.200 September 2016 open-source
    Release**](#Release_V3.200)

-   [**LaGriT V3.108 July 2016 Release**](#Release_V3.108)

-   [**LaGriT V3.106 August 2015**](#Release_V3.106)

-   [**LaGriT V3.100 November 2012**](#Release_V3.100)

-   [**LaGriT V3.001 August 2011 Release**](#Release_V3.001)

-   [**LaGriT V2.200 November 2010 Release**](#Release_V2.200)

-   [**LaGriT V2.106 June 2010**](#Release_V2.106)

-   [**LaGriT V2.100 August 2009 Release**](#Release_V2.100)

------------------------------------------------------------------------

Release_V3.203

LaGriT V3.203 July 2017
-----------------------

Major upgrade to LaGriT build and test scripts.

Added install.h and improved documentation for building LaGriT on Linux
and Mac machines.

Update ExodusII to 7.01 using git clone
https://github.com/gsjaardema/seacas.git

Removed exodus include files from src directory and changed build to use

$EXODUSII\_HOME/include

Updates to lagrit.lanl.gov web pages and documents, added text regarding
open source and github

These issues were fixed:

 segmentation fault during triangulate

 massage command failing to de-refine and no error is reported

 ExodusII output for 2D Planar incorrectly written as 3 Dimensions

 second call to addmesh/excavate command causing memory error or
 segmentation fault

 output adjusted to make room for large numbers when reporting dudded
 nodes from filter command

Additional tests and improvements to suite.py for running and reporting
the test suite New tests include:

 level01/createpts\_filter - illustrates the difference between filter
 and the more accurate filterkd commands

 level01/smooth\_massage - use massage with smooth to refine and
 de-refine a triangulation

 level01/write\_exo - write 3D and 2D Exodus files with psets, eltsets,
 and facesets

 level01/pflotran\_stor - write pflotran style Voronoi geometric
 coefficient (volume, face area) files

PyLaGriT new features:

 read\_sheetij - creates a quad mesh from an elevation file reading
 Z(i,j) into mesh zic attribute

 read\_modflow - creates a modflow rectilinear mesh using modflow files
 using elev\_top.mod and init\_bnds.inf

 read\_fehm - read FEHM format mesh file with geometry and connectivity
 (LaGriT does not have this option)

 examples/arctic\_w\_ice\_wedges - example Mesh for 2D polygon with ice
 wedges by Chuck Abolt

 examples/julia - Example of using PyLaGriT within a Julia script.
 Requires that the Julia package PyCall is installed.

Known Issues:

 Builds with compiler gfortran v5 or greater can generate run-time
 signal exceptions such as IEEE\_DNORMAL. Most exceptions have been
 tracked down and fixed according to picky compiler suggestions. These
 exceptions have not changed the behavior of the code. Exceptions will
 continue to be fixed as they show up.

 

 During testing it was found that the filter command may find and
 remove fewer nodes than the command filterkd. The command filterkd is
 recommended where precision might be an issue. Documentation will be
 changed to reflect this. It is expected that the filter command and
 algorithm will be deprecated and replaced by filterkd. See
 test/level01/createpts\_filter

 

 Paraview has trouble displaying ExodusII facesets for 2D grids in 3
 Dimensions, but is fine with 2D planar where Z values are ignored.
 When viewing these same ExodusII files with GMV, the facesets look
 correct. Checking files by converting to ASCII with ncdump, the files
 look correct. We do not know if this is a LaGriT, ExodusII, or
 Paraview issue. See test/level01/write\_exo/reference for ExodusII
 example files.

------------------------------------------------------------------------

Release_V3.200

LaGriT V3.200 September 2016
----------------------------

LaGriT V3 LACC-15-069 is now distributed as open-source software under a
BSD 3-Clause License.

This version moves the entire LaGriT repo from a local mercurial version
control to github and now includes the python driven version PyLaGriT.

For most current versions of source and documentation use the
open-source repository at

 https://github.com/lanl/LaGriT

*Compiled executable versions of LaGriT will continue to available
through https://lagrit.lanl.gov/licensing.md*

------------------------------------------------------------------------

Release_V3.108

LaGriT V3.108 July 2016
-----------------------

Last version under open distribution license:

    2012 LaGriT Version 3 LACC-2012-084
    Copyright Notice: This program was prepared by Los Alamos National
    Security, LLC at Los Alamos National Laboratory (LANL) under contract 
    No. DE-AC52-06NA25396 with the U.S. Department of Energy (DOE). All rights 
    in the program are reserved by the DOE and Los Alamos National Security, LLC. 
    Permission is granted to the public to copy and use this software without 
    charge, provided that this Notice and any statement of authorship are 
    reproduced on all copies. Neither the U.S. Government nor LANS makes any 
    warranty, express or implied, or assumes any liability or responsibility 
    for the use of this software.

Change from Mercurial open distribution to open source on Github. Tag
this version Release V3.108 and use clone for V3.2 for start of
open-source repository.

    LaGriT V3.108 from https://ancho.lanl.gov/lagrit/hg/lagrit
    tag:         Release V3.108
    parent:      349:8524530343c2
    parent:      346:a5f6fb10ecce
    user:        Terry Miller 
    date:        Tue Jul 26 09:48:53 2016 -0600

This merges student Mikita V3.106 WIN development with master.

Mikita comments and updates to mercurial/trac repository include:

-   **test**

    Added top level python control suite, and standarized level02 output
    files.
-   **build windows**

    The windows version of LaGrit is compiled using cygwin by Mikita.
    See files instructions.txt (copied to build\_win.txt), and
    cmake-script.
-   **build static**

    The static version of LaGrit is compiled on Ubuntu by Mikita.
    Instructions Ubuntu are not written, however the instructions
    written for Windows static compilation using Cygwin are almost
    exactly as they are for Ubuntu.
-   **lg\_util library**

    changed both opsys.h and type\_sizes.h to account for win64 and
    changed define for SIZEOF\_LONG

        opsys.h
        157,165d156
        < #ifdef win64
        < #define FCV_UNDERSCORE
        < #define SIZEOF_INT 4
        < #define SIZEOF_LONG 8
        < #define SIZEOF_VOIDP 8
        < #define MAX_UINT 18446744073709551615.00 
        < #endif
        < 
        < 
        185,187d175
        < #ifdef win64
        < #define int_ptrsize long long
        < #else
        189d176
        < #endif

        type_sizes.h
        46a47,54
          **/** ** Cygwin ** ****
         #ifdef __CYGWIN__
         #define FCV_UNDERSCORE
         #define SIZEOF_INT 4
         #define SIZEOF_LONG 4
         #define SIZEOF_VOIDP 8
         #endif ***

        machine_header.h
        68a69,74
         #ifdef win64
         #define FCV_UNDERSCORE
         #define SIZEOF_INT 4
         #define SIZEOF_LONG 8
         #define SIZEOF_VOIDP 8
         #endif

        Makefile
        94a95,109
         ifeq ($(COMPILER), cygwin)
         SUFFC = _cygwin
         FC = /bin/gfortran
         CC = /bin/gcc
         CXX = /bin/c++
         FC90 = /bin/gfortran
         
         OSTAG = _cygwin
         
         FFLAGS = -fcray-pointer -fdefault-integer-8 -m64 -Dwin64
         FF90FLAGS = -fcray-pointer -fdefault-integer-8 -m64 -Dwin64
         CFLAGS = -m64 -Dwin64 
         # -I/usr/include
         endif
         

------------------------------------------------------------------------

Release_V3.106

LaGriT V3.106 August 2015
-------------------------

Major update to write PFLOTRAN type option stor file and new syntax
using Exodus II 6.9 libraries.

This version is used for code development by summer student Mikita Yanki
that includes changes and scripts to build LaGriT on windows with Exodus
II V6 libraries. This is a branch that does not include some of the
V3.107 updates. Mikita's version is merged and contained in the V3.2
release.

*Note: The LaGriT run-time banner shows V3.2 with compile date Aug 2015,
even though it is actually a branch from V3.106.*

Major updates include:

-   **dump / pflotran**

    Writes .uge file for pflotran and is used by the DFN suite of
    scripts. The first block are the list of ids of cells and the
    coordinates of cell centroids and the volumes of the cells. The
    second block consists of a list of ids of the connecting cells
    (id\_up, id\_dn), coordinates of the face centroid between the two
    connected cells and areas of the faces.

    Test directory is in /n/swdev/LAGRIT/work/pflotran/test along with
    upper directory with dev work.

    The routines are called from matbld 2D and 3D and use the same
    syntax as dump/stor and look like:

            dump / pflotran / root_name / cmo_name
            dump / pflotran / root_name / cmo_name / nofilter_zero

-   **dump / exo**

    New code syntax using ExodusII 6.09, LaGriT commands remain the
    same.

           http://sourceforge.net/projects/exodusii/files/
           Exodus II 6.09
           HDF5 version 1.8.6
           netcdf-4.1.3

-   **exo block id** modified to single digit.

    All exodus files are same as Exodus II 5, except for the block id.
    Tests have been updated resulting in the following differences:

        Exodus 6.09:
        <               :api_version = 6.09f ;
        <               :version = 6.09f ;
        ---
        <  eb_prop1 = 1, 2, 3 ;

        Exodus 5.22a:
                       :api_version = 5.22f ;
                       :version = 5.22f ;
        ---
          eb_prop1 = 10000, 20000, 30000 ;

Minor updates to master version include:

-   **attribute compress\_eps**

    new attribute for stor file allowing user to extend range of ccoef
    values by setting mesh attribute compress\_eps (from default 1e-8).
    This will keep connection regardless of coefficient value. Changing
    value of compress\_epsilon seemed to help loss of coeffs with large
    aspect ratios.
-   **dump / stor (2D)**

    Corrected bug that overwrites volic with incorrect value if grid is
    non-planer.
-   **dump / fehm**

    add space between ns and nelements, increase to i12
-   **read / fehm**

    Fixed seg fault for 0 elem report message by using a,a instead of a
    format.
-   **build exodus libs**

    The following is a list of issues when dealing with building static
    libraries for exodus and its compilation.

        Library inclusion order in the following places:
        1. /n/swdev/src/exodusii/exodus-6.09/exodus/cbind/CMakeList.txt 
            Line 284
        2. /n/swdev/src/exodusoo/exodus-6.09/exodus/forbind/CMakeList.txt
            Line 62
        Solution was to switch ${HDF5_LIBRARY with ${HDF5HL_LIBRARY

------------------------------------------------------------------------

Release_V3.101

LaGriT V3.101 November 2013
---------------------------

This version does NOT have the PFLOTRAN file output used by DFN
workflow.

There are no major enhancements, code is compiled and tested for
release. This is in preparation for major update to Exodus 6 which
changes routine calls and syntax.

    V3.103 is last version lagrit code using Exodus 5 libs
    V3.104 is new  version lagrit code using Exodus 6 libs

    - Build/test V3.103 old RHEL RedHat 5 on aquifer with Exodus 5
    - Build/test V3.103 new RHEL RedHat 6.5 on darcy or talik with Exodus 5
    - Build/test V3.103 Ubuntu-14.04-x86_64 with Exodus 5
    - Build/test V3.104 Ubuntu-14.04-x86_64 with Exodus 6

    - Build/test V3.104 WIN7 with Exodus 6
    - Build/test V3.104 MacOS with Exodus 6
    - Build/test V3.104 RHEL 6.5 with Exodus 6

    Current Stable Linux version:
      LaGriT V3.101 Linux m64
      date_compile: 2013/04/25  RH gf4.5

    These are the external libs used with LaGriT V3.1 As of November 2012
       http://sourceforge.net/projects/exodusii/files/
         Exodus II 5.22a
         HDF5 version 1.8.6
         netcdf-4.1.3

    Linux RHEL Exodus 5 libraries were built in
     /n/swdev/LAGRIT/VERS_3.100_012_NOV09/build_lagrit/exodus

    Build executable for linux:
    gfortran -O -Dlinx64 -static -fcray-pointer -fdefault-integer-8 -fno-sign-zero -o mylagrit lagrit_main.o lagrit_fdate.o lagrit_lin64_o_gf4.5.a /n/swdev/LAGRIT/VERS_3.100_012_NOV09/build_lagrit/lg_util/lib/util_lin64_o_gfort4.5.a -L /n/swdev/LAGRIT/VERS_3.100_012_NOV09/build_lagrit/exodus/lin64/lib -lexoIIv2for -lexodus -lnetcdf -lhdf5_hl -lhdf5 -lz -lm -lstdc++

This release include the following enhancements:

-   **read / zonezone\_element **/
    

    Added option zone\_element which allows reading of node or element
    list in FEHM zone or zonn format. Each node or element number found
    in the list has attribute tagged.

    

This release include the following fixes:

-   **
#105: addatt/ mo\_tri / unit\_area\_normal**
    

    Fixed incorrect zero result and fixed attribute handling so vector
    array is formed using irank = 3.

    

-   **
#108: cmo/addatt/mo/area\_normal/xyz/att\_v\_area**
    

    Result is off by factor of 2, fixed area normal to assign half the
    cross product (for triangles).

    

-   **
#132: synth\_norm**
    

    Fixed handling of attributes. The synthetic normals were creating a
    dummy attribute not used because offsetsurf is creating x\_n\_norm
    y\_n\_norm z\_n\_norm on the input cmo. Attribute names are ignored
    on the command line, added better reporting for this.

------------------------------------------------------------------------

Release_V3.100

LaGriT V3.100 November 2012
---------------------------

Major changes to most parts of the code to enable 64 bit compilation and
added external Exodus II 5 libraries to write Exodus basic mesh files.
These are the external libs used with this release:

A few minor changes/fixes include: cmo/copyatt fix copy from node
attribute to elem attribute of equal length. cmo/readatt fix to allow
char in first position. Memory and bug fixes related to 64 bit code
changes. Improved error catching for common routines.

### Command Enhancements

-   **dump / exo**
    

    Now includes netcdf and exodus libs for writing exodus mesh files
    and reading and writing facesets.
        Syntax:
          dump / exo / ifile / cmoname

          Dump exodus files with/without facesets, fast/slow options:
          dump / exo / ifile / cmoname / facesets / on
          dump / exo / ifile / cmoname / facesets / off
          dump / exo / ifile / cmoname / facesets / on file1,file2,...filen
          dump / exo / ifile / cmoname / facesets / off file1,file2,...filen

          write exo pset and eltsets:
          dump / exo / filenam.exo / cmoname / psets / eltsets /
          dump/exo/mesh_07.exo/mo7//eltsets/ &
             facesets bc01.faceset &
             bc02.faceset bc03.faceset bc04.faceset &
             bc05.faceset bc08.faceset bc09.faceset
          dump/exo/mesh_06.exo/mo6/psets// &
             facesets bc01.faceset &
             bc02.faceset bc03.faceset bc04.faceset &
             bc05.faceset bc08.faceset bc09.faceset

    

-   **dump** (short version)
    

    3 token short syntax for dump (avs,gmv,lg,lagrit,ts,exo)

    

-   **extract/surfmesh**
    

    Now creates attributes to hold element local face numbers of 3D
    input mesh that occur on either side of output mesh face, idface0
    and idface1. Now copies user-created node-based attributes from
    source.

    

-   **interpolate**
    

    Changed interpolate to "find" more points on edges this will permit
    nodes to find a nearest edge or point and be "inside" the triangle
    for extreme small or large numbers where epsilon values are
    difficult to evaluate correctly.

    

-   **massage**
    

    Added option for massage to refine based on an attribute field.
        Syntax:
          massage / [bisection length/field name] / merge_length / toldamage / ...

    

-   **massage2**
    

    Under development massage2 syntax for incremental refinement
    strategies.
        Syntax:
          massage2/ [file name] / [Target Length Scale]/[field name]/ &
             merge_length/toldamage/[tolroughness]/[ifirst,ilast,istride]/ ...

    

-   **math**
    

    add modulo and mod options

    

-   **recon**
    

    Code improvements related to recon 0 and recon 1 will result in
    slightly different but better connectivity results.

    

-   **sort**
    

    Added line sort by nodes or elements for creating valid polygons
    that can be read and used by other routines.
        Syntax:
          sort / line_graph / cmo / ascending  descending / [key] / [nodes/elements]

------------------------------------------------------------------------

Release_V3.001

LaGriT V3.001 August 2011
-------------------------

Major changes incoporating work from Andrew Kuprat (64bit work) and
summer student Adam Cozzette to add more capability. Changes to make
64bit code more consistent and easier to modify for various platforms.
The code for stack routines are combined into stack\_options.f from
temptam.f and read\_trilayers.f. The beads algorithm and routines are
now all in their own file beads\_ona\_ring.f

LaGriT assumes that the size of an integer is the same size as a
pointer. Use the preprocessor and configure settings to select the
integer type so that it matches the size of a pointer.

    #if SIZEOF_INT == SIZEOF_VOIDP
    #define int_ptrsize int
    #elif SIZEOF_LONG == SIZEOF_VOIDP
    #define int_ptrsize long

    Makefile changes for 64 bit compile:
      -fcray-pointer
    Enables the Cray pointer extension, which provides a C-like pointer

    -falign-commons (will try to reorder commons so this is not needed)
    By default, gfortran enforces proper alignment of all variables in a COMMON block by padding them as needed. On certain platforms this is mandatory, on others it increases performance. If a COMMON block is not declared with consistent data types everywhere, this padding can cause trouble, and -fno-align-commons can be used to disable automatic alignment. The same form of this option should be used for all files that share a COMMON block. To avoid potential alignment issues in COMMON blocks, it is recommended to order objects from largests to smallest.

Adam Cozzette changes:

-   anothermatbld3d.c

            - added several functions for computing the hybrid point of a control volume:
              tetisOnBoundary, intersectSegmentWithFace, getHybridPoint
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
            - added #if to match format string to size of integers being used

-   anothermatbld3d\_wrapper.f

            - changed subroutine call to add hybrid_factor
                     subroutine anothermatbld3d_wrapper
        -     x           (ifile,io_type,num_area_coef,ifcompress)
        +     x           (ifile,io_type,num_area_coef,ifcompress, ifhybrid)

-   cmo\_addatt.f

            - added the hybrid_volume to the cmo // addatt command
            - added a metric for quad quality  cmo // addatt / quad_quality

-   connect2d\_lg.f

            - fix bug in 2D delaunay connect where the code
              doubled the coordinates of the first Voronoi point

-   cr\_copy.f

            - fixed an off-by-one error by making an array one element longer
              in a call to mmgetblk with length + 1
              this was causing segfault errors

-   dumpfehm.f

            - changed subroutine call to add hybrid_factor
               subroutine dumpfehm(ifile,ifileini,ioption,iomode,
        -     *       area_coef_option,compress_opt,attrib_option,area_option)
        +     *       area_coef_option,compress_opt,attrib_option,area_option,
        +     *       hybrid_option)

-   eset.f

            - Added support for writing element sets out to a file based on pset logic
              changed the behavior so that eltset // write will write each element set
              to a separate file if it is given the -all- option
              filenames now end with .cellset
            - fixed a bug where attempting to redefine an element set, the set is now
              zeroed out and written afresh

-   geniee.f

            - changed loop to check condition before starting, this avoids
              writing to memory is invalid

-   intersect\_cmo.f

            - added sort // line_graph after performing the intersection
            - fixed loop that was looping wrong number of times
              changed 1,npointsa to 1,npoint

-   pcc\_test.f

            - fixed Warning so it is given once instead of once for every element

-   pset.F

            - changed pset to verify a point before writing to file
            - added the -all- option that writes each pset to seperate files
              with the new file extension .vertexset

-   quality.f

            - Added support for quad metrics in quality / quad

-   reorder.f

            - fixed reorder so that it doesn't rely on the numerical values
              to determine whether to sort nodes or elements
            - added messages to indicate possible WARNINGS for reorder

-   rotatelo.f

            - Fixed a bug whereby rotateln would rotate some points in one
              direction and some points in the opposite direction.

-   sortbins.f

            - Added support for sort // line_graph. line_graph_sort.cpp does most of the
              real work. I also had to fix this subroutine to create the sort keys correctly.
              Previously it used an integer length to decide whether to use 'nnodes' or
              'nelements', but now it is careful to choose based on whether it is actually
              sorting nodes or elements.

-   sparseMatrix.c

            - removed unused variables in order to get rid of compiler warnings
            - added #if for printf to use string according to integer size

-   writedump.f

            - changes to facilitate the 'hybrid' option and Rao's new dump / exo code



------------------------------------------------------------------------

Release_V2.200

LaGriT V2.200 November 2010
---------------------------

Banner when LaGriT is started (eg Linux):

``` {wrap=
*               *    Program:  LaGriT V2.200   Linux m32      *                 
*               *    date_compile: 2010/11/22                 *     
```


+-----------------------------------------------------------------------+
 The next release, expected in early 2011, will have 64 bit memory     
 pointers. This will allow access to much more real and virtual memory 
 so that the present limitations on memory and mesh size that can be   
 built and manipulated will be greatly expanded.
                      
 ### Modifications and New Capabilities Index:
                        

 -   [**dump / zone\_outside / ...**](#dump__zone_outside__...)        
 -   [**cmo / addatt / voronoi\_varea /                                
     ...**](#cmo__addatt__voronoi_varea__...)                          
 -   [**extract / surfmesh / ...**](#extract__surfmesh__...)           
 -   **[addmesh / excavate / ...](#addmesh__excavate__...)
            
     **                                                                
 -   [**interpolate / ...**](#interpolate__...)                        
 -   [**read /...**](#read_...)                                        
 -   **[dump / ...](#dump_...)
                                        
     **                                                                
 -   [**cmo / attribute\_union / ...**](#cmo__attribute_union__...)    
 -   [**compute / linear\_extrapolate /                                
     ...**](#compute__linear_extrapolate__...)                         
 -   [**grid2grid /...**](#grid2grid_...)                              
 -   [**dump / stor / ...**](#dump__stor__...)                         
 -   [**pset /...**](#pset_...)                                        
 -   **[memory / ...](#memory__...)**[]{style="font-weight: bold;"    

 #### dump__zone_outside__...** **dump / zone\_outside / ...
      
 ** ** {#dump-zone_outside-... style="font-family: Courier New,Courier, 
 monospace;"                                                          

 Changed FEHM outside area calculation to default to Voronoi area      
 associated with nodes of a 3D tetrahedral mesh instead of computing   
 Median area. FEHM file file\_root\_outside.area changed to            
 file\_root\_outside\_vor.area For dump/zone, added keywords           
 keepatt\_area or keepatt\_voronoi which will compute and keep voronoi 
 vector areas xn\_varea, yn\_varea, zn\_varea and keepatt\_median will 
 compute area/num nodes on face and keep attributes xn\_area,          
 yn\_area, zn\_area The written file file\_root\_outside\_vor.area or  
 file\_name\_outside\_med.area is a list of 2D area vectors            
 (Ax\_i,Ay\_i,Az\_i) associated with each node.                        
 [https://lagrit.lanl.gov/docs/DUMP3.md                              
 dump/zone\_outside](https://lagrit.lanl.gov/docs/DUMP3.md%20dump/zo 
 ne_outside)
                                                          
 #### cmo__addatt__voronoi_varea__...** **cmo / addatt / voronoi\_ 
 varea / ...
                                                          
 ** ** {#cmo-addatt-voronoi_varea-... style="font-family: Courier New,C 
 ourier,monospace;"                                                   

 This module will do the same voronoi calculation on triangles as is   
 done with the outside area for a 3D tetrahderal mesh. The call will   
 create vector components for each node and fill the node attributes   
 xn\_varea, yn\_varea, zn\_varea.                                      
 <https://lagrit.lanl.gov/docs/cmo_addatt.md
                       
 #### extract__surfmesh__...** **extract / surfmesh / ...
         
 ** ** {#extract-surfmesh-... style="font-family: Courier New,Courier,m 
 onospace;"                                                           

 Now creates attributes to hold element local face numbers of 3D input 
 mesh that occur on either side of output mesh face, idface0 and       
 idface1. Now copies user-created node-based attributes from source    
 mesh into extracted sink mesh.                                        
 <https://lagrit.lanl.gov/docs/EXTRACT1.md
                         
 #### addmesh__excavate__...** **addmesh / excavate / ...** ** {#ad 
 dmesh-excavate-... style="font-family: Courier New,Courier,monospace; 
 "                                                                    

 excavate - remove nodes and elements if they fall with the            
 circumsphere of triangles on the input mesh.
                         
 
                                                                     
 mesh1 must be a 3D mesh (of any geometry) and mesh2 must be a 2D      
 triangular mesh. This command then excavates a volume in mesh1 around 
 mesh2, such that the surface could then be inserted into the 3D mesh  
 (such as to insert a fault into a background terrain mesh). The       
 background mesh, minus the excavated/removed nodes, is put into       
 mesh3. If the optional [bfs] argument is given, the routine will    
 use a breadth-first search algorithm to find nodes to remove, as      
 opposed to the default KD-tree algorithm. If the optional [connect] 
 argument is given, the program will, after excavation, execute an     
 addmesh/append, and then a connect, to produce a fully connected mesh 
 with the surface (mesh2) inserted into the background (mesh1).        
 [https://lagrit.lanl.gov/docs/ADDMESH.md
                           
 ](https://lagrit.lanl.gov/docs/ADDMESH.md)                          
 #### interpolate__...** **interpolate / ...
                      
 ** ** {#interpolate-... style="font-family: Courier New,Courier,monosp 
 ace;"                                                                

 Changed interpolate to "find" more points on edges this will permit   
 nodes to find a nearest edge or point and be "inside" the triangle    
 for extreme small or large numbers where epsilon values are difficult 
 to evaluate correctly. Note, this changed test results for            
 interpolate, test/level01 results were updated for these              
 improvements. <https://lagrit.lanl.gov/docs/main_interpolate.md
   
 
                                                                     
 intrp\_gtg.f
                                                         
 A bug was fixed in interpolation that would sometimes save a node id  
 in pt\_gtg or el\_gtg attributes that was not related to the found    
 candidate and value. This could occur where there are multiple        
 candidates for the source and if epsilon values are near machine      
 limits. The test in level01/intrp\_2D\_sizes was changed to capture   
 and evaluate these issues.
                                           
 
                                                                     
 intrp\_gtg.f, inside\_lg.f
                                           
 There are changes to interpolate using tests for finding points that  
 are inside or on edges or vertices of an element. The epsilon tests   
 have been relaxed to allow points that are "near" to be found on edge 
 - if within the chosen epsilon. The interpolation has been changed to 
 evaluate candidate points based on the confidence of being inside the 
 associated triangle. A result indicating the point is inside will     
 "win" over a candidate result that is on edge or vertice. If idebug   
 attribute is set to a number of 5 or greater, there will be many more 
 statements written that are related to the inside triangle and        
 epsilon tests.
                                                       
 
                                                                     
 Substantial changes and additions to DUMP2.md which describes all   
 the dump file\_types and include more descriptions of FEHM files.     
 DUMP3.md for dump/fehm now includes descriptions of FEHM files.     
 https://lagrit.lanl.gov/docs/DUMP2.md                               
 https://lagrit.lanl.gov/docs/DUMP3.md
                              
 #### read_...** **read /...** ** {#read-... style="font-family: Co 
 urier New,Courier,monospace;"                                        

 Three token read implemented. Files are recognized based on their     
 suffix (AVS, GMV, ...) rather than requiring that the second token    
 specify the file type.
                                               
 <https://lagrit.lanl.gov/docs/READ.md
                             
 #### dump_...** **dump** ** ** **/...** ** {#dump-... style="font-fa 
 mily: Courier New,Courier,monospace;"                                

 Two and three token dump implemented. Files types (AVS, GMV, ...) are 
 recognized based on their suffix rather than requiring that the       
 second token specify the file type. Two token write does not require  
 a MO name. The default MO is used.
                                   
 <https://lagrit.lanl.gov/docs/DUMP2.md                             
 #### cmo__attribute_union__...** **cmo / attribute\_union / ...
  
 ** ** {#cmo-attribute_union-... style="font-family: Courier New,Courie 
 r,monospace;"                                                        

 Change two meshes so they both share the same set of attributes       
 (taking the union of their sets of attributes)
                       
 <https://lagrit.lanl.gov/docs/cmo_att_derive.md
                   
 #### compute__linear_extrapolate__...** **compute / linear\_extra 
 polate / ...
                                                         
 ** ** {#compute-linear_extrapolate-... style="font-family: Courier New 
 ,Courier,monospace;"                                                 

 linear\_extrapolate - keyword for an extrapolation from an attribute  
 value in a surface onto every node of a 3D mesh. Given a 3D mesh and  
 a 2D surface, this command will extrapolate a scalar value from that  
 surface onto every point of the mesh. This can be used to (for        
 example):
                                                            
     
* Propogate head values from a surface onto all nodes of a       
 mesh.
                                                                
     
* Expand a mesh to fit a surface, by propogating the appropriate 
 spatial coordinate.
                                                  
     
* Compute the depth relative to a topographic surface to each    
 node of a mesh. <https://lagrit.lanl.gov/docs/COMPUTE.md
          
 #### grid2grid_...** **grid2grid /...** ** {#grid2grid-... style=" 
 font-family: Courier New,Courier,monospace;"                         

 grid2grid wrapper for hextotet. Use to convert:
                      
 -   quadtotri2    quad to 2 triangles, no new points.                 
 -   prismtotet3   prism to 3 tets, no new points.                     
 -   quattotri4    quad to 4 triangles, with one new point.            
 -   pyrtotet4     pyramid to 4 tets, with one new point.              
 -   hextotet5     hex to 5 tets, no new points.                       
 -   hextotet6     hex to 6 tets, no new points.                       
 -   prismtotet14  prism to 14 tets, four new points (1 + 3 faces).    
 -   prismtotet18  prism to 18 tets, six new points (1 + 5 faces).     
 -   hextotet24    hex to 24 tets, seven new points (1 + 6 faces).     
 -   tree\_to\_fe    quadtree or octree grid to grid with no           
     parent-type elements.                                             

 <https://lagrit.lanl.gov/docs/GRID2GRID.md
                        
 #### dump__stor__...** **dump / stor / ...
                       
 ** ** {#dump-stor-... style="font-family: Courier New,Courier,monospac 
 e;"                                                                  

 anothermatbld3d\_wrapper.f
                                           
 Create two new node vectors, ccoef, ij\_ccoef Put the negative ij     
 coefficient value into the two nodes connected to the ij edge. The    
 vector ij\_coef will assign the j index value to node i so that one   
 can determine which edge is associated with the neative coefficient   
 that is assigned to nodes. <https://lagrit.lanl.gov/docs/DUMP2.md
 
 
                                                                     
 <https://lagrit.lanl.gov/docs/DUMP3.md
                            
 
                                                                     
 Changes to include TranslateTetToZero for geometric calculations (not 
 sure it is really helping with some of our neg ccoef issues)
         
 
                                                                     
 anothermatbld3d\_wrapper.f
                                           
 Extensive chages to error handling and messages, but not to the logic 
 of program This code has same logic as matbld3d - but uses linked     
 lists instead of mmgetblk calls Use io\_type to toggle creation of    
 attribute for voronoi volumes or to write to stor file added          
 extensive error checking to eliminate segmentation faults added error 
 check and message for every mmgetblk and mmrelblk added calls to      
 mmprint when mm calls fail cleaned up variable declarations and added 
 comments added istatus to check for errors and completion of matrix   
 changed all routine messages to start with AMatbld3d\_stor to         
 distinguish from matrix built with Matbld3d\_stor added idebug        
 options added status report at end of routine
                        
 
                                                                     
 matbld3d\_stor.f
                                                     
 Extensive chages to error handling and messages, but not to the logic 
 of program This code uses many mmgetblk calls and about 40 percent    
 more memory than linked list version added extensive error checking   
 to eliminate segmentation faults added error check and message for    
 every mmgetblk and mmrelblk added calls to mmprint when mm calls fail 
 cleaned up variable declarations and added comments added istatus to  
 check for errors and completion of matrix added idebug options added  
 status report at end of routine
                                      
 matbld3d\_stor.f
                                                     
 add warning for newlen call with uncertain effect
                    
 
                                                                     
 sparseMatrix.c
                                                       
 initialize list pointers to null assign null to pointers after free   
 add warning messages for failure to free
                             
 
                                                                     
 dumpfehm.f
                                                           
 Add compress\_opt to dumpfehm arguments add comments and error        
 checking to clarify code logic check options and set for 2D or 3D     
 calls to matbld use matbld3d\_stor for compress options none and      
 coefs use anothermatbld3d\_wrapper for compress options all and graph 
 Note anothermatbld3d\_wrapper can write only scalar coef values
      
 #### pset_...** **pset /...** ** {#pset-... style="font-family: Co 
 urier New,Courier,monospace;"                                        

 Add option to pset/ / zone for user specified zone id number.
        
 <https://lagrit.lanl.gov/docs/PSET.md
                             
 #### ** **dump **/** {#dump style="font-family: Courier New,Courier,mo 
 nospace;"                                                            

 writedump.f
                                                          
 declare implicit none and initialize variables add comments to        
 clarify the case switches add more error checking and messages change 
 syntax for dump/ fehm and dump/ stor old keywords not needed include  
 alternate\_scalar, binaryc, asciic compression keywords are now none, 
 coefs, graph, or all old syntax still works, but now code checks for  
 keywords after filename and cmo and sets options for the fehm and     
 stor routine calls The man pages are updated and corrected.           
 <https://lagrit.lanl.gov/docs/DUMP2.md
                            
 #### **memory__...**memory / ...** **
                            
  {#memory-... style="font-family: monospace;"                        

 New options to print and check memory manager and report memory       
 usage. This superseeds old utilities mmprint, mmcheck, etc.           
 [https://lagrit.lanl.gov/docs/memory.md
                            
 ](https://lagrit.lanl.gov/docs/memory.md)                           
 ### Test Cases Modified:                                              

 test/level01
                                                         
 Change reference files to results from Linux Updated zone\_outside    
 and zone\_outside\_minmax to current voronoi version
                 
+-----------------------------------------------------------------------+

------------------------------------------------------------------------

Release_V2.106

LaGriT V2.106 June 2010
-----------------------

This includes a minor changes and added error checking. The following
are new syntax that are code wrappers for common command calls.

-   cmo/attribute\_union
-   compute / linear\_extrapolate
-   grid2grid/ tree\_to\_fe

    grid2grid/ quadtotri4

------------------------------------------------------------------------

Release_V2.100

LaGriT V2.100 August 2009
-------------------------

This is a major update to LaGriT and the lg\_util library. Major changes
have been made in the core memory management routines to allow
development for a 64 bit release. These changes will be invisible to
most users but allows better reporting of errors and memory usage for
useful diagnostic information.

Other changes include:

Executable build for Mac with Intel chip

New capability in compute module to compute signed distance fields

Incorporate METIS source code for graph partition and reorder package
with LaGriT.

For details of METIS algorithms and descriptions of the third command
line argument see: http://glaros.dtc.umn.edu/gkhome/views/metis

Add option to create node attribute that is the Voronoi volume
associated with each node of a Delaunay mesh

Module addmesh modified to handle errors so that it can be used in a
loop without needing to have first call be different

Updates stor file commands so default uses newest version of code to
build sparse matrix. This uses less memory and takes less time to
build.

New syntax options for stor file compression include all (default),
graph, coefs, or none.

Update manual and web pages

Bug fix, various modules

