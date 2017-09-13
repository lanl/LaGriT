---
title: 'LaGriT Release Notes V3.0'
---

LaGriT Release Notes
====================

-   [**LaGriT V3.001 August 2011 Release**](#Release_V3.001)

    

-   [**LaGriT V2.200 November 2010 Release**](#Release_V2.200)

    

-   [**LaGriT V2.106 June 2010**](#Release_V2.106)

    

-   [**LaGriT V2.100 August 2009 Release**](#Release_V2.100)

    





------------------------------------------------------------------------

[]{#Release_V3.001

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

Andrew Kuprat changes:




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

[]{#Release_V2.200

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

 #### []{#dump__zone_outside__...****dump / zone\_outside / ...
      
 **** {#dump-zone_outside-... style="font-family: Courier New,Courier, 
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
                                                          
 #### []{#cmo__addatt__voronoi_varea__...****cmo / addatt / voronoi\_ 
 varea / ...
                                                          
 **** {#cmo-addatt-voronoi_varea-... style="font-family: Courier New,C 
 ourier,monospace;"                                                   

 This module will do the same voronoi calculation on triangles as is   
 done with the outside area for a 3D tetrahderal mesh. The call will   
 create vector components for each node and fill the node attributes   
 xn\_varea, yn\_varea, zn\_varea.                                      
 <https://lagrit.lanl.gov/docs/cmo_addatt.md
                       
 #### []{#extract__surfmesh__...****extract / surfmesh / ...
         
 **** {#extract-surfmesh-... style="font-family: Courier New,Courier,m 
 onospace;"                                                           

 Now creates attributes to hold element local face numbers of 3D input 
 mesh that occur on either side of output mesh face, idface0 and       
 idface1. Now copies user-created node-based attributes from source    
 mesh into extracted sink mesh.                                        
 <https://lagrit.lanl.gov/docs/EXTRACT1.md
                         
 #### []{#addmesh__excavate__...****addmesh / excavate / ...**** {#ad 
 dmesh-excavate-... style="font-family: Courier New,Courier,monospace; 
 "                                                                    

 excavate - remove nodes and elements if they fall with the            
 circumsphere of triangles on the input mesh.
                         
 
                                                                     
 mesh1 must be a 3D mesh (of any geometry) and mesh2 must be a 2D      
 triangular mesh. This command then excavates a volume in mesh1 around 
 mesh2, such that the surface could then be inserted into the 3D mesh  
 (such as to insert a fault into a background terrain mesh). The       
 background mesh, minus the excavated/removed nodes, is put into       
 mesh3. If the optional 
[bfs
] argument is given, the routine will    
 use a breadth-first search algorithm to find nodes to remove, as      
 opposed to the default KD-tree algorithm. If the optional 
[connect
] 
 argument is given, the program will, after excavation, execute an     
 addmesh/append, and then a connect, to produce a fully connected mesh 
 with the surface (mesh2) inserted into the background (mesh1).        
 [https://lagrit.lanl.gov/docs/ADDMESH.md
                           
 ](https://lagrit.lanl.gov/docs/ADDMESH.md)                          
 #### []{#interpolate__...****interpolate / ...
                      
 **** {#interpolate-... style="font-family: Courier New,Courier,monosp 
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
                              
 #### []{#read_...****read /...**** {#read-... style="font-family: Co 
 urier New,Courier,monospace;"                                        

 Three token read implemented. Files are recognized based on their     
 suffix (AVS, GMV, ...) rather than requiring that the second token    
 specify the file type.
                                               
 <https://lagrit.lanl.gov/docs/READ.md
                             
 #### []{#dump_...****dump**** ****/...**** {#dump-... style="font-fa 
 mily: Courier New,Courier,monospace;"                                

 Two and three token dump implemented. Files types (AVS, GMV, ...) are 
 recognized based on their suffix rather than requiring that the       
 second token specify the file type. Two token write does not require  
 a MO name. The default MO is used.
                                   
 <https://lagrit.lanl.gov/docs/DUMP2.md                             
 #### []{#cmo__attribute_union__...****cmo / attribute\_union / ...
  
 **** {#cmo-attribute_union-... style="font-family: Courier New,Courie 
 r,monospace;"                                                        

 Change two meshes so they both share the same set of attributes       
 (taking the union of their sets of attributes)
                       
 <https://lagrit.lanl.gov/docs/cmo_att_derive.md
                   
 #### []{#compute__linear_extrapolate__...****compute / linear\_extra 
 polate / ...
                                                         
 **** {#compute-linear_extrapolate-... style="font-family: Courier New 
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
          
 #### []{#grid2grid_...****grid2grid /...**** {#grid2grid-... style=" 
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
                        
 #### []{#dump__stor__...****dump / stor / ...
                       
 **** {#dump-stor-... style="font-family: Courier New,Courier,monospac 
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
      
 #### []{#pset_...****pset /...**** {#pset-... style="font-family: Co 
 urier New,Courier,monospace;"                                        

 Add option to pset/ / zone for user specified zone id number.
        
 <https://lagrit.lanl.gov/docs/PSET.md
                             
 #### ****dump **/** {#dump style="font-family: Courier New,Courier,mo 
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
                            
 #### **[]{#memory__...**memory / ...****
                            
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

[]{#Release_V2.106

LaGriT V2.106 June 2010
-----------------------

This includes a minor changes and added error checking. The following
are new syntax that are code wrappers for common command calls.

-   cmo/attribute\_union
-   compute / linear\_extrapolate
-   grid2grid/ tree\_to\_fe

    grid2grid/ quadtotri4




------------------------------------------------------------------------

[]{#Release_V2.100

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





