---
GENERATOR: 'Mozilla/4.79C-SGI 
[en
] (X11; U; IRIX64 6.5 IP30) 
[Netscape
]'
Generator: Microsoft Word 97
title: 'Document2-1'
---

 

<div align="right">

**LA-UR-95-3608**



**I.  INTRODUCTION**

 A. [LaGriT](LaGriT.md)

 B.  [Tutorial](Tutoria1.md)

### II.  LAGRIT COMMANDS

 A. [Conventions](conventions.md)

 B.  Alphabetic listing of LaGriT commands

  ------------------------------------------------------------ ------------------------------------------------------------ -------------------------------------------------------------
  [ADDMESH](ADDMESH.md) (join meshes)                        [ASSIGN](ASSIGN.md) (set global variables)                 
  [BOUNDARY](BOUNDAR1.md) (set attributes on surfaces)       [BOUNDARY\_COMPONENTS](BOUNDARY_C.md) (count bc's)         [BUBBLE](bubble.md) (extrude to 3d and extract bndry)
  [CALC\_RDIST](calc_rdist.md) (calculate radial dist.)      [CMO](CMO2.md) (modify mesh object)                        [COLORMAP](COLORMAP.md) (build adjacency map)
  [CONNECT](CONNECT1.md) (make tetrahedral mesh)             [COPYPTS](COPYPTS.md) (copy points)                        [COORDSYS](COORDSY.HTML) (change coordinate system)
  [CREATEPTS](createpts.md) (create points)                  [CREATE\_GRAPH](create_graph.md)(create adjacency graph)   
  [DEFINE](DEFINE.md) (give a name to a number)              [DEREFINE](DEREFINE.md) (merge nodes away)                 [DOPING](DOPING1.md) (set an attribute)
  [DUMP](DUMP2.md) (write output files)                      [DUMP\_RECOLOR](DUMP_RECOLOR.md) (use adjacency map)       
  [EDIT](EDIT2.md) (prints some mesh info)                   [ELTSET](ELTSET2.md) (select, name a set of elements)      [EXTRACT](EXTRACT1.md) (extract a surface)
  [EXTRUDE](extrude.md) (extrude a surface)                                                                               
  [FIELD](FIELD.md) (manipulate a field attribute)           [FILTER](FILTER.md) (filter nodes)                         [FINISH](FINISH.md) (end processing)
  [GENIEE](GENIEE.md) (make element connectivity)            [GEOMETRY](geometry.md) (set the geometry name)            
  [HELP](HELP.md) (print global variable)                    [HEXTOTET](HEXTOTE.md) (convert element types)             
  [INFILE](INPUT.md) (read input from a file)                [INPUT](INPUT.md) (read input from a file)                 [INTERSECT\_ELEMENTS](intersectelements.md) ( ..2 meshes)
  [INTERSECT](INTERSECT.md) (..2 2d meshes to get line)      [INTERPOLATE](main_interpolate.md) (.. attribute values    
  [KDTREE](kdtree.md) (represent mesh as kd-tree)                                                                         
  [LOG](LOG.md) (turn log file off and on)                   [LOOP](loop.md) (execute command many times)               [LOWER\_D](lower_d.md) (create lower dimen. structs.)
  [MASSAGE](MASSAGE.md) (optimize the grid)                  [MATH](MATH.md) (do math on attributes)                    [MERGE](MERGE.md) (remove nodes)
  [METIS](metis.md) (create partition)                       [MODE](MODE.md) (set modes)                                [MREGION](MREGION.md) (define a material region)
  [NEGATIVE\_AIJ](NEGATIVE.md) (test bndry for neg. coef.)                                                                
  [OFFSETSURF](OFFSETSURF.md) (..triangulated surface)                                                                    
  [PSTATUS](PSTATUS.md) (operate on point set)               [PERTURB](PERTURB.md) (perturb node locations)             [PSET](PSET.md) (define, name sets of nodes)
  [QUADXY](QUADXY.md) (define a logical xy node set)         [QUADXYZ](QUADXYZ1.HTML) (define a logical xyz node set)     [QUALITY](QUALITY.md) (evaluate mesh quality)
  [RADAPT](RADAPT.md) (adaptive smoothing)                   [RANKVOLUME](rankvolume.md)(list small volume elements)    [READ](READ.md) (read data)
  [RECON](RECON.md) (swap edges/faces)                       [REFINE](REFINE.md) (refine elements, edges)               [REGION](REGION.md) (define a geometric region)
  [REGNPTS](REGNPTS.md) (distributes nodes in region)        [REORDER](REORDER.md) (reorder nodes in a mesh)            [RESETPTS](RESETPT.md) (reset node values)
  [RM](RM.md) (remove nodes in area)                         [RMMAT](RMMAT.md) (remove a material)                      [RMPOINT](RMPOINT.md) (remove nodes/elements)
  [RMREGION](RMREGION.md) (remove a geometric region)        [RMSPHERE](RMSPHERE.md) (remove nodes in a sphere)         [RMSURF](RMSURF.md) (remove nodes in /on a surface)
  [ROTATELN](ROTATELN.md) (rotate nodes about a line)        [ROTATEPT](ROTATEPT.md)  (rotate nodes about a point)      [RZ](RZ.md) (see createpts)
  [RZAMR](RZAMR.md) (see createpts)                          [RZBRICK](RZBRICK.md) (see createpts)                      [RZRAN](RZRAN.md) (see createpts)
  [RZS](RZS.md) (see createpts)                              [RZV](RZV_LG.md) (see createpts)                           
  [SCALE](SCALE.md) (scale node coordinates)                 [SETPTS](SETPTS.md) (set node type and material)           [SETSIZE](SETSIZE.md) (calc size of space)
  [SETTETS](SETTETS.md) (make child nodes, etc)              [SMOOTH](SMOOTH.md) (node smoothing)                       [SORT](SORT.md) (sort an attribute)
  [STACK](STACK.md) (read,merge surfaces)                    [SURFACE](SURFACE.md) (define a geometric surface)         [SURFPTS](SURFPTS.md) (make nodes on a surface)
  [TRANS](TRANS.md) (translate node coordinates)             [TRIANGULATE](TRIAGN.md) (make triangles)                  
  [UNG2AVS](UNG2AVS.md) (UNGenerate to AVS)                                                                               
  [ZQ](ZQ.md) (see [cmo/setatt](cmo_setatt.md))                                                                         
  ------------------------------------------------------------ ------------------------------------------------------------ -------------------------------------------------------------

 C.   LaGriT commands by category:

  Geometry commands:

  [SURFACE](SURFACE.md) (define a geometric surface)

  [REGION](REGION.md) (define a geometric region)

  [MREGION](MREGION.md) (define a material region)

  [GEOMETRY](geometry.md) (set the geometry name)

  [OFFSETSURF](OFFSETSURF.md) (..triangulated surface)
 
  Point Placement:

  [CREATEPTS](createpts.md) (create points)

  [QUADXY](QUADXY.md) (define a logical xy node set)

  [QUADXYZ](QUADXYZ1.HTML) (define a logical xyz node set)

  [REGNPTS](REGNPTS.md) (distributes nodes in region)

  [SURFPTS](SURFPTS.md) (make nodes on a surface)

  [COPYPTS](COPYPTS.md) (copy existing points)
 
  Point Modification and Selection:

  [PSET](PSET.md) (define, name sets of nodes)

  [TRANS](TRANS.md) (translate node coordinates)

  [SCALE](SCALE.md) (scale node coordinates)

  [ROTATEPT](ROTATEPT.md)  (rotate nodes about a point)

  [ROTATELN](ROTATELN.md) (rotate nodes about a line)

  [RMPOINT](RMPOINT.md) (remove nodes/elements)

  [FILTER](FILTER.md) (filter nodes)

  [COORDSYS](COORDSY.HTML) (change coordinate system)

  [COPYPTS](COPYPTS.md) (copy existing points)

  [PERTURB](PERTURB.md) (perturb node locations)

  [RM](RM.md) (remove nodes in area)

  [RMMAT](RMMAT.md) (remove a material)

  [RMREGION](RMREGION.md) (remove a geometric region)

  [RMSPHERE](RMSPHERE.md) (remove nodes in a sphere)

  [RMSURF](RMSURF.md) (remove nodes in /on a surface
 
  Connecting the Mesh:

  [SETPTS](SETPTS.md) (set node type and material)

  [CONNECT](CONNECT1.md) (make tetrahedral mesh)

  [SETTETS](SETTETS.md) (make child nodes, set element material)

  [RZBRICK](RZBRICK.md) (create a brick, hex mesh)

  [TRIANGULATE](TRIAGN.md) (make triangles)

  [GENIEE](GENIEE.md) (make element connectivity)
 
  Element Modification and Selection:

  [ELTSET](ELTSET2.md) (select, name a set of elements)

  [RMPOINT](RMPOINT.md) (remove nodes/elements)
 
  Creating, modifying, assessing and deleting mesh objects and their
  attributes:

  [CMO](CMO2.md) (modify mesh object)

  [COPYPTS](COPYPTS.md) (copy points)

  [EXTRUDE](extrude.md) (extrude a surface)

  [FIELD](FIELD.md) (manipulate a field attribute)

  [INTERPOLATE](main_interpolate.md) (interpolate attribute values
  from nodes or elements )

  [INTERSECT](INTERSECT.md) (..2 2d meshes to get line)

  [INTERSECT\_ELEMENTS](intersectelements.md) ( ..2 meshes)

  [LOWER\_D](lower_d.md) (create lower dimen. structs.)

  [EXTRACT](EXTRACT1.md) (extract a surface)

  [MATH](MATH.md) (do math on attributes)

  [QUALITY](QUALITY.md) (evaluate mesh quality)

  [RANKVOLUME](rankvolume.md)(list small volume elements)

  [SORT](SORT.md) (sort an attribute)

  [KDTREE](kdtree.md) (represent mesh as kd-tree)

  [ADDMESH](ADDMESH.md) (join meshes)

  [BOUNDARY](BOUNDAR1.md) (set attributes on surfaces)
 
  Optimize or customize the mesh:

  [MASSAGE](MASSAGE.md)(optimize the grid)

  [REFINE](REFINE.md) (refine elements, edges)

  [DEREFINE](DEREFINE.md) (merge nodes away)

  [SMOOTH](SMOOTH.md) (node smoothing)

  [RADAPT](RADAPT.md) (adaptive smoothing)

  [MERGE](MERGE.md) (remove nodes)

  [HEXTOTET](HEXTOTE.md) (convert element types)

  [RECON](RECON.md) (swap edges/faces)

  [MODE](MODE.md) (set modes)
 
  Input/Output:

  [READ](READ.md) (read data)

  [DUMP](DUMP2.md) (write output files)

  [STACK](STACK.md) (read,merge surfaces)

  [UNG2AVS](UNG2AVS.md) (UNGenerate to AVS)

  [LOG](LOG.md) (turn log file off and on)

  [INPUT](INPUT.md) (read input from a file)

  [ASSIGN](ASSIGN.md) (set global variables)
 
    

### III. MESH OBJECTS

 A. [Mesh Object Definition](meshobject.md)

 B.[Command Interface](commandi.md)

 C. [Fortran Interface](fortran.md)

 D. [Mesh Object Connectivity](meshobjcon.md)

 E. [Geometries](geometries.md)

### IV.  INTERFACING USER ROUTINES TO LAGRIT

  A. [Building an Executable and Running LaGriT](build.md)

  B. [Issuing Commands from a User Program](issuing.md)

  C. [Writing User Commands](writing.md)

  D. [Accessing the Mesh Object](accessing.md)

  E.  Utility Subroutines

 1
. [Memory Manager](memmang.md)(mmgetblk,mmrelblk,mmrelprt,mmincblk,

      mmfindbk,mmgettyp,mmgetlen,mmgetnam,mmprint,mmverify,mmggetbk)

 2. [Mesh Objects](meshob.md) (cmo\_create, cmo\_get\_info,
 cmo\_set\_info, cmo\_get\_name,

       cmo\_set\_name, cmo\_get\_attribute\_name, cmo\_newlen,
 cmo\_get\_intinfo,

       cmo\_release, cmo\_get\_attinfo, cmo\_get\_length,
 cmo\_set\_attinfo,

       cmo\_get\_attparam)

 3. [Point Selection](pointsel.md) (getptyp, unpackpc, unpacktp)

 4. [Character Length](charlen.md)  (icharln, icharlnf, icharlnb,
 nulltoblank\_lg)

 5. [Retrieving Point Sets and Element Sets ](retpts.md) (eltlimc,
 pntlimc, pntlimn)

 6. [Array Compression](arrcomp.md) (kmprsm, kmprsn, kmprsnr,
 kmprsnrrr, kmprsp, kmprspr, kmprsz, kmprszr)

 7. [Array Sorting](arrsort.md) (hpsort, hpsort1, hpsorti, hpsortim,
 hpsortimp, hpsortip, hpsortrmp)

 8. [Miscellaneous](miscell.md)
 (setsize,set\_user\_bounds,inside,volume\_element, user\_interpolate)

 9.[Geometry Information](geom.md)(geom\_lg.h, get\_material\_number)

### V.  ERRORS

A. [Errors in parsing or executing commands](errors.md#parse).

B.  [Out of memory errors.](errors.md#memory)

C.  [Fatal memory management errors](errors.md#panic) (PANIC!)

  

### [LAGRIT REFERENCES:](References.md)

 **LaGriT User's Manual**

**Text to Search For: **

**Boolean: **ANDOR

**Case **InsensitiveSensitive

------------------------------------------------------------------------

  

 [Return to LaGriT Home Page](../index.md)

------------------------------------------------------------------------

 ***These pages are maintained by Denise George at
 <dgeorge@lanl.gov***
