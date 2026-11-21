---
GENERATOR: 'Mozilla/4.05C-SGI [en] (X11; I; IRIX 6.5 IP32) [Netscape]'
Generator: Microsoft Word 98
title: 'DUMP/FEHM and DUMP/STOR'
---

# DUMP/ fehm
# DUMP/ stor

Output a set of files that are of general use but are specifically
designed for the [FEHM](http://fehm.lanl.gov) porous flow and transport
code. Both commands **dump/fehm** and **dump/stor** use the same set of optional settings after the required file root name and cmo name.

The keyword **fehm** writes the full set of 7 FEHM files.

The keyword **stor** writes a single FEHM sparse matrix stor file.

## OUTPUT FILES:

The *root* will be used to form full names for the files that are written:
```
rootname.fehmn              rootname_interface.zone      rootname_outside_vor.area     
rootname_material.zone      rootname_multi_mat.zone      rootname_outside.zone       rootname.stor
```


1.  **.fehmn** is a list of
    mesh object node x,y,z values and the element connectivity list in ASCII FEHM format (also dump/coord/... command).
2.  **\_material.zone**
    is a file with a node list for each integer material (imt) value (also dump/zone/imt/... command).
3.  **\_interface.zone**
    is output of FEHM zone format files of nodes along an interface
    where interface is defined as a node to node connection where the
    integer attribute imt changes. *Note this file will have 0 length if
    there is only one material.*
4.  **\_multi_mat.zone**
    is a list of multi-material connections,
    where multi-material is defined as a node to node connection where
    the integer attribute imt changes. Each list consists of a header
    followed by the list of node pairs. The header consists of the
    material number followed by "multi-material connections" on the same
    line, followed by "nnum" (nedges) on the next line, followed by the
    number of entries in the list. The list of node pairs are written
    where first node is inside the current material and the second is
    the connected node on other side of material interface. The lists
    are sorted by the first node. *Note this file will have 0 length if
    there is only one material.*
5.  **\_outside.zone**
    is a list of each node located on the outside areas. 

    If the keyword **keepatt** is included, then 6 node based attributes are added to
    the mesh object with the names top, bottom, left\_w, right\_e,
    back\_n, and front\_s. Note that a node can belong to more than 1
    zone list. For example, in an orthogonal cube aligned with the
    coordinate axes, a corner node can belong to 3 zone lists (e.g.
    front\_s, top and left\_w lists). By default, these node
    attributes are deleted after the zone file is written and the mesh
    object remains unchanged. (also dump/zone\_outside/... command).
    
    These outside zones are defined as 6 possible external boundaries for
    rectangular geometries and are defined as:

```
    1 = top = top = positive z direction (0,0,1)

    2 = bottom = bottom = negative z direction (0,0,-1)

    3 = left\_w = left or west = negative x direction (-1,0,0)

    4 = front\_s = front or south = negative y direction (0,-1,0)

    5 = right\_e = right or east = positive x direction (1,0,0)

    6 = back\_n = back or north = positive y direction (0,1,0)

 ```
 
6.  **\_outside_vor.area** (default) or **\_outside_med.area**
    (with **cv_dual** also **\_outside_hyb.area**, named by the
    control volumes; see [cv_dual OR matbld](#cv_dual-or-matbld))
    uses outside nodes to write a list of
    2D area or 1D length vectors (Ax\_i,Ay\_i,Az\_i) associated with
    each and listed the same order as zones and nodes in the file root_name_outside.zone.

    optional keyword **keepatt\_voronoi** will keep the attributes created for outside voronoi areas.
    Three node attributes (xn\_varea, yn\_varea, zn\_varea)
    are added and they contain the vector area associated with the
    voronoi areas for each of the nodes located on their external
    triangles.

    optional keyword **keepatt\_median** will keep the attributes created for outside median areas.
    Three node attributes (xn\_marea, yn\_marea, zn\_marea)
    are added and they contain the vector area associated with the
    median area for each of the nodes located on their external
    triangles. These area vectors are computed by computing the median
    mesh (triangle centroids connected to triangle edge centers)  If the
    input mesh are 2D triangles, the median length of external edges
    incident upon a node are written. 
7.  **.stor**
    is geometric coefficient matrix written in FEHM stor file format.
    These are the Voronoi (control volume) area and volume associated
    with each node and the sparse matrix structure. 
    ASCII compressed is the default file output.

    The **stor** file represents a sparse coefficient matrix and is used for
    solving PDE on a triangular or tetrahedral Delaunay mesh. The stor
    format is written for FEHM input and is described at this page:   
    [FEHM STOR File Format](../../STOR_Form.md)

    **Large coordinates:** every stor writer (2D and 3D, every
    compression option, hybrid) computes the geometry of each cell
    relative to one of its vertices, so a mesh at UTM-sized
    coordinates (x about 7e5, y about 3.2e6) gets the same volumes and
    coefficients as the same mesh at the origin (since V3.4.0; before,
    the 3D default gave wrong volumes and coefficients there).
    Checked by the suite test/suite/test_stor_utm.

    **17 digits:** the ASCII .stor, the .uge (dump / pflotran) and the
    coordinates of the .fehmn file are written with 17 significant
    digits (1pe24.16), so a value read back is the double LaGriT
    computed (since V3.4.0; before, 13 digits: at UTM coordinates a
    vertex was rounded to 1e-6). FEHM and PFLOTRAN read these files
    by white space, so the wider fields need no change there.



## FORMAT:

### Simple:

 **dump** / **fehm** / root\_name / cmo\_name
 
 **dump** / **stor** / root\_name / cmo\_name

 
 The root\_name will be used to form names for all files written.

 **dump/fehm** will write full set of FEHM input files including a stor file

 **dump/stor** will write a sparse matrix stor file only (default is ASCII compressed scalar)

 
### Optional Keywords:

The following command settings are optional and can occur in any order after the cmo\_name.

 

**ascii** or **binary**  indicate IO Mode Options for the stor file. Default is ascii.


**scalar**,  **vector**,  **both**,  **area\_scalar**,  **area\_vector**, or **area\_both** are Area Coefficient Options for writing stor file coefficient values. Default is scalar.
 

**all**,  **graph**,  **coefs**, or  **none** are Compression Options for the stor file. Default is all.


**delatt** or  **keepatt**  deletes or keeps CMO Attributes created to find outside zone nodes. Default is delatt.


**hybrid** or **nohybrid** Specify whether hybrid median-Voronoi control volumes should be used. Default is nohybrid. With **cv_dual** (the default), **hybrid** is cv_dual's hybrid; with **matbld**, matbld's.


**cv_dual** or **matbld** choose the code that computes the control volumes, the coefficients and the boundary areas. Default is cv_dual (since V3.4.0; before, matbld). **matbld** gives the files LaGriT wrote before. See [cv_dual OR matbld](#cv_dual-or-matbld).


**nofilter_zero**, with **cv_dual**: keep the zero-area connections in the .stor (one per mesh edge). Default: they are left out.


**voronoi**, **median** or **hybrid**, with **cv_dual**: the kind of control volumes. Default is voronoi.
 


#### STOR IO MODES:

   
   **binary**             Output sparse matrix stor file in Fortran unformatted format *Note: These files are platform dependent.* 

   **ascii** (default)    Output sparse matrix stor file as ASCII format. This is a larger file but not machine dependent.


#### STOR Area Coefficient OPTIONS:


   **scalar** (default)    Area/distance               coefficients are output as scalars 
   
   
   **vector**              Area/distance               coefficients are output as vectors 
   
   
   **both**                Area/distance               coefficients are output as scalars and vectors 
   
   
   **area\_scalar**        Area                        coefficients are output as scalars
   
   
   **area\_vector**        Area                        coefficients are output as vectors 
   
   
   **area\_both **         Area                        coefficients are output as scalars and vectors 

 


#### STOR Compression OPTIONS:


**all** (default)   (\_astor) compression of area coefficients and compression of coefficient indices
 

**graph**           (\_gstor) compression of area coefficient indices (edge compression)  



**coefs**           (\_cstor) compression of area coefficient to a list of unique values. *Note: This older algorithm uses more space and time.*


**none**            (\_nstor) full indices and area coefficient list. *Note: This older algorithm uses more space and time.*


*With **matbld** only.* With cv_dual (the default since V3.4.0) these two attributes are not made: use **matbld**, or [dump/control_volume](DUMP_CONTROL_VOLUME.md) with its **ccoef_\*** keywords (the smallest and largest area, coefficient and length of each vertex's connections, and the vertex at the other end).

The attributes ccoef and ij\_ccoef may be added if negative area coefficients are detected when the \_astor or \_gstor compression algorithms are invoked (default) or by using the **all** or **graph** keywords, these two new attributes are added to the CMO. Since the area coefficients are really edge based quantities but we only have access to node and element quantities, the following convention is used.


If any area coefficient is negative the integer node array ij\_ccoef and real node array ccoef are created. All connections with area coefficients >= 0 are set to zero in these attributes, so values will range from the largest negative value to 0. This is not implemented for 2D and there is no option to turn this feature on or off.

If no area coefficients are negative the arrays are not created.

If the area coefficient A\_ij, between nodes i and j is negative then
```
ccoef(i)    = ccoef(j) = A\_ij
ij\_ccoef(i) = j
ij\_ccoef(j) = i
```
                                                     
 *Note: The old syntax using the alternate\_scalar keyword is now the
 default option using scalar and compressed. It is the same as using keywords
 all or graph and if used will be recognized.*


#### CMO Attributes for outside zones:

 
**delatt** (default)  No new cmo attributes are created.

   
**keepatt** Six node attributes are created (top, bottom, left\_w, right\_e,back\_n, front\_s) which are assigned values according to the direction of the octant of their normal vector. Each outside node can occur in multiple directions. For instance an edge between top and left side will be in both top and left\_w attributes. Attribute values greater than 0 are nodes found in that direction.

   

#### STOR Hybrid OPTIONS:

   
**nohybrid** (default)   Do not use hybrid median-Voronoi control volumes


**hybrid**               Use hybrid median-Voronoi control volumes.
 

 These hybrid volumes represent a means for addressing poorly shaped
 tetrahedra on a boundary of the mesh. Boundary tetrahedra whose
 Voronoi centers are outside the mesh lead to incorrect modeling
 results. Alternatively, if we construct control volumes using medians
 (centroids), the center point for each element always lies within that
 element, but median meshes lack other nice properties of Voronoi
 meshes.    

 As a compromise between the median and Voronoi approaches, we start
 with a Voronoi mesh and fix boundary tetrahedra whose circumcenters
 are not contained within the boundary of the mesh. To fix such an
 element, we draw a line segment from the median center to the Voronoi
 center and find the point at which this segment intersects the surface
 of the element. This intersection then becomes the center point for
 the purposes of determining control volumes. Essentially we move the
 Voronoi center toward the median point until it just reaches the
 element.   

 When we use the hybrid approach, we also make a slight change to the
 way we calculate the area coefficients. Voronoi control volumes have
 the property that their faces are always perpendicular to the mesh
 edges that intersect them. With hybrid control volumes (as well as
 median control volumes), this is not the case. To compensate for this,
 when we compute the area coefficient we only consider the component of
 the face area vector which is in the same direction as the edge. We
 accomplish this by taking the area vector and dotting it with a unit
 vector in the direction of the edge.    

 The **hybrid** option may lead to poor results if it is applied to a
 mesh that is non-Delaunay, because there may be elements which it
 cannot fix, such as interior elements whose circumcenters are outside
 the mesh. A warning will be printed if the code detects that the mesh
 appears to be non-Delaunay.    

 *NOTE: The hybrid option is only available with the default implmentation using no other options or including **all**, **graph**, and **scalar** in the command.*

 With **cv_dual** (below), **hybrid** means cv_dual's hybrid instead: it
 starts as this one does (a boundary cell whose circumcenter lies
 outside has its point moved onto its boundary face), then checks
 every edge and moves cell points toward their centroids, only as far
 as needed, until no coefficient is negative. It works in 2D, 3D and
 on fracture networks, and with every option cv_dual has.


#### cv_dual OR matbld:

**cv_dual** (the default since V3.4.0) and **matbld** (the default
before) compute the control volumes, the coefficients and the boundary
areas. Without either word, a mesh whose cells are not all triangles
or all tets (hexes, prisms, a mixed mesh) goes to matbld, with one
line saying so: cv_dual has no other cells. Every dump says which code
wrote it, in a line such as `dump/stor: control volumes by cv_dual
(voronoi)`.

**matbld** is LaGriT's sparse matrix code described above, unchanged:
name it to get the files LaGriT wrote before V3.4.0, its ccoef and
ij\_ccoef attributes, or an option cv_dual does not have. matbld
leaves out connections whose coefficient is below compress\_eps
(1e-8) times the largest, and the control volumes of their vertices
lose those connections' shares (on 308 random points in a box, 8
connections and volumes off by up to 3e-5 of a vertex's volume). To
keep them, give the mesh object a smaller compress\_eps:

```
cmo / addatt / mo / compress_eps / REAL / scalar / scalar / constant / permanent / / 1.0e-14
```

cv_dual keeps every connection that is not zero-area.

**cv_dual** computes the control volumes, their geometric coefficients
and the boundary areas with cv_dual, the code of the command
[dump/control_volume](DUMP_CONTROL_VOLUME.md) (its
[manual](../../cv_dual/cv_dual_manual.md)), on the mesh object's own
arrays. The mesh must be all triangles (2D, or a fracture network) or
all tets. cv_dual first checks the mesh (cell types, positive
area/volume, conformity, orientation, no parent/child chains) and,
for every kind of control volume but median, the Delaunay property. A mesh it refuses
gives cv_dual's message and an ERROR line, and nothing is written:
with **dump/fehm** not the geometry and zone files either. Without
either word the message also names **matbld** (the file matbld wrote
before) and, for a mesh that is not Delaunay, **median**. The facets the Delaunay check flags (beyond its tolerance,
and the few slightly off that it accepts with a WARNING) go to
file\_name\_violations.vtu, beside the dump's files, for ParaView.

The kind of control volumes, with **cv_dual**:

* **voronoi** (default): the Voronoi control volumes.
* **median**: the median control volumes (cell centroids). The mesh
  need not be Delaunay. Only with **cv_dual**.
* **hybrid**: cv_dual's hybrid, no negative coefficients (above).

The other options, with **cv_dual**:

| option | with cv_dual |
|---|---|
| **all**, **coefs** | the .stor compressed (cv_dual's compress on) |
| **graph**, **none** | not compressed (compress off) |
| **scalar** (default) | as is |
| **dump/stor**, **dump/fehm**: zero-area connections | left out (matbld leaves them out in 3D); **nofilter_zero** keeps them, one connection per mesh edge |
| **dump/pflotran**: **filter_zero** (default), **nofilter_zero** | zero-area connections left out, or kept |
| **vector**, **both**, **area_scalar**, **area_vector**, **area_both**, **binary**, **unformatted**, pflotran's **verbose** | cv_dual does not have these: matbld writes this dump, with one line saying so |
| **keepatt_voronoi**, **keepatt_median** | keep the boundary-area attributes, named by the control volumes: xn_varea, xn_marea or xn_harea (and y, z) |

What is written with **cv_dual**:

* **.stor** (**dump/stor**, **dump/fehm**) and **.uge**
  (**dump/pflotran**): cv_dual's, with 17 significant digits. The
  .stor's first line names the control volumes (Voronoi, Median or
  Hybrid Coefficients); its second line ends with the command, in
  parentheses: `(cv_dual dump / stor / root / mo / cv_dual)`.
* **\_outside\_vor.area**, **\_outside\_med.area**,
  **\_outside\_hyb.area** (**dump/fehm**, **dump/zone_outside**):
  each outside vertex's boundary vector area is the part of its own
  control volume on the mesh boundary, from the same points as the
  coefficients. Voronoi splits each boundary triangle at its
  circumcenter, median gives each vertex a third, as matbld does; the
  numbers equal matbld's to rounding. Hybrid uses the points the
  hybrid moved. On each side of a box the vertices' areas add up to
  the side's area, for every kind. In 2D and on a fracture's edges,
  each vertex gets half of each boundary edge, as a length vector.
* The other files (.fehmn and the zone files) as matbld writes them.

`dump / control_volume` writes the same files and more (ParaView
files, a report) with every cv_dual keyword; its **outside_area**
keyword lists the boundary areas of every boundary vertex in one file.

Errors (an ERROR line, nothing written): **median** with
**matbld**; **cv_dual** with **matbld**; two of **voronoi**,
**median**, **hybrid**; **median** with an option cv_dual does not
have (matbld, which would write it, has no median).


## EXAMPLES:

```
 dump / fehm / file_name / cmo   (write all FEHM files, STOR file will be ascii compressed) 
 dump / stor / file_name / cmo   (write the FEHM STOR file in ascii compressed format) 
 dump / fehm / file_name / cmo / binary / none (write all FEHM files, STOR file will be unformatted compressed) 
 dump / stor / file_name_as / cmo / ascii / none / vector (write ascii STOR file with vector coefficients) 
 dump / stor / file_name_as / cmo / ascii / none / area_scalar (ascii STOR file with area coefficients without distance) 
```

cv_dual (the default) and matbld:

```
 dump / stor / file_name / cmo   (cv_dual's Voronoi .stor, ascii compressed, zero-area connections left out)
 dump / stor / file_name / cmo / nofilter_zero   (the same, every mesh edge a connection)
 dump / stor / file_name / cmo / matbld   (the .stor LaGriT wrote before V3.4.0, with ccoef and ij_ccoef)
 dump / fehm / file_name / cmo / hybrid   (all FEHM files; the .stor and file_name_outside_hyb.area from cv_dual's hybrid)
 dump / fehm / file_name / cmo / median / keepatt_median   (median control volumes; keep xn_marea, yn_marea, zn_marea)
 dump / pflotran / file_name / cmo / nofilter_zero   (cv_dual's .uge, zero-area connections kept)
 dump / zone_outside / file_name / cmo   (the outside zones, and Voronoi areas from cv_dual)
 dump / stor / file_name / cmo / vector   (cv_dual has no vector coefficients: matbld writes this .stor, and says so)
```

## OLD EXAMPLES (recognized but no longer used):

```
  dump / stor / file_name_as / cmo / ascii  / / alternate_scalar 
  dump / stor / file_name_as / cmo / asciic / / alternate_scalar 
  dump / fehm / file_name / cmo / binaryc / / alternate_scalar/ keepatt 
```



