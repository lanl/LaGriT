---
GENERATOR: 'Mozilla/4.05C-SGI [en] (X11; I; IRIX 6.5 IP32) [Netscape]'
Generator: Microsoft Word 98
title: 'DUMP/FEHM and DUMP/STOR'
---

**DUMP/ fehm** and **DUMP/ stor**

Output a set of files that are of general use but are specifically
designed for the [FEHM](http://fehm.lanl.gov)porous flow and transport
code.

The simplest command line with root file\_name and cmo\_name will assume
the most common options and will write an ASCII compressed stor file
with scalar area coefficients. If an option is not valid, the program
will continue but use a valid default setting. Both
**[dump/fehm]{style="font-family: Courier New,Courier,monospace;"** and
**dump/stor** use the same set of optional settings after the required[file\_name]{style="font-family: Courier New,Courier,monospace;". 

Though the syntax for both are the same, with command[stor]{style="font-weight: bold;", only the stor file is written. With
command [fehm]{style="font-weight: bold;", the full set of 7 FEHM files
are written. The full set of fehm files will look like the following,
where "[file\_name]{style="font-style: italic; is the root of each
name:

    file_name.fehmn              file_name_interface.zone      file_name_outside_vor.area     
    file_name_material.zone      file_name_multi_mat.zone      file_name_outside.zone       file_name.stor

1.  **[file\_name]{style="font-style: italic;".fehmn** is a list of
    mesh object node x,y,z values and element connectivity list in ASCII
    FEHM format (also dump/coord/... command).
2.  [file\_name]{style="font-style: italic; font-weight: bold;"[\_material.zone]{style="font-weight: bold;"
    is node list for each integer material (imt) value (also
    dump/zone\_imt/... command).
3.  [file\_name]{style="font-style: italic; font-weight: bold;"[\_interface.zone]{style="font-weight: bold;"
    is output of FEHM zone format files of nodes along an interface
    where interface is defined as a node to node connection where the
    integer attribute imt changes. Note this file will have 0 length if
    there is only one material.
4.  [file\_name]{style="font-style: italic; font-weight: bold;"[\_multi\_mat.zone]{style="font-weight: bold;"
    is output of FEHM zone format files of multi-material connections,
    where multi-material is defined as a node to node connection where
    the integer attribute imt changes. Each list consists of a header
    followed by the list of node pairs. The header consists of the
    material number followed by "multi-material connections" on the same
    line, followed by "nnum" (nedges) on the next line, followed by the
    number of entries in the list. The list of node pairs are written
    where first node is inside the current material and the second is
    the connected node on other side of material interface. The lists
    are sorted by the first node. Note this file will have 0 length if
    there is only one material.
5.  [file\_name]{style="font-style: italic; font-weight: bold;"[\_outside.zone]{style="font-weight: bold;"
    is a list of each node associated with each of six possible outside
    areas.

    The outside zones are defined as 6 possible external boundaries for
    rectangular geometries and are defined as:

    <div style="margin-left: 40px;"

    1 = top = top = positive z direction (0,0,1)

    2 = bottom = bottom = negative z direction (0,0,-1)

    3 = left\_w = left or west = negative x direction (-1,0,0)

    4 = front\_s = front or south = negative y direction (0,-1,0)

    5 = right\_e = right or east = positive x direction (1,0,0)

    6 = back\_n = back or north = positive y direction (0,1,0)

    

    If keepatt is specified, then 6 node based attributes are added to
    the mesh object with the names top, bottom, left\_w, right\_e,
    back\_n, and front\_s. Note that a node can belong to more than 1
    zone list. For example, in an orthogonal cube aligned with the
    coordinate axes, a corner node can belong to 3 zone lists (e.g.
    front\_s, top and left\_w lists). If
    [delatt]{style="font-weight: bold;" (default) is specified, these
    attributes are deleted after the zone file is written and the mesh
    object remains unchanged. (also dump/zone\_outside/... command).
6.  [file\_name]{style="font-style: italic; font-weight: bold;"[\_outside\_vor.area]{style="font-weight: bold;"
    (default) or
    [file\_name]{style="font-style: italic; font-weight: bold;"[\_outside\_med.area]{style="font-weight: bold;"
    is written after the outside zone nodes are identified and a list of
    2D area or 1D length vectors (Ax\_i,Ay\_i,Az\_i) associated with
    each and listed the same order as zones and nodes in the file
    [file\_name]{style="font-style: italic;"\_outside.zone.

    If the keyword [keepatt\_voronoi]{style="font-weight: bold;" is
    specified, three node attributes (xn\_varea, yn\_varea, zn\_varea)
    are added and they contain the vector area associated with the
    voronoi areas for each of the nodes located on their external
    triangles.

    If the keyword [keepatt\_median]{style="font-weight: bold;" is
    specified, three node attributes (xn\_marea, yn\_marea, zn\_marea)
    are added and they contain the vector area associated with the
    median area for each of the nodes located on their external
    triangles. These area vectors are computed by computing the median
    mesh (triangle centroids connected to triangle edge centers)  If the
    input mesh are 2D triangles, the median length of external edges
    incident upon a node are written. (also dump/zone\_outside/...
    command)
7.  [file\_name]{style="font-style: italic; font-weight: bold;"[.stor]{style="font-weight: bold;"
    is output of FEHM format file with geometric coefficient matrix,
    these are the Voronoi (control volume) area and volume associated
    with each node and the sparce matrix structure Default format is
    ASCII compressed.

    The stor file represents a sparse coefficient matrix and is used for
    solving PDE on a triangular or tetrahedral Delaunay mesh. The stor
    format is written for FEHM input and is described at this page:   
    [FEHM STOR File Format](../../STOR_Form.md)



**FORMAT:**

 **dump / fehm  stor **/ file\_name / [cmo\_name]

 

 

 

 The file\_name will be used as a root name for all files written.

 **dump/fehm** will write full set of FEHM input files and stor file

 **dump/stor** will write a sparse matrix stor file only

 Default options applied are: ASCII, scalar, all compressed
 

 The following command settings are optional and can occur in any order
 after the cmo\_name.

 

  / [ **ascii  binary** ]  

  / [ **scalar  vector  both  area\_scalar  area\_vector 
 area\_both**]

   / [ **all  graph  coefs  none** ]

  / [ **delatt,  keepatt** ]

 / [ **hybrid, nohybrid** ]
 IO Mode Options for stor file

 Area Coefficient Options for writing stor file coefficient values

 Compression Options for the stor file

 CMO Attribute created during creation of outside zone files

 Specify whether hybrid median-Voronoi control volumes should be used


IO MODE OPTIONS for STOR File:

   ---------------------- ---------------------------------------------------------------
   **binary**             Output sparse matrix stor file in Fortran unformatted format

                          Note: These files are platform dependent. 

   **ascii** (default)    Output sparse matrix stor file as ASCII format 
   ---------------------- ---------------------------------------------------------------

 

 *Note: The old syntax using asciic and binaryc keywords are no longer
 needed to toggle the compression settings.*


Area Coefficient OPTIONS for STOR File:

   ----------------------- --------------------------------------------------------------------------------
   **scalar** (default)    Area/distance               coefficients are output as scalars 
   **vector**              Area/distance               coefficients are output as vectors 
   **both**                Area/distance               coefficients are output as scalars and vectors 
   **area\_scalar **       Area                           coefficients are output as scalars
   **area\_vector**        Area                           coefficients are output as vectors 
   **area\_both **         Area                           coefficients are output as scalars and vectors 
   ----------------------- --------------------------------------------------------------------------------

 


Compression OPTIONS for STOR File:

   ------------------- -------------------------------------------------------------------------------------
   **all** (default)   (\_astor) compression of area coefficients and compression of coefficient indices  

   **graph**           (\_gstor) compression of area coefficient indices (edge compression)  

   oefs**           (\_cstor) compression of area coefficient to a list of unique values

                       Note: This older algorithm uses more space and time.  

   **none**            (\_nstor) full indices and area coefficient list

                       Note: This older algorithm uses more space and time.  
   ------------------- -------------------------------------------------------------------------------------

 

 *Note: The old syntax using the alternate\_scalar keyword is now the
 default option of scalar compressed. It is the same as using keywords
 all or graph and if used will be recognized.*


CMO Attribute OPTIONS:

   ------------------------- ------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------
   **delatt** (default)
     No new cmo attributes are created.

   
                         

   **keepatt**
              Used for the outside zone file, six node attributes are created (top, bottom, left\_w, right\_e,back\_n, front\_s) which are assigned values according to the direction of the octant of their normal vector. 

   
                           

   NOTE: ccoef, ij\_ccoef
   When the \_astor or \_gstor compression algorithms are invoked by default or by using the [all]{style="font-family: Courier New,Courier,monospace;" or [graph keywords, ]{style="font-family: Courier New,Courier,monospace;"[two new attributes]{style="font-family: Times New Roman,Times,serif;" are created, ccoef and ij\_ccoef, if and only if there are some negative area coefficients. Since the area coefficients are really edge based quantities but we only have access to node and element quantities, the following convention is used.

                             

                             If any area coefficient is negative the integer node array ij\_ccoef and real node array ccoef are created.

                             If no area coefficients are negative the arrays are not created.

                             If the area coefficient A\_ij, between nodes i and j is negative then

                             [ccoef(i)    = ccoef(j) = A\_ij]{style="font-family: Courier New,Courier,monospace;"

                             [ij\_ccoef(i) = j]{style="font-family: Courier New,Courier,monospace;"

                             [ij\_ccoef(j) = i]{style="font-family: Courier New,Courier,monospace;"

                             

                             All connections with area coefficients &gt;= 0 are set to zero.

                             

                             There is no option to turn this feature on or off.

   ------------------------- ------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------

Hybrid OPTIONS:

   ------------------------ --------------------------------------------------
   **nohybrid** (default)   Do not use hybrid median-Voronoi control volumes
   **hybrid**               Use hybrid median-Voronoi control volumes.
   ------------------------ --------------------------------------------------

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

 NOTE: The hybrid option is only available with the **all** (default)
 and **graph** compression options. It also requires the **scalar**
 (default) area coefficient option.

EXAMPLES

     dump / fehm / file_name / cmo   (write all FEHM files, STOR file will be ascii compressed) 
     dump / stor / file_name / cmo   (write the FEHM STOR file in ascii compressed format) 
     dump / fehm / file_name / cmo / binary / none (write all FEHM files, STOR file will be unformatted compressed) 
     dump / stor / file_name_as / cmo / ascii / none / vector (write ascii STOR file with vector coefficients) 
     dump / stor / file_name_as / cmo / ascii / none / area_scalar (ascii STOR file with area coefficients without distance) 

OLD EXAMPLES (recognized but no longer used)

     dump / stor / file_name_as / cmo / ascii  / / alternate_scalar 
     dump / stor / file_name_as / cmo / asciic / / alternate_scalar 
     dump / fehm / file_name / cmo / binaryc / / alternate_scalar/ keepatt 


