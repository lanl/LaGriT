---
title: EXTRACT/SURFMESH
---

# EXTRACT / SURFMESH


  This routine extracts the boundary of a mesh. If the original mesh
  is a solid mesh, it extracts the surface mesh. If it is a surface
  mesh, it extracts the edge mesh. If the interface elements have
  "parent-child" nodes, then only the parent nodes and elements are
  extracted. 

Not all attributes in the input mesh object are created
  or initialize in the output mesh object. Among the array-valued
  attributes, only xic,yic,zic,itet,jtet,itetoff,itettyp, and icr1
  are set. The node attribute imt is set to 1. The icontab array is copied from the
  input mesh object to the output mesh object.


Note:  the **itp** array of the input mesh object must be correctly set. 
  Use the command [**`resetpts/itp`**](../RESETPTS.md) which will identify nodes on the outside and also on material interfaces for multi-value itetclr values. 
See more about this attribute at [Mesh Object](../meshobject.md).

 

## SYNTAX

<pre>

 <b>extract / surfmesh</b> / 1,0,0 /cmo_out /[ cmo_in ][ <b>external</b> ]

</pre>

`1,0,0` is the range convention where the integers represent start,stop,stride and 1,0,0 represents all. The **`pset`** or **`eltset`** selections are also allowed.

`cmo_out` is the name of the extracted mesh object.

`cmo_in` is optional and is the name of the mesh object to extract from, the default is the current mesh object.

**`external`** means only the exterior surface mesh will be extracted and not the interior interfaces (extracted by default).
Note if itetclr is set to a single value and the itp array is updated, there will be no interior interfaces to extract.

**`-all-`** deprecated option at end of syntax is ignored. By default, all interior and exterior surfaces are extracted.
 

## ADDED ATTRIBUTES


```
                   |    mregion2
  mregion1         | 
                   |------> normal 
                   | 
       itetclr = 1 |   itetclr = 2
      itetclr0 = 1 |  itetclr1 = 2
      element# = 1 |  element# = 2
      idelem0  = 1 |  idelem1  = 2

```

 Six new element attributes and one node attribute, are added to the output mesh
  indicating the material numbers (itetclr) on each side of the mesh
  faces, i.e., the color of elements that existed on each side of a
  face in the original mesh.  For multi-material, the convention is that the normal points
  into the larger material id (itetclr) material.

* **itetclr0** and **itetclr1** material colors (itetclr) of each side of the extracted surface. 0 indicates a face on the boundary.
* **idelem0**  and **idelem1**  element number on each side of the extracted surface.  
* **idface0**  and **idface1**  the local face number for each element on either side of the extracted surface. 
* **facecol** is a model face number constructed from the itetclr0 and itetclr1 attributes and is not guaranteed that the same facecol value will not be given to two disjoint patches of mesh faces.  
* **idnode0** provides the mapping from nodes in the extracted interface network to (parent) nodes in the input mesh object; that is, IDNODE0(J) is
   the parent node in the input mesh object that corresponds to node J in the output mesh object. 


## EXAMPLES
```
extract/surfmesh/1,0,0/ mos_all / MO_MESH

extract/surfmesh/1,0,0/ mos_out / MO_MESH / external

```

## DEMO

This demonstrates the difference between extracting all or just external boundaries. The first image shows the extracted surfmesh
which includes the interface between the materials. The second image shows the surfmesh extracted from external boundary only.
Note the face elements will depend on the element face being extracted, the second surfmesh has been converted from quad elements to tri elements. This may be needed for commands or applications that expect a triangle surface.

|   | 
| :---:  | :---:  | 
|   |  |
|  **Extract all**  |   **Extract external**  | 
| <img width="250" src="https://lanl.github.io/LaGriT/assets/images/box_surfmesh_all.png">  |  <img width="250" src="https://lanl.github.io/LaGriT/assets/images/box_surfmesh_tri_external.png">  | 

```

define MO_MESH mo_hex
define/R0/  0.0
define/Z0/  0.0
define/R1/ 10.0
define/Z1/  8.0
define/ND/  11
define/NZ/  9

cmo / create / MO_MESH / / / hex
createpts/brick/xyz/ND ND NZ/R0 R0 Z0/R1 R1 Z1/1 1 1

# COLOR elements material 1 and material 2 
pset/p2/attribute xic/1,0,0/ gt 6.
eltset/e2/ inclusive pset,get,p2
cmo / setatt / MO_MESH / itetclr 1 
cmo / setatt / MO_MESH / itetclr eltset,get,e2 2 

# SET BOUNDARIES AND INTERFACES
resetpts/itp

# EXTRACT ALL EXTERNAL AND INTERFACE BOUNDARIES
extract/surfmesh/1,0,0/ mos_all / MO_MESH
cmo/copyatt/ mos_all mos_all / itetclr itetclr1

# EXTRACT EXTERNAL ONLY 
extract/surfmesh/1,0,0/ mos_ext / MO_MESH / external
cmo/copyatt/ mos_ext mos_ext / itetclr itetclr1

# CONVERT SURFMESH QUADS to TRI
grid2grid/quadtotri2/ mos_tri / mos_ext

# write files
dump/avs/ surfmesh_all.inp / mos_all
dump/avs/ surfmesh_external.inp / mos_ext
dump/avs/ surfmesh_ext_tri.inp / mos_tri

cmo/status

finish
```
