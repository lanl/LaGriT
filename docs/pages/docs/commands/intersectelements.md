---
title: INTERSECT_ELEMENTS
tags: intersect elements
---

# INTERSECT_ELEMENTS

------------------------

This command takes two meshes and creates an element-based attribute with the number of elements intersected.
<br> We define intersection as two elements sharing any common point.


## SYNTAX

<pre>
<b>intersect_elements</b> / mesh1 / mesh2 / [attrib_name]
</pre>


`mesh1` has the added attribute containing the number of elements that were intersected.


`mesh2` is the object used for the intersection.


`attrib_name` is the name of the element based attribute that is created by this command. The default name for this
 attribute is **in_**`mesh2` using the mesh name to form the new attribute name.
 This attribute stores the number of intersections.  It is worth noting that some mesh viewers do not
 take kindly to names that are longer than eight characters and may truncate them.


### Description


This code has been slightly modified to work with AMR grids. This modification depends on an element based attribute called **itetkid**. If this attribute is not present, **intersect_elements** will **NOT** be able to recognize the AMR
 grid, and will intersect all elements of the octree. With the **itetkid** attribute present, only leaves of the octree which intersect will be flagged.


This table shows the available intersect capabilities. An **X** in the box means that the intersection is supported.



| element |  point   | line     |  tri     |   quad   |  tet     |   pyr     |   hex
| :------- | :-------  | :-------  | :-------  |  :------- |  :------- |  :-------  |  :-------
| point |  **X**  | **X** | **X** | **X** | **X** | **X** | **X**
| line |  **X** | **X** | **X** | **X** | **X** |         |     **X**
| tri  |   **X**  | **X** | **X** | **X** | **X** |      |    **X**
| quad |  **X** | **X** | **X** | **X** | **X** |       |    **X**
| tet |   **X** | **X** | **X** | **X** | **X** |       |    **X**
| pyr |   **X** |       |        |      |       |      |     
| hex |   **X** | **X** | **X** | **X** | **X** |       |     **X**



For example, this means that if you have a mesh that has hexes and tets in it, you could intersect it with a mesh that has anything but
 pyramids in it.

  
Note: **intersect_elements** is based on a k-D-R tree implementation to improve performance in many circumstances.
 Unfortunately, there is no way to improve performance if the elements being intersected have many candidate elements in their bounding
 boxes. As such, there are situations where running time may be improved by refining mesh2 such that its elements are of comparable
 size with those of mesh1.

 

## EXAMPLES

```
intersect_elements/cmo_grid/cmo_sphere/
```
Intersect grid with sphere and tag elements in default attribute in_cmo_sphere.

```
intersect_elements/cmo_grid/cmo_well/obswell
```
Intersect grid with well and tag elements in default attribute in_obswell.

```
read avs river_lines.inp moli

intersect_elements / cmotri / moli / inriver
  eltset / eriver / inriver / gt 0
  pset/priver/ eltset / eriver
  cmo/ setatt/ cmotri/ itetclr / eltset,get,eriver 1
  cmo/ setatt/ cmotri/ imt / pset,get,priver 1
```
Intersect to find nodes and elements at river object, tagged elements are in attribute inriver.

 

