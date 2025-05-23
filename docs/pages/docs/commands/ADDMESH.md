---
title: ADDMESH
tags: addmesh merge amr append glue excavate 
--- 

# ADDMESH #

-------------------------------------

This routine joins two meshes together at their common interface to
produce a third mesh. The **merge** and **append** options are commonly used to create a single mesh object from many. This can make refinement based on ojects easier to manage.

Some operations may only work with tet meshes.

NOTE: Care must be taken when using these commands because nothing is
done to clean up the point type (itp) array after the **addmesh**
operation. The user must often use the commands
[**resetpts/itp**](RESETPT.md) and [**filter**](FILTER.md)




## SYNTAX 

<pre>
<b>addmesh / add</b> / mesh3 / mesh1 / mesh2 / [refine_factor] / [tet edge]
<b>addmesh / amr</b> / mesh3 / mesh1 / mesh2 /
<b>addmesh / append</b> / mesh3 / mesh1 / mesh2 /
<b>addmesh / delete</b> / mesh3 / mesh1 / mesh2 /
<b>addmesh / glue</b> / mesh3 / mesh1 / mesh2 /
<b>addmesh / intersect</b> / pset_name / mesh1 / mesh2 /
<b>addmesh / match</b> / mesh3 / mesh1 / mesh2 / i1 12 i3 i4 i5 i6/
<b>addmesh / match</b> / mesh3 / mesh1 / mesh2 /rx1 ry1 rz1/rx2 ry2 rz2/rx3 ry3 rz3/rx4 ry4 rz4/rx5 ry5 rz5/rx6/ry6/rz6/
<b>addmesh / merge</b> / mesh3 / mesh1 / mesh2 /
<b>addmesh / pyramid</b> / mesh3 / mesh1 / mesh2 /
<b>addmesh / excavate</b> / mesh3 / mesh1 / mesh2 / [bfs] / [connect] /
</pre>

<br>


 **`add`** - Find the intersection of mesh1 and mesh2. Refine mesh1 where
 it overlaps mesh2 using the following criteria. `refine_factor`
 specifies the number of times that the background mesh will be
 refined. If this number is negative, or if it does not appear, then
 the it will use the default. The default method determines the number
 of refinement iterations based on the volumes of the tets. It
 continues to refine until the elements on the interface boundary of
 the background mesh object are within a given factor (5.0) of the
 volume of the elements on the border of the incoming mesh object. This
 factor is a parameter constant called `size_difference` in the code
 `continue_refinement.f`. For example, if `size_difference` is set to
 5.0, then the background mesh will be refined until the maximum volume
 element on the boundary with the incoming mesh object is no bigger
 than 5 times the volume of the maximum volume element on the border of
 the incoming mesh. refine_type is the type of refinement that is
 executed. If the string `tet` appears, then tetrahedral refinement
 is performed. Otherwise, `edge` based refinement is performed. After
 the above refine steps have been done, the intersection of mesh1 and
 mesh2 is found, elements that overlap are deleted from mesh1 and mesh2
 is appended to mesh1 to create mesh3.

 **`merge`** - Append `mesh2` to to `mesh1` and create `mesh3`. Essentially
 this just concatenates two mesh objects.

 **`glue`** - Synonym for `merge`.

 **`append`** - Append mesh2 to mesh1 and create mesh3. Similar to
 `merge` except `imt`, `icr`, `itetclr` of mesh2 have the value
 `max(imt(mesh1))` added to mesh2.

 **`delete`** - Create mesh3 which is mesh1 with elements that intersect
 mesh2 deleted.

 **`intersect`** - Create a pset called `pset_name` that contains all
 nodes in mesh1 which intersect elements of mesh2.

 **`amr`** - Use Adaptive mesh refinement to connect background mesh1
 with submesh mesh2 and create mesh3.

 **`match`** - Same as `merge` except the second mesh can be moved,
 rotated and translated. The first mesh does not move scale or rotate.
 If the interface needs to be scaled, translated and rotated that is
 accomplished by specifing 3 node numbers in each mesh or 3 node
 coordinates from each mesh that are to become coincident. If nodes are
 given match `i1-i4`, `i2-i5`, `i3-i6`. If coordinates are given match
 `(x1,y1,z1)-(x4,y4,z4)`, etc.

 **`pyramid`** - join a hex mesh to a tet mesh. The common surface must
 have matching nodes (i.e. there must be exactly two triangle faces on
 the tet grid that fit into one quad face of the hex grid). Pyramid
 elements will be constructed in the region where the two meshes join.

 **`excavate`** - The circumscribed sphere of each triangle of mesh2 is computed and any node in mesh1 that falls inside one of the circumscribed spheres is marked as a dudded node, along with any cells of mesh1 associated with these nodes.  mesh1 must be a 3D mesh (of any geometry) and mesh2
 must be a 2D triangular mesh. This then excavates an area in mesh1
 around mesh2, such that the surface could then be inserted into the 3D
 mesh (such as to insert a fault into a background terrain mesh). The
 background mesh, minus the excavated/removed nodes, is put into mesh3.
 The following options are available:
 
 - `[bfs]` will use a breadth-first search algorithm to find nodes to remove, as opposed to
 the default KD-tree algorithm. This will find candidate nodes for deletion within the maximum circumradius of the surface. Then a breadth-first search across the surface will be searched for an element large enough to contain the candidate node.
 - `[connect]` after excavation the following commands will be executed: **addmesh/append**
 and then **connect**. This will produce a fully connected mesh with the surface
 (mesh2) inserted into the background (mesh1).


## Examples

```
addmesh/merge/ moall / moall / mo_fault1
addmesh/merge/ moall / moall / mo_fault2
addmesh/merge/ moall / moall / mo_fault3
cmo/printatt/ moall / itetclr / minmax
cmo/printatt/ moall / imt / minmax
```

In this example, mo_fault1, mo_fault2, and mo_fault3 are all copied into the single mesh object named "moall". The values of itetclr and imt are not changed, so if itetclr values are 1, 2, and 3 respectively - then the itetclr minmax values will be 1 and 3. 
As an alternative **addmesh/append** will increment itetclr and imt values based on *mesh2* and *mesh3* values. If all *mesh2* values are 1, the itetclr minmax value at end of calls will be 1 a 3.

Once merged, a mesh can be intersected and refined based on moall instead of multiple calls using mo_fault1, mo_fault2, and mo_fault3. 


[Click here for demos](../demos/index.md)

