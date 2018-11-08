---
title: ADDMESH
tags: ok
--- 

# ADDMESH #

  This routine joins two meshes together at their common interface to
  produce a third mesh.

 **FORMAT:**

    addmesh / add** /mesh3 / mesh1 / mesh2 / [refine\_factor] / [tet **edge**]
    addmesh** / **amr** / mesh3 / mesh1 / mesh2 /
    addmesh / append** / mesh3 / mesh1 / mesh2 /
    **addmesh / delete** / mesh3 / mesh1 / mesh2 /
    **addmesh / glue** / mesh3 / mesh1 / mesh2 /

  **addmesh / intersect** / pset\_name / mesh1 / mesh2 /

  **addmesh / match** / mesh3 / mesh1 / mesh2 / i1 12 i3 i4 i5 i6/

  **addmesh / match** / mesh3 / mesh1 / mesh2 /rx1 ry1 rz1/rx2 ry2
  rz2/rx3 ry3 rz3/rx4 ry4 rz4/rx5 ry5 rz5/rx6/ry6/rz6/

  **addmesh / merge**/ mesh3 / mesh1 / mesh2 /

  **addmesh / pyramid** / mesh3 / mesh1 / mesh2 /

  **addmesh / excavate** / mesh3 / mesh1 / mesh2 / [bfs] / [connect] /

 **add** - Find the intersection of mesh1 and mesh2. Refine mesh1 where
 it overlaps mesh2 using the following criteria. refine\_factor
 specifies the number of times that the background mesh will be
 refined. If this number is negative, or if it does not appear, then
 the it will use the default. The default method determines the number
 of refinement iterations based on the volumes of the tets. It
 continues to refine until the elements on the interface boundary of
 the background mesh object are within a given factor (5.0) of the
 volume of the elements on the border of the incoming mesh object. This
 factor is a parameter constant called size\_difference in the code
 continue\_refinement.f. For example, if size\_difference is set to
 5.0, then the background mesh will be refined until the maximum volume
 element on the boundary with the incoming mesh object is no bigger
 than 5 times the volume of the maximum volume element on the border of
 the incoming mesh. refine\_type is the type of refinement that is
 executed. If the string **tet** appears, then tetrahedral refinement
 is performed. Otherwise, **edge** based refinement is performed. After
 the above refine steps have been done, the intersection of mesh1 and
 mesh2 is found, elements that overlap are deleted from mesh1 and mesh2
 is appended to mesh1 to create mesh3.

 **merge** - Append mesh2 to to mesh1 and create mesh3. Essentially
 this just concatenates two mesh objects.

 **glue** - Synonym for **merge**.

 **append** - Append mesh2 to mesh1 and create mesh3. Similar to
 **merge** except imt, icr, itetclr of mesh2 have the value
 max(imt(mesh1)) added to mesh2.

 **delete** - Create mesh3 which is mesh1 with elements that intersect
 mesh2 deleted.

 **intersect** - Create a pset called pset\_name that contains all
 nodes in mesh1 which intersect elements of mesh2.

 **amr** - Use Adaptive mesh refinement to connect background mesh1
 with submesh mesh2 and create mesh3.

 **match** - Same as **merge** except the second mesh can be moved,
 rotated and translated. The first mesh does not move scale or rotate.
 If the interface needs to be scaled, translated and rotated that is
 accomplished by specifing 3 node numbers in each mesh or 3 node
 coordinates from each mesh that are to become coincident. If nodes are
 given match i1-i4, i2-i5, i3-i6. If coordinates are given match
 (x1,y1,z1)-(x4,y4,z4), etc.

 **pyramid** - join a hex mesh to a tet mesh. The common surface must
 have matching nodes (i.e. there must be exactly two triangle faces on
 the tet grid that fit into one quad face of the hex grid). Pyramid
 elements will be constructed in the region where the two meshes join.

 **excavate** - mesh1 must be a 3D mesh (of any geometry) and mesh2
 must be a 2D triangular mesh. This then excavates an area in mesh1
 around mesh2, such that the surface could then be inserted into the 3D
 mesh (such as to insert a fault into a background terrain mesh). The
 background mesh, minus the excavated/removed nodes, is put into mesh3.
 If the optional [bfs] argument is given, the routine will use a
 breadth-first search algorithm to find nodes to remove, as opposed to
 the default KD-tree algorithm. If the optional [connect] argument is
 given, the program will, after excavation, execute an addmesh/append,
 and then a connect, to produce a fully connected mesh with the surface
 (mesh2) inserted into the background (mesh1).

 NOTE: Care must be taken when using these commands because nothing is
 done to clean up the point type (itp) array after the **addmesh**
 operation. The user must often execute a series of
 [**resetpts** **/itp**](RESETPT.md) and **[filter](FILTER.md)**
 commands to get the final desired result.

 NOTE: Some operations may only work with tet meshes.

 [Click here for demos](../demos/index.md)

