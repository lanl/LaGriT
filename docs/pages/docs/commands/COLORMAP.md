---
title: COLORMAP
tags: ok
---
 
**COLORMAP**

  This command builds the colormap.  In reality it only builds the
  material adjacency graph, from with the colormap can be quickly
  generated when needed.  Three actions are possible:

  **colormap** **/ [add] OR [create] OR [delete]**/[cmo\_name]
  
  **add** -- The material adjacency characteristics of the specified
  mesh object is added to the existing material adjacency graph, which
  is created if it didn't exist.  This is the default action.

  **create** -- The existing material adjacency graph is deleted and a
  new one created from the specified mesh object.

  **delete** -- The material adjacency graph is deleted if it exists. 
  Any specified mesh object is ignored.

   

 **EXAMPLES:**

  **colormap/create**/ mesh1

  **colormap**/mesh2

  **colormap** **/delete**

