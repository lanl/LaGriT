---
GENERATOR: 'Mozilla/4.05C-SGI [en] (X11; I; IRIX64 6.5 IP28) [Netscape]'
---

 [<img height="300" width="300" src="../images/arrow2.gif">"30"
"30"](mstkla.md#parent-child) [<img height="300" width="300" src="../images/arrow3.gif">"30"
"30"](mstkla.md) [<img height="300" width="300" src="../images/arrow4.gif">"30"
"30"](prelim.md) 

------------------------------------------------------------------------

------------------------------------------------------------------------

**Parent-Child Node (Vertex) Issues** 

------------------------------------------------------------------------

 

All the operators are designed to work consistently with parent and
child nodes(if they exist) consistently. Operators exist for getting the
parent of a vertex, edge and face. The parent edge/face of a  edge/face
is constructed from the parents of its component vertices. Operators
also exist for obtaining the child vertices/edges/faces of
vertices/edges/faces.

The upward and downward adjacency information is designed to be returned
in a consistent manner as described below. If upward adjacency
information is requested for a child entity then only those higher order
entities connected to the child will be returned. On the other hand, if
it is requested for a parent, upward adjacency information from all its
children will be consolidated and returned. For example, if regions of a
child vertex (node) are requested, then only the regions classified in
that model region will  be returned. On the other hand if one wants all
the regions in all model regions connected to the node, one should get
the parent of the vertex and ask for the regions connected to the parent
vertex.  The behavior of downward adjacency information queries is
determined by the type of entity and how it is constructed. Asking for
the nodes of a vertex will return only the child nodes if any of the
nodes are on  an interface. Parent faces and edges on an interface will
return parent nodes and child faces and edges on an interface will
return child nodes. *Lastly, if the entity is not on an interface, it is
its own parent or its own child.*
[<img height="300" width="300" src="../images/arrow2.gif">"30"
"30"](mstkla.md#parent-child) [<img height="300" width="300" src="../images/arrow3.gif">"30"
"30"](mstkla.md) [<img height="300" width="300" src="../images/arrow4.gif">"30"
"30"](prelim.md)
