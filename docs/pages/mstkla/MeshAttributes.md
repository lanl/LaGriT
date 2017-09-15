---
GENERATOR: 'Mozilla/4.05C-SGI [en] (X11; I; IRIX64 6.5 IP28) [Netscape]'
---
[<img height="300" width="300" src="../images/arrow2.gif">"30"
"30"](mstkla.md#MESH%20VERTEX:) [<img height="300" width="300" src="../images/arrow3.gif">"30"
"30"](MeshEdge.md) [<img height="300" width="300" src="../images/arrow4.gif">"30"
"30"](GenRegion.md)

 

------------------------------------------------------------------------

------------------------------------------------------------------------

   **MESH ATTRIBUTE OPERATORS**

 

------------------------------------------------------------------------

*void* **MESH\_AddAtt**(*PMeshObj* mesh, *char 
**attname, *AttType*
attype, *Rank* rank,

*MType* mtype, *Interp* interp, *int* persist, *char 
**ioflags,
*double* defval);

*AttType*, *Rank* and *Interp* are typedef enums which are available if
one

includes "*mstkla.h*". The specific names (which are pretty much the
strings

that are used in LaGriT) are given in *mstkla\_types.h* (See [MSTKLA
typedefs](prelim.md))

'persist' must be 0 for temporary atts and 1 for permanent attribs

'ioflags' is a string as in lagrit  (may be any combination of 'a', 'g',
'f', 's', 'x', 'no' for

AVS, GMV, FEHM, SGI, LaGriT or None)

mtype is **MVERTEX** if the attribute is to be node based and is
**MFACE** or

**MREGION** if the attribute is to be element based depending on whether
the mesh

is 2D or 3D respectively.

------------------------------------------------------------------------

*void* **MESH\_ModAtt**(*PMeshObj* mesh, *char 
**attname, *char

**fieldname, *int* inew,

*double* rnew, *char 
**cnew);

Here depending on the fieldname, one has to fill the right value for
inew,

rnew or cnew.

*
*
* NOT TESTED 
*
*
*

------------------------------------------------------------------------

*void* **MESH\_CopyAtt**(*PMeshObj* trgmsh, *PMeshObj* srcmsh, *char*

*trgatt, *char* 
*srcatt);

*
*
* NOT TESTED 
*
*
*

------------------------------------------------------------------------

 *void* **MESH\_DelAtt**(*PMeshObj* mesh, *char 
**attname);

------------------------------------------------------------------------

*void* **MESH\_SetAtt**(*PMeshObj* mesh, *char 
**attname, *int* ifirst,
*int* ilast, *int*

istride, *char 
**psetname, *double* value);

If psetname is specified ifirst is ignored

*
*
* NOT TESTED 
*
*
*

------------------------------------------------------------------------


------------------------------------------------------------------------

Once attributes have been created, one can go to an individual mesh
entity

and ask for the value of an attribute for that entity or change the
value.

The entities can be Mesh Vertices (always), Mesh Faces (for a surface
mesh

only), Mesh Regions (for a solid mesh only).

For example one can use **MR\_GetAttVal** operator to get the value of
an

attribute for a given element or use **MR\_SetAttVal** to set it.

See attribute operators for[Mesh Vertices](MeshVertex.md#MV-Attribs),[Mesh Regions](MeshRegion.md#MR-Attribs) and [Mesh
Faces](MeshFace.md#MF-Attribs)

 
[<img height="300" width="300" src="../images/arrow2.gif">"30"
"30"](mstkla.md#MESH%20VERTEX:) [<img height="300" width="300" src="../images/arrow3.gif">"30"
"30"](MeshEdge.md) [<img height="300" width="300" src="../images/arrow4.gif">"30"
"30"](GenRegion.md)
