---
GENERATOR: 'Mozilla/4.5 [en] (X11; I; IRIX 6.3 IP32) [Netscape]'
Generator: Microsoft Word 98
Template: 
    Macintosh HD:Applications:Microsoft Office 98:Word Custom
    Install:Microsoft Office 98:Templates:Web Pages:Blank Web Page
title: EXTRACT
---

 

 **EXTRACT/SURFMESH**

  

  This routine extracts the boundary of a mesh. If the original mesh
  is a solid mesh, it extracts the surface mesh.  If it is a surface
  mesh, it extracts the edge mesh. If the interface elements have
  "parent-child" nodes, then only the parent nodes and elements are
  extracted.  Not all attributes in the input mesh object are created
  or initialize in the output mesh object.  Among the array-valued
  attributes,  only xic,yic,zic,itet,jtet,itetoff,itettyp, and icr1
  are set. imt is set to 1. The  icontab array is copied from the
  input mesh object to the output mesh object.

  

  Six new element based attributes, **itetclr0, itetclr1, idelem0**
  and **idelem1, idface0, idface1** are added to the output mesh
  indicating the material numbers (itetclr) on each side of the mesh
  faces, i.e., the color of elements that existed on each side of a
  face in the original mesh. The convention is that the normal points
  into the larger material id (itetclr) material.
  [itetclr0]{style="font-weight: bold;" indicates the color of the
  elements on smaller itetclr value side (the inside) of the face
  normal (0 if the face is on an external boundary) and
  [itetclr1]{style="font-weight: bold;" indicates the color of the
  elements on the outside of the normal. The attributes **idelem0**
  and **idelem1** record the element numbers of the input mesh that
  produced the lower dimensional output mesh. The attributes
  [idface0]{style="font-weight: bold;" and
  [idface1]{style="font-weight: bold;" record the local face number
  of the input mesh objects.

  [

                     ]{style="font-family: monospace;"

  [                   ------&gt;
  normal]{style="font-family: monospace;"

  [                   ]{style="font-family: monospace;"

  [       itetclr = 1    itetclr =
  2]{style="font-family: monospace;"

  [      itetclr0 = 1   itetclr1 = 2

        element
# = 1   element
# = 2

        idelem0  = 1   idelem1  =
  2]{style="font-family: monospace;"

  

  A node attribute, [idnode0]{style="font-weight: bold;", records the
  node number of the input mesh object node.

  

  In addition another element attribute called[ facecol
  ]{style="font-weight: bold;"is added to the elements of the new
  mesh. The [facecol ]{style="font-weight: bold;"attribute is a model
  face number constructed from the
  [itetclr0]{style="font-weight: bold;" and
  [itetclr1]{style="font-weight: bold;" attributes. The way the
  [facecol]{style="font-weight: bold;"  attribute is constructed does
  not guarantee that the same [facecol]{style="font-weight: bold;"
  value will not be given to two disjoint patches of mesh faces.
 
  If the keyword **external** is supplied, only the exterior surface
  mesh will be extracted.  i.e., the interior interfaces between
  materials will not be extracted.  The default is for the interior
  interfaces to be extracted as well.

 
  The itp array of the input mesh object must be correctly set prior
  to calling
  [extract]{style="font-family: Courier New,Courier,monospace;". To
  insure itp is correctly set a call to
  [resetpts/itp]{style="font-family: Courier New,Courier,monospace;"
  may be required.

 
 

      **FORMAT:**

       
 [extract/surfmesh/1,0,0/]{style="font-family: Courier New,Courier,monospace;"cmo\_out/[cmo\_in]/[external]



