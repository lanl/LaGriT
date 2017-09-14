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
  "uses" i.e.  "parent-child" nodes, then only the parent nodes and
  elements are extracted.  Due to the nature of this extraction, not
  all attributes in the  output mesh object are given values.  Among
  the array-valued attributes,  only
  xic,yic,zic,itet,jtet,itetoff,itettyp, and icr1 are set. The 
  icontab array is copied from the input mesh object to the output
  mesh object.  Two element based attributes, "itetclr0" and
  "itetclr1" are added to the output mesh indicating the material
  regions on each side of the mesh faces, i.e., the color of elements
  that existed on each side of a face in the original mesh. The
  convention is that the "itetclr0" indicates the color of the
  elements on the side of the face normal (0 if the face is on an
  external boundary) and "itetclr1"  indicates the color of the
  elements on the opposite side of the normal.

                     

                     ------&gt; normal

                     

   mregion5     mregion1
 
  For the face shown in the figure above itetclr0 = 1, itetclr1 = 5
 
  In addition another attribute called 'facecol' is added to the
  elements  of the new mesh. The 'facecol' attribute is a model face
  number constructed from the itetclr0 and itetclr1 attributes. The
  facecol  attribute does not guarantee that the same ID will not be
  given to two disjoint patces of mesh faces.
 
  If the keyword **external** is supplied, only the exterior surface
  mesh will be extracted.  I.e., the interior interfaces between
  materials will not be extracted.  The default is for the interior
  interfaces to be extracted as well.
 
  NOTE TO DEVELOPERS: The comments in the file extract\_surfmesh.f say
  that  the jtet array is not set in the new mesh. This is incorrect.
  Since the introduction of jtet cycles to accomodate multiple number
  of faces coming into an edge in a surface mesh,
  **extract\_surfmesh** has been modified to  update jtet values in
  the output mesh. This is not adequately reflected  in the code
  comments.

 

      **FORMAT:**

       
 **extract/surfmesh**/1,0,0/cmo\_out/[cmo\_in]/[**external**]
