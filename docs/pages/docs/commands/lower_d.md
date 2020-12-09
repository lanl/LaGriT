---
title: LOWER_D
tags: lower_d
---

# LOWER_D

---------------------

These commands are advised for expert users only.


This suite of commands creates and handles the lower dimension structures associated with a mesh.  The existing mesh is labeled 'd0'.  The next lower dimension mesh 'd1' and so on.  For an
  original 3D mesh, the d1 structures are the surfaces (2D) separating
  material regions, the d2 structures are the lines separating the d1
  surfaces and the d3 structures are the nodes at the ends of the d2 lines.
  
 
 ## SYNTAX

<pre>
<b>lower_d / filter </b>/ [cmo_name] /[<b>iclr1  itp  imt</b> ] / value [ <b>and</b>  or  <b>new</b> ]
</pre>

create lower_d structures in mesh object
<pre>
<b>lower_d</b> / <b>create</b>/ [cmo_name]
</pre>

release lower_d structures
<pre>
<b>lower_d</b> / <b>release</b>/ [cmo_name]
</pre>
 
create lower_d structures into named mesh objects cmo1, cmo2, cmo3
<pre>
<b>lower_d</b> / <b>extract</b>/ [cmo_name/cmo1/cmo2/cmo2]
</pre>
 

Several new attributes are created which belong the original mesh object:

   
 
| name     |      type   |   length
| :------- | :------ | :----- 
    d0_nnodes_topo     |  VINT, nnodes  | 0 = interior, 1 = surface, node 2 = line node, 3 = line end node
    d1_nnodes | INT, scalar | number of nodes in this   structure 
    d1_elements             | INT, scalar |              number of elements in   this structure 
    d1_nef_cmo       | INT, scalar |              number of facets/element in this structure 
    d1_nee_cmo             | INT, scalar |              number of edges/element in this structure 
    d1_nen_cmo             | INT, scalar |              number of nodes/element in this structure 
    d1_jtet_cycle_max     | INT, scalar |              the longest jtet cycle  in this structure 
    d1_itettyp              | VINT d1_elements |         element type
    d1_itetclr              | VINT d1_elements |         element selection number
    d1_itet off             | VINT d1_elements |        offset to d1_itet
    d1_jtet off             | VINT d1_elements |        offset to d1_jtet
    d1_itet                 | VINT d1_elements |        list of nodes for each xd1_neu_cmo  element
    d1_jtet                 | VINT d1_elements |        list of face neighbors xd1_nef_cmo            
    d1_elm_d0              | VINT d1_elements |        elements face # in original mesh that this element came from
    d2_nnodes            |   INT, scalar            |  number of nodes in this         structure 
    d2_elements             | INT, scalar              | number of elements in         this structure 
    d2_nef_cmo             | INT, scalar              | number of facets/element in this structure 
    d2_nee_cmo             | INT, scalar              | number of edges/element in this structure 
    d3_nen_cmo             | INT, scalar              | number of nodes/element in this structure 
    d2_jtet_cycle_max     | INT, scalar            |  the longest jtet cycle  in this structure 
    d2_itettyp              | VINT d2_elements        | element type
    d2_itetclr              | VINT d2_elements        | element material number
    d2_itet off             | VINT d2_elements        | offset to d2_itet
    d2_jtet off             | VINT d2_elements        | offset to d2_jtet
    d2_itet                 | VINT d2_elements        | list of nodes for each xd2_neu_cmo  element
    d2_jtet                 | VINT d2_elements        | list of face neighbors xd2_nef_cmo            
    d2_elm_d1              | VINT d2_elements  |      element & face that this element came from in next higher level structure 
    d3_nnodes    |           INT, scalar          |    number of nodes in this structure 
    lower_d_flag           | INT, scalar          |    0= no lower d structure exist =1 lower_d structures exist and are valid =2 lower_d structures not valid
 
 
The above set of attributes are created if the original mesh is 3D. If the original mesh is 2D then the d1 structures are created, but the d2 structures are simply the d2_nnodes.  If the original mesh is 1D, then only the d1_nnodes structure is created.
 
At the time the lower_d structures are created color table attributes: d0_clrtab, d0_nclrs, .. are also created.

 
 
