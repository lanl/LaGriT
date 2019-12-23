---
title: 'cmo/addatt'
tags: cmo addatt 
---


# cmo/addatt

-------------------

 The **`cmo/addatt`** command is used to add and initialize a mesh object attribute. 

 
The general version of the [**cmo/addatt**](#general) adds and initializes a new mesh object attribute. 
<br>


The [keyword](#keywords) version of the command recognizes keywords to create and/or fill attribute as defined by the keyword being used.
If the named attribute already exists, values in the attribute will be overwritten.


**Valid Keywords:**

|          |             |              |               |               
| :-------        |  :-------      |  :-------     |   :-------     |   
| [scalar](#scalar)  |  [vector](#vector)    | [median](#median)   |     |
| [edge_connections](#edge_connections) | [num_node_diff](#num_node_diff) | [xyz_rtp](#xyz_rtp)  |  [xyz_rtz](#xyz_rtz)    | 
| [volume](#volume)  | [voronoi](#voronoi)  |  [voronoi_volume](#voronoi_volume)    |  [hybrid_volume](#hybrid_volume)   | 
| [sumnode](#sumnode)   |  [avgnode](#avgnode)   | [minnode](#minnode)   |  [maxnode](#maxnode)   |
| [ang_mind](#ang_mind)   |  [ang_minr](#ang_minr)   | [ang_maxd](#ang_maxd)   |  [ang_maxr](#ang_maxr)   |
| [ang_mind_solid](#ang_mind_solid)   |  [ang_minr_solid](#ang_minr_solid)   | [ang_maxd_solid](#ang_maxd_solid)   |  [ang_maxr_solid](#ang_maxr_solid)   |


**Valid Keywords for 2D:**

Some calculations are only supported for mesh objects of type line, tri, and quad. 
If one wants to compute the normals to, for example, the outside nodes of a hex or tet mesh, one must first extract a surface mesh and then 
compute the normals to the surface mesh. 

|          |             |              |               |               
| :-------        |  :-------      |  :-------     |   :-------     |   
| [area_normal](#area_normal)  |  [unit_area_normal](#unit_area_normal)    |  [synth_normal_area](#synth_normal_area)   |  [synth_normal_angle](#area_normal_angle)   |
| [area](#volume)  |  [length](#volume) |  [voronoi_varea](#voronoi_varea)    |  [quad_quality](#quad_quality)   |   



## GENERAL SYNTAX <a name="general"></a>

<pre>
<b>cmo/addatt</b> / mo_name / att_name / [ type / rank / length / interpolate / persistence / ioflag / value ]
</pre>


`mo_name` is the name of the mesh object to add attribute to.

`att_name` is the name of the new attribute for the mesh object.

`type / rank / length / interpolate / persistence / ioflag / value` are parameters for the new attribute. The defaults are: type (vdouble), rank (scalar), length (nnodes), interpolation (linear), persistance (temporary), ioflag (agl), value (0.0).  See [**`MODATT`**](cmo_modatt.md) for descriptions of mesh object attribute parameters.



## KEYWORD SYNTAX <a name="keywords"></a> 

<pre>
<b>cmo/addatt</b> / mo_name / keyword / keyword_parameters / 
</pre>
 
<a name="scalar"></a>
**scalar** / `att_1snk att_2snk att_3snk` / `att_v_src`  creates three node or element scalar attributes from a vector attribute.


<a name="vector"></a>
**vector** / `att_v_snk` / `att_1src att_2src att_3src` creates node or element attribute of rank vector from three existing scalar attributes. The new attribute will have a length = rank x nnodes (or nelements).  *Note: vector type attributes are not supported in many of the **dump** formats including AVS and GMV. Convert a vector attribute to multiple scalar attributes before writing to file.*


<a name="median"></a>
**median** / [`xmed ymed zmed`] creates three element attributes that are the coordinates of the median point (average value of the vertices) of each element. This is valid for all element types.  (default attribute names are xmed, ymed, zmed) 


<a name="edge_connections"></a>
**edge_connections** / `att_sink` creates an integer node attribute with the number of edge connections to each node. 


<a name="node_num_diff"></a>
**node_num_diff** / `att_sink` creates an integer attribute with the maximum difference in node number between the node and any node it is connected to. 
That is for node i connected to nodes j_1, j_2, ...j_n, the attribute will contain attribute=max(i-j_1,i-j_2, ... i-j_n)



<a name="volume"></a>
**volume** or **area** / `att_name` creates an element attribute of type VDOUBLE. For **volume** the attribute is filled with **volume**(if 3D), **area**(if 2D) or **length**(if lines). Currently implemented for triangle areas.



<a name="voronoi"></a>
**voronoi** / `xvor, yvor, zvor` creates three element attributes that are the x y z coordinates of the Voronoi point (center of circumscribed circle or sphere) of each element. This is only valid for elements of type tri and tet. This command does not check if a mesh is Delaunay, so a better keyword might be circumscribed_center. (Default attribute names are xvor, yvor, zvor).



<a name="voronoi_volume"></a>
**voronoi_volume** / `att_name` creates a node attribute of type VDOUBLE. Currently implemented for a tetrahedral mesh by calling the build stor function to form the Voronoi bounding area for each node. See more about build stor in [dump/stor](../DUMP2.md).


<a name="hybrid_volume"></a>
**hybrid_volume** / `att_name` creates a node attribute of type VDOUBLE which contains the volume of each hybrid median-Voronoi control volume. This is currently implemented for a tetrahedral mesh by calling the build stor function with the hybrid option. See [dump/stor](../DUMP2.md) for details on what hybrid median-Voronoi volumes are. Currently this option is only available for 3D tetrahedral meshes.



<a name="sumnode"></a>
**sumnode** / `att_name_elem` / `att_name_node`  creates an element attribute and fills it with the sum of each elements' node attribute values.


<a name="avgnode"></a>
**avgnode** / `att_name_elem` / `att_name_node`  creates an element attribute and fills it with the average of each elements' node attribute values.


<a name="minnode"></a>
**minnode** / `att_name_elem` / `att_name_node`  creates an element attribute and fills it with the minimum of each elements' node attribute values.


<a name="maxnode"></a>
**maxnode** / `att_name_elem` / `att_name_node`  creates an element attribute and fills it with the maximum of each elements' node attribute values.


<a name="xyz_rtp"></a>
**xyz_rtp** / [ `[att_node_r att_node_theta att_node_phi` ] creates three node attributes and fills them with the node coordinate, x y z in spherical coordinates, radius, theta, phi. (Defaults are c_r, c_theta, c_phi). 


<a name="xyz_rtz"></a>
**xyz_rtz** / [ `c_r, c_theta, c_z` ] creates three node attributes and fills them with the node coordinate, x y z in cylindrical coordinates, radius, theta, z. (Defaults are c_r, c_theta, c_z).



<a name="area_normal"></a>
**area_normal** / `normal_type / att_v_name` creates an element attribute of rank vector and fills with the x y z components of area normals for each face. 
The `normal_type` choices include **xyz, rtz**, and **rtp**. 
The area normal is a vector perpendicular to the triangle face with length equal to the area of the triangle. Currently implemented for **xyz** on triangles only.


<a name="unit_area_normal"></a>
**unit_area_normal** / `normal_type / att_v_name` creates an element attribute of rank vector and fills with the x y z components of vector perpendicular to the face and with length equal to one.
The `normal_type` choices include **xyz, rtz**, and **rtp**.                                                                          
The area normal is a vector perpendicular to the triangle face with length equal to the area of the triangle. Currently implemented for **xyz** on triangles only.


  
<a name="voronoi_varea"></a>
**voronoi_varea** / `att_name_xn att_name_yn att_name_zn` creates three node attributes with type VDOUBLE. The attributes represent each of the x y z components for the Voronoi areas formed by surrounding nodes. Currently implemented for a triangle mesh by calling the same routine that is used to compute the Voronoi areas for the external faces of a tetrahedral mesh. Works only on a triangle mesh. See more in [dump/zone_outside](../DUMP2.md).
  

<a name="synth_normal_area"></a>
**synth_normal_area**   creates three node attributes, x_n_norm, y_n_norm, z_n_norm, and fills them with the area weighted normal of each node.


<a name="synth_normal_angle"></a>
**synth_normal_angle** creates three node attributes, x_n_norm, y_n_norm, z_n_norm, and fills them with the angle weighted normal of each node. 


*The synthetic normal is computed by computing the normal to all elements incident upon a node and then taking the weighted average of all the normals. The weight factor is based on area or incident angle depending upon which option is selected.*

 
<a name="quad_quality"></a>
**quad_quality** / `att_name_quality / att_name_regularity / att_name_flag` creates element attributes att_name_quality, att_name_regularity, and att_name_flag. These three attributes all describe the quality of each quadrilateral in a quad mesh. See example below.



The following commands create element attributes related to element angle measurments.

The Dihedral angle calculations are supported for tri, quad, tet, pyramid, prism, hex.
The Solid angle calculation is only supported for tet elements.

See also the command [**`QUALITY`**](../QUALITY.md) for adding attributes such as element aspect ratio(quality/aratio) and edge length ratio (quality/edge_ratio), minimum edge length (quality/ edge_min) and maximum edge length (quality/edge_max).

<a name="ang_mind"></a>
**ang_mind** create a scalar element attribute and fill it with the minimum dihedral angle (degrees) of the element

<a name="ang_min"></a>
**ang_minr** create a scalar element attribute and fill it with the minimum dihedral angle (radian) of the element

<a name="ang_maxd"></a>
**ang_maxd** create a scalar element attribute and fill it with the maximum dihedral angle (degrees) of the element

<a name="ang_maxr"></a>
**ang_maxr** create a scalar element attribute and fill it with the maximum dihedral angle (radian) of the element

*These dihedral angles will be between 0 and 2pi.*

<a name="ang_mind_solid"></a>
**ang_mind_solid** create a scalar element attribute and fill it with the minimum solid angle (degrees) of the element

<a name="ang_mind_solid"></a>
**ang_maxd_solid** create a scalar element attribute and fill it with the minimum solid angle (radian) of the element

<a name="ang_mind_solid"></a>
**ang_minr_solid** create a scalar element attribute and fill it with the maximum solid angle (degrees) of the element

<a name="ang_mind_solid"></a>
**ang_maxr_solid** create a scalar element attribute and fill it with the maximum solid angle (radian) of the element

*These solid angles will be between 0 and 4pi.*



## EXAMPLES of GENERAL FORMAT

```
cmo/addatt/ cmo1 / boron1 /VDOUBLE/scalar/nnodes/asinh/permanent
```
Create node attribute named boron1 with interpolate method of asinh.

```  
cmo/addatt/ cmo1 /z_save/VDOUBLE/scalar/nnodes/linear/permanent/
cmo/copyatt/ cmo1 cmo1 / z_save zic
```  
Create node attribute named z_save then use **copyatt** to copy values from zic to z_save.
  
```  
cmo/addatt/ cmo1 /boron3 /VDOUBLE/scalar/nnodes/user/temporary/ .1
```  
Create temporary node attribute named boron3 and fill with value .1
  
```  
cmo/addatt/-default-/boron3
```
Create attribute named boron3 with default mesh object settings.
  
  
  
  
## EXAMPLES of KEWORD FORMAT
  
  
```
cmo/addatt/ cmotri / area_normal / anorm
```
  
Create and fill element vector named anorm with the x,y,z components for area normals of each triangle.
  
  
```
cmo/addatt/ cmotri / unit_area_normal / n_face
```
Create and fill element vector named n\_face with the x,y,z components for unit area normals of each triangle.
  
  
```
cmo/addatt/cmo1/ scalar / xnorm, ynorm, znorm / anorm
```
Create attributes xnorm, ynorm, znorm from the three components of the vector attribute anorm.
  
  
```
cmo/addatt/cmo1/ vector / vnorm /xnorm, ynorm, znorm
```
Create vector attribute vnorm from the three attributes xnorm, ynorm, znorm with length nnode (or nelement) x 3.
  
  
```
cmo/addatt/ cmotri / area / darea
```
Create and fill attribute named darea with area of each triangle.
  
```
cmo/addatt / cmotet / voronoi_volume / vor_vol
```
Create and fill attribute named vor_vol with Voronoi volume of each node in tetrahedral mesh.

```
cmo/addatt / cmotet / hybrid_volume / hybrid_vol
```
Create and fill an attribute named hybrid_vol with the hybrid median-Voronoi volume of each node in a tetrahedral mesh.


```
cmo/addatt/cmotri/ voronoi_varea / xvarea yvarea zvarea
```
Create and fill attributes xvarea, yvarea, and zvarea with xyz components of the Voronoi areas for each node in triangle mesh.


```
cmo / addatt / cmo / ang_mind / ang_mind
cmo / addatt / cmo / ang_minr / ang_minr
cmo / addatt / cmo / ang_maxd / ang_maxd
cmo / addatt / cmo / ang_maxr / ang_maxr
cmo / addatt / cmo / ang_mind_solid / sang_mind
cmo / addatt / cmo / ang_minr_solid / sang_minr
cmo / addatt / cmo / ang_maxd_solid / sang_maxd
cmo / addatt / cmo / ang_maxr_solid / sang_maxr
cmo / addatt / cmo / synth_normal_area
cmo / addatt / cmo / synth_normal_angle
cmo / addatt / cmo / sumnode / elem_sum_imt / imt 
cmo / addatt / cmo / maxnode / elem_max_boron / boron 
```
Various examples.


## EXAMPLE QUAD QUALITY:

```
cmo/addatt/cmoquad / quad_quality / quality regularity flag
```

Create attributes named quality, regularity, and flag with several quad quality measures.

The first attribute represents the measure: <img src="https://lanl.github.io/LaGriT/assets/images/quad_quality_equation.png">

where <img src="https://lanl.github.io/LaGriT/assets/images/alpha.png"> is a normalization constant and

<img src="https://lanl.github.io/LaGriT/assets/images/h_max.png"> is the longest length among the fouredges and the diagonals.


<img src="https://lanl.github.io/LaGriT/assets/images/h_s.png"> is the mininum area of the four triangles constructed by adding a diagonal to the quad. This metric ranges from zero (poor) to one (good).

<img src="https://lanl.github.io/LaGriT/assets/images/quad_quality.jpg"> 


The second attribute represetns the *regularity* of each quad ABCD, defined as
<img src="https://lanl.github.io/LaGriT/assets/images/regularity.png">

where <img src="https://lanl.github.io/LaGriT/assets/images/p_ac.png"> and <img src="https://lanl.github.io/LaGriT/assets/images/p_bd.png">

This is a measure of the extent to which the quad is planar. The values range from zero (non-planar) to one (planar).

The third and final attribute is an integer flag which gives information about the quad:

    0 (good) = the quad is non-degenerate. For drawing a diagonal across it, the area vectors of the triangles have a positive dot product.
    
    1 (degenerate) = at least one of the triangles detirmined by three of the vertices of the quad has zero area. That is the quad has degenerated into a triangle or worse.
    
    2 (warped) = one of the quad's diagonals divides it into two triangles whose area vectors have a negative dot product. If the quad is planar, then this is self-intersecting or vertices are in wrong order by mistake.
    
    













