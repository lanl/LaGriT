---
GENERATOR: 'Mozilla/4. [en] (X11; I; IRIX 6.5 IP32) [Netscape]'
title: 'cmo/addatt'
---


# cmo/addatt


 The **cmo/addatt** command is used to add and initialize a new mesh
 object attribute. There are two variations on the command syntax, general and keyword.
 
 ## GENERAL FORMAT:
 
 The general version of the **cmo/addatt** command is used to add and initialize a new mesh
 object attribute. Note for the general form, parameters will resort to default settings if not
 included on the command line. 
 See the **[modatt](cmo_modatt.md)** command for details on these  attribute parameters.

  **cmo/addatt** / mo\_name / att\_name / [ type / rank / length /
  interpolate / persistence / ioflag / value ]

 
 ## KEYWORD FORMAT:
 
 The keyword syntax uses keywords to create and/or fill a valid attribute
 with values as defined by the keyword being used.
 The keyword is the fourth token on the command line and the syntax for each keyword 
 is unique as defined below. If the named attribute already exists, values will be
 overwritten with values as indicated by the keyword.
 This syntax can also derive a vector attribute from three attributes or derive three 
 scalar attributes from a vector attribute.

  **cmo/addatt** / mo\_name / **area\_normal** / normal\_type /
  att\_v\_name

  **cmo/addatt** / mo\_name / **unit\_area\_normal** / normal\_type /
  att\_v\_name

  **cmo/addatt** / mo\_name / **volume** / att\_name

  **cmo/addatt** / mo\_name / **vector** / att\_v\_snk / att\_1src,
  att\_2src, att\_3src

  **cmo/addatt** / mo\_name / **scalar** / att\_1snk, att\_2snk,
  att\_3snk / att\_v\_src


 ### KEYWORDS:
 
    **vector**: creates att\_v\_snk of rank vector from three existing attributes att\_1src, att\_2src, and att\_3src

    **scalar**: creates three attributes att\_1snk, att\_2snk, and att\_3snk from an attribute att\_v\_src of rank vector.

    **area\_normal**: creates vector attribute att\_v\_name and fills with the x,y,z components of area normals for each face. 
    The new attribute is nelements in length, type is VDOUBLE, and rank is vector. normal\_type choices include **xyz, rtz**, and **rtp**. 
    The area normal is a vector perpendicular to the triangle face with length equal to the area of the triangle. Currently implemented for **xyz** on triangles only.


    **unit\_area\_normal**: is a vector perpendicular to the triangle face with length equal to one. 
    This command has the same format as **area\_normal**.


    **volume** or **area**: creates an attribute nelements in length and type VDOUBLE. For **volume** keyword the att_name attribute 
    is filled with **volume**(if 3D), **area**(if 2D) or **length**(if lines). Currently implemented for triangle areas.


    **median** creates three element attributes (default names xmed, ymed, zmed) that are the coordinates of the median point (average value of the vertices) of each element. This is valid for all element types.


    **voronoi** creates three element attributes (default names xvor, yvor, zvor) that are the coordinates of the Voronoi point (center of circumscribed circle or sphere) of each element. This is only valid for elements of type tri and tet. This command does not check if a mesh is Delaunay, so a better syntax might be to call this circumscribed_center.
  
  
    **voronoi_volume** creates a node attribute nnodes in length and type VDOUBLE. Currently implemented for a tetrahedral mesh by calling the build stor function to form the Voronoi bounding area for each node. (See more in dump/stor.)
  
    **hybrid_volume** creates a node attribute nnodes in length and of type VDOUBLE which contains the volume of each hybrid median-Voronoi control volume. This is currently implemented for a tetrahedral mesh by calling the build stor function with the hybrid option. See dump/stor for details on what hybrid median-Voronoi volumes are. Currently this option is only available for 3D tetrahedral meshes.
  
  
    **voronoi_varea** creates three node attributes nnodes in length and type VDOUBLE. The attributes represent each of the x,y,z components for the Voronoi areas formed by surrounding nodes. Currently implemented for a triangle mesh by calling the same routine that is used to compute the Voronoi areas for the external faces of a tetrahedral mesh. Works only on a triangle mesh. (See more in dump/zone_outside)
  
  
    **edge_connections** creates and integer attribute with the number of edge connections to each node. node_num_diff: creates an integer attribute with the maximum difference in node number between the node and any node it is connected to. That is for node i connected to nodes j_1, j_2, ...j_n, the attribute will contain attribute=max(|i-j_1|,|i-j_2|, ... |i-j_n|)


    **xyz_rtp** creates three node attributes and fill them with the node coordinate, x,y,z in spherical coordinates, r,theta,phi. Default values for the attribute, if not specified are, c_r, c_theta, c_phi. 


    **xyz_rtz** create three node attributes and fill them with the node coordinate, x,y,z in cylindrical coordinates, r,theta,z. Default values for the attribute, if not specified are, c_r, c_theta, c_z .


    **synth_normal_area** create node attributes, x_n_norm, y_n_norm, z_n_norm, and fill them with the area weighted normal of each node.


    **synth_normal_angle** create a node attributes, x_n_norm, y_n_norm, z_n_norm, and fill them with the angle weighted normal of each node. 


*Note these synthetic node normal calculations are only supported for mesh objects of type line, tri and quad. If one wants to compute the normals to, for example, the outside nodes of a hex or tet mesh, one must first extract a surface mesh and then compute the normals to the surface mesh. The synthetic normal is computed by computing the normal to all elements incident upon a node and then taking the weighted average of all the normals. The weight factor is based on area or incident angle depending upon which option is selected.*


    **sumnode** create an element attribute att_name_elem and fill it with the sum of the elements' node attribute, att_name_node.
  

    **avgnode** create an element attribute att_name_elem and fill it with the average of the elements' node attribute, att_name_node.


    **minnode** create an element attribute att_name_elem and fill it with the minimum value of the elements' node attribute, att_name_node.


    **maxnode** create an element attribute att_name_elem and fill it with the maximum of the elements' node attribute, att_name_node.
 
    **quad_quality** creates element attributes att_name_quality, att_name_regularity, and att_name_flag. These three attributes all describe the quality of each quadrilateral in a quad mesh. See example below.

  
The following commands create element attributes related to element angle measurments.

The Dihedral angle calculations are supported for tri, quad, tet, pyramid, prism, hex.

The Solid angle calculation is only supported for tet elements.

See also the command quality for adding attributes such as element aspect ratio(quality/aratio) and edge length ratio 

(quality/edge_ratio), minimum edge length (quality/ edge_min) and maximum edge length (quality/edge_max).



    **ang_mind** create a scalar element attribute and fill it with the minimum dihedral angle (degrees) of the element

    **ang_minr** create a scalar element attribute and fill it with the minimum dihedral angle (radian) of the element

    **ang_maxd** create a scalar element attribute and fill it with the maximum dihedral angle (degrees) of the element

    **ang_maxr** create a scalar element attribute and fill it with the maximum dihedral angle (radian) of the element

*Dihedral angle will be between 0 and 2π.*


    **ang_mind_solid** create a scalar element attribute and fill it with the minimum solid angle (degrees) of the element

    **ang_maxd_solid** create a scalar element attribute and fill it with the minimum solid angle (radian) of the element

    **ang_minr_solid** create a scalar element attribute and fill it with the maximum solid angle (degrees) of the element

    **ang_maxr_solid** create a scalar element attribute and fill it with the maximum solid angle (radian) of the element

*Solid angle will be between 0 and 4π.*



## EXAMPLES of GENERAL FORMAT:


  **cmo** **/addatt**/cmo1/boron1**/VDOUBLE** **/scalar** **/nnodes** **/asinh** **/permanent**
  
  Create node attribute named boron1 with interpolate method of asinh.
  
  **cmo/addatt**/-cmo-/boron2 **/VDOUBLE/scalar/nnodes/asinh/permanent/gl**/2.0
  
  Create node attribute named boron2 and fill with value 2.0, set gmv and lagrit file formats.
  
  
  **cmo/addatt**/cmo1/boron3 **/VDOUBLE/scalar/nnodes/user/temporary**
  
  Create temporary node attribute named boron3.
  
  
  **cmo/addatt/-default-**/boron3
  
  Create attribute named boron3 with default mesh object settings.
  
  
  ## EXAMPLES of KEWORD FORMAT:
  
  
  **cmo/addatt**/ cmotri / **area\_normal** / anorm
  
  Create and fill element vector named anorm with the x,y,z components for area normals of each triangle.
  
  
  **cmo/addatt**/ cmotri / **unit\_area\_normal** / n\_face
  
  Create and fill element vector named n\_face with the x,y,z components for unit area normals of each triangle.
  
  
  **cmo/addatt**/cmo1/ **scalar** / xnorm, ynorm, znorm / anorm
  
  Create attributes xnorm, ynorm, znorm from the three components of the vector attribute anorm.
  
  
  **cmo/addatt**/cmo1/ **vector** / vnorm /xnorm, ynorm, znorm
  
  Create vector attribute vnorm from the three attributes xnorm, ynorm, znorm.
  
  
  **cmo/addatt**/ cmotri / **area** / darea
  
  Create and fill attribute named darea with area of each triangle.
  
  **cmo/addatt** / cmotet / **voronoi_volume** / vor_vol
  
Create and fill attribute named vor_vol with Voronoi volume of each node in tetrahedral mesh.

**cmo/addatt** / cmotet / **hybrid_volume** / hybrid_vol

Create and fill an attribute named hybrid_vol with the hybrid median-Voronoi volume of each node in a tetrahedral mesh.


**cmo/addatt** / cmotri / **voronoi_varea** / xvarea yvarea zvarea

Create and fill attributes xvarea, yvarea, and zvarea with xyz components of the Voronoi areas for each node in triangle mesh.


**cmo / addatt** / cmo / **ang_mind** / ang_mind

**cmo / addatt** / cmo / **ang_minr** / ang_minr

**cmo / addatt** / cmo / **ang_maxd** / ang_maxd

**cmo / addatt** / cmo / **ang_maxr** / ang_maxr

**cmo / addatt** / cmo / **ang_mind_solid** / sang_mind

**cmo / addatt** / cmo / **ang_minr_solid** / sang_minr

**cmo / addatt** / cmo / **ang_maxd_solid** / sang_maxd

**cmo / addatt** / cmo / **ang_maxr_solid** / sang_maxr

**cmo / addatt** / cmo / **synth_normal_area**

**cmo / addatt** / cmo / **synth_normal_angle**

**cmo / addatt** / cmo / **sumnode** / elem_sum_imt / imt 

**cmo / addatt** / cmo / **maxnode** / elem_max_boron / boron 

## EXAMPLE QUAD QUALITY:

**cmo/addatt** / cmoquad / **quad_quality** / quality regularity flag

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
    
    













