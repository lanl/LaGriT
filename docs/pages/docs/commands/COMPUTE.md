
 **COMPUTE**

  This command contains modules that compute various attributes and
  functions based on one or more

  mesh objects. This operation will (often) result in new attributes
  being added to the mesh objects. The

  action of the command will be controled by the keyword in the second
  argument position.
 
  distance\_field - keyword for distance field calculation. Determine
  the minimum distance from any node in

  mo\_source to every node in mo\_sink and place the result in the
  node based floating point attribute,

  distance\_field\_attribute in mo\_sink. The computation is
  accelerated by using the
  [kdtree](kdtree.md) search

  algorithm.

  

  signed\_distance\_field - keyword for signed distance field
  calculation. Determine the minimum distance    

  from any node in mo\_source to every node in mo\_sink and place the
  result in the node based floating        

  point attribute, distance\_field\_attribute in mo\_sink. The
  computation is accelerated by using the      

  [kdtree](kdtree.md) search algorithm.
  Using this option the mo\_source MUST be either a triangle or quad
  surface        

  mesh object. If the surfaces form a topologically closed volume then
  positive, 'above' distance is in the    

  direction of the surface normal vector. Negative is 'below' the
  surface. If the surface is not a closed        

  volume, then the assumptions described in the
  [surface](SURFACE.md) command are used to determine what is above
     

  and what is below the surface.
 
  linear\_transform - keyword for an extrapolation from an attribute
  value in a surface onto every node of

  a 3D mesh.
  
  Given a 3D
  mesh and a 2D surface, this command will extrapolate a scalar value
  from that surface

  onto every point of the mesh. This can be used to (for
  example):
 
  -   Propagate head values from a surface onto all nodes of a
      mesh.
  -   Expand a mesh to fit a surface, by propogating the
      appropriate spatial coordinate.
  -   Compute the depth relative to a topographic surface to each
      node of a mesh.

 
  This is highly dependant on the spatial relation between the mesh
  and the surface - values from the

  surface are extrapolated "downward" into the mesh in the direction
  specified in the command. The

  direction specified in the command must be one of
   **zpos, zneg, ypos, yneg, xpos, xneg**
   
  For example,

  specifing [zpos] will result in the upper (positive

  z-axis) side of the mesh having attribute values conforming exactly
  to those on the surface, while the

  lower side of the mesh will have whatever attribute values it had
  previous, with all nodes in between

  having attribute values distributed linearly between the two
  extremes. If a direction is not specified,

  it will default to zpos. If an
  attribute is not specified, it will default to the spatial attribute
  appropriate

  to the chosen direction (i.e. if the direction is
  yneg, the attribute will default to yic, the
  y-coordinate of

  each node. The attribute chosen must already exist in both the
  surface and main meshes.
 
  Other places to look for modules that compute some standard mesh
  attributes include, [quality](QUALITY.md "Mesh Quality"), which
  will

  compute aspect ratio and volume,
  [cmo/addatt](cmo/cmo_addatt.md), which will
  compute normal vectors, dihedral angles, solid

  angles, meadian points, Voronoi points and more. User functions can
  be computed with the [math](MATH.md "Math Functions") module.

 **FORMAT**:
 
 
 compute/distance\_field/mo\_sink/mo\_source/distance\_field\_attribute

 compute/signed\_distance\_field/mo\_sink/mo\_source/distance\_field\_attribute

  
 compute/linear\_transform/mo\_main/mo\_surface/[direction/att\_name

 **EXAMPLES**:

    compute / distance_field / mo_sink / mo_src / dfield
  
    compute / signed_distance_field / mo_sink / mo_src / dfield
 
    compute / linear_transform / mo_sink / mo_surf                                               
                                                                        
  
  Example: distance\_field
  
      cmo / create / cmo_src                                            
      createpts/rtz/1,91,1/3.,0.,0./3.,270.,0./1,1,1/                   
      cmo / create / cmo_snk                                            
      createpts / xyz / 30 30 1 / -5. -5. -5. / 5. 5. 5. / 1 1 1        
      compute / distance_field / cmo_snk / cmo_src / dfield             
      finish           
  
  <img src="https://lanl.github.io/LaGriT/assets/images/distance_field_01.png">     
                                                                                                           
