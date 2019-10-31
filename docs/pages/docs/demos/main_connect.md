
# Examples: connect 3D



## [Example connect cube](description_connect.md)

**`connect`** single material 3D box of points into a tetrahedral mesh that satisfies the Delaunay criterion.
The output consists of two mesh files - one showing the point distribution and the other showing the connected tetrahedral mesh.

<a href="output/connect_cube_nodes.png"><img width="250" src="output/connect_cube_nodes.png"> </a>
<a href="output/connect_cube_tet.png"><img width="250" src="output/connect_cube_tet.png"> </a>



## [Example connect non-convex stacked mesh](description2_connect.md)

**`connect / noadd`** with input points from a hex mesh created with **`stack/layers`**. This example has a non-convex boundary and bad elements are created connecting across the boundary. 

This fix uses **`interpolate/map`** from the hex mesh to the tet mesh to tag and remove bad tets connecting across the boundary.


<a href="output/wave_connect0.png"><img width="250" src="output/wave_connect0.png"></a>
<a href="output/wave_connect_tet_edges.png"><img width="250" src="output/wave_connect_tet_edges.png"></a>



## [Example connect sloped interfaces causing popped tets](description3_connect.md)


**`connect`** with input points from a convex hex mesh created with **`stack/layers`**. 
This example has a thin layer formed from sloped internal interfaces. Bad elements are created across the internal interfaces resulting in "popped" tets.  

This example is fixed with resolution that improves the height to width ratio of the mesh spacing.

<a href="output/wave_tet_popped_explode.png"><img width="250" src="output/wave_tet_popped_explode.png"></a>
<a href="output/wave_tet_highres_explode.png"><img width="250" src="output/wave_tet_highres_explode.png"></a>



## [Example connect with check_interface ](description4_connect.md)

**`connect / check_interface`** with input points from a convex hex mesh created with **`stack/layers`**. 

This example has a thin layer formed from sloped internal interfaces. Bad elements are created connecting across the internal interfaces resulting in "popped" tets.  

This example is fixed using defined geometry **`region`**  and the option  **`check_interface`** to add points at the interface. Note that the interface is preserved but the added nodes may result in thin tets.


<a href="output/wave_tet_popped_explode.png"><img width="250" src="output/wave_tet_popped_explode.png"></a>
<a href="output/wave_tet_check_ex.png"><img width="250" src="output/wave_tet_check_ex.png"></a>





## See Related Examples 

**`grid2grid`** used to convert each mesh element to tri or tet elements.





