<!-- Begin breadcrumb -->
<ul class="uk-breadcrumb">
    <li><a href="{{ "/pages/tutorial/index.html" | relative_url }}">Tutorials &amp; Examples</a></li>
    <li><span>Stratigraphic Hex Mesh Tutorial</span></li>
</ul>
<!-- End breadcrumb -->

<!-- Begin image -->
<img data-src="{{ "/pages/tutorial/stratigraphy/images/21_tet_01_fehm_voronoi.png" | relative_url }}" width="639" height="525" alt="" uk-img>
<br/>
<!-- End image -->

<h2 id="dump-fehm" class="uk-h3 uk-margin-remove">10. Write FEHM Files</h2>

Write out a series of files for the FEHM flow and transport code. 
FEHM uses the control volume finite element method (CVFE) with node materials
and properties assigned to the mesh nodes.

The image shows the tetrahedral mesh colored by the 6 node `imt` (materials)
values and showing the voronoi cell edges.

These two close-up images show the relationship between the Delaunay tet mesh
edges and the voronoi volumes, the latter of which are used by FEHM.
The mesh nodes are the vertices of the tetrahedral elements and are the center
of each voronoi volume.

<!-- Lightbox -->
<div class="uk-child-width-1-2@m" uk-grid uk-lightbox="animation: slide">
    <div>
        <a class="uk-inline" href="{{ "/pages/tutorial/stratigraphy/images/21_tet_01_fehm_tet_a.png" | relative_url }}" data-caption="Close-up view of mesh with Delaunay tetrahedral cells">
            <img src="{{ "/pages/tutorial/stratigraphy/images/21_tet_01_fehm_tet_a.png" | relative_url }}" alt="">
        </a>
    </div>
    <div>
        <a class="uk-inline" href="{{ "/pages/tutorial/stratigraphy/images/21_tet_01_fehm_vor_a.png" | relative_url }}" data-caption="Close-up view of mesh with Voronoi cells">
            <img src="{{ "/pages/tutorial/stratigraphy/images/21_tet_01_fehm_vor_a.png" | relative_url }}" alt="">
        </a>
    </div>
</div>

We prepare the tet mesh for FEHM with the following commands. These ensure
that there are no duplicate nodes and sets the elements to a single material.
We also make sure nodes are not doubly defined and parent-child chains are
removed.

```
cmo/select/motet                                                               
resetpts/parent                                                                 
filter/1,0,0     
rmpoint/compress
```

LaGriT can write a full set of FEHM model files with a single command.
The `keepatt` option will save attributes tagging nodes on the outside boundaries.
These can be used to define additional point sets and zones for FEHM.

The following files are written:

* `.fehm` - mesh coordinates and geometry 
* `_material.zone` - node imt (material) zone lists 
* `_outside.zone` - node external boundary zone lists 
* `_outside_vor.area` - node external boundary area lists 
* `_interface.zone` - zone lists for nodes along material interfaces
* `_multi_mat.zone` - lists of node pairs connected across material interfaces
* `.stor` - FEHM format file giving the voronoi (control volume) associated with each node and the sparce matrix structure

```
dump / fehm / mesh / motet / keepatt
```

Review the report written to the screen and the output file (`lagrit.out`) for
mesh quantities.
These are usful summaries and will help in finding problems in the mesh design.
Check that material node counts are as expected.

```
*** Write FEHMN GEOM AND ZONE FILES ***                                         
*********dump_material_lists********                                            
Minimum material ID value =      1                                              
Maximum material ID value =      6                                              
Total possible materials  =      6                                              
Material           1 has     23944 nodes. #nodes/nnodes is   0.207223027945     
Material           2 has      6867 nodes. #nodes/nnodes is   0.594303607941E-01 
Material           3 has     16072 nodes. #nodes/nnodes is   0.139094918966     
Material           4 has     33192 nodes. #nodes/nnodes is   0.287259727716     
Material           5 has     12871 nodes. #nodes/nnodes is   0.111391901970     
Material           6 has     22601 nodes. #nodes/nnodes is   0.195600062609   
```

Check that the Sparse Matrix volumes are positive.
Negative coefficient values can result from poorly formed tetrahedra.
These usually occur on non-convex boundaries.
FEHM will still run but accuracy may be impacted.

```
*** Construct and Compress Sparse Matrix:3D ***                                 
   *** Compress Area Coefficient Values ***                                     
 
AMatbld3d_stor: Matrix compress_eps:  0.1000000E-07                             
AMatbld3d_stor: Local epsilon:  0.1000000E-14                                   
AMatbld3d_stor: *****Zero Negative Coefficients ******                          
AMatbld3d_stor: Number of 'zero' (< compress_eps) coefs         0               
AMatbld3d_stor: npoints =   115547  ncoefs =     964637                         
AMatbld3d_stor: Number of unique coefs =       313                              
AMatbld3d_stor: Maximum num. connections to a node =         25                 
AMatbld3d_stor: Volume min =   7.4999999E+02                                    
AMatbld3d_stor: Volume max =   7.6800000E+05                                    
AMatbld3d_stor: Total Volume:   2.3846400E+10                                   
AMatbld3d_stor: abs(Aij/xij) min =   0.0000000E+00                              
AMatbld3d_stor: abs(Aij/xij) max =   1.3500000E+02                              
AMatbld3d_stor: (Aij/xij) max =   0.0000000E+00                                 
AMatbld3d_stor: (Aij/xij) min =  -1.3500000E+02                                 
AMatbld3d_stor Matrix coefficient values stored as scalar area/distance         
AMatbld3d_stor Matrix compression used for graph and coefficient values         
ascii STOR file written with name mesh.stor   
```                            

Visually inspect the mesh for the assignment of boundary zones such as top nodes.

<!-- Lightbox -->
<div class="uk-child-width-1-2@m" uk-grid uk-lightbox="animation: slide">
    <div>
        <a class="uk-inline" href="{{ "/pages/tutorial/stratigraphy/images/21_tet_01_fehm_node_zone_top.png" | relative_url }}" data-caption="GMV visualization of mesh nodes, with top nodes colored red">
            <img src="{{ "/pages/tutorial/stratigraphy/images/21_tet_01_fehm_node_zone_top.png" | relative_url }}" alt="">
        </a>
    </div>
</div>

<!-- Next / Prev -->
<ul class="uk-pagination">
    <li><a href="{{ "/pages/tutorial/stratigraphy/step_09.html" | relative_url }}"><span class="uk-margin-small-right" uk-pagination-previous></span> Previous</a></li>
    <li class="uk-margin-auto-left"><a href="{{ "/pages/tutorial/stratigraphy/step_10_exo.html" | relative_url }}">Next <span class="uk-margin-small-left" uk-pagination-next></span></a></li>
</ul>