<!-- This is a button toggling the modal with the default close button -->
<button class="uk-button uk-button-default uk-margin-small-right" type="button" uk-toggle="target: #modal-close-default">Mesh Design, why does it matter?</button>

<!-- This is the modal with the default close button -->
<div id="modal-close-default" uk-modal>
    <div class="uk-modal-dialog uk-modal-body">
        <button class="uk-modal-close-default" type="button" uk-close></button>
        <h2 class="uk-modal-title">Mesh Design, why does it matter?</h2>
        <p>
            The mesh design is chosen with consideration of the physics to be modeled, mesh size restrictions (number of degrees of freedom)
            versus mesh resolution needed for model features, and the mesh and model information needed by the model application.
            <br/><br/>
            For modeling applications with complex stratigraphy, depending on the mesh, you can get a stable but inaccurate solution
            to the physics (Zyvoloski and Vesselinov, 2006). Choose a mesh design and meshing method that gives the best performance from
            the modeling application possible, with respect to the difficulty in generating the mesh.
            <br/><br/>
            LaGriT is used to generate meshes with control volume discretization such that the underlying control volumes are Voronoi
            tessellations. Developed by Los Alamos National Laboratory as open source software, LaGriT provides a variety of meshing
            tools with capabilities specific to geologic applications and Voronoi control volume solvers. Examples of methods used
            for geologic applications include unstructured and structured, both with adaptive refinement to geological features.
            The unstructured approach allows the creation of meshes that exactly conform to the geometric model, but
            requires some expertise in building the mesh such that it will also meet the Delaunay criteria. The easier
            method is to use a structured mesh with fine resolution, or a coarser mesh that uses octree refinement to increase
            resolution in user specified regions of interest. These result in stair-stepped  geometries instead of smooth, but can
            be acceptable for where the geometry spacing is small relative to the full model domain. (Sentis and Gable, 2017).
        </p>
    </div>
</div>

<h1 class="uk-heading-line"><span>Tutorials</span></h1>

<!-- Tutorial card -->
<div uk-filter="target: .js-filter">
    <div class="js-filter uk-child-width-1-2 uk-child-width-1-2@m uk-text-center" uk-grid="masonry: true">
            <a href="{{ "/pages/tutorial/lagrit_introduction/index.html" | relative_url }}">
                <div class="uk-card uk-card-default uk-card-body uk-card-hover">
                    <div class="uk-card-media-top">
                        <img style="max-height:200px;" src="lagrit_introduction/step_04/04_tet_nodes_imt_well_pts.png" alt="">
                    </div>
                    <p> 
                        <h3 class="uk-card-title">LaGriT Introdution Tutorial</h3>
                        <p>Designed to familiarize new users with LaGriT commands, definitions, and tools.</p>
                        <span class="uk-badge">Beginner</span>
                    </p>
                </div>
            </a>
            <a href="{{ "/pages/tutorial/stratigraphy/index.html" | relative_url }}">
                <div class="uk-card uk-card-default uk-card-body uk-card-hover">
                    <div class="uk-card-media-top">
                        <img style="max-height:200px;" src="stratigraphy/images/21_tet_01_exo_blocks.png" alt="">
                    </div>
                    <p> 
                        <h3 class="uk-card-title">Stratigraphic Hex to Tet Mesh Tutorial</h3>
                        <p>Hex mesh using octree refinement with stratigraphy, faults, wells, and subsurface layering</p>
                        <span class="uk-badge">Advanced</span>
                    </p>
                </div>
            </a>
    </div>
</div>
<!-- End tutorial card -->

<!-- Begin Example cards - Order is controlled by height of the images and words. Default .uk-card-default card height is set at min-height of 350px -->
<!-- .uk-card-command card height is set at max-height of 200px and in assets/uikit_theme.css -->
<h1 class="uk-heading-line"><span>Examples</span></h1>
<!-- Types are tutorial, demo, utility, commands in order from most complex to simple -->

The following are grouped according to the level of complexity. For specific command examples, see the [command index](/pages/commands.md).
Select All, or by one of the categories:

- <b>Commands</b> are command examples showing a few lines for syntax and usage. 
- <b>Utility</b>  is  a set of commands for a particular task. 
- <b>Demo</b> is a set of commands from start to finish demonstrating a particular method.
- <b>Tutorial</b> is a full set of files and commands involving many steps to achieve a final mesh. 

<hr>


<!-- Cards for Example commands, utility, and demo, tutorial  -->
<div uk-filter="target: .js-filter">
    <ul class="uk-subnav uk-subnav-pill">
        <li class="uk-active" uk-filter-control><a href="#">All</a></li>
        <li uk-filter-control="[data-color='command']"><a href="#">Commands</a></li>
        <li uk-filter-control="[data-color='utility']"><a href="#">Utility</a></li>
        <li uk-filter-control="[data-color='demo']"><a href="#">Demo</a></li>
        <li uk-filter-control="[data-color='tutorial']"><a href="#">Tutorial</a></li>
    </ul>
    <ul class="js-filter uk-child-width-1-2 uk-child-width-1-3@m uk-text-center" uk-grid="masonry: true">
<!-- demo cards -->      	
<!-- Basic -->
	<li data-color="demo">
            <div>
                <a href="{{ "/pages/docs/demos/description_brick_trunc.html" | relative_url }}">
                    <div class="uk-card uk-card-default uk-card-body uk-card-hover">
                        <div class="uk-card-media-top">
                            <img style="max-height:200px;" src="/LaGriT/pages/docs/demos/output/brick_tet_truncated.png" alt="">
                        </div>
                        <p> Structured Mesh with 2 Materials <br> Connect Tets and Truncate top <br>Basic</p>
                    </div>
                </a>
            </div>
        </li>
       <li data-color="demo">
            <div>
                <a href="{{ "/pages/docs/demos/main_extract_surfmesh.html" | relative_url }}">
                    <div class="uk-card uk-card-default uk-card-body uk-card-hover">
                        <div class="uk-card-media-top">
                            <img style="max-height:200px;" src="/LaGriT/pages/docs/demos/output/box_surf_all.png" alt="">
                        </div>
                        <p>Extract Boundary Surfaces <br> from Multi-material 3D Mesh <br> Basic</p>
                    </div>
                </a>
            </div>
        </li>
<!-- Moderate -->
        <li data-color="demo">
            <div>
                <a href="https://github.com/lanl/GeologicMeshing/tree/master/GDSA/">
                    <div class="uk-card uk-card-default uk-card-body uk-card-hover">
                        <div class="uk-card-media-top">
                            <img style="max-height:200px;" src="/LaGriT/pages/docs/demos/GDSA_4Tests_images.png" alt="">
                        </div>
                        <p> Four Octree Mesh Examples <br> for GDSA Testing (on github) <br> Moderate</p> 
                    </div>
                </a>
            </div>
        </li>	    
	    <li data-color="demo">
            <div>
                <a href="https://meshing.lanl.gov/proj/examples/ex_octree_refine_intersect_object/index.html">
                    <div class="uk-card uk-card-default uk-card-body uk-card-hover">
                        <div class="uk-card-media-top">
                            <img style="max-height:200px;" src="https://meshing.lanl.gov/proj/examples/ex_octree_refine_intersect_object/tets_add_02_TN.PNG" alt="">
                        </div>
                        <p>Octree Refine Hex <br> using Intersect with surfaces <br> Moderate</p>
                    </div>
                </a>
            </div>
        </li>
        <li data-color="demo">
            <div>
                <a href="{{ "/pages/docs/demos/main_rotateln.html" | relative_url }}">
                    <div class="uk-card uk-card-default uk-card-body uk-card-hover">
                        <div class="uk-card-media-top">
                            <img style="max-height:200px;" src="/LaGriT/pages/docs/demos/output/mesh_rot_tet.png" alt="">
                        </div>
                        <p>createpts and regnpts <br> for Rotated Cylinder in Box <br>Moderate</p> 
                    </div>
                </a>
            </div>
        </li>
        <li data-color="demo">
            <div>
                <a href="{{ "/pages/docs/demos/main_connect.html" | relative_url }}">
                    <div class="uk-card uk-card-default uk-card-body uk-card-hover">
                        <div class="uk-card-media-top">
                            <img style="max-height:200px;" src="/LaGriT/pages/docs/demos/output/wave_tet_highres.png" alt="">
                        </div>
                        <p>Stack cos() surfaces <br> and Connect Delaunay<br>Moderate  </p>
                    </div>
                </a>
            </div>
        </li>	    
	    <li data-color="demo">
            <div>
                <a href="{{ "/pages/docs/demos/description_hextet.html" | relative_url }}">
                    <div class="uk-card uk-card-default uk-card-body uk-card-hover">
                        <div class="uk-card-media-top">
                            <img style="max-height:200px;" src="/LaGriT/pages/docs/demos/output/wave_hextotet5_tet_edges.png" alt="">
                        </div>
                        <p>Stack cos() surfaces <br> convert Hex into Tets <br>Moderate</p>
                    </div>
                </a>
            </div>
        </li>
<!-- Advanced -->
         <li data-color="demo">
            <div>
                <a href="https://meshing.lanl.gov/proj/examples/stack_fs_from_bndry/method.html">
                    <div class="uk-card uk-card-default uk-card-body uk-card-hover">
                        <div class="uk-card-media-top">
                            <img style="max-height:200px;" src="https://meshing.lanl.gov/proj/examples/stack_fs_from_bndry/mesh_mat_fs5_and_fs8.png" alt="">
                        </div>
                        <p>Stack surfaces for Exodus Mesh<br> with Boundary facesets <br>Advanced</p>
                    </div>
                </a>
            </div>
        </li>
	<li data-color="demo">
            <div>
                <a href="{{ "/pages/docs/demos/description_poi_circle.html" | relative_url }}">
                    <div class="uk-card uk-card-default uk-card-body uk-card-hover">
                        <div class="uk-card-media-top">
                            <img style="max-height:200px;" src="/LaGriT/pages/docs/demos/output/poi_output04_tri_var1_lines.png" alt="">
                        </div>
                        <p>Createpts poisson_disk <br> from Circle polygon <br>Advanced</p>
                    </div>
                </a>
            </div>
        </li>
	<li data-color="demo">
            <div>
                <a href="{{ "/pages/docs/demos/demo_creatept_interp.html" | relative_url }}">
                    <div class="uk-card uk-card-default uk-card-body uk-card-hover">
                        <div class="uk-card-media-top">
                            <img style="max-height:200px;" src="/LaGriT/pages/docs/demos/output/cyl_stack_hex.png" alt="">
                        </div>
                        <p>2D Surface from Hex Sphere <br> Stack into Hex Cylinder <br>Advanced</p>
                    </div>
                </a>
            </div>
        </li>
<!-- tutorial cards -->
        <li data-color="tutorial">
            <div>
                <a href="{{ "/pages/tutorial/lagrit_introduction/index.html" | relative_url }}">
                    <div class="uk-card uk-card-default uk-card-body uk-card-hover">
                        <div class="uk-card-media-top">
                            <img style="max-height:200px;" src="lagrit_introduction/step_04/04_tet_nodes_imt_well_pts.png" alt="">
                        </div>
                        <p>Tutorial: Introduction to LaGriT<br>Beginner</p>
                    </div>
                </a>
            </div>
        </li>
      <li data-color="tutorial">
            <div>
                <a href="{{ "/pages/tutorial/stratigraphy/index.html" | relative_url }}">
                    <div class="uk-card uk-card-default uk-card-body uk-card-hover">
                        <div class="uk-card-media-top">
                            <img style="max-height:200px;" src="stratigraphy/images/21_tet_01_exo_blocks.png" alt="">
                        </div>
                        <p>Tutorial: Octree Mesh with <br>  Well and Stratigraphic Surfaces <br>Advanced</p>
                    </div>
                </a>
            </div>
        </li>
	<li data-color="tutorial">
            <div>
                <a href="{{ "/pages/tutorial/tutorials/regions_points_to_tet.html" | relative_url }}">
                    <div class="uk-card uk-card-default uk-card-body uk-card-hover">
                        <div class="uk-card-media-top">
                            <img style="max-height:200px;" src="https://lanl.github.io/LaGriT/assets/images/Image229.gif" alt="">
                        </div>
                        <p>Tutorial: Mesh from Regions and Ray Shooting<br>Deprecrated</p>
                    </div>
                </a>
            </div>
        </li>
<!-- utility cards -->
        <li data-color="utility">
            <div>
                <a href="https://meshing.lanl.gov/proj/examples/ex_quad_surface_prevent_crossing/index.html">
                    <div class="uk-card uk-card-command uk-card-body uk-card-hover">
                        <p>Merge Intersecting Surfaces</p>
                    </div>
                </a>
            </div>
        </li>
        <li data-color="utility">
            <div>
                <a href="{{ "/pages/docs/demos/extract_facesets.html" | relative_url }}">
                    <div class="uk-card uk-card-command uk-card-body uk-card-hover">
                        <p> Extract Face and Node Sets <br> with extract/surfmesh</p>
                    </div>
                </a>
            </div>
        </li>
	    <li data-color="utility">
            <div>
                <a href="{{ "/pages/tutorial/utility/convert_feet_to_meters.html" | relative_url }}">
                    <div class="uk-card uk-card-command uk-card-body uk-card-hover">
                        <p>Convert <br> Meters and Feet</p>
                    </div>
                </a>
            </div>
        </li>
        <li data-color="utility">
            <div>
                <a href="{{ "/pages/tutorial/utility/reset_truncated_nodes_imt.html" | relative_url }}">
                    <div class="uk-card uk-card-command uk-card-body uk-card-hover">
                        <p>Reset imt values <br> for truncated mesh</p>
                    </div>
                </a>
            </div>
        </li>
        <li data-color="utility">
            <div>
                <a href="{{ "/pages/tutorial/utility/cmo_addatt_vorvol.html" | relative_url }}">
                    <div class="uk-card uk-card-command uk-card-body uk-card-hover">
                        <p>Voronoi node attribute </p>
                    </div>
                </a>
            </div>
        </li>
<!-- command cards alphabetical -->
        <li data-color="command">
            <a href="{{ "/pages/docs/demos/main_addmesh.html" | relative_url }}" style="font-weight: bold;">
                <div class="uk-card uk-card-command uk-card-body">addmesh</div>
            </a>
        </li>
        <li data-color="command">
            <a href="{{ "/pages/docs/demos/main_2d_connect.html" | relative_url }}" style="font-weight: bold;">
                <div class="uk-card uk-card-command uk-card-body">connect 2D</div>
            </a>
        </li>
       <li data-color="command">
            <a href="{{ "/pages/docs/demos/description_connect.html" | relative_url }}" style="font-weight: bold;">
                <div class="uk-card uk-card-command uk-card-body">connect 3D</div>
            </a>
        </li>
	 <li data-color="command">
            <a href="{{ "/pages/docs/demos/main_createpts.html" | relative_url }}" style="font-weight: bold;">
                <div class="uk-card uk-card-command uk-card-body">createpts</div>
            </a>
        </li>
        <li data-color="command">
            <a href="{{ "/pages/docs/demos/main_dump.html" | relative_url }}" style="font-weight: bold;">
                <div class="uk-card uk-card-command uk-card-body">dump</div>
            </a>
        </li>
        <li data-color="command">
            <a href="{{ "/pages/docs/demos/description_hextet.html" | relative_url }}" style="font-weight: bold;">
                <div class="uk-card uk-card-command uk-card-body">grid2grid hextotet5</div>
            </a>
        </li>
        <li data-color="command">
            <a href="{{ "/pages/docs/demos/main_rivara.html" | relative_url }}" style="font-weight: bold;">
                <div class="uk-card uk-card-command uk-card-body">negative_aij</div>
            </a>
        </li>
        <li data-color="command">
            <a href="{{ "/pages/docs/demos/main_pset.html" | relative_url }}" style="font-weight: bold;">
                <div class="uk-card uk-card-command uk-card-body">pset</div>
            </a>
        </li>
        <li data-color="command">
            <a href="{{ "/pages/docs/demos/main_qual.html" | relative_url }}" style="font-weight: bold;">
                <div class="uk-card uk-card-command uk-card-body">quality</div>
            </a>
        </li>
        <li data-color="command">
            <a href="{{ "/pages/docs/demos/main_2d_recon.html" | relative_url }}" style="font-weight: bold;">
                <div class="uk-card uk-card-command uk-card-body">recon 2D</div>
            </a>
        </li>
        <li data-color="command">
            <a href="{{ "/pages/docs/demos/main_regnpts.html" | relative_url }}" style="font-weight: bold;">
                <div class="uk-card uk-card-command uk-card-body">regnpts</div>
            </a>
        </li>
        <li data-color="command">
            <a href="{{ "/pages/docs/demos/main_rmmat.html" | relative_url }}" style="font-weight: bold;">
                <div class="uk-card uk-card-command uk-card-body">rmmat</div>
            </a>
        </li>
        <li data-color="command">
            <a href="{{ "/pages/docs/demos/description_rotatept.html" | relative_url }}" style="font-weight: bold;">
                <div class="uk-card uk-card-command uk-card-body">rotatept</div>
            </a>
        </li>
        <li data-color="command">
            <a href="{{ "/pages/docs/demos/main_sort.html" | relative_url }}" style="font-weight: bold;">
                <div class="uk-card uk-card-command uk-card-body">sort</div>
            </a>
        </li>
        <li data-color="command">
            <a href="{{ "/pages/docs/demos/main_trans.html" | relative_url }}" style="font-weight: bold;">
                <div class="uk-card uk-card-command uk-card-body">trans</div>
            </a>
        </li>
        <li data-color="command">
            <a href="{{ "/pages/docs/demos/main_tri.html" | relative_url }}" style="font-weight: bold;">
                <div class="uk-card uk-card-command uk-card-body">triangulate</div>
            </a>
        </li>
    </ul>
</div>
<!-- Examples demos grid -->
