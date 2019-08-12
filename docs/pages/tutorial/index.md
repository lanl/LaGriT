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
<div class="uk-card uk-card-default uk-grid-collapse uk-child-width-1-2@s uk-margin" uk-grid>
        <div class="uk-card-media-left uk-cover-container">
            <img src="{{ "/pages/tutorial/stratigraphy/images/19_hex_01_to_tet.png" | relative_url }}" alt="" uk-cover>
            <canvas width="600" height="400"></canvas>
        </div>
    <a href="{{ "/pages/tutorial/stratigraphy/index.html" | relative_url }}">
        <div>
            <div class="uk-card-body">
                <h3 class="uk-card-title">Stratigraphic Hex Mesh Tutorial</h3>
                <p>Complex stratigraphic mesh creation with faults, wells, and subsurface layering</p>
                <span class="uk-badge">Advanced</span>
            </div>
        </div>
    </a>
</div>
<!-- End tutorial card -->

<br/><br/>

<h1 class="uk-heading-line"><span>Examples</span></h1>

<!-- Examples grid card -->
<div uk-filter="target: .js-filter">

    <ul class="uk-subnav uk-subnav-pill">
        <li class="uk-active" uk-filter-control><a href="#">All</a></li>
        <li uk-filter-control="[data-color='command']"><a href="#">Commands</a></li>
        <li uk-filter-control="[data-color='utility']"><a href="#">Utility</a></li>
        <li uk-filter-control="[data-color='workflow']"><a href="#">Workflow</a></li>
    </ul>

    <ul class="js-filter uk-child-width-1-2 uk-child-width-1-3@m uk-text-center" uk-grid="masonry: true">
        <li data-color="workflow">
            <div>
                <a href="https://meshing.lanl.gov/proj/examples/stack_fs_from_bndry/method.html">
                    <div class="uk-card uk-card-default uk-card-body uk-card-hover">
                        <div class="uk-card-media-top">
                            <img src="https://meshing.lanl.gov/proj/examples/stack_fs_from_bndry/mesh_mat_fs5_and_fs8.png" alt="">
                        </div>
                        <p>Stacked Mesh with Facesets from Boundary Lines</p>
                    </div>
                </a>
            </div>
        </li>
        <li data-color="workflow">
            <div>
                <a href="https://meshing.lanl.gov/proj/examples/ex_octree_refine_intersect_object/index.html">
                    <div class="uk-card uk-card-default uk-card-body uk-card-hover">
                        <div class="uk-card-media-top">
                            <img src="https://meshing.lanl.gov/proj/examples/ex_octree_refine_intersect_object/tets_add_02_TN.PNG" alt="">
                        </div>
                        <p>Octree Refine Hex, Intersect Object</p>
                    </div>
                </a>
            </div>
        </li>
        <li data-color="utility">
            <div>
                <a href="https://meshing.lanl.gov/proj/examples/ex_quad_surface_prevent_crossing/index.html">
                    <div class="uk-card uk-card-default uk-card-body uk-card-hover">
                        <div class="uk-card-media-top">
                            <img src="https://meshing.lanl.gov/proj/examples/ex_quad_surface_prevent_crossing/03_output_modified_surface.gif" alt="">
                        </div>
                        <p>Merge Intersecting Surfaces</p>
                    </div>
                </a>
            </div>
        </li>
        <li data-color="utility">
            <div>
                <a href="{{ "/pages/tutorial/utility/convert_feet_to_meters.html" | relative_url }}">
                    <div class="uk-card uk-card-default uk-card-body uk-card-hover">
                        <div class="uk-card-media-top">
                            <span uk-icon="icon: code; ratio: 2"></span>
                        </div>
                        <p>Convert Between Meters and Feet</p>
                    </div>
                </a>
            </div>
        </li>
        <li data-color="workflow">
            <div>
                <a href="{{ "/pages/tutorial/workflow/regions_points_to_tet.html" | relative_url }}">
                    <div class="uk-card uk-card-default uk-card-body uk-card-hover">
                        <div class="uk-card-media-top">
                            <img src="https://lanl.github.io/LaGriT/assets/images/Image229.gif" alt="">
                        </div>
                        <p>Regions and Points to Tet Mesh</p>
                    </div>
                </a>
            </div>
        </li>
        <li data-color="command">
            <a href="{{ "/pages/docs/demos/main_2d_connect.html" | relative_url }}" style="font-weight: bold;">
                <div class="uk-card uk-card-default uk-card-body">2d_connect</div>
            </a>
        </li>
        <li data-color="command">
            <a href="{{ "/pages/docs/demos/main_rotatept.html" | relative_url }}" style="font-weight: bold;">
                <div class="uk-card uk-card-default uk-card-body">rotatept</div>
            </a>
        </li>
        <li data-color="command">
            <a href="{{ "/pages/docs/demos/main_createpts.html" | relative_url }}" style="font-weight: bold;">
                <div class="uk-card uk-card-default uk-card-body">createpts</div>
            </a>
        </li>
        <li data-color="command">
            <a href="{{ "/pages/docs/demos/main_regnpts.html" | relative_url }}" style="font-weight: bold;">
                <div class="uk-card uk-card-default uk-card-body">regnpts</div>
            </a>
        </li>
        <li data-color="command">
            <a href="{{ "/pages/docs/demos/main_tri.html" | relative_url }}" style="font-weight: bold;">
                <div class="uk-card uk-card-default uk-card-body">triangulate</div>
            </a>
        </li>
        <li data-color="command">
            <a href="{{ "/pages/docs/demos/main_dump.html" | relative_url }}" style="font-weight: bold;">
                <div class="uk-card uk-card-default uk-card-body">dump</div>
            </a>
        </li>
        <li data-color="command">
            <a href="{{ "/pages/docs/demos/main_hextet.html" | relative_url }}" style="font-weight: bold;">
                <div class="uk-card uk-card-default uk-card-body">hextotet</div>
            </a>
        </li>
        <li data-color="command">
            <a href="{{ "/pages/docs/demos/main_sort.html" | relative_url }}" style="font-weight: bold;">
                <div class="uk-card uk-card-default uk-card-body">sort</div>
            </a>
        </li>
        <li data-color="command">
            <a href="{{ "/pages/docs/demos/main_connect.html" | relative_url }}" style="font-weight: bold;">
                <div class="uk-card uk-card-default uk-card-body">connect</div>
            </a>
        </li>
        <li data-color="command">
            <a href="{{ "/pages/docs/demos/main_qual.html" | relative_url }}" style="font-weight: bold;">
                <div class="uk-card uk-card-default uk-card-body">quality</div>
            </a>
        </li>
        <li data-color="command">
            <a href="{{ "/pages/docs/demos/main_trans.html" | relative_url }}" style="font-weight: bold;">
                <div class="uk-card uk-card-default uk-card-body">trans</div>
            </a>
        </li>
        <li data-color="command">
            <a href="{{ "/pages/docs/demos/main_pset.html" | relative_url }}" style="font-weight: bold;">
                <div class="uk-card uk-card-default uk-card-body">pset</div>
            </a>
        </li>
        <li data-color="command">
            <a href="{{ "/pages/docs/demos/main_2d_recon.html" | relative_url }}" style="font-weight: bold;">
                <div class="uk-card uk-card-default uk-card-body">2d_recon</div>
            </a>
        </li>
        <li data-color="command">
            <a href="{{ "/pages/docs/demos/main_addmesh.html" | relative_url }}" style="font-weight: bold;">
                <div class="uk-card uk-card-default uk-card-body">addmesh</div>
            </a>
        </li>
        <li data-color="command">
            <a href="{{ "/pages/docs/demos/main_rmmat.html" | relative_url }}" style="font-weight: bold;">
                <div class="uk-card uk-card-default uk-card-body">rmmat</div>
            </a>
        </li>
        <li data-color="command">
            <a href="{{ "/pages/docs/demos/main_rivara.html" | relative_url }}" style="font-weight: bold;">
                <div class="uk-card uk-card-default uk-card-body">negative_aij</div>
            </a>
        </li>
    </ul>
</div>
<!-- Examples demos grid -->