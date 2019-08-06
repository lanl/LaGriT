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
    <a href="{{ "/pages/tutorial/stratigraphy/index.html" | relative_url }}">
        <div class="uk-card-media-left uk-cover-container">
            <img src="{{ "/pages/tutorial/stratigraphy/images/19_hex_01_to_tet.png" | relative_url }}" alt="" uk-cover>
            <canvas width="600" height="400"></canvas>
        </div>
    </a>
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

<h1 class="uk-heading-line"><span>Examples &amp; Demos</span></h1>

<!-- Demos grid card -->
<div class="uk-child-width-1-4@m uk-grid-small uk-grid-match" uk-grid>
    <div>
        <a href="{{ "/pages/docs/demos/main_2d_connect.html" | relative_url }}">
            <div class="uk-card uk-card-default uk-card-body uk-card-hover">
                <div class="uk-card-media-top">
                    <img src="https://lagrit.lanl.gov/docs/demos/2d_connect/test/html/image/2d_connect2_tn.gif" alt="">
                </div>
                <p>2d_connect</p>
            </div>
        </a>
    </div>

    <div>
        <a href="{{ "/pages/docs/demos/main_rotatept.html" | relative_url }}">
            <div class="uk-card uk-card-default uk-card-body uk-card-hover">
                <div class="uk-card-media-top">
                    <img src="https://lagrit.lanl.gov/docs/demos/rotatept/test/html/image/rotatept2_tn.gif" alt="">
                </div>
                <p>rotatept</p>
            </div>
        </a>
    </div>

    <div>
        <a href="{{ "/pages/docs/demos/main_createpts.html" | relative_url }}">
            <div class="uk-card uk-card-default uk-card-body uk-card-hover">
                <div class="uk-card-media-top">
                    <img src="https://lagrit.lanl.gov/docs/demos/createpts/test/html/image/image6tn.gif" alt="" width="70">
                </div>
                <p>createpts</p>
            </div>
        </a>
    </div>

    <div>
        <a href="{{ "/pages/docs/demos/main_regnpts.html" | relative_url }}">
            <div class="uk-card uk-card-default uk-card-body uk-card-hover">
                <div class="uk-card-media-top">
                    <img src="https://lagrit.lanl.gov/docs/demos/regnpts/test/html/image/regnpts2_tn.gif" alt="">
                </div>
                <p>regnpts</p>
            </div>
        </a>
    </div>

    <div>
        <a href="{{ "/pages/docs/demos/main_tri.html" | relative_url }}">
            <div class="uk-card uk-card-default uk-card-body uk-card-hover">
                <div class="uk-card-media-top">
                    <img src="https://lagrit.lanl.gov/docs/demos/triangulate/test/html/image/triang4_tn.gif" alt="">
                </div>
                <p>triangulate</p>
            </div>
        </a>
    </div>

    <div>
        <a href="{{ "/pages/docs/demos/main_dump.html" | relative_url }}">
            <div class="uk-card uk-card-default uk-card-body uk-card-hover">
                <div class="uk-card-media-top">
                    <img src="https://lagrit.lanl.gov/docs/demos/dump/test/html/image/output_tn.gif" alt="">
                </div>
                <p>dump</p>
            </div>
        </a>
    </div>

    <div>
        <a href="{{ "/pages/docs/demos/main_hextet.html" | relative_url }}">
            <div class="uk-card uk-card-default uk-card-body uk-card-hover">
                <div class="uk-card-media-top">
                    <img src="https://lagrit.lanl.gov/docs/demos/hextotet/test/html/image/output_tet_tn.gif" alt="">
                </div>
                <p>hextotet</p>
            </div>
        </a>
    </div>

    <div>
        <a href="{{ "/pages/docs/demos/main_sort.html" | relative_url }}">
            <div class="uk-card uk-card-default uk-card-body uk-card-hover">
                <div class="uk-card-media-top">
                    <img src="https://lagrit.lanl.gov/docs/demos/sort/test/html/image/sort_tn.gif" alt="">
                </div>
                <p>sort</p>
            </div>
        </a>
    </div>

    <div>
        <a href="{{ "/pages/docs/demos/main_connect.html" | relative_url }}">
            <div class="uk-card uk-card-default uk-card-body uk-card-hover">
                <div class="uk-card-media-top">
                    <img src="https://lagrit.lanl.gov/docs/demos/connect/test/html/image/output_connect_tn.gif" alt="">
                </div>
                <p>connect</p>
            </div>
        </a>
    </div>

    <div>
        <a href="{{ "/pages/docs/demos/main_qual.html" | relative_url }}">
            <div class="uk-card uk-card-default uk-card-body uk-card-hover">
                <div class="uk-card-media-top">
                    <img src="https://lagrit.lanl.gov/docs/demos/2d_recon/test/html/image/image1_tn.gif" alt="" width="65">
                </div>
                <p>quality</p>
            </div>
        </a>
    </div>

    <div>
        <a href="{{ "/pages/docs/demos/main_trans.html" | relative_url }}">
            <div class="uk-card uk-card-default uk-card-body uk-card-hover">
                <div class="uk-card-media-top">
                    <img src="https://lagrit.lanl.gov/docs/demos/trans/test/html/image/trans2_tn.gif" alt="">
                </div>
                <p>trans</p>
            </div>
        </a>
    </div>

    <div>
        <a href="{{ "/pages/docs/demos/main_pset.html" | relative_url }}">
            <div class="uk-card uk-card-default uk-card-body uk-card-hover">
                <div class="uk-card-media-top">
                    <img src="https://lagrit.lanl.gov/docs/demos/pset/test/html/image/pset2_tn.gif" alt="">
                </div>
                <p>pset</p>
            </div>
        </a>
    </div>

    <div>
        <a href="{{ "/pages/docs/demos/main_2d_recon.html" | relative_url }}">
            <div class="uk-card uk-card-default uk-card-body uk-card-hover">
                <div class="uk-card-media-top">
                    <img src="https://lagrit.lanl.gov/docs/demos/2d_recon/test/html/image/image1_tn.gif" alt="" width="65">
                </div>
                <p>2d_recon</p>
            </div>
        </a>
    </div>

    <div>
        <a href="{{ "/pages/docs/demos/main_addmesh.html" | relative_url }}">
            <div class="uk-card uk-card-default uk-card-body uk-card-hover">
                <div class="uk-card-media-top">
                    <img src="https://lagrit.lanl.gov/docs/demos/addmesh/test/html/image/addmesh_add/addmesh_out1_tn.gif" alt="">
                </div>
                <p>addmesh</p>
            </div>
        </a>
    </div>

    <div>
        <a href="{{ "/pages/docs/demos/main_rmmat.html" | relative_url }}">
            <div class="uk-card uk-card-default uk-card-body uk-card-hover">
                <div class="uk-card-media-top">
                    <img src="https://lagrit.lanl.gov/docs/demos/rmmat/test/html/image/rmmat4_tn.gif" alt="">
                </div>
                <p>rmmat</p>
            </div>
        </a>
    </div>

    <div>
        <a href="{{ "/pages/docs/demos/main_rivara.html" | relative_url }}">
            <div class="uk-card uk-card-default uk-card-body uk-card-hover">
                <div class="uk-card-media-top">
                    <img src="https://lagrit.lanl.gov/docs/demos/refine_rivara/test/html/image/rivara2_tn.gif" alt="">
                </div>
                <p>negative_aij</p>
            </div>
        </a>
    </div>
</div>
<!-- End demos grid -->