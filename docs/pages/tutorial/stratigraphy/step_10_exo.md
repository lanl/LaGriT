<!-- Begin breadcrumb -->
<ul class="uk-breadcrumb">
    <li><a href="{{ "/pages/tutorial/index.html" | relative_url }}">Tutorials &amp; Examples</a></li>
    <li><span>Stratigraphic Hex Mesh Tutorial</span></li>
</ul>
<!-- End breadcrumb -->

<!-- Begin image -->
<img data-src="{{ "/pages/tutorial/stratigraphy/images/21_tet_01_exo_blocks.png" | relative_url }}" width="639" height="525" alt="" uk-img>
<br/>
<!-- End image -->

<h2 id="dump-exo" class="uk-h3 uk-margin-remove">10.2 Write ExodusII Files</h2>

Write the tet mesh in Exodus II format. This format defines materials as blocks
and is element based.
The node properties will be ignored. The below image shows the Exodus mesh
(read with GMV or ParaView) with material blocks shown in 'exploded' view.

<!-- Lightbox -->
<div class="uk-child-width-1-2@m" uk-grid uk-lightbox="animation: slide">
    <div>
        <a class="uk-inline" href="{{ "/pages/tutorial/stratigraphy/images/21_tet_01_exo_blocks_ex.png" | relative_url }}" data-caption="Exodus cells 'exploded' by material blocks">
            <img src="{{ "/pages/tutorial/stratigraphy/images/21_tet_01_exo_blocks_ex.png" | relative_url }}" alt="">
        </a>
    </div>
</div>

We can define the boundary faces for this mesh by extracting the 2D external
surface and writing the element and face id for each. These can be written as
sets based on element selection.

In this example we use `settets/normal` to assign values 1-6 to the faces
based on normal directions where `1=bot`, `2=top`, `3=right`, `4=back`, `5=left`,
`6=front`. Define element sets for top, bottom, and lump sides
all together for set 3. Write the faceset relation for each element set.

```
define / FILENAME / ss3_sides.faceset
define / SS_ID / 3
  cmo / copy / mo_tmp / mo_surf
  cmo / select / mo_tmp
  eltset / e_keep / itetclr / eq / SS_ID
  eltset / e_delete / not / e_keep
  rmpoint / element / eltset get e_delete
  rmpoint / compress
  dump / avs2 / FILENAME / mo_tmp / 0 0 0 2
  cmo / delete / mo_tmp


define / FILENAME / ss1_bottom.faceset
define / SS_ID / 1
  cmo / copy / mo_tmp / mo_surf
  cmo / select / mo_tmp
  eltset / e_keep / itetclr / eq / SS_ID
  eltset / e_delete / not / e_keep
  rmpoint / element / eltset get e_delete
  rmpoint / compress
  dump / avs2 / FILENAME / mo_tmp / 0 0 0 2
  cmo / delete / mo_tmp


define / FILENAME / ss2_top.faceset
define / SS_ID / 2
  cmo / copy / mo_tmp / mo_surf
  cmo / select / mo_tmp
  eltset / e_keep / itetclr / eq / SS_ID
  eltset / e_delete / not / e_keep
  rmpoint / element / eltset get e_delete
  rmpoint / compress
  dump / avs2 / FILENAME / mo_tmp / 0 0 0 2
  cmo / delete / mo_tmp
```

Write the Exodus II mesh with the faceset files. This mesh and associated
face sets can be viewed with GMV or ParaView.

```
dump/exo/ mesh_fs.exo / MO_MESH / / / facesets &
    ss1_bottom.faceset, ss2_top.faceset,  ss3_sides.faceset
```

Check the summary report to see that mesh quantities are as expected.
The report shows 6 element blocks (materials) and 3 side sets with appropriate face counts.

```
Title: LAGRIT TO EXODUSII                                                       
number of dimension:               3                                            
number of nodes:              115547                                            
number of elements:           651950                                            
number of edges:                   0                                            
number of edge blocks:             0                                            
number of element blocks:          6                                            
number of face blocks:             0                                            
number of node sets:               0                                            
number of edge sets:               0                                            
number of element sets:            0                                            
number of side sets:               3                                            
number of face sets:               0                                            
number of node maps:               0                                            
number of edge maps:               0                                            
number of face maps:               0                                            
number of element maps:            0                                            
 
 
------------------------------------------                                      
EXPSS loop:                                                                     
        1 Side Set tag:             1 Faces:          4098                      
        2 Side Set tag:             2 Faces:          4640                      
        3 Side Set tag:             3 Faces:         11812                      
------------------------------------------                                      
Done ExodusII Side Sets Total:       3                                   
```

Image show the Exodus face sets (side sets) with bottom (blue), top (red), and sides (light blue).

<!-- Lightbox -->
<div class="uk-child-width-1-2@m" uk-grid uk-lightbox="animation: slide">
    <div>
        <a class="uk-inline" href="{{ "/pages/tutorial/stratigraphy/images/21_tet_01_exo_bndry_faces.png" | relative_url }}" data-caption="Exodus side sets: bottom (blue), top (red), and sides (light blue).">
            <img src="{{ "/pages/tutorial/stratigraphy/images/21_tet_01_exo_bndry_faces.png" | relative_url }}" alt="">
        </a>
    </div>
</div>

<!-- Next / Prev -->
<ul class="uk-pagination">
    <li><a href="{{ "/pages/tutorial/stratigraphy/step_10_fehm.html" | relative_url }}"><span class="uk-margin-small-right" uk-pagination-previous></span> Previous</a></li>
</ul>

<!-- Sidebar -->
<div class="tm-sidebar-right uk-visible@l">
    <div uk-sticky="offset: 160" class="uk-sticky uk-active uk-sticky-fixed" style="position: fixed; top: 160px; width: 200px;">
        <ul uk-scrollspy-nav="closest: li; scroll: true; offset: 100" class="uk-nav uk-nav-default tm-nav uk-nav-parent-icon">
            <li class=""><a href="{{ "/pages/tutorial/stratigraphy/index.html" | relative_url }}">Index</a></li>
            <li class=""><a href="{{ "/pages/tutorial/stratigraphy/step_01.html" | relative_url }}">1. Building a Hex Mesh</a></li>
            <li class=""><a href="{{ "/pages/tutorial/stratigraphy/step_02.html" | relative_url }}">2. Define Boundaries Using Point Sets</a></li>
            <li class=""><a href="{{ "/pages/tutorial/stratigraphy/step_03.html" | relative_url }}">3. Constructing Stratigraphy</a></li>
            <li class=""><a href="{{ "/pages/tutorial/stratigraphy/step_04.html" | relative_url }}">4. Map Surfaces to Mesh</a></li>
            <li class=""><a href="{{ "/pages/tutorial/stratigraphy/step_05.html" | relative_url }}">5. Constructing a Fault</a></li>
            <li class=""><a href="{{ "/pages/tutorial/stratigraphy/step_06.html" | relative_url }}">6. Truncate with Polyline</a></li>
            <li class=""><a href="{{ "/pages/tutorial/stratigraphy/step_07.html" | relative_url }}">7. Refine Fault</a></li>
            <li class=""><a href="{{ "/pages/tutorial/stratigraphy/step_08.html" | relative_url }}">8. Insert Wells</a></li>
            <li class=""><a href="{{ "/pages/tutorial/stratigraphy/step_09.html" | relative_url }}">9. Convert Hex Mesh to Tet</a></li>
            <li class=""><a href="{{ "/pages/tutorial/stratigraphy/step_10_fehm.html" | relative_url }}">10.1 Write FEHM Files</a></li>
            <li class="uk-active"><a href="#dump-exo">10.2 Write ExodusII Files</a></li>
            <li class="uk-nav-divider"></li>
            <!---->
            <li><a href="{{ "/pages/tutorial/stratigraphy/images/gallery.html" | relative_url }}" target="_blank"><span uk-icon="icon: image" class="uk-margin-small-right uk-icon"></span> <span class="uk-text-middle">Image Gallery</span></a></li>
            <li><a href="https://github.com/lanl/LaGriT/issues" target="_blank"><span uk-icon="icon: warning" class="uk-margin-small-right uk-icon"><svg width="20" height="20" viewBox="0 0 20 20" xmlns="http://www.w3.org/2000/svg" data-svg="warning"><circle cx="10" cy="14" r="1"></circle><circle fill="none" stroke="#000" stroke-width="1.1" cx="10" cy="10" r="9"></circle><path d="M10.97,7.72 C10.85,9.54 10.56,11.29 10.56,11.29 C10.51,11.87 10.27,12 9.99,12 C9.69,12 9.49,11.87 9.43,11.29 C9.43,11.29 9.16,9.54 9.03,7.72 C8.96,6.54 9.03,6 9.03,6 C9.03,5.45 9.46,5.02 9.99,5 C10.53,5.01 10.97,5.44 10.97,6 C10.97,6 11.04,6.54 10.97,7.72 L10.97,7.72 Z"></path></svg></span> <span class="uk-text-middle">Report issue</span></a></li>
            <li><a href="mailto:lagrit-dev@lanl.gov" target="_blank"><span uk-icon="icon: commenting" class="uk-margin-small-right uk-icon"><svg width="20" height="20" viewBox="0 0 20 20" xmlns="http://www.w3.org/2000/svg" data-svg="commenting"><polygon fill="none" stroke="#000" points="1.5,1.5 18.5,1.5 18.5,13.5 10.5,13.5 6.5,17.5 6.5,13.5 1.5,13.5"></polygon><circle cx="10" cy="8" r="1"></circle><circle cx="6" cy="8" r="1"></circle><circle cx="14" cy="8" r="1"></circle></svg></span> <span class="uk-text-middle">Get help</span></a></li>
        </ul>
    </div>
</div>