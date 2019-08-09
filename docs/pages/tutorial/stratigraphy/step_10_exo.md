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