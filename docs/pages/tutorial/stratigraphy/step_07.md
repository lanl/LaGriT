<!-- Begin breadcrumb -->
<ul class="uk-breadcrumb">
    <li><a href="{{ "/pages/tutorial/index.html" | relative_url }}">Tutorials &amp; Examples</a></li>
    <li><span>Stratigraphic Hex Mesh Tutorial</span></li>
</ul>
<!-- End breadcrumb -->

<!-- Begin image slideshow -->
<div class="uk-position-relative uk-visible-toggle uk-light" tabindex="-1" uk-slideshow>
    <ul class="uk-slideshow-items">
        <li>
            <img data-src="{{ "/pages/tutorial/stratigraphy/images/07_hex_01_fault_refine.png" | relative_url }}" width="1800" height="1200" alt="" uk-cover uk-img="target: !.uk-slideshow-items">
        </li>
        <li>
            <img data-src="{{ "/pages/tutorial/stratigraphy/images/07_hex_01_fault_refine_close_up.png" | relative_url }}" width="1800" height="1200" alt="" uk-cover uk-img="target: !.uk-slideshow-items">
        </li>
    </ul>
    <a class="uk-position-center-left uk-position-small uk-hidden-hover" href="#" uk-slidenav-previous uk-slideshow-item="previous"></a>
    <a class="uk-position-center-right uk-position-small uk-hidden-hover" href="#" uk-slidenav-next uk-slideshow-item="next"></a>
</div>
<!-- End image slideshow -->
<br/>

<h2 id="refine-fault" class="uk-h3 uk-margin-remove">7. Refine Fault</h2>

In step 5, we defined a fault surface (named `s_f`) intersecting the mesh. In 
this section, we are going to refine the mesh where the fault intersects it.

The LaGriT command `intersect_elements` takes two meshes and creates an
element-based attribute in mesh1 that contains the number of elements in mesh2
that intersected the respective element in mesh1.

Performing the intersection between the mesh `MONAME` and fault `s_f`, storing
the intersection count in attribute `if_inter`:

```
cmo / select / MONAME
intersect_elements / MONAME / s_f / if_inter
```

The attribute `if_inter` will be non-zero everywhere there is an intersection,
and zero where there was not intersection. Taking advantage of this fact,
we can create an eltset to refine:

```
eltset / e_refine / if_inter / gt / 0
refine / eltset / eltset get e_refine
cmo / DELATT / MONAME / if_inter
```

The element density has now increased where the mesh intersects `s_f`.
We can then run this step *again*, further refining elements along that intersection:

```
intersect_elements / MONAME / s_f / if_inter
eltset / e_refine / if_inter / gt / 0
refine/ eltset / eltset get e_refine
cmo / setatt / MONAME / if_inter / 1 0 0 / 0
```

And finally, remove the attribute `if_inter`, as it is no longer needed:

```
cmo / DELATT / MONAME / if_inter
```

At this point, many different mesh objects and surfaces still exist in memory from previous steps. Let's deallocate them and verify with the `memory` command:

```
cmo / status / brief
cmo / status / MONAME
memory / verify
memory / print

cmo / delete / s_bndry
cmo / delete / mo_fence
cmo / delete / mo_bndry
cmo / delete / s_f
cmo / delete / s_2_fp
cmo / delete / s_1_fp
cmo / delete / s_2_fm
cmo / delete / s_1_fm
cmo / delete / s_2
cmo / delete / s_1

memory / verify
memory / print
```

<!-- Next / Prev -->
<ul class="uk-pagination">
    <li><a href="{{ "/pages/tutorial/stratigraphy/step_06.html" | relative_url }}"><span class="uk-margin-small-right" uk-pagination-previous></span> Previous</a></li>
    <li class="uk-margin-auto-left"><a href="{{ "/pages/tutorial/stratigraphy/step_08.html" | relative_url }}">Next <span class="uk-margin-small-left" uk-pagination-next></span></a></li>
</ul>

<!-- Sidebar -->
<div class="tm-sidebar-right uk-visible@l">
    <div uk-sticky="offset: 160" class="uk-sticky uk-active uk-sticky-fixed" style="position: fixed; top: 160px; width: 200px;">
        <ul uk-scrollspy-nav="closest: li; scroll: true; offset: 100" class="uk-nav uk-nav-default tm-nav uk-nav-parent-icon">
            <li class=""><a href="{{ "/pages/tutorial/stratigraphy/index.html" | relative_url }}">Index</a></li>
            <li class=""><a href="{{ "/pages/tutorial/stratigraphy/step_01.html" | relative_url }}">Building a Hex Mesh</a></li>
            <li class=""><a href="{{ "/pages/tutorial/stratigraphy/step_02.html" | relative_url }}">Define Boundaries Using Point Sets</a></li>
            <li class=""><a href="{{ "/pages/tutorial/stratigraphy/step_03.html" | relative_url }}">Constructing Stratigraphy</a></li>
            <li class=""><a href="{{ "/pages/tutorial/stratigraphy/step_04.html" | relative_url }}">Map Surfaces to Mesh</a></li>
            <li class=""><a href="{{ "/pages/tutorial/stratigraphy/step_05.html" | relative_url }}">Constructing a Fault</a></li>
            <li class=""><a href="{{ "/pages/tutorial/stratigraphy/step_06.html" | relative_url }}">Truncate with Polyline</a></li>
            <li class="uk-active"><a href="#refine-fault">Refine Fault</a></li>
            <li class=""><a href="{{ "/pages/tutorial/stratigraphy/step_08.html" | relative_url }}">Insert Well</a></li>
            <li class=""><a href="{{ "/pages/tutorial/stratigraphy/step_09.html" | relative_url }}">Convert Hex Mesh to Tet</a></li>
            <li class="uk-nav-divider"></li>
            <!---->
            <li><a href="https://github.com/lanl/LaGriT/issues" target="_blank"><span uk-icon="icon: warning" class="uk-margin-small-right uk-icon"><svg width="20" height="20" viewBox="0 0 20 20" xmlns="http://www.w3.org/2000/svg" data-svg="warning"><circle cx="10" cy="14" r="1"></circle><circle fill="none" stroke="#000" stroke-width="1.1" cx="10" cy="10" r="9"></circle><path d="M10.97,7.72 C10.85,9.54 10.56,11.29 10.56,11.29 C10.51,11.87 10.27,12 9.99,12 C9.69,12 9.49,11.87 9.43,11.29 C9.43,11.29 9.16,9.54 9.03,7.72 C8.96,6.54 9.03,6 9.03,6 C9.03,5.45 9.46,5.02 9.99,5 C10.53,5.01 10.97,5.44 10.97,6 C10.97,6 11.04,6.54 10.97,7.72 L10.97,7.72 Z"></path></svg></span> <span class="uk-text-middle">Report issue</span></a></li>
            <li><a href="mailto:lagrit-dev@lanl.gov" target="_blank"><span uk-icon="icon: commenting" class="uk-margin-small-right uk-icon"><svg width="20" height="20" viewBox="0 0 20 20" xmlns="http://www.w3.org/2000/svg" data-svg="commenting"><polygon fill="none" stroke="#000" points="1.5,1.5 18.5,1.5 18.5,13.5 10.5,13.5 6.5,17.5 6.5,13.5 1.5,13.5"></polygon><circle cx="10" cy="8" r="1"></circle><circle cx="6" cy="8" r="1"></circle><circle cx="14" cy="8" r="1"></circle></svg></span> <span class="uk-text-middle">Get help</span></a></li>
        </ul>
    </div>
</div>