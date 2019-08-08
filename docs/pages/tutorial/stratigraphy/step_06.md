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
            <img data-src="{{ "/pages/tutorial/stratigraphy/images/06_truncate_set_id.png" | relative_url }}" width="1800" height="1200" alt="" uk-cover uk-img="target: !.uk-slideshow-items">
        </li>
        <li>
            <img data-src width="1800" height="1200" alt="" uk-cover uk-img="target: !.uk-slideshow-items">
        </li>
        <li>
            <img data-src width="1800" height="1200" alt="" uk-cover uk-img="target: !.uk-slideshow-items">
        </li>
        <li>
            <img data-src width="1800" height="1200" alt="" uk-cover uk-img="target: !.uk-slideshow-items">
        </li>
    </ul>
</div>
<!-- End image slideshow -->
<br/>

<h2 id="truncate-with-polyline" class="uk-h3 uk-margin-remove">6. Truncate with Polyline</h2>

### 6.1 Create boundary surface

We now have a mesh with complex stratigraphy encoded in its material ID. 
However, the domain of this mesh is a rather boring cuboid and doesn't
accurately reflect the geospatial domain that we are trying to model.

By importing a polyline, the exterior boundary of the mesh can be truncated.
As in previous steps, we will use a mesh to define a `region` that will be used
for element-wise operations.

However, the boundary is a line object. In order to use it as a surface/region,
it must be a surface. A polyline can be turned into a vertical surface by
'extruding' it in the vertical (0,0,1) direction:

```
read / avs / basin_bnd_ply_rescale.inp / mo_bndry
extrude / mo_fence / mo_bndry / const / 3200. / volume / 0. 0. -1.
```

We will also translate the extrusion so that it covers the vertical extent
of the hex mesh:

```
trans / 1 0 0 / 0. 0. -3200. / 0. 0. 0.
```

<!-- Lightbox -->
<div class="uk-child-width-1-2@m" uk-grid uk-lightbox="animation: slide">
    <div>
        <a class="uk-inline" href="{{ "/pages/tutorial/stratigraphy/images/06_boundary_truncate.png" | relative_url }}" data-caption="Polyline boundary">
            <img src="{{ "/pages/tutorial/stratigraphy/images/06_boundary_truncate.png" | relative_url }}" alt="">
        </a>
    </div>
    <div>
        <a class="uk-inline" href="{{ "/pages/tutorial/stratigraphy/images/06_boundary_truncate_fence.png" | relative_url }}" data-caption="Polyline boundary extruded">
            <img src="{{ "/pages/tutorial/stratigraphy/images/06_boundary_truncate_fence.png" | relative_url }}" alt="">
        </a>
    </div>
</div>

### 6.2 Truncate mesh

Next, we use the boundary to truncate (remove) cells outside the boundary.

There are three ways to define 'outside':

1. Only remove a cell if ALL vertices are outside
2. Remove a cell if the centroid (average of all vertices) is outside
3. Remove a cell if one or more vertices are outside

```
cmo / select / MONAME
surface / s_bndry / reflect / sheet / mo_fence
cmo / select / MONAME
region / r_bndry / ge s_bndry
pset / p_bndry / region r_bndry
```
**Method 1:**

    eltset / e_delete1 / exclusive / pset get p_bndry

**Method 2:**

    eltset / e_delete2 / region r_bndry

**Method 3:**

    eltset / e_delete3 / inclusive / pset get p_bndry

Next, add the integer cell attribute `id_in_out_bndry`:

    cmo / addatt / MONAME / id_in_out_bndry / vint / scalar / nelements

and 'color' it based on the three element sets created above:

```
cmo / setatt / MONAME / id_in_out_bndry / 1 0 0 / 4
cmo / setatt / MONAME / id_in_out_bndry / eltset get e_delete3 / 3
cmo / setatt / MONAME / id_in_out_bndry / eltset get e_delete2 / 2
cmo / setatt / MONAME / id_in_out_bndry / eltset get e_delete1 / 1
```

All elements colored 4 (the default value for `id_in_out_bndry`) are elements
who evaluate false for all of the above `eltsets`.

Create an element set for all `id_in_out_bndry(cell_i) == 4`:

```
eltset / e_delete4 /    id_in_out_bndry / eq / 4
eltset / e_delete3 /    id_in_out_bndry / eq / 3
eltset / e_delete2 /    id_in_out_bndry / eq / 2
eltset / e_delete1 /    id_in_out_bndry / eq / 1
```

Finally, remove all elements 'colored' by method 3 and all elements *not* colored by any of methods 1, 2, or 3:

```
rmpoint / element / eltset get e_delete4
rmpoint / element / eltset get e_delete3
rmpoint / compress
resetpts / itp
```

<!-- Lightbox -->
<div class="uk-child-width-1-2@m" uk-grid uk-lightbox="animation: slide">
    <div>
        <a class="uk-inline" href="{{ "/pages/tutorial/stratigraphy/images/06_truncate_set_id_close_up.png" | relative_url }}" data-caption="Boundary fence intersection with hex mesh">
            <img src="{{ "/pages/tutorial/stratigraphy/images/06_truncate_set_id_close_up.png" | relative_url }}" alt="">
        </a>
    </div>
    <div>
        <a class="uk-inline" href="{{ "/pages/tutorial/stratigraphy/images/06_hex_01_truncate_w_grid.png" | relative_url }}" data-caption="Hex mesh truncated with boundary fence">
            <img src="{{ "/pages/tutorial/stratigraphy/images/06_hex_01_truncate_w_grid.png" | relative_url }}" alt="">
        </a>
    </div>
</div>

<!-- Next / Prev -->
<ul class="uk-pagination">
    <li><a href="{{ "/pages/tutorial/stratigraphy/step_05.html" | relative_url }}"><span class="uk-margin-small-right" uk-pagination-previous></span> Previous</a></li>
    <li class="uk-margin-auto-left"><a href="{{ "/pages/tutorial/stratigraphy/step_07.html" | relative_url }}">Next <span class="uk-margin-small-left" uk-pagination-next></span></a></li>
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
            <li class="uk-active"><a href="#truncate-with-polyline">Truncate with Polyline</a></li>
            <li class=""><a href="{{ "/pages/tutorial/stratigraphy/step_07.html" | relative_url }}">Refine Fault</a></li>
            <li class=""><a href="{{ "/pages/tutorial/stratigraphy/step_08.html" | relative_url }}">Insert Well</a></li>
            <li class=""><a href="{{ "/pages/tutorial/stratigraphy/step_09.html" | relative_url }}">Convert Hex Mesh to Tet</a></li>
            <li class="uk-nav-divider"></li>
            <!---->
            <li><a href="https://github.com/lanl/LaGriT/issues" target="_blank"><span uk-icon="icon: warning" class="uk-margin-small-right uk-icon"><svg width="20" height="20" viewBox="0 0 20 20" xmlns="http://www.w3.org/2000/svg" data-svg="warning"><circle cx="10" cy="14" r="1"></circle><circle fill="none" stroke="#000" stroke-width="1.1" cx="10" cy="10" r="9"></circle><path d="M10.97,7.72 C10.85,9.54 10.56,11.29 10.56,11.29 C10.51,11.87 10.27,12 9.99,12 C9.69,12 9.49,11.87 9.43,11.29 C9.43,11.29 9.16,9.54 9.03,7.72 C8.96,6.54 9.03,6 9.03,6 C9.03,5.45 9.46,5.02 9.99,5 C10.53,5.01 10.97,5.44 10.97,6 C10.97,6 11.04,6.54 10.97,7.72 L10.97,7.72 Z"></path></svg></span> <span class="uk-text-middle">Report issue</span></a></li>
            <li><a href="mailto:lagrit-dev@lanl.gov" target="_blank"><span uk-icon="icon: commenting" class="uk-margin-small-right uk-icon"><svg width="20" height="20" viewBox="0 0 20 20" xmlns="http://www.w3.org/2000/svg" data-svg="commenting"><polygon fill="none" stroke="#000" points="1.5,1.5 18.5,1.5 18.5,13.5 10.5,13.5 6.5,17.5 6.5,13.5 1.5,13.5"></polygon><circle cx="10" cy="8" r="1"></circle><circle cx="6" cy="8" r="1"></circle><circle cx="14" cy="8" r="1"></circle></svg></span> <span class="uk-text-middle">Get help</span></a></li>
        </ul>
    </div>
</div>