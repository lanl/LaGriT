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
            <img data-src="{{ "/pages/tutorial/stratigraphy/images/02_hex_01_top_region.png" | relative_url }}" width="1800" height="1200" alt="" uk-cover uk-img="target: !.uk-slideshow-items">
        </li>
    </ul>
</div>
<!-- End image slideshow -->
<br/>

<h2 id="psets" class="uk-h3 uk-margin-remove">2. Define Boundaries Using Point Sets</h2>

In LaGriT, a pset (or *point-set*) is a collection of points (nodes) within a 
mesh object. Similarly, an eltset (*element-set*) is a collection of mesh
elements. By capturing points and elements, discrete manipulations can be
performed on them, such as translation, removal, or attribute functions. 

In this example, point sets are used to create a boundary on the top surface of 
the created hex mesh. The boundary will be the intersection of three circles on
the top layer of the mesh.

```
# Set vertices (imt) and cells (itetlcr) to 1
cmo / setatt / mohex / imt / 1 0 0 / 1
cmo / setatt / mohex / itetclr / 1 0 0 / 1

resetpts / itp
```

Capturing a `pset` is done through the command

```
pset / pset_name / select_type / select_type_options
```

where `pset_name` is an arbitrary variable name to store the pset into, 
`select_type` is the method of pset selection, and `select_type_options` are
parameters specific to the chosen `select_type` for configuring the subset
selection (see the [documentation](../../docs/commands/PSET.md) for more information).

### 2.1 PSet Definitions

As the boundary will live only in the top layer, nodes belonging to the top 
layer will be identified first.

```
pset / p_top / attribute / zic / 1 0 0 / ge / Z1
```

Here, a pset named `p_top` is created. This pset contains all nodes 
(stride = `1 0 0`) where the node's Z value (`zic`) is greater than or equal to
(`ge`) the top of the mesh (`Z1`). Remember that we defined `Z1` above for the
initial creation of the mesh - and that by simply changing `Z1` and re-running
the script, this pset capture will still be valid for any value of `Z1`
(where `Z1 > Z0`).

Now that the top is defined, we will move to defining three cylindrical
objects.

This is done through the command

```
pset / pset_name / geom / rtz / ifirst,ilast,istride / r1,t1,z1 / r2,t2,z2 / xcen,ycen,zcen
```

which forms a pset of nodes within the cylinder or cylindrical shell given by 
radius r1 to r2, angle theta t1 to t2 and height z1 to z2.

```
pset / p_circle1 / geom / rtz / 1 0 0 / 0. 0. -1.0 / &
       1100. 360. 1.e4 / 1500. 1500. 0.
pset / p_circle2 / geom / rtz / 1 0 0 / 0. 0. -1.0 / &
       1100. 360. 1.e4 / 2500. 2500. 0.
pset / p_circle3 / geom / rtz / 1 0 0 / 0. 0. -1.0 / &
       1100. 360. 1.e4 / 2500. 1500. 0.
```

Above, any points within a full circle (theta = `360`) of radius `1100.` and 
within `Z={-1.0,1.e4}` are captured. Three different cylinders are created,
where only the centroids change.

Finally, all four psets are intersected such that all points belonging to the
union of all given sets are preserved into the point seet `p_region`:

```
pset / p_region / inter / p_top p_circle1 p_circle2 p_circle3
```

This creates a point-set where all points (i) live on the top layer, and (ii)
live within the intersection of the three cylinders.

### 2.2 Map PSets to an Attribute

As a simple sanity check during meshing, it can be helpful to map mesh
operations to an attribute. These can be visualized at intermediate steps in
the meshing process to provide a form of verification or debugging.

First, create an attribute using the `cmo / addatt` command:

```
cmo / addatt / MONAME / id_top_region / vint / scalar / nnodes
```

Here, an attribute named `id_top_region` is created within the mesh `MONAME`,
and has type integer (`vint`), is of scalar dimensions, and is a node-based
attribute (`nnodes`).

The attribute can be progressively filled in with different values based on
the psets:

```
# Fill the entire attribute with 1
cmo / setatt / MONAME / id_top_region / 1 0 0 / 1

# Color all nodes in the pset p_circle1 with the value 2
cmo / setatt / MONAME / id_top_region / pset get p_circle1 / 2

# Color all nodes in the pset p_circle2 with the value 2
cmo / setatt / MONAME / id_top_region / pset get p_circle2 / 3

# And so on...
cmo / setatt / MONAME / id_top_region / pset get p_circle3 / 4
cmo / setatt / MONAME / id_top_region / pset get p_region / 5
```

Finally, release the psets from memory:

```
pset / p_top / release
pset / p_circle1 / release
pset / p_circle2 / release
pset / p_circle3 / release
pset / p_region / release
```

Cutting through the mesh in ParaView, we can visualize `id_top_region` and
validate that the mesh is being constructed as expected:

<!-- Lightbox image -->
<div class="uk-child-width-1-3@m" uk-grid uk-lightbox="animation: slide">
    <div>
        <a class="uk-inline" href="{{ "/pages/tutorial/stratigraphy/images/ch2_transparent_cbar.png" | relative_url }}" data-caption="Visualization of id_top_region">
            <img src="{{ "/pages/tutorial/stratigraphy/images/ch2_transparent_cbar.png" | relative_url }}" alt="">
        </a>
    </div>
</div>

<!-- Next / Prev -->
<ul class="uk-pagination">
    <li><a href="{{ "/pages/tutorial/stratigraphy/step_01.html" | relative_url }}"><span class="uk-margin-small-right" uk-pagination-previous></span> Previous</a></li>
    <li class="uk-margin-auto-left"><a href="{{ "/pages/tutorial/stratigraphy/step_03.html" | relative_url }}">Next <span class="uk-margin-small-left" uk-pagination-next></span></a></li>
</ul>

<!-- Sidebar -->
<div class="tm-sidebar-right uk-visible@l">
    <div uk-sticky="offset: 160" class="uk-sticky uk-active uk-sticky-fixed" style="position: fixed; top: 160px; width: 200px;">
        <ul uk-scrollspy-nav="closest: li; scroll: true; offset: 100" class="uk-nav uk-nav-default tm-nav uk-nav-parent-icon">
            <li class=""><a href="{{ "/pages/tutorial/stratigraphy/index.html" | relative_url }}">Index</a></li>
            <li class=""><a href="{{ "/pages/tutorial/stratigraphy/step_01.html" | relative_url }}">Building a Hex Mesh</a></li>
            <li class="uk-active"><a href="#psets">Define Boundaries Using Point Sets</a></li>
            <li class=""><a href="{{ "/pages/tutorial/stratigraphy/step_03.html" | relative_url }}">Constructing Stratigraphy</a></li>
            <li class=""><a href="{{ "/pages/tutorial/stratigraphy/step_04.html" | relative_url }}">Map Surfaces to Mesh</a></li>
            <li class=""><a href="{{ "/pages/tutorial/stratigraphy/step_05.html" | relative_url }}">Constructing a Fault</a></li>
            <li class=""><a href="{{ "/pages/tutorial/stratigraphy/step_06.html" | relative_url }}">Truncate with Polyline</a></li>
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