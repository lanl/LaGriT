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
            <img data-src="{{ "/pages/tutorial/stratigraphy/images/08_hex_01_insert_wells.png" | relative_url }}" width="1800" height="1200" alt="" uk-cover uk-img="target: !.uk-slideshow-items">
        </li>
        <li>
            <img data-src="{{ "/pages/tutorial/stratigraphy/images/08_hex_01_insert_wells_close_up.png" | relative_url }}" width="1800" height="1200" alt="" uk-cover uk-img="target: !.uk-slideshow-items">
        </li>
        <li>
            <img data-src="{{ "/pages/tutorial/stratigraphy/images/08_hex_01_insert_wells_dfield_wells.png" | relative_url }}" width="1800" height="1200" alt="" uk-cover uk-img="target: !.uk-slideshow-items">
        </li>
    </ul>
    <a class="uk-position-center-left uk-position-small uk-hidden-hover" href="#" uk-slidenav-previous uk-slideshow-item="previous"></a>
    <a class="uk-position-center-right uk-position-small uk-hidden-hover" href="#" uk-slidenav-next uk-slideshow-item="next"></a>
</div>
<!-- End image slideshow -->
<br/>

<h2 id="insert-well" class="uk-h3 uk-margin-remove">8. Insert Wells</h2>

In this step, we will generate two cylindrical 'wells', refine the mesh
`MONAME` around them, and identify a line of nodes that will be the well
source/sink for boundary conditions (ultimately writing these nodes
to `zone` files).

### 8.1 Generating Cylindrical Tetrahedral Wells

First, we define variables for the well's position (`XWELL`,`YWELL`), radius (`RADIUS_WELL`), and number of nodes across the cylindrical radius
(`NRADIUS`):

```
define / XWELL1 / 1234.56
define / YWELL1 / 1987.65

define / XWELL2 / 2243.21
define / YWELL2 / 1212.34

define / RADIUS_WELL / 25.0
define / NRADIUS / 2
```

Now, we create a cylindrical point cloud defining the first well using
`createpts / rtz`:

```
cmo / create / mo_well1 / / / tet
createpts / rtz / NRADIUS 9 NZ / 0. 0. 3100. / RADIUS_WELL 360. 1500. / 1 1 1
```

This creates a point cloud centered around (0,0,0) with a radius of `RADIUS_WELL`, an angular component spanning a full 360 degrees (`φ = {0., 360.}`), and a Z range of `{3100.,1500.}`.

Run `filter`, `rmpoint / compress`, and set `imt` to 1 for the well:

```
filter / 1 0 0
rmpoint / compress
cmo / setatt / mo_well1 / imt / 1 0 0 / 1
```

Next, connect the point cloud into a tetrahedral mesh and translate the
X and Y origin to `{XWELL1,YWELL1}`:

```
connect
resetpts / itp
cmo / printatt / mo_well1 / -xyz- / minmax
trans / 1 0 0 / 0. 0. 0. / XWELL1 YWELL1 0.0
cmo / printatt / mo_well1 / -xyz- / minmax
```

The first 'well' mesh object has been generated. Repeat this process with different parameters to create the second well:

```
cmo / create / mo_well2 / / / tet
createpts / rtz / NRADIUS 9 NZ / 0. 0. 3100. / RADIUS_WELL 360. 2200. / 1 1 1
filter / 1 0 0
rmpoint / compress
cmo / setatt / mo_well1 / imt / 1 0 0 / 1
connect
resetpts / itp
cmo / printatt / mo_well2 / -xyz- / minmax
trans / 1 0 0 / 0. 0. 0. / XWELL2 YWELL2 0.0
cmo / printatt / mo_well2 / -xyz- / minmax
```

Finally, join the two distinct wells into a single mesh object with `addmesh / merge`:

```
addmesh / merge / mo_wells / mo_well1 mo_well2
```

### 8.2 Refining `MONAME` around the wells

As we did for the fault in step 7, we refine the main mesh `MONAME` around the wells:

```
# First pass refinement
cmo / select / MONAME
intersect_elements / MONAME / mo_wells / if_inter
eltset / e_refine / if_inter / gt / 0
refine/ eltset / eltset get e_refine
cmo / setatt / MONAME / if_inter / 1 0 0 / 0
eltset / e_refine / delete

# Second pass refinement
cmo / select / MONAME
intersect_elements / MONAME / mo_wells / if_inter
eltset / e_refine / if_inter / gt / 0
refine/ eltset / eltset get e_refine
cmo / setatt / MONAME / if_inter / 1 0 0 / 0
eltset / e_refine / delete

# Third pass refinement
cmo / select / MONAME
intersect_elements / MONAME / mo_wells / if_inter
eltset / e_refine / if_inter / gt / 0
refine/ eltset / eltset get e_refine
cmo / setatt / MONAME / if_inter / 1 0 0 / 0
eltset / e_refine / delete
```

The refinement process returns a octree grid object, which stores information about parent-children relationships, among other properties. It's important, as the prepare to finalize the mesh for exporting, to strip this information and convert the octree grid object to a standard mesh object.

This conversion is done through the `grid2grid / tree_to_fe` command:

```
grid2grid / tree_to_fe / mohex_octree /  mohex
define / MONAME / mohex_octree
```

<!-- Lightbox -->
<div class="uk-child-width-1-2@m" uk-grid uk-lightbox="animation: slide">
    <div>
        <a class="uk-inline" href="{{ "/pages/tutorial/stratigraphy/images/08_hex_01_insert_wells_dfield_wells.png" | relative_url }}" data-caption="Node Euclidean distances to wells">
            <img src="{{ "/pages/tutorial/stratigraphy/images/08_hex_01_insert_wells_dfield_wells.png" | relative_url }}" alt="">
        </a>
    </div>
</div>

### 8.3 Writing `zone` files based on well distances

The `zone` files are lists of node numbers in [FEHM](https://fehm.lanl.gov) file format and used to identify materials, well source/sinks, and boundary conditions.

In this subsection, we will generate `zone` files describing all nodes within 32, 16, 8, 4, 2 and 1 meters of the wells.

To begin, we will compute the well point cloud again, as we did above. First, for well 1:

```
cmo / create / mo_pts1
createpts / rtz / 2 2 1000 / 0. 0. 3100. / 0.0 360. 2200. / 1 1 1
trans / 1 0 0 / 0. 0. 0. / XWELL1 YWELL1 0.0
```

Then for well 2:

```
cmo / create / mo_pts2
createpts / rtz / 2 2 1000 / 0. 0. 3100. / 0.0 360. 2200. / 1 1 1
trans / 1 0 0 / 0. 0. 0. / XWELL2 YWELL2 0.0
```

and joining them into a single mesh object, `mo_pts`:

```
addmesh / merge / mo_pts / mo_pts1 / mo_pts2
cmo / select / mo_pts
filter / 1 0 0
rmpoint / compress
```

Next, we will compute a distance field attribute, `dfield_well`, which is a node-based attribute storing the Euclidean distance from `node_i` in one mesh to the closest node in another mesh. In other words, all nodes in `MONAME` store their distance to the closest well (`mo_pts`) node.

```
compute / distance_field / MONAME / mo_pts / dfield_well
```

Clean up unneeded mesh objects:

```
cmo / delete / mo_pts1
cmo / delete / mo_pts2
cmo / delete / mo_pts
cmo / delete / mo_wells
cmo / delete / mo_well1
cmo / delete / mo_well2
```

And finally, for each mesh-to-well distance in {32,16,8,4,2,1} (which is stored in `dfield_well`), (i) create a pset object
containing all nodes within that distance, and (ii) write those nodes to a `zone` file:

```
cmo / select / MONAME

pset / pwell / attribute / dfield_well / 1 0 0 / le / 1.0
pset / pwell / zone / zone_radius_01.0.zone

pset / pwell / attribute / dfield_well / 1 0 0 / le / 2.0
pset / pwell / zone / zone_radius_02.0.zone

pset / pwell / attribute / dfield_well / 1 0 0 / le / 4.0
pset / pwell / zone / zone_radius_04.0.zone

pset / pwell / attribute / dfield_well / 1 0 0 / le / 8.0
pset / pwell / zone / zone_radius_08.0.zone

pset / pwell / attribute / dfield_well / 1 0 0 / le / 16.0
pset / pwell / zone / zone_radius_16.0.zone

pset / pwell / attribute / dfield_well / 1 0 0 / le / 32.0
pset / pwell / zone / zone_radius_32.0.zone
```

<!-- Next / Prev -->
<ul class="uk-pagination">
    <li><a href="{{ "/pages/tutorial/stratigraphy/step_07.html" | relative_url }}"><span class="uk-margin-small-right" uk-pagination-previous></span> Previous</a></li>
    <li class="uk-margin-auto-left"><a href="{{ "/pages/tutorial/stratigraphy/step_09.html" | relative_url }}">Next <span class="uk-margin-small-left" uk-pagination-next></span></a></li>
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
            <li class="uk-active"><a href="#insert-well">8. Insert Wells</a></li>
            <li class=""><a href="{{ "/pages/tutorial/stratigraphy/step_09.html" | relative_url }}">9. Convert Hex Mesh to Tet</a></li>
            <li class="uk-nav-divider"></li>
            <!---->
            <li><a href="https://github.com/lanl/LaGriT/issues" target="_blank"><span uk-icon="icon: warning" class="uk-margin-small-right uk-icon"><svg width="20" height="20" viewBox="0 0 20 20" xmlns="http://www.w3.org/2000/svg" data-svg="warning"><circle cx="10" cy="14" r="1"></circle><circle fill="none" stroke="#000" stroke-width="1.1" cx="10" cy="10" r="9"></circle><path d="M10.97,7.72 C10.85,9.54 10.56,11.29 10.56,11.29 C10.51,11.87 10.27,12 9.99,12 C9.69,12 9.49,11.87 9.43,11.29 C9.43,11.29 9.16,9.54 9.03,7.72 C8.96,6.54 9.03,6 9.03,6 C9.03,5.45 9.46,5.02 9.99,5 C10.53,5.01 10.97,5.44 10.97,6 C10.97,6 11.04,6.54 10.97,7.72 L10.97,7.72 Z"></path></svg></span> <span class="uk-text-middle">Report issue</span></a></li>
            <li><a href="mailto:lagrit-dev@lanl.gov" target="_blank"><span uk-icon="icon: commenting" class="uk-margin-small-right uk-icon"><svg width="20" height="20" viewBox="0 0 20 20" xmlns="http://www.w3.org/2000/svg" data-svg="commenting"><polygon fill="none" stroke="#000" points="1.5,1.5 18.5,1.5 18.5,13.5 10.5,13.5 6.5,17.5 6.5,13.5 1.5,13.5"></polygon><circle cx="10" cy="8" r="1"></circle><circle cx="6" cy="8" r="1"></circle><circle cx="14" cy="8" r="1"></circle></svg></span> <span class="uk-text-middle">Get help</span></a></li>
        </ul>
    </div>
</div>