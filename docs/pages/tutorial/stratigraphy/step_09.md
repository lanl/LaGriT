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
            <img data-src="{{ "/pages/tutorial/stratigraphy/images/19_hex_01_to_tet.png" | relative_url }}" width="1800" height="1200" alt="" uk-cover uk-img="target: !.uk-slideshow-items">
        </li>
    </ul>
</div>
<!-- End image slideshow -->
<br/>

<h2 id="convert-hex-mesh-to-tet" class="uk-h3 uk-margin-remove">9. Convert Hex Mesh to Tet</h2>

In this final step, we will convert our mesh from hexahedral to tetrahedral
elements.

Create an empty mesh object, `motet`, and copy all nodes from `MONAME`
(or, `mohex_octree`) to `motet`:

```
cmo / create / motet

copypts / motet / mohex_octree
```

Next, reset the `imt` and `itp` variables and connect the nodes into
tetrahedral elements:

```
cmo / setatt / motet / imt / 1 0 0 / 1
cmo / setatt / motet / itp / 1 0 0 / 0
connect
resetpts / itp
```

Interpolate the node and cell 'colors' over the tetrahedral mesh, using
`interpolate / voronoi` for node-to-node interpolations, and 
`interpolate / map` for cell-to-cell interpolations:

```
interpolate / voronoi / motet / imt / 1 0 0 / mohex_octree / imt
interpolate / map / motet / itetclr / 1 0 0 / mohex_octree / itetclr
```

Recall in step 5 that we set `imt` and `itetclr` to the value 7 for all
nodes and elements that weren't captured by the surface-created element sets. 
We can use the command `rmmat / 7` to remove all nodes and elements with
`imt` and `itetclr` values of 7:

```
rmmat / 7
rmpoint / compress
resetpts / itp
```

Check the mesh that it has all positive element volumes and no bad aspect ratios.
Use the `quality` command for a report on these mesh quantities.

```
quality

epsilonl, epsilonaspect:   1.3230281E-09  2.3158330E-27                         
--------------------------------------------                                    
elements with aspect ratio < .01:                    0                          
elements with aspect ratio b/w .01 and .02:          0                          
elements with aspect ratio b/w .02 and .05:          0                          
elements with aspect ratio b/w .05 and .1 :          0                          
elements with aspect ratio b/w .1  and .2 :         18                          
elements with aspect ratio b/w .2  and .5 :      20538                          
elements with aspect ratio b/w .5  and 1. :     631394                          
min aspect ratio =  0.1613E+00  max aspect ratio =  0.9377E+00                  
 
epsilonvol:   8.8249408E-03                                                     
---------------------------------------                                         
element volumes b/w  0.2500E+03 and  0.8706E+03:     43109                      
element volumes b/w  0.8706E+03 and  0.3031E+04:    273639                      
element volumes b/w  0.3031E+04 and  0.1056E+05:     98379                      
element volumes b/w  0.1056E+05 and  0.3676E+05:     63075                      
element volumes b/w  0.3676E+05 and  0.1280E+06:    173748                      
min volume =   2.5000000E+02  max volume =   1.2800000E+05                      
-----------------------------------------------------------                     
    651950 total elements evaluated.                                        
```

Finally, write the mesh object to AVS UCD file format and signal the EOF
`finish` command:

```
dump/avs/tet_mesh.inp/motet                                                     

finish
```

<!-- Next / Prev -->
<ul class="uk-pagination">
    <li><a href="{{ "/pages/tutorial/stratigraphy/step_08.html" | relative_url }}"><span class="uk-margin-small-right" uk-pagination-previous></span> Previous</a></li>
    <li class="uk-margin-auto-left"><a href="{{ "/pages/tutorial/stratigraphy/step_10_fehm.html" | relative_url }}">Next <span class="uk-margin-small-left" uk-pagination-next></span></a></li>
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
            <li class="uk-active"><a href="#convert-hex-mesh-to-tet">9. Convert Hex Mesh to Tet</a></li>
            <li class=""><a href="{{ "/pages/tutorial/stratigraphy/step_10_fehm.html" | relative_url }}">10.1 Write FEHM Files</a></li>
            <li class=""><a href="{{ "/pages/tutorial/stratigraphy/step_10_exo.html" | relative_url }}">10.2 Write ExodusII Files</a></li>
            <li class="uk-nav-divider"></li>
            <!---->
            <li><a href="{{ "/pages/tutorial/stratigraphy/images/gallery.html" | relative_url }}" target="_blank"><span uk-icon="icon: image" class="uk-margin-small-right uk-icon"></span> <span class="uk-text-middle">Image Gallery</span></a></li>
            <li><a href="https://github.com/lanl/LaGriT/issues" target="_blank"><span uk-icon="icon: warning" class="uk-margin-small-right uk-icon"><svg width="20" height="20" viewBox="0 0 20 20" xmlns="http://www.w3.org/2000/svg" data-svg="warning"><circle cx="10" cy="14" r="1"></circle><circle fill="none" stroke="#000" stroke-width="1.1" cx="10" cy="10" r="9"></circle><path d="M10.97,7.72 C10.85,9.54 10.56,11.29 10.56,11.29 C10.51,11.87 10.27,12 9.99,12 C9.69,12 9.49,11.87 9.43,11.29 C9.43,11.29 9.16,9.54 9.03,7.72 C8.96,6.54 9.03,6 9.03,6 C9.03,5.45 9.46,5.02 9.99,5 C10.53,5.01 10.97,5.44 10.97,6 C10.97,6 11.04,6.54 10.97,7.72 L10.97,7.72 Z"></path></svg></span> <span class="uk-text-middle">Report issue</span></a></li>
            <li><a href="mailto:lagrit-dev@lanl.gov" target="_blank"><span uk-icon="icon: commenting" class="uk-margin-small-right uk-icon"><svg width="20" height="20" viewBox="0 0 20 20" xmlns="http://www.w3.org/2000/svg" data-svg="commenting"><polygon fill="none" stroke="#000" points="1.5,1.5 18.5,1.5 18.5,13.5 10.5,13.5 6.5,17.5 6.5,13.5 1.5,13.5"></polygon><circle cx="10" cy="8" r="1"></circle><circle cx="6" cy="8" r="1"></circle><circle cx="14" cy="8" r="1"></circle></svg></span> <span class="uk-text-middle">Get help</span></a></li>
        </ul>
    </div>
</div>