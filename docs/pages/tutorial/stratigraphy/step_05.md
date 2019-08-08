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
            <img data-src="{{ "/pages/tutorial/stratigraphy/images/05_hex_01_fault_imt_itetclr.png" | relative_url }}" width="1800" height="1200" alt="" uk-cover uk-img="target: !.uk-slideshow-items">
        </li>
    </ul>
</div>
<!-- End image slideshow -->
<br/>

<h2 id="constructing-a-fault" class="uk-h3 uk-margin-remove">5. Constructing a Fault and Layers</h2>

### 5.1 Creating a Fault and Subsurface Layers

Next, we are going to map a fault and surfaces to our mesh. The objects created
will be:


<!-- Lightbox image -->
<div class="uk-child-width-1-2@m" uk-grid uk-lightbox="animation: slide">
    <div>
        <a class="uk-inline" href="{{ "/pages/tutorial/stratigraphy/images/05_fault_objects.png" | relative_url }}" data-caption="Fault and layer surfaces">
            <img src="{{ "/pages/tutorial/stratigraphy/images/05_fault_objects.png" | relative_url }}" alt="">
        </a>
    </div>
</div>


For all five of these surfaces, we will:

1. Define the X,Y,Z extent
2. Use `quadxy` to generate the point distribution
3. Connect the points into a quad mesh using `createpts/brick`.

For the main fault mesh, this process looks like:

```
cmo / create / mosurf_fault
cmo / select / mosurf_fault

define / X0S /  -20.0
define / X1S / 4020.0

define / Y0S /  -20.0
define / Y1S / 4020.0

define / Z1 / -1.e4
define / Z2 / -1.e4
define / Z3 /  1.e4
define / Z4 /  1.e4

quadxy / NX NY /X0S Y0S Z1/X1S Y0S Z2/X1S Y1S Z3/X0S Y1S Z4
createpts/brick/xyz/ NX NY 1 /1,0,0/connect
cmo / printatt / mosurf / -xyz- / minmax
```

For the remaining four surfaces this process is repeated, with `Z1,Z2,Z3,Z4`
altered independently.

The created surfaces have the names `mosurf1_fminus`, `mosurf2_fminus`,
`mosurf1_fplus`, `mosurf2_fplus`, and `mosurf_fault`.

### 5.2 Define Geometry of Hydrostratigraphic Model

Recall in step 4 how we used two surface meshes to alter `imt` and `itetclr`
values: first, by defining `surfaces` from the planar meshes; second, using the
`surface` objects to define `region` objects; third, creating `psets` and
`eltsets` from the `regions`; and finally, by modifying `itetclr` and `imt`
through the defined `psets` and `eltsets`.

This process is replicated here. First, by creating the `surfaces`:

```
surface / s_1_fm / reflect / sheet / mosurf1_fminus
surface / s_2_fm / reflect / sheet / mosurf2_fminus
surface / s_1_fp / reflect / sheet / mosurf1_fplus
surface / s_2_fp / reflect / sheet / mosurf2_fplus
surface / s_f    / reflect / sheet / mosurf_fault
```

Then, by mapping the surfaces to regions (and deleting the planar meshes to
free up memory):

```
region / r_1_fm / le s_1_fm and               le s_f
region / r_2_fm / gt s_1_fm and le s_2_fm and le s_f
region / r_3_fm / gt s_2_fm and               le s_f
region / r_1_fp / le s_1_fp and               gt s_f
region / r_2_fp / gt s_1_fp and le s_2_fp and gt s_f
region / r_3_fp / gt s_2_fp and               gt s_f

cmo / delete / mosurf1_fminus
cmo / delete / mosurf2_fminus
cmo / delete / mosurf1_fplus
cmo / delete / mosurf2_fplus
cmo / delete / mosurf_fault
```

And finally, by creating `psets` and `eltsets` and using them to modify material
attributes of the parent mesh:

```
pset   / p_r_1_fm / region / r_1_fm / 1 0 0
pset   / p_r_2_fm / region / r_2_fm / 1 0 0
pset   / p_r_3_fm / region / r_3_fm / 1 0 0
pset   / p_r_1_fp / region / r_1_fp / 1 0 0
pset   / p_r_2_fp / region / r_2_fp / 1 0 0
pset   / p_r_3_fp / region / r_3_fp / 1 0 0

eltset / e_r_1_fm / region / r_1_fm
eltset / e_r_2_fm / region / r_2_fm
eltset / e_r_3_fm / region / r_3_fm
eltset / e_r_1_fp / region / r_1_fp
eltset / e_r_2_fp / region / r_2_fp
eltset / e_r_3_fp / region / r_3_fp

cmo / setatt / MONAME / imt / 1 0 0 / 7
cmo / setatt / MONAME / itetclr / 1 0 0 / 7

cmo / setatt / MONAME / imt     / pset   get p_r_1_fm / 1
cmo / setatt / MONAME / imt     / pset   get p_r_2_fm / 2
cmo / setatt / MONAME / imt     / pset   get p_r_3_fm / 3
cmo / setatt / MONAME / imt     / pset   get p_r_1_fp / 4
cmo / setatt / MONAME / imt     / pset   get p_r_2_fp / 5
cmo / setatt / MONAME / imt     / pset   get p_r_3_fp / 6

cmo / setatt / MONAME / itetclr / eltset get e_r_1_fm / 1
cmo / setatt / MONAME / itetclr / eltset get e_r_2_fm / 2
cmo / setatt / MONAME / itetclr / eltset get e_r_3_fm / 3
cmo / setatt / MONAME / itetclr / eltset get e_r_1_fp / 4
cmo / setatt / MONAME / itetclr / eltset get e_r_2_fp / 5
cmo / setatt / MONAME / itetclr / eltset get e_r_3_fp / 6
```

The six distinct regions can now be seen by viewing `itetclr` on the
parent mesh:


<!-- Lightbox image -->
<div class="uk-child-width-1-2@m" uk-grid uk-lightbox="animation: slide">
    <div>
        <a class="uk-inline" href="{{ "/pages/tutorial/stratigraphy/images/05_hex_01_fault_imt_itetclr.png" | relative_url }}" data-caption="Visualizing the fault in relation to subsurface layers">
            <img src="{{ "/pages/tutorial/stratigraphy/images/05_hex_01_fault_imt_itetclr.png" | relative_url }}" alt="">
        </a>
    </div>
</div>


<!-- Next / Prev -->
<ul class="uk-pagination">
    <li><a href="{{ "/pages/tutorial/stratigraphy/step_04.html" | relative_url }}"><span class="uk-margin-small-right" uk-pagination-previous></span> Previous</a></li>
    <li class="uk-margin-auto-left"><a href="{{ "/pages/tutorial/stratigraphy/step_06.html" | relative_url }}">Next <span class="uk-margin-small-left" uk-pagination-next></span></a></li>
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
            <li class="uk-active"><a href="#constructing-a-fault">5. Constructing a Fault</a></li>
            <li class=""><a href="{{ "/pages/tutorial/stratigraphy/step_06.html" | relative_url }}">6. Truncate with Polyline</a></li>
            <li class=""><a href="{{ "/pages/tutorial/stratigraphy/step_07.html" | relative_url }}">7. Refine Fault</a></li>
            <li class=""><a href="{{ "/pages/tutorial/stratigraphy/step_08.html" | relative_url }}">8. Insert Wells</a></li>
            <li class=""><a href="{{ "/pages/tutorial/stratigraphy/step_09.html" | relative_url }}">9. Convert Hex Mesh to Tet</a></li>
            <li class="uk-nav-divider"></li>
            <!---->
            <li><a href="https://github.com/lanl/LaGriT/issues" target="_blank"><span uk-icon="icon: warning" class="uk-margin-small-right uk-icon"><svg width="20" height="20" viewBox="0 0 20 20" xmlns="http://www.w3.org/2000/svg" data-svg="warning"><circle cx="10" cy="14" r="1"></circle><circle fill="none" stroke="#000" stroke-width="1.1" cx="10" cy="10" r="9"></circle><path d="M10.97,7.72 C10.85,9.54 10.56,11.29 10.56,11.29 C10.51,11.87 10.27,12 9.99,12 C9.69,12 9.49,11.87 9.43,11.29 C9.43,11.29 9.16,9.54 9.03,7.72 C8.96,6.54 9.03,6 9.03,6 C9.03,5.45 9.46,5.02 9.99,5 C10.53,5.01 10.97,5.44 10.97,6 C10.97,6 11.04,6.54 10.97,7.72 L10.97,7.72 Z"></path></svg></span> <span class="uk-text-middle">Report issue</span></a></li>
            <li><a href="mailto:lagrit-dev@lanl.gov" target="_blank"><span uk-icon="icon: commenting" class="uk-margin-small-right uk-icon"><svg width="20" height="20" viewBox="0 0 20 20" xmlns="http://www.w3.org/2000/svg" data-svg="commenting"><polygon fill="none" stroke="#000" points="1.5,1.5 18.5,1.5 18.5,13.5 10.5,13.5 6.5,17.5 6.5,13.5 1.5,13.5"></polygon><circle cx="10" cy="8" r="1"></circle><circle cx="6" cy="8" r="1"></circle><circle cx="14" cy="8" r="1"></circle></svg></span> <span class="uk-text-middle">Get help</span></a></li>
        </ul>
    </div>
</div>