<div class="uk-position-relative uk-visible-toggle uk-light" tabindex="-1" uk-slideshow>

    <ul class="uk-slideshow-items">
        <li>
            <img data-src="{{ "/pages/tutorial/stratigraphy/images/03_hex_01_set_imt_itetclr.png" | relative_url }}" width="1800" height="1200" alt="" uk-cover uk-img="target: !.uk-slideshow-items">
        </li>
        <li>
            <img data-src="{{ "/pages/tutorial/stratigraphy/images/03_hex_01_set_imt_itetclr_threshold_remove_material3.png" | relative_url }}" width="1800" height="1200" alt="" uk-cover uk-img="target: !.uk-slideshow-items">
        </li>
    </ul>

    <a class="uk-position-center-left uk-position-small uk-hidden-hover" href="#" uk-slidenav-previous uk-slideshow-item="previous"></a>
    <a class="uk-position-center-right uk-position-small uk-hidden-hover" href="#" uk-slidenav-next uk-slideshow-item="next"></a>

</div>

<h2 id="const-stratigraphy" class="uk-h3 uk-margin-remove">3. Constructing Stratigraphy</h2>

In the next step of this tutorial, we will build some surfaces to define
stratigraphy.
In a real model, the surfaces would come from some geologic framework model
and would define geologic or hydro-geologic horizons and topography.

These surfaces will be planar quad meshes that cut through a defined section of
the hex mesh. Later, we will map the intersections of the surfaces to the hex
mesh.

#### Create the top surface:

```
cmo / create / mosurf1
cmo / select / mosurf1

define / X0S /  -20.0
define / X1S / 4020.0
define / Y0S /  -20.0
define / Y1S / 4020.0

define / Z1 / 1000.
define / Z2 / 1500.
define / Z3 / 2500.
define / Z4 /  500.

quadxy / NX NY /X0S Y0S Z1/X1S Y0S Z2/X1S Y1S Z3/X0S Y1S Z4
createpts/brick/xyz/ NX NY 1 /1,0,0/connect
```

Note that the X and Y domains of the quad mesh exceed that of the hex mesh.
This serves two purposes. First, it serves as a helpful visualization aid,
allowing one to easily see how the surfaces cut the hex mesh without adjusting
opacity. Second, and more importantly, it ensures that all elements cut by the
surfaces will be properly recognized as such. Rounding errors may affect
elements at the perimeter of the cutting planes from being properly labeled.

#### Create the bottom surface:

```
cmo / create / mosurf2
cmo / select / mosurf2

define / Z1 / 1800.
define / Z2 / 2100.
define / Z3 / 2800.
define / Z4 /  800.
quadxy / NX NY /X0S Y0S Z1/X1S Y0S Z2/X1S Y1S Z3/X0S Y1S Z4
createpts/brick/xyz/ NX NY 1 /1,0,0/connect
```

<ul class="uk-pagination">
    <li><a href="{{ "/pages/tutorial/stratigraphy/step_02.html" | relative_url }}"><span class="uk-margin-small-right" uk-pagination-previous></span> Previous</a></li>
    <li class="uk-margin-auto-left"><a href="{{ "/pages/tutorial/stratigraphy/step_04.html" | relative_url }}">Next <span class="uk-margin-small-left" uk-pagination-next></span></a></li>
</ul>


<!-- Sidebar -->
<div class="tm-sidebar-right uk-visible@l">
    <div uk-sticky="offset: 160" class="uk-sticky uk-active uk-sticky-fixed" style="position: fixed; top: 160px; width: 200px;">
        <ul uk-scrollspy-nav="closest: li; scroll: true; offset: 100" class="uk-nav uk-nav-default tm-nav uk-nav-parent-icon">
            <li class=""><a href="{{ "/pages/tutorial/stratigraphy/index.html" | relative_url }}">Index</a></li>
            <li class=""><a href="{{ "/pages/tutorial/stratigraphy/step_01.html" | relative_url }}">Building a Hex Mesh</a></li>
            <li class=""><a href="{{ "/pages/tutorial/stratigraphy/step_02.html" | relative_url }}">Define Boundaries Using Point Sets</a></li>
            <li class="uk-active"><a href="#const-stratigraphy">Constructing Stratigraphy</a></li>
            <li class="uk-nav-divider"></li>
            <!---->
            <li><a href="https://github.com/lanl/LaGriT/issues" target="_blank"><span uk-icon="icon: warning" class="uk-margin-small-right uk-icon"><svg width="20" height="20" viewBox="0 0 20 20" xmlns="http://www.w3.org/2000/svg" data-svg="warning"><circle cx="10" cy="14" r="1"></circle><circle fill="none" stroke="#000" stroke-width="1.1" cx="10" cy="10" r="9"></circle><path d="M10.97,7.72 C10.85,9.54 10.56,11.29 10.56,11.29 C10.51,11.87 10.27,12 9.99,12 C9.69,12 9.49,11.87 9.43,11.29 C9.43,11.29 9.16,9.54 9.03,7.72 C8.96,6.54 9.03,6 9.03,6 C9.03,5.45 9.46,5.02 9.99,5 C10.53,5.01 10.97,5.44 10.97,6 C10.97,6 11.04,6.54 10.97,7.72 L10.97,7.72 Z"></path></svg></span> <span class="uk-text-middle">Report issue</span></a></li>
            <li><a href="mailto:lagrit-dev@lanl.gov" target="_blank"><span uk-icon="icon: commenting" class="uk-margin-small-right uk-icon"><svg width="20" height="20" viewBox="0 0 20 20" xmlns="http://www.w3.org/2000/svg" data-svg="commenting"><polygon fill="none" stroke="#000" points="1.5,1.5 18.5,1.5 18.5,13.5 10.5,13.5 6.5,17.5 6.5,13.5 1.5,13.5"></polygon><circle cx="10" cy="8" r="1"></circle><circle cx="6" cy="8" r="1"></circle><circle cx="14" cy="8" r="1"></circle></svg></span> <span class="uk-text-middle">Get help</span></a></li>
        </ul>
    </div>
</div>