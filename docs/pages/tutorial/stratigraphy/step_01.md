<div class="uk-position-relative uk-visible-toggle uk-light" tabindex="-1" uk-slideshow>

    <ul class="uk-slideshow-items">
        <li>
            <img data-src="{{ "/pages/tutorial/stratigraphy/images/ch1.png" | relative_url }}" width="1800" height="1200" alt="" uk-cover uk-img="target: !.uk-slideshow-items">
        </li>
    </ul>

</div>

<h2 id="build-hex" class="uk-h3 uk-margin-remove">1. Building a Hex Mesh</h2>

First, we are going to construct a structured hex mesh. The hex mesh will span
from 0 meters to 4000 m, 4000 m, and 3000 m, for the x/y/z coordinates
respectively.

For both consistency and rapid parameter manipulation, these can be stored in
variables. In LaGriT, variables are assigned using the `define` keyword. 

```
define / X0 / 0.
define / X1 / 4000.
define / Y0 / 0.
define / Y1 / 4000.
define / Z0 / 0.
define / Z1 / 3000.

define / NX / 51
define / NY / 51
define / NZ / 26

define / MONAME / mohex
```

Above, the spatial domain (`X0,X1,Y0,...`), element density (`NX/NY/NZ`), and
mesh name (`MONAME`) have been defined.

Next, we will create an empty mesh object, with element type `hex`, using the
[`cmo / create`](null.md) command:

```
cmo / create / MONAME / / / hex
```

Due to the variable assignment of `MONAME <- mohex` above, this command is
translated internally as:

```
cmo / create / mohex / / / hex
```

This empty object can then be populated with nodes and elements. 
The [`createpts / brick`](null.md) command will generate a defined number of
hex elements across a defined domain. 

```
createpts / brick / xyz / NX NY NZ / X0 Y0 Z0 / X1 Y1 Z1 / 1 1 1
```

`NX` number of hex elements, along with their corresponding vertices, have been
created in the spatial domain spanning `X0->X1`, along with `NY` elements in
the Y domain and `NZ` elements in the Z domain.

Optionally, [save the mesh object](null.md):

```
dump / avs / tmp_hex_01.inp / MONAME
```

This file can be rendered in certain scientific 3D visualization applications,
such as [ParaView](https://www.paraview.org).

<!-- Page left / right -->
<ul class="uk-pagination">
    <li><a href="{{ "/pages/tutorial/stratigraphy/index.html" | relative_url }}"><span class="uk-margin-small-right" uk-pagination-previous></span> Previous</a></li>
    <li class="uk-margin-auto-left"><a href="{{ "/pages/tutorial/stratigraphy/step_02.html" | relative_url }}">Next <span class="uk-margin-small-left" uk-pagination-next></span></a></li>
</ul>


<!-- Sidebar -->
<div class="tm-sidebar-right uk-visible@l">
    <div uk-sticky="offset: 160" class="uk-sticky uk-active uk-sticky-fixed" style="position: fixed; top: 160px; width: 200px;">
        <ul uk-scrollspy-nav="closest: li; scroll: true; offset: 100" class="uk-nav uk-nav-default tm-nav uk-nav-parent-icon">
            <li class=""><a href="{{ "/pages/tutorial/stratigraphy/index.html" | relative_url }}">Index</a></li>
            <li class="uk-active"><a href="#build-hex">Building a Hex Mesh</a></li>
            <li class=""><a href="{{ "/pages/tutorial/stratigraphy/step_02.html" | relative_url }}">Define Boundaries Using Point Sets</a></li>
            <li class=""><a href="{{ "/pages/tutorial/stratigraphy/step_03.html" | relative_url }}">Constructing Stratigraphy</a></li>
            <li class="uk-nav-divider"></li>
            <!---->
            <li><a href="https://github.com/lanl/LaGriT/issues" target="_blank"><span uk-icon="icon: warning" class="uk-margin-small-right uk-icon"><svg width="20" height="20" viewBox="0 0 20 20" xmlns="http://www.w3.org/2000/svg" data-svg="warning"><circle cx="10" cy="14" r="1"></circle><circle fill="none" stroke="#000" stroke-width="1.1" cx="10" cy="10" r="9"></circle><path d="M10.97,7.72 C10.85,9.54 10.56,11.29 10.56,11.29 C10.51,11.87 10.27,12 9.99,12 C9.69,12 9.49,11.87 9.43,11.29 C9.43,11.29 9.16,9.54 9.03,7.72 C8.96,6.54 9.03,6 9.03,6 C9.03,5.45 9.46,5.02 9.99,5 C10.53,5.01 10.97,5.44 10.97,6 C10.97,6 11.04,6.54 10.97,7.72 L10.97,7.72 Z"></path></svg></span> <span class="uk-text-middle">Report issue</span></a></li>
            <li><a href="mailto:lagrit-dev@lanl.gov" target="_blank"><span uk-icon="icon: commenting" class="uk-margin-small-right uk-icon"><svg width="20" height="20" viewBox="0 0 20 20" xmlns="http://www.w3.org/2000/svg" data-svg="commenting"><polygon fill="none" stroke="#000" points="1.5,1.5 18.5,1.5 18.5,13.5 10.5,13.5 6.5,17.5 6.5,13.5 1.5,13.5"></polygon><circle cx="10" cy="8" r="1"></circle><circle cx="6" cy="8" r="1"></circle><circle cx="14" cy="8" r="1"></circle></svg></span> <span class="uk-text-middle">Get help</span></a></li>
        </ul>
    </div>
</div>