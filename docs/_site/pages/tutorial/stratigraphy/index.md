<ul class="uk-breadcrumb">
    <li><a href="{{ "/pages/tutorial/index.html" | relative_url }}">Tutorials &amp; Examples</a></li>
    <li><span>Stratigraphic Hex Mesh Tutorial</span></li>
</ul>

<div class="uk-inline">
    <img src="{{ "/pages/tutorial/stratigraphy/images/19_hex_01_to_tet.png" | relative_url }}" alt="">
    <div class="uk-overlay uk-light uk-position-bottom">
        <h2 id="strat-index" class="uk-h3 uk-margin-remove">Stratigraphic Hex Mesh Tutorial</h2>
    </div>
</div>

<a href="{{ "/pages/tutorial/stratigraphy/Tutorial_Hex_Mesh.zip" | relative_url }}" class="uk-button uk-button-primary tm-button-default uk-icon">
    Download Resources (7.5 MB)
    <span uk-icon="download"></span>
</a>

<div uk-alert>
    In this tutorial, we will be constructing a complex geostratigraphic mesh with a fault, distinct subsurface layering and multiple wells.
    The goal of this tutorial is to introduce the reader to intermediate and advanced LaGriT topics, including refinement, truncation, point-sets, element-sets and more.
</div>

<ul class="uk-pagination">
    <li class="uk-margin-auto-left"><a href="{{ "/pages/tutorial/stratigraphy/step_01.html" | relative_url }}">Next <span class="uk-margin-small-left" uk-pagination-next></span></a></li>
</ul>

<div class="tm-sidebar-right uk-visible@l">
    <div uk-sticky="offset: 160" class="uk-sticky uk-active uk-sticky-fixed" style="position: fixed; top: 160px; width: 200px;">
        <ul uk-scrollspy-nav="closest: li; scroll: true; offset: 100" class="uk-nav uk-nav-default tm-nav uk-nav-parent-icon">
            <li class="uk-active"><a href="#strat-index">Index</a></li>
            <li class=""><a href="{{ "/pages/tutorial/stratigraphy/step_01.html" | relative_url }}">1. Building a Hex Mesh</a></li>
            <li class=""><a href="{{ "/pages/tutorial/stratigraphy/step_02.html" | relative_url }}">2. Define Boundaries Using Point Sets</a></li>
            <li class=""><a href="{{ "/pages/tutorial/stratigraphy/step_03.html" | relative_url }}">3. Constructing Stratigraphy</a></li>
            <li class=""><a href="{{ "/pages/tutorial/stratigraphy/step_04.html" | relative_url }}">4. Map Surfaces to Mesh</a></li>
            <li class=""><a href="{{ "/pages/tutorial/stratigraphy/step_05.html" | relative_url }}">5. Constructing a Fault</a></li>
            <li class=""><a href="{{ "/pages/tutorial/stratigraphy/step_06.html" | relative_url }}">6. Truncate with Polyline</a></li>
            <li class=""><a href="{{ "/pages/tutorial/stratigraphy/step_07.html" | relative_url }}">7. Refine Fault</a></li>
            <li class=""><a href="{{ "/pages/tutorial/stratigraphy/step_08.html" | relative_url }}">8. Insert Well</a></li>
            <li class=""><a href="{{ "/pages/tutorial/stratigraphy/step_09.html" | relative_url }}">9. Convert Hex Mesh to Tet</a></li>
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