---
title: Online Manual
tags: manual, index
tipue_search_active: true
header_image: manual.jpg
header_title: Online Manual
header_subtitle: Commands, mesh objects and examples for building, refining and writing meshes with LaGriT.
---

{% assign u = "/pages/" | relative_url %}

<p class="lg-intro">
LaGriT builds, optimizes and writes 2D and 3D meshes for physics codes such as FEHM, PFLOTRAN and Amanzi.
Start with the introduction and tutorials, or go straight to the command reference.
</p>

<div class="lg-cards">
<div class="lg-card">
<div class="lg-card-img" style="background-image:url('{{ "/assets/img/gallery/cyl_sphere.jpg" | relative_url }}')"></div>
<div class="lg-card-body">
<div class="lg-card-num">Section I</div>
<h2>Introduction</h2>
<ul>
<li><a href="{{ u }}docs/lagrit.html">LaGriT and PyLaGriT</a></li>
<li><a href="{{ u }}features.html">LaGriT Features</a></li>
<li><a href="{{ u }}docs/meshing.html">Mesh Design Considerations</a></li>
<li><a href="{{ u }}tutorial/index.html">Tutorials and Examples</a></li>
</ul>
</div>
</div>
<div class="lg-card">
<div class="lg-card-img" style="background-image:url('{{ "/assets/img/gallery/octree.jpg" | relative_url }}')"></div>
<div class="lg-card-body">
<div class="lg-card-num">Section II</div>
<h2>LaGriT Commands</h2>
<ul>
<li><a href="{{ u }}docs/conventions.html">Syntax Conventions</a></li>
<li><a href="{{ u }}commands.html">Alphabetic listing of LaGriT commands</a></li>
<li><a href="{{ u }}commands_cat.html">LaGriT commands by Category</a></li>
</ul>
</div>
</div>
<div class="lg-card">
<div class="lg-card-img" style="background-image:url('{{ "/assets/img/gallery/graded_tets.jpg" | relative_url }}')"></div>
<div class="lg-card-body">
<div class="lg-card-num">Section III</div>
<h2>Mesh Objects</h2>
<ul>
<li><a href="{{ u }}docs/meshobject.html">Mesh Object Definition</a></li>
<li><a href="{{ u }}docs/meshobjcon.html">Mesh Object Connectivity</a></li>
<li><a href="{{ u }}docs/supported.html">Supported Element Types</a></li>
<li><a href="{{ u }}docs/geometries.html">Geometries</a></li>
<li><a href="{{ u }}docs/commandi.html">Command Interface</a></li>
<li><a href="{{ u }}docs/fortran.html">Fortran Interface</a></li>
</ul>
</div>
</div>
<div class="lg-card">
<div class="lg-card-img" style="background-image:url('{{ "/assets/img/gallery/letters3d.jpg" | relative_url }}')"></div>
<div class="lg-card-body">
<div class="lg-card-num">Section IV</div>
<h2>Interfacing User Routines</h2>
<ul>
<li><a href="{{ u }}docs/build.html">Building an Executable and Running LaGriT</a></li>
<li><a href="{{ u }}docs/issuing.html">Issuing Commands from a User Program</a></li>
<li><a href="{{ u }}docs/writing.html">Writing User Commands</a></li>
<li><a href="{{ u }}docs/accessing.html">Accessing the Mesh Object</a></li>
<li><a href="{{ u }}util.html">Utility Subroutines</a></li>
</ul>
</div>
</div>
<div class="lg-card">
<div class="lg-card-img" style="background-image:url('{{ "/assets/img/gallery/faults.jpg" | relative_url }}')"></div>
<div class="lg-card-body">
<div class="lg-card-num">Section V</div>
<h2>Errors</h2>
<ul>
<li><a href="{{ u }}docs/errors.html#parse">Errors in parsing or executing commands</a></li>
<li><a href="{{ u }}docs/errors.html#memory">Out of memory errors</a></li>
<li><a href="{{ u }}docs/errors.html#panic">Fatal memory management errors</a></li>
</ul>
</div>
</div>
</div>

<div class="lg-pair">
<h2>A Delaunay mesh and its Voronoi dual</h2>
<p>A discrete fracture network (DFN) meshed with LaGriT and <a href="https://lanl.github.io/dfnWorks/intro.html">dfnWorks</a>. The control volumes on the right are what <code>dump / stor</code> uses for the coefficients it writes to FEHM and PFLOTRAN.</p>
<div class="lg-pair-row">
<figure><img src="{{ "/assets/img/gallery/dfn_delaunay.jpg" | relative_url }}" alt="Delaunay triangle mesh of a discrete fracture network, coloured by vertex number"><figcaption>Delaunay triangulation</figcaption></figure>
<figure><img src="{{ "/assets/img/gallery/dfn_voronoi.jpg" | relative_url }}" alt="Voronoi control volumes of the same fracture network"><figcaption>Voronoi control volumes</figcaption></figure>
</div>
</div>

<div class="lg-more">
<a href="{{ u }}docs/References.html">LaGriT References</a>
<a href="{{ u }}publications.html">LaGriT Publications</a>
<a href="{{ u }}release.html">Release Notes</a>
<a href="https://github.com/lanl/LaGriT">LaGriT on GitHub</a>
</div>
