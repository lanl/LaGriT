---
title: LaGriT - Home
---

<link rel="stylesheet" href="{{ "/assets/css/index_page_style.css" | relative_url }}">

<div class="extended-content-container">
  <div class="extended-content">
    
    <br>Powerful mesh generation,<br>optimization, and maintenance.<br>
                
    <div class="button" id="button-3">
    <div id="circle"></div>
    <a href="https://github.com/lanl/LaGriT/releases">Download</a>
    </div>
                
    <div class="button" id="button-4">
    <div id="underline"></div>
    <a href="https://lanl.github.io/LaGriT/pages/manual.html">LaGriT Manual</a>
    </div>

    <div class="button" id="button-4">
    <div id="underline"></div>
    <a href="https://lanl.github.io/LaGriT/pages/tutorial/index.html">LaGriT Tutorial</a>
    </div>

    <div class="button" id="button-4">
    <div id="underline"></div>
    <a href="https://lanl.github.io/LaGriT/pylagrit/original/index.html">PyLaGriT Manual</a>
    </div>

  </div>
</div>

<div class="accordion">
  <div class="accordion-item">
    <input type="checkbox" id="lagrit-toggle">
    <label class="accordion-header" for="lagrit-toggle">
      <bold>LaGriT (Los Alamos Grid Toolbox) LA-CC-15-069</bold>
    </label>
    <div class="accordion-content">
      <p><br>A library of user
      callable tools that provide mesh generation, mesh optimization and
      dynamic mesh maintenance in two and three dimensions. LaGriT is used for
      a variety of geology and geophysics modeling applications including
      porous flow and transport model construction, finite element modeling of
      stress/strain in crustal fault systems, seismology, discrete fracture
      networks, asteroids and hydrothermal systems. The general capabilities
      of LaGriT can also be used outside of earth science applications and
      applied to nearly any system that requires a grid/mesh and initial and
      boundary conditions, setting of material properties and other model
      setup functions. It can also be use as a tool to pre- and post-process
      and analyze vertex and mesh based data.</p>
      <p>Geometric regions for LaGriT are defined as combinations of bounding
      surfaces, where the surfaces are described analytically or as
      tessellated surfaces (triangles and/or quadrilaterals). A variety of
      techniques for distributing points within these geometric regions are
      provided. Mesh connectivity uses a Delaunay tetrahedralization algorithm
      that respects material interfaces. The data structures created to
      implement this algorithm are compact and powerful and expandable to
      include hybrid meshes (tet, hex, prism, pyramid, quadrilateral,
      triangle, line) however the main algorithms are for triangle and
      tetrahedral Delaunay meshes.</p>
      <p>Mesh refinement, derefinement and smoothing are available to modify the
      mesh to provide more resolution in areas of interest. Mesh refinement
      adds nodes to the mesh based on geometric criteria such as edge length
      or based on field variable shape. Mesh smoothing moves nodes to adapt
      the mesh to field variable measures, and, at the same time, maintains
      quality elements.</p>
      <p>LaGriT has three modes of use
        <ol>
          <li>command line</li>
          <li>batch driven via a control file</li> 
          <li>calls from C/Fortran programs</li>
        </ol>
      There is no GUI interface.</p>    
    </div>
  </div>
</div>

<div class="accordion">
  <div class="accordion-item">
    <input type="checkbox" id="pylagrit-toggle">
    <label class="accordion-header" for="pylagrit-toggle">
      <bold>PyLaGriT</bold>
    </label>
    <div class="accordion-content">
      <p><br>A Python interface that allows LaGriT functionality to
      be used interactively and in batch mode. It combines the meshing
      capabilities of LaGriT with the numeric and scientific functionality of
      Python including the quering of mesh properties, enhanced looping
      functionality, and user defined error checking. PyLaGriT has been
      developed to easily generate meshes by extrusion, dimensional reduction,
      coarsening and refinement of synthetic and realistic data. PyLaGriT
      enhances the workflow, enabling rapid iterations for use in simulations
      incorporating uncertainty in system geometry and automatic mesh
      generation.</p>    
    </div>
  </div>
</div>

## About LaGriT


- [Licensing](pages/licensing.md)
- [Applications](pages/applications.md)
- [Graphics examples](pages/graphics.md)
- [Publications](pages/publications.md)
- [Release Notes](pages/release.md)
- [LaGriT Style Guide](pages/github_pages_example.md)



### LaGriT Development Team [Contacts](pages/development.md)
