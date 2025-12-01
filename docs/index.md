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
    <div class="button" id="button-3">
      <div id="circle"></div>
        <a href="{{ "/pages/tutorial/index.html" | relative_url }}">LaGriT Tutorial</a>
    </div>
    <div class="button" id="button-3">
      <div id="circle"></div>
        <a href="https://lagrit.lanl.gov/">Contact Us</a>
    </div>
  </div>
</div>

**LaGriT (Los Alamos Grid Toolbox) LA-CC-15-069** is a library of user
callable tools that provide mesh generation, mesh optimization and
dynamic mesh maintenance in two and three dimensions. LaGriT is used for
a variety of geology and geophysics modeling applications including
porous flow and transport model construction, finite element modeling of
stress/strain in crustal fault systems, seismology, discrete fracture
networks, asteroids and hydrothermal systems. 


The general capabilities of LaGriT can also be used outside of earth science applications and
applied to nearly any system that requires a grid/mesh and initial and
boundary conditions, setting of material properties and other model
setup functions. It can also be use as a tool to pre- and post-process
and analyze vertex and mesh based data.


<div class="accordion">
  <div class="accordion-item">
    <input type="checkbox" id="lagrit-toggle">
    <label class="accordion-header" for="lagrit-toggle">
      <bold>About LaGriT </bold> | Description | Capabilities
    </label>
    <div class="accordion-content">
      <p><br><b>LaGriT</b> provides a variety of meshing tools specific (but not limited) to geologic applications and Voronoi control volume solvers. These tools were developed to generate and modify meshes, and also to create meshes with control volume discretization such that the underlying control volumes are Voronoi tessellations as preferred by some modeling applications. Capabilities include:
        <ul>
          <li>Representation of 2- and 3-dimensional complex geometries with multiple materials or regions </li>
          <li>Unstructured triangle/tetrahedral and structured or unstructured quadrilateral/hexahedral meshing</li>
          <li>Model set-up including assigning material properties, boundary conditions, and initial conditions</li>
          <li>Adaptive mesh refinement, smoothing, and optimization</li>
          <li>Distribute uniform or variable spaced points using Poisson Disk sampling</li>
          <li>2D and 3D Delaunay triangulation conforming to complex geometry</li>
          <li>Output for solver packages including specialized format for FEHM, Amanzi/ATS, PFLOTRAN, and TOUGH2</li>
          <li>Interactive command line, batch input file, or embedded in Fortran/C interfaces (no GUI)</li>
        </ul>
      </p>
      <p><b>PyLaGriT</b> is a Python interface that allows LaGriT functionality to
      be used interactively and in batch mode. It combines the meshing
      capabilities of LaGriT with the numeric and scientific functionality of
      Python including the quering of mesh properties, enhanced looping
      functionality, and user defined error checking. PyLaGriT has been
      developed to easily generate meshes by extrusion, dimensional reduction,
      coarsening and refinement of synthetic and realistic data. PyLaGriT
      enhances the workflow, enabling rapid iterations for use in simulations
      incorporating uncertainty in system geometry and automatic mesh
      generation.
      <a href="{{ "/pylagrit/original/index.html" | relative_url }}">PyLaGriT Manual</a>
      </p>
    </div>
  </div>
</div>

<div class="accordion">
  <div class="accordion-item">
    <input type="checkbox" id="using-toggle">
    <label class="accordion-header" for="using-toggle">
      <bold>Using LaGriT </bold> | Commands | Syntax | Mesh Design
    </label>
    <div class="accordion-content">
    <p><br>LaGriT works on a single or multiple mesh objects which can be created or read from a file. The actions on the mesh object are driven by mesh commands and their options. These commands can be called interactively on a command line, or scripted with an input file. LaGriT will write two output files upon completion, by default they are lagrit.out (summary and reports for each command) and lagrit.log (saved commands). See the following about using the commands. Refer to Tutorials and Examples for help with syntax and work flow. 
    </p>
    <ul>
      <li><a href="{{ "/pages/commands.html" | relative_url }}">Commands listed Alphabetically</a></li>
      <li><a href="{{ "/pages/commands_cat.html" | relative_url }}">Commands listed by Category</a></li>
      <li><a href="{{ "/pages/docs/commandi.html" | relative_url }}">Command Interface</a></li>
      <li><a href="{{ "/pages/docs/conventions.html" | relative_url }}">Syntax Conventions</a></li>
      <li><a href="{{ "/pages/docs/meshing.html" | relative_url }}">Mesh Design Considerations</a></li>
      <li><a href="{{ "/pages/docs/errors.html" | relative_url }}">LaGriT Error reporting</a></li>
    </ul>
    </div>
  </div>
</div>

<div class="accordion">
  <div class="accordion-item">
    <input type="checkbox" id="examples-toggle">
    <label class="accordion-header" for="examples-toggle">
      <bold>LaGriT Examples</bold> | Tutorials | Test Cases | Projects
    </label>
    <div class="accordion-content">
    <p><br>The easiest way to use LaGriT is by copying from working examples. The Tutorials will help you to understand how commands can be combined into a work flow. Examples and demos provide methods to create meshes and ways to optimize for and check for improved mesh quality.
    </p>
    <ul>
      <li><a href="{{ "/pages/tutorial/index.html" | relative_url }}">Tutorials and Examples</a></li>
      <li><a href="{{ "/pages/docs/test_list.html" | relative_url }}">Test Suite Cases</a></li>
      <li><a href="https://meshing.lanl.gov/proj/index.shtml">Meshing Projects Page</a></li>
      <li><a href="{{ "/pages/graphics.html" | relative_url }}">Some Graphics Examples</a></li>
    </ul>
    </div>
  </div>
</div>

<div class="accordion">
  <div class="accordion-item">
    <input type="checkbox" id="details-toggle">
    <label class="accordion-header" for="details-toggle">
      <bold>LaGriT Details</bold> | Mesh Objects | Elements | Geometries
    </label>
    <div class="accordion-content">
    <p><br>The Mesh Object is the data structure which contains the information necessary to define a mesh. It consists of attributes that include coordinates, connectivity, and other data. Attributes are updated by LaGriT routines and can be modified by the user. Descriptions of the mesh object and associated details are included here. 
    </p>
    <ul>
      <li><a href="{{ "/pages/docs/meshobject.html" | relative_url }}">Mesh Object Definition</a></li>
      <li><a href="{{ "/pages/docs/meshobjcon.html" | relative_url }}">Mesh Object Connectivity</a></li>
      <li><a href="{{ "/pages/docs/supported.html" | relative_url }}">Supported Element Types</a></li>
      <li><a href="{{ "/pages/docs/geometries.html" | relative_url }}">Geometries and Regions</a></li>
    </ul>
    </div>
  </div>
</div>

<div class="accordion">
  <div class="accordion-item">
    <input type="checkbox" id="dev-toggle">
    <label class="accordion-header" for="dev-toggle">
      <bold>LaGriT Development</bold> | Build | C and Fortran | User Programs | Utilities | GitHub Pages
    </label>
    <div class="accordion-content">
    <p><br>LaGriT was originally written with Fortran and C and now includes C++ routines. New wrappers enable C++ to access fortran code needed to maintain and manipulate mesh objects.  CMake is used to generate the build system and works with C, C++, and compatible Fortran compilers. 
    </p>
    <ul>
      <li><a href="{{ "/pages/docs/build.html" | relative_url }}">Building an Executable and Running LaGriT</a></li>
      <li><a href="{{ "/pages/docs/fortran.html" | relative_url }}">Fortran Interface to LaGriT code</a></li>
      <li><a href="{{ "/pages/docs/c-fortran.html" | relative_url }}">C-Fortran Interface to LaGriT code</a></li>
      <li><a href="{{ "/pages/docs/issuing.html" | relative_url }}">Issuing Commands from a User Program</a></li>
      <li><a href="{{ "/pages/docs/writing.html" | relative_url }}">Writing User Commands</a></li>
      <li><a href="{{ "/pages/docs/accessing.html" | relative_url }}">Accessing the Mesh Object</a></li>
      <li><a href="{{ "/pages/util.html" | relative_url }}">Utility and Memory Subroutines</a></li>
      <li><a href="{{ "/pages/github_pages_example.html" | relative_url }}">Style Guide and Conventions</a></li>
    </ul>
    <br>
    <a href="{{ "/pages/manual.html" | relative_url }}">Old Manual Index</a>
    </div>
  </div>
</div>
