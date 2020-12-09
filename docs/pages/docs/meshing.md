---
title: Mesh Design and Considerations 
tags: mesh design, quality 
---

# Mesh Design Considerations


LaGriT provides a variety of meshing tools with capabilities specific to geologic applications and Voronoi control volume solvers. These tools were developed to generate and modify meshes, and also to create meshes with control volume discretization such that the underlying control volumes are Voronoi tessellations as preferred by modeling applications such as FEHM.



Some basic considerations include dimension, domain, and mesh size restrictions (number of degrees of freedom) versus mesh resolution needed for model features. What are the modeling application requirements?  Are mesh properties defined on the cell or the vertices? What element types do you need, hexahedral, tetrahedral? Choose a mesh design and meshing method that gives the best performance from the modeling application possible, with respect to the difficulty in generating the mesh.


Choose a design and method to create a mesh that is appropriate for the constraints for accuracy and numerical stability imposed by the modeling application and the simulations being run. Consider the physics to be modeled, mesh size restrictions (number of degrees of freedom) versus mesh resolution needed for model features, and the mesh and model information needed by the model application. 



Some modeling applications can solve problems only on orthogonal regular structured meshes, while others can handle unstructured meshes that conform to complex geologic stratigraphy and geologic structures such as sloped layers, faults, erosion and deposition. 



The unstructured approach allows the creation of meshes that exactly conform to the geometric model, but requires some expertise in building the mesh such that it will also meet the Delaunay criteria if required.
The easier method is to use a structured mesh with fine resolution, or a coarser mesh that uses octree refinement to increase resolution in user specified regions of interest. These result in stair-stepped geometries instead of smooth, but can be acceptable for where the geometry spacing is small relative to the full model domain. (Sentis and Gable, 2017). 


## Structured Mesh Methods


Although structured meshes are not as flexible as unstructured meshes in fitting complex geometry, tests have shown that they provide accurate solutions so long as there is adequate resolution to represent the geometry of the different materials in each hydrogeologic layer.  Moreover, there must be enough resolution to account for any large gradients.  The sufficiency of grid resolution is usually investigated by running a flow model using various grids of differing resolutions.


Example [mregion with surface command](https://lanl.github.io/LaGriT/pages/docs/commands/MREGION.html)


## Structured Mesh with Octree Refinement Methods

For a model with intersecting internal surfaces a mesh that conforms with the material layers is more difficult, especially in 3 dimensions. Rather than using an unstructured mesh requiring careful design and discretization, you can create a structured mesh and use octree refinement to capture the material geometry. Start with a hexahedral mesh with coarse spacing. LaGriT is used to intersect the extracted interfaces with the coarse hex mesh, the intersected cells are refined with the octree method multiple times to achieve the desired resolution at these interfaces. This point distribution can be connected into a tetrahedral Delaunay mesh. 


Example [Octree Refinement](https://meshing.lanl.gov/proj/examples/ex_octree_refine_intersect_object/index.html)


## Stacked Unstructured Methods

For a model with horizontal non-intersecting surfaces, you can create a computational mesh with conforming interfaces by using a stacked mesh method. This works well for modeling applications like Amanzi/ATS that can compute on hexahedral or prism elements. This method stacks quad or triangle surfaces and connects layers into a 2 ½ Dimension hex or prism mesh. 


Example [stack](https://lanl.github.io/LaGriT/pages/docs/demos/description2_connect.html)


## Unstructured Methods

A fully unstructured mesh can be built using volume surfaces that represent a geometry. It is necessary that these surfaces are coincident where they share a boundary. There cannot be any gaps or intersections where surfaces cross each other. The interface nodes may need refinement or buffering to ensure elements conform at the boundary. 


For modeling applications that require the Delaunay criteria, this can be very difficult. The mesh elements will conform to the geometry, but the mesh quality will likely contain a poor topology and generate negative coupling coefficients. The solutions from simulations may be inaccurate. You can mitigate the impact by adjusting the mesh resolution. Generally high aspect ratio tet elements (long dimension along the external boundary) are more of a problem. This means that mesh refinement that brings the mesh closer to unit aspect ratio will help.


Example [sphere in box](https://meshing.lanl.gov/proj/examples/ex_sphere_in_cube/index.html)




## Examples


[**Manual Tutorial and Examples**](https://lanl.github.io/LaGriT/pages/tutorial/)


[**LANL Meshing Projects**](https://meshing.lanl.gov/proj/index.shtml) 


[**GDSA Example with 4 Geologic Model Types**](https://meshing.lanl.gov/proj/SFWD_models/images/index.html)  


[**Tet Element Quality Measures with LaGriT**](QUALITY_sliver_cap_needle_wedge.md) 


## References


2017 Sentis, M.L., Gable, C.W., Coupling LaGrit unstructured mesh generation and model setup with TOUGH2 flow and transport: A case study, Computers & Geosciences, 2017, DOI:doi.org/10.1016/j.cageo.2017.06.012.
 
 
2006 Zyvoloski, G. & Vesselinov, Velimir. An Investigation of Numerical Grid Effects in Parameter Estimation. Ground water. 44. 814-25. 10.1111/j.1745-6584.2006.00203.x.


