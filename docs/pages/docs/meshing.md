---
title: Mesh Design and Considerations 
tags: mesh design, quality 
---

# Mesh Design Considerations

------------

The mesh design is chosen with consideration of the physics to be modeled, mesh size restrictions (number of degrees of freedom) versus mesh resolution needed for model features, and the mesh and model information needed by the model application. 


For modeling applications with complex stratigraphy, depending on the mesh, you can get a stable but inaccurate solution to the physics (Zyvoloski and Vesselinov, 2006). Choose a mesh design and meshing method that gives the best performance from the modeling application possible, with respect to the difficulty in generating the mesh. 


LaGriT is used to generate meshes with control volume discretization such that the underlying control volumes are Voronoi tessellations. Developed by Los Alamos National Laboratory as open source software, LaGriT provides a variety of meshing tools with capabilities specific to geologic applications and Voronoi control volume solvers. Examples of methods used for geologic applications include unstructured and structured, both with adaptive refinement to geological features. The unstructured approach allows the creation of meshes that exactly conform to the geometric model, but requires some expertise in building the mesh such that it will also meet the Delaunay criteria. The easier method is to use a structured mesh with fine resolution, or a coarser mesh that uses octree refinement to increase resolution in user specified regions of interest. These result in stair-stepped geometries instead of smooth, but can be acceptable for where the geometry spacing is small relative to the full model domain. (Sentis and Gable, 2017).


2017 Sentis, M.L., Gable, C.W., Coupling LaGrit unstructured mesh generation and model setup with TOUGH2 flow and transport: A case study, Computers & Geosciences, 2017, DOI:doi.org/10.1016/j.cageo.2017.06.012.
 
2006 Zyvoloski, G. & Vesselinov, Velimir. An Investigation of Numerical Grid Effects in Parameter Estimation. Ground water. 44. 814-25. 10.1111/j.1745-6584.2006.00203.x.


<hr>


[**Tet Quality Measures with LaGriT**](QUALITY_sliver_cap_needle_wedge.md) 


[**GDSA Example with 4 Geologic Model Types**](https://meshing.lanl.gov/proj/SFWD_models/images/index.html)  


[**LANL Meshing Examples**](https://meshing.lanl.gov/)  


 


