---
title: "CREATEPTS"
tags: createpts, options
---
 
# CREATEPTS 

--------------------

This command adds points to a mesh object. For some special cases, it will also produce element connectivity. The **createpts** command is a wrapper for the deprecated **rz** type commands.
  

**[createpts/xyz rtz rtp line](createpts/CRTPTSRZ.md)** Create points and distribute by coordinate type selection. No connectivity created.

**[createpts/brick](createpts/CRTPTBRICK.md)** Create points in a rectangular distribution and create finite element hexahedral or quad connectivity.

**[createpts/interp](createpts/createpts_interp.md)** Creates points by linear interpolation between two point sets.

**[createpts/sphere](createpts/cresphere.md)** Create points for a sphere.

**[createpts/random](createpts/CRTPTRZRAN.md)** Add random points within region.

**[createpts/vector](createpts/CRTPTRZV_LG.md)** Create points in region of space spanned by input vectors.

**[createpts/voronoi](createpts/createpts_voronoi.md)** Creates element attributes xvor, yvor, and zvor.

**[createpts/median](createpts/createpts_median.md)** Creates element attributes xmed, ymed, and zmed.

**[createpts/amr](createpts/CREATEPTSAMR.md)** Create points using octree type refinement on hexahedral mesh.
  
  

 [Click here for demos](../demos/main_createpts.md)

