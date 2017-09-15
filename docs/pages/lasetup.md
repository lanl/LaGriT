---
Keywords: 
    Mesh generation focused on Delaunay triangle and tetrahedral meshing.
    Mesh generation for geological applications. Mesh smoothing
    (optimization), reconnection, hybrid meshing (quadrilateral, prism,
    pyramid, line elements). Constructive solid geometry, Voronoi control
    volume area volume coefficient calculation.
description: 
    LaGriT is a library of user callable tools that provide mesh generation,
    mesh optimization and dynamic mesh maintenance in two and three
    dimensions for a variety of applications.
title: LANL  LaGriT 
---

 
Lasetup
-------

lasetup is a user interface for reading or creating, and writing, the
geometry portions of LaGriT input files. The advantage that LaGriTsetup
gives you is that you can see what you are creating, thus avoiding
mistakes, and it can save you from a certain amount of tedious typing.
lasetup deals with the LaGriT surface, region, and mregion commands
only.

lasetup uses the OpenGL and Motif graphics libraries. You will not be
able to use lasetup unless these libraries are installed on your
workstation.

You will also need a 24-bit TrueColor visual, preferably double
buffered, available on your machine. You can find out what is available
by running xdpyinfo. Also the X server must be running the GLX
extension. Extensions are listed near the top of the output of xdpyinfo.
[lasetup Requirements](lasetup_require.md)
[lasetup User's Manual(ps)](lasetupdoc.ps)
[lasetup User's Manual(pdf)](<a href="https://lanl.github.io/LaGriT/assets/images/lasetupdoc.pdf" download> </a>
[Download lasetupsgi](lasetupsgi)
[Download lasetupsolaris](lasetupsolaris)
[Download lasetuplinux](lasetuplinux)

Questions may be directed to: John Fowler at <jxf@lanl.gov

