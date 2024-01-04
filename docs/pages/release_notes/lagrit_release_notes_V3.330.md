LaGriT V3.3.3 Release Notes
====================

This page describes version updates for V3.3.3 release. This release updates code to prepare for major capability developments using new C++ routines. CMake tools are used to enable the cross platform compile and linking of C/C++ to LaGriT Fortran code. 

Previous release  LaGriT v3.3.2  Apr 16, 2019 

### Enhancements:

- 2D Poisson Disc Sampling for point generation. See command createpts/poisson_disk
- New CMake files to interface C++ codes to lagrit fortran routines.
- Much improved CMake installation and some automatic Exodus lib handling
- read/gocad for 3D .so and 2D .ts files.
- AVS UCD pnt format added to make it easy for Paraview to read and view points.
- GMV option ipolydat = no is now the default, files will have smaller size without voronoi polygons included
- improvements to PyLaGriT scripts in support of stacked surfaces.
- The manual and command pages were heavily edited to add examples and remove unused large files
- Improvements to error reporting and warnings
- Test scripts and organization improved, level01 (always), level02 (for TPLs), level03 (dev only)

### Major Change in Build 

This release includes new build scripts and associated files that use cmake to compile, build, and test LaGriT with and without optional Exodus libs. 

Our cmake scripts do not always use the latest features, instead favoring readability and straightforward statements to help with debugging and troubleshooting. The cmake and install scripts are commented with suggestions and the build documentation has details for all steps. See README.md and links on main page.

- Cmake is controlled by CMakeLists.txt and settings found in /cmake files.
- The LaGriT build is fairly simple without ExodusII and is recommended if you do not need to write exodus files.
- For Exodus install, configure, and build for LaGrit, use scripts install-exodus.sh or Mac_install-exodus.sh

### Known Issues:

- New Mac compilers are showing a precision differences for very small numbers near zero. This does not appear to affect performance but might be noticed when comparing new mac output files with old versions. For instance in "degenerate" cases with 4 vertices on circumscribed circle, you can put the diagonal edge of the rectangle on either diagonal and round off may be the deciding factor. The different connection of this degenerate case is still correct/Delaunay. See issue #252

- If compiled with gfortran V5 or greater, LaGriT may exit on exceptions not caught in old portions of code.
Avoid this by using the flag -ffpe-trap=invalid,zero,overflow,underflow,denormal

- Though Window machines have been supported in the past, this release has not been built and tested on Windows.

- For more comments, changes, see Merge from Daniel Livingson branch: https://github.com/lanl/LaGriT/pull/237





```
 
