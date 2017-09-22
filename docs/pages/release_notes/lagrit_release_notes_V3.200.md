LaGriT Release Notes
====================

This document is a summary of LaGriT versions up to latest version of LaGriT V3.
Older versions with high impact are also described starting with V2.1 2009 Release. 

-------------------------

## LaGriT V3.203 July 2017

- Major upgrade to LaGriT build and test scripts.
- Added install.h and improved documentation for building LaGriT on Linux and Mac machines.
- Update ExodusII to 7.01 using git clone ```https://github.com/gsjaardema/seacas.git```
- Removed exodus include files from src directory and use install.sh instead
- Convert lagrit.lanl.gov to github Markdown pages at ```https://lanl.github.io/LaGriT```

### These issues were fixed:

- segmentation fault during triangulate
- massage command failing to de-refine and no error is reported
- ExodusII output for 2D Planar incorrectly written as 3 Dimensions
- second call to addmesh/excavate command causing memory error or segmentation fault
- output adjusted to make room for large numbers when reporting dudded nodes from filter command

### Added to Test Suite:

- *level01/createpts_filter*  illustrates the difference between filter and the more accurate filterkd commands
- *level01/smooth_massage*  use massage with smooth to refine and de-refine a triangulation
- *level01/write_exo*  write 3D and 2D Exodus files with psets, eltsets, and facesets
- *level01/pflotran_stor*  write pflotran style Voronoi geometric coefficient (volume, face area) files

### PyLaGriT new features:

- **read_sheetij**  creates a quad mesh from an elevation file reading Z(i,j) into mesh zic attribute
- **read_modflow** creates a modflow rectilinear mesh using modflow files using elev\_top.mod and init_bnds.inf
- **read_fehm** read FEHM format mesh file with geometry and connectivity (LaGriT does not have this option)
- *examples/arctic_w_ice_wedges* example Mesh for 2D polygon with ice wedges by Chuck Abolt
- *examples/julia* Example of using PyLaGriT within a Julia script. Requires that the Julia package PyCall is installed.


### Known Issues:

- Builds with compiler gfortran v5 or greater can generate run-time signal exceptions such as IEEE_DNORMAL. Most exceptions have been tracked down and fixed according to picky compiler suggestions. These exceptions have not changed the behavior of the code. Exceptions will continue to be fixed as they show up.

- During testing it was found that the filter command may find and remove fewer nodes than the command filterkd. The command filterkd is recommended where precision might be an issue. Documentation will be changed to reflect this. It is expected that the filter command and algorithm will be deprecated and replaced by filterkd. See *test/level01/createpts_filter*

- Paraview has trouble displaying ExodusII facesets for 2D grids in 3 Dimensions, but is fine with 2D planar where Z values are ignored. When viewing these same ExodusII files with GMV, the facesets look correct. Checking files by converting to ASCII with ncdump, the files look correct. We do not know if this is a LaGriT, ExodusII, or Paraview issue. See *test/level01/write_exo* for ExodusII example files.

---------------------------

## LaGriT V3.200 September 2016


LaGriT V3 LACC-15-069 is now distributed as open-source software under a BSD 3-Clause License.

This version moves the entire LaGriT repo from a local mercurial version control to public github and now includes the python driven version PyLaGriT. For most current versions of source and documentation use the open-source repository at
``` https://github.com/lanl/LaGriT```

Compiled executable versions of LaGriT will continue to available through https://lagrit.lanl.gov/licensing.md


*See code and issues at https://github.com/lanl/LaGriT*

