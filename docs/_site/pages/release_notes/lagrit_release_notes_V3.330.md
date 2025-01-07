LaGriT V3.3.3 Release Notes
====================

This page describes version updates for V3.3.3 release. This release updates code to prepare for major capability developments using new C++ routines. CMake tools are used to enable the cross platform compile and linking of C/C++ to LaGriT Fortran code. 

Previous release  LaGriT v3.3.2  Apr 16, 2019 

### Enhancements:

- 2D Poisson Disc Sampling for point generation. See command createpts/poisson_disk
- Modified triangulate so that it will try clockwise and counterclockwise polygon orientation.
- read/gocad for 3D .so and 2D .ts files.
- AVS UCD pnt format added to make it easy for Paraview to read and view points.
- GMV option ipolydat = no is now the default, files will have smaller size without voronoi polygons included
- Improvements to PyLaGriT commands and usage, test cases added to the directory
- The manual and command pages were heavily edited to add examples and remove unused large files
- Test scripts and organization improved to include level01 (always), level02 (exodus libs), level03 (dev only)

### Major Change in Build 

This release includes new build scripts and associated files that use cmake to compile, build, and test LaGriT with and without optional Exodus libs. The build process with and without Exodus has been tested on Linux (Ubuntu 18 and 20) and MacOS (Ventura Intel and M2). Further work, including for Windows, will continue in future releases.

Our cmake scripts do not always use the latest features, instead favoring readability and straightforward statements to help with debugging and troubleshooting. The cmake and install scripts are commented with suggestions and the build documentation has details for all steps. See README.md and links on main page.

- Cmake is controlled by CMakeLists.txt and settings found in /cmake files.
- Cmake creates lagrit.h using PROJECT_VERSION_* in template lagrit.h.in
- Cmake creates fc_mangle.h for c-fortran routines to handle symbol mangling, CMakeLists.txt names should match declarations in src/lg_f_interface.h
- The LaGriT build is fairly simple without ExodusII and is recommended if you do not need to write exodus files.
- For Exodus install, configure, and build for LaGrit, use scripts install-exodus.sh or Mac_install-exodus.sh

### Major Codes Added

The new poisson-disk routines are written in C++ and depend on new codes in support of c-fortran interfaces. See src/poi_* for the new poisson-disk codes. The file src/lg_example.cpp explains how the c-fortran codes are handled.

```
lg_fc_wrappers.f90 - used to assign mesh object cray pointer to c pointer
lg_c_wrappers.cpp - cpp wrappers for dotask and cmo_get_info routines
lg_f_interface.h  - fortran declarations for routines in public_interface.cpp
lg_c_interface.h  - constants and c declarations
fc_mangle.h       - created by cmake and CMakeLists.txt to mangle fortran declarations
lg_routines are C++ wrappers calling fortran using C arguments
fc_routines are f90 wrappers calling fortran using c-fortran arguments
In all cases types passed must match types in fortran where integer=8bytes, real*8=8bytes, pointer=8bytes
```


### Known Issues

- New Mac compilers are showing a precision differences for very small numbers near zero. This does not appear to affect performance but might be noticed when comparing new mac output files with old versions. For instance in "degenerate" cases with 4 vertices on circumscribed circle, you can put the diagonal edge of the rectangle on either diagonal and round off may be the deciding factor. The different connection of this degenerate case is still correct/Delaunay. See issue #252

- If compiled with gfortran V5 or greater, LaGriT may exit on exceptions not caught in old portions of code.
Avoid this by using the flag -ffpe-trap=invalid,zero,overflow,underflow,denormal

- Though Window machines have been supported in the past, this release has not been compiled and tested on Windows.

### Log Summary

```
 git log --pretty=format:"%h - %an, %ar : %s" --since=3.years

35f2fc81 - Terry Miller, 24 hours ago : Update lagrit_release_notes_V3.330.md
f77526bb - Terry Miller, 3 days ago : Update README.md
80509d5b - Erica Hinrichs, 3 days ago : Adding an Exodus install script for MAC
9d9da4a6 - Erica Hinrichs, 3 days ago : Updates to runtest.py and README files
ab02ec31 - Terry Miller, 3 days ago : level03 with precision diffs for mac and linux
945f7523 - Terry Miller, 2 weeks ago : Examples for poisson development
05664065 - Erica Hinrichs, 2 weeks ago : Modified and improved test suite for usability and to fix arguments not working issue.
a4889204 - Terry Miller, 2 weeks ago : Added new tests for stack and mac precision test
77fd45c2 - Terry Miller, 2 weeks ago : Moved hex_3x4 to level03 test for mac precision issue
a6cc9fdc - Terry Miller, 2 weeks ago : Updates to reference files with added screen output
8020b578 - Terry Miller, 2 weeks ago : Update createpts_poisson.md
901b7e6e - Terry Miller, 2 weeks ago : exodus build
d2a29f79 - Jeffrey Hyman, 2 weeks ago : Merge branch 'master' into poisson_disk
80401237 - Carl Walter Gable, 2 weeks ago : Merge branch 'poisson_disk' of github.com:lanl/LaGriT into poisson_disk
dd5db696 - Erica Hinrichs, 2 weeks ago : Small edits to fix implicit declaration errors.
b72fe9d1 - Terry Miller, 3 weeks ago : poisson_disk documentation
54bd7a12 - Terry Miller, 3 weeks ago : Merge branch 'master' of github.com:lanl/LaGriT
7109fa05 - Terry Miller, 3 weeks ago : Running the script will FIX Exodus cmake files for a successful linux build
9729129e - Terry Miller, 3 weeks ago : Add instructions to use SSH clone for git push/pull
896c4432 - Carl Walter Gable, 3 weeks ago : Working version of 2D Poisson disk code, added advanced options for seed, sweeps, samples, and a few typos corrected in *.cpp files.
08d4d081 - Terry Miller, 3 weeks ago : Update lagrit_release_notes_V3.330.md
d0c973dd - Terry Miller, 4 weeks ago : Update README.md
810be706 - Jeffrey Hyman, 4 months ago : grid memory allocation
d0307191 - Jeffrey Hyman, 4 months ago : preallocated vectors in poisson disk
bc1ba76c - Terry Miller, 6 months ago : Added README.md and modified files in this documentation directory to clarify these files are pre-2019 from mercurial and web page documentation. Current documentation is in the docs directory.
e3345ad7 - Terry Miller, 6 months ago : Update build.md WINDOWS under dev
affb3420 - Terry Miller, 6 months ago : Update build.md regarding cmake
94da5982 - Terry Miller, 6 months ago : Update development.md contacts
0724f698 - Terry Miller, 6 months ago : Update index.md broken link
b9972b9e - Terry Miller, 6 months ago : Update index.md with quick links at top
7d0779e0 - Jeffrey Hyman, 6 months ago : Merge branch 'master' into poisson_disk
822059b7 - Jeffrey Hyman, 6 months ago : Merge branch 'master' into poisson_disk
1ad05506 - Terry Miller, 6 months ago : Small edits to build lagrit on Linux for V3.3.3
d54129db - Terry Miller, 6 months ago : CMakeLists.txt working on linux and mac with exodus and no exodus
aa6b03cf - Terry Miller, 6 months ago : Removed original_pylagrit_website
fd1c851b - Terry Miller, 6 months ago : remove unused m64 flag, add error reporting
d249f6a9 - Jeffrey Hyman, 10 months ago : added z value to polygon sampling
f03ba1c7 - Jeffrey Hyman, 10 months ago : more formatting
bb1e52fa - Jeffrey Hyman, 10 months ago : cleaing up Poisson Disk screen output
b89018e6 - Jeffrey Hyman, 10 months ago : advanced user options for Poisson Disck Sampling
6d1b4140 - Jeffrey Hyman, 10 months ago : adding seed option to C++ code. needs to be added to LaGrit high-level command
7223f104 - Jeffrey Hyman, 1 year, 1 month ago : AStyle formatting of c++ files
edd7740c - Jeffrey Hyman, 1 year, 1 month ago : mo_poi_h_field added to C++ poisson input arguments
fcc1ab8f - Terry Miller, 1 year, 3 months ago : update instructions
3893346e - Terry Miller, 1 year, 3 months ago : cmake options
3d83906a - Terry Miller, 1 year, 3 months ago : exodus install
965ccdb5 - Terry Miller, 1 year, 3 months ago : update instructions
d7ea4a51 - Terry Miller, 1 year, 3 months ago : Fixes to cmake files to build with Exodus on Linux and macOSX
5dd59c84 - Terry Miller, 1 year, 5 months ago : make in build dir
8878d2d2 - Terry Miller, 1 year, 5 months ago : stack
733efb9f - Terry Miller, 1 year, 5 months ago : add 4 octree examples
ee71c25d - Terry Miller, 1 year, 6 months ago : add instructions for build and test
402fd797 - Carl Walter Gable, 1 year, 1 month ago : Modifid arguments to poisson_2d. Cleaned up some screen output. Get rid of unused variables. Refined steps taken for nonconvex polygons.
fd256570 - Carl Walter Gable, 1 year, 2 months ago : Added code to poi_driver.f to handle nonconvex polygon input. Modified triangulate_lg.f so that it will try clockwise and counterclockwise polygon orientation.
69f4008a - Carl Walter Gable, 1 year, 2 months ago : First working version of Poisson disk driver subroutine
2f34a017 - Carl Walter Gable, 1 year, 2 months ago : Change default name of node attribute to id_node. It was ialias
ecdb22c2 - Jeffrey Hyman, 1 year, 3 months ago : linux gnu 7.5 doesn't like vector.reserve
a1d466f8 - Jeffrey Hyman, 1 year, 3 months ago : commiting to vectors
9678020f - Jeffrey Hyman, 1 year, 3 months ago : working again
535a664e - Jeffrey Hyman, 1 year, 3 months ago : revert back from vector:
6da61e7a - Jeffrey Hyman, 1 year, 3 months ago : convert to linear index vectors
dbb25433 - Jeffrey Hyman, 1 year, 3 months ago : revert
9a5d4db4 - Jeffrey Hyman, 1 year, 3 months ago : cleaning up lagrit calls
b5dc9738 - Jeffrey Hyman, 1 year, 3 months ago : switch to linear indexing
72bedc53 - Jeffrey Hyman, 1 year, 3 months ago : changing grid to linear indexing
9028310d - Jeffrey Hyman, 1 year, 3 months ago : debugging neighbor grid
a80b78b2 - Jeffrey Hyman, 1 year, 3 months ago : debugging neighbor grid
dd9625b0 - Jeffrey Hyman, 1 year, 3 months ago : Cleaned out dumps
9d0122d0 - Jeffrey Hyman, 1 year, 3 months ago : I tihnk it's working?
aea1dea3 - Jeffrey Hyman, 1 year, 3 months ago : names are cleaned up and working
fe3cdb7a - Jeffrey Hyman, 1 year, 3 months ago : cleaned up a bunch of names
148058a1 - Jeffrey Hyman, 1 year, 3 months ago : adding checks
de3bf69c - Jeffrey Hyman, 1 year, 3 months ago : working on reading in the vertices
db203abd - Jeffrey Hyman, 1 year, 3 months ago : fixed line length
dbf1477b - Jeffrey Hyman, 1 year, 3 months ago : set up for merge
1a0b624d - Jeffrey Hyman, 1 year, 3 months ago : set up for merge
8f7bfda8 - Jeffrey Hyman, 1 year, 3 months ago : distance field load seems to be working npw
fc7a6eae - Jeffrey Hyman, 1 year, 3 months ago : distance field indexing is wrong. radius assigned to h. Seems to work as a hack for now
e002cbb0 - Terry Miller, 1 year, 3 months ago : verbose
747e51d4 - Terry Miller, 1 year, 3 months ago : verbose options
dc77a616 - Terry Miller, 1 year, 3 months ago : update instructions
65c61a71 - Terry Miller, 1 year, 3 months ago : cmake options
fb25fdf9 - Terry Miller, 1 year, 3 months ago : exodus install
5dd67dce - Terry Miller, 1 year, 3 months ago : update instructions
3fcd4572 - Terry Miller, 1 year, 3 months ago : Fixes to cmake files to build with Exodus on Linux and macOSX
49c5bcd2 - Jeffrey Hyman, 1 year, 5 months ago : PD-2D seems to be working
aa8d96ab - Jeffrey Hyman, 1 year, 5 months ago : poisson disk 2d uniform working
425a2f18 - Jeffrey Hyman, 1 year, 5 months ago : trying to pass doubles / Reals to C++
a042018f - Jeffrey Hyman, 1 year, 5 months ago : loaded points onto polygon from cmo
08be2c18 - Jeffrey Hyman, 1 year, 5 months ago : fixed up conflict
5f98c47f - Jeffrey Hyman, 1 year, 5 months ago : setup for merge
aed1dba6 - Carl Walter Gable, 1 year, 5 months ago : fortran driver for PD
00435baa - Terry Miller, 1 year, 5 months ago : dev
a174b0d5 - Terry Miller, 1 year, 5 months ago : make in build dir
ae8ae294 - Terry Miller, 1 year, 5 months ago : stack
1b15c844 - Terry Miller, 1 year, 5 months ago : img
1c1be6e7 - Terry Miller, 1 year, 5 months ago : add image
1e98921e - Terry Miller, 1 year, 5 months ago : GDSA 4 Tests image
40642680 - Terry Miller, 1 year, 5 months ago : add 4 octree examples
82e6c86f - Terry Miller, 1 year, 6 months ago : add instructions for build and test
53fd3bbd - Terry Miller, 1 year, 7 months ago : These files contain routines to interface C++ codes to lagrit fortran routines. fc_mangle.h included but will be overwritten with cmake Note lg_ files are similar to Daniel's C++ interface on windows branch
8eedca21 - Terry Miller, 1 year, 7 months ago : add command poisson and Jeffrey's files for 2D version these are under development to add syntax and methods
409c05f2 - Terry Miller, 1 year, 7 months ago : Corrected to return REAL value for REAL option instead of int Added checks for 8 byte sizes passed by argument
c6180cfe - Terry Miller, 1 year, 7 months ago : Added portion to create src/fc_mangle.h to bind C-Fortran subroutine names
87986f8c - Terry Miller, 1 year, 8 months ago : examples
8b696073 - Terry Miller, 1 year, 8 months ago : Update for compile with cmake, add test using cmo_get calls make sure user_sub not called if command not recognized added test command
65ebad45 - Terry Miller, 1 year, 9 months ago : new files
c5fa0a2b - Terry Miller, 1 year, 9 months ago : make
4464fc3d - Terry Miller, 1 year, 9 months ago : no exodus
c33a9288 - Terry Miller, 1 year, 9 months ago : links
439579dd - Terry Miller, 1 year, 9 months ago : add workflow
17406ca8 - Terry Miller, 1 year, 9 months ago : -99
42e6826a - Terry Miller, 1 year, 10 months ago : put back with new img
a5c0d7f2 - Terry Miller, 1 year, 10 months ago : remove tutorial section
d5d62f36 - Terry Miller, 1 year, 10 months ago : img
ab311ff0 - Terry Miller, 1 year, 10 months ago : img
fbae5abb - Terry Miller, 2 years ago : update instructions
3be8a088 - Terry Miller, 2 years ago : clarify instructions
e546cb99 - Terry Miller, 2 years ago : tested and working examples moved to level01 remaining files need more work and testing
901423ba - Terry Miller, 2 years ago : clarification on level03 test directories
3561e088 - Terry Miller, 2 years ago : test removed from level03 and replaced with new test in level01
31cffeac - Terry Miller, 2 years ago : test removed from level03 and replaced with new test in level01
59ff12d1 - Terry Miller, 2 years ago : for V3.32 and older
18ac81d6 - Terry Miller, 2 years ago : Update lagrit_release_notes_V3.330.md
cceb3d16 - Daniel Livingston, 2 years ago : Merge pull request #237 from lanl/RC-v3.3.3
869ff333 - Daniel Livingston, 2 years ago : Fixed test suite for quadquality test
3d39f905 - Daniel Reece Livingston, 2 years ago : Update test readme
1d800023 - Daniel Reece Livingston, 2 years ago : Remove extra file
b4378726 - Daniel Reece Livingston, 2 years ago : Release cleanup
6ab71b89 - Daniel Livingston, 2 years ago : Added test info to readme
7dc091da - Daniel Livingston, 2 years ago : Merge branch 'master' of github.com:lanl/LaGriT into RC-v3.3.3
a720479d - Daniel Livingston, 2 years ago : Clean up test cases
7a9e341c - Daniel Livingston, 2 years ago : More modernized test suite
164b7bfc - Daniel Reece Livingston, 2 years, 1 month ago : Added unittest testing
8d51b78b - Daniel Reece Livingston, 2 years, 1 month ago : Attempt to update GitHub Actions again
dccc97e5 - Daniel Reece Livingston, 2 years, 1 month ago : Update GitHub CI with compilers
f8928477 - Terry Miller, 2 years, 1 month ago : update
4f112c7d - Daniel Reece Livingston, 2 years, 1 month ago : Updated CMake significantly
bb3964c1 - Daniel Reece Livingston, 2 years, 1 month ago : Beginning CMake writeup
b869bbc2 - Daniel Reece Livingston, 2 years, 1 month ago : CMake now writes mm2000.h
44db2213 - Daniel Reece Livingston, 2 years, 1 month ago : Testing LaGriT
0d461985 - Daniel Reece Livingston, 2 years, 1 month ago : Fixed missing gfortran in macOS
234235c7 - Daniel Reece Livingston, 2 years, 1 month ago : Fixed CI Python issue
6b7d86f5 - Daniel Reece Livingston, 2 years, 1 month ago : Improvements to Exodus build
cd71aeb4 - Daniel Reece Livingston, 2 years, 1 month ago : Some bug fixes
fe3b42ee - Daniel Reece Livingston, 2 years, 1 month ago : Update Python version for CI
11fbc6d3 - Daniel Reece Livingston, 2 years, 1 month ago : Update GitHub CI
900cdc47 - Daniel Livingston, 2 years, 1 month ago : Update test-lagrit.yml
cefb5d97 - Daniel Livingston, 2 years, 1 month ago : Exous scripts
08405dae - Daniel Livingston, 2 years, 1 month ago : Added auto-push compiled to CI
51c94207 - Daniel Livingston, 2 years, 1 month ago : Updated CI
ab55fc97 - Daniel Livingston, 2 years, 1 month ago : Updated build
93e52c45 - Daniel Livingston, 2 years, 1 month ago : Updated LaGriT CMake
01dd9c2a - Daniel Livingston, 2 years, 1 month ago : Much improved CMake installation and some automatic Exodus lib handling
35bdf6ca - Daniel Livingston, 2 years, 1 month ago : Working static and dynamic linkage
2db46e78 - Daniel Livingston, 2 years, 1 month ago : Changed Fortran preproc settings for CMake 3.11
c8bd8bc3 - Daniel Livingston, 2 years, 1 month ago : Modularizing CMake
47290b68 - Daniel Livingston, 2 years, 1 month ago : Updates to build
8bd255df - Daniel Livingston, 2 years, 1 month ago : Working build - Clang C/GNU Fortran
d0342330 - Daniel Reece Livingston, 2 years, 1 month ago : Compilation working with CMakeLists
d3938f14 - Daniel Reece Livingston, 2 years, 1 month ago : Changing build to CMake
5943e3ea - Daniel Livingston, 2 years, 1 month ago : Removed filename conflicts
92f32c15 - Terry Miller, 2 years, 1 month ago : Add test of gocad files used by dfnWorks and SFWD Jewelsuite projects Includes fixes to attribute names and result reporting
56dc0b2d - Terry Miller, 2 years, 1 month ago : remove gmv files, comment dump commands, update to V3.33
5c22b429 - Terry Miller, 2 years, 1 month ago : remove lagrit output files
0a0bd306 - Terry Miller, 2 years, 1 month ago : move from level03 to level01 to ensure testing for this common command removed binary files and update to V3.33
0584bb21 - Terry Miller, 2 years, 1 month ago : Merge branch 'master' of https://github.com/lanl/LaGriT
1ee072a7 - Terry Miller, 2 years, 1 month ago : fix table
fa279afd - Terry Miller, 2 years, 1 month ago : link refine
60a4b4a0 - Terry Miller, 2 years, 1 month ago : tree_to_fe explained
6c2ae8c9 - Terry Miller, 2 years, 1 month ago : Update reference to V3.33
0a1e57ae - Terry Miller, 2 years, 2 months ago : removed extra directories and files, update README
f75d25d3 - Terry Miller, 2 years, 2 months ago : move write_exo from level01 to level02 because it depends on exo libs. update reference to V3.33
65a9a078 - Terry Miller, 2 years, 2 months ago : heavily modified input.lgi with improved syntax and added comments update reference to V3.33 release and newer exodus 7 versions
90847269 - Terry Miller, 2 years, 2 months ago : replace gmv with avs files, update reference to V3.33 update reference exo from api_version = 5.22f to 7 block ids no longer have 0000 so 1 is 1 instead of 10000
99894ca2 - Terry Miller, 2 years, 2 months ago : final clean-up for V3.33 release
c31c20c1 - Terry Miller, 2 years, 2 months ago : remove old text for old versions of test suite
896124f0 - Terry Miller, 2 years, 2 months ago : remove extra files and update reference to V3.33
5f6d63be - Terry Miller, 2 years, 2 months ago : remove extra files and update reference to V3.33
4a0a49ec - Terry Miller, 2 years, 2 months ago : update reference to V3.33
c9b77654 - Terry Miller, 2 years, 2 months ago : update reference to V3.33
54b8ad9b - Terry Miller, 2 years, 2 months ago : removed unused portion of test and moved to level03/sort as it depends on platform and seed value and checks are visual this test should now be independent of platform update reference to V3.33
97eff5ed - Terry Miller, 2 years, 2 months ago : remove extra files, replace gmv with avs files update reference to V3.33
865b5576 - Terry Miller, 2 years, 2 months ago : replace gmv files with avs and updae to V3.33
b7581990 - Terry Miller, 2 years, 2 months ago : rename files with input and output prefix, update reference to V3.33
4e71e29d - Terry Miller, 2 years, 2 months ago : replace gmv with avs and update reference to V3.33
763ad594 - Terry Miller, 2 years, 2 months ago : remove extra files and update reference to V3.33
accf1367 - Terry Miller, 2 years, 2 months ago : replace gmv with avs and update reference to V3.33
fc452558 - Terry Miller, 2 years, 2 months ago : update reference to V3.33
4c635660 - Terry Miller, 2 years, 2 months ago : replace gmv with avs and update reference to V3.33
4cd843fa - Terry Miller, 2 years, 2 months ago : replace gmv with avs, update reference to V3.33
c431e1c4 - Terry Miller, 2 years, 2 months ago : update reference to V3.33
bd0a0994 - Terry Miller, 2 years, 2 months ago : replace gmv with avs, rename test with input_ update reference V3.33
a083d123 - Terry Miller, 2 years, 2 months ago : Merge branch 'master' of https://github.com/lanl/LaGriT
4e971f86 - Terry Miller, 2 years, 2 months ago : update reference to V3.33
c32ca5e8 - Terry Miller, 2 years, 2 months ago : rename written files to start with "out_" so they can be cleaned update reference to V3.33
f5b65bcb - Terry Miller, 2 years, 2 months ago : update reference to V3.33
018e7031 - Daniel Livingston, 2 years, 2 months ago : Added some debugging params
52bed9ee - Terry Miller, 2 years, 2 months ago : Merge branch 'master' of https://github.com/lanl/LaGriT
a506c819 - Daniel Livingston, 2 years, 2 months ago : Added macOS to GitHub Actions
9b03dd87 - Terry Miller, 2 years, 2 months ago : remove gmv and update reference to V3.33
fc287904 - Daniel Livingston, 2 years, 2 months ago : Fixing GitHub Actions CI
919de121 - Terry Miller, 2 years, 2 months ago : remove extra files, add intersect_elements, fix warnings update reference to V3.33
8beadf5d - Terry Miller, 2 years, 2 months ago : minor adjustment to input.lgi and outx3dgen in reference
887ce884 - Terry Miller, 2 years, 2 months ago : remove many extra files unneeded for this test update to V3.33
38342309 - Terry Miller, 2 years, 2 months ago : replace binary gmv with ascii avs, update reference to V3.33
dd0d090b - Terry Miller, 2 years, 2 months ago : replace all gmv output with single avs files update reference to V3.33
7e266c38 - Terry Miller, 2 years, 2 months ago : remove output files from test directory
b7a1bbda - Terry Miller, 2 years, 2 months ago : remove gmv, update reference to V3.33
93072d4a - Terry Miller, 2 years, 2 months ago : remove gmv files, rename output files, update to V3.33
edf2989f - Terry Miller, 2 years, 2 months ago : remove gmv files, rename written files to "output", remove extra files update reference to V3.33
e467ec90 - Terry Miller, 2 years, 2 months ago : remove extra files, avs vector no longer writes (invalid) file modified input.lgi to remove before writing avs file removed gmv files
3f6ae594 - Terry Miller, 2 years, 2 months ago : change output files to start with name "output"
3287f0f8 - Terry Miller, 2 years, 2 months ago : remove gmv and extra files, update reference to V3.33
47b0c428 - Terry Miller, 2 years, 2 months ago : remove gmv files and update reference to V3.33 test
e127dcda - Terry Miller, 2 years, 2 months ago : remove hard-wired dump/gmv/polygon.gmv and leave polygon.inp
ebdc9315 - Daniel Livingston, 2 years, 2 months ago : Update Makefile
c0c31d25 - Terry Miller, 2 years, 3 months ago : GOCAD keywords
26c829f9 - Terry Miller, 2 years, 3 months ago : update gocad examples
e10c7641 - Terry Miller, 2 years, 3 months ago : edit
647f735d - Terry Miller, 2 years, 3 months ago : release notes for V3.3.3
a872714f - Terry Miller, 2 years, 3 months ago : renamed read gocad test files to have input_ for file names
6dae464f - Terry Miller, 2 years, 3 months ago : test directory for read/gocad 2D and 3D files
a1792928 - Terry Miller, 2 years, 3 months ago : Merge branch 'master' of https://github.com/lanl/LaGriT
af1509f1 - Terry Miller, 2 years, 3 months ago : added missing input*inp for level01/quality
fdcda231 - Terry Miller, 2 years, 3 months ago : Update reference to include new warning for mo with 0 elements
b5c61765 - Daniel Livingston, 2 years, 3 months ago : Delete .travis.yml
f4864bb8 - Terry Miller, 2 years, 3 months ago : Changed ipolydat default to no for writing gmv files
1f9c813c - Terry Miller, 2 years, 3 months ago : Fixed over reporting, buffer issues, better file handling for GOCAD files #222 and #204
1e8f8468 - Terry Miller, 2 years, 3 months ago : Added check to avoie 0 element errors from recon #228
4373c8d7 - Terry Miller, 2 years, 3 months ago : Update lagrit.h to release V3.3.3 October 8 2021
e2c20f9a - Terry Miller, 2 years, 3 months ago : add ZVD ref
6decf3e6 - Terry Miller, 2 years, 4 months ago : fix center
952c7375 - Daniel Reece Livingston, 2 years, 8 months ago : Fixed PyLaGriT warnings
3542b8c2 - Daniel Reece Livingston, 2 years, 8 months ago : Fixed failing test cases for CI
bf7fd833 - Daniel Reece Livingston, 2 years, 8 months ago : Used Black formatting on PyLaGriT files
a4fc5ee5 - Daniel Reece Livingston, 2 years, 8 months ago : Used Black formatting on test Python files
61d1d101 - Daniel Reece Livingston, 2 years, 8 months ago : Changed makefile flag to work with gcc > 10 and < 10
af72ab1e - Daniel Livingston, 2 years, 8 months ago : Update GH Action to remove update-defaults
0e9ed448 - Daniel Livingston, 2 years, 8 months ago : Update test-lagrit.yml
9277a257 - Daniel Livingston, 2 years, 8 months ago : Added new GitHub Actions status badge to repo
b039a67b - Daniel Livingston, 2 years, 8 months ago : Added GitHub Actions for testing LaGriT
3ca8d468 - Daniel Livingston, 2 years, 8 months ago : Changed PyLaGriT setup to setuptools from distutil
03666b6c - Terry Miller, 2 years, 8 months ago : links
a59d2a6d - Terry Miller, 2 years, 8 months ago : fix gmv link
8b6ebcb7 - Terry Miller, 2 years, 8 months ago : add links
50584497 - Daniel Reece Livingston, 2 years, 9 months ago : Added std=legacy flag to remove errors on newer gfortran compilers
```
 
