LaGriT V3.3.3 Release Notes
====================

This page describes version updates for V3.3.3 release. This release updates code to prepare for major capabiltiy improvements planned for next year. This release includes minor bug
fixes, error reporting, and improvements to the LaGriT test suite and builds using cmake. See Issues for a full list.

Previous release  LaGriT v3.3.2  Apr 16, 2019 


### Enhancements:


- New C++ routines and Fortran drivers for poisson disk capabilities under development. See command createpts/poisson_disk
- New files and additions to cmake files to interface C++ codes to lagrit fortran routines.
- read/gocad for 3D .so and 2D .ts files. See test/level01/read_gocad for file examples.
- AVS UCD pnt format added to make it easy for Paraview to read and view points.
- GMV option ipolydat = no is now the default, files will have smaller size without voronoi polygons included
- add option to command line to define output file names
- improvements to PyLaGriT scripts in support of stacked surfaces.
- The manual and command pages were heavily edited to remove old directories and files,
to update command syntax, and to add examples and demos.
- bug fixes mainly to improve lagrit reporting and error reporting.
- test scripts and organization improved, level01 (always), level02 (for TPLs), level03 (dev only)

### Major Change in Build 

This release includes new build scripts and associated files that use cmake to compile, build, and test LaGriT based on the OS platform. 

These cmake scripts do not always use the latest features, instead favoring readability and straightforward statements to help with debugging and troubleshooting. The cmake and install scripts are commented with suggestions and the build documentation has added details for all steps. See README.md and links on that page.

- Cmake is controlled by CMakeLists.txt and settings found in /cmake files.
- The LaGriT build is fairly simple without ExodusII and is suggested if you do not need to write exodus files.
- For issues with exodus builds, see added documentation and comments in install install-exodus.sh

### Known Issues:

- This release compiles with GNU Fortran 7.5 compiler, but may exit on exceptions not caught in old portions of code.
Avoid this by using the flag -ffpe-trap=invalid,zero,overflow,underflow,denormal

- Builds with compiler gfortran v5 or greater can generate run-time signal exceptions such as IEEE_DNORMAL. Most exceptions have been tracked down and fixed according to picky compiler suggestions. These exceptions have not changed the behavior of the code. Exceptions will continue to be fixed as they show up.

- During testing it was found that the filter command may find and remove fewer nodes than the command filterkd. The command filterkd is recommended where precision might be an issue. Documentation will be changed to reflect this. It is expected that the filter command and algorithm will be deprecated and replaced by filterkd. See *test/level01/createpts_filter*


### Commit log for Daniel's branch merge

```
You can view, comment on, or merge this pull request online at:

  https://github.com/lanl/LaGriT/pull/237

Commit Summary

5943e3e Removed filename conflicts
d3938f1 Changing build to CMake
d034233 Compilation working with CMakeLists
8bd255d Working build - Clang C/GNU Fortran
47290b6 Updates to build
c8bd8bc Modularizing CMake
2db46e7 Changed Fortran preproc settings for CMake 3.11
35bdf6c Working static and dynamic linkage
01dd9c2 Much improved CMake installation and some automatic Exodus lib handling
93e52c4 Updated LaGriT CMake
ab55fc9 Updated build
51c9420 Updated CI
08405da Added auto-push compiled to CI
cefb5d9 Exous scripts
900cdc4 Update test-lagrit.yml
11fbc6d Update GitHub CI
fe3b42e Update Python version for CI
cd71aeb Some bug fixes
6b7d86f Improvements to Exodus build
234235c Fixed CI Python issue
0d46198 Fixed missing gfortran in macOS
44db221 Testing LaGriT
b869bbc CMake now writes mm2000.h
bb3964c Beginning CMake writeup
4f112c7 Updated CMake significantly
dccc97e Update GitHub CI with compilers
8d51b78 Attempt to update GitHub Actions again
164b7bf Added unittest testing
7a9e341 More modernized test suite
a720479 Clean up test cases
7dc091d Merge branch 'master' of github.com:lanl/LaGriT into RC-v3.3.3
6ab71b8 Added test info to readme
3413da7 Update README:
b437872 Release cleanup
1d80002 Remove extra file
3d39f90 Update test readme
869ff33 Fixed test suite for quadquality test
File Changes  (66 files)
M .github/workflows/test-lagrit.yml (70)
M .gitignore (15)
A CMakeLists.txt (203)
M README.md (88)
A cmake/CompilerFlags-C.cmake (25)
A cmake/CompilerFlags-Fortran.cmake (8)
A cmake/DetectBitSize.cmake (24)
A cmake/PlatformSettings.cmake (24)
A cmake/README.md (35)
A cmake/modules/FindExodus.cmake (27)
A cmake/modules/FindNetCDF.cmake (30)
A examples/liblagrit/Makefile (5)
A examples/liblagrit/lagrit.h (14)
A examples/liblagrit/main.cxx (20)
A install-exodus.sh (44)
R lg_util/src/mm2000.h.in (12)
D lg_util/src/mm2000_m32.h (76)
D lg_util/src/mm2000_m64.h (75)
M lg_util/src/opsys.h (63)
M src/dumpexodusII.f (8)
M src/excre_wrapper.c (5)
M src/exo_init_ext.c (4)
M src/exo_put_sets.c (4)
D src/lagrit.h (32)
R src/lagrit.h.in (25)
D src/lagrit.template.h (31)
D src/lagrit_mac.h (35)
M src/lagrit_main.f (46)
D src/lagrit_win.h (33)
A src/user_sub.f (45)
M src/writinit.f (125)
M test/README (19)
A test/level01/quad_quality/test03.inp (348)
A test/level01/quad_quality/test04.inp (348)
A test/level01/quad_quality/test05.inp (348)
D test/level03/read_3token/input.lgi (16)
D test/level03/read_3token/reference/input.lgi (16)
D test/level03/read_3token/reference/logx3dgen (12)
D test/level03/read_3token/reference/outx3dgen (132)
D test/level03/read_3token/reference/test.AvS (28)
D test/level03/read_3token/reference/test.InP (28)
D test/level03/read_3token/reference/test.LaGriT (0)
D test/level03/read_3token/reference/test.avs (28)
D test/level03/read_3token/reference/test.gmv (76)
D test/level03/read_3token/reference/test.inp (28)
D test/level03/read_3token/reference/test.lg (0)
D test/level03/read_3token/test.AvS (28)
D test/level03/read_3token/test.InP (28)
D test/level03/read_3token/test.LaGriT (0)
D test/level03/read_3token/test.avs (28)
D test/level03/read_3token/test.gmv (76)
D test/level03/read_3token/test.inp (28)
D test/level03/read_3token/test.lg (0)
D test/lg_test_lib/__init__.py (100)
D test/lg_test_lib/clean_test.py (52)
D test/lg_test_lib/run_test.py (245)
A test/runtests.py (58)
D test/suite.py (181)
A test/testlagrit/__init__.py (5)
R test/testlagrit/check_test.py (0)
A test/testlagrit/diff.py (459)
A test/testlagrit/find_lagrit.py (41)
A test/testlagrit/helper_functions.py (40)
A test/testlagrit/logger.py (5)
A test/testlagrit/run_lagrit.py (58)
A test/testlagrit/run_test.py (14)
Patch Links:

https://github.com/lanl/LaGriT/pull/237.patch
https://github.com/lanl/LaGriT/pull/237.diff
```



### commit log:

```
git log --pretty=format:"%h - %an, %ar : %s" --since=3.years
a872714f - Terry Miller, 3 days ago : renamed read gocad test files to have input_ for file names
6dae464f - Terry Miller, 3 days ago : test directory for read/gocad 2D and 3D files
a1792928 - Terry Miller, 3 days ago : Merge branch 'master' of https://github.com/lanl/LaGriT
af1509f1 - Terry Miller, 3 days ago : added missing input*inp for level01/quality
fdcda231 - Terry Miller, 3 days ago : Update reference to include new warning for mo with 0 elements
b5c61765 - Daniel Livingston, 7 days ago : Delete .travis.yml
f4864bb8 - Terry Miller, 7 days ago : Changed ipolydat default to no for writing gmv files
1f9c813c - Terry Miller, 10 days ago : Fixed over reporting, buffer issues, better file handling for GOCAD files #222 and #204
1e8f8468 - Terry Miller, 10 days ago : Added check to avoie 0 element errors from recon #228
952c7375 - Daniel Reece Livingston, 6 months ago : Fixed PyLaGriT warnings
3542b8c2 - Daniel Reece Livingston, 6 months ago : Fixed failing test cases for CI
bf7fd833 - Daniel Reece Livingston, 6 months ago : Used Black formatting on PyLaGriT files
a4fc5ee5 - Daniel Reece Livingston, 6 months ago : Used Black formatting on test Python files
61d1d101 - Daniel Reece Livingston, 6 months ago : Changed makefile flag to work with gcc > 10 and < 10
af72ab1e - Daniel Livingston, 6 months ago : Update GH Action to remove update-defaults
0e9ed448 - Daniel Livingston, 6 months ago : Update test-lagrit.yml
9277a257 - Daniel Livingston, 6 months ago : Added new GitHub Actions status badge to repo
b039a67b - Daniel Livingston, 6 months ago : Added GitHub Actions for testing LaGriT
3ca8d468 - Daniel Livingston, 6 months ago : Changed PyLaGriT setup to setuptools from distutil
50584497 - Daniel Reece Livingston, 6 months ago : Added std=legacy flag to remove errors on newer gfortran compilers
ebe1d250 - Dylan Robert Harp, 10 months ago : Fixed bug in PyLaGriT addatt_voronoi_varea
825525b7 - Dylan Harp, 11 months ago : Added addatt_voronoi_varea method to pylagrit
dfb07abc - Daniel Livingston, 11 months ago : Merge pull request #212 from keurfonluu/master
7d36db6a - Dylan Harp, 11 months ago : PyLaGriT: change trun option in stack to trunc
7561fb46 - Keurfon Luu, 11 months ago : remove duplicate method intersect_elements
13b8f1c2 - Keurfon Luu, 11 months ago : add argument attr_name in function eltset_object
00f98ed7 - Terry Miller, 11 months ago : Add demo for creatpts/interp, extract, and stack
b54e70a8 - Daniel Livingston, 1 year ago : Updates to work on Ubuntu 18+
644835af - Terry Miller, 1 year, 2 months ago : Finish removing obvious extra files and old copies.
5a7f155b - Terry Miller, 1 year, 2 months ago : Remove duplicate old versions of files
bbf78f9b - Terry Miller, 1 year, 2 months ago : Remove many duplicate files, LaGriT/docs/pages/docs were mostly old versions of files that are found under LaGriT/docs/pages/docs/commands Scripts for finding duplicate files are in LaGriT/docs/edit_scripts
6a4601bc - Terry Miller, 1 year, 2 months ago : Add truncated brick page
08f8d698 - Terry Miller, 1 year, 2 months ago : Add Example for Truncated brick mesh
9d69b24f - Terry Miller, 1 year, 2 months ago : change to accept Title as tag
0de1b979 - Terry Miller, 1 year, 2 months ago : Update site_map, still need to deal with unused files
d8b0d27f - Terry Miller, 1 year, 3 months ago : Remove dump/avs from test as it is incorrect and does not impact test of normals
a8691fb7 - Terry Miller, 1 year, 3 months ago : Add 3D TETRA and properties as attributes
d0b8cd25 - Terry Miller, 1 year, 3 months ago : add excavate words
fd70fc4e - Terry Miller, 1 year, 4 months ago : add readatt example
4245198e - Dylan Robert Harp, 1 year, 4 months ago : PyLaGriT: Added dpinchout_opt to stack_layers method
596c2d65 - Daniel Livingston, 1 year, 5 months ago : Update regions_points_to_tet.md
ebeafb71 - Daniel Livingston, 1 year, 6 months ago : Merge pull request #196 from banesullivan/remove-pexpect
e3541017 - banesullivan, 1 year, 6 months ago : Add numpy as requirement
eb3b816b - banesullivan, 1 year, 6 months ago : Line endings
be5a668c - banesullivan, 1 year, 6 months ago : Remove unused imports
20ff99e6 - banesullivan, 1 year, 6 months ago : Remove pexpect from package
e0b64fe6 - Daniel Livingston, 1 year, 7 months ago : Minor installation doc fixes
473a8440 - Dylan Harp, 1 year, 7 months ago : Added development team to pylagrit documentation
b26663b0 - Dylan Harp, 1 year, 7 months ago : Added needed inp file for stratigraphic hex mesh pylagrit example
ce073c2f - Dylan Harp, 1 year, 7 months ago : Added link to github file for stratigraphic example
f0a02f45 - Dylan Harp, 1 year, 7 months ago : Maybe this time for PyLaGriT docs
1d3d256d - Dylan Harp, 1 year, 7 months ago : Adding PyLaGriT doc static folder files
5fa049cd - Dylan Harp, 1 year, 7 months ago : Trying different pylagrit doc folder
5fac1f9e - Dylan Harp, 1 year, 7 months ago : Hopefully last PyLaGriT doc fix
5f257970 - Dylan Harp, 1 year, 7 months ago : Renaming PyLaGriT doc files
9ff7071d - Dylan Harp, 1 year, 7 months ago : Added new PyLaGriT doc files
2fa9ad3f - Dylan Harp, 1 year, 7 months ago : Updated PyLaGriT documentation to include stratigraphic hex mesh tutorial
517f3870 - Dylan Harp, 1 year, 7 months ago : Added stratigraphic hesh mesh tutorial example from Guoyan Jiang <gyjiang@whu.edu.cn>
1afdeac4 - Dylan Harp, 1 year, 7 months ago : Added rmmat, rmmat_node, and rmmat_element methods to MO class
787e8d14 - Dylan Robert Harp, 1 year, 7 months ago : Merge branch 'master' of https://github.com/lanl/LaGriT
74966b9b - Dylan Robert Harp, 1 year, 7 months ago : Merge remote-tracking branch 'refs/remotes/origin/master'
9c81f58e - Dylan Robert Harp, 1 year, 7 months ago : Added add_node_attribute and add_element_attribute
79f304d8 - jbeisman, 1 year, 7 months ago : removed obsolete example; added region command example
806c42c2 - jbeisman, 1 year, 7 months ago : added setpts method to PyLaGriT
102578b7 - jbeisman, 1 year, 7 months ago : added regnpts command to PyLaGriT
62bc5401 - jbeisman, 1 year, 7 months ago : PyLaGriT: refactored surface and region commands; added mregion and rmregion commands
3dc89c70 - jbeisman, 1 year, 7 months ago : added eltset write command to PyLaGriT
58564eb2 - jbeisman, 1 year, 7 months ago : added dump / pflotran command to pylagrit
fb8cb2bd - Dylan Robert Harp, 1 year, 7 months ago : PyLaGriT arctic_2D example fixes
132f567b - Daniel Livingston, 1 year, 7 months ago : Merge pull request #192 from lanl/livingston/fehmn_fix
a8e5d26b - Daniel Reece Livingston, 1 year, 7 months ago : Formatting fix for #191
724a3de7 - Dylan Robert Harp, 1 year, 8 months ago : Fixes to pylagrit example ex_rotate_stack.py
fc2863b9 - jbeisman, 1 year, 8 months ago : changed name of function argument in createpts_brick methods from ctr to vc_switch for consistency with other createpts methods
0a8846dc - jbeisman, 1 year, 8 months ago : final commit to fix spacing issues in FEHM spherical .stor file
802ac04d - jbeisman, 1 year, 8 months ago : removed unnecessary flag from Pylagrit utilities
fb637ee0 - jbeisman, 1 year, 8 months ago : fixed spacing in FEHM spherical stor file utility
5fac112f - Daniel Reece Livingston, 1 year, 8 months ago : quadxy fix for line that spans 80 chars
9357b75b - jbeisman, 1 year, 8 months ago : Added node vertex/cell center switch argument to several PyLaGriT createpts methods. Fixes #190
0999eb39 - jbeisman, 1 year, 8 months ago : changed FEHM util filename from .inp to .fehmn; added 'stop' statement at end of fehmn file
8ef6831b - jbeisman, 1 year, 8 months ago : created new file with standalone pylagrit utilities for FEHM spherical simulations
418fced3 - Terry Miller, 1 year, 8 months ago : clarify connect
56152eb2 - jbeisman, 1 year, 8 months ago : Merge branch 'master' of github.com:lanl/LaGriT
ac74ef36 - jbeisman, 1 year, 8 months ago : added 'quadxyz' command to pylagrit; minor cleanup of 'quadxy' command
16114566 - jbeisman, 1 year, 8 months ago : one line correction in pylagrit description of set_id
2da50668 - Terry A Miller, 1 year, 9 months ago : remove not_on_website directory with junk files
5f5d388c - Terry A Miller, 1 year, 9 months ago : Merge branch 'master' of https://github.com/lanl/LaGriT
6e3bcacc - Daniel Livingston, 1 year, 9 months ago : Remove information on using apt-get Exodus
18f411fc - jbeisman, 1 year, 9 months ago : added one line to make pylagrit example complete
96a78684 - Terry A Miller, 1 year, 9 months ago : Add face and node set for general use
6ed16ffe - jbeisman, 1 year, 9 months ago : Merge branch 'master' of github.com:lanl/LaGriT
993f6bb3 - jbeisman, 1 year, 9 months ago : added cmo/set_id command to PyLaGrit
6a9e070e - Terry A Miller, 1 year, 9 months ago : Add examples for writing node sets and facesets
9ade8f50 - Terry A Miller, 1 year, 9 months ago : Fix some links and images on extract/surfmesh
298a55b5 - Terry A Miller, 1 year, 9 months ago : Add demo for extract surfmesh based on user issues
2f5bea0e - Daniel Livingston, 1 year, 9 months ago : Travis update to fix brew errors
4f9bc1ec - Daniel Livingston, 1 year, 9 months ago : Minor - fix commandi.md web compile
12ea3531 - Terry A Miller, 1 year, 10 months ago : Add breaks to the keywords after parameter lists
4a744a17 - Terry A Miller, 1 year, 10 months ago : Add spaces to table cells
cae4be13 - Terry A Miller, 1 year, 10 months ago : Bug in dump/avs for writing vector attribute, AVS will exit now. Update to documentation.
00cd5afe - Terry A Miller, 1 year, 10 months ago : Add zq to command list
26b0d663 - Terry A Miller, 1 year, 10 months ago : Finish format and commands ending with ZQ
34d96501 - Terry A Miller, 1 year, 10 months ago : Format and fixes up to command UPSCALE
99cffbc0 - Terry A Miller, 1 year, 10 months ago : Format and fixes for commands up to TRANS
d0dd48d4 - Terry A Miller, 1 year, 10 months ago : format and fixes to STACK and examples
86a29986 - Terry A Miller, 1 year, 10 months ago : format and fix SORT.md change all "lagrit_input references to demos/input/lagritwith.txt remove height and leave width for img to avoid distorted images
0bbadde7 - Terry A Miller, 1 year, 10 months ago : Major redo of SMOOTH command page. Needs a good read through.
3b6d7c66 - Terry A Miller, 1 year, 10 months ago : format and fixes up to command settets
045e4e01 - Terry A Miller, 1 year, 10 months ago : format and edits up to RZV commands
f0a702cc - Terry A Miller, 1 year, 10 months ago : format and fix createpts
d65865eb - Terry A Miller, 1 year, 10 months ago : Format and fix command pages through ROTATELN
015a8314 - Terry A Miller, 1 year, 10 months ago : Formats and edits up to command RM
c5f1a067 - Daniel Livingston, 1 year, 10 months ago : Update REFINE1.md
4b44437d - Daniel Livingston, 1 year, 10 months ago : Update REFINE1.md
159c6f77 - Daniel Livingston, 1 year, 10 months ago : Increase spacing between lists
c7f1c4bc - Terry A Miller, 1 year, 10 months ago : format and edits and fixed links up to REFINE
1e18145f - Terry A Miller, 1 year, 10 months ago : formats and fixes up to refine command
b242c99d - Terry A Miller, 1 year, 10 months ago : Formats and images for Q commands
191370a9 - Terry A Miller, 1 year, 11 months ago : Format and fix commands and add missing images for QUADXY and QUADXYZ
4888f768 - Terry A Miller, 1 year, 11 months ago : formats up to Q commands add missing files for quadxy
c5e23a56 - Terry Miller, 1 year, 11 months ago : extensive clean-up
e179fdd8 - Terry A Miller, 1 year, 11 months ago : Merge branch 'master' of https://github.com/lanl/LaGriT
0ac86c02 - Daniel Livingston, 1 year, 11 months ago : Update uikit_theme.css
646082a4 - Daniel Livingston, 1 year, 11 months ago : Add text-decoration to inline code
981680c2 - Terry A Miller, 1 year, 11 months ago : Merge branch 'master' of https://github.com/lanl/LaGriT
8b4700f4 - Terry A Miller, 1 year, 11 months ago : Update Exodus page and examples
04e7ca48 - Daniel Livingston, 1 year, 11 months ago : Change font settings for inline code
db1013c2 - Terry A Miller, 1 year, 11 months ago : fixed duplicate file cmo_setatt.md
1fd9c819 - Terry Miller, 1 year, 11 months ago : Update github_pages_example.md
b7929b99 - Terry A Miller, 1 year, 11 months ago : Add demo for extract/surfmesh
26c85e6a - Terry A Miller, 1 year, 11 months ago : math command
ca22f8da - Terry A Miller, 1 year, 11 months ago : example index cards alphabetical
e04f9442 - Dylan Robert Harp, 1 year, 11 months ago : Changed check_rc to be consistent with Daniel's changes
495f7e36 - Dylan Harp, 1 year, 11 months ago : Updated print statements in examples folder to python3
8472fffa - Dylan Harp, 1 year, 11 months ago : Fixed problem with pylagritrc file: newer python reads in the quotes as part of the string from pylagritrc file, so they need to be removed
f6e58c56 - Daniel Livingston, 1 year, 11 months ago : Update CRTPTRZRAN.md
9d251942 - Daniel Livingston, 1 year, 11 months ago : Update PSET.md
c3c61411 - Daniel Livingston, 1 year, 11 months ago : Update PSET.md
851ed0d7 - Daniel Livingston, 1 year, 11 months ago : Fix tutorial img paths
82888034 - Terry A Miller, 1 year, 11 months ago : Add pages to examples
20116524 - Terry A Miller, 1 year, 11 months ago : Correct and clarify node ordering for lagrit and AVS
6660bfb1 - Terry A Miller, 1 year, 11 months ago : connect and grid2grid workflow examples and images
a31fff9c - Terry A Miller, 2 years ago : Update connect examples
0e63816e - Daniel Livingston, 2 years ago : Put lgoutput CSS class into footer.css
fbc2bde6 - Terry A Miller, 2 years ago : new lg-output for pre formats
2eba0624 - Daniel Livingston, 2 years ago : Added output-specific styling
a8ea40bf - Terry A Miller, 2 years ago : Merge branch 'master' of https://github.com/lanl/LaGriT
18e4c320 - Terry A Miller, 2 years ago : connect examples
34d393b3 - Daniel Reece Livingston, 2 years ago : Sanitize strings from pylagritrc
df3013c8 - Terry Miller, 2 years ago : fixed syntax for sheet
b643c5fb - Terry Miller, 2 years, 1 month ago : fix broken text and format
7a1a9e9c - Daniel Livingston, 2 years, 1 month ago : Update cmo_setatt.md
a1e9ed5a - Terry Miller, 2 years, 1 month ago : fixed lost characters and format
99789a95 - Daniel Livingston, 2 years, 2 months ago : Update publications.md
212f7309 - Daniel Livingston, 2 years, 2 months ago : Update publications.md
351babe1 - Terry Miller, 2 years, 2 months ago : Update and fix Mesh Object related pages (#175)
9a9d14b8 - Daniel Livingston, 2 years, 2 months ago : Removed tipue input formatting which overrode UIKit
bdd59f5b - Daniel Livingston, 2 years, 2 months ago : Attempt to fix web search
45f53189 - Dylan Robert Harp, 2 years, 2 months ago : Added check for missing lagrit_exe option during init of PyLaGriT
4c3d6978 - Daniel Reece Livingston, 2 years, 2 months ago : Minor doc fixes
8557bf78 - Daniel Reece Livingston, 2 years, 2 months ago : Fixed quality testcase failing
3a5db7c4 - Daniel Livingston, 2 years, 2 months ago : Huge update to website theme + tutorials + examples (#174)
a81a510c - Terry A Miller, 2 years, 2 months ago : Merge branch 'master' of https://github.com/lanl/LaGriT
859ccafb - Terry A Miller, 2 years, 2 months ago : Replace loop through 100000 lines to unlimited number of lines and add some minimal error checking and reporting
53856d73 - Terry Miller, 2 years, 2 months ago : Add example with images
17716155 - Terry A Miller, 2 years, 2 months ago : Merge branch 'master' of https://github.com/lanl/LaGriT
da2e6d0c - Terry A Miller, 2 years, 2 months ago : Test and images for crush_thin_tets Add reporting and fix addatt for tolrelwidth
2c153010 - Terry Miller, 2 years, 3 months ago : format
34ed5a0f - Terry Miller, 2 years, 3 months ago : add crush_thin_tets
2e1de3df - Terry Miller, 2 years, 3 months ago : add line returns
3044874a - Terry Miller, 2 years, 3 months ago : add crush_thin_tets
cd77d3d9 - Terry Miller, 2 years, 3 months ago : Create crush_thin_tets.md file
e35a70b3 - Terry A Miller, 2 years, 3 months ago : Merge branch 'master' of https://github.com/lanl/LaGriT
2767365a - Terry A Miller, 2 years, 3 months ago : Fix bugs in quality reports and update associated test reference files.
4bac42b2 - Terry Miller, 2 years, 3 months ago : minor clarifications
3ceea7ef - Terry Miller, 2 years, 3 months ago : corrected cap coords
0f3bd607 - Terry Miller, 2 years, 3 months ago : clean up
ea2669a5 - Terry Miller, 2 years, 3 months ago : fix format
ddb547d1 - Terry A Miller, 2 years, 3 months ago : Merge branch 'master' of https://github.com/lanl/LaGriT
21cbb8b1 - Terry A Miller, 2 years, 3 months ago : Add recon 0 after call to connect 2D and update test files
8dfb87d9 - Terry Miller, 2 years, 3 months ago : fix link syntax
c64bbb3a - Terry Miller, 2 years, 3 months ago : fix links and images
cef56469 - Terry Miller, 2 years, 3 months ago : add .txt to name
f3b24cb7 - Terry Miller, 2 years, 3 months ago : Link to md
ed912ec5 - Terry Miller, 2 years, 3 months ago : convert pdf notes to md
a4f4f939 - Terry Miller, 2 years, 3 months ago : Add instructions and notes
1f237780 - Terry Miller, 2 years, 3 months ago : formats
43250fde - Terry Miller, 2 years, 3 months ago : title
e3f2f04f - Daniel Livingston, 2 years, 3 months ago : Fixed ZLIB build process for CYGWIN
c5988c02 - U-WIN\r281416, 2 years, 3 months ago : Fixed Exodus build for Win
31bbf69e - U-WIN\r281416, 2 years, 3 months ago : Updated to support Cygwin
23c2a250 - Dylan Harp, 2 years, 4 months ago : Added missing pylagrit doc folders
d8bca565 - Dylan Harp, 2 years, 4 months ago : Fixed pylagrit documentation by file manipulation in Makefile
f462157e - Dylan Harp, 2 years, 4 months ago : Trying to remove .nojekyll from main docs folder
3bb6547f - Dylan Harp, 2 years, 4 months ago : Another .nojekyll attempt
eb913dae - Dylan Harp, 2 years, 4 months ago : Added .nojekyll file to try to bypass jekylls ignoring of underscores
dd9a4331 - Dylan Harp, 2 years, 4 months ago : More pylagrit documenation updates
931896bd - Dylan Harp, 2 years, 4 months ago : More pylagrit documenation updates
de6e546e - Dylan Harp, 2 years, 4 months ago : Updated pylagrit documentation; all member now included in autodoc
c75759ce - Daniel Livingston, 2 years, 4 months ago : Fixed GCC 9.x breaking error
67acd7f1 - Daniel Livingston, 2 years, 5 months ago : Fixed footer text on webpages
1b58651a - Daniel Reece Livingston, 2 years, 6 months ago : Build fix for latest Exodus
c1ab5deb - Daniel Livingston, 2 years, 6 months ago : Updated CMake for Travis
e62675ad - Daniel Reece Livingston, 2 years, 6 months ago : TravisCI fix
74485e8d - Daniel Livingston, 2 years, 6 months ago : Update .travis.yml
dbd8622f - Daniel Livingston, 2 years, 6 months ago : Update .travis.yml
d7b7bb14 - Daniel Livingston, 2 years, 6 months ago : Changing python to python3
ebd1dd35 - Daniel Livingston, 2 years, 6 months ago : Syntax fix for travis
710ca3b0 - Daniel Livingston, 2 years, 6 months ago : Update .travis.yml
4b1fd0c9 - jbeisman, 2 years, 6 months ago : Merge branch 'master' of github.com:lanl/LaGriT
a9e5f2a0 - jbeisman, 2 years, 6 months ago : added pset_attribute functionality to pset class
abbeece4 - Daniel Reece Livingston, 2 years, 6 months ago : New test suite feature - single dir test
dd833d96 - Daniel Reece Livingston, 2 years, 6 months ago : Fixed pset outx file
dbae94f1 - Daniel Reece Livingston, 2 years, 6 months ago : Banner updated to 3.3.2
30c63cbf - Daniel Reece Livingston, 2 years, 6 months ago : Revert "Updated outx3dgen test files"
2d63df5e - Daniel Reece Livingston, 2 years, 6 months ago : Updated outx3dgen test files
08f50e33 - Daniel Livingston, 2 years, 6 months ago : Merge pull request #152 from lanl/revised-test-suite
ace7b0b6 - Daniel Livingston, 2 years, 6 months ago : Merge branch 'master' into revised-test-suite
b7b6a7a2 - Terry Miller, 2 years, 6 months ago : Add missing and updated outx3dgen reference files (#157)
927ef57d - Terry Miller, 2 years, 6 months ago : Merge pull request #156 from lanl/tamiller_dev
cf0e8934 - Terry A Miller, 2 years, 6 months ago : Update to information in outx3dgen files, no change to data results
9fdb5ccd - Terry A Miller, 2 years, 6 months ago : add idebug options and better screen information report invalid elements once instead of for each element
39048bc0 - Daniel Reece Livingston, 2 years, 6 months ago : Ignore build and test artifacts
a6314d56 - Daniel Reece Livingston, 2 years, 6 months ago : Modified and improved test suite
503bcf95 - Terry Miller, 2 years, 6 months ago : Merge pull request #151 from lanl/tamiller_dev
18e96bc7 - Terry A Miller, 2 years, 6 months ago : Update copyright and banner to Triad
0fd9737b - Terry A Miller, 2 years, 6 months ago : Update README for test suite and add hextotet test
715ef5a3 - Terry A Miller, 2 years, 6 months ago : Merge branch 'master' into tamiller_dev interpolate added reporting and debug
2ed4e633 - Daniel Livingston, 2 years, 7 months ago : Update licensing.md
f09e85dc - Daniel Livingston, 2 years, 7 months ago : Update licensing.md
e9ed1988 - Terry Miller, 2 years, 7 months ago : Merge branch 'master' into master
19812035 - Terry A Miller, 2 years, 7 months ago : Merge branch 'master' into tamiller_dev Update docs
1be68b7a - Terry Miller, 2 years, 7 months ago : Millerta docs (#148)
627f0a7e - Terry Miller, 2 years, 7 months ago : add debug (#146)
7b0fa2e6 - Terry Miller, 2 years, 7 months ago : add debug (#145)
69788fd1 - Terry Miller, 2 years, 7 months ago : Fix NaN values in input_tet24.inp (#144)
6625398d - Terry Miller, 2 years, 7 months ago : Interpolation pages (#143)
08773032 - Terry Miller, 2 years, 7 months ago : Merge pull request #1 from lanl/master
149f303c - Terry A Miller, 2 years, 7 months ago : Merge branch 'master' into tamiller_dev Replaced reference test file that had NaN values
369bf9dc - Terry A Miller, 2 years, 7 months ago : Fix NaN values in input_tet24.inp
a4d12cb2 - Daniel Livingston, 2 years, 7 months ago : New interpolate (#141)
e0d7fe37 - Terry Miller, 2 years, 7 months ago : Merge pull request #140 from lanl/tamiller_dev
d7fcef51 - Terry A Miller, 2 years, 7 months ago : protect against calls to aratio routines that have invalid element types added better controls for output messages
1f3e89db - Terry Miller, 2 years, 7 months ago : Merge pull request #139 from lanl/tamiller_dev read_fehm_zone.f
4c2759d0 - Terry A Miller, 2 years, 7 months ago : fixed bug where wrong cmsgin is getting wrong length for option token
9b020a23 - Terry Miller, 2 years, 7 months ago : Add release notes for early versions
e9ffebea - Daniel Reece Livingston, 2 years, 7 months ago : Fixed empty x3d files
a595be49 - Terry Miller, 2 years, 7 months ago : update copyright (#137)
a24a9ee4 - Terry Miller, 2 years, 7 months ago : update copyright
42507ad3 - Daniel Reece Livingston, 2 years, 7 months ago : Fixed single triangle case
c2477a8a - Daniel Livingston, 2 years, 7 months ago : Update main_interpolate.md
c00423c0 - Daniel Livingston, 2 years, 7 months ago : Update main_interpolate.md
6318011f - Daniel Livingston, 2 years, 7 months ago : Update main_interpolate.md
a378b5fb - Terry Miller, 2 years, 7 months ago : clarify connectivity
069b1319 - Terry Miller, 2 years, 7 months ago : require hex or quad cmo
4f14be99 - Terry Miller, 2 years, 7 months ago : requires tet cmo
9b93bd44 - Terry Miller, 2 years, 7 months ago : update descriptions
f58087a3 - Terry Miller, 2 years, 8 months ago : Update copyright to Triad
96cd2b1b - Terry Miller, 2 years, 8 months ago : add missing text
52aa3111 - Daniel Livingston, 2 years, 8 months ago : Update QUALITY_sliver_cap_needle_wedge.md
5ab71364 - Dylan Robert Harp, 2 years, 8 months ago : Added quality methods to pylagrit mesh object class
5094c775 - Dylan Robert Harp, 2 years, 8 months ago : Added smooth method to PyLaGriT PSet class
1f31e5c1 - Daniel Livingston, 2 years, 8 months ago : Pages modifications
298fefaa - Daniel Reece Livingston, 2 years, 8 months ago : Removed dangling print statement
2a2189e9 - Terry Miller, 2 years, 8 months ago : Format and clean up
5b023e15 - Terry Miller, 2 years, 8 months ago : example with pset_name
53fa1350 - Daniel Reece Livingston, 2 years, 8 months ago : Modified information function to return under verbose=0
905f563b - jbeisman, 2 years, 8 months ago : Merge branch 'master' of https://github.com/lanl/LaGriT
e3de111c - jbeisman, 2 years, 8 months ago : Added PyLaGriT functionality for compute function
8dbfc03b - Daniel Reece Livingston, 2 years, 9 months ago : Sanity check for 3 node mesh
2942ca3a - Daniel Reece Livingston, 2 years, 9 months ago : Better handling of floating point exceptions
96e9af97 - Daniel Reece Livingston, 2 years, 9 months ago : Remove print statement in test
cf5751eb - Daniel Reece Livingston, 2 years, 9 months ago : Test suite modifications for new log/out fnames
00d2a232 - Terry Miller, 2 years, 9 months ago : spacing
dc2106f5 - Terry Miller, 2 years, 9 months ago : add sort example
a09fea56 - Daniel Reece Livingston, 2 years, 9 months ago : Merge branch 'master' of https://github.com/lanl/LaGriT
8c0cca34 - Daniel Reece Livingston, 2 years, 9 months ago : Basic command line parsing
faec2d60 - Terry Miller, 2 years, 9 months ago : flip slash
f63ed342 - Terry Miller, 2 years, 9 months ago : format for sort
ed2f22c8 - Daniel Livingston, 2 years, 9 months ago : Added PyLaGriT to testing
87424153 - Daniel Livingston, 2 years, 9 months ago : TravisCI change to upgrade Linux to latest cmake
cdb11e0c - Daniel Reece Livingston, 2 years, 9 months ago : Minor bug fix
7cda0d99 - jbeisman, 2 years, 9 months ago : Added logic to refine_to_object allowing use of level option
2eb5c2eb - jbeisman, 2 years, 9 months ago : Modified quadxy to create and connect in one step and fixed a typo
fde4f1da - jbeisman, 2 years, 9 months ago : Merge branch 'master' of https://github.com/lanl/LaGriT
ba036942 - jbeisman, 2 years, 9 months ago : Changes to pylagrit merge function
40bc9eac - Daniel Livingston, 2 years, 9 months ago : [skip travis] Added troubleshooting to install page
f52183df - Daniel Livingston, 2 years, 9 months ago : Improved LaGriT compilation process (#129)
9b65f591 - Terry Miller, 2 years, 10 months ago : start cleanup on this example page
a6f6e810 - Terry Miller, 2 years, 10 months ago : added new syntax options
55c61a0a - Terry A Miller, 2 years, 10 months ago : Update for avs code changes Merge branch 'master' of https://github.com/lanl/LaGriT
bbbcd560 - Terry A Miller, 2 years, 10 months ago : Add AVS UCD pt format and switch avs2 to default avs Add checks and warnings, fix output for elem attribute files
ce34208f - Terry Miller, 2 years, 10 months ago : att_node and att_elem no longer supported
fc422ef0 - Daniel Reece Livingston, 2 years, 10 months ago : minor bug fix in perturb
83b48222 - Daniel Reece Livingston, 2 years, 10 months ago : New PyLaGriT functions
c4d8df07 - Terry A Miller, 2 years, 10 months ago : Update with master
7bffab91 - Terry A Miller, 2 years, 10 months ago : various images for sphere examples
6ee26993 - Terry Miller, 2 years, 10 months ago : added images
fb116e28 - Terry A Miller, 2 years, 10 months ago : images for createsphere
b37c709e - Terry Miller, 2 years, 10 months ago : Added examples
5cd69ac8 - Terry Miller, 2 years, 10 months ago : corrected markdown (#126)
e0f15c0c - Daniel Livingston, 2 years, 10 months ago : Update SETTETS.md
bcffbd28 - Terry A Miller, 2 years, 10 months ago : small size image for web page
17a9263b - Terry A Miller, 2 years, 10 months ago : Add image for settets normal with 26 colors
bd71ef70 - Terry Miller, 2 years, 10 months ago : Add description and values for settets/normal
df86f23d - Daniel Livingston, 2 years, 10 months ago : Update to examples (#125)
```
 
