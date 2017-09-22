---
title: 'LaGriT Release Notes V3.1'
---

## LaGriT V3.108 July 2016

The was the Last version released under Open Distribution license LA-CC-2012-084 before Open Source.\
This code was tagged in Mercurial as V3.108 and used to start open-source repository on github.

This includes work by summer student Mikita Yankouski with WIN development using Cygwin.
- Added top level python control suite, and standarized level02 output files. See instructions.txt and cmake-script. \
- Files changed for WIN are opsys.h and type_sizes.h to account for win64 and changed define for SIZEOF_LONG

```
        file: opsys.h
         #ifdef win64
         #define FCV_UNDERSCORE
         #define SIZEOF_INT 4
         #define SIZEOF_LONG 8
         #define SIZEOF_VOIDP 8
         #define MAX_UINT 18446744073709551615.00 
         #endif
         
         #ifdef win64
         #define int_ptrsize long long

        file: type_sizes.h
         #ifdef __CYGWIN__
         #define FCV_UNDERSCORE
         #define SIZEOF_INT 4
         #define SIZEOF_LONG 4
         #define SIZEOF_VOIDP 8
         #endif ***

        file: machine_header.h
         #ifdef win64
         #define FCV_UNDERSCORE
         #define SIZEOF_INT 4
         #define SIZEOF_LONG 8
         #define SIZEOF_VOIDP 8
         #endif

        file: Makefile
         ifeq ($(COMPILER), cygwin)
         SUFFC = _cygwin
         FC = /bin/gfortran
         CC = /bin/gcc
         CXX = /bin/c++
         FC90 = /bin/gfortran
         OSTAG = _cygwin
         
         FFLAGS = -fcray-pointer -fdefault-integer-8 -m64 -Dwin64
         FF90FLAGS = -fcray-pointer -fdefault-integer-8 -m64 -Dwin64
         CFLAGS = -m64 -Dwin64 
 ```        

------------------------------------------------------------------------

## LaGriT V3.106 August 2015

Major update to write PFLOTRAN type option stor file and new syntax using Exodus II 6.9 libraries.

*Note: The LaGriT run-time banner shows V3.2 with compile date Aug 2015,
even though it is actually a branch from V3.106.*

### New Features:

-   **dump / pflotran** Writes .uge file for pflotran and is used by the DFN suite of scripts. The deve directory is in */n/swdev/LAGRIT/work/pflotran*. The syntax looks like:

            dump / pflotran / root_name / cmo_name
            dump / pflotran / root_name / cmo_name / nofilter_zero

-  **dump / exo** calls ExodusII new routines changed from V5 to V6. LaGriT command syntax is unchanged.

           http://sourceforge.net/projects/exodusii/files/
           Exodus II 6.09
           HDF5 version 1.8.6
           netcdf-4.1.3

-  *exo block id* modified to input digit instead of *digit*0000. All exodus files are same as Exodus II 5, except for the block id.
    Tests have been updated resulting in the following differences:

        Exodus 6.09:
        <               :api_version = 6.09f ;
        <               :version = 6.09f ;
        ---
        <  eb_prop1 = 1, 2, 3 ;

        Exodus 5.22a:
                       :api_version = 5.22f ;
                       :version = 5.22f ;
        ---
          eb_prop1 = 10000, 20000, 30000 ;

-   **compress_eps** new cmo attribute for stor file allowing user to extend range of ccoef values by setting mesh attribute compress_eps (from default 1e-8). Changing value of compress_epsilon seemed to help loss of coeffs with large aspect ratios.


### These issues were fixed:

-   **dump / stor** corrected bug for 2D grids that overwrite volic with incorrect value if grid is non-planer.
-   **dump / fehm** add space between ns and nelements, increase to i12
-   **read / fehm** fixed seg fault for 0 elem report message by using a,a instead of a in write format.
-   *build ExodusII6 libraries* The following issue was fixed when building static libraries with exodus:

```
These are the external libs used with LaGriT V3.1 As of November 2012
         http://sourceforge.net/projects/exodusii/files/
         Exodus II 5.22a
         HDF5 version 1.8.6
         netcdf-4.1.3
         
Error in Library inclusion order in the following places:
        1. /n/swdev/src/exodusii/exodus-6.09/exodus/cbind/CMakeList.txt 
            Line 284
        2. /n/swdev/src/exodusoo/exodus-6.09/exodus/forbind/CMakeList.txt
            Line 62
            
Solution was to switch ${HDF5_LIBRARY with ${HDF5HL_LIBRARY
    
Linux RHEL Exodus 5 libraries were built in /n/swdev/LAGRIT/VERS_3.100_012_NOV09/build_lagrit/exodus

Build executable for linux:
gfortran -O -Dlinx64 -static -fcray-pointer -fdefault-integer-8 -fno-sign-zero -o mylagrit lagrit_main.o lagrit_fdate.o lagrit_lin64_o_gf4.5.a /n/swdev/LAGRIT/VERS_3.100_012_NOV09/build_lagrit/lg_util/lib/util_lin64_o_gfort4.5.a -L /n/swdev/LAGRIT/VERS_3.100_012_NOV09/build_lagrit/exodus/lin64/lib -lexoIIv2for -lexodus -lnetcdf -lhdf5_hl -lhdf5 -lz -lm -lstdc++

```

----------------------------------

## LaGriT V3.101 November 2013

Note for DFNWorks applications using LaGriT, this version does NOT have the PFLOTRAN file option.
This version of code uses ExodusII 5 routine calls. These are replaced with ExodusII 6 in newer versions.

V3.103 is last version lagrit code using Exodus 5 libs \
V3.104 is new  version lagrit code using Exodus 6 libs

### Enhancements:

-   **read / zone | zone_element** added option zone_element which allows reading of node or element list in FEHM zone or zonn format. Each node or element number found in the list has attribute tagged.

    
### These issues were fixed:

- **addatt**/mo_tri / **unit_area_normal** fixed incorrect zero result and fixed attribute handling so vector array is formed using irank = 3.
- **cmo/addatt/** mo/ **area_normal/xyz/** Result is off by factor of 2, fixed area normal to assign half the cross product (for triangles).
- **synth_norm** fixed handling of attributes. The synthetic normals were creating a dummy attribute not used because offsetsurf is creating x_n_norm y_n_norm z_n_norm on the input cmo. Attribute names are ignored on the command line, added better reporting for this.

------------------------------------------------------------------------

## LaGriT V3.100 November 2012

Major changes to most parts of the code to enable 64 bit compilation and
added external ExodusII 5 libraries to write Exodus basic mesh files.
These are the external libs used with this release.

A few minor changes/fixes include: cmo/copyatt fix copy from node
attribute to elem attribute of equal length. cmo/readatt fix to allow
char in first position. Memory and bug fixes related to 64 bit code
changes. Improved error catching for common routines.

### Enhancements:

- **dump / exo** Now includes netcdf and exodus libs for writing exodus mesh files and reading and writing facesets.

```
        Syntax:
          dump / exo / ifile / cmoname

          Dump exodus files with/without facesets, fast/slow options:
          dump / exo / ifile / cmoname / facesets / on
          dump / exo / ifile / cmoname / facesets / off
          dump / exo / ifile / cmoname / facesets / on file1,file2,...filen
          dump / exo / ifile / cmoname / facesets / off file1,file2,...filen

          write exo pset and eltsets:
          dump / exo / filenam.exo / cmoname / psets / eltsets /
          dump/exo/mesh_07.exo/mo7//eltsets/ &
             facesets bc01.faceset &
             bc02.faceset bc03.faceset bc04.faceset &
             bc05.faceset bc08.faceset bc09.faceset
          dump/exo/mesh_06.exo/mo6/psets// &
             facesets bc01.faceset &
             bc02.faceset bc03.faceset bc04.faceset &
             bc05.faceset bc08.faceset bc09.faceset
```
   
- **dump** 3 token short syntax for dump (avs,gmv,lg,lagrit,ts,exo)
- **extract/surfmesh** Now creates attributes to hold element local face numbers of 3D
    input mesh that occur on either side of output mesh face, idface0
    and idface1. Now copies user-created node-based attributes from
    source.
-  **interpolate**  Changed interpolate to "find" more points on edges this will permit
    nodes to find a nearest edge or point and be "inside" the triangle
    for extreme small or large numbers where epsilon values are
    difficult to evaluate correctly.
-  **massage** Added option for massage to refine based on an attribute field.
```
  Syntax:
          massage / [bisection length/field name] / merge_length / toldamage / ...
```
-  **massage2** Under development massage2 syntax for incremental refinement strategies.
```
  Syntax:
          massage2/ [file name] / [Target Length Scale]/[field name]/ &
             merge_length/toldamage/[tolroughness]/[ifirst,ilast,istride]/ ...
```
-  **math** add modulo and mod options
-  **recon** Code improvements related to recon 0 and recon 1 will result in
    slightly different but better connectivity results.
-  **sort** Added line sort by nodes or elements for creating valid polygons
    that can be read and used by other routines.
```
   Syntax:
          sort / line_graph / cmo / ascending  descending / [key] / [nodes/elements]
```

------------------------------------------------------------------------

Changesets tracked in Mercurial/Trac on ancho.lanl.gov/lagrit


