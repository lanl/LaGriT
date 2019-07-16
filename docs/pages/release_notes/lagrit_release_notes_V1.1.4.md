---
title: 'LaGriT V1.1.3 and V1.1.4 Release Notes 2005'
---

## LaGriT V1.1.3 and V1.1.4 Release Notes 2005

This combines notes for development work June 2005 and November 2005. These notes are converted from
old pdf files and may contain translation mistakes. See the original pdf for clarification.

<a href="/assets/images/release_notes15.pdf" download> LaGriT V1.1.4 November 2005 </a> PDF Version 

<a href="/assets/images/release_notes14.pdf" download> LaGriT V1.1.3 June 2005 </a> PDF Version 


A summary of the major changes found in this release are listed below. A complete list of
changes is included at the end of this document. Refer to the user manual for a complete
description of the new, enhanced and revised commands.

## New Commands:

**fset** fset/f1/pset,get,p1

face sets are defined as faces belonging to the elements
corresponding to pointsets where the face is on the boundary. In
other words, face sets become entities for applying boundary
conditions.

## Enhanced Commands:

**dump/avs** Added avs2 option witch will output node and element attributes as real or
integer. avs option outputs all node and element attributes as real. Code will test
the max(abs(attribute)) and format integers to use only as many columns as are needed.

**cmo** cmo_command_nosb.f added fillatt option that avoids warnings for attributes that already exist and allows the attribute to be filled 

**dump** dump material_list.f added argument iselect to subroutine call that allows the user to choose a single selected zone value and corrected code to finish writing zone file if no materials are found. 

**dump** **dumpfehm** writedump_nosb added argument imat_select to dump_marerial_list call that enables a single zone to be selected added option for geoFEST output dump/geofest 

**calc_rdist** msgtty added option to call calc_rdist.

**math** math_nosb added min, max options 

**connect** multi_material_nosb add option to test all interface edges if a material interface is crossed. connect/check_interface 

**massage** massage add norecon option that turns off all calls to the reconnection routines. 

**filter** filter_elem_graph mark and optionally delete duplicate elements msgtty_nosb filter/element 


## Code Changes:

```
02/28/05 intersect elements intrp_gtg 
10/12/05 dumpavs_nosb changes needed for avs2 option.  writedump_nosb
10/18/05 readavs extend input file name to 132 characters fix error if number of nodes = 0 and number of elements not zero readdump, extend input file name to 132 characters
readx3d_att,
readnurbs_iges_grid,
read_sheetij,
read_gocad_tsurf
10/26/05 math_sum added significant figures and put name of attribute on output line.
```

## Bug Fixes:

```
07/22/04 dumpgmv_hybrid_nosb readgmv_ascii_nosb 
02/28/05 pset_nosb 
04/06/05 dumpgmv_hybrid_nosb 

implicit none and added code to report progress of the element search. associate imt and others with imtl type names 
Insert code to invert connectivity so that tet and hex are not inside out. Also added simdate and codename keyword to header of ascii GMV files. These keywords are used in the read/gmv code now to detect the new corrected connectivity 
Insert code to invert connectivity so that tet and hex are not inside out. Also added simdate and codename keyword to header of ascii GMV files. These keywords are used in the read/gmv code now to detect the new corrected connectivity 
added imask to shiftr for union of more than one set, fixed logic so that names that start the same will be treated as different, e.g. imtl imtl temp 
fixed bug introduced by inversion of connectivity when outputting tet or hex elements. 

10/03/05 dumpgmv_hybrid_nosb Distinguish between node and element attributes by checking clen of the attribute.
10/12/05 tritri.f Added implicit none and declared logmess correctly.
10/12/05 intersect_elements.f Minor changes to log messages.
11/02/05 translate Avoid crashing if mesh has no nodes.
11/03/05 do_extract_nosb Fix definition of d in ptnorm option
11/03/05 dumpavs_nosb Handles mesh with only element attributes and fixed formats so they are long enough for num elements, num nodes.

03/06/06 isosurface Fixed problem with the case where no elements were created. For example in
the case where a plane is extracted from a 3D object but the plane does not
intersect the 3D object anywhere. In that case a new mesh object was created
that was 2*nnodes and 2*nelementsof the input mesh object but there was
nothing in the output mesh object. Now the output mesh object is created but nnodes=nelements=0.  

```

