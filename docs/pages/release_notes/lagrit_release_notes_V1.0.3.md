---
title: 'LaGriT Release Notes'
---

# LaGriT V1.0.3 Release Notes Notes 2001 


This text is converted from old pdf files and may have translation errors.
See original pdf for clarification.


<a href="/assets/images/release_notes9.pdf" download> LaGriT V1.0.3 2001 </a> PDF Version   


A summary of the major changes found in this release are listed below. A complete list of
changes is included at the end of this document. Refer to the user‘s manual for a complete
description of the new, enhanced and revised commands.

## New Commands:

reorder/cmo/sort_key will reorder a the mesh object according to the sort_key. See sort

interpolate 

```
interpolate/map|continuous|voronoi/cmo_sink/attr_sink/ifirst,ilast,istride, /cmo_source/attr_source/[mintie|maxtie]/ [value|p|us1|nearest source,attr/[keepatt|delatt]
```

Enhanced Commands:

extrude 
```
extrude/cmoout/cmoin/interp/layers/range1,range2
```

smooth 
```
smooth/position/network/pset,get,name/number_of_iterations/ weight/[check|nocheck]
```

createpts createpts/voronoi


cmo cmo/set_id/cmoname/both|node|element [attributename1/attributename2]

sort 
```
sort/cmoname/bins /ascending|decending]/[ikey]/in_att

sort/cmoname/index|rank/ ascending|decending]/[ikey]/in,att1 in_att2 in_att3
```

extract 
extract only exterior surface.
```
extract/surfmesh/ifirst,ilast,istride/cm0_out/[cmo_in]/[external]
```

recon 

If checkaxy is specified, then for the case of 2D triangular meshes, we check xy projected areas are positive and larger than epsilona.
```
recon/[0,1]/[toldamage]/[checkaxy] 
```


## Bug fixes


06/06/00 dumpgmv_hybrid use i3.3 format to write 'created' material names.

07/24/00 setsize_nosb test against mbndry_old not mbndry for noop condttion

07/25/00 set_global_nosb set only integer or real (don't Wipe out previously set variable)

07/27/00 lower_d_lg set ioff before using, increase dimension of tmsgout, imsgout

08/16/00 connect ﬁxed memory problems with failure lists

09/08/00 boundary ﬁx several bugs relating to resetting icr values

09/08/00 rotatept ﬁx getting pset if using numeric arguments

09/08/00 rzbrick3 call cmo_get_name before calling cmo_get_info

09/27/00 multi_material_2d_lg missing argument in surftstv calls

10/12/00 pset restore 'eq' as default operation

10/20/00 reﬁne face add ﬁx problems with second pass on 2d refine 

03/23/01 surface ﬁx problem with cone type - parameters saved in wrong place

05/07/01 cer_chain make reﬁne on roughness work for 2d meshes



## Code Improvements

06/08/00 cmo_mesh_type new mesh type triplane like tri but ndimensions_geom=2.

08/25/00 addmesh pyramid use kdtree to ﬁnd matching grid boundaries

09/03/00 pset, eset changed formats so big indices will print

cmo_interpolate implement user option - user must supply user_nterpolation subroutine

mega_error, mega_hessian fix allocation of over large tmp array



## Code Changes


cmo_get_info return itype=4 for pointer retrteval

control_command_lg echo comments to outx3dgen

mm2000 print block in address order

rmpoint ﬁx allocations for temp space to use a better estimate of size and integer type if possible.

refine_face_add skip call to settets if attribute skip settets is =0

rzbrick3, pset, eset filter, hextotet hybrid, reﬁne, readngptet, readngphex, dump_fehm, extract_interface 
remove references to ialias

mmrelblk comment out  warning if block does not exist

cel_chain force exclusive for rivara_truncated reﬁnes

connect,delaunay change epsilons for point insertion tests

statementfunctions.h new function DSZIRTRI caluculates the SIGNED inscribed radius of tri.

refine_face_add use cmo_interpolate values for coordinates of new points

recon all recon commands processed through subroutine recon, toldamage now computed if not supplied

reﬁne We now pass psetname= -def- in the case of 2D with no surface.  In this case the entire 2-D grid will be in the ‘pset' for refinement

cer_chain We commented out the RECON after reﬁnement.

agd3d We now refrain from merging out nodes if they would create a roughness>0.8*TOLROUGHNESS.

getgsynth Initial revision. Computes synthetic normals for ALL nodes in 2-D

massage  We now take TOLROUGHNESS in the argument list. This is a format change, but old decks should still work.




