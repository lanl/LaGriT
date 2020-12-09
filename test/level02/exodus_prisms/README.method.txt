### Method Description for writing exo for stacked prism with facesets
Original Draft written May 1 2013 tamiller@lanl.gov

See file input.lgi for complete set of commands.

## Stack triangle surfaces into prism mesh
#  This mesh should have itetclr set for exodus cell blocks
#  The node properties are ignored as exodus is cell based
#  Different from FEHM which has properties on nodes

cmo / create / mostack / / / tri
stack / layers / avs / one_tri_z0.inp 1 one_tri_z1.inp 2 one_tri_z2.inp
stack / fill / moprism / mostack
resetpts / itp

#  Check the 3D stack mesh for expected values
quality
cmo / printatt / moprism / -all- minmax

## IDENTIFY FACESETS for top (and other 5 directions) ####

## IMPORTANT ###################
The cells MUST be pre-sorted by itetclr material values
Exodus will reorder elements internally, we do not want the
cells to be re-ordered. If they are, faceset correlation to
the cell numbers will no longer be correct.
Check that facesets are correct by reading the exo file into GMV
and selecting Surfaces under Display.

In dump exodus (internal to lagrit):
dump/exo/prism_one.exo/moprism

ExodusII: Start writing to file: prism_one.exo using cmo: moprism

cmo/set_id/moprism/element/e_num_temp
cmo/addatt/moprism e_num_temp/VINT/scalar/nelements/linear/permanent//0
sort/moprism/index/ascending/ikey_utr/itetclr/e_num_temp
cmo/addatt/moprism/ikey_utr/vint/scalar/nelements///gax/0
 SORT: order key written to attribute: ikey_utr


#--------- BEFORE EXTRACT --------------------------------------
# sort based on itetclr values and cell location
# secondary sort conventions are up to user
# xmed, ymed, zmed will arrange into columns (after itetclr sort)

createpts / median
sort / mohex / index / ascending / ikey / itetclr xmed ymed zmed
reorder / mohex / ikey
  dump / gmv / out_tmp_sort1.gmv / mohex
  cmo / DELATT / mohex / ikey

# sort nodes based on mesh convention z, y, then x
sort / mohex / index / ascending / ikey / zic yic xic
reorder / mohex / ikey
#----------------------------------------------------------------

(Note in this example sort was not used, but itetclr is ordered correctly
during the stack process, so facesets are correct)


# Extract the outside surface of the 3D mesh                                             
  For a prism mesh, the extracted surface will have quads on the sides
  and triangles on the top and bottom. 
  The extracted surface will have the element id and element face number
  that were extracted from the 3D mesh elements
  idelem1 and idface1 are written to faceset files
  with each file grouped into seperate faceset files

extract / surfmesh / 1 0 0 / mo_surf / mo_pri / external
cmo / printatt / mo_surf / -all- / minmax

## Set surface elements to direction they face based on normals
#  These values will be written to itetclr, copy to id_side attribute
#  1 = bottom 
#  2 = top
#  3 = east right
#  4 = north back
#  5 = west left
#  6 = south front

settets / normal
cmo / copyatt / mo_surf mo_surf / id_side itetclr

# check attributes and remove all except idelem1 and idface1

dump gmv prism_outside_faces.gmv mo_surf 

cmo / printatt / mo_surf / -all- / minmax

cmo / DELATT / mo_surf / itetclr0
cmo / DELATT / mo_surf / itetclr1
cmo / DELATT / mo_surf / idnode0
cmo / DELATT / mo_surf / idelem0
cmo / DELATT / mo_surf / facecol
cmo / DELATT / mo_surf / idface0
cmo / DELATT / mo_surf / layertyp


## For each of the 6 values write the faceset file.
   Write AVS format that writes element attribute only

cmo/copy / mo_tmp / mo_surf
cmo/select / mo_tmp
  eltset/ e_top / id_side / eq / NUM_TOP
  eltset/ e_delete / not / e_top
  rmpoint/element / eltset get e_delete
  rmpoint/compress
  cmo/DELATT/mo_tmp/id_side
  dump/ avs2 / FILE_FS_2  / mo_tmp / 0 0 0 2
  cmo/delete / mo_tmp


## After faceset files have been written, write the final files
   that have the mesh materials and the facesets
   Note the file order will detirmine integer value of set in exo
   so have set 1 first, set 2 second, ...

dump / exo / mesh_fsets.exo / mo_pri / / / facesets &
  FILE_FS_1 FILE_FS_2 FILE_FS_3 &
  FILE_FS_4 FILE_FS_5 FILE_FS_6


## Provide single material mesh for user
   Set all materials to 1 and write all files again
cmo setatt mo_pri itetclr 1
cmo setatt mo_pri imt 1
resetpts itp

dump / exo / mesh_fsets_single_mat.exo / mo_pri / / / facesets &
  FILE_FS_1 FILE_FS_2 FILE_FS_3 &
  FILE_FS_4 FILE_FS_5 FILE_FS_6

## CHECK that top and bottom sets each have same number elements in set
   Look at outx3dgen lagrit output and exo file mesh_fsets_ascii.exo

LaGriT outx3dgen:

 THE ELTSET e_bottom                         HAS        200 ELEMENTS            
 THE ELTSET e_top                            HAS        200 ELEMENTS            

## CHECK the dump/exo found the correct number nodes,elements, sets

Title: LAGRIT TO EXODUS                                                         
number of dimension:               3                                            
number of nodes:                 363                                            
number of elements:              400                                            
number of edges:                   0                                            
number of edge blocks:             0                                            
number of element blocks:          2                                            
number of face blocks:             0                                            
number of node sets:               0                                            
number of edge sets:               3                                            
number of element sets:            0                                            
number of side sets:               6                                            
number of face sets:               0                                            
number of node maps:               0                                            
number of edge maps:               0                                            
number of face maps:               0                                            
number of element maps:            0                                            
 
ExodusII: Done writing to file: mesh_fsets.exo using cmo: moprism         

## CHECK exo file has correct number elements, materials,sets
   by converting to ascii so it can be read
   ncdump_lin64 mesh_fsets.exo > mesh_fsets_ascii.exo

From file mesh_fsets_ascii.exo:

        num_nodes = 363 ;
        num_elem = 400 ;
        num_el_blk = 2 ;
        num_side_sets = 6 ;

        num_side_ss1 = 200 ;
        num_side_ss2 = 200 ;
        num_side_ss3 = 20 ;
        num_side_ss4 = 20 ;
        num_side_ss5 = 20 ;
        num_side_ss6 = 20 ;

Element (cell) properties from itetclr (value+'0000')
 eb_prop1 = 10000, 20000 ;

Faceset integer tags (from file order)
 ss_prop1 = 1, 2, 3, 4, 5, 6 ;

elem_ss sets should have the same elem numbers as associated faceset file from lagrit.
Note the side_ss face numbers may be different as they are in exodus scheme

elem_ss6 = 1, 5, 32, 74, 106, 116, 148, 149, 161, 190, 201, 205, 232, 274,
    306, 316, 348, 349, 361, 390 ;

 side_ss6 = 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1 ;

fs_6_front.faceset -----------------------------------------
         0          0          0          2          0
00002  1  1
idelem1, integer 
idface1, integer 
     1   3
     5   3
    32   3
    74   3
   106   3
   116   3
   148   3
   149   3
   161   3
   190   3
   201   3
   205   3
   232   3
   274   3
   306   3
   316   3
   348   3
   349   3
   361   3
   390   3


