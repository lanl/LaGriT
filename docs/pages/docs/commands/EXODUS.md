---
title: dump exo 
tags: dump, exo, exodus, sort 
---

# DUMP / EXO

Write a mesh object to a file in the ExodusII format. 
This command can be used to define ExodusII blocks (itetclr), node sets (psets), element sets (eltsets), and side sets (facesets).
The facesets are commonly used to define faces on the external mesh to define boundary conditions.

The command [**`extract/surfmesh`**](docs/commands/dump/EXTRACT_SURFMESH.md) can be used to extract the mesh boundary surface and create the faceset attributes to define in an ExodusII mesh file.


NOTE: ExodusII requires that elements are ordered by their block ID (element itetclr value). 
ExodusII routines will automatically sort by these values, so we pre-sort before calling **`dump/exo`** 
so that element and node numbering is consistent with any set definitions.
Both Paraview (sets) and GMV (surfaces) can be used to view ExodusII mesh files with face sets.

See Examples below for more on writing ExodusII files with LaGriT.


## SYNTAX
<pre>
<b>dump/exo</b>/file_name/cmo_name 

<b>dump/exodusii</b>/file_name/mo_name 

<b>dump/exo</b>/file_name/mo_name [<b>psets</b>] / [<b>eltsets</b>] / [<b>facesets</b> file1 file2 ... filen ]
</pre>

`file_name` and `cmo_name` are the names of the ExodusII file and mesh object to write.

**`psets`** Dump ExodusII file with vertex sets.
The keyword pests as token 5 will cause all psets (lists of vertex numbers) associated with the mesh object to be written to the ExodusII output file. 

**`eltsets`** Dump ExodusII file with element sets.
The keyword eltsets as token 6 will cause all eltsets (lists of cell numbers) associated with the mesh object to be written to the ExodusII output file. 


**`facesets`** `file1 file2 ... filen`
If face set information is being provided from files (file1 file2 ... filen) the format of the file is written in AVS UCD cell
attribute format. The first column is the global cell number (**idelem1**), the second column is the local face number (**idface1**).

LIMIT ON FILENAMES: If a command line maximum length is 16384 characters, and if the faceset file names have 12 characters (face0001.inp), you can run dump/exo with about 1300 faceset files before running out of command line space. 


Dump ExodusII file with face set information
where face set information is read from files. This avoids automatic
facesets generated for all psets and eltsets. Only the facesets in the
files listed in the command will be used to define facesets.

**`facesets`** / (no file list) Dump ExodusII file with face sets (ExodusII  side sets).  If no faceset files are listed, the face sets will be automatically generated.  Note facesets will be created for all the nodes in each pset, and for all elements in each eltset. This can generate a large number of facesets.



## EXAMPLES

```
# sort based on cell itetclr id and cell centers 
createpts / median
sort / MO_MESH / index / ascending / ikey / itetclr xmed ymed zmed
reorder / MO_MESH / ikey

dump/exo/hex.exo / MO_MESH 
```
ExodusII will sort elements by their block ID (element itetclr value), do a pre-sort so element numbering is consistent with the written ExodusII file.  This example calls the sort command using the element itetclr values and element  median points (xmed, ymed, zmed). 
No vertex set, element set or face sets are written for this mesh.


```
dump/exo/ prism_fs.exo / moprism / / / facesets &
  fs_001.faceset, fs_002.faceset, fs_003.faceset, fs_004.faceset & 
  fs_005.faceset, fs_006.faceset, fs_007.faceset 
```
Write an ExodusII file for a prism mesh with 7 facesets defined.


```
dump / exo / out_2D_tri_pset_eltset.exo / motri / psets / eltsets /
dump / exo / out_2D_tri_pset.exo        / motri / psets /  /
dump / exo / out_2D_tri_eltset.exo      / motri /       / eltsets /
```
This example mesh object has both psets and eltsets defined before the dump command.
The first line writes an ExodusII file with vertex set, element set and no face set.
The second line writes an ExodusII file with vertex set, and no element set and no face set.
The third line writes an ExodusII file with a element set, no vertex set, and no face set.


### DEMO EXODUSII WITH FACESET FILES ON BOUNDARY

This Demo includes a full set of input files to create a stacked mesh, define boundary facesets, and write the ExodusII file.

<img src="https://meshing.lanl.gov/proj/examples/stack_fs_from_bndry/mesh_mat_fs5_and_fs8.png" width="250" alt=""> 
<a href="https://meshing.lanl.gov/proj/examples/stack_fs_from_bndry/method.html">Example Stack facesets from Boundary Polygon </a> 


### DEMO EXODUSII FACESET FILES ON CUBE


The following creates a cube shaped hex mesh then writes an ExodusII file with top and bottom facesets.


Input LaGriT command file [write_cube_exo_facesets.lgi](docs/demos/input/write_cube_exo_facesets.lgi.txt)

The hex mesh is created with **`createpts`** and assigned 2 materials to the itetclr array. The mesh elements are sorted by the itetclr values and median xmed, ymed, zmed points. This will ensure that element and face numbers are consistent with ExodusII requirements.

After **`sort`** and **`reorder`** the mesh boundary faces are extracted into a mesh object using [**`extract/surfmesh`**](docs/commands/dump/EXTRACT_SURFMESH.md) The surface mesh is subset into regions representing the top, bottom, and side boundaries defined by the 6 normal directions set by command **`settets/normal`** where **itetclr** is set to 1 (down), 2 (up), 3 (right), 4 (back), 5 (left), and 6 (front).

```
# Get top faces and write faceset file
cmo / copy / mo_tmp / mo_surf
cmo / select / mo_tmp
eltset / e_top / id_side / eq / 2
eltset / e_delete / not / e_top
rmpoint / element / eltset get e_delete
rmpoint / compress
  cmo / DELATT / mo_tmp / id_side
  dump / avs2 / output_2_top.faceset / mo_tmp / 0 0 0 2
  cmo / printatt / mo_tmp / idelem1
  cmo / printatt / mo_tmp / idface1
  cmo / delete / mo_tmp
```
The **itetclr** values are saved into new attribute **id_side** and are used to select and subset the surface mesh to top faces with the value 2. The faceset file named "output_2_top.faceset" is written. 


The format of the AVS faceset file includes the list of element-face relationships  written in AVS UCD cell attribute format.  
The first column is the global cell number, the second column is the local face number.
Subset the surface into sets such as top and bottom, write faceset files from each set. 


```
    File output_1_bottom.faceset:    File output_2_top.faceset:   
        0 0 0 2 0                          0 0 0 2 0                     
        00002  1  1                        00002  1  1                   
        idelem1, integer                   idelem1, integer              
        idface1, integer                   idface1, integer              
           2  1                               1  2                       
           5  1                               3  2                       
           7  1                               4  2                       
           8  1                               6  2                       
```
These are the first lines of each faceset file for top and bottom showing the AVS format and values written.

```
dump / exo / output_hex_final.exo / mohex / / / facesets &
  output_1_bottom.faceset output_2_top.faceset

```
The faceset files can be listed in the **`dump/exo`** command. 
Note the ExodusII ID for each set will be in the same order as the file names, ie first file will be given set ID=1, the second will have set ID=2.

The ExodusII file is written as a binary file and can be read by various
visualization software such as GMV and Paraview. For documentation on
the ExodusII API visit <http://gsjaardema.github.io/seacas/md/index.md
The Exodus utility ncdump can be used to convert the binary to ASCII file for checking that all mesh sections are written as expected. 


The result is an ExodusII binary mesh with 2 block materials and 2 side sets as reported in the LaGriT output:

<pre class="lg-output">

dump/exo/output_hex_final.exo/mohex///facesets output_1_bottom.faceset output_2_top.faceset
got       1 output_1_bottom.faceset                           
got       2 output_2_top.faceset                            

ExodusII: Start writing to file: output_hex_final.exo using cmo: mohex 
 
Exodus FACESETS imported from files.    2
 Total read:                4
 Current offset:            0
 Set tag:                   1  nfaces:   4
 first:                     2            1
 last:                      8            1
 Set new offset:            4
 Total read:                8
 Current offset:            4
 Set tag:                   2  nfaces:  4
 first:                     1           2
 last:                      6           2
 Set new offset:            8
 
Title: LAGRIT TO EXODUSII                                                       
number of dimension:               3                                            
number of nodes:                  27                                            
number of elements:                8                                            
number of edges:                   0                                            
number of edge blocks:             0                                            
number of element blocks:          2                                            
number of face blocks:             0                                            
number of node sets:               0                                            
number of edge sets:               0                                            
number of element sets:            0                                            
number of side sets:               2                                            
number of face sets:               0                                            
number of node maps:               0                                            
number of edge maps:               0                                            
number of face maps:               0                                            
number of element maps:            0                                            
 

------------------------------------------  
EXPSS loop:                                                                     
        1 Side Set tag:    1 Faces:     4      
        2 Side Set tag:    2 Faces:     4     
------------------------------------------   
Done ExodusII Side Sets Total:          2 
 
ExodusII: Done writing to ExodusII file: output_hex_final.exo using cmo: mohex  

</pre>



### DEMO USING PSETS AND ELTSETS

Input LaGriT command file [write_exo_pset_eltset.lgi](docs/demos/input/exo_pset_eltset.lgi.txt)

This command file is an example for writing **`psets/eltsets`**, both defined in the mesh object before writing the file.
Note all psets and eltsets defined in the mesh object will be written to the ExodusII file.

```
dump/exo/test2D_tri_pset_eltset.exo/motri / psets / eltsets /     

```

<pre class="lg-output">

ExodusII: Start writing to file: out_2D_tri_eltset.exo using cmo: motri  
 
Title: LAGRIT TO EXODUSII   
number of dimension:               2
number of nodes:                  16
number of elements:               18
number of edges:                   0
number of edge blocks:             0
number of element blocks:          3
number of face blocks:             0
number of node sets:               0
number of edge sets:               0
number of element sets:            3
number of side sets:               0
number of face sets:               0
number of node maps:               0
number of edge maps:               0
number of face maps:               0
number of element maps:            0                                            


WRITING EXODUS ELEMENT SETS:      3 sets in total 

                      Elemset Names           Set ID     # elements in set   
Done writing set no. 1 to ExodusII file 
                              e1                1             9   
Done writing set no. 2 to ExodusII file 
                              e2                2             3   
Done writing set no. 3 to ExodusII file 
                              e3                3             6   
 
ExodusII: Done writing to ExodusII file: out_2D_tri_eltset.exo using cmo: motri 

</pre>

Using **ncdump** command,  the binary ExodusII file can be converted for viewing the psets (node_ns) and eltsets (elem_els):

```
node_ns1 = 4, 5, 6, 7, 8, 9, 10, 11, 12, 13, 14, 15, 16 ;        

node_ns2 = 2, 3, 5, 6, 11, 12, 15 ;                              

node_ns3 = 1, 2, 3, 4, 5, 6, 7, 9, 10, 11, 14 ;                  

elem_els1 = 1, 2, 3, 4, 5, 6, 7, 8, 9 ;                          

elem_els2 = 10, 11, 12 ;                                         

elem_els3 = 13, 14, 15, 16, 17, 18 ;    
```
                                                                     
