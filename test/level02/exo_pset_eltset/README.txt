Note from Rao:

Only want to point out that exodus uses 'es' for edge sets and 'els' for element sets. I hand edited an exodus II file after converting it to ascii using ncdump and I've attached it here.


Test from Carl:
On 05/06/15 08:18, Gable, Carl wrote:
The dump/exo  code in LaGriT does support vertex and cell set output to ExodusII files. The documentation needs to be updated. I think some quick tests with the current LaGriT build should be done but I think that it should all work.

I created a test and it looks like it works fine (It should be incorporated into the LaGriT test suite):
/home/gable/src/lagrit/work/exodusII/element_sets
The test driver is:
test_eltset_pset.lgi
I tested pset and elstset output on tet, hex and tri mesh types. I tried three different options, pset&eltset, pset only, eltset only. Works fine.

What I think needs to be worked out is the name and order convention. I think the *.exo file does not maintain the 'name' of the pset or eltset. It just outputs them in LaGriT order and names them:
node_ns1, node_ns2, … node_nsn
elem_es1, elem_es2, … elem_esn

Documentation now says:
dump / exo / file_name/ mo_name 
dump / exo / file_name/ mo_name / psets / eltsets / facesets 
dump / exo / file_name/ mo_name / psets / eltsets / facesets file1 file2 ... filen
dump / exodusii / file_name/ mo_name 
dump / exodusII / file_name/ mo_name 

Write a mesh object to a file in the Exodus II format. 
I would suggest documentation should say:
Dump ExodusII file with no vertex set, element set or face set information:
dump / exo / file_name/ mo_name 
dump / exodusii / file_name/ mo_name 
dump / exodusII / file_name/ mo_name 
Dump ExodusII file with vertex set, element set and face set information:
dump / exo / file_name/ mo_name / psets / eltsets / facesets 
Dump ExodusII file with vertex set, element set and face set information where face set information is read from files:
dump / exo / file_name/ mo_name / psets / eltsets / facesets file1 file2 … filen
Dump ExodusII file with vertex set, element set and no face set information:
dump / exo / file_name/ mo_name / psets / eltsets
Dump ExodusII file with no vertex set,  no element set and face set information from files:
dump / exo / file_name/ mo_name / / / facesets file1 file2 … filen
Write a mesh object to a file in the Exodus II format. The keyword pests as token 5 will cause all psets (lists of vertex numbers) associated with the mesh object to be written to the ExodusII output file. The keyword eltsets as token 6 will cause all eltsets (lists of cell numbers) associated with the mesh object to be written to the ExodusII output file.  If face set information is being provided from files (file1 file2 … filen) the format of the file is written in AVS UCD cell attribute format. The first column is the global cell number, the second column is the local face number: For example:
0 0 0 2 0
2  1  1
idelem1, integer 
idface1, integer 
      1   1
      4   1
      7   2
     10   2
     13   3

Here is example of LaGriT screen/logfile output during ExodusII output:
ExodusII: Start writing to file: test2D_tri_pset_eltset.exo using cmo: motri    
 
Title: LaGriT to Exodus 
number of dimension: 2 
number of nodes: 16 
number of elements: 18 
number of edges: 0 
number of edge blocks: 0 
number of element blocks: 3 
number of face blocks: 0 
number of node sets: 3 
number of edge sets: 0 
number of element sets: 3 
number of side sets: 0 
number of face sets: 0 
number of node maps: 0 
number of edge maps: 0 
number of face maps: 0 
number of element maps: 0 

 NUMBER OF NODE SETS:                     3
 Writing to EXO file nodeset no.                     1
 Nodeset name: p1
Done writing set no. 1 to exodus file 
 Writing to EXO file nodeset no.                     2
 Nodeset name: p2
Done writing set no. 2 to exodus file 
 Writing to EXO file nodeset no.                     3
 Nodeset name: p3
Done writing set no. 3 to exodus file 

 NUMBER OF ELEMENT SETS:                     3
 Writing to EXO file eltset no.                     1
 Eltset name: e1
Done writing set no. 1 to exodus file 
 Writing to EXO file eltset no.                     2
 Eltset name: e2
Done writing set no. 2 to exodus file 
 Writing to EXO file eltset no.                     3
 Eltset name: e3
Done writing set no. 3 to exodus file 
 
ExodusII: Done writing to file: test2D_tri_pset_eltset.exo using cmo: motri     

