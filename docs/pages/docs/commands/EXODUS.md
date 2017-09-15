
**dump / exo  exodusii** / file\_name/ mo\_name [ **psets** ] / [
**eltsets**] / [ **facesets** file1 file2 ... filen ]

Write a mesh object to a file in the Exodus II format. The keyword pests
as token 5 will cause all psets (lists of vertex numbers) associated
with the mesh object to be written to the ExodusII output file. The
keyword eltsets as token 6 will cause all eltsets (lists of cell
numbers) associated with the mesh object to be written to the ExodusII
output file. If face set information is being provided from files (file1
file2 ... filen) the format of the file is written in AVS UCD cell
attribute format. The first column is the global cell number, the second
column is the local face number.

SYNTAX:

Dump plain ExodusII file with no vertex set, element set or face set
information:

**dump / exo** / file\_name/ mo\_name

**dump / exodusii** / file\_name/ mo\_name

**dump / exodusII** / file\_name/ mo\_name

Dump ExodusII with vertex set, element set, and face set information.
Note facesets will be created for all the nodes in each pset, and for
all elements in each eltset. This can generate a large number of
facesets.

**dump / exo** / file\_name/ mo\_name / **psets** / **eltsets** /
**facesets**

Dump ExodusII file with vertex set, element set and face set information
where face set information is read from files. This avoids automatic
facesets generated for all psets and eltsets. Only the facesets in the
files listed in the command will be used to define facesets:

**dump / exo** / file\_name/ mo\_name / **psets** / **eltsets** /
**facesets** file1 file2 ... filen

Dump ExodusII file with vertex set, element set and no face set
information:

**dump / exo** / file\_name/ mo\_name / **psets** / **eltsets**

Dump ExodusII file with no vertex set, no element set and face set
information from files:

**dump / exo** / file\_name/ mo\_name / / / **facesets** file1 file2 ...
filen

EXAMPLE WITH FACESET FILES:

The following are examples for writing an ExodusII file with top and
bottom facesets for a hex mesh.

If face set information is being provided from files (file1 file2 ...
filen) the format of the file is written in AVS UCD cell attribute
format.

The first column is the global cell number, the second column is the
local face number.

    File output\_1\_bottom.faceset:    File output\_1\_bottom.faceset:   
        0 0 0 2 0                          0 0 0 2 0                     
        00002  1  1                        00002  1  1                   
        idelem1, integer                   idelem1, integer              
        idface1, integer                   idface1, integer              
           2  1                               1  2                       
           5  1                               3  2                       
           7  1                               4  2                       
           8  1                               6  2                       

This is the LaGriT screen/logfile output during ExodusII output using
faceset files:

+-----------------------------------------------------------------------+

     dump/exo/output_hex_final.exo/mohex///facesets/output_1_bottom.faceset output_2_top.faceset                                            
      got                     1 output_1_bottom.faceset                

      got                     2 output_2_top.faceset                   

     ExodusII: Start writing to file: output_hex_final.exo using cmo: mohex                                                                 
     Exodus FACESETS imported from files.  2                           

     done reading output_1_bottom.faceset                              

     total values      4                                               
     Total read:       4                                               
     Current offset:   0                                               
     Set tag:          1  nfaces: 4                                    
     first:            2          1                                    
     last:             8          1                                    
     Set new offset:              4                                    

     done reading output_2_top.faceset                                 

     total values      4                                               
     Total read:       8                                               
     Current offset:   4                                               
     Set tag:          2  nfaces: 4                                    
     first:            1          2                                    
     last:             6          2                                    
     Set new offset:              8                                    

     Title: LaGriT to Exodus                                           
     number of dimension: 3                                            
     number of nodes: 27                                               
     number of elements: 8                                             
     number of edges: 0                                                
     number of edge blocks: 0                                          
     number of element blocks: 2                                       
     number of face blocks: 0                                          
     number of node sets: 0                                            
     number of edge sets: 0                                            
     number of element sets: 0                                         
     number of side sets: 2                                            
     number of face sets: 0                                            
     number of node maps: 0                                            
     number of edge maps: 0                                            
     number of face maps: 0                                            
     number of element maps: 0                                         

     EXODUSII write element blocks:    2                               
     EXODUSII writing side sets:       2                               
     ------------------------------------------                        
     EXPSS loop:         1                                             
     sideset tag:        1 nfaces:     4                               
     index starting at   1                                             
        nfaces_ss :      4                                             
        sselemlist :     2                                             
        ssfacelist :     5                                             
     ------------------------------------------                        
     ------------------------------------------                        
     EXPSS loop:         2                                             
     sideset tag:        2 nfaces:     4                               
     index starting at   5                                             
        nfaces_ss :      4                                             
        sselemlist :     1                                             
        ssfacelist :     6                                             
     ------------------------------------------                        

     ExodusII: Done writing to ExodusII file: output_hex_final.exo using cmo: mohex                                                         
+-----------------------------------------------------------------------+

Output ExodusII file using **facesets**:

The ExodusII file is written as a binary file and can be read by various
visualization software such as GMV and Paraview. For documentation on
the ExodusII API visit
<http://gsjaardema.github.io/seacas/md/index.md

The mesh written to the ExodusII binary file with the following mesh
object information. (The Exodus utility ncdump is used to convert the
binary to ASCII file shown here).

Number of nodes and elements are num\_nodes and num\_elem

The mesh materials in itetclr are written as blocks so num\_el\_blk are
the number of itetclr materials.

The face sets are written as side sets, so num\_side\_sets are the
number of facesets.

The elements for the facesets are in elem\_ss1

The corresponding faces of the elements are in side\_ss1

         dimensions:                                                       
             len_string = 33 ;                                             
             len_line = 81 ;                                               
             four = 4 ;                                                    
             len_name = 33 ;                                               
             time_step = UNLIMITED ; // (0 currently)                      
             num_dim = 3 ;                                                 
             num_nodes = 27 ;                                              
             num_elem = 8 ;                                                
             num_el_blk = 2 ;                                              
             num_side_sets = 2 ;                                           
             num_qa_rec = 1 ;                                              
             num_el_in_blk1 = 6 ;                                          
             num_nod_per_el1 = 8 ;                                         
             num_att_in_blk1 = 1 ;                                         
             num_el_in_blk2 = 2 ;                                          
             num_nod_per_el2 = 8 ;                                         
             num_att_in_blk2 = 1 ;                                         
             num_side_ss1 = 4 ;                                            
             num_side_ss2 = 4 ;                                            

         data:                                                             

          eb_status = 1, 1 ;                                               

          eb_prop1 = 1, 2 ;                                                

          ss_status = 1, 1 ;                                               

          ss_prop1 = 1, 2 ;                                                

          coordx = 0, 0.5, 1, 0, 0.5, 1, 0, 0.5, 1, 0, 0.5, 1, 0, 0.5, 1,  
     0, 0.5, 1,                                                            
             0, 0.5, 1, 0, 0.5, 1, 0, 0.5, 1 ;                             

          coordy = 0, 0, 0, 0.5, 0.5, 0.5, 1, 1, 1, 0, 0, 0, 0.5, 0.5, 0.5 
     , 1, 1, 1,                                                            
             0, 0, 0, 0.5, 0.5, 0.5, 1, 1, 1 ;                             

          coordz = 0, 0, 0, 0, 0, 0, 0, 0, 0, 0.5, 0.5, 0.5, 0.5, 0.5, 0.5 
     , 0.5, 0.5,                                                           
             0.5, 1, 1, 1, 1, 1, 1, 1, 1, 1 ;                              

          eb_names =                                                       
           ,                                                             
            ;                                                            

          ss_names =                                                       
           ,                                                             
            ;                                                            

          coor_names =                                                     
           ,                                                             
           ,                                                             
            ;                                                            

          qa_records =                                                     
           "mohex",                                                        
           "probname",                                                     
           "Today",                                                        
           "Time" ;                                                        

          attrib1 =                                                        
           0,                                                              
           0,                                                              
           0,                                                              
           0,                                                              
           0,                                                              
           0 ;                                                             

          attrib_name1 =                                                   
            ;                                                            

          connect1 =                                                       
           10, 11, 14, 13, 19, 20, 23, 22,                                 
           4, 5, 8, 7, 13, 14, 17, 16,                                     
           13, 14, 17, 16, 22, 23, 26, 25,                                 
           11, 12, 15, 14, 20, 21, 24, 23,                                 
           5, 6, 9, 8, 14, 15, 18, 17,                                     
           14, 15, 18, 17, 23, 24, 27, 26 ;                                

          attrib2 =                                                        
           0,                                                              
           0 ;                                                             

          attrib_name2 =                                                   
            ;                                                            

          connect2 =                                                       
           1, 2, 5, 4, 10, 11, 14, 13,                                     
           2, 3, 6, 5, 11, 12, 15, 14 ;                                    

          elem_ss1 = 2, 5, 7, 8 ;                                          

          side_ss1 = 5, 5, 5, 5 ;                                          

          elem_ss2 = 1, 3, 4, 6 ;                                          

          side_ss2 = 6, 6, 6, 6 ;                                          
                                                                      
+-----------------------------------------------------------------------+

The input faceset files can be compared to the ExodusII side set values
by using this mapping between local face numbers from LaGriT to
ExodusII.

           data lag2exo_fmap / 1, 0, 0, 0, 0, 0, ! Point               
          &                    1, 2, 0, 0, 0, 0, ! Line                
          &                    2, 3, 1, 0, 0, 0, ! Tri                 
          &                    1, 2, 3, 4, 0, 0, ! Quad                
          &                    2, 3, 1, 4, 0, 0, ! Tet                 
          &                    0, 0, 0, 0, 0, 0, ! Pyramid             
          &                    4, 5, 1, 2, 3, 0, ! Wedge/Prism         
          &                    5, 6, 1, 2, 3, 4/ ! Hex                 

EXAMPLE USING PSETS AND ELTSETS:

Here is example of LaGriT screen/logfile output during ExodusII output
for **psets/eltsets**:

         dump/exo/test2D_tri_pset_eltset.exo/motri / psets / eltsets /     

         ExodusII: Start writing to file: test2D_tri_pset_eltset.exo using 
      cmo: motri                                                           

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

         EXODUSII write element blocks:           3                        
         Writing to EXO file nodeset no.          1                        
         Nodeset name: p1                                                  
         Done writing set no. 1 to exodus file                             
         Writing to EXO file nodeset no.          2                        
         Nodeset name: p2                                                  
         Done writing set no. 2 to exodus file                             
         Writing to EXO file nodeset no.          3                        
         Nodeset name: p3                                                  
         Done writing set no. 3 to exodus file                             

         EXODUSII write element sets:             3                        
         Writing to EXO file eltset no.           1                        
         Eltset name: e1                                                   
         Done writing set no. 1 to exodus file                             
         Writing to EXO file eltset no.           2                        
         Eltset name: e2                                                   
         Done writing set no. 2 to exodus file                             
         Writing to EXO file eltset no.           3                        
         Eltset name: e3                                                   
         Done writing set no. 3 to exodus file                             
         EXODUSII writing side sets:              0                        

         ExodusII: Done writing to ExodusII file: test2D_tri_pset_eltset.e 
     xo using cmo: motri                                                   

Output ExodusII file using **psets/eltsets**:

     netcdf test2D_tri_pset_eltset {                                   
     dimensions:                                                       
         len_string = 33 ;                                             
         len_line = 81 ;                                               
         four = 4 ;                                                    
         len_name = 33 ;                                               
         time_step = UNLIMITED ; // (0 currently)                      
         num_dim = 2 ;                                                 
         num_nodes = 16 ;                                              
         num_elem = 18 ;                                               
         num_el_blk = 3 ;                                              
         num_node_sets = 3 ;                                           
         num_elem_sets = 3 ;                                           
         num_qa_rec = 1 ;                                              
         num_el_in_blk1 = 9 ;                                          
         num_nod_per_el1 = 3 ;                                         
         num_att_in_blk1 = 1 ;                                         
         num_el_in_blk2 = 3 ;                                          
         num_nod_per_el2 = 3 ;                                         
         num_att_in_blk2 = 1 ;                                         
         num_el_in_blk3 = 6 ;                                          
         num_nod_per_el3 = 3 ;                                         
         num_att_in_blk3 = 1 ;                                         
         num_nod_ns1 = 13 ;                                            
         num_nod_ns2 = 7 ;                                             
         num_nod_ns3 = 11 ;                                            
         num_ele_els1 = 9 ;                                            
         num_ele_els2 = 3 ;                                            
         num_ele_els3 = 6 ;                                            
     variables:                                                        
         double time_whole(time_step) ;                                
         int eb_status(num_el_blk) ;                                   
         int eb_prop1(num_el_blk) ;                                    
             eb_prop1:name = "ID" ;                                    
         int ns_status(num_node_sets) ;                                
         int ns_prop1(num_node_sets) ;                                 
             ns_prop1:name = "ID" ;                                    
         int els_status(num_elem_sets) ;                               
         int els_prop1(num_elem_sets) ;                                
             els_prop1:name = "ID" ;                                   
         double coordx(num_nodes) ;                                    
         double coordy(num_nodes) ;                                    
         char eb_names(num_el_blk, len_name) ;                         
         char ns_names(num_node_sets, len_name) ;                      
         char els_names(num_elem_sets, len_name) ;                     
         char coor_names(num_dim, len_name) ;                          
         char qa_records(num_qa_rec, four, len_string) ;               
         double attrib1(num_el_in_blk1, num_att_in_blk1) ;             
         char attrib_name1(num_att_in_blk1, len_name) ;                
         int connect1(num_el_in_blk1, num_nod_per_el1) ;               
             connect1:elem_type = "TRI3" ;                             
         double attrib2(num_el_in_blk2, num_att_in_blk2) ;             
         char attrib_name2(num_att_in_blk2, len_name) ;                
         int connect2(num_el_in_blk2, num_nod_per_el2) ;               
             connect2:elem_type = "TRI3" ;                             
         double attrib3(num_el_in_blk3, num_att_in_blk3) ;             
         char attrib_name3(num_att_in_blk3, len_name) ;                
         int connect3(num_el_in_blk3, num_nod_per_el3) ;               
             connect3:elem_type = "TRI3" ;                             
         int node_ns1(num_nod_ns1) ;                                   
         int node_ns2(num_nod_ns2) ;                                   
         int node_ns3(num_nod_ns3) ;                                   
         int elem_els1(num_ele_els1) ;                                 
         int elem_els2(num_ele_els2) ;                                 
         int elem_els3(num_ele_els3) ;                                 

     // global attributes:                                             
             :api_version = 6.36f ;                                    
             :version = 6.36f ;                                        
             :floating_point_word_size = 8 ;                           
             :file_size = 1 ;                                          
             :maximum_name_length = 32 ;                               
             :int64_status = 0 ;                                       
             :title = "LaGriT to Exodus" ;                             
     data:                                                             

      eb_status = 1, 1, 1 ;                                            

      eb_prop1 = 1, 2, 3 ;                                             

      ns_status = 1, 1, 1 ;                                            

      ns_prop1 = 1, 2, 3 ;                                             

      els_status = 1, 1, 1 ;                                           

      els_prop1 = 1, 2, 3 ;                                            

      coordx = 0, 0.333333333333333, 0.666666666666667, 1, 0, 0.333333 
     333333333,                                                            
             0.666666666666667, 1, 0, 0.333333333333333, 0.666666666666667 
     , 1, 0,                                                               
             0.333333333333333, 0.666666666666667, 1 ;                     

          coordy = 0, 0, 0, 0, 0.333333333333333, 0.333333333333333,       
             0.333333333333333, 0.333333333333333, 0.666666666666667,      
             0.666666666666667, 0.666666666666667, 0.666666666666667, 1, 1 
     , 1, 1 ;                                                              

      eb_names =                                                       
       ,                                                             
       ,                                                             
        ;                                                            

      ns_names =                                                       
       "p1[W
374
177",                                                 
       "p2[W
374
177",                                                 
       "p3[W
374
177" ;                                                

      els_names =                                                      
       "e1[W
374
177",                                                 
       "e2[W
374
177",                                                 
       "e3[W
374
177" ;                                                

      coor_names =                                                     
       ,                                                             
        ;                                                            

      qa_records =                                                     
       "motri",                                                        
       "probname",                                                     
       "Today",                                                        
       "Time" ;                                                        

      attrib1 =                                                        
       0,                                                              
       0,                                                              
       0,                                                              
       0,                                                              
       0,                                                              
       0,                                                              
       0,                                                              
       0,                                                              
       0 ;                                                             

      attrib_name1 =                                                   
        ;                                                            

      connect1 =                                                       
       11, 15, 14,                                                     
       9, 10, 13,                                                      
       15, 12, 16,                                                     
       6, 7, 10,                                                       
       7, 4, 8,                                                        
       7, 8, 11,                                                       
       11, 8, 12,                                                      
       10, 14, 13,                                                     
       5, 6, 9 ;                                                       

      attrib2 =                                                        
       0,                                                              
       0,                                                              
       0 ;                                                             

      attrib_name2 =                                                   
        ;                                                            

      connect2 =                                                       
       2, 3, 6,                                                        
       11, 12, 15,                                                     
       2, 6, 5 ;                                                       

      attrib3 =                                                        
       0,                                                              
       0,                                                              
       0,                                                              
       0,                                                              
       0,                                                              
       0 ;                                                             

      attrib_name3 =                                                   
        ;                                                            

      connect3 =                                                       
       1, 2, 5,                                                        
       6, 10, 9,                                                       
       10, 11, 14,                                                     
       7, 11, 10,                                                      
       3, 7, 6,                                                        
       3, 4, 7 ;                                                       

      node_ns1 = 4, 5, 6, 7, 8, 9, 10, 11, 12, 13, 14, 15, 16 ;        

      node_ns2 = 2, 3, 5, 6, 11, 12, 15 ;                              

      node_ns3 = 1, 2, 3, 4, 5, 6, 7, 9, 10, 11, 14 ;                  

      elem_els1 = 1, 2, 3, 4, 5, 6, 7, 8, 9 ;                          

      elem_els2 = 10, 11, 12 ;                                         

      elem_els3 = 13, 14, 15, 16, 17, 18 ;                             
                                                                     
