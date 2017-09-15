---
GENERATOR: 'Mozilla/4.7C-SGI [en] (X11; I; IRIX64 6.5 IP30) [Netscape]'
Generator: Microsoft Word 98
Template: 'Macintosh HD:Microsoft Office 98:Templates:Web Pages:Blank Web Page'
title: 1
---

 

red = face number (for 3D figures, number of front face is printed below
the figure)

blue = node number

green = edge number (arrow gives edge direction)

The 3D face ordering is such that the right-hand-normals of all facets
point outward.

 

 

**Supported Element Types**
+-----------------+-----------------+-----------------+-----------------+
                                   Nodes for faces  Edges for face  
+-----------------+-----------------+-----------------+-----------------+
 1.node (point)   <img height="300" width="300" src="point.gif">{   0                 0             
                  "105"                                       
                  "27"                                      
+-----------------+-----------------+-----------------+-----------------+
 2
. line         <img height="300" width="300" src="line.gif">{w  1                 0              
                  idth="123"       2
                               
                  "48"                                      
+-----------------+-----------------+-----------------+-----------------+
 3
. triangle     <img height="300" width="300" src="triangle.gi  2,3               3              
                  f)"169"   3,1               2              
                  "121"                                     
                                   1,2
              1
             
                                                     
              
                                                                    
                                                                    
                                                                    
+-----------------+-----------------+-----------------+-----------------+
 4
. quad         <img height="300" width="300" src="square.gif">  1, 2              1              
                  "174"     2, 3              3              
                  "154"                                     
                                   3, 4              4              

                                   4 ,1
             2              
                                                                    
                                                                    
                                                                    
+-----------------+-----------------+-----------------+-----------------+
 5
. tetrahedron  <img height="300" width="300" src="tet1.gif">{w  2 ,3, 4          6, 5, 4         
                  idth="176"       1, 4, 3          6, 2, 3         
                  "139"                                     
                                   1, 2, 4          5, 3, 1         

                                   1, 3, 2          4, 1, 2         

                                                                    
+-----------------+-----------------+-----------------+-----------------+
 6
. pyramid      <img height="300" width="300" src="pyramid.gif  1, 4, 3, 2       2, 6, 4, 1      
                  )"211"    1, 2, 5          1, 5, 3         
                  "143"                                     
                                   2, 3, 5          4, 7, 5         

                                   3, 4, 5          6, 8, 7         

                                   4, 1, 5
         2, 3, 8         
                                    
                               
                                                                    

                                                                    
+-----------------+-----------------+-----------------+-----------------+
 7
. prism        <img height="300" width="300" src="prism.gif">{  1, 3, 2           2,  4,  1      
                  "217"       4, 5, 6         7, 9, 8         
                  "208"                                     
                                   1, 2, 5, 4       1, 5, 7, 3      

                                   2, 3, 6, 5       4, 6, 9, 5      

                                   1, 4, 6, 3
      3, 8, 6, 2      
                                                                    

                                                                    
+-----------------+-----------------+-----------------+-----------------+
 8
. hexahedron   <img height="300" width="300" src="hex.gif">{wi   1,  4,  3 , 2    2,  6,  4,  1  
                  dth="244"        5,  6,  7,  8
   9,  11,  12,    
                  "188"                     10              

                                   1,  2,  6,  5    1,  5,  9,  3   

                                   2, 3, 7, 6
      4,  7,  11,  5  
                                                                    
                                                    6,  8,  12,  7  
                                   3,  4,  8,  7 
                  
                                                    3,  10,  8,  2  

                                   1,  5,  8,  4
                   
                                                                    
+-----------------+-----------------+-----------------+-----------------+
