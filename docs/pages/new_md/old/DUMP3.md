---
GENERATOR: 'Mozilla/4.05C-SGI [en] (X11; I; IRIX 6.5 IP32) [Netscape]'
Generator: Microsoft Word 98
title: DUMP
---

 **dump/fehm**/file\_name/[cmo\_name]/ [**binary** **ascii** 
 **binaryc** **asciic** ]  //
 [**scalar,vector,both,area\_scalar,area\_vector,area\_both,alternate\_scalar**]
 /  [**delatt,keepatt**]

 

 

 The **dump / fehm** command will call the **dump/coord** and
 **dump/zone** options first. The main function of the **dump / fehm**
 command is to form a sparse coefficient matrix used for solving PDE on
 a triangular or tetrahedral delaunay grid.

 Options for the 5th argument are:

   --------------------- ------------------------------------------------------------------------
   **binary**(default)   Output sparse matrix in Fortran unformatted format 
   **ascii**             Output sparse matrix as ASCII format 
   **binaryc **          Output sparse matrix as Fortran unformatted format with a compression 
                         algorithm applied to the area coefficients. 
   **asciic **           Output sparse matrix as ASCII format with a compression 
                         algorithm applied to the area coefficients. 
   --------------------- ------------------------------------------------------------------------

 The 6th argument is empty.

 Options for the 7th argument are:

   ----------------------- ------------------------------------------------------------------------------------------------------
   **scalar**(default)     Area/distance                   coefficients are output as scalars 
   **vector**              Area/distance                   coefficients are output as vectors 
   **both**                Area/distance                   coefficients are output as scalars and vectors 
   **area\_scalar **       Area                                 coefficients are output as scalars
   **area\_vector**        Area                                 coefficients are output as vectors 
   **area\_both **         Area                                 coefficients are output as scalars and vectors 
   **alternate\_scalar**   Area/distance                   coefficients are output as scalars with compression of coefficients 
                                                              & applied during calculation of coefficients. 
   ----------------------- ------------------------------------------------------------------------------------------------------

  

 Options for the 8th aregument are:

   --------------------- -------------------------------------------------------------------------------------------
   **delatt**(default)   No new node attributes are created.

   **ascii**             Output sparse matrix as ASCII format 

   **keepatt**           Six node attributes are created (top, bottom, left\_w, right\_e,back\_n, front\_s) which 

                         are assigned values according to the direction of the octant of their normal vector. 

                           
   --------------------- -------------------------------------------------------------------------------------------

  

EXAMPLE:

 **dump****/gmv**/gmv.out/3dmesh/

 **dump/lagrit**/LaGriT.out**/-all-**/binary
