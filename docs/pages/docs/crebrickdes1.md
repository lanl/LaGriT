---
GENERATOR: 'Mozilla/4.7 [en] (X11; I; IRIX 6.5 IP32) [Netscape]'
Generator: 'Microsoft Word 97/98'
Template: 'Macintosh HD:Microsoft Office 98:Templates:Web Pages:Blank Web Page'
title: '
*arguments:'
---

 Description

  createpts/brick/xyz/3,2,3/0.,0.,0./1.,1.,1./1,1,1
  Creates a hex grid 2x1x2 cells in the unit cube

   

  Arguments:

   ------- ------------------------------------------------------------------------------------------------------------
   xyz     specifies cartesian coordinates
   3,2,3   create 3 points in the x direction, create 2 points in the y direction, create 3 points in the z direction
   0,0,0   xmin= 0, ymin= 0, zmin= 0
   1,1,1   xmax= 1, ymax= 1, zmax= 1
   1,1,1   xmin and xmax are cell vertices, ymin and ymax are cell vertices, zmin and zmax are cell vertices
   ------- ------------------------------------------------------------------------------------------------------------

  

          Input file:

  
*input.hex

  
* create a hexaheral grid

  cmo create abc///hex

  quadxyz 5 7 5 0. 0. 0. 1. 0. 0. 1.5 .5 2. .5 .2 2.5 -1. 1.5 0. 2. 2.
  0. &

           2.1 1.9 2.4 -.2 1.8 2.3 /

  setpts

  creatpts/brick/xyz/5 7 5/1,0,0/connect/

  settets

  dump/gmv/gmv.hex

  finish

   

 

