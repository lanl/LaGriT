---
GENERATOR: 'Mozilla/4.7 
[en
] (X11; I; IRIX 6.5 IP32) 
[Netscape
]'
Generator: 'Microsoft Word 97/98'
Template: 'Macintosh HD:Microsoft Office 98:Templates:Web Pages:Blank Web Page'
title: '
*arguments:'
---

 Description

  createpts/xyz/5,3,4/0,0,0/1,1,1/1,1,1/1,0,0/1.5,1,1
  This results in a set of 60 points , five across from x=0. to x=1, 3
  deep from x=0. to x=1. and 4 high from z=0. to z=1.  Mins and maxs
  are used as cell vertices.  Points are distributed using the xyz
  coordinate system using coordinates as end points.  Ratio zoning is
  in the x-direction.  Points are not distributed evenly in this
  direction.  They are distributed at a distance multiplied by 1.5 for
  each subsequent point.  Note that the rtz always results in a
  (partial) cylinder of points centered around the z axis. Use the
  rotateln command to orient the cylinder. For example, to center the
  cylinder around the y axis, specify the x axis as the line of
  rotation in the rotateln command.

  Arguments:

   --------- ------------------------------------------------------------------------------------------------------------
   xyz       specifies cartesian coordinates
   5,3,4     create 5 points in the x direction, create 3 points in the y direction, create 4 points in the z direction
   0,0,0     xmin= 0, ymin= 0, zmin= 0
   1,1,1     xmax= 1, ymax= 1, zmax= 1
   1,1,1     xmin and xmax are cell vertices, ymin and ymax are cell vertices, zmin and zmax are cell vertices
   1,0,0     x ratio zoning switch is on,  y ratio zoning switch is off,  z ratio zoning switch is off
   1.5,1,1   x-distance is multiplied by 1.5 for each subsequent point
   --------- ------------------------------------------------------------------------------------------------------------

  

          Input file:

       cmo/create/3dmesh

       surface/outer/reflect/box/0,0,0/1,1,1

       region/r1/ le outer /

       mregion/m1/ le outer /

       createpts/xyz/5,3,4/0,0,0/1,1,1/1,1,1/1,0,0/1.5,1,1/

       setpts

       dump/gmv/gmv1

       finish

 

  

  


