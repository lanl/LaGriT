---
GENERATOR: 'Mozilla/4.7 
[en
] (X11; I; IRIX 6.5 IP32) 
[Netscape
]'
Generator: Microsoft Word 98
title: RZBRICK
---

 

 **RZBRICK**

Builds a brick mesh and generates a nearest neighbor connectivity
matrix. This command is similar to the rz command format except here we
have symmetry flags to input. A second format specifies that a mesh be
created and connected.

**xyz** specifies Cartesian coordinates.

**rtz** specifies cylindrical coordinates.

**rtp** specifies spherical coordinates.

ni,nj,nk number of points to be created in each direction.

xmin,ymin,zmin minimums for coordinates.

xmax,ymax,zmax maximums for coordinates.

iiz,ijz,ikz  if =0 then mins and maxs are used as cell centers

if =1 then mins and maxs are used as cell vertices

iirat,ijrat,ikrat ratio zoning switches (0=off,1=on)

xrz,yrz,zrz ratio zoning value - distance is multiplied by the value for
each subsequent point.

name name of pset containing starting point number

isym,jsym,ksym symmetry flags - not documented

Warning:  This command does not create a 2D grid, it has mem errors. 
**rzbrick****/xyz**/5,10,1/0. 0. 0./10. 20. 0. /1,1,1

for 2D this will work:

  mo create** cmo1///quad

  **quadxy** 5 5/ 0. 0. 0. / 20. 0. 0./20. 20. 0. / 0. 20. 0.

  **rzbrick****/xyz**/5,5,1/1,0,0/onnect**

  **dump gmv** quad5x5.gmv

 

 

**FORMAT:**

**rzbrick****/xyz****rtz****rtp**/ni,nj,nk/xmin,ymin,zmin/xmax,ymax,zmax/

iiz,ijz,ikz/
[iirat,ijrat,ikrat/xrz,yrz,zrz/isym,jsym,ksym
]

or

**rzbrick/xyzrtzrtp**/ni,nj,nk**/pset,get,**name/onnect**/

Use this option with **quadxyz** to connect logically rectangular grids.

EXAMPLE:

**rzbrick/xyz**/3,2,3/0.,0.,0./1.,1.,1./1,1,1

creates a hex grid 2x1x2 cells in the unit cube

**quadxyz**/5,7,5/0.,0.,0./1.,0.,0./1.5,0.5,2.0/.5,.2,2.5/

-1.,1.5,0./2.0,0.,0.0/2.1,1.9,2.4/-0.2,1.8,2.3/

**setpts**

**rzbrick/xyz**/5,7,5/1,0,0/onnect**

creates a hex grid inside the **** hexahedral specified by the 8 corners
passed to **quadxyz**
