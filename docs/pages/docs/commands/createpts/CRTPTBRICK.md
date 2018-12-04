---
GENERATOR: 'Mozilla/4.7 [en] (X11; I; IRIX 6.5 IP32) [Netscape]'
Generator: Microsoft Word 98
title: CREATEPTS/BRICK
---

 

 # CREATEPTS/BRICK

Builds a set nodes (logically rectangular) and create finite element
hexahedral connectivity. This command is similar to the rz command
format except here we have symmetry flags to input. A second format
specifies that a mesh be created and connected.

# FORMAT:

**createpts/brick** **/xyz** **rtz** **rtp**/ ni,nj,nk / xmin,ymin,zmin / xmax,ymax,zmax / iiz,ijz,ikz / [ iirat,ijrat,ikrat / xrz,yrz,zrz /isym,jsym,ksym ]

or

**createpts/brick/ xyz rtz rtp** / ni,nj,nk / **pset,get,** name / **connect** /

Use this option (for example, with **quadxyz** ) to create finite element hexahedral connectivity on a logically rectangular set of nodes created via another method..


**xyz** specifies Cartesian coordinates.

**rtz** specifies cylindrical coordinates.

**rtp** specifies spherical coordinates.

ni,nj,nk are the number of points to be created in each direction.

xmin,ymin,zmin are the minimums for coordinates.

xmax,ymax,zmax are the maximums for coordinates.

iiz,ijz,ikz  if = 0 then mins and maxs are used as cell centers if =1 then mins and maxs are used as cell vertices. The default is 1,1,1

iirat,ijrat,ikrat are optional and are the ratio zoning switches (0=off,1=on)

xrz,yrz,zrz are optional and are the ratio zoning value - distance is multiplied by the value for each subsequent point.


isym,jsym,ksym symmetry flags - not documented


## EXAMPLES:

<pre>
createpts/brick/xyz / 3,2,3 /0.,0.,0./1.,1.,1./1,1,1
</pre>

creates a set of hex points (3x2x3 nodes) and elements (2x1x2 cells) in
the unit cube. The connect option should not be used in this case because the connectivity is created.


<pre>
cmo/create/cmohex / / / hex
quadxyz /5,7,5/ 0.,0.,0./1.,0.,0./1.5,0.5,2.0/.5,.2,2.5/ &
         -1.,1.5,0./2.0,0.,0.0/2.1,1.9,2.4/-0.2,1.8,2.3/
createpts/brick/xyz/5,7,5/1,0,0/connect
</pre>

Creates a 3D set of nodes using **quadxyz** and then creates hex connectivity with the **createpts** command.
 
<pre>
cmo/create/cmoquad/ / / quad
quadxy/ 11, 11 / 0. 0. 0. / 1. 0. 0. / 1. 1. 0. / 0. 1. 0.
createpts/brick/xyz/11,11,1/1 0 0 / connect
</pre>

Creates a 2D set of nodes using **quadxy** and then creates quad connectivity with the **creaetpts** command.
