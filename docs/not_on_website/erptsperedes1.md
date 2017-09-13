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

  createpts/sphere/8/5,162,0.0,1.0/0.0,0.0,0.0/1/
  Build a sphere.

  Arguments:

   --------------- --------------------------------------------------------------------------------------------------------------------------------------
   8
              generates a hexahedral icosahedron grid.  Distributes points and generates the grid connectivity data structures. 
                   

   
               create 5 radial shells with the upper limit of 162 points.  1 is the outer radii of the sphere. .5 is the inner radii of the sphere.
   5,162,1.,0.5
   
    
              
                   

   0,0,0           xcen= 0, ycen= 0 zmin= 0

   1,0,0           mins and maxs are used as cell vertices.  ratio zoning is off.  
   --------------- --------------------------------------------------------------------------------------------------------------------------------------

  

 Input file:

  cmo/create/cmo1/

  surface/s1/reflect/sphere/0.,0.,0.,3./

  surface/s2/intrface/sphere/0.,0.,0.,2./

  surface/s3/intrface/sphere/0.,0.,0.,1./

  region/r1/ le s1 and gt s2 /

  region/r2/ le s2 and ge s3 /

  region/r3/ lt s3 /

  mregion/mr1/ le s1 and gt s2 /

  mregion/mr2/ lt s2 and gt s3 /

  mregion/mr3/ lt s3 /

  createpts/sphere/8/5/162/1.0,0.5/0.,0.,0./1,0.,0.

  filter/1,0,0/

  geniee

  
*zq/imt/1,0,0/1/

  setpts

  settets

  dump/gmv/gmv2/

  
