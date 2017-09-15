---
GENERATOR: 'Mozilla/4.7 [en] (X11; I; IRIX 6.5 IP32) [Netscape]'
Generator: 'Microsoft Word /98'
Template: 'Macintosh HD:Microsoft Office 98:Templates:Web Pages:Blank Web Page'
title: '
*arguments:'
---

 Example 3: creatpts in a spherical geometry

  The objective is to add points to a spherical mesh using the
  **createpts** command.
  A spherical geometry is defined. The **createpts/rtp** command is
  used to create a point

  distribution within the sphere. The output consists of one gmv file.

   

 Images of GMV output

  <img height="300" width="300" src="https://lanl.github.io/LaGriT/assets/images/image3tn.gif">"116"
  "138"](./image/image3.gif">[spherical
  geometry](image/image3.gif">

   

 Input file

 
*TEST createpts/rtp (lagrit\_input\_creatertp)

 cmo/create/cmo1/

 surface/s3/reflect/sphere/0.,0.,0./1.,1.,1./

 region/r3/ lt s3 /

 mregion/mr3/ lt s3 /

 createpts/rtp/2,6,30/0,0,0/1,,360/1,1,1

 dump/gmv/output\_creatertp.gmv/

 
* begin compare here

 cmo/status

 cmo/printatt//-all-/minmax

 quality

 finish
