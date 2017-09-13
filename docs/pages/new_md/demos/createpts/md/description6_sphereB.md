---
GENERATOR: 'Mozilla/4.7 [en] (X11; I; IRIX 6.5 IP32) [Netscape]'
Generator: 'Microsoft Word 97/98'
Template: 'Macintosh HD:Microsoft Office 98:Templates:Web Pages:Blank Web Page'
title: '
*arguments:'
---

 Example 6: creatpts in a spherical geometry using shells as region
 boundaries

  The objective is to create a spherical mesh using the
  **createpts/sphere** command.
  A spherical geometry is defined containing three regions, each
  defined as the region between two spherical shells. The
  **createpts/sphere/itype=2** command is used to create a point
  distribution between spherical shells for all diamonds of the
  defining icosahedron. The output consists of one gmv file.

 

 Images of GMV output

  [<img height="300" width="300" src="https://lanl.github.io/docs/assets/images/image6tn.gif">"171"
  "161"](image/image6.gif"> [sphere-points](image/image6.gif">

 

  

 Input file

 
*TEST createpts/sphere (lagrit\_input\_createsphere2)

 cmo/create/cmo1/ / / hex

 surface/s1/reflect/sphere/0.,0.,0.,3./

 surface/s2/intrface/sphere/0.,0.,0.,2./

 surface/s3/intrface/sphere/0.,0.,0.,1./

 region/r1/ le s1 and gt s2 /

 region/r2/ le s2 and ge s3 /

 region/r3/ lt s3 /

 mregion/mr1/ le s1 and gt s2 /

 mregion/mr2/ lt s2 and gt s3 /

 mregion/mr3/ lt s3 /

 createpts/sphere/2/5/162/1.0,3.0/0.0,0.0,0.0/1,0,0

 filter/1,0,0/

 setpts

 dump/gmv/output\_createsphere2.gmv/

 
* begin compare here

 cmo/status

 cmo/printatt//-all-/minmax

 quality

 finish
