---
GENERATOR: 'Mozilla/4.7 [en] (X11; I; IRIX 6.5 IP32) [Netscape]'
Generator: 'Microsoft Word /98'
Template: 'Macintosh HD:Microsoft Office 98:Templates:Web Pages:Blank Web Page'
title: '
*arguments:'
---

 Example 5: createpts in a spherical geometry using shells as region
 boundaries

  The objective is to create a spherical mesh using the
  **createpts/sphere** command.
  A spherical geometry is defined containing three regions, each
  defined as the region between two spherical shells. The
  **createpts/sphere/diamond** command is used to create a point
  distribution between spherical shells for one diamond of the
  defining icosahedron. The output consists of one gmv file.

 Image of GMV output

  <img height="300" width="300" src="https://lanl.github.io/LaGriT/assets/images/image5tn.gif">"141"
  "166"](image/image5.gif"> [View 1](image/image5.gif">

 

  

 Input file

 
*TEST createpts/sphere (lagrit\_input\_createsphere)

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

 createpts/sphere/diamond/5/162/1.,3./0.0,0.0,0.0/1,0,0

 filter/1,0,0/

 rmpoint / compress

 setpts

 dump/gmv/output\_createsphere.gmv/

 
* begin compare here

 cmo/status

 cmo/printatt//-all-/minmax

 quality

 finish
