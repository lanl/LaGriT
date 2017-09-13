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

 Example 8: creatpts in a spherical geometry using shells as region
 boundaries

  The objective is to create a spherical mesh using the
  **createpts/sphere** command.
  A spherical geometry is defined containing three regions, each
  defined as the region between two spherical shells. The
  **createpts/sphere/itype=2** command is used to create a point
  distribution between spherical shells for all diamonds of the
  defining icosahedron. The output consists of one gmv file.

 

 Images of GMV output

 [<img height="300" width="300" src="/assets/images/image8tn.gif">"156" "153"](image/image8a.gif">
 [View A ](image/image8a.gif">[<img height="300" width="300" src="/assets/images/image8btn.gif">"170"
 ](image/image8b.gif"> [View B](image/image8b.gif">

  

  

 Input file

 
*TEST createpts/sphere (lagrit\_input\_createsphere3)

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

   dump/gmv/output\_createsphere3.gmv/

 
* begin compare here

 cmo/status

 cmo/printatt//-all-/minmax

 quality
