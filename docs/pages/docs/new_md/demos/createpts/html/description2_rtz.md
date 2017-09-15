---
GENERATOR: 'Mozilla/4.7 [en] (X11; I; IRIX 6.5 IP32) [Netscape]'
Generator: 'Microsoft Word /98'
Template: 'Macintosh HD:Microsoft Office 98:Templates:Web Pages:Blank Web Page'
title: '
*arguments:'
---

 Example 2: createpts in a cylindrical geometry

  The objective is to add points to a cylindrical mesh using the
  **createpts** command.
  A cylindrical geometry is defined. The **createpts/rtz** command is
  used to create a point distribution within the geometry exercising
  various options available to the command. The output consists of one
  gmv file.

   

 Output Image

  <img height="300" width="300" src="https://lanl.github.io/LaGriT/assets/images/image2tn.gif">"116"
  "138"](image/image2.gif"> [cylindrical
  grid](image/image2.gif">

 Input file

 
*TEST createpts/rtz (lagrit\_input\_creatertz)

 cmo/create/3dmesh

 surface/outer/reflect/cylinder/0,0,0/1,0,0/1/

 region/r1/ le outer /

 mregion/m1/ le outer /

 createpts/rtz/4,6,11/0,0,0/3,,10/1,0,1/

 dump/gmv/output\_creatertz.gmv

 
* begin compare here

 cmo/status

 cmo/printatt//-all-/minmax

 quality

 finish

 
