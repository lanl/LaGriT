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

 Example 1: createpts in a rectangular geometry

  The objective is to add points to a rectangular mesh using the
  **createpts** command.
  A rectangular geometry is defined. The **createpts/xyz** command is
  used to create a point distribution within the geometry exercising
  various options available to the command. The output consists of one
  gmv file.

  [](../lagrit_input_createxyz) 

 Images of GMV output

  [<img height="300" width="300" src="/assets/images/image1tn.gif">"112"
  "87"](image/image1.gif">[rectangular grid](image/image1.gif">

   

   

 Input file

 
*TEST createpts/xyz (lagrit\_input\_createxyz)

 cmo/create/3dmesh

 surface/outer/reflect/box/0,0,0/1,1,1

 region/r1/ le outer /

 mregion/m1/ le outer /

 createpts/xyz/5,3,4/0,0,0/1,1,1/1,1,1/1,0,0/1.5,1,1/

 setpts

 dump/gmv/output\_createxyz.gmv

 
* begin compare here

 cmo/status

 cmo/printatt//-all-/minmax

 quality

 finish
