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

 Example 7: creatpts in a logically rectangular geometry

  The objective is to create a brick mesh using the
  **createpts/brick** command.
  A logically rectangular geometry is defined. The **createpts/brick**
  command is used

  to create a point distribution and mesh connectivity list. The
  output consists of one gmv file.

  

 Images of GMV output

     [<img height="300" width="300" src="/assets/images/image7tn.gif">"164"
 "167"](image/image7a.gif"> [View
 A](image/image7a.gif">[<img height="300" width="300" src="/assets/images/image7btn.gif">"225"
 "175"](image/image7b.gif"> [View B](image/image7b.gif">

  

  

  

 Input file

 
*TEST createpts/brick (lagrit\_input\_createbrick2)

 cmo/create/abc///hex

 quadxyz/5 7 5/ 0. 0. 0. /1. 0. 0./ 1. 0. 0.75 /0. 0. 1./ &

  0. 0.8 0./ 1. 1.25 0./ 1.0 1.0 1.2/ 0. 1. 0.8/

 setpts

 creatpts/brick/xyz/5,7,5/1,0,0/connect/

 settets

 dump/gmv/output\_createbrick2.gmv

 
* begin compare here

 cmo/status

 cmo/printatt//-all-/minmax

 quality

 finish
