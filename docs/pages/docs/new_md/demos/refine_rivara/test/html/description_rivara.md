---
GENERATOR: 'Mozilla/4.05C-SGI [en] (X11; I; IRIX64 6.5 IP30) [Netscape]'
Generator: 'Microsoft Word 97/98'
Template: 'Macintosh HD:Microsoft Office 98:Templates:Web Pages:Blank Web Page'
title: '
*arguments:'
---

Example 1: test mesh edges and boundary faces for negative coupling
coefficients.

 The objective is to use the **negative\_aij / rivara** command that
 first identifies elements that have negative coupling coefficents, and
 then proceeds with a rivara boundary refinement to reduce or eliminate
 the negative couplings.

 The output consists of one gmv file.

Input

 [lagrit\_input\_rivara](../lagrit_input_rivara)

Images of GMV input and output

 The details of the coupling coefficient statistics can be found in the
 output log file.
[Input geometry <img height="300" width="300" src="https://lanl.github.io/LaGriT/assets/images/rivara1_tn.gif">"114"
"89"](image/rivara1.gif">
[Output geometry](image/rivara2.gif">
[(view A)<img height="300" width="300" src="https://lanl.github.io/LaGriT/assets/images/rivara2_tn.gif">"114"
"89"](image/rivara2.gif">
[Output geometry](image/rivara3.gif">
[(view B)<img height="300" width="300" src="https://lanl.github.io/LaGriT/assets/images/rivara3_tn.gif">"114"
"89"](image/rivara3.gif">
