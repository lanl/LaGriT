---
title: MASSAGE2
tags: ok
--- 

**MASSAGE2**

 **MASSAGE2** iteratively calls MASSAGE to refine adaptively according
 to a gradient field. Thus, the **bisection\_length** option must be a
 field.

 **FORMAT:**

 **massage2** / file\_name / min\_scale / bisection\_length / merge\_length / toldamage / [tolroughness] / [ifirst,ilast,istride]/
    
 [**nosmooth**]/[**norecon**][**strictmergelength**]/[**ignoremats**]/[**lite]**/[**heckaxy**]/[**semiexclusive**]**/[exclusive**]

 **file\_name** is a file which contains a set of LaGriT commands that
 calculates the gradient field based on the distance field. In other
 words, the gradient field is a function of the distance field. It is
 necessary to have this file when using this routine, as the field must
 be updated after each refinement iteration.
 
 **Creating user function file for MASSAGE2 routine**

 This file contains a set of LaGriT commands which calculate the
 gradient field for refinement based on the distance field.

 A file could be written like this:

      #user_function.mlgi
      #An example of calculating the gradient field **F** as a linear
      function of the distance field **D**
      #Define some coefficients for the function
      define / COEF_A /
      define / COEF_B /

      #Formula **F** = COEF_A * **D** + COEF_B
      #First remove any distance field that exists and recompute the
      distance field
      cmo / DELATT / mo_sink / dfield
      compute / distance_field / mo_sink / mo_src / dfield

      #Calculate **F**
      math / multiply / mo_sink / ref_field / 1,0,0 / mo_sink / dfield /
      COEF_A
      math / add / mo_sink / ref_field / 1,0,0 / mo_sink / ref_field /
      COEF_B

      finish

 The user does not have to put a floor value for the gradient field in
 this case (unlike in MASSAGE), as MASSAGE2 will calculate the floor
 value automatically. However, the minimum length scale 'min\_scale'
 must be specified.
 
 The user must also create a node-based attribute for the gradient
 field before calling MASSAGE2. In the example above, attribute
 'ref\_field' must already exist in the mesh object. The name of the
 field must also match the 'field\_name' argument in the MASSAGE2
 command.
 
 **min\_scale** is the minimum length scale of the mesh (the minimum
 desired edge length).
 
 See [**MASSAGE**](MASSAGE.md "MASSAGE") for other arguments.

 EXAMPLE:

      massage2 / user_function.mlgi / 0.1 / ref_field / 1.e-5 / 1.e-5 / strictmergelength

