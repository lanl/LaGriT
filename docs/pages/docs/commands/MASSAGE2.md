---
title: MASSAGE2
tags: massage2, refinement
--- 

# MASSAGE2

-----------------------


**`MASSAGE2`** iteratively calls [**`MASSAGE`**](MASSAGE.md) to refine adaptively according
 to a gradient field. Thus, the `bisection_length` option must be a field.

## SYNTAX

<pre>
<b>massage2</b> / infile_name / min_scale / bisection_length / merge_length / toldamage / &
[tolroughness] /[ifirst,ilast,istride]/ [<b>nosmooth**]/[<b>norecon**] &
[<b>strictmergelength</b> ]/[<b>ignoremats</b> ]/[<b>lite]</b> /[<b>checkaxy</b>]/[<b>semiexclusive</b>]/[<b>exclusive</b>]
</pre>
    

`infile_name` is a file which contains a set of LaGriT [**`infile`**](INPUT.md) commands that
 calculates the gradient field based on the distance field. In other words, the gradient field is a function of the distance field. It is necessary to have this file when using this routine, as the field must be updated after each refinement iteration. See sample infile below.
 
`min_scale` is the minimum length scale of the mesh (the minimum desired edge length).
  
See [**`MASSAGE`**](MASSAGE.md) for a full description of all command parameters.
 
 
## Creating user function infile for MASSAGE2

This file contains a set of LaGriT commands which calculate the gradient field for refinement based on the distance field.

 A file could be written like this:

      #user_function.mlgi
      #An example of calculating the gradient field F 
      # as a linear function of the distance field D
      
      #Define some coefficients for the function
      define / COEF_A /
      define / COEF_B /

      #Formula F = COEF_A times D + COEF_B
      #First remove any distance field that exists and recompute the distance field
      cmo / DELATT / mo_sink / dfield
      compute / distance_field / mo_sink / mo_src / dfield

      #Calculate F
      math / multiply / mo_sink / ref_field / 1,0,0 / mo_sink / dfield /COEF_A
      math / add / mo_sink / ref_field / 1,0,0 / mo_sink / ref_field /COEF_B

      finish

 The user does not have to put a floor value for the gradient field in
 this case (unlike in MASSAGE), as MASSAGE2 will calculate the floor
 value automatically. However, the minimum length scale 'min_scale'
 must be specified.
 
 The user must also create a node-based attribute for the gradient
 field before calling MASSAGE2. In the example above, attribute
 'ref_field' must already exist in the mesh object. The name of the
 field must also match the 'field_name' argument in the MASSAGE2
 command.
 



## EXAMPLES

```
massage2 / user_function.mlgi / 0.1 / ref_field / 1.e-5 / 1.e-5 / strictmergelength
```
Call massage refinement with commands defined in the infile user_function.mlgi using attribute ref_field.


