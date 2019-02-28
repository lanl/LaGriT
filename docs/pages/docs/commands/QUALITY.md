---
title: QUALITY
tags: quality mesh metrics
---

# QUALITY #

**quality** provides a collection of mesh quality measurements. Together with commands in **cmo/addatt** a good summary of mesh metrics can be reported or used for further mesh optimization. 

## SYNTAX ## 

<pre>
<b>quality</b> / [quality_type] / [quality_type_options]
</pre>

Where *`quality-type`* can be **aspect, pcc, volume, angle, quad, or taylor**. The *`quality_type_options`* depend on the *`quality-type`* used. 


**`quality`** 

with no arguments writes to screen and outx3dgen or lagrit.out logfile giving volume and aspect ratio distribution information. Aspect ratios and element volumes are binned into 5 bins then totaled, min and max values are also reported. 

**`quality/aspect`** / [y] 

displays a count of the number of elements whose aspect ratio falls in each of 7 bins .  If y is specified create an attribute named aratio that will contain the value of the aspect ratio of each element. 

**`quality/edge_ratio`** / [y] 

displays a count of the edge length minimum/edge length maximum in each of 7 bins. If y is specified create an attribute named eratio that will contain the value of the min/max edge ratio of each element.

**`quality/edge_min`** / [y] 

displays a count of the minimum edge length in each of 7 bins. If y is specified create an attribute named edgemin that will contain the value of the min edge length of each element.

**`quality/edge_max`** / [y] 

displays a count of the maximum edge length in each of 7 bins. If y is specified create an attribute named edgemax that will contain the value of the max edge length of each element.

**`quality/angle`** / **gt** OR **lt** / value / 

displays a count of the number of elements with a dihedral angle that is greater than or less than the supplied value. See also **cmo/addatt/cmo/ang_** commands for dihedral angle and solid angle calculations.

**`quality/pc`c**

creates an element based attribute called 'neg_coup_coeff' which is a "negative coupling coefficient" indicator.  A value of 1 means the coupling coefficient is OK.  Anything less than 1 means it is negative.  This is  element attribute and is useful when viewing a mesh with GMV to find the negative coupling coefficients. 

**`quality/quad`**

generates some quality measures for quads and displays them after binning them into seven bins. Please see cmo / addatt // quad_quality for details on the quality measures used.

**`quality/taylor`**/ fieldname / value /

displays a count of the number of element-edge pairs with a taylor error estimate value whose absolute value is greater than the supplied value. 

Any combination of the quality_type_options may occur with these volume commands. For example: 
```
quality/volume/ number_of_bins / itetclr / lt | gt | eq | ne | xvalue / eltset,get,ename 
```

**`quality/volume`** / [number_of_bins] / [ **itetclr** ]

number_of_bins is an integer value controlling the number of bins in which to distribute the volume values for display. if number_of_bins is 0, then binning of distributed volumes is skipped, and only min and max volumes are reported. number_of_bins must be the 2nd argument to quality if used. 

itetclr is a keyword that will give volume information according to the values in the itetclr attribute. Number_of_bins applies to each tetclr value.  For each itetclr value, the volume of elements will be totaled. 

**`quality/volume`** / **gt** OR **lt** OR **eq** OR **ne** / value

will report volumes based on compare operator and given xvalue, for instance quality/volume/lt 0.0/ will report total number of elements with volumes less than 0.0 It may be used in combination of other volume options. if used with itetclr keyword, values will be reported by itetclr value 

**`quality/volume`** / **eltset, get,** eset_name

eltset,get,ename will report volumes on elements in defined eltset can be used in combination with previous options with operations done only on the chosen eltset. itetclr will still report for each of the values in itetclr.


## EXAMPLES ## 

```
quality
```
display summary of volume and aspect ratios

```
quality/aspect
```
display summary of aspect ratio distribution in 7 bins

```
quality/aspect/ y  	
```
create attribute **aratio** and display summary of aspect the aratio distribution

```
quality/angle/gt/179/
```
return count of elements with a dihedral angle > 179

```
quality/taylor/boron/1.e-10/	
```
run taylor error estimate and return count of element edge pairs with absolute error greater than 1.e-10

```
quality/volume	
quality/volume/ 2 
```
The first call will display summary of volumes with 5 bins, the second command will show summary with 2 bins

```
quality/volume/itetclr	
quality/volume /itetclr/lt .03	
quality/volume/itetclr/eltset,get,e2
```
All three commands will loop through itetclr (material values) and report a total volume for each itetclr value.
The second command will report element volumes lt .03 by the itetclr values.
The third command will report element volumes by itetclr values, but only for elements in the set e2











 
