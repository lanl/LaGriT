---
title: QUALITY
tags: quality mesh metrics
---

# QUALITY #

**quality** provides a collection of mesh quality measurements. Together with commands in **cmo/addatt** a good summary of mesh metrics can be reported or used for further mesh optimization. See [Quality Measures](https://lanl.github.io/LaGriT/pages/docs/QUALITY_sliver_cap_needle_wedge.html).

## SYNTAX ## 

<pre>
<b>quality</b> / [quality_type] / [quality_type_options]
</pre>

## DESCRIPTIONS ##

The following are definitions for valid *`quality-type`* commands and their *`quality_type_options`*. 


**`quality`** 

with no arguments writes to screen and outx3dgen or lagrit.out logfile giving volume and aspect ratio distribution information. Aspect ratios and element volumes are binned into 5 bins then totaled, min and max values are also reported. 

**`quality/aspect`** / [y] 

computes the ratio of the radius of the circumsphere to the radius of the inscribed sphere of a tetrahedron. The ratio is multiplied by 3 so that a value of 1 indicates a regular tetrahedron. The display is a count of the number of elements whose aspect ratio falls in each of 7 bins.  Option y creates the element attribute named **aratio** that will contain the value of the aspect ratio for each element. Valid element types are tet and tri, hex, and quad (where the length of the hex or quad diagonals are used). 

**`quality/edge_ratio`** / [y] 

computes the ratio (shortest element edge/longest element edge) and displays a count of the min/max edge ratio in each of 7 bins. Option y creates the element attribute named **eratio** that will contain the value of the min/max edge ratio of each element.

**`quality/edge_min`** / [y] 

displays a count of the minimum edge length in each of 7 bins. Option y creates the element attribute named **edgemin** that will contain the value of the min edge length of each element.

**`quality/edge_max`** / [y] 

displays a count of the maximum edge length in each of 7 bins. Option y creates the element attribute named **edgemax** that will contain the value of the max edge length of each element.

**`quality/angle`** / **gt** OR **lt** / value / 

finds the max and min dihedral angles between adjacent faces (or 2D edges) of an element in radians. If the compare  option is used, it displays a count of the number of elements with a dihedral angle that is greater than or less than the supplied value. See also **cmo/addatt/cmo/ang_** commands for dihedral angle and solid angle calculations.

**`quality/pcc`**

creates an element based attribute called **neg_coup_coeff** which is a "negative coupling coefficient" indicator.  A value of 1 means the coupling coefficient is OK.  Anything less than 1 means it is negative.  This is useful when viewing a mesh to find where the negative coupling coefficients occur. 

**`quality/quad`**

generates some quality measures for quads and displays them after binning them into seven bins. Please see cmo / addatt // quad_quality for details on the quality measures used.

**`quality/taylor`**/ fieldname / value /

creates and displays a count of the number of element-edge pairs with a taylor error estimate value whose absolute value is greater than the supplied value. This creates the attribute **quality_taylor** with length in attribute **quality_taylor_len**.

**`quality/volume`** allows any combination of the *`quality_type_options`* for example: 
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

```
quality
quality/ aspect   / y
quality/ edge_max / y
cmo/addatt /cmotri /area_normal/ xyz/ anorm
cmo/addatt /cmotri /scalar /xnorm ynorm znorm / anorm
cmo/printatt /cmotri / aratio  minmax
cmo/printatt /cmotri / edgemax minmax
cmo/printatt /cmotri / znorm   minmax
```
Example calls using **quality** and **cmo/addatt** commands to show mesh quality statistics.

```
quality  

epsilonl, epsilonaspect:   1.4360051E-10  2.9612012E-30                        
--------------------------------------------                                   
elements with aspect ratio < .01:                    0                         
elements with aspect ratio b/w .01 and .02:          0                         
elements with aspect ratio b/w .02 and .05:          0                         
elements with aspect ratio b/w .05 and .1 :          0                         
elements with aspect ratio b/w .1  and .2 :      12020                         
elements with aspect ratio b/w .2  and .5 :     625891                         
elements with aspect ratio b/w .5  and 1. :     157053                         
min aspect ratio =  0.1053E+00  max aspect ratio =  0.7229E+00                 
epsilonvol:   1.1335396E-05                                                    
---------------------------------------                                        
element volumes b/w  0.1667E+02 and  0.3318E+02:    612924                     
element volumes b/w  0.3318E+02 and  0.6605E+02:     54612                     
element volumes b/w  0.6605E+02 and  0.1315E+03:     18204                     
element volumes b/w  0.1315E+03 and  0.2617E+03:     45510                     
element volumes b/w  0.2617E+03 and  0.5210E+03:     63714                     
element volumes b/w  0.2617E+03 and  0.5210E+03:     63714                     
min volume =   1.6666667E+01  max volume =   5.2100000E+02                     
-----------------------------------------------------------                    
    794964 total elements evaluated.    
```
Example call and report from the **quality** command with no arguments.


[Click Here For more Examples on Quality Measurements](https://lanl.github.io/LaGriT/pages/docs/QUALITY_sliver_cap_needle_wedge.html)












 
