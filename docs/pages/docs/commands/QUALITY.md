---
title: QUALITY
tags: quality mesh metrics
---

# QUALITY #

------------------------

**`quality`** provides a collection of mesh quality measurements. Together with commands in **`cmo/addatt`** a good summary of mesh metrics can be reported or used for further mesh optimization. For quality measurments to characterize tet elements by various types, see [Quality Measures](https://lanl.github.io/LaGriT/pages/docs/QUALITY_sliver_cap_needle_wedge.html).

## SYNTAX ## 

<pre>
<b>quality</b> / [quality_type] / [quality_type_options]
</pre>


## DESCRIPTIONS ##

The following are definitions for valid *`quality-type`* commands and their *`quality_type_options`*. 


**`quality`** 

with no arguments writes to screen and outx3dgen or lagrit.out logfile reports on the min, max, and counts of aspect ratios and volumes. The aspect ratios are displayed with 7 bins (log scale) with counts of values between .01 and 1. The element volumes are displayed with 5 bins with counts of values between the min and max volumes.

**`quality/aspect`** / [y] 

computes the ratio of the radius of the circumsphere to the radius of the inscribed sphere of a tetrahedron. The ratio is multiplied by 3 so that a value of 1 indicates a regular tetrahedron. The display is a binned count of the number of elements whose aspect ratio falls in each of 7 bins (log scale between .01 and 1).
**y** option creates **aratio** mesh element attribute containing the value of the aspect ratio for each element. Valid element types are tet and tri, hex, and quad (where the length of the hex or quad diagonals are used). 

**`quality/edge_ratio`** / [y] 

computes the ratio (shortest element edge/longest element edge) and displays a binned count of the min/max edge ratio in each of 7 bins (log scale between .01 and 1). **y** option creates **eratio** mesh element attribute containing the value of the min/max edge ratio for each element.


**`quality/edge_min`** / [y] 

displays a binned count of the minimum edge lengths displayed log scale between .01 and 1. The binned values are normalized by dividing with the max value. The min and max of the mininum edge length for each element are also reported.
**y** option creates **edgemin** mesh element attribute containing the value of the min edge length of each element.



**`quality/edge_max`** / [y] 

displays a binned count of the maximum edge lengths displayed log scale between .01 and 1. 
The binned values are normalized by dividing with the max value. The min and max of the maximum edge length for each element are also reported.
**y** option creates **edgemax** mesh element attribute containing the value of the max edge length of each element.


**`quality/minsine`** / [y] 

computes, for each tet, the smallest sine of its six dihedral angles (the angles between pairs of faces that share an edge). For the edge of length *L* shared by faces with areas *A1* and *A2* in a tet of volume *V*, the sine of the dihedral angle at that edge is 3 *V L* / (2 *A1 A2*). Because sin(&theta;) is small both when &theta; is near 0&deg; and when it is near 180&deg;, the min sine catches every kind of poorly shaped tet in one number: slivers, caps, needles and wedges all have a min sine near 0. A regular tet has min sine 0.9428 (all dihedral angles 70.53&deg;); a value of 0.1736 means the worst angle is 10&deg; or 170&deg;; 0.5 means 30&deg; or 150&deg;. Large dihedral angles matter for accuracy (a-priori error estimates for elliptic problems typically grow like 1/sin(angle)) and small ones for conditioning, so min sine is a common objective for mesh improvement (Klingner &amp; Shewchuk, "Aggressive Tetrahedral Mesh Improvement", 16th International Meshing Roundtable, 2007). The absolute volume is used, so an inverted tet gets the value of its mirror image; a flat tet gets 0. The display is a binned count in 7 bins (log scale between .01 and 1) and the min and max values.
**y** option creates **minsine** mesh cell attribute containing the min sine of each cell. Only tet cells are valid; other cell types get 0 and a warning with their count.

Note: one **quality** command computes one of **aspect**, **edge_ratio**, **edge_min**, **edge_max** and **minsine** (the first one given). Use a separate command for each.


**`quality/angle`** / **gt** OR **lt** / value / 

finds the max and min dihedral angles between adjacent faces (or 2D edges) of an element in radians. If the compare  option is used, it displays a count of the number of elements with a dihedral angle that is greater than or less than the supplied value. See also **cmo/addatt/cmo/ang_** commands for dihedral angle and solid angle calculations.

**`quality/pcc`**

creates an element based attribute called **neg_coup_coeff** which is a "negative coupling coefficient" indicator.  A value of 1 means the coupling coefficient is OK.  Anything less than 1 means it is negative.  This is useful when viewing a mesh to find where the negative coupling coefficients occur. 


**`quality/quad`**

generates some quality measures for quads and displays them after binning them into seven bins. For details on the quad quality measures see [cmo/addatt/ quad_quality](https://lanl.github.io/LaGriT/pages/docs/commands/cmo/cmo_addatt.html).


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
quality/aspect
```
display summary of aspect ratio distribution in 7 bins

```
quality/aspect/ y  	
```
create attribute **aratio** and display summary of aspect the aratio distribution

```
quality/minsine/ y
cmo/printatt/ -def- / minsine / minmax
eltset / e_poor / minsine / lt / 0.1736
```
create attribute **minsine**, display its distribution, and make a cell set (eltset) of the tets with a dihedral angle below 10&deg; or above 170&deg; (min sine below sin 10&deg; = 0.1736)

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
```
The  default with no arguments will calculate and display element volumes and aspect ratios for the current mesh object.

Output for the quality command with no arguments:

<pre class="lg-output">
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
</pre>


[Click Here For more Examples on Quality Measurements](https://lanl.github.io/LaGriT/pages/docs/QUALITY_sliver_cap_needle_wedge.html)












 
