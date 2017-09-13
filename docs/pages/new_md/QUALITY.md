---
GENERATOR: 'Mozilla/4.79C-SGI [en] (X11; U; IRIX64 6.5 IP30) [Netscape]'
Generator: Microsoft Word 98
title: QUALITY
---

 

 **QUALITY**

  quality provides a collection of mesh quality measures

 

 **FORMAT:**

**quality **/[quality\_type] / [quality\_type options]

Where quality-type can be **aspect**, **pcc**, **volume**, **angle** or
**taylor** Quality-type options depends on quality-type.

**quality** (no arguments)

writes to screen and outx3dgen logfile giving volume and aspect ratio
distribution information. Aspect ratios and element volumes are binned
into 5 bins then totaled, min and max values are also reported.

**quality/aspect**/[**y**]

displays a count of the number of elements whose aspect ratio falls in
each of 7 bins .  If **y** is specified create an attribute named aratio
that will contain the value of the aspect ratio of each element.

**quality****/angle****/gt****lt**/value]/

displays a count of the number of elements with a dihedral angle that is
greater than or less than the supplied value.

**quality****/pcc**

creates an element based attribute called 'neg\_coup\_coeff' which is a
"negative coupling coefficient" indicator.  A value of 1 means the
coupling coefficient is OK.  Anything less than 1 means it is negative. 
This is  **element** attribute and is useful when viewing a mesh with
GMV to find the negative coupling coefficients.

**quality****/taylor**/fieldname/value/

displays a count of the number of element-edge pairs with a taylor error
estimate value whose absolute value is greater than the supplied value.

**quality/volume**

quality/volume can appear with or without any of the following options:

**quality****/volume** / **** number\_of\_bins

number\_of\_bins is an integer value controlling the number of bins in
which to distribute the volume values for display. if number\_of\_bins
is 0, then binning of distributed volumes is skipped, and only min and
max volumes are reported. number\_of\_bins must be the 2nd argument to
**quality** if used.

**quality/volume****/itetclr**

 output element volume distributions with 5 bins. loop through
**itetclr** values and report total volume for each material

**quality****/volume**/number\_of\_bins**/itetclr**

**itetclr** is a keyword that will give volume information according to
the values in the itetclr attribute. Number\_of\_bins applies to each
tetclr value.  For each itetclr value, the volume of elements will be
totaled.

**quality/volume****/lt****gt****eq****ne**/xvalue

will report volumes based on compare operator and given xvalue, for
instance quality/volume**/lt** 0.0/ will report total number of elements
with volumes less than 0.0 It may be used in combination of other volume
options. if used with itetclr keyword, values will be reported by
itetclr value

**quality/volume****/eltset**,**get**,ename

eltset,get,ename will report volumes on elements in defined eltset can
be used in combination with previous options with operations done only
on the chosen eltset. itetclr will still report for each of the values
in itetclr .

Any combination of quality\_type options may occur with the **volume**
quality\_type, for example:

**quality****/volume**/number\_of\_bins**/itetclr****/lt** **gt** 
**eq**  **ne**  xvalue**/eltset**,**get**,ename.

 

**EXAMPLES:**

  ------------------------------------------------------ -------------------------------------------------------------------------------------------------------------------------------------------------------------
  **quality**/                                           display volume and aspect ratio
  **quality****/aspect**/                                display aspect ratiodistribution in 7 bins
  **quality****/aspect****/y**/                          display aspect ratio distribution and add attribute named aratio
  **quality****/angle****/gt**/179/                      return count of elements with a dihedral angle &gt; 179.
  **quality****/taylor**/boron/1.e-10/                   run taylor error estimate and return count of element edge pairs with absolute error greater than value
  **quality****/volume**                                 output element volume distribution with 5 bins
  **quality****/volume** / 2                             output element volume distribution with 2 bins
  **quality****/volume****/itetclr**                     loop through **itetclr** values and report total volume for each material
  **quality****/volume****/lt .03**                      count and report element volumes lt .03
  **quality****/volume** **/itetclr****/lt .03**         count and report element volumes lt .03 by **itetclr** value
  **quality****/volume****/eltset**,**get**,e2           report on element volumes only for those in the set e2
  **quality****/volume****/itetclr****/eltset,get,e2**   report on element volumes only for those in the set e2, loop through each of the **itetclr** values, report total volume by material for elements in set e2
  ------------------------------------------------------ -------------------------------------------------------------------------------------------------------------------------------------------------------------

 

 
[Click here for demos](demos/quality_pcc/test/md/main_qual.md)
