---
title: FSET 
tags: fset 
---

# FSET

---------------

 This command  is used to  define a  set  of element faces. Defining
 face sets can be useful for either defining boundary conditions,
 material interfaces, or surface subsets for further mesh processing.

 **FORMAT:**

  [fset ] / name / pset, get, pointsetname
  
  The fset name must be an integer or
  character string. Currently, only 32 named face sets may exist.
  However, any number of integer-numbered face sets (up to the total
  number of faces in the problem) may be defined. Face sets may be
  used in all of the usual ways that eltsets and psets may be used,
  e.g :
  
  mo/setatt/3dmesh/fluid\_structure **/fset**,**get**,blue/
 
  where fluid\_structure is the name of a face set attribute.
  
  **NOTE: All modules do not support use of fset.**

 EXAMPLE:

     fset / fname / pset, get psetname
