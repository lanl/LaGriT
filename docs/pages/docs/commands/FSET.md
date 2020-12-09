---
title: FSET 
tags: fset 
---

# FSET

---------------

 This command  is used to  define a  set  of element faces. Defining
 face sets can be useful for either defining boundary conditions,
 material interfaces, or surface subsets for further mesh processing.

## SYNTAX

<pre>
<b>fset</b> / name / pset, get, pointsetname
</pre>
  
`name` must be an integer or
  character string. Currently, only 32 named face sets may exist.
  However, any number of integer-numbered face sets (up to the total
  number of faces in the problem) may be defined. 
  
 Face sets may be used in all of the usual ways that eltsets and psets may be used.
 
  
 NOTE: Not all modules support use of **fset.**

## EXAMPLES

```
fset / fname / pset,get, psetname
```

```
cmo/setatt/3dmesh/fluid_structure /fset,get,blue/
```
where fluid_structure is the name of a face set attribute



