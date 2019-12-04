---
title: REFINE2D
tags: refine2d
--- 

# REFINE2D

---------------------------

This routine deterministically refines a triangle by bisecting all the edges and connect these bisection points to form new triangles.  
Thus, after calling **`REFINE2D`** once, a triangle will be tessellated into 4 triangles.

## SYNTAX

<pre>
<b>refine2d</b>
<pre>

## Examples

The following command file will refine an input triangulation for resampling.

```
read / avs / generic_source.inp / mo_z
refine2d
refine2d
refine2d
refine2d
refine2d

read / avs / generic_template.inp / mo_sink

cmo / addatt / mo_z / zsave / vdouble / scalar / nnodes
cmo / addatt / mo_sink / zsave / vdouble / scalar / nnodes
cmo / copyatt / mo_z / mo_z / zsave / zic

cmo / setatt / mo_z / zic / 1 0 0 / 0.0
cmo / setatt / mo_sink / zic / 1 0 0 / 0.0
interpolate / continuous / mo_sink / zsave / 1 0 0 / &
              mo_z /    zsave / nearest zsave

cmo / copyatt / mo_sink / mo_sink / zic / zsave
cmo / setatt / mo_sink / itetclr / 1 0 0 / 1

dump / gmv / generic_out.gmv / mo_sink

finish
```
