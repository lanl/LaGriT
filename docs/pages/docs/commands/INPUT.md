---
title: INPUT INFILE
tags: infile input command 
---

# INPUT or INFILE

These commands instruct LaGriT to begin processing commands from a file. The **`infile`** commands may be nested. Each file  set of commands MUST be terminated with a **finish** command.

This works well with the **`define`** commands for multiple calls to the same command file.
 

## SYNTAX

<pre>
<b>infile</b> / file_name

<b>input</b> / file_name
</pre>

## EXAMPLES
```
loop / do / NL 1 10 1 / loop_end &
      infile lagrit.input_refine_smooth
```
Use **loop** command to run 10 iterations commands to smooth using file "lagrit.input_refine_smooth":
<pre class="lg-output">

pset / psmooth / geom / xyz / 1 0 0 / -3. -3. -3. / 3. 3. 3.
recon / 0
smooth / position / esug / pset get psmooth

finish
</pre>


```

* Well R-60   
define NORDER   149 
define WELLID   4600001 
define SCREENID 4811 
define MO_WELL mo 
define XC  495828.64 
define YC  539043.30 
define RTOP  1797.76 
define RBOT  1791.39 
define WLEN  6.37 
define R0   0.0 
define R1   56.0 
define NRAYS   8 
define NRADIAL 4 
define R_RATIO 2.0 
define WELLAVS 149_4600001_cyl.inp 

infile build_1well.mlgi
```
Set variables and build a cylinder well using commands in file "build_1well.mlgi":
<pre class="lg-output">
cmo / create / MO_WELL / / / tet
createpts/brick/rtz/NRADIAL,NRAYS,2/0. 0. 0. /R1,360.,WLEN/ &
            1,1,1/1 0 0/ R_RATIO 1.0 1.0

cmo/addatt/MO_WELL/wellid/VINT/scalar/nnodes//permanent/agfx/0
cmo/addatt/MO_WELL/screen/VINT/scalar/nnodes//permanent/agfx/0
cmo/addatt/MO_WELL/topbot/VINT/scalar/nnodes//permanent/agfx/0
cmo/setatt/MO_WELL/wellid WELLID
cmo/setatt/MO_WELL/screen SCREENID
cmo/setatt/MO_WELL/imt1 NORDER
cmo/setatt/MO_WELL/itetclr 1

cmo select MO_WELL
filter / 1 0 0
rmpoint / compress
resetpts / itp

cmo select MO_WELL
pset/pbot/attribute zic/1,0,0/ eq 0.
cmo setatt MO_WELL topbot 1
cmo setatt MO_WELL topbot pset,get,pbot -1

trans/1,0,0/ 0. 0. 0./XC YC RBOT

dump avs WELLAVS MO_WELL
cmo printatt MO_WELL imt minmax
cmo printatt MO_WELL topbot minmax
cmo printatt MO_WELL -xyz- minmax

finish
</pre>

