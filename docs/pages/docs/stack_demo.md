# EXAMPLES: stack / layers #

<img height="300" width="300" src="https://lanl.github.io/LaGriT/assets/images/simple_stack.gif">

<img height="300" width="300" src="https://lanl.github.io/LaGriT/assets/images/simple_tet.gif">

## Example 1 ##

no buffers, no refinement, no truncation<br>
STACK TRI LAYERS into cmo name cmo1.
Read 4 AVS triangulated surfaces into 3 materials.
Stack and pinchout if layers cross.
Label material colors with 1 on bottom and 3 on top.

The syntax **stack/layers/avs** means stack a list of AVS triangle surface files.
This is followed by a list of files with integers after each file,
the first file in the list is the bottom, the last is the top.
The first integer after each file name is the material value for cells between layers.
(a second integer would be the refinement number, default is 0)


<pre>
cmo create cmo1
stack/layers/avs/ &
     surf-12.inp 1 / surf-5.inp 2/ surf5.inp 3/ surf25.inp   
</pre>

Screen output:

<pre>
cmo create cmo1
stack/layers/avs surf-12.inp 1 surf-5.inp 2 surf5.inp 3 surf25.inp 
Layers to create:          4
Max material number:          4
Reading     4 surface files...
................................................................
         surface name  layer color type llcorner  zic
          surf-12.inp     1    1   -1         1)  -1.200000E+01
           surf-5.inp     2    2    0        37)  -5.000000E+00
            surf5.inp     3    3    0        73)   5.000000E+00
           surf25.inp     4    4   -2       109)   2.500000E+01
Elements per layer:         48 total:     192
Nodes    per layer:         36 total:     144
STACK DONE:         4 files read out of     4
................................................................
</pre>
 

The following commands convert the stacked 2D layers into 3D volumes.
In this example, triangles are stacked into prism elements. Each of the prism
elements are then converted into 6 tet elements.
The command boundary components will check for 1 single boundary.

<pre>
stack/fill/cmopri/cmo1/
hextotet/6/ cmotet / cmopri
boundary_components
finish
</pre>

Screen output:

<pre>
1 different boundary components identified.
2 is a representative vertex
</pre>

 
## Example 2 ##

<img height="300" width="300" src="https://lanl.github.io/LaGriT/assets/images/simple_tet_clr2.gif">

Stack/layers with material colors reversed. <br>
Material 1 (green) is now the top unit. Material 2 (purple) is middle and 1 (red) is bottom.

The unit value was changed from above example on the command line:

<pre>
cmo/create/cmo1
stack/layers/avs/ surf-12.inp 3/ surf-5.inp 2/ surf5.inp 1/ surf25.inp 1
</pre>

Screen output:

<pre>
................................................................

         surface name  layer color type llcorner  zic
          surf-12.inp     1    3   -1         1)  -1.200000E+01
           surf-5.inp     2    2    0        37)  -5.000000E+00
            surf5.inp     3    1    0        73)   5.000000E+00
           surf25.inp     4    1   -2       109)   2.500000E+01

Elements per layer:         48 total:        192
Nodes    per layer:         36 total:        144
STACK DONE:         4 files read out of         4
................................................................
</pre>


## Example 3 ##

<img height="300" width="300" src="https://lanl.github.io/LaGriT/assets/images/hex_buffers.jpg">

<img height="300" width="300" src="https://lanl.github.io/LaGriT/assets/images/hex_buffers.jpg">


Stack quad layers and fill with hex elements, with buffered interfaces. <br>
Read 6 AVS quad surfaces into 5 material units.
Label unit colors with 1 on bottom and 5 on top.
Stack and pinchout if layers are closer than .19 meters.
Add a buffer layer .2 meters above and below each interface.
Truncate by 5th file to read.
Note the *`&`* character added so command line is not too long to read.

<pre>
cmo/create/cmo1
stack/layers/avs &
  q-1.inp   1   &
  q0.inp    2   &
  qhole.inp 3   &
  q3.inp    4   &
  qtop.inp  5   &
  q5.inp    5   &
  truncate 5 / buffer .2 / pinch .19
  
 stack/fill/cmohex/cmo1
 </pre>
 
Screen output:

<pre>
................................................................
         surface name  layer color type llcorner  zic
              q-1.inp     1    1   -1         1)  -1.000000E+00
               buffer     2    1    1        34)  -2.000000E-01
               q0.inp     3    2    0        67)   0.000000E+00
               buffer     4    2    1       100)   2.000000E-01
               buffer     5    2    1       133)   8.000000E-01
            qhole.inp     6    3    0       166)   1.000000E+00
               buffer     7    3    1       199)   1.000000E+00             
               buffer     8    3    1       232)   1.000000E+00
               q3.inp     9    4    0       265)   1.000000E+00
               buffer    10    4    1       298)   1.000000E+00
               buffer    11    4    1       331)   1.000000E+00
             qtop.inp    12    5    0       364)   1.000000E+00
               buffer    13    5    1       3)   1.200000E+00
               q5.inp    14    5   -2       430)   5.000000E+00
Elements per layer:         20 total:        280
Nodes    per layer:         33 total:        462
Layers truncated by qtop.inp layer        12
STACK DONE:         6 files read out of         6
STACK DONE:         8 layers created for total        14
................................................................
</pre>


## Example 4 ## 

<img height="300" width="300" src="https://lanl.github.io/LaGriT/assets/images/tet_trunc_refine.jpg">

<img height="300" width="300" src="https://lanl.github.io/LaGriT/assets/images/tet_trunc_refine_exp.jpg">


tri layer to prism then to tet with truncating surface and layer refinement. <br>
STACK TRI LAYERS into cmo named cmo1.
Read 5 AVS triangulated surfaces into 4 material units.
Label unit colors with 1 on bottom and 3 on top.
Stack and pinchout if layers cross.
Add two layers for refinement in second unit.

Note layer type attribute:
* -1 is bottom, -2 is top surface
* 0  is surface read from file
* 1  is layer derived between surface at constant distance
* 2  is layer derived between surface at proportional distance

<pre>
cmo create cmo1
stack/layers/avs       &
  surf-12.inp      1   &
  surf-5.inp       2   &
  surf5.inp        3 2 &
  surf2_slope.inp  4   &
  surf25.inp       4   &
  truncate 4 / pinch 0.
  
stack/fill/ cmopri /cmo1
hextotet// cmotet / cmopri
</pre>
  
Screen output:

<pre>
................................................................
         surface name  layer color type llcorner  zic
          surf-12.inp     1    1   -1         1)  -1.200000E+01
           surf-5.inp     2    2    0        37)  -5.000000E+00
               refine     3    2    2        73)  -1.666667E+00
               refine     4    2    2       109)   1.666667E+00
            surf5.inp     5    3    0       1)   5.000000E+00
      surf2_slope.inp     6    4    0       181)   1.800000E+01
           surf25.inp     7    4   -2       217)   2.500000E+01
Elements per layer:         48 total:        336
Nodes    per layer:         36 total:        252
Layers truncated by surf2_slope.inp layer         6
STACK DONE:         5 files read out of         5
................................................................
</pre>
 

 

