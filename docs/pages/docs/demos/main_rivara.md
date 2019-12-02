
# Example negative_aij / rivara


Example 1: test mesh edges and boundary faces for negative coupling coefficients.

The objective is to use the negative_aij / rivara command that first identifies elements that have negative coupling coefficents, and then proceeds with a rivara boundary refinement to reduce or eliminate the negative couplings.

<img width="250" src="https://lanl.github.io/LaGriT/assets/images/rivara1.gif">
Input Mesh

<br>

<img width="250" src="https://lanl.github.io/LaGriT/assets/images/rivara2.gif">
Output Mesh

<br>
<br>

<img width="300" src="https://lanl.github.io/LaGriT/assets/images/rivara3.gif">
Output Mesh Refine Detail


## LaGriT Command File

```
cmo/create/cmo
surface/outside/reflect/box/0,0,0/3,2,1
region/all/le outside
mregion/all/le outside

createpts/xyz/3,3,3/0,0,0/3,2,1/1,1,1
createpts/xyz/2,2,2/.2,.05,.1/3,2,1/0,0,0
createpts/xyz/3,3,3/0,0,0/1.5,1.0 .5/1,1,1
createpts/xyz/2,2,2/.1,0,0/1.5,1.0 .5/0,0,0

# remove duplicate points
filter/1,0,0
cmo/setatt/cmo/imt/1,0,0/1

# set node material imt
setpts

# connect points into Delaunay mesh
connect
dump/gmv/output1.gmv

negative_aij/rivara/
negative_aij/eltset
dump/gmv/output2.gmv

# report results
cmo/status
cmo/printatt//-all-/minmax
quality

finish
```

## LaGriT Output 

The output log is very long with information for each command and refinement operation. The following shows the end outpu of the commands.

<pre class="lg-output">

cmo/printatt//-all-/minmax                                                      
ATTRIBUTE NAME              MIN               MAX         DIFFERENCE    LENGTH  
 -def-              0.000000000E+00  0.000000000E+00 0.000000000E+00       840  
 scalar                           1                1               0         1  
 vector                           3                3               0         1  
 nnodes                         840              840               0         1  
 nedges                           0                0               0         1  
 nfaces                           0                0               0         1  
 nelements                     2432             2432               0         1  
 mbndry                    16000000         16000000               0         1  
 ndimensions_topo                 3                3               0         1  
 ndimensions_geom                 3                3               0         1  
 nodes_per_element                4                4               0         1  
 edges_per_element                6                6               0         1  
 faces_per_element                4                4               0         1  
 isetwd                           0                0               0       840  
 ialias                           0              840             840       840  
 imt1                             0                1               1       840  
 itp1                             0               10              10       840  
 icr1                             0                1               1       840  
 isn1                             0                0               0       840  
 xic                0.000000000E+00  3.000000000E+00 3.000000000E+00       840  
 yic                0.000000000E+00  2.000000000E+00 2.000000000E+00       840  
 zic                0.000000000E+00  1.000000000E+00 1.000000000E+00       840  
 xtetwd                           0            26688           26688      2432  
 itetclr                          1                1               0      2432  
 itettyp                          5                5               0      2432  
 itetoff                          0             9724            9724      2432  
 jtetoff                          0             9724            9724      2432  
 itet                             3              840             837      2432x4
 jtet                             1         16000000        15999999      2432x4
 epsilon            1.000000004E-15  1.000000004E-15 0.000000000E+00         1  
 epsilonl           8.308148362E-13  8.308148362E-13 0.000000000E+00         1  
 epsilona           3.108624469E-12  3.108624469E-12 0.000000000E+00         1  
 epsilonv           1.332267630E-12  1.332267630E-12 0.000000000E+00         1  
 ipointi                        807              807               0         1  
 ipointj                        840              840               0         1  
 idebug                           0                0               0         1  
 itypconv_sm                      1                1               0         1  
 maxiter_sm                      25               25               0         1  
 tolconv_sm         1.000000000E+00  1.000000000E+00 0.000000000E+00         1  
 nnfreq                           1                1               0         1  
 ivoronoi                         1                1               0         1  
 iopt2to2                         2                2               0         1  
 xmin               0.000000000E+00  0.000000000E+00 0.000000000E+00         1  
 ymin               0.000000000E+00  0.000000000E+00 0.000000000E+00         1  
 zmin               0.000000000E+00  0.000000000E+00 0.000000000E+00         1  
 xmax               3.000000000E+00  3.000000000E+00 0.000000000E+00         1  
 ymax               2.000000000E+00  2.000000000E+00 0.000000000E+00         1  
 zmax               1.000000000E+00  1.000000000E+00 0.000000000E+00         1  
 kdtree_level                     0                0               0         1  
 max_number_sets                 64               64               0         1  
 number_of_psets                  0                0               0         1  
 number_of_eltsets                0                0               0         1  
 number_of_fsets                  0                0               0         1  
 ncon50                        2500             2500               0         1  
 nconbnd                          1                1               0         1  
 icontab                          0                2               2      2500  
 num_neg_coup_coef              140              140               0         1  
 neg_coup_coeff    -1.204850781E-01 -2.441406250E-04 1.202409374E-01       140  
 ietet_aij                        1             2401            2400       140x3
 
 Enter a command
quality                                                                         
 
epsilonl, epsilonaspect:   8.3081484E-13  5.7347268E-37                         
--------------------------------------------                                    
elements with aspect ratio < .01:                    2                          
elements with aspect ratio b/w .01 and .02:          4                          
elements with aspect ratio b/w .02 and .05:        105                          
elements with aspect ratio b/w .05 and .1 :        212                          
elements with aspect ratio b/w .1  and .2 :        347                          
elements with aspect ratio b/w .2  and .5 :       1107                          
elements with aspect ratio b/w .5  and 1. :        655                          
min aspect ratio =  0.3823E-02  max aspect ratio =  0.9883E+00                  
 
epsilonvol:   1.3322676E-12                                                     
---------------------------------------                                         
element volumes b/w  0.7629E-05 and  0.4307E-04:       123                      
element volumes b/w  0.4307E-04 and  0.2432E-03:       639                      
element volumes b/w  0.2432E-03 and  0.1373E-02:       854                      
element volumes b/w  0.1373E-02 and  0.7750E-02:       600                      
element volumes b/w  0.7750E-02 and  0.4375E-01:       216                      
min volume =   7.6293945E-06  max volume =   4.3750000E-02                      
-----------------------------------------------------------                     
      2432 total elements evaluated.                      


finish
LaGriT successfully completed                                                   
</pre>



