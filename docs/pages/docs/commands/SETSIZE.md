---
title: setsize
tags: setsize
---

# SETSIZE

-------------------

**setsize** will set the mesh object attributes xmin,xmax,ymin,ymax,zmin,zmax from the xic,yic,zic values of all
'real' points (dudded and merged points will be ignored).

The variables **epsilonl**, **epsilona** and **epsilonv** are mesh object attributes; hence they may be different for all meshes in a given run. 
**epsilona**, **epsilonv** and **epsilonl** may be set by the user with the **[cmo/setatt](cmo/cmo_setatt.md)** command.  

setsize is called internally by some of LaGriT commands, especially those that add nodes to the mesh; **[copypts](COPYPTS.md)**, **[createpts](createpts.md)**, **[regpnts](REGNPTS.md)**, **[recon](RECON.md)**, **[scale](SCALE.md)**, and **[translate](TRANS.md)**

Many LaGriT algorithms use **epsilonl**; for example, if a node falls on a interface or boundary surface. It uses **epsilonv** to determine if a node
can be connected. Errors from **setpts** and **connect** may result if inconsistant or wrong values of epsilons are used.

## SYNTAX
<pre>
<b>setsize</b>
</pre>

If **epsilonv** is very small, it is set to **epsilona** 

**epsilonl** is set by a call to set_epsilon 
**epsilonl** is set to the square root of **epsilona** unless this number would be too small in which case **epsilonl** is to
```
(( xmax-xmin) + (ymax-ymin) + (zmax-zmin)) * 1.e=8/3
```


**epsilonv** = 
```
abs(xmax-xmin) * abs(ymax-ymin) * abs(zmax-zmin) * epsilonr * 1000
```

**epsilona** =
```
((xmax-xmin)**2 + (ymax-ymin)**2 + (zmax-zmin)**2) epsilonr * 1000
```

**epsilonr** is set at initialization time by:
``` 
     x2=one
     do i=1,1000
      x2=x2/two
      x1=one+x2
      if(x1.le.one) go to 11
     enddo

     11 epsilonr = x2*2.
```
where the values of 'one' and 'two' are obtained from the include file 'consts.h'.

The command [cmo/printatt](cmo/cmo_printatt.md) can be used to view any of these cmo attributes.
e.g. cmo/printatt/xmax


## EXAMPLES

```
read/avs/box.inp/ mo

setsize

cmo/printatt/mo/-all-/minmax                                                    
```
setsize will set the min and max attributes for coordinates and set the epsilon values. The report from the **printatt** command will look like:

<pre class="lg-output">

ATTRIBUTE NAME              MIN               MAX         DIFFERENCE    LENGTH  
 -def-              0.000000000E+00  0.000000000E+00 0.000000000E+00    132651  
 scalar                           1                1               0         1  
 vector                           3                3               0         1  
 nnodes                      132651           132651               0         1  
 nedges                           0                0               0         1  
 nfaces                           0                0               0         1  
 nelements                        0                0               0         1  
 mbndry                    16000000         16000000               0         1  
 ndimensions_topo                 3                3               0         1  
 ndimensions_geom                 3                3               0         1  
 nodes_per_element                4                4               0         1  
 edges_per_element                6                6               0         1  
 faces_per_element                4                4               0         1  
 isetwd                           0                0               0    132651  
 ialias                           0                0               0    132651  
 imt1                             2                2               0    132651  
 itp1                             0                0               0    132651  
 icr1                             0                0               0    132651  
 isn1                             0                0               0    132651  
 xic                0.000000000E+00  1.000000000E+00 1.000000000E+00    132651  
 yic                0.000000000E+00  1.000000000E+00 1.000000000E+00    132651  
 zic                0.000000000E+00  1.000000000E+00 1.000000000E+00    132651  
 0 length attribute: xtetwd                                                     
 0 length attribute: itetclr                                                    
 0 length attribute: itettyp                                                    
 0 length attribute: itetoff                                                    
 0 length attribute: jtetoff                                                    
 0 length attribute: itet                                                       
 0 length attribute: jtet                                                       
 epsilon            1.000000004E-15  1.000000004E-15 0.000000000E+00         1  
 epsilonl           3.845925373E-13  3.845925373E-13 0.000000000E+00         1  
 epsilona           6.661338148E-13  6.661338148E-13 0.000000000E+00         1  
 epsilonv           2.220446049E-13  2.220446049E-13 0.000000000E+00         1  
 ipointi                          1                1               0         1  
 ipointj                     132651           132651               0         1  
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
 xmax               1.000000000E+00  1.000000000E+00 0.000000000E+00         1  
 ymax               1.000000000E+00  1.000000000E+00 0.000000000E+00         1  
 zmax               1.000000000E+00  1.000000000E+00 0.000000000E+00         1  
 kdtree_level                     0                0               0         1  
 max_number_sets                 64               64               0         1  
 number_of_psets                  0                0               0         1  
 number_of_eltsets                0                0               0         1  
 number_of_fsets                  0                0               0         1  

</pre>
Note this mesh object does not have elements, therefore some element based attributes are 0 length.


