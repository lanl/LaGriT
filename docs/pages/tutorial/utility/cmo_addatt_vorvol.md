# Add Node attribute voronoi volume 

For a tet or tri mesh, an attribute can be added for each node giving the voronoi volume.
This uses the same routine used to calculate voronoi volumes for the FEHM stor file.
The commands for 3D tet and 2D tri are slightly different.

## 2D Voronoi node attribute

We do not have a voronoi area option for 2D except as represented each of the x y z components for the Voronoi areas formed by the nodes.  As long your surface is planar, you will get the values you want.
That is if your surface has normal in z direction, the z component will be your voronoi area.
 
In this example we create the attributes xvarea, yvarea, zvarea and write a truncated AVS file
which writes only node attributes. The file can be written for all nodes, or just a selected set.
 


After reading the tri surface into a mesh object named *mo_tri*:
```
# add attribute to save node id as attribute
# this is important if you subset or re-order nodes
cmo/set_id/mo_tri/ node / id_node
 
# create 3 attributes with voronoi areas
cmo/addatt/mo_tri/ voronoi_varea / xvarea yvarea zvarea
 
# display the min and max values of attributes
cmo printatt mo_tri -all- minmax
 
 
# turn off extra attributes for writing
cmo/modatt/mo_tri/ itp / ioflag / l
cmo/modatt/mo_tri/ isn / ioflag / l
cmo/modatt/mo_tri/ icr / ioflag / l
 
# write node attributes with avs flag
dump/avs/node_values.dat/ mo_tri 0 0 1 0
 
# find nodes along well and subset to the well nodes
# we copy nodes to new cmo to remove connectivity
cmo/create/motmp
copypts/motmp/mo_tri
 
cmo/select/motmp
pset/pwell/attribute imt/1,0,0/ eq 4
pset/pdel/ not pwell
rmpoint/pset,get,pdel
rmpoint compress
 
dump/avs/node_well_values.dat/ motmp 0 0 1 0

finish

```

The output AVS attribute file will look similar to this with attribute names first,
and the attribute values for rest of file. In this example
the triangle surface is xz, so voronoi areas are in yvarea attribute.

If you have a planar mesh, only one component of the xvarea, yvarea, zvarea will be non-zero. You can turn off all the attributes you don’t want so that when you write out the AVS attributes file it will just be a single column of floating point values, or perhaps more useful would be a file with two columns of output where column 1 would be the integer vertex id number and the second column would be the floating point area.
 
node_well_values.dat 
<pre class="lg-output"> 
         0          0          5          0          0
00005  1  1  1  1  1
imt1, integer 
id_node, integer 
xvarea, real 
yvarea, real 
zvarea, real 
   1   4   182  0.000000000000E+00 -0.100000000000E+05  0.000000000000E+00
   2   4   210  0.000000000000E+00 -0.100000000000E+05  0.000000000000E+00
   3   4   238  0.000000000000E+00 -0.100000000000E+05  0.000000000000E+00
   4   4   266  0.000000000000E+00 -0.100000000000E+05  0.000000000000E+00
</pre>
 
 
