---
title: INTERPOLATE
tags: map continuous voronoi nearest node 
---

# INTERPOLATE

------------------

The **`interpolate`** (or **`intrp`**) command is used to interpolate
attribute values from nodes or elements of a source mesh to node or
element attributes of a sink mesh. Normally the source grid is coarser or the same resolution as the sink grid.
Consider using [**`upscale`**](UPSCALE.md) to assign values from high resolution to coarse resolution.

Note: **`interpolate/map`** replaces the command **`doping/integer1`** which copied source itetclr values
to sink imt values. **`interpolate/voronoi`** replaces **`doping/integer2`** which copied nearest node source imt to sink imt.
**`interpolate/continuous`** evolved from the **`doping/table`** command. These
commands are still similar to the old versions except that this interpolation has been updated 
to include attribute selections and expanded options.


## SYNTAX

<pre>
<b>interpolate</b> / intrp_method / cmosink, attsink / 1,0,0 / cmosrc, attsrc / [tie_option] [flag_option] [keep_option] [intrp_function]

<b>interpolate</b> /  <b>map</b> or  <b>voronoi</b> or  <b>continuous</b> or default &
  /cmosink, attsink / 1,0,0 /cmosrc, attsrc/ &
  [ <b>tiemin</b> or <b>tiemax</b> ] [ flag_value or   <b>plus1</b> ] &
  [<b>nearest,</b> node_attribute ] [<b>keepatt</b> or <b>delatt</b> ]  [ intrp_function ]

</pre>



## PARAMETER DESCRIPTIONS


`intrp_method` defines the method of interpolation used from mesh object cmosrc to mesh object cmosink.

**`map`** method copies the value from the enclosing source element to sink node or element (centroid). 
Sink nodes located outside the source elements are tagged with values according to flag options. 
If undefined, the flag value will be a value 1 greater than the max source attribute values. To copy from a source of type node, use voronoi method.

**`voronoi`** copies the value from the nearest source node to the sink node. By selecting the nearest source points, 
Voronoi regions are generated around each sink point. The resulting sink point (node or centroid) is given the value of the 
attribute associated with the Voronoi generating point whose Voronoi cell the sink point lands in. The outside flag options
do not apply for this method, even if a sink point is outside the source, a nearest node will be found.


**`continuous`** interpolates values from the enclosing source element nodes to the sink nodes (or centriod of element). 
The interpolation is the sum of vertice values multiplied by the relative volume of elements formed by the sink point 
location on or inside the found element. The element is divided into volumes as determined by the sink point location 
and its relationship to the vertices of the enclosing element. A triangle becomes three triangles each with a vertices 
on the sink point. A quad becomes four quads.  A tet becomes four tets. The assigned sink point value is the sum of 
these values divided by the number of element vertices. The interpolation function belonging to the attribute is applied 
to the vertice values before being summed. See also the `interp_function` option.
**WARNING** A hex becomes 8 hexs which depends on orthogonal hexs and so is not currently supported. 
Use hextotet to convert hex elements to tets. 

**`default`** - If source attribute is element type then use `map`. If source attribute is node type then use `continuous`.


`cmosink`, `attsink` are the sink mesh object name and attribute to write interpolated values to. If the sink attribute is element type, centroids are calculated for each element and these are used for the interpolation methods. 


`indexed_set` 1,0,0 (start,stride,stop) or pset,get,pset_name or eltset,get,eltset_name are the set of sink nodes or elements to write interpolated values to. 1,0,0 will select all sink nodes or elements. 


`cmosrc`, `attsrc` are the source mesh object name and attribute values interpolating from.



The following parameters are optional on the command line:


`tie_option` is used to break a tie when a sink point has more than one valid candidate source node or element. 
Along with kdtree search, nearestpoint() and retrieve_within_eps() routines return a list of
candidate objects for a sink point. These can be either a list of
closest points, or a list of elements the point is on or inside.
`tie_option` chooses one candidate from the possible candidates. The
result is a one-to-one correspondence with each sink point paired with
a single source node or a single source element. See `keep_option` for saving attributes with this correspondence.

*  **tiemax** selects the maximum value from candidate nodes or
  elements. This is the default selection.

*  **tiemin** selects the minimum value from candidate nodes or
  elements.
  

`flag_option` is used to assign an error value, or to assign a value for points not inside the source mesh.
These flag values indicate either that there was an error and a value could
not be written to the sink attribute. The kdtree element search will
assign a flag value if a sink point is located outside the source
grid. *This does not apply to Voronoi method which finds nearest node and does not depend on finding an enclosing element.*

* **plus1** will assign a flag value of maximum source value plus 1, this is the default behavior.

* **nearest** `node_attribute` will find the nearest source node and
 use the node's attribute value as the flag value. The keyword
 **nearest** must be followed with the name of the source attribute name to be used for the flag values.

* `flag_value` given as an integer or real value will use this numeric value for the flag assignments.


`keep_option` is useful during multiple calls to **interpolate** to use attributes pt_gtg and el_gtg instead of creating these search attributes for every call. The sink and source mesh objects and their index selection must not change.
The `interpolate` command uses kdtree and candidate searches to create sink
attributes that pair sink points to associated source node or element. By using the keyword 'keepatt' these
attributes are not deleted. On subsequent calls to
interpolate these attributes are used to look up associated node or element
numbers.  If **map** or **continuous** methods are used, the element
attribute named el_gtg will be created. If **voronoi** or the
flag_option **nearest** are used, the node attribute named pt_gtg
will be created. The keyword **delatt** deletes any attributes created during the kdtree searches. By default these attributes are removed.
 

`intrp_function` replaces the interpolation function associated with
the sink attribute. This interpolation function is applied to the
final interpolated field value. Valid interpolate functions are
linear, asinh, log, copy, sequence, min, incmin, max, incmax, and, or,
user. Functions such as min and max pass the interpolation value
unchanged.


### DEBUG SYNTAX
<pre>
<b>cmo/setatt/</b> cmosink / <b>idebug</b> debug_level
</pre>
For debugging purposes, setting **idebug** will output additional information from the interpolate routines. The integer value 'debug_level' is used to control the amount reported, 0 is no debug (default), 1 is minimum output and triggers calls to mmverify(), 5 is the middle, 9 and greater is verbose and includes information on every point and or element in the mesh.



## Table 1 Attribute Types for Interpolate

This table indicates the type of attributes that can be used with the interpolation methods. 

| **Method**       | **Sink**     | **Source**   
| :---------------- | :-------------| :------------- 
| **map**          | node         | element 
| **map**          | element      | element
| **continuous**   | node         | nodes (on element)
| **continuous**   | element      | nodes (on element)
| **voronoi**      | node         | node
| **voronoi**      | element      | node

 

## Table 2 Interpolation Methods and Options

This Table shows supported applications for each of the interpolation methods.

(parenthesis) means the option should work, but may be undefined.

NOT indicates Not Supported


|       OPTIONS                |             **MAP**            |         **CONTINUOUS**         |         **VORONOI**            |
| :---------------------------| :--------------------------- | :-------------------------- | :---------------------------- |
| **source element type**      | tri, quad, hex, tet, (pyr), (pri), (line)        | tri, quad, NOT hex, tet, (pyr), (pri), (line)    | tri, quad, hex, tet, (pyr), (pri), (line), (pnt)  |
| **sink element type**        | tri, quad, hex, tet, (pyr), (pri), (line), (pnt) | tri, quad, hex, tet, (pyr), (pri), (line), (pnt) | tri, quad, hex, tet, (pyr), (pri), (line), (pnt)  |
| **source attribute**         | element                                          | nodes (on element)                                           | node                                  |
| **sink attribute**           | node or element (centroid)                       | node or element (centroid)                       | node or element (centroid)                        |
| **source attribute type**    | integer or double                                | integer or double                                | integer or double                                 |
| **sink attribute type**      | integer or double                                | double, NOT integer                              | integer or double                                 |
| **interp function**          | linear, log, sinh, all others pass unaltered     | linear, log, sinh, all others pass unaltered     | linear, log, sinh, all others pass unaltered      |
| **tie option**               | tiemin or tiemax                | tiemin or tiemax           | tiemin or tiemax                                  |
| **flag option**              | plus1, nearest, or user value   | plus1, nearest, or user value       | plus1 or user value                               |
| **keepatt option**           | element attribute `el_gtg`          | element attribute `el_gtg`        | node attribute `pt_gtg`                |




## Examples

```
interpolate / map / cmo_sink imt /1,0,0/ cmo_src itetclr
```
For each node in cmo_sink find an enclosing element from mesh
cmo_src. Assign the element's itetclr value to the corresponding
imt attribute of `cmo_sink`. For sink points outside of the source elements, a value 1 greater than itetclr max value is assigned.

```
interpolate / voronoi / cmo_sink itetclr /1,0,0/ cmo_src imt / keepatt
```
For each element centroid in cmo_sink find nearest node in cmo_src. Assign the source node imt value to the corresponding
itetclr attribute of `cmo_sink`. The keepatt option keeps the sink attribute pt_gtg with nearest node values for each element centroid.

```
interpolate / map / cmo_sink Pval /1,0,0/ cmo_src Vval / tiemin, log
```
This command will assign source Vval values to sink Pval for
elements enclosing cmo_sink points. If the sink point is found
within more than one element, the min value of the candidate
elements will be chosen. Since the interpolation function "log" is
named, it will be applied to the source Vval value before being
written to sink attribute Pval.

```
interpolate/ voronoi / cmo_sink imt /1,0,0/ cmo_src imt
```
For each node in cmo_sink, find the closest node in cmo_src.
Assign the imt value from the closest cmo_src node to the imt
attribute of cmo_sink.

```
interpolate/ continuous / cmo_sink xval /1,0,0/ cmo_src Pv
```
For each node in cmo_sink, find a cmo_src element the node is
inside. Interpolate the element node values in Pv on to the sink
point and write to the sink attribute xval.

```
interpolate/ map /cmo_sink imt /1,0,0/ cmo_src itetclr / nearest, imt / keepatt

interpolate/ map /cmo_sink imtreal /1,0,0/ cmo_src itetreal / nearest, imtreal
```

The first call to interpolate will assign itetclr values from source
elements to imt in the sink cmo for points inside the source
elements. Any sink point not inside the source grid will be assigned
the imt value of the nearest source point. Since keepatt is set,
both attributes `pt_gtg` and `el_gtg` will be kept as sink cmo
attributes and hold the node and element numbers for each sink
point.

The second call to interpolate will find the sink attributes `pt_gtg`
and `el_gtg`. The nearest point and enclosing element kdtree searches
will be skipped. This time the element value in attribute itetreal
will be assigned to the sink node attribute imtreal. For points
outside the grid, values from nearest node attribute imtreal will be
used. Note that the **`delatt`** keyword does not have to exist, the
interpolate attributes are always deleted unless the keyword **`keepatt`** is used.

 
 
## Demos


 [Examples: interpolate / voronoi](../description_voronoi.md)  Examples from test/level01/interp_voronoi

 [Examples: interpolate / map](../description_map.md)  Examples from test/level01/interp_map

 [Examples: interpolate / continuous](../description_cont.md)  Examples from test/level01/interp_continuous
