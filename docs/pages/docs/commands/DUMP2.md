---
title: "DUMP"
categories: files
---

# DUMP #

This command produces an output file from a Mesh Object. Some of the standard graphics packages are supported including AVS, GMV and TECPLOT. See below for full list of file types that can be written. The list is in alphabetic order and describes each valid file_type with syntax and usage. 

## GENERAL SYNTAX: ##

<pre>
<b>dump</b> / file_type / file_name /[cmo_name]/
</pre>

The dump command is followed by a keyword for file type. Valid file type keywords are listed below and include: `gmv`, `avs`, `avs2`, `chad`, `coord`, `datex`, `elem_adj_node`, `elem_adj_elem`, `fehm`, `geofest`, `geom`, `gmv`, `gocad`, `lagrit`, `recolor`, `stl`, `stor`, `tecplot`, `zone`, `zone_imt`, and `zone_outside`.   

The `file_type` keyword is followed by a string to be used as whole or part of file name as described below.


## SHORT SYNTAX: ##

<pre>
<b>dump</b> / file_name.extension / [cmo_name]
</pre>

where extension implies the file type designation. Valid exetensions are recognized:   
AVS (`.inp` or `.avs`), Exodus (`.exo`), GMV (`.gmv`), LaGriT (`.lagrit` or `.lg`), and GoCAD (`.ts`). 

## FILE TYPES: ##

### Jump to file format: ###

| [`avs`](#avs)       | [`avs2`](#avs2)  | [`chad`](#chad)  | [`coord`](#coord)  |  [`datex`](#datex) |
|---------------------|------------------|------------------|--------------------|--------------------|
| [`elem_adj_*`](#elem_adj) | [`exo`](#exo)  | [`fehm`](#fehm)  | [`geofest`](#geofest)  | [`geom`](#geom)  |
| [`gmv`](#gmv)  | [`gocad`](#gocad)  | [`lagrit`](#lagrit)  | [`pflotran`](#pflotran)  | [`recolor`](#recolor)  |
| [`stl`](#stl)  | [`stor`](#stor)  | [`tecplot`](#tecplot)  | [`zone`](#zone)  |   |

<br>

### `AVS` <a name="avs"></a>

<pre>
<b>dump / avs</b> / file_name/ [cmo_name] / [iopt_points, iopt_elements, iopt_node_attributes, iopt_element_attributes] 
</pre>

Output in AVS-UCD (Unstructured Cell Data) format. One can turn on or off the output of node coordinates (`iopt_points`), element connectivity (`iopt_elements`), node attributes (`iopt_node_attributes`) and element attributes (`iopt_element_attributes`). 1 (default) is on, 2 is on but the first column will not include the node number or element number, 0 turns off output of that part of the file.

For example, 

<pre>
dump / avs / file.inp / cmo_name / 1, 1, 0, 0
</pre>

will write node coordinates and element connectivity, but not node attributes or element attributes.    

*Note the **2** option writes an abreviated form of the file format that is non-standard and probably not recognized outside of LaGriT.*

For a description of the AVS file format see the [`read/avs` command](../read_avs.md).

<br>

### `AVS-2` <a name="avs2"></a>

<pre>
<b>dump / avs2</b> / file_name/[cmo_name]/[iopt_points,iopt_elements,iopt_node_attributes,iopt_element_attributes]
</pre>

This option will output integers as integers instead of floating point. The other avs option converts integers to reals on output. The `/avs/` option above outputs all attributes as real numbers. This option is slower but the files are smaller if there are integers in the node or element attributes. 

<br>

### `CHAD` <a name="chad"></a>

<pre>
<b>dump / chad</b> / file_name /[cmo_name]/ 
</pre>

Will output a file nodes, faces, and connectivity for tet, hex, pyr, or pri in CHAD format. Writes attributes imt and itp.

<br>

### `COORD` <a name="coord"></a>

<pre>
<b>dump / coord</b> / file_name /[cmo_name]/
</pre>

> See also `dump/fehm`

Will output a single file with node list x,y,z values and element connectivity list in FEHM format. Files are written in FEHM format and are described by [clicking here for details](dump/DUMP3.md).

The coord file is one of a set of files written when the fehm file type is called. 

<br>

### `DATEX` <a name="datex"></a>

<pre>
<b>dump / datex</b>  OR  <b>simul</b> / file_name / [cmo_name]
</pre>

will output a file with Geometry, Element, Region, Location, and Dataset in DATEX format.

<br>

### `ELEM_ADJ_ELEM` && `ELEM_ADJ_NODE` <a name="elem_adj"></a>

<pre>
<b>dump / elem_adj_elem</b> / file_name* / mo_name / [ <b>delatt</b>  OR  <b>keepatt</b>  OR  <b>attonly</b> ]
</pre>


* Option: `delatt` - Write adjacency information to an ascii file. Write list of all elements adjacent to each element. 
  * File format: `elem_number ean_num e1 e2 ... en`
* Option: `keepatt` - write file and add node attribute `ean_num` (number of elements adjacent to each node) 
* Option: `attonly` - do not write file, add node attribute `ean_num`, a dummy argument is still required in the file_name field 


<pre>
<b>dump / elem_adj_node</b> / file_name / mo_name 
</pre>

Write adjacency information to an ascii file. Write list of all elements adjacent to each node. 

File format: `node_number number_of_adjacent_elem e1 e2 ... en`

<br>

### `Exodus` <a name="exodus"></a>

<pre>
<b>dump / exo</b>  OR  <b>exodusii</b> / file_name / mo_name [ psets ] / [ eltsets] / [ facesets file1 file2 ... filen ] 
</pre>

Write a mesh object to a file in the Exodus II format. The keyword psets as token 5 will cause all psets (lists of vertex numbers) associated with the mesh object to be written to the ExodusII output file. 

The keyword eltsets as token 6 will cause all eltsets (lists of cell numbers) associated with the mesh object to be written to the ExodusII output file. 

If face set information is being provided from files (`file1 file2 ... filen`) the format of the file is written in AVS UCD cell attribute format. The first column is the global cell number, the second column is the local face number.


Click here for [more details on options and files that are written for ExodusII](EXODUS.md).

<br>

### `FEHM` <a name="fehm"></a>

<pre>
<b>dump/ fehm</b> / rootname / cmo\_name / [ optional keywords ]
</pre>

Write out a series of files for the FEHM flow and transport code. The tokens after the cmo name are all optional. 

The following keyword commands are optional and can occur in any order after the cmo\_name.

* `ascii` or `binary`  indicate IO Mode Options for the stor file. Default is ascii.
* `scalar`,  `vector`,  `both`,  `area_scalar`,  `area_vector`, or `area_both` are Area Coefficient Options for writing stor file coefficient values. Default is scalar.
* `all`,  `graph`,  `coefs`, or  `none` are Compression Options for the stor file. Default is all.
* `delatt` or  `keepatt`  deletes or keeps CMO Attributes created to find outside zone nodes. Default is delatt.
* `hybrid` or `nohybrid` Specify whether hybrid median-Voronoi control volumes should be used. Default is nohybrid.

The default options will delete the outside node attributes and will not add attributes for the outside voronoi or median areas. 
The stor file will be written in ASCII format with scalar coefficient values with compression of area coefficient list and indices.

The `rootname` will be used to form full names for the files that are written:

```
rootname.fehmn              rootname_interface.zone      rootname_outside_vor.area     
rootname_material.zone      rootname_multi_mat.zone      rootname_outside.zone       rootname.stor
```

* `.fehm` - mesh coordinates and geometry ( see `dump/coord/...` command)
* `_material.zone` - node imt (material) zone lists ( see `dump/zone_imt/...` command)
* `_outside.zone` - node external boundary zone lists (see `dump/zone_outside/...` command)
* `_outside_vor.area` - node external boundary area lists (see `dump/zone_outside/...` command)
* `_interface.zone` - zone lists for nodes along material interfaces 
* `_multi_mat.zone` - lists of node pairs connected across material interfaces 
* `.stor` - FEHM format file giving the voronoi (control volume) associated with each node and the sparce matrix structure

Click here for [more details on the FEHM files and options.](dump/DUMP3.md)
Click here for the [FEHM style STOR file format.](../STOR_Form.md) 

<br>

### `GEOFEST` <a name="geofest"></a>

<pre>
<b>dump/ geofest /</b> file_name 
</pre>

Write a file to be read by the GeoFEST, Geophysical Finite Element Simulation Tool hosted by Open Channel Foundation. The output file is ascii.

<br>

### `GEOM` <a name="geom"></a>

<pre>
<b>dump / geom /</b> file_name 
</pre>

will write an ascii file containing the geometry information for the current run. This information includes the region and mregion definitions and surface, names, types and definitions. 

<br>

### `GMV` <a name="gmv"></a>

<pre>
<b>dump / gmv /</b> file_name / [mesh-object] / [binary OR ascii] 
</pre>

Write a file to be read by the graphics program GMV.  The defaults are binary and current mesh object.

*NOTE:  For LaGriT versions dated after October 1999, use `cmo/setatt//ipolydat/no` to reduce file size. This command will keep the polygon data from being written to GMV files.*

For more on GMV visit: http://www.generalmeshviewer.com

<br>

### `GOCAD` <a name="gocad"></a>

<pre>
<b>dump / gocad /</b> file_name 
</pre>

Write a gocad TSURF file.

<br>

### `LaGriT` <a name="lagrit"></a>

<pre>
<b>dump / lagrit /</b> file_name / [cmo_name]/ [binary OR ascii] 
</pre>

Write a LaGriT restart file that contains geometry and mesh object information.  `cmo_name` can be `-all-` in which case all mesh objects are written to the file or it can specify a list of mesh objects to be written. A subsequent read/lagrit command will restart the code at the state at which the `dump` command was issued. The default file type is binary. 

<br>

### `PFLOTRAN` <a name="pflotran"></a>

<pre>
<b>dump / pflotran</b> / file_name_root / cmo_name / 
<b>dump / pflotran</b> / file_name_root / cmo_name / nofilter_zero
</pre>

Write coefficient matrix (stor) style values in PFLOTRAN `.uge` format file. The default `dump/pflotran` command does not write zero coupling coefficients. Use the keyword nofilter_zero to include zero coupling coefficients in the file.

The following is the format used by PFLOTRAN for `.uge` (explicit unstructured grid) file.

The first block are the list of ids of cells and the coordinates of cell centroids and the volumes of the cells. The PFLOTRAN cells are Voronoi volumes, one for each node.

```
CELLS <integer>    integer = # cells (N)
id_1  x_1  y_1  z_1 volume_1
d_2   x_2  y_2  z_2  volume_2
...
...
id_N x_N y_N z_N volume_N
```

The second block consists of a list of ids of the connecting cells (id_up, id_dn), 
coordinates of the face centroid between the two connected cells and 
areas of the faces.

```
CONNECTIONS <integer>   integer = # connections (M)
id_up_1 id_dn_1 x_1 y_1 z_1 area_1
id_up_2 id_dn_2 x_2 y_2 z_2 area_2
...
...
id_up_M id_dn_M x_M y_M z_M area_M
```

<br>

### `RECOLOR` <a name="recolor"></a>

<pre>
<b>dump / recolor /</b> file_name 
</pre>

This command writes the existing colormap to the specified file.  [See colormap command](COLORMAP.md)

<br>

### `STL` <a name="stl"></a>

<pre>
<b>dump / stl /</b> file_name 
</pre>

Output in STL, stereo lithography format. This is only supported for triangle mesh objects.

<br>

### `STOR` <a name="stor"></a>

<pre>
<b>dump / stor /</b> file_name_root / cmo_name /
[ascii  OR  binary ] / 
[scalar  OR  vector  OR  both  OR  area_scalar  OR  area_vector  OR  area_both] \
[all  OR  graph  OR  coefs  OR  none] / [hybrid  OR  nohybrid ] 
</pre>

Same syntax as `dump/fehm` except the only output is the FEHM sparse matrix coefficient STOR file `rootname.stor`. 
File can be written in ascii or binary (fortran unformatted platform dependent). The area coefficient values can be written as scalar or vector.
The compression default is `all` which will compress both the list of area coefficients and the indices. The coefs compression, or none compression both use and older algorithm and will result in larger files and may take longer to run.
The stor file is one of a set of files written when the fehm file type is called. 

[Click here for further explanation of syntax options.](dump/DUMP3.md)

[Click here for the FEHM style STOR file format.](../STOR_Form.md) 

<br>

### `TECPLOT` <a name="tecplot"></a>

<pre>
<b>dump / tecplot /</b> file_name 
</pre>

Write a file to be read by the Tecplot graphics package.  The output file is ascii. Only node attributes are output, element attributes are ignored and not output. Tecplot does not support prism or pyramid element types so they are written as eight node, degenerate hex elements. 
The ioflag parameter is used to control if the node attributes are output or not is the AVS ioflag. The expected suffix for the file name is `.plt`. If a name is given without the `.plt` suffix, a suffix `.plt` is added. 

Output is ascii. This output format does not support output of a mesh with nodes but zero elements. If there are zero elements, a header is written but node coordinate information is not output.

<br>

### `ZONE` <a name="zone"></a>

<pre>
<b>dump / zone /</b> file_name/ [cmo_name] / [delatt  OR  keepatt]   [keepatt_voronoi  OR  keepatt_median] 
</pre>

Write out a set of fehm format zone files for the mesh object nodes. These include zones for mesh materials and the external faces of the mesh as described below. The keepatt option will keep node attributes that tag nodes on external mesh boundaries (see zone_outside). 

The delatt option will delete the outside attributes if they exist (the are removed by default). The area attributes for outside nodes can be created with the keepatt_voronoi or keepatt_median options (see zone_outside). 

Files are written in FEHM format and are described in the dump/fehm command by [clicking here for details.](dump/DUMP3.md)

---------------

The `file_name` is used to create names for the following 5 files:

* `file_name_material.zone` - node imt (material) zone lists (see `dump/zone_imt/...` command)
* `file_name_outside.zone` - node external boundary zone lists (see `dump/zone_outside/...` command)
* `file_name_outside_vor.area` or file_name_outside_med.area - node external boundary area lists (see `dump/zone_outside/...` command)
* `file_name_interface.zone` - zone lists for nodes along material interfaces, 0 length file if mesh is single material 
* `file_name_multi_mat.zone` - lists of node pairs connected across material interfaces, 0 length file if mesh is single material 


<pre>
<b>dump / zone_imt /</b> file_name / [cmo_name] / [ imt_value ]   
</pre>

will output only one file with name file_name_material.zone. 
It is written in FEHM zone format and are described by 
[clicking here for details.](dump/DUMP3.md)

`file_name_material.zone` is node list for each integer material (imt) value. 
If the optional fifth argument is specified as an integer, then a node list file is written only listing the nodes with the value specified by imt_value. 
For options to output PSET's as ZONE/ZONN files see [PSET](PSET.md).

The `zone_imt` file is one of a set of files written when the fehm file type is called. 

<pre>
<b>dump / zone_outside</b>  OR  <b>zone_outside_minmax /</b> file_name /[cmo_name] / 
[delatt  OR  keepatt]  [keepatt_voronoi  OR  keepatt_median] 
</pre>

Write fehm zone format files that contain the outside node list and the associated outside area list. 

There are two files written: 

1. file_name_outside.zone is a node list for each of 6 possible external boundaries. 

If keepatt is specified, then 6 node based attributes are added to the mesh object with the names `top`, `bottom`, `left_w`, `right_e`, `back_n`, and `front_s`. A node can occur in multiple zones. For instance, a node located on a top corner of the mesh can be found in zones for `top`, `front_s`, and `left_w`.

* 1 = top = top = positive z direction (0,0,1) 
* 2 = bottom = bottom = negative z direction (0,0,-1) 
* 3 = left_w = left or west = negative x direction (-1,0,0) 
* 4 = front_s = front or south = negative y direction (0,-1,0) 
* 5 = right_e = right or east = positive x direction (1,0,0) 
* 6 = back_n = back or north = positive y direction (0,1,0) 


2. `file_name_outside_vor.area` is a list of Voronoi area vectors `(Ax_i,Ay_i,Az_i)` associated with each external node. 
It is written to match the node lists as written in the `outside.zone` file. Along with each outside zone tag (such as top), there is a sum of each vector for that zone. For applications such as infiltration, the z component (each 3rd value) would be used from the top zone list. 

```
00001  top   Sum VORONOI vectors:  0.5000000E+00 0.5000000E+00 0.5000000E+00
nnum
   3
  -2.500000000000E-01  -2.500000000000E-01   2.500000000000E-01   2.500000000000E-01   0.000000000000E+00   1.250000000000E-01
   0.000000000000E+00   2.500000000000E-01   1.250000000000E-01
```

If the keyword keepatt_voronoi is specified, three node attributes `(xn_varea, yn_varea, zn_varea)` representing the voronoi area are added.
If the keyword keepatt_median is specified, three node attributes `(xn_marea, yn_marea, zn_marea)` representing the median area are added and the file name will be file_name_outside_med.area. 
Note that the old version file name file_name_outside.area has area vectors computed with the median strategy.

The option `zone_outside_minmax` is used to find the min and max external node along each row and column of the regular grid.
<a href="../../images/zone_outside.png" target="_blank">Click here for an image </a> showing difference between the default and the minmax options for outside nodes. 

These zone_outside files are part of a set of files written when the zone or fehm file type is called. The fehm zone format and descriptions are  in the `dump/fehm` command details. 


## EXAMPLES: ##

    dump / gmv /file_name.gmv/cmo_name/
    dump / gmv /file_name.gmv/cmo_name/ascii

    dump / file_name.gmv / cmo_name

    dump / tecplot /file_name.plt/cmo_name

    dump / lagrit /file_name.lg/-all-/binary

    dump/file_name.inp/cmo_name
    dump / avs /file_name.inp/cmo_name
    dump / avs /file_name.inp/cmo_name/1 0 0 0 (output only node coordinates)
    dump / avs /file_name.inp/cmo_name/1 1 0 0 (output node coordinates and element connectivity)
    dump / avs /file_name.inp/cmo_name/0 0 0 1 (output element attributes)
    dump / avs /file_name.inp/cmo_name/0 0 2 2 (output node and element attributes without node numbers as first column of output)
    dump / avs2 /file_name.inp/cmo_name/1 1 1 0 (output node coordinates, element connectivity and node attributes)

    dump / fehm /file_root/cmo_name/ (write ascii compressed STOR file and full set of fehm input files)
    dump / stor /file_root/cmo_name/ (write ascii compressed STOR file)
    dump / stor /file_root/cmo_name/ binary (write unformatted compressed STOR file - platform dependent)
    dump / stor /file_name/cmo_name/ascii/area_scalar

    dump / zone_outside /file_root/cmo_name/keepatt (write outside node zones and voronoi areas, keep outside attributes)
    dump / zone_outside /file_root/cmo_name/keepatt_voronoi (write outside node zones and keep Voronoi area attributes)
    dump / zone_outside_minmax /file_root/cmo_name (write outside nodes at minmax extent of each column)

    dump / zone /file_root/cmo_name/ delatt keepatt_voronoi (write all FEHM zone and area files, delete the outside attributes and keep the voronoi area attributes)

    dump/ exo / file_name / cmo_name
    
Write generic exodus output without any sets.

    dump/ exo / file_name / cmo_name / psets

Write exodus output with point sets only.

    dump/ exo / file_name / cmo_name / / eltsets

Write exodus output with element sets only.

    dump/ exo / file_name / cmo_name / / / facesets

Write exodus output with face sets only. The facesets are internally calculated and defined. Note that the algorithm is computationally expensive and can take a long time to finish.

    dump/ exo / file_name / cmo_name / / / facesets file1,file2,...,filen

Write exodus output with face sets only. The face sets are imported from file1, file2, ..., filen.
    
    dump/ exo / file_name / cmo_name / psets / eltsets / facesets file1,file2,...,filen

Write exodus output with all psets, element sets, and face sets. The face sets are imported from file1, file2, ..., filen.


[Click here for demos](../main_dump.md)
