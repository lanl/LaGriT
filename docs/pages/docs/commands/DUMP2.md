---
title: "DUMP"
tags: write output files
---

# DUMP 

--------------------------------------------

This command produces an output file from a Mesh Object. Some of the standard graphics packages are supported including AVS, GMV, and TECPLOT. See alphabetic list below for descriptions and syntax.

## SYNTAX 

<hr>

<pre>
<b>dump</b> / file_type / file_name /[cmo_name]/

<b>dump</b> / file<b>.inp .gmv .lagrit .lg .exo .ts</b> / [cmo_name]/
</pre>

The **`dump`** command is followed by a keyword `file_type` to indicate the type of file to write. 'cmo_name' is the mesh object to write. 
<br>
The second line shows the short form where `file_type` is recognized by the file name extension as listed below.


### EXTENSIONS for SHORT SYNTAX

|   |   |  | | |
| :------ | :---------- | :---------- | :---------- | :---------- | 
|&nbsp;&nbsp; **AVS** [**.inp** or **.avs**](#avs) |&nbsp;&nbsp; **GMV** [**.gmv**](#gmv)  |&nbsp;&nbsp; **Exodusii** [**.exo**](#exodus) |&nbsp;&nbsp; **GoCAD** [**.ts**](#gocad) |&nbsp;&nbsp; **LaGriT** [**.lagrit** or **.lg**](#lagrit) | 


### ADDITIONAL FILE TYPES

|   |   |    |   |
| :------ | :---------- | :------ | :---------- | 
|&nbsp;&nbsp; [**chad**](#chad) &nbsp;&nbsp; |&nbsp;&nbsp; [**coord**](#coord) &nbsp;&nbsp; |&nbsp;&nbsp; [**datex**](#datex) &nbsp;&nbsp; |&nbsp;&nbsp; [**elem_adj_node**](#elem_adj) &nbsp;&nbsp; |
|&nbsp;&nbsp; [**elem_adj_elem**](#elem_adj) &nbsp;&nbsp; |&nbsp;&nbsp; [**fehm**](#fehm) &nbsp;&nbsp; |&nbsp;&nbsp; [**geofest**](#geofest) &nbsp;&nbsp; |&nbsp;&nbsp; [**geom**](#geom) &nbsp;&nbsp; |
|&nbsp;&nbsp; [**pflotran**](#pflotran) &nbsp;&nbsp; |&nbsp;&nbsp; [**recolor**](#recolor) &nbsp;&nbsp; |&nbsp;&nbsp; [**stl**](#stl) &nbsp;&nbsp; |&nbsp;&nbsp; [**stor**](#stor) &nbsp;&nbsp; |
|&nbsp;&nbsp; [**tecplot**](#tecplot) &nbsp;&nbsp; |&nbsp;&nbsp; [**zone**](#zone) &nbsp;&nbsp; |&nbsp;&nbsp; [**zone_imt**](#zone) &nbsp;&nbsp; |&nbsp;&nbsp; [**zone_outside**](#zone) &nbsp;&nbsp;   |


<hr>

### **`AVS`** <a name="avs"></a>

<pre>
<b>dump / avs</b> / file_name/ [cmo_name] / [iopt_points, iopt_elements, iopt_node_attributes, iopt_elem_attributes] 
</pre>

Will write the AVS UCD (Unstructured Cell Data) file format. 

**`avs`**  = **`avs2`** writes data values as real or integer depending on type with spacing dependent on size of values. 

**`avs1`** = old avs writes all data values as real with large spacing (larger file size). 

`iopt values` indicate which data to write or skip, default is everything on with `iopt` values = 1 1 1 1.


For example, 

<pre>
<b>dump</b> / avs / file.inp / cmo_name

<b>dump</b> / avs / file.inp / cmo_name / 1, 1, 0, 0

<b>dump</b>/ file_name<b>.inp</b> / cmo_name
</pre>

the first line will write node coordinates, element connectivity, and node and element attributes if they exist. The second line will write node coordinates and element connectivity, but not node attributes or element attributes. 

If *`iopt_`* values = 2, or if *`iopt_points`* = 0, then the output will not be a valid AVS UCD format file. 
These options are provided to enable a user the flexiblity of writing ASCII files with desired information, and are not intended to be used with **`read/avs`** or other Applications that read AVS UCD files. A WARNING message is given for non-standard AVS output. The following describe valid *`iopt_`* values.

*`iopt_points`* is the first section listing node id and their x y z coordinates.
* = 0 Do not output node coordinate section.
* = 1 Output node coordinate information (node_id  x  y  z)
* = 2 Output node coordinates information without node number in first column (  x  y  z)

*`iopt_elements`* is the second section listing element id, material, type, node vertices.
* = 0 Do not output element connectivity information
* = 1 Output element connectivity information 
* = 3 Output points as AVS pt type for VIS applications. This is only for nodes with 0 elements.

*`iopt_node_attributes`* are the node attributes listing name, type, node_id and values for each.  
Note by default the AVS files include the mesh object node attributes imt1, itp1, icr1, and isn1        
* = 0 Do not output node attribute information
* = 1 Output node attribute information 
* = 2 Output node attribute information without node id in first column

*`iopt_elem_attributes`* are the node attributes listing name, type, element_id and values for each.
* iopt_values_elem = 0 Do not output element attribute information
* iopt_values_elem = 1 Output element attribute information 
* iopt_values_elem = 2 Output element attribute information without element id in first column

Note LaGriT Versions V3.30 and older have the following keyword definitions:
<pre>
avs      =  All numbers written as reals.
att_node =  Node Attributes are written as real and integer, header info lines start with #
att_elem =  Element Attributes are written as real and integer, header info lines start with #
</pre>
   

For a description of the AVS file format see the [`read/avs` command](../read_avs.md).

<br>

### **`CHAD`** <a name="chad"></a>

<pre>
<b>dump / chad</b> / file_name /[cmo_name]/ 
</pre>

Will output a file nodes, faces, and connectivity for tet, hex, pyr, or pri in CHAD format. Writes attributes imt and itp.

<br>

### **`COORD`** <a name="coord"></a>

<pre>
<b>dump / coord</b> / file_name /[cmo_name]/
</pre>

See also **`dump/fehm`**

Will output a single file with node list x,y,z values and element connectivity list in FEHM format. Files are written in FEHM format and are described by [clicking here for details](dump/DUMP3.md).

The coord file is one of a set of files written when the fehm file type is called. 

<br>

### **`DATEX`** <a name="datex"></a>

<pre>
<b>dump / datex</b>  OR  <b>simul</b> / file_name / [cmo_name]
</pre>

will output a file with Geometry, Element, Region, Location, and Dataset in DATEX format.

<br>

### **`ELEM_ADJ_ELEM`** or **`ELEM_ADJ_NODE`** <a name="elem_adj"></a>

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

Write adjacency information to an ascii file. Write list of all elements adjacent to each node. (-99 is a boundary)

File format: `node_number number_of_adjacent_elem e1 e2 ... en` 

<br>

### **`EXO`** or **`EXODUSII`** <a name="exodus"></a>

<pre>
<b>dump / exo</b>  OR  <b>exodusii</b> / file_name / mo_name [ psets ] / [ eltsets] / [ facesets file1 file2 ... filen ] 
</pre>

Write a mesh object to a file in the Exodus II format. The keyword psets as token 5 will cause all psets (lists of vertex numbers) associated with the mesh object to be written to the ExodusII output file. 

The keyword eltsets as token 6 will cause all eltsets (lists of cell numbers) associated with the mesh object to be written to the ExodusII output file. 

If face set information is being provided from files (`file1 file2 ... filen`) the format of the file is written in AVS UCD cell attribute format. The first column is the global cell number, the second column is the local face number.


Click here for [more details on options and files that are written for ExodusII](EXODUS.md).

<br>

### **`FEHM`** <a name="fehm"></a>

<pre>
<b>dump/ fehm</b> / rootname / cmo_name / [ optional keywords ]
</pre>

Write out a series of files for the FEHM flow and transport code. The tokens after the cmo name are all optional. 

The following keyword commands are optional and can occur in any order after the cmo name.

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

### **`GEOFEST`** <a name="geofest"></a>

<pre>
<b>dump/ geofest /</b> file_name 
</pre>

Write a file to be read by the GeoFEST, Geophysical Finite Element Simulation Tool hosted by Open Channel Foundation. The output file is ascii.

<br>

### **`GEOM`** <a name="geom"></a>

<pre>
<b>dump / geom /</b> file_name 
</pre>

will write an ascii file containing the geometry information for the current run. This information includes the region and mregion definitions and surface, names, types and definitions. 

<br>

### **`GMV`** <a name="gmv"></a>

<pre>
<b>dump / gmv /</b> file_name / [mo_name] / [<u><b>binary</b></u> or <u><b>ascii</b></u> ]

<b>dump</b>/ file_name<b>.gmv</b> / [mo_name] /
</pre>

Write a file to be read by the graphics program GMV.  The defaults are binary and current mesh object.
Use **cmo/setatt//ipolydat/no** to reduce file size. This command will keep the polygon data from being written to GMV files.

For more on GMV visit: http://www.generalmeshviewer.com

<br>

### **`GOCAD`** <a name="gocad"></a>

<pre>
<b>dump / gocad /</b> file_name 
</pre>

Write a gocad TSURF file of triangle elements.

<br>

### **`LaGriT`** <a name="lagrit"></a>

<pre>
<b>dump / lagrit /</b> file_name / [cmo_name]/ [binary OR ascii] 
</pre>

Write a LaGriT restart file that contains geometry and mesh object information. The geometry belongs to the `cmo_name` with which it was created.  The  `cmo_name` can be **-all-** in which case all mesh objects are written to the file or it can specify a list of mesh objects to be written. A subsequent read/lagrit command will restart the code at the state at which the **`dump`** command was issued. The default file type is binary. 

<br>

### **`PFLOTRAN`** <a name="pflotran"></a>

<pre>
<b>dump / pflotran</b> / file_name_root / cmo_name / 
<b>dump / pflotran</b> / file_name_root / cmo_name / nofilter_zero
</pre>

Write coefficient matrix (stor) style values in PFLOTRAN **.uge** format file. The default **`dump/pflotran`** command does not write zero coupling coefficients. Use the keyword nofilter_zero to include zero coupling coefficients in the file.

The following is the format used by PFLOTRAN for **.uge** (explicit unstructured grid) file.

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

### **`RECOLOR`** <a name="recolor"></a>

<pre>
<b>dump / recolor /</b> file_name 
</pre>

This command writes the existing colormap to the specified file.  [See colormap command](COLORMAP.md)

<br>

### **`STL`** <a name="stl"></a>

<pre>
<b>dump / stl /</b> file_name 
</pre>

Output in STL, stereo lithography format. This is only supported for triangle mesh objects.
See more about the STL format at [dump examples](../demos/description_dump.md)

<br>

### **`STOR`** <a name="stor"></a>

<pre>
<b>dump / stor /</b> file_name_root / cmo_name /
[ascii  OR  binary ] / 
[scalar  OR  vector  OR  both  OR  area_scalar  OR  area_vector  OR  area_both] \
[all  OR  graph  OR  coefs  OR  none] / [hybrid  OR  nohybrid ] 
</pre>

Same syntax as **`dump/fehm`** except the only output is the FEHM sparse matrix coefficient STOR file `rootname.stor`. 
File can be written in ascii or binary (fortran unformatted platform dependent). The area coefficient values can be written as scalar or vector.
The compression default is **all** which will compress both the list of area coefficients and the indices. The coefs compression, or none compression both use and older algorithm and will result in larger files and may take longer to run.
The stor file is one of a set of files written when the fehm file type is called. 

[Click here for further explanation of syntax options.](dump/DUMP3.md)

[Click here for the FEHM style STOR file format.](../STOR_Form.md) 

<br>

### **`TECPLOT`** <a name="tecplot"></a>

<pre>
<b>dump / tecplot /</b> file_name 
</pre>

Write a file to be read by the Tecplot graphics package.  The output file is ascii. Only node attributes are output, element attributes are ignored and not output. Tecplot does not support prism or pyramid element types so they are written as eight node, degenerate hex elements. 
The ioflag parameter is used to control if the node attributes are output or not is the AVS ioflag. The expected suffix for the file name is **.plt**. If a name is given without the **.plt** suffix, a suffix ".plt" is added. 

Output is ascii. This output format does not support output of a mesh with nodes but zero elements. If there are zero elements, a header is written but node coordinate information is not output.

<br>

### **`ZONE`** <a name="zone"></a>

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

If keepatt is specified, then 6 node based attributes are added to the mesh object with the names  **bottom**, **top**,  **right_e**, **back_n**, **front_s**, and **left_w**. A node can occur in multiple zones. For instance, a node located on a top corner of the mesh can be found in zones for top, front_s, and left_w.

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

The option `zone_outside_minmax` is used to find the min and max external node along each row and column of a regular structured grid where the index for i, j, and k can be detirmined. The node attributes `i_index, j_index, and k_index` are created.

<a href="https://lanl.github.io/LaGriT/assets/images/zone_outside.png" target="_blank"> Click here for an image </a> showing difference between the default and the minmax options for outside nodes. 

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
