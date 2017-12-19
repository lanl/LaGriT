**read/avs**

read an AVS format file into a mesh object. This command requires either cmo-name to be given in
the command or for a mesh object to have been created previously. This file format contains no geometry
information. 

**FORMAT:**

**read**/ filename.[inp or .avs] / cmo_name

**read** **/avs**/filename/[cmo-name]/[node\_flags/element\_flag/attribute\_flag]

*Note that the filename is case-sensitive, though the extension itself is not.*




**OPTIONS:**
 
 
| Argument | Default | Description |
| ----------------- |  ------------- | ----------------------- |
| node_flag  |  (default=1) |  0 skip node data /  1 read node data |
| element_flag |    (default=1) |  0 skip element data / 1 read element data |
| attribute_flag |  (default=1) |  0 skip attribute data / 1 read attribute data |


**EXAMPLES:**

```
read/ file1.inp /cmo1
read/ avs/ file1 /cmo1
```

read into existing and current mesh object
and skip, do not read the attribute data
```
read/ avs / file2.avs / /1/1/0
```

**AVS UCD ASCII FILE FORMAT**

AVS (avs.com) is a data visualization tool with ASCII mesh file formats used by LaGriT as they are easy to read and easy to convert from and to other mesh file formats. The Unstructured Cell Data (UCD) format is described here.


<pre>
A UCD data structure consists of an irregular coordinate structure (or “model) made up of cells of various types.
Each cell has a corresponding number of nodes and connectivity. 
Mesh Data can be associated with the entire structure, with each cell, or with each node. 
The data is structured as a set of components. Each component can be either a scalar or a vector.
The input file cannot contain blank lines or lines with leading blanks. Comments, if present, must precede all data in the file. 
The general order of the data is as follows
 
1. Numbers defining the overall structure, including the number of nodes, the number of cells, and the length of the vector of data associated with the nodes, cells, and the model.
 
2. For each node, its node id and the coordinates of that node in space. Node ids must be integers, but any number including non sequential numbers can be used. Mid edge nodes are treated like any other node.
 
3. For each cell: its cell id, material, cell type (hexahedral, pyramid, etc.), and the list of node ids that correspond to each of the cell's vertices. The below table specifies the different cell types and the keyword used to represent them in the file.
 
Line line
Triangle tri
Quadrilateral quad
Hexahedron hex
Prism prism
Tetrahedron tet
Pyramid pyr
Point pt

4. Node based data descriptions, if present, the data vector associated with nodes, how many components that vector is divided into (e.g., a vector of 5 floating point numbers may be treated as 3 components: a scalar, a vector of 3, and another scalar, which would be specified as 3 1 3 1).
 
5. For each node data component in a separate line, a component label/unit label pair, separated by a comma.

6. For each node in a separate line, the vector of data values associated with it. This is the end of node definitions.

7.Cell based data descriptions, if present, then follow in the same order and format as items 4, 5, and 6.


This is the format of the AVS ASCII UCD file:

# <comment n> 
1. <num_nodes> <num_cells> <num_ndata> <num_cdata> <num_mdata> 
2. <node_id 1> <x> <y> <z> 
<node_id 2> <x> <y> <z>
. 
. 
. 
<node_id num_nodes> <x> <y> <z> 
3. <cell_id 1> <mat_id> <cell_type> <cell_vert 1> ... <cell_vert n> 
<cell_id 2> <mat_id> <cell_type> <cell_vert 1> ... <cell_vert n> 
. 
. 
. 
<cell_id num_cells> <mat_id> <cell_type> <cell_vert 1> ...<cell_vert n> 
4. <num_comp for node data> <size comp 1> <size comp 2>...<size comp n> 
5. <node_comp_label 1> , <units_label 1> 
<node_comp_label 2> , <units_label 2> 
. 
. 
. 
<node_comp_label num_comp> , <units_label num_comp>

6. <node_id 1> <node_data 1> ... <node_data num_ndata> 
<node_id 2> <node_data 1> ... <node_data num_ndata> 
. 
. 
. 
<node_id num_nodes> <node_data 1> ... <node_data num_ndata> 
7. <num_comp for cell's data> <size comp 1> <size comp 2>...<size comp n> 
<cellcomponentlabel 1> , <unitslabel 1> 
<cellcomponentlabel 2> , <unitslabel 2> 
. 
. 
. 
<cellcomponentlabel n> , <unitslabel n> 
8. <cellid 1> <celldata 1> ... <celldata num_cdata> 
<cellid 2> <celldata 1> ... <celldata num_cdata> 
. 
. 
. 
<cellid num_cells> <celldata 1> <celldata num_cdata>

</pre>

This is an Example ASCII UCD File for a single hexahedral cell with 8 nodes. Associated with each node is a data value, there are no cell data. The first line indicates 8 nodes, 1 cell, 1 scaler attribute for the nodes, no attributes for cell or model.

<pre>
8 1 1 0 0 
1 0.000 0.000 1.000 
2 1.000 0.000 1.000 
3 1.000 1.000 1.000 
4 0.000 1.000 1.000 
5 0.000 0.000 0.000 
6 1.000 0.000 0.000 
7 1.000 1.000 0.000 
8 0.000 1.000 0.000 
1 1 hex 1 2 3 4 5 6 7 8 
1 1 
stress, lb/in**2 
1   4999.9999  
2  18749.9999 
3  37500.0000 
4  56250.0000 
5  74999.9999 
6  93750.0001 
7 107500.0003 
8   5000.0001 

</pre>
