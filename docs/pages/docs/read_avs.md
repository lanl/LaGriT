# read / avs

read an AVS format file into a mesh object. This command requires either cmo-name to be given in
the command or for a mesh object to have been created previously. This file format contains no geometry
information. 

## SYNTAX

**read**/ filename.[inp or .avs] / cmo_name

**read** **/avs**/ filename /[cmoname]/[ node_flag / element_flag / attribute_flag]


## OPTIONS
 

| Argument           | Default | Description |
| :----------------- | :------ | :----------------------- |
| node_flag          |  1      |  0=skip node data   1=read node data |
| element_flag       |  1      |  0=skip element data  1=read element data |
| attribute_flag     |  1      |  0=skip attribute data 1=read attribute data |


## EXAMPLES

```
read/ file1.inp /cmo1
read/ avs / file1 /cmo1
```

read into existing and current mesh object
and skip, do not read the attribute data
```
read/ avs / file2.avs / /1/1/0
```

## AVS UCD ASCII FILE FORMAT

AVS (avs.com) is a data visualization tool with ASCII mesh file formats used by LaGriT as they are easy to read and easy to convert from and to other mesh file formats. The Unstructured Cell Data (UCD) format is described here.

A UCD data structure consists of an irregular coordinate structure made up of cells of various types.
Each cell has a corresponding number of nodes and connectivity. 
Data can be associated with each node, cell, or the entire "model".  
The data is structured as a set of components. Each component can be either a scalar or a vector.
The input file cannot contain blank lines or lines with leading blanks. 
Comments, if present, must precede all data in the file. 
The general order of the data is as follows.
 
1. Numbers defining the overall structure, including the number of nodes, the number of cells, and the length of the vector of data associated with the nodes, cells, and the model.
 
2. For each node, its node id and the coordinates of that node in space. Node ids must be integers, but any number including non sequential numbers can be used. Mid edge nodes are treated like any other node.
 
3. For each cell: its cell id, material, cell type, and the list of nodes for the cell connectivity. 

These are the valid AVS cell types with node order relative to LaGriT ordering conventions as shown at **[LaGriT Supported Element Types](supported.md)**

```
pt    (Point)          avs order  = lagrit order
line  (Line)           avs order  = lagrit order
tri   (Triangle)       avs order  = lagrit order
quad  (Quadrilateral)  avs order  = lagrit order
tet   (Tetrahedron)    avs order  = 1 2 4 3 
pyr   (Pyramid)        avs order  = 5 1 2 3 4 
prism (Prism)          avs order  = 4 5 6 1 2 3 
hex   (Hexahedron)     avs order  = 5 6 7 8 1 2 3 4 
```

4. Optional Node based data descriptions: data vector for nodes and number components that vector is divided into.
 
5. For each node data a label on seperate lines; label/unit label pair, separated by a comma.

6. For each node in a separate line, the vector of data values associated with it.

7. Cell based data descriptions, if present, then follow in the same order and format as items 4, 5, and 6.


This is the format of the AVS ASCII UCD file:
<pre>

# comment  
num_nodes  num_cells  num_node_data  num_cell_data  num_model_data 
node_id_1 x y z 
. . .
node_id_n x y z
cell_id_1 mat_id  cell_type  cell_vert 1 ... cell_vert n 
. . .
cell_id_n mat_id  cell_type  cell_vert 1 ... cell_vert n
num_node_data  node_data_1_size node_data_n_size
node_data_1_label, units_data_1 
. . .
node_data_n_label, units_data_n
node_id_1  node_data_1 ...  node_data_n
. . .
node_id_n  node_data_1 ...  node_data_n
num_cell_data  cell_data_1_size cell_data_n_size
cell_data_1_label, units_data_1 
. . .
cell_data_n_label, units_data_n
cell_id_1  cell_data_1 ...  cell_data_n
. . .
cell_id_n  cell_data_1 ...  cell_data_n
</pre>

This is an Example ASCII UCD File for a single hexahedral cell with 8 nodes.  The first line header indicates 8 nodes, 1 element, 2 node attributes, 0 element attributes, 0 model attributes. In the section where attributes are defined, the first line indicates there are 2 attributes, both scalar. After the first attribute line is the list of attribute names along with their type.

<pre>
8 1 2 0 0 
1 0.000 0.000 1.000 
2 1.000 0.000 1.000 
3 1.000 1.000 1.000 
4 0.000 1.000 1.000 
5 0.000 0.000 0.000 
6 1.000 0.000 0.000 
7 1.000 1.000 0.000 
8 0.000 1.000 0.000 
1 1 hex 1 2 3 4 5 6 7 8 
002 1 1
layer, integer
stress, real
1  1   4999.9999  
2  1   18749.9999 
3  1   37500.0000 
4  1   56250.0000 
5  0   74999.9999 
6  0   93750.0001 
7  0   107500.0003 
8  0   5000.0001 

</pre>

This is an Example ASCII UCD File for a point, this is handy for use with Paraview to render the point object instead of reading point values into a table. This example has 3 nodes, each as UCD object **pt**. The file has 0 attributes for nodes, elements, and model.

<pre>
         3          3          0          0          0
001   3.304250000000E+05  4.309168000000E+06  0.000000000000E+00
002   3.308220000000E+05  4.314749000000E+06  0.000000000000E+00
003   3.259190000000E+05  4.313480000000E+06  0.000000000000E+00
001        1    pt  1
002        1    pt  2
003        1    pt  3

 </pre>
