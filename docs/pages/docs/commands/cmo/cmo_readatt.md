---
Title: "cmo/readatt"
Tags: cmo readatt
---

# CMO / READATT

-----------------

Read field style data from file. Expects same number of values each line.
<br>Any line starting with "#" will be ignored.
<br>Any line starting with a character string will be ignored.




## SYNTAX

<pre>
<b>cmo/readatt </b> /cmo_name/attr1/[attr2..] operation / filename
</pre>

`cmo_name` is type character, required. The data will be read into the node attributes of this mesh object.

`attr1` is the data to read the first field into, `attr2` would be the second field, etc. Not all fields need to be read,
but they will be read in the order they occur. Attributes are assumed to be node length, see example below how to copy values into element length attributes. 

`operation` is either **add** which will cause new nodes to be added
  to the mesh object or `ifirst, islast, istride` which specifies the
  nodes whose values will be replaced.

`filename` is type character required and specifies the ASCII file to be read.
  


## EXAMPLES

```
cmo / create / mo_name / / / tet

cmo/readatt/mo_name/ xic,yic,zic,node_val1,node_val2  / 1,0,0 / input_tab.dat 
```

Read x,y,z coordinates and some values into node attributes node_val1 and node_val2 from a file of tabular data.
The first three lines are ignored. None of the tagged information is retained. Ignored lines can be indicated by either a # in column one or anything other than a real or integer as the first token in a line.

Input file input_tab.dat can be a TecPlot format file something like:
<pre class="lg-output">
TITLE="Heterogeneity of TMCM #39, 1=newzone 81, 2=newzone 82"
variables="x","y","z", "zone", "element"
zone t="facies" I=191, J=136, K= 57
0.53600E+06 0.41020E+07 0.00000E+00 2  1
0.53610E+06 0.41020E+07 0.00000E+00 2  2
0.53620E+06 0.41020E+07 0.00000E+00 2  3
  ...
</pre>
 
 
``` 
cmo/readatt /mo_tet2/xic,yic,zic/add /// myfile
```
New nodes will be added to mo_tet2, and their coordinates will be supplied from the file. The value of nnodes will be updated.
For this example 2 nodes and their values will be added to mo_tet2.

Contents of myfile:
<pre class="lg-output">
0.017     12.65     7.25 
1.1       10.2      3.4
</pre>



```
cmo/readatt /mo_tet2/itp1/new_node_attr/pset,set,p1/myfile
```
The values of itp1 will be replaced. If new_node\attr does not exist it will be created as a VDOUBLE node based attribute. 

```
# read values into temporary mesh node attributes
cmo / create / mo_temp
cmo / readatt / mo_temp / permx permy permz por / 1 0 0 / file.table

read/avs/dfm_tet_mes.inp/mo

# create element attributes
# nelements should be equal to the node length of mo_temp
cmo / addatt / mo / permx / vdouble / scalar / nelements
cmo / addatt / mo / permy / vdouble / scalar / nelements
cmo / addatt / mo / permz / vdouble / scalar / nelements
cmo / addatt / mo / por   / vdouble / scalar / nelements
cmo / copyatt / mo / mo_temp / permx / permx
cmo / copyatt / mo / mo_temp / permy / permy
cmo / copyatt / mo / mo_temp / permz / permz
cmo / copyatt / mo / mo_temp / por   / por

cmo / delete / mo_temp
```
Read data into temporary mesh object with attribute nnodes long. The data is copied into element attributes of length equal to the mo_temp node attributes.

```
define MINX 498000.                                                             
define MAXX 500500.                                                             
define MINY 537000.                                                             
define MAXY 540500.                                                             
define NX 101                                                                    
define NY 141 

cmo/create/cmoquad///quad                                                       
quadxy NX NY/MINX MINY 0./MAXX MINY 0./MAXX MAXY 0./MINX MAXY 0.                
  rzbrick/xyz/NX,NY,1/1,0,0/connect                                               

cmo/readatt/cmoquad/ xic,yic,zic /1,0,0/input_ev.dat    

```
Read into created quad mesh of known spacing 14261 coordinate values. This is useful for grid files where the connectivity is implied but not included in the coordinate file. 


