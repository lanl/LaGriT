**read/avs**

read an AVS format file into a mesh object. This command requires either cmo-name to be given in
the command or for a mesh object to have been created previously. This file format contains no geometry
information. 

**FORMAT:**

**read**/ filename.[inp or .avs] / cmo_name

**read** **/avs**/filename/[cmo-name]/[node\_flags/element\_flag/attribute\_flag]

[Note that the filename is case-sensitive, though the extension itself is not.]




**OPTIONS:**
 
Argument | Default | Description
----------------- |  ------------- | -----------------------
node_flag  |  (default=1) |  0 skip node data /  1 read node data
element_flag |    (default=1) |  0 skip element data / 1 read element data
attribute_flag |  (default=1) |  0 skip attribute data / 1 read attribute data

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
