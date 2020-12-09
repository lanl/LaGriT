# read/gocad

Read an ascii GoCad TSURF triangle file (.ts) or a TSolid tet file (.so). Note old versions of LaGriT only read the 2D TSURF files.
The GOCAD header contains information on the mesh data in the file. This includes VRTX or PVTRX x, y, z [vertex properties] and TRI or TET connectivity list, [cell properties]. Some applications such as JewelSuite use a left-hand coordinate system and the header will include keywords **ZPOSITIVE Depth**. LaGriT will convert this to a right-hand coordinate system with elevations Z positive.

See http://paulbourke.net/dataformats/gocad/ for further details on GoCad.
  
## SYNTAX

**read** / **gocad** / filename[.ts or .so] / cmoname


## EXAMPLES

```
read/gocad / JS_mesh.so /cmotet
cmo/printatt/cmotet/ -all- minmax
quality
```

read 3D TETRA into mesh object cmotet, check the min and max values of added attributes (created from GOCAD PROPERTY arrays). Check connectivity was properly read by making sure volumes are positive.


## EXAMPLE GOCAD 3D TETRA FILE FORMAT

```
GOCAD TSolid 1
HEADER {
name:3D Mesh Structural Model SMALL
*solid*color:0.498039 0.498039 0.498039 0.5
}
GOCAD_ORIGINAL_COORDINATE_SYSTEM
NAME Default
AXIS_NAME "X" "Y" "Z"
AXIS_UNIT "m" "m" "m"
ZPOSITIVE Depth
END_ORIGINAL_COORDINATE_SYSTEM
TETRA_PROPERTIES ZoneId
TETRA_PROP_LEGAL_RANGES **none** **none**
TETRA_NO_DATA_VALUES -999.25
TETRA_PROPERTY_CLASSES ZoneId
TETRA_PROPERTY_KINDS unknown
TETRA_PROPERTY_SUBCLASSES QUANTITY Float
TETRA_ESIZES 1
TETRA_UNITS unitless
TETRA_PROPERTY_CLASS_HEADER ZoneId{
low_clip:1
high_clip:2
}
TVOLUME
VRTX 1 241542.64788705 3569942.9650014676 -684.72122323805638
VRTX 2 241569.11887199868 3569823.3059474104 -701.15237792621951
VRTX 3 241601.07618168893 3569932.2043372113 -750.27786898670399
VRTX 4 241527.6427429766 3569878.153216748 -786.94401445577932
VRTX 5 241630.376379692 3569879.0671290001 -814.42141248757616
TETRA 1 2 3 4 1
TETRA 3 4 2 5 1
END
```

The screen output for reading this file will look like:

<pre>
read gocad ex_2tet.so mo1                                                       
Reading GOCAD file: ex_2tet.so                                                  
cmo/create/mo1///tet                                                            
finish                                                                          
There are no Node Properties.                                                   
Attributes set for Node Properties    0                                         
...................................................                             
READ VRTX data and properties:        5    0                                       
cmo/addatt/mo1/ZoneId/VINT scalar/nelements/linear/permanent/gxaf/0.0/          
finish                                                                          
Attributes set for Cell Properties    1                                         
...................................................                             
READ CELL data and properties:        6    1                                       
geniee                                                                          
finish                                                                          
--- READ GOCAD --------                                                         
 Mesh Type:    TSolid                                                           
 ZPOSITIVE:    Z Depth                                                          
 Nodes:                5                                                        
 Tets:                 2                                                        
 Cell Properties:      1                                                          
 LINES read:           32                                                    
-----------------------                   
</pre>
  

  
