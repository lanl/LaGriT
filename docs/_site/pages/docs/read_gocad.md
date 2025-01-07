# read/gocad

Read an ascii GOCAD TSurf triangle file (.ts) or a TSolid tet file (.so). 

GoCAD has a number of possible data types contained within files and may contain multiple data sets. LaGrit can read a limited set of these types that are commonly used by our applications. The data types contained in the GOCAD file are described in the HEADER area and KEYWORDS. Many KEWORDS are not needed for the mesh and are ignored. The following are supported:

| KEYWORD           |  Description |
| :----------------- | :----------------------- |
| GOCAD          |    followed by mesh type TSolid or Tsurf |
| ZPOSITIVE       |    indicates Z as Depth or Elevation (default) |
| PROPERTIES     |    vertex property names for values listed after VRTX x y z, LaGriT will add as mesh node attributes |
| TETRA_PROPERTIES     |    tet property names for values listed after TETRA id1 id2 id3 id4, LaGriT will add as mesh element attributes |
| TRGL_PROPERTIES     |    tri property names for values listed after TRGL id1 id2 id3, LaGriT will add as mesh element attributes |
| VRTX or PVRTX         |  vertex keyword followed by x y z and optional property values, node number is implied by order |
| TETRA         |  tet element keyword followed by node numbers and optional property values, element number is implied by order |
| TRGL         |  tri element keyword followed by node numbers and optional property values, element number is implied by order |
| TFACE          |    triggers a new set of elements, attribute **iblock** will be incremented |
| END          |    end of mesh set |


Some applications such as JewelSuite use a left-hand coordinate system and the header will include keywords **ZPOSITIVE Depth**. LaGriT will convert this to a right-hand coordinate system with elevations Z positive.

See *LaGriT/test/level03/read_gocad* for example GOCAD files.

See http://paulbourke.net/dataformats/gocad/ for further details on GoCad.
  
## SYNTAX

**read** / **gocad** / filename[.ts or .so] / cmoname


## EXAMPLES

```
read/gocad / JS_mesh.so /cmotet
cmo/printatt/cmotet/ -all- minmax
quality
```

read 3D TETRA into mesh object cmotet, check the min and max values of added attributes (created from GOCAD PROPERTY arrays). Check that read worked correctly by using **quality** command to report positive volumes.

```
read/gocad /input_3tri_all_props.ts / cmotri
quality
```

read 2D triangulated surface (tsurf) file into mesh object cmotri. Check that read worked correctly by using **quality** command to report positive volumes.



## EXAMPLE GOCAD 3D TETRA FILE ex_2tet.so

This example has 2 tet elements, 0 node properties, and 1 cell property named "ZoneId". This was written by JewelSuite which defines Z as Depth.

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
cmo/create/mo1///tet                                                                                                                                     
...................................................                             
SET VRTX properties:                                                            
There are no VRTX properties.                                                   
READ VRTX data with word count:     5                                           
READ VRTX property with index:      0                                           
...................................................                             
SET CELL properties:                                                            
cmo/addatt/mo1/ZoneId/VINT scalar/nelements/linear/permanent/gxaf/0.0/                                                                                  
READ CELL data with word count:     6                                           
READ CELL property with index:      1                                           
geniee                                                                                                                                                
--- READ GOCAD FINISHED --------                                                
 Mesh Type:    TSolid                                                           
 ZPOSITIVE:    Z Depth                                                          
 Nodes:                5                                                        
 Tets:                 2                                                        
 Cells:                2                                                        
 Cell properties:      1                                                        
 LINES read:                 32                                                 
                                                                
The current-mesh-object(CMO) is: mo1                                            
 
  1 Mesh Object name: mo1                                                       
    number of nodes =             5        number of elements =            2    
    dimensions geometry =         3        element type =                tet    
    dimensions topology =         3        4 nodes      4 faces      6 edges    
    boundary flag =        16000000        status =                   active       
</pre>
  

  
