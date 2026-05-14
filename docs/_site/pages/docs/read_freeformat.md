 **read/gmvfreeformat**

Option **read/gmvfreeformat** for ascii gmv files to be read with read(*) type read statements.

This command requires that the mesh object name be specified or that a mesh object has been created
  previously.  This file format contains no geometry information.  The file will be read using free format  read statement, i.e.  read(unit, *)


## SYNTAX

  **read**/gmvfreeformat/filename/[cmo-name]



## EXAMPLES

```
read/ gmvfreeformat/file.gmv/cmo1


cmo/create/cmo1
read/ gmvfreeformat/file.gmv  
```
   

