
# read/lagrit #

Read a LaGriT restart file, the mesh object name and definitions are included in this file.

## SYNTAX ##

<pre>
<b>read</b>/ filename.<b>lg</b> or filename.<b>lagrit</b> [cmo_name (ignored)] [<b>ascii</b> or <b>binary</b>]

<b>read / lagrit</b>/ filename.<b>lg</b> or filename.<b>lagrit</b> / [cmo_name (ignored)] / [<b>ascii</b> or <b>binary</b>]
</pre>


filename.**`lg`** or filename.**`lagrit`** - these extensions are recognized and indicate a LaGriT restart file.

*`cmo_name`*  - This argument is ignored as all mesh objects in the restart file are named and defined.

**`ascii`** or  **`binary`**  - The default is **`ascii`**, but the code will determine file type if this option is missing.



## EXAMPLES ##

```
read / lagrit / file1.lg
```

file1.lg will be read as an ascii restart file.  If the read fails to find the ascii check string, the file will be closed and reopened for a binary read.

```
read / lagrit / hex1_mohex.bin.lg / binary
```
hex1_mohex.bin.lg will be read as a binary restart file. This example has the mesh object named mohex defined.
