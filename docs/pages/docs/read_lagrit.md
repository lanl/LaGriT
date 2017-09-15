**read/LaGriT**

read in a LaGriT restart file

SHORT **FORMAT:**

**read**/filename.[lglagritLaGriT]/cmo\_name

[Note that the filename is case-sensitive, though the extension
itself is not. When using the short format, the file will be read in
ascii mode.]

LONG **FORMAT:**

**read** **/LaGriT**/file\_name/[cmo-name]/[**ascii**
**binary**]

     cmo\_name    -    ignored all mesh objects are read from the
file

    The default is **ascii**, but the code will determine file type
if nessary.

**EXAMPLES:**

**read**[/]**LaGriT**[/file1]
file1 will be read as ascii.  If the read fails to find the ascii
check string, the file will be closed and reopened for a binary
read.

**read** **/LaGriT**/file2/**/binary**
