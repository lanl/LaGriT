---
Author: Jan Wills
GENERATOR: 'Mozilla/4.7 [en] (X11; I; IRIX 6.5 IP32) [Netscape]'
---

 **readatt**/mo\_name/attr\_name1/[attr\_name2..]operation/filename

  cmo\_name is type character, required

  attr\_name1 is type character, required

  operation is either **add** which will cause new nodes to be added
  to the mesh object or ifirst, islast, istride which specifies the
  nodes whose values will be replaced.

  filename is type character required and specifies the ASCII file to
  be read.

 ** **EXAMPLES:** **

  **cmo/readatt **/mo\_tet2/xic,yic,zic**/add**///myfile

  new nodes will be added to mo\_tet2, and their coordinates will be
  supplied from the file. The value of nnodes will be updated.

  Contents of myfile:

0.017     12.65     7.25

1.1         10.2         3.4

will cause 2 nodes to be added to the mesh.

**cmo/readatt **/mo\_tet2/itp1/newattr**/pset,set,**p1/myfile

The values of itp1 will be replaced. If newattr does not exist it will
be created as a VDOUBLE node based attribute. If **pset**p1 contains the
nodes 12, 27 and myfile has the contents:

    0        100.2

              2         99.6

then itp1(12) will be set to 0, itp1(27) will be 2; newattr(12) will be
set to 100.2 and newattr(27) will be set to 99.6.
