---
Author: Jan Wills
GENERATOR: 'Mozilla/4.7 
[en
] (X11; I; IRIX 6.5 IP32) 
[Netscape
]'
---

 mo/readatt/mo\_name/attr\_name1/
[attr\_name2..
]operation/filename**

  cmo\_name is type character, required

  attr\_name1 is type character, required

  operation is either **add** which will cause new nodes to be added
  to the mesh object or ifirst, islast, istride which specifies the
  nodes whose values will be replaced.

  filename is type character required and specifies the ASCII file to
  be read.

  

  File parsing is set so that any line that has '
#' in column is
  ignored.

  Any line with the first word, not necessarily beginning with column
  one, not a real or integer is ignored.


 **EXAMPLES:**

  Read x,y,z coordinates and some attributes (node\_att1, node\_att2) 
  from a file of tabular data:

  mo / create / mo\_name / / / tet

  cmo/readatt/mo\_name/ xic,yic,zic,node\_att1,node\_att2 / 1,0,0 /
  input\_file

  

  **Where the input file may be a TecPlot format file something
  like:**

  **TITLE="Heterogeneity of TMCM 
#39, 1=newzone 81, 2=newzone 82"

  variables="x","y","z", "zone", "element"

  zone t="facies" I=191, J=136, K= 57

   0.53600E+06 0.41020E+07 0.00000E+00  2       1

   0.53610E+06 0.41020E+07 0.00000E+00  2       2

   0.53620E+06 0.41020E+07 0.00000E+00  2       3

  ...

  

  In the example above, the first three lines are ignored. None of the
  tagged information is retained. Ignored lines can be indicated by
  either a 
# in column one or anything other than a real or integer
  as the first token in a line.

  

  mo/readatt /mo\_tet2/xic,yic,zic/add///myfile**

  new nodes will be added to mo\_tet2, and their coordinates will be
  supplied from the file. The value of nnodes will be updated.

  Contents of myfile:

0.017     12.65     7.25

1.1         10.2         3.4

will cause 2 nodes to be added to the mesh.

mo/readatt /mo\_tet2/itp1/new\_node\_attr/pset,set,p1/myfile**

The values of itp1 will be replaced. If new\_node\_attr does not exist
it will be created as a VDOUBLE node based attribute. If **pset**p1
contains the nodes 12, 27 and myfile has the contents:

    0        100.2

              2         99.6

then itp1(12) will be set to 0, itp1(27) will be 2; newattr(12) will be
set to 100.2 and newattr(27) will be set to 99.6.



