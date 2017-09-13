---
Author: Carl Gable
GENERATOR: 'Mozilla/4.7C-SGI 
[en
] (X11; I; IRIX64 6.5 IP30) 
[Netscape
]'
title: loop
---

 

loop
----

 

 The **loop** command repeatedly executes any LaGriT command with
 either a do loop type or foreach type variable argument control.

 **loop** supports loops up to 10 deep with a maximum of 250 tokens in
 the /foreach/ list.



 

FORMAT:

 **loop**/ **do** / variable / loop\_start / loop\_stop / loop\_stride
 / **loop\_end** / lagrit\_command

 **loop**/ **foreach** / variable / list\_var1 list\_var2 ...
 list\_varN / **loop\_end** / lagrit\_command

 **do** or **foreach** : are keywords that determine the type of loop
 operation.

 variable : is a character string that can be used in the
 lagrit\_command. The variable values are controled by the loop range
 or given in a variable list.

 loop range: loop\_start / loop\_stop / loop\_stride : Integers that
 specify the initial, limit, and increment of variable.

 variable\_list: list\_var1 list\_var2 ... list\_varN : Integers, reals
 or characters that are substituted into variable prior to each
 execution of the lagrit\_command.

 **loop\_end**: Required keyword string to terminate a foreach list. It
 is also REQUIRED after loop\_stride.

 lagrit\_command: Any valid LaGriT command including another **loop**
 command.

  

  

EXAMPLES:

 **loop / foreach** / MO / cmo1 cmo2 cmo3 / **loop\_end** / **cmo** /
 **delete** / MO

  

 Delete a list of mesh objects.

 **loop** / **foreach** / FILE / file1 file2 file3 / **loop\_end** &

 **loop** / **foreach** / MO / cmo1 cmo2 cmo3 / **loop\_end** &

 **infile** lagrit.input\_control\_file

  

 Execute a set of commands in a LaGriT control file that utilize the
 variables FILE and MO.

 

 **loop** / **do** / NX 2 3 1 / **loop\_end** &

 **loop** / **do** / NY 4 5 1 / **loop\_end** &

 **loop** / **do** / NZ 6 7 1 / **loop\_end** &

 **loop** / **foreach** / X0 0 5.5 10.2345678 / **loop\_end** &

 **createpts****/xyz**/NX,NY,NZ/X0 0. 0. / 100. 100. 100.

  

 Execute a set of commands in a LaGriT control file that utilize the
 variables NX,NY,NZ,X0.

 **loop** / **foreach** / MO cmo1 cmo2 cmo3 / **loop\_end** &

 **loop** / **foreach** / ATTRIBUTE / icr isn itp / **loop\_end** &

 **cmo****/modatt**/MO/ATTRIBUTE**/ioflag**/l

  

 Modify IO flags of three attributes in three different mesh objects.

[](demos/trans/test/md/main_trans.md)
