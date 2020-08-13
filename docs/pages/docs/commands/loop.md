---
title: loop
tags: loop, do, foreach
---


# loop

--------------------


 
 The **`loop`** command repeatedly executes any LaGriT command with
 either a do loop type or foreach type variable argument control.

 The coomand supports loops up to 10 deep with a maximum of 250 tokens in the **`foreach`** list.

 
## SYNTAX

<pre>
<b>loop</b>/ <b>do</b> / variable / loop_start / loop_stop / loop_stride/ loop_end / LaGriT_command

<b>loop</b>/ <b>foreach</b> / variable / list_var1 list_var2 ...
 list_varN / loop_end / LaGriT_command
</pre>
 
 

 **`do`** or **`foreach`**  are keywords that determine the type of loop operation.

 `variable`  is a character string that can be used in the
 LaGriT\_command. The variable values are controled by the loop range
 or given in a variable list.

 `loop_start` / `loop_stop` / `loop_stride`  Integers that
 specify the initial, limit, and increment of variable.

`variable` / `list_var1` `list_var2` ... `list_varN`  Integers, reals
 or characters that are substituted into variable prior to each
 execution of the `LaGriT_command`.

 `loop_end` Required keyword string to terminate a foreach list. It
 is also REQUIRED after `loop_stride`.

 `LaGriT_command` Any valid LaGriT command including another **`loop`** command.

  
## EXAMPLES

```
 loop / foreach / MO / cmo1 cmo2 cmo3 / loop_end / cmo /
 delete / MO
 ```
 Delete a list of mesh objects.

```
loop / foreach / FILE / file1 file2 file3 / loop_end &

loop / foreach / MO / cmo1 cmo2 cmo3 / loop_end &

infile LaGriT.input_control_file
```
  
Execute a set of commands in a LaGriT control file that utilize the
 variables FILE and MO.

 
```
loop/ do / NX 2 3 1 / loop_end &

loop/ do / NY 4 5 1 / loop_end &

loop/ do / NZ 6 7 1 / loop_end &

loop/ foreach / X0 0 5.5 10.23678 / loop_end  &

createpts/xyz/NX,NY,NZ/X0 0. 0. / 100. 100. 100.
```
Execute a set of commands in a LaGriT control file that utilize the
 variables NX,NY,NZ,X0.

```
 loop / foreach / MO cmo1 cmo2 cmo3 / loop_end &

 loop / foreach / ATTRIBUTE / icr isn itp / loop_end &

 cmo/modatt/MO/ATTRIBUTE/ioflag/l
 ```
 
 

  

 Modify IO flags of three attributes in three different mesh objects.
[](../demos/trans/test/md/main_trans.md)
