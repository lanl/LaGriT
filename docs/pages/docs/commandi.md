---
Title: "command interface"
Tags: Manual command interface
---


# Command Interface

---


LaGriT runs on the command line as interactive mode or with an input command file. LaGriT will write two output files upon completion, by default they are `lagrit.out` (summary and reports for each command) and `lagrit.log` (saved commands).
Run on the command line with either method:

```
lagrit
    
lagrit < input_commands.lgi
```


Run the Tutorial and see Example command files at [Tutorials and Examples](tutorial/index.md)


To get started with the commands, the user must create a mesh object.
The mesh object can be defined by reading in a mesh file, or by creating one.


To create an empty mesh object use the command [**cmo** **/create**/](commands/cmo/cmo_create.md) mesh_name. 

There is no limit on the number of Mesh Objects that can be
defined, but at any time there is only one 'current' or 'active' Mesh
Object. For more advanced problems, such as those requiring more than
one Mesh Object or requiring extensions to the basic Mesh Object
template, the Mesh Object(s) is(are) manipulated via the [**cmo**](commands/CMO2.md) commands.


For example, additional user defined attributes may be added to
a Mesh Object by using the [**cmo** **/addatt**](commands/cmo/cmo_addatt.md) command, or the
'active' Mesh Object can be changed using the [**cmo/select**](commands/cmo/cmo_select.md) command.

This example session reads 2 mesh files into 2 mesh objects mo_tet and cmo_1. The command **cmo/list** reports all mesh objects including the default template and the 2 new mesh objects. The last mesh object created is the current mesh object. 

The command **cmo / status / -all- / brief** gives a brief description of each mesh object.

The command **finish** will exit the command line interface.

```
read / avs / tet.inp / mo_tet                                                    
                                                                
read / avs / test_tet_small.inp / cmo_1
                                                                 
cmo / list
                                                                        
The current-mesh-object(CMO) is: cmo_1                                          
 
  0    Mesh Object name: -default-                                              
  1    Mesh Object name: mo_tet                                                 
  2    Mesh Object name: cmo_1                                                  

cmo / status / -all- / brief
                                                        

The current-mesh-object(CMO) is: cmo_1                                          
 
  1 Mesh Object name: mo_tet                                                    
    number of nodes =          1110        number of elements =         5031    
    dimensions geometry =         3        element type =                tet    
    dimensions topology =         3        4 nodes      4 faces      6 edges    
    boundary flag =        16000000        status =                 inactive    
 
  2 Mesh Object name: cmo_1                                                     
    number of nodes =             5        number of elements =            2    
    dimensions geometry =         3        element type =                tet    
    dimensions topology =         3        4 nodes      4 faces      6 edges    
    boundary flag =        16000000        status =                   active 
   
   
finish
 
```

The output file lagrit.out will save the same information you see on the screen while running LaGriT. The output file lagrit.log will save the commands and you can copy this file and possibly edit to run again.

```
% cat lagrit.log

read/avs/tet.inp/mo_tet                                                         
read/avs/test_tet_small.inp/cmo_1                                               
cmo/list                                                                        
cmo/status/-all-/brief                                                          
finish                  
```
