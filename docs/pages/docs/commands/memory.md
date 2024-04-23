---
title: memory
tags: memory
---


# MEMORY


These commands report the current state of LaGriT's dynamic memory
allocation. LaGriT arrays are referenced by memory management by a two
part name, block name and partition name. It is allocated in integer or
real blocks. Each memory block is preceeded by a header and terminated by a trailer. Different platforms
will have different values for integer and real word lengths. 

Read more on LaGriT memory management at [Memory Manager](../memmang.md)


The following memory command options are recognized:

**`memory/verify`**

verify the integerity of LaGriT memory manager storage by checking
 that the known blocks have not been overwritten. If corruption is
 detected, an array map will be printed. 
 Nothing is printed if there memory is successfully verified.

**`memory/print`**

print an address map of the LaGriT managed arrays. For each array the
 following is printed; index, length, type, memory address, associated
 name, and partition. The partition is the grouping of arrays by usage.
 Common partitions include the mesh object (by name), global memory,
 and temporary memory for work arrays.
 

**`memory/maxmalloc`**

Report estimate of possible amount of memory available for allocation
 by LaGriT. This test will make incremental calls to internal LaGriT
 memory allocation (mmgetblk) until failure. The report will include
 the total Megabytes where allocation succeeded, and amount at which
 allocation failed. This command will also print a map of the memory
 manager storage.


## EXAMPLES

```
memory / verify
```
Will be silent if no problems are detected.

```
memory / print
```
Sample for 64 bit output showing sizes and allocated memory:
<pre class="lg-output">

 MEMORY SIZES :
 Sizeof char    (type 3) =  1 bytes      Sizeof long        =   8 bytes
 Sizeof real*8  (type 2) =  8 bytes      Sizeof pointer     =   8 bytes
 Sizeof integer (type 1) =  4 bytes      Sizeof INT_PTRSIZE =   8 bytes

INDEX         LENGTH    TYPE       ADDRESS     NAME                  PARTITION
  29          40000000   2       -14248416     xic                   cmo1    
   1                10   3       143632720     global_name           global_lg
  31          40000000   2      1760710688     zic                   cmo1    
  30          40000000   2      2080714784     yic                   cmo1   

Total BYTES =    2.400E+09   Total MEGABYTES =    2.400E+03
</pre>

```
memory/maxmalloc
```
Sample 64 bit output showing max allocation on Ubuntu Linux (this can take awhile before failure):

<pre class="lg-output">
 Looking for malloc to fail, expect errors ....
 Allocate blocks of                200000  real values and    1600000.0000000000       bytes
 Max unsigned for 32 bit is              4,294,967,295
 Max unsigned for 64 bit is 18,446,744,073,709,551,615
 Stop test value for allocated bytes is    1.8000000404716257E+019
           1  >>            1  number reals =    200000.00000000000       total bytes =    1600000.0000000000
           2  >>            1  number reals =    400000.00000000000       total bytes =    3200000.0000000000
           3  >>            1  number reals =    800000.00000000000       total bytes =    6400000.0000000000
 ...
           18  >>            1  number reals =    26214400000.000000       total bytes =    209715200000.00000
Killed
</pre>





