---
GENERATOR: 'Mozilla/4.7C-SGI 
[en
] (X11; I; IRIX64 6.5 IP30) 
[Netscape
]'
Generator: Microsoft Word 98
title: 1
---

**   Memory Manager**

LaGriT uses dynamic memory allocation. Memory is referenced by a two
part name, block name and partition name. It is allocated in integer or
real blocks(real is implemented as real
*8). Each memory block is
preceeded by a header and terminated by a trailer. The memory manager
always returns the pointer to the data section of the memory block.
Length is specified in words. Type indicates if the words are integer or
real. Different platforms will have different values for integer and
real word lengths. These machine dependent values are collected in the
include file machine.h.

The following parameter names are used by the memory management
routines:

 

  ----------- ----------------------------------------------------------------------
  blk         block name of memory block
  prt         partition name of memory block 
  iadr        pointer to memory block (data section) 
  length      number of words in block or to be allocated
  itype       1 for integer, 2 for real(real
*8) , 3 for character
  icscode     return code, 0 for no errors 
  increment   numbers of words to use as increment to size of memory block
  need        required size of memory block
  extra       extra amount to add to size of memory block when increasing its size
  ----------- ----------------------------------------------------------------------

In the following list underlined parameters are required input
parameters.

 

Allocate a block of memory:

**mmgetblk**(blk,prt,iadr,length,itype,icscode)

Release a block of memory:

**mmrelblk**(blk,prt,iadr,icscode)

 

Release a partition of memory -- all blocks belonging to this partition
will be released:

**mmrelprt**(prt,icscode)

Increment a block of memory by increment:

**mmincblk**(blk,prt,iadr,increment,icscode)

 

Find pointer (idar) and length (length) to a block of memory:

**mmfindbk**(blk,prt,iadr,length,icscode)

 

Return type (itype) of a block of memory:

**mmgettyp**(iadr,itype,icscode)

 

Return number of words (length) in a block of memory given the address
of the block:

**mmgetlen**(iadr,length,icscode)

 

Return number of words (length) in a block of memory given the name of
the block:

**mmblklen**(blk,prt,iadr,length,icscode)

Return name (blk, prt) of a block of memory:

**mmgetnam**(iadr,blk,prt,icscode)

Return address (iadr) of a block of memory:

**mmgetpr** (blk, prt,iadr,icscode)

Print a dump of allocated memory. This is useful for debugging purposes;
the dump is listed by increasing pointer address:

**mmprint**()

Verify memory integrity. Print debug information if the memory block
headers or trailers have been overwritten.

**mmverify**()

Find a block of memory, increase it if shorter than length, or allocate
it if it doesn't exist;

**mmggetbk**(blk,prt,iadr,length,itype,icscode)

Change the length of a block of memory to length.

**mmnewlen**(blk,prt,iadr,length,icscode)

Change the name of a partition to prt1 from prt2

**mmnamprt**(prt1,prt2,icscode)

Get the address of the requested block and change the size of the
storage to need+extra if need &gt; length.  Always return in length the
new length of the block.  If length is 0 on entry, allocate a new
block.

**mm\_ovall**(blk,prt,iadr,need,extra,length,type,status)
