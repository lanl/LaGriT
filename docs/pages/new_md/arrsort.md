---
GENERATOR: 'Mozilla/4.75 
[en
] (X11; U; IRIX 6.5 IP32) 
[Netscape
]'
Generator: Microsoft Word 98
title: 6
---

**  Array sorting**

 The following utility routines sort arrays.

 ** hpsort1(n,ra,ascend,iprm)**

   --------- ---------------------------------------
   n         number of elements to be sorted
   ra        a real array
   ascend    real which controls direction of sort
   iprm      integer arry to be reordered
   --------- ---------------------------------------

 **hpsorti(n,ia)**

   ---- ---------------------------------
   n    number of elements to be sorted
   ia   integer array to be sorted
   ---- ---------------------------------

 **hpsortim(n,m,md,itemp,ia)**

   ------- --------------------------------------------------------------
   n           no. of columns to sort into ascending order
   m           maximum number of keys (rows) to consult for comparisons
   md          column length of array IA (M&lt;=MD)
   itemp       temp array of length MD.
   ia          integer array of MD-tuples to be reordered
   ------- --------------------------------------------------------------

 ** hpsortimp(n,m,md,ia,ascend,iprm)**

   -------- --------------------------------------------------------------------
   n        no. of elements to be sorted
   m        we interpret array IA as M-tuples
   md       actual first dimension of arry IA
   ia       integer array of values which determine how IPRM will be reordered
   ascend   real
*8 which controls direction of sort.
   iprm     integer array to be reordered
   -------- --------------------------------------------------------------------

 (this routine available but not distributed - contact site manager)

  

 **hpsort(n,ra)**

   ---- ---------------------------------------
   n             no. of elements to be sorted
   ra            real
*8 array to be sorted
   ---- ---------------------------------------

 **hpsortrmp(n,m,md,a,ascend,iprm)**

   -------- -------------------------------------------------------------------------
   n        no. of elements to be sorted
   m        array A is treated a M-tuples
   md       actual first dimension of A
   a        real
*8 array to be used in ordering  IPRM
   ascend   real
*8 to control ascending descending sort order (1=ascend,0=descend)
   iprm     integer array that will be reordered
   -------- -------------------------------------------------------------------------

 **hpsortip(n,ia,ascent,iprm)**

   -------- ---------------------------------------------------------------------------
   n        number to sort
   ia       integer array that determines ho IPRM will be sorted
   ascend   real
*8 that controls if sort order is ascending or descending (1=ascend)
   iprm     sorted array of integers 
   -------- ---------------------------------------------------------------------------



