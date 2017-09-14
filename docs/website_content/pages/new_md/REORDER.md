---
GENERATOR: 'Mozilla/4.79 [en] (X11; U; Linux 2.4.18-3 i686) [Netscape]'
Generator: Microsoft Word 98
title: READ
---

 

 **REORDER**

  This command will reorder a MO according to a designated permutation
  vector.  The permutation vector can be any integer vector nnodes or
  nelements long with min value = 1, max value = nnodes/nelements and
  no repeated entries.   sort\_key is the permutation vector - i.e. an
  integer node/element based mesh object attribute.
  Reorder command will decide to reorder nodes or elements based on
  the length of the permutaion vector. When elements are reordered all
  element attributes are also reordered. itet and jtet arrays are also
  updated. When nodes are reordered, all node based attributes are
  also reordered. Arrays such as isn are also updated.

 **FORMAT:**

  **reorder**/cmo\_name/sort\_key/

  **reorder/ -def-** /sort\_key/

 EXAMPLE:

 
