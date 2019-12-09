---
title: REORDER
tags: reorder, sort
---

# REORDER

-------

This command will reorder a mesh object according to a designated permutation vector. This can be used after the [**`SORT`**](SORT.md) command to reorder a mesh based on a sort key.
 

The command will decide to reorder nodes or elements based on the length of the permutaion vector. When elements are reordered all
  element attributes are also reordered. Mesh object arrays **itet** and **jtet** are updated. When nodes are reordered, all node based attributes are
  also reordered. Permanent mesh object attribute arrays such as **isn** are also updated.


## SYNTAX

<pre>     
<b>reorder</b>/mo_name/sort_key/
<b>reorder / -def- </b> /sort_key/
</pre>     
 
`sort_key` is the permutation vector - i.e. an integer node/element based mesh object attribute.

The permutation attribute can be any integer vector nnodes or nelements long with min value = 1, max value = nnodes/nelements and no repeated entries. 

## EXAMPLES

```
createpts / median
sort/ mo_pri /index/ascending/ ikey /itetclr xmed ymed zmed
reorder / mo_pri / ikey
```
sort and reorder the mesh mo_pri based on itetclr values and cell center location xmed, ymed, zmed. This will arrange into columns (after itetclr sort).

