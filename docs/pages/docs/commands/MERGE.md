---
title: MERGE
tags: merge
--- 

# MERGE

--------------------

Merge pairs of points together. 

The command [massage](MASSAGE.md) may be used to merge nodes together based on the edge distance separating the nodes.


## SYNTAX

<pre>
<b>merge</b>/first_point/second_point

<b>merge</b>/1st_of_pair1/2nd_of_pair1/1st_of_pair2/2nd_of_pair2/../1st_of_pairn/2nd_of_pairn
</pre>

Parameters are pairs of points to merge.

On return, the first_point of a pair is the survivor unless `first_point` may not be removed ( a
  corner point for example), then the command operates as if `first_point` and `second_point` have been interchanged. If there is no confirmation of the merge, one or both of the points may be inactive, or the merge may be illegal because the points are not neighbors or because this merge is disallowed by the merge tables.
  
Merging may trigger other merges by the reconnection step that follows the merge.

## EXAMPLES

```
merge/21,22/
```
