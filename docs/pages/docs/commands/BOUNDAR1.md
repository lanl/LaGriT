---
title: BOUNDARY 
tags: ok
---

# BOUNDARY

The boundary routine operates on the current mesh object. For the
nodes lying on the specified surface(s), it sets the specified node
based attribute to the specified value. Optionally boundary will
call the user supplied subroutine `set_user_bounds` ([see IV.e.8](../miscell.md)).

## FORMAT:

<pre>
<b>boundary/dirichlet</b>/ attr_name / <i>[valueidentifier]</i> /surface_list
</pre>

where:

| Argument     | Description
|--------------|---------------------------------------------------------------------------
| `dirichlet`  | is currently unused but must be specified
| `attr_name`  | is the name of the attribute to be set
| `value`      | is a constant, and is the value to which the attribute is set
| `identifier` | is a character string that will be passed to subroutine `set_user_bounds`

------------------------

`surface_list` is one of:
  
* `-all-` (all boundary nodes)
* `surface_name/inclusive` (all bndry nodes on surface)
* `surface_name/exclusive` (all bndry nodes ONLY on surface)
* `surface_name/` (same as exclusive)
* `surface_name1/surface_name2/inclusive` (all bndry nodes on the union of the surfaces)
* `surface_name1/surface_name2/exclusive` (default) (all bndry nodes ONLY on the intersection of the surfaces)
* `surface_name1/surface_name2/surface_name3/...` (same as exclusive)

 
## EXAMPLES:

<pre>
<b>boundary/dirichlet</b>/vd_v/7.0/<b>-all-</b>/
</pre>

sets the attribute `vd_v` for all boundary nodes to be 7.0.

<pre>
<b>boundary/dirichlet</b>/vi_s/8.0/pbot/
<b>boundary/dirichlet</b>/vd_v/9.0/pbot/inclusive/
</pre>

sets the attribute `vd_v` for the nodes that are on the surface `pbot` to be 9.0.

<pre>
<b>boundary/dirichlet</b>/vd_s/13.0/pfrt
</pre>

sets the attribute `vd_s` for the nodes that are on the union of the surfaces `pfrt` and `prgt` to 13.0.

<pre>
<b>boundary/dirichlet</b>/vi_t/12.0/prgt/
<b>boundary/dirichlet</b>/bconds/top_plane/s1,s2,s3/
</pre>

will pass the set of nodes on the intersection of surfaces s1,s2, and s3 along with the string top-plane to subroutine `set_user_bounds`.
