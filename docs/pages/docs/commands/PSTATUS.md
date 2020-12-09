---
title: PSTATUS
tags: pstatus 
---

# PSTATUS See also [PSET](PSET.md)

---------------------

Saves, removes, retrieves, or replaces a specified set of points,
  usually the last set of points defined by a generator command or the
  set of points defined by ifirst,ilast,istride. Note that point sets
  must be specified in sequence in order for this command to work
  properly.


## SYNTAX

<pre>
<b>pstatus</b>
</pre>
Returns current point status counters

<pre>
<b>pstatus</b> /<b>save</b>/name/ifirst,ilast,istride
</pre>
Saves the point status numbers, ifirst,ilast,istride under name

<pre>
<b>pstatus</b> /<b>store</b>/name/ifirst,ilast,istride
</pre>
Overwrites what was in name with ifirst,ilast,istride

<pre>
<b>pstatus/ delete</b>/name
</pre>
Deletes values from name

<pre>
<b>pstatus/get</b>/name
</pre>
Retrieves values from name


