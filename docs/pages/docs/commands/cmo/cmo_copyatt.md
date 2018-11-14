---
title: 'cmo/copyatt'
---
 

# cmo/copyatt #

The `mo/copyatt` command is used to copy one attribute field to
another. There is currently no provision for indexed sets.



## FORMAT: ##

<pre>
<b>mo / copyatt</b> / cmo_sink / cmo_src / attname_sink / attname_src
</pre>

**`cmo_sink / cmo_src`** : are the mesh object names to write to(sink) and
from (source). The two names can represent the same mesh object.

**`attname_sink / attname_src`** : are the mesh object attributes to write
to (sink) and from (source). If the two attributes differ in type or
length, a message will be written. The routine does allow the values of
an element attribute to be written to the nodes of that belong to each
element. To create a mesh object where each element has its own unique
set of nodes, create parent-child chains for each element. This can be
done using commands `mo/set_id` and `settets`.



## EXAMPLES: ##

<pre>
<b>mo / copyatt</b> / cmo1 / cmo2 / itetclr / itetclr
<b>mo / copyatt</b> / cmo1 / cmo2 / itetclr
</pre>

Both versions will copy `itetclr` from cmo2 to cmo1.
 
<pre>
<b>mo / addatt</b> / cmotet / elevation
<b>mo / copyatt</b> / cmotet cmotet / elevation zic
</pre>

In the mesh object cmotet, attribute zic is copied to attribute elevation.

<pre>
<b>mo / copyatt</b> cmotri cmotri/ itetsav itetclr
<b>mo / set_id</b> / cmotri / element / itetclr
<b>settets</b>

<b>mo / copyatt</b> / cmotri cmotri / imt itetsav
</pre>

 Copy element `itetclr` values into attribute `itetsav`. Create
 parent-child chains so each element has its own set of nodes. Copy the
 saved `itetsav` values into node attribute `imt` for each element. Each
 element will have nodes where the `imt` values match element `itetsav`
 values.
 
