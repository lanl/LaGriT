---
title: TRANS
tags: trans, translate
---

# TRANS

---------------------

Translate a mesh object from old to new coordinates.

## SYNTAX
<pre>

<b>trans</b>/ifirst,ilast,istride/xold,yold,zold/xnew,ynew,znew

<b>trans</b>/ifirst,ilast,istride/<b>enter</b> <b>zero</b> <b>original</b>/[<b>xyz</b> <b>rtp</b> <b>rtz</b>]/ [xdim,ydim,zdim]
</pre>


`ifirst,ilast,istride` is the range of points to translate. They can be defined as **pset,get,** pset_name  or node numbers where 1,0,0 means all.


### First form of this command:

translates a set of points **from** point `xold,yold,zold` **to** new coordinate `xnew,ynew,znew` with a linear translation.  This will then cause the remaining points in the set to be moved by the same translation.


### Second form of this command: 


**`enter`**  point set is translated so that (0,0,0) is located at the midpoint of min x,y,z and max x,y,z of the mesh. 


**`zero`** point set is translated so that (0,0,0) is located at the min x,y,z of the mesh. 


**`original`**  point set is translated to the original location before **enter** or **zero** was called. 


**`xyz`** is the default (**rtp** and **rtz** are reserved for future implementation). 


`xdim,ydim,zdim` indicate the axes along which to translate. For example, 1,1,0 or x,y will translate along
the x and y axes, the z values will not change.



## EXAMPLES

```
trans/pset,get,mypoints/ 0.,0.,0./2.0,2.0,0./
```
The points in the set mypoints will be moved 2 in the positive x direction and 2 in the positive y direction.

```
cmo/create/motet
copypts/motet/mopts

cmo/select/motet
trans/1,0,0/ zero

connect

trans/1,0,0/ original
```
Moving points close to zero coordinates will improve accuracy where coordinate numbers are large.
Here we move the points to zero, connect, and them move back to original coordinates.



[Click here for demos](../demos/description_trans.md)
