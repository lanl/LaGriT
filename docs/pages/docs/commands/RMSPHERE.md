---
title: RMSPHERE
tags: rmsphere
---

# RMSPHERE

---------------

Removes a sphere of points from a point distribution.


## SYNTAX

<pre>
<b>rmsphere</b> / inner_radius/ outer_radius/ xcen,ycen,zcen/
</pre>

`inner_radius`  and `outer_radius` are the distances from sphere center point.

`xcen,ycen,zcen` is the sphere center point.


## EXAMPLES

```
rmsphere / 0. .1 / 0. 0. 0.

rmpoint/compress
resetpts/itp
```
Remove nodes and elements defined by the sphere at zero coordinates and within a distance of .1. 
First the nodes are marked for removal, rmpoint/compress removes the marked nodes. The command resetpts/itp updates the itp boundary array.


