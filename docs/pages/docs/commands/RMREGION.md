---
title: RMREGION
tags: rmregion
---


# RMREGION

---------------

Removes points that lie within the specified region. See [**`REGION`**](REGION.md).

## SYNTAX

<pre>
<b>rmregion</b> / region_name /
</pre>

`region_name` is the name of the previously defined geometry region.


## EXAMPLES

```
surface/insurf/intrface/sphere/0.,0.,0./.1/
region/sph_inside/ le insurf /

rmregion/sph_inside
rmpoint/compress
resetpts/itp
```
Remove nodes and elements defined by the sphere shaped region sph_inside.
First the region nodes are marked for removal, rmpoint/compress removes the marked nodes. The command resetpts/itp updates the itp boundary array.
