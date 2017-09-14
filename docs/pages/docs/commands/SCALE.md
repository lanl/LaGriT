---
title: SCALE
---

 **SCALE**

Scale a point distribution specified by ifirst,ilast,istride
according to the scale factors iscale,jscale, and kscale. One can
substitute the syntax pse get pset\_name for the ifirst, ilast,
istride variable to access a pset. The letters i,j, and k in the scale
factors correspond to coordinates specified by one of the geometry types
[**xyz** (Cartesian), **rtz** (cylindrical), **rtp** (spherical)].  
For example,

if geometry = **rtz** then iscale = radial coordinate, jscale = theta
coordiante, and kscale = z coordinate.

If geometry = **rtp** the iscale = radial coordiante, jscale = theta
coordinate and kscale = phi coordinate.

If the scaling option is **relative** then the scaling factors are
unitless multipliers with reference to some geometric center
(xcen,ycen,zcen). If the scaling option is **absolute** then the
scaling factors are constants added on to the existing coordinates. 
That is, absolute is really a translation rather than a rescale.

**FORMAT:** 

**scale**/ifirst,ilast,istride**/absolute** **relative**/xyz/rtzrtp/iscale,jscale,kscale/xcen,ycen,zcen
