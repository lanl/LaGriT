---
title: CREATEPTS/RANDOM
tags: CREATEPTS/random 
---


# CREATEPTS/RANDOM


--------------------


This routine is used to add random points with a given target spacing to the region of space defined by the input minimum and
maximum coordinate values using the specified geometry (xyz, rtz, or rtp), and the given local origin (specified in xyz coordinates).

Within the bounding geometry, the points are distributed uniformly in space, with the average separation targeted at the input value of
the spacing.  Near the boundaries of the geometry, the uniform distribution is modified slightly in order to create a well defined
outer boundary.  Points are added separately on the corners, edges, and surfaces of the bounding geometry, uniformly randomly
distributed with the same target spacing on each of these boundary objects. Points in the interior are offset by the specified edge
protection distance from the exterior.  

This separation helps LaGriT's connect algorithm avoid creating artificial "pits" in the interface surfaces.

While only `createpts/random` is required (will result in a single point at the origin), it is recommended that you use as the minimal command, first line below.


## SYNTAX

<pre>
<b>createpts/random</b> /geom/spacing/ rmin1,rmin2,rmin3 / rmax1,rmax2,rmax3 

<b>createpts/random</b> /geom/spacing/rmin1,rmin2,rmin3 /rmax1,rmax2,rmax3 / &
                        [ xoff,yoff,zoff / edgedist / ranseed1,ranseed2 ]
</pre>


### Geometry Options for `geom`:

 
  **xyz** specifies Cartesian coordinates (default).

  **rtz** specifies cylindrical coordinates.

  **rtp** specifies spherical coordinates.

 
###  Options:

`spacing` is the target separation between the random points, values must be &gt; 0 (default is 1).

 
`rmin1,rmin2,rmin3` / `rmax1,rmax2,rmax3` are the minimum and maximum coordinate values (defaults: rmin=0, rmax=rmin).
For **rtz** rmax2-rmin2 must be  &lt;= 360. For **rtp** the values should be rmax2 &lt;= 180, and  rmax3-rmin3 &lt;= 360.
All min values must be &gt;= 0.


 
`xoff,yoff,zoff` is the local origin shift specified in xyz coordinate system (default is 0).

 
`edgedist` is the edge protection distance (default and recommended: spacing/2).
Note: if the spacing is larger with respect to the dimension of the geometry, the default setting may result in few or no interior
  nodes.  In this case decrease the value of edgedist.  


`ranseed1, ranseed2` are seeds for the random number generator, the  default is -1 (do not re-seed, recommended).  If either seed is .le. zero, the seeds are ignored.  Recommended values if reseed:
<br>
large-ish integers, ranseed1 > ranseed2 > 0, ranseed2 odd.
<br>
No initial seeds are needed, and repeating the command with the identical parameters and seeds should result
    in the identical point distribution. Repeating the command with no seeds specified should result in
    different point locations with the same distribution.  


<hr>   

## EXAMPLES

<pre>
<b>createpts / random / xyz</b> / .1 / 0 0 0 / 1 1 1 /
</pre>

Random points with target spacing 0.1 in a 1x1x1 box.
 
<pre>
<b>createpts / random / rtz</b> / .1 /  0,0,0 / 1,,360 / 2,3,4 / 0.2
</pre>

Random points with target spacing 0.1 in a cylinder of radius 1 centered
at xyz=(2,3,4) and with an edge protection distance of 0.2.
 
<pre>
<b>createpts / random / rtp</b> / .5 /  5,0,0 / 5,,360 /  , ,  /  / 98765 4321/
</pre>

Random points with target spacing 0.5 on the surface of a sphere of radius 5
centered at the origin with new random seeds.

## CAVEATS

Filter should be used afterwards to remove possibly duplicate
points. The algorithm to insure the points are uniformly distributed
in space is not clever about handling values outside the allowed
range for **rtz** and **rtp** geometries and so it simply truncates
them to the allowed range if possible or aborts. Most importantly,
angles are in degrees and theta for the rtp geometry runs from 0 to
 degrees, with 0 degrees being the +z axis. It does know about
the angular periodicity and there should be only the "corner" point
artifacts of, eg, the +x axis being the origin of phi (rtp) or theta
(rtz) if a full 360 degfrees for these two variables in their
respective coordinate systems is used.
