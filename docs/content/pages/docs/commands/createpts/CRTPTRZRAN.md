---
Author: Jan Wills
GENERATOR: 'Mozilla/4.7 [en] (X11; I; IRIX 6.5 IP32) [Netscape]'
---

 

 **CREATEPTS/RANDOM**

  This routine is used to add random points with a given target
  spacing to the region of space defined by the input minimum and
  maximum coordinate values using the specified geometry (xyz, rtz, or
  rtp), and the given local origin (specified in xyz coordinates).
  Within the bounding geometry, the points are distributed uniformly
  in space, with the average separation targeted at the input value of
  the spacing.  Near the boundaries of the geometry, the uniform
  distribution is modified slightly in order to create a well defined
  outer boundary.  Points are added separately on the corners, edges,
  and surfaces of the bounding geometry, uniformly randomly
  distributed with the same target spacing on each of these boundary
  objects. Points in the interior are offset by the specified edge
  protection distance from the exterior.  This separation helps
  LaGriT's connect algorithm avoid creating artificial "pits" in the
  interface surfaces.

 FORMAT

  reatepts/random** / cgeom/ spacing / rmin1,rmin2,rmin3 /
  rmax1,rmax2,rmax3  & [/ xoff,yoff,zoff / edgedist /
  ranseed1,ranseed2 ]
 
  while only reatepts/random** is required (will result in a single
  point at the origin), it is recommended that you use as the minimal
  command: reatepts/random** / cgeom / spacing /  rmin1,rmin2,rmin3
  / rmax1,rmax2,rmax3
 
  cgeom

     geometry label (same convention as for rz)

     allowed values: xyzrtprtz (cartesian, cylindrical, spherical)

     default: xyz if not present, error return if not allowed
 
  spacing

     target separation between the random points

     allowed values: spacing&gt;0

     default: spacing=1
 
  rmin1,rmin2,rmin3 / rmax1,rmax2,rmax3

     minimum and maximum coordinate values

     allowed values:

        all geometries: rmax.ge.rmin

        **rtz:** rmin1.ge.0, rmax2-rmin2.le.360

        **rtp**: rmin1.ge.0, rmin2.ge.0, rmax2.le.,
  rmax3-rmin3.le.360

      defaults: rmin=0, rmax=rmin
 
  xoff,yoff,zoff (specified in xyz coordinate system)

     local origin shift

     defaults: 0
 
  edgedist

     edge protection distance

     default: spacing/2

     recommended value: spacing/2

  if the spacing is larger with respect to the dimension of the
  geometry, the default setting may result in few or no interior
  nodes.  In this case decrease the value of edgedlist.
 
  ranseed1,ranseed2

     seeds for the random number generator

     defaults: -1 (do not re-seed, recommended)

        if either seed is .le. zero, the seeds are ignored

     recommended values if reseed:

        large-ish integers, ranseed1&gt;ranseed2&gt;0, ranseed2 odd.

     No initial seeds are needed, and repeating the command

     with the identical parameters and seeds should result

     in the identical point distribution. Repeating the

     command with no seeds specified should result in

     different point locations with the same distribution.

   

 EXAMPLES

  reatepts/random** / **xyz** / .1 / 0 0 0 / 1 1 1 /

     random points with target spacing 0.1 in a 1x1x1 box
 
  reatepts/random**/ **rtz** / .1 /  0,0,0 / 1,,360 / 2,3,4 /
  0.2

     random points with target spacing 0.1 in a cylinder

     of radius 1 centered at xyz=(2,3,4) and with an

     edge protection distance of 0.2
 
  reatepts/random/rtp** / .5 /  5,0,0 / 5,,360 /  , ,  /  /
  98765 4321/

     random points with target spacing 0.5 on the surface

     of a sphere of radius 5 centered at the origin

     with new random seeds

 CAVEATS

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
