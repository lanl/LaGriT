Description

    createpts/rtz/4,6,11/0,0,0/3,,10/1,0,1

Distribute points using the rtz cylindrical coordinate system.  This
results in 264 points arranged around the z- axis. There are 3 rings
of points at distances r=1., r=2. and r=3. from the z-axis. There
are11 sets of these three rings of points and heights z=0., z=1.,
z=2.,...,z=10. In each ring there are 6 points where each pair of
points is separated by 60°; note that ijz=0 requests that points be
placed at cell centers, hence the first point will be at 30° not at
0°. Corresponding to r=0, there will be 6 identical points at 11
intervals along the z-axis at heights z=0., z=1., z=2.,...z=10.
Filter should be used to remove these duplicate points. Note:
Cylinders are open but finite. To create a closed cylinder cap both
ends with planes.

Argument | Description
---------  | --------------------------------------------------------------------------------------------------------------------------------
rtz |   specifies cylindrical coordinates
4,6,11 | create 4 points in the r direction, create 6 points in the t direction, create 11 points in the z direction
0,0,0 |   rmin = 0, tmin = 0, zmin = 0
3,,10 | radius from 0 to 3, angle in the xy-plane is , height along the z-axis is 10
1,0,1 |   rmins and rmaxs are used as cell vertices, tmins and tmaxs are used as cell centers, zmins and zmaxs are used as cell vertices

Input deck:

    cmo/create/3dmesh

    surface/outer/reflect/cylinder/0,0,0/1,0,0/1/

    region/r1/ le outer /

    mregion/m1/ le outer /

    createpts/rtz/4,6,11/0,0,0/3,,10/1,0,1/

    dump/gmv/gmv2

    finish

