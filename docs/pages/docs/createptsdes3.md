
Add points using spherical coordinates.
    
    createpts/rtp/2,6,30/0,0,0/1,45,360/1,1,1

Argument | Description
---------- | ---
rtp |      specifies sperical coordinates
2,6,30 |   create 2 points in the r direction, create 6 points in the t direction, create 30 points in the z direction
0,0,0   |   rmin = 0, tmin = 0, zmin = 0
1,45,360 |  radius from 0 to 1, angle in the xy-plane is 45 degrees, angle in the zy-plane is 360 degrees
1,0,1   |   rmins and rmaxs are used as cell vertices, tmins and tmaxs are used as cell vertices, zmins and zmaxs are used as cell vertices

Input deck:

    cmo/create/cmo1/
    surface/s3/reflect/sphere/0.,0.,0./1.,1.,1./
    region/r3/ lt s3 /
    mregion/mr3/ lt s3 /
    createpts/rtp/2,6,30/0,0,0/1,45,360/1,1,1
    dump/gmv/gmv2/
    finish

