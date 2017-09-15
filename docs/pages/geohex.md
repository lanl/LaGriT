        cmo/create/3dmesh

        surface/outsurf/reflect/sphere/0.,0.,0./1.0/

        region/outside/ le outsurf /

        mregion/moutside/ le outsurf /

        rzs/2/2,200/0.,1./0.,0.,0.0/1/0,0.0/

        filter/1,0,0

        dump/gmv/gmv.points

        setpts

        search/delaunay

        settets/

        dump/gmv/gmv.spheres

        finish
[Return to LaGriT Home Page](../index.md)

 

