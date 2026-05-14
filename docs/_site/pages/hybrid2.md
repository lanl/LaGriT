
        * create a 3-D hexahedral mesh object and name it 3dmesh

        cmo/create/3dmesh///hex/

        * create a cylinder inside a cylinder

        * inner cylinder will be replaced by tet grid

        surface/cyl1/reflect/cylinder/0.0,0.0,0.0/0.0,0.0,10.0/5.0/

        surface/top/reflect/plane/0.,0.,10./1.,0.,10./1.,1.,10. /

        surface/bot/reflect/plane/0.,0.,0./1.,0.,0./1.,1.,0. /

        surface/cyl2/intrface/cylinder/0.0,0.0,1.0/0.0,0.0,9.0/4.0/

        surface/cutpl1/intrface/plane/0.,0.,1./1.,0.,1./1.,1.,1./

        surface/cutpl2/intrface/plane/0.,0.,9./1.,0.,9./1.,1.,9./

        *define geometric regions

        region/inner/ lt cyl2 and lt cutpl2 and gt cutpl1 /

        region/outer/ le cyl1 and le top and ge bot and &

        ( ge cyl2 or ge cutpl2 or le cutpl1 )

        * define material regions

        mregion/inner/ lt cyl2 and lt cutpl2 and gt cutpl1 /

        mregion/outer/ le cyl1 and le top and ge bot and &

        ( gt cyl2 or gt cutpl2 or lt cutpl1 )

        * distribute points in cylindrical arrangement

        rz/rtz/11,61,21/0.,0.,0./5.,360.,10./1,1,1/

        * assign material colors to the points

        * identify points that are on material interfaces

        * identify constrained points

        * connect hex mesh

        rzbrick/xyz/61,11,21/1,0,0/connect/

        zq/itp/1,0,0/0

        zq/imt/1,0,0/0

        zq/icr/1,0,0/0

        setpts

        dump/x3d/x3d1

        settets

        zq/itp/100,130,1

        dump/gmv/gmv1

        * remove inner region

        rmmat/inner

        rmpoint/compress

        geniee

        *zq/xic/1,0,0

        *zq/itp/1,0,0

        dump/gmv/gmv2

        * grid tet part

        cmo/create/tetgrid///tet

        rz/rtz/9,61,17/0.,0.,1./1.,360.,9./1,1,1/

        filter/1,0,0/

        search

        zq/imt/1,0,0/1

        zq/xic/1,0,0

        zq/itp/1,0,0

        * add meshes

        dump/x3d/x3d1

        addmesh/pyramid/hybrid/tetgrid/3dmesh/

        surface/cyl1/reflect/cylinder/0.0,0.0,0.0/0.0,0.0,6.0/2.0/

        surface/top/reflect/plane/0.,0.,6./1.,0.,6./1.,1.,6. /

        surface/bot/reflect/plane/0.,0.,0./1.,0.,0./1.,1.,0. /

        surface/cyl2/intrface/cylinder/0.0,0.0,2.0/0.0,0.0,4.0/1.0/

        surface/cutpl1/intrface/plane/0.,0.,2./1.,0.,2./1.,1.,2./

        surface/cutpl2/intrface/plane/0.,0.,4./1.,0.,4./1.,1.,4./

        *define geometric regions

        region/inner/ le cyl2 and le cutpl2 and ge cutpl1 /

        region/outer/ le cyl1 and le top and ge bot and &

        ( gt cyl2 or gt cutpl2 or lt cutpl1 )

        * define material regions

        mregion/inner/ lt cyl2 and lt cutpl2 and gt cutpl1 /

        mregion/outer/ le cyl1 and le top and ge bot and &

        ( gt cyl2 or gt cutpl2 or lt cutpl1 )

        filter/1,0,0/

        dump/x3d/x3d2

        geniee

        zq/imt/1,0,0/0/

        zq/itp/1,0,0/0/

        zq/icr/1,0,0/0/

        setpts

        settets

        * dump mesh to some output form

        dump/gmv/gmv.hybrid/hybrid

        * terminate processing

        finish



[Return to LaGriT Home Page](index.md)

 

