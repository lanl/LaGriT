


        *input.hybrid


        * this deck will create a hybrid grid using addmesh


        * create tet grid first


        * remove region where hex grid will be inserted


        * create hext grid


        * use addmesh to create hybrid grid


        * repeat geometry specifications for hybrid grid


        * create a 3-D tetrahedral mesh object and name it 3dmesh

        cmo/create/3dmesh/


        * unit cube

        surface/cube/reflect/box/0.0,0.0,0.0/1.0,1.0,1.0/


        * define z=.4,.6 plane as interface

        surface/cutpl1/intrface/plane/0.,0.,.4/1.,0.,.4/1.,1.,.4/

        surface/cutpl2/intrface/plane/0.,0.,.6/1.,0.,.6/1.,1.,.6/


        *define geometric regions

        region/top/ le cube and ge cutpl2 /

        region/bottom/ le cube and le cutpl1 /

        region/middle/ le cube and gt cutpl1 and lt cutpl2 /


        * define material regions

        mregion/top/ le cube and gt cutpl2 /

        mregion/bottom/ le cube and lt cutpl1 /

        mregion/middle/ le cube and gt cutpl1 and lt cutpl2 /


        * create 25 points (5x5x1) in a plane above the unit cube


        * place points on the boundaries in the x and y directions (1,1,0)

        rz/xyz/5,5,11/0.,0.,0./1.,1.,1.0/1,1,1/


        * give the points defined by the rz command the name, rayend


        * assign material colors to the points


        * identify points that are on material interfaces


        * identify constrained points

        filter/1,0,0/

        setpts


        * connect the points into a Delaunay tetrahedral mesh


        * do not connect across material interfaces -


        * add points if necessary to resolve material interfaces

        search


        * remove middle region

        rmregion/middle

        rmpoint/compress


        * grid hex part

        cmo/create/hexgrid///hex


        * unit cube

        surface/cube/reflect/box/0.0,0.0,0.0/1.0,1.0,1.0/


        * define z=.4,.6 plane as interface

        surface/cutpl1/intrface/plane/0.,0.,.4/1.,0.,.4/1.,1.,.4/

        surface/cutpl2/intrface/plane/0.,0.,.6/1.,0.,.6/1.,1.,.6/


        *define geometric regions

        region/top/ le cube and ge cutpl2 /

        region/bottom/ le cube and le cutpl1 /

        region/middle/ le cube and gt cutpl1 and lt cutpl2 /


        * define material regions

        mregion/top/ le cube and gt cutpl2 /

        mregion/bottom/ le cube and lt cutpl1 /

        mregion/middle/ le cube and gt cutpl1 and lt cutpl2 /

        rz/xyz/5,5,3/0.,0.,.4/1.,1.,.6/1,1,1/

        rzbrick/xyz/5,5,3/1,0,0/connect/


        * add meshes

        addmesh/glue/hybrid/3dmesh/hexgrid/


        * unit cube

        surface/cube/reflect/box/0.0,0.0,0.0/1.0,1.0,1.0/


        * define z=.4,.6 plane as interface

        surface/cutpl1/intrface/plane/0.,0.,.4/1.,0.,.4/1.,1.,.4/

        surface/cutpl2/intrface/plane/0.,0.,.6/1.,0.,.6/1.,1.,.6/


        *define geometric regions

        region/top/ le cube and ge cutpl2 /

        region/bottom/ le cube and le cutpl1 /

        region/middle/ le cube and gt cutpl1 and lt cutpl2 /


        * define material regions

        mregion/top/ le cube and gt cutpl2 /

        mregion/bottom/ le cube and lt cutpl1 /

        mregion/middle/ le cube and gt cutpl1 and lt cutpl2 /


        * set element (tetrahedral) type

        filter/1,0,0/

        geniee

        zq/imt/1,0,0/0/

        zq/itp/1,0,0/0/

        setpts

        settets/geometry


        * dump mesh to some output form

        dump/gmv/gmv.hybrid/hybrid


        * terminate processing

        finish







[Return to LaGriT Home Page](index.md)





