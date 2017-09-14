
*
*
*
*
*
*
*
*
*
*
*
*
*
*
*
*
*
*
*
*
*
*
*
*
*
*
*
*
*
*
*
*
*
*
*
*
*
*
*
*
*
*
*
*
*


* Read avs files


*
*
*
*
*
*
*
*
*
*
*
*
*
*
*
*
*
*
*
*
*
*
*
*
*
*
*
*
*
*
*
*
*
*
*
*
*
*
*
*
*
*
*
*
*

cmo/create/exterior/60//3/2/3/3/3/

read/avs/exterior.avs/exterior/1/1/0/


*

cmo/create/sigas/500/500/3/2/3/3/3/

read/avs/cont.0.avs/sigas/1/1/0/


*

cmo/create/oxgas/1000/2000/3/2/3/3/3/

read/avs/cont.12.avs/oxgas/1/1/0/


*

cmo/create/vp/1000/2000/3/2/3/3/3/

read/avs/cont.3.avs/vp/1/1/0/


*


*
*
*
*
*
*
*
*
*
*
*
*
*
*
*
*
*
*
*
*
*
*
*
*
*
*
*
*
*
*
*
*
*
*
*
*
*
*
*
*
*
*
*
*
*


* Define surfaces regions materials.


*
*
*
*
*
*
*
*
*
*
*
*
*
*
*
*
*
*
*
*
*
*
*
*
*
*
*
*
*
*
*
*
*
*
*
*
*
*
*
*
*
*
*
*
*

cmo/create/mesh/3000/3000/

surface/bb/reflect/sheet/exterior/

surface/sg/intrface/sheet/sigas/

surface/og/intrface/ sheet/ oxgas/

region/sub/le bb and lt sg /

region/ox /le bb and ge sg and le og /

region/gas/le bb and gt og /

mregion/matsub/ le bb and lt sg /

mregion/matox / le bb and gt sg and lt og /

mregion/matgas/ le bb and gt og /

cmo/verify/-all-


*


*
*
*
*
*
*
*
*
*
*
*
*
*
*
*
*
*
*
*
*
*
*
*
*
*
*
*
*
*
*
*
*
*
*
*
*
*
*
*
*
*
*
*
*
*


* Copy points from the sheets.


*
*
*
*
*
*
*
*
*
*
*
*
*
*
*
*
*
*
*
*
*
*
*
*
*
*
*
*
*
*
*
*
*
*
*
*
*
*
*
*
*
*
*
*
*

copypts/mesh/exterior/

copypts/mesh/sigas/

zq/itp/0, 0, 0/0

copypts/mesh/oxgas/

zq/itp/0, 0, 0/0

copypts/mesh/vp/

zq/itp/0, 0, 0/0

rz/xyz/21 29 33/1 0 0/2.25 1.75 4/1 1 1


*rz/xyz/11 15/1 1 0/5. 2.25 1.75 5./1 1 1


*pset/rays/seq/0 0 0/


*regnpts/ox/1/pset/get/rays/xyz/ 0. 0. -5./ 1. 0. -5./ 1. 1. -5./


*zq/itp/pset/get/rays/dud/


*


*
*
*
*
*
*
*
*
*
*
*
*
*
*
*
*
*
*
*
*
*
*
*
*
*
*
*
*
*
*
*
*
*
*
*
*
*
*
*
*
*
*
*
*
*


* Generate the mesh.


*
*
*
*
*
*
*
*
*
*
*
*
*
*
*
*
*
*
*
*
*
*
*
*
*
*
*
*
*
*
*
*
*
*
*
*
*
*
*
*
*
*
*
*
*

filter/1 0 0 /

setpts

dump/x3d/x3d1

search

settets/geometry

dump/gmv/TL.gmv/mesh

finish

 

 

 


[Return to LaGriT Home Page](index.md)

 



