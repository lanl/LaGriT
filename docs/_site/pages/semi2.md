cmo/create/3dmesh

surface/brt/reflect /plane/1.0,0.0,0.0/1.0,1.0,0.0/1.0,0.0,1.0/

surface/blt/reflect /plane/0.0,0.0,0.0/0.0,0.0,1.0/0.0,1.0,0.0/

surface/btp/reflect /plane/0.0,0.0,2.0/1.0,0.0,2.0/0.0,1.0,2.0/

surface/bbt/reflect /plane/0.0,0.0,0.0/0.0,1.0,0.0/1.0,0.0,0.0/

surface/bfr/reflect /plane/0.0,-0.2,0.0/1.0,-0.2,0.0/0.0,-0.2,1.0/

surface/bbk/intrface/plane/0.0, 1.0,0.0/0.0, 1.0,1.0/1.0, 1.0,0.0/

surface/b1 /intrface/plane/0.4,0.0,0.0/0.4,1.0,0.0/0.4,0.0,1.0/

surface/b2 /intrface/plane/0.6,0.0,0.0/0.6,1.0,0.0/0.6,0.0,1.0/

surface/b3 /intrface/plane/0.7,0.0,0.0/0.7,1.0,0.0/0.7,0.0,1.0/

surface/pl1/intrface/plane/0.0,0.0,2.0/0.4,0.0,2.0/0.4,0.0,0.0/

surface/pl2/intrface/plane/0.4,0.0,2.0/0.6,0.05,2.0/0.6,0.05,0.0/

surface/pl3/intrface/plane/0.6,0.05,2.0/0.7,0.2,2.0/0.7,0.2,0.0/

surface/pl4/intrface/plane/0.7,0.2,2.0/1.0,0.2,2.0/1.0,0.2,0.0/

surface/pl5/intrface/plane/0.0,-0.01,2.0/0.4,-0.01,2.0/0.4,-0.01,0.0/

surface/pl6/intrface/plane/0.4,-0.01,2.0/0.6,0.04,2.0/0.6,0.04,0.0/

surface/pl7/intrface/plane/0.6,0.04,2.0/0.7,-0.1,2.0/0.7,-0.1,0.0/

surface/pl8/intrface/plane/0.7,-0.1,2.0/1.0,-0.1,2.0/1.0,-0.1,0.0/

surface/pl9/intrface/plane/0.0,0.0,0.5/1.0,0.0,0.5/0.0,1.0,0.5/

surface/pla/intrface/plane/0.0,1.0,1.5/1.0,0.0,1.5/0.0,0.0,1.5/

surface/plb/intrface/plane/0.0,0.0,0.1/1.0,0.0,0.1/0.0,1.0,0.1/

surface/plc/intrface/plane/0.0,1.0,0.4/1.0,0.0,0.4/0.0,0.0,0.4/

surface/pld/intrface/plane/0.0,0.0,1.6/1.0,0.0,1.6/0.0,1.0,1.6/

surface/ple/intrface/plane/0.0,1.0,1.9/1.0,0.0,1.9/0.0,0.0,1.9/

surface/plf/intrface/plane/0.0,-0.15,2.0/0.4,-0.15,2.0/0.4,-0.15,0.0/

surface/plg/reflect /plane/0.0,1.2,0.0/0.0,1.2,2.0/1.0,1.2,0.0/

mregion/reg1/ ( le blt and lt b1 and le btp and le bbt and lt bbk &

and gt pl1 ) or &

( ge b1 and lt b2 and le btp and le bbt and lt bbk &

and gt pl2 ) or &

( ge b2 and lt b3 and le btp and le bbt and lt bbk &

and gt pl3 ) or &

( ge b3 and le brt and le btp and le bbt and lt bbk &

and gt pl4 )

mregion/reg2/ ( le blt and lt b1 &

and lt pl1 and gt pl5 and gt pl9 and gt pla ) or &

( ge b1 and le b2 &

and lt pl2 and gt pl6 and gt pl9 and gt pla ) or &

( gt b2 and lt b3 and le btp and le bbt &

and lt pl3 and gt pl7 ) or &

( ge b3 and le brt and le btp and le bbt &

and lt pl4 and gt pl8 )

mregion/reg3/ ( le blt and le b1 and le bbt and le bfr &

and lt pl1 and lt plb ) or &

( le blt and le b1 and le bfr &

and lt plf and ge plb and ge plc ) or &

( le blt and le b1 and le bfr &

and lt pl1 and lt plc and lt pl9 ) or &

( le blt and le b1 and le bfr &

and lt plf and ge pl9 and ge pla ) or &

( le blt and le b1 and le bfr &

and lt pl1 and lt pla and lt pld ) or &

( le blt and le b1 and le bfr &

and lt plf and ge pld and ge ple ) or &

( le blt and le b1 and le btp and le bfr &

and lt pl1 and lt ple ) or &

( gt b1 and lt b2 and le bfr &

and lt plf and ge pl9 and ge pla ) or &

( gt b1 and lt b2 and le bfr and le btp and le bbt &

and lt pl2 and ( lt pl9 or lt pla ) ) or &

( ge b2 and le b3 and le bfr &

and lt plf and ge pl9 and ge pla ) or &

( ge b2 and le b3 and le bfr and le btp and le bbt &

and lt pl7 and ( lt pl9 or lt pla ) ) or &

( gt b3 and le brt and le btp and le bfr &

and lt pl8 and le bbt )

mregion/reg4/ ( le blt and lt b1 &

and lt pl1 and gt plf and gt plb and gt plc )

mregion/reg5/ ( le blt and lt b1 &

and lt pl1 and gt plf and gt pld and gt ple )

mregion/reg6/ ( le blt and lt b1 &

and lt pl5 and gt plf and gt pl9 and gt pla ) or &

( ge b1 and lt b2 &

and lt pl6 and gt plf and gt pl9 and gt pla ) or &

( ge b2 and lt b3 &

and lt pl7 and gt plf and gt pl9 and gt pla )

mregion/reg7/ le plg and le brt and le blt and le btp and le bbt &

and gt bbk

region/reg1a/( le blt and lt b1 and le btp and le bbt and lt bbk &

and gt pl1 )

region/reg1b/( ge b1 and lt b2 and le btp and le bbt and lt bbk &

and ge pl2 )

region/reg1c/( ge b2 and lt b3 and le btp and le bbt and lt bbk &

and ge pl3 )

region/reg1d/( ge b3 and le brt and le btp and le bbt and lt bbk &

and ge pl4 )

region/reg2a/( le blt and lt b1 &

and le pl1 and ge pl5 and ge pl9 and ge pla )

region/reg2b/( ge b1 and lt b2 &

and lt pl2 and ge pl6 and ge pl9 and ge pla )

region/reg2c/( ge b2 and lt b3 and le btp and le bbt &

and lt pl3 and ge pl7 )

region/reg2d/( ge b3 and le brt and le btp and le bbt &

and lt pl4 and ge pl8 )

region/reg3a/ ( le blt and le b1 and le bbt and le bfr &

and le pl1 and lt plb )

region/reg3b/( le blt and le b1 and le bfr &

and lt plf and ge plb and ge plc )

region/reg3c/( le blt and le b1 and le bfr &

and le pl1 and lt plc and lt pl9 )

region/reg3d/( le blt and le b1 and le bfr &

and lt plf and ge pl9 and ge pla )

region/reg3e/( le blt and le b1 and le bfr &

and le pl1 and lt pla and lt pld )

region/reg3f/( le blt and le b1 and le bfr &

and lt plf and ge pld and ge ple )

region/reg3g/( le blt and le b1 and le btp and le bfr &

and le pl1 and lt ple )

region/reg3h/( gt b1 and lt b2 and le bfr &

and lt plf and ge pl9 and ge pla )

region/reg3i/( gt b1 and lt b2 and le btp and le bbt and le bfr &

and lt pl2 and ( lt pl9 or lt pla ) )

region/reg3j/( ge b2 and le b3 and le bfr &

and lt plf and ge pl9 and ge pla )

region/reg3k/( ge b2 and le b3 and le btp and le bbt and le bfr &

and lt pl7 and ( lt pl9 or lt pla ) )

region/reg3l/( gt b3 and le brt and le btp and le bfr and le bbt &

and lt pl8 )

region/reg4/ ( le blt and le b1 &

and le pl1 and ge plf and ge plb and ge plc )

region/reg5/ ( le blt and le b1 &

and le pl1 and ge plf and ge pld and ge ple )

region/reg6a/( le blt and lt b1 &

and lt pl5 and ge plf and ge pl9 and ge pla )

region/reg6b/( ge b1 and lt b2 &

and lt pl6 and ge plf and ge pl9 and ge pla )

region/reg6c/( ge b2 and le b3 &

and lt pl7 and ge plf and ge pl9 and ge pla )

region/reg7/ ( le plg and le brt and le blt and le btp and le bbt &

and ge bbk )

rz/xyz/ 5,1, 4/0.0,3.0,0.0/0.4,3.0,0.1/1,0,1/

rz/xyz/9,1,7/0.0,3.0,0.1/0.4,3.0,0.4/1,0,1/

rz/xyz/ 5,1, 4/0.0,3.0,0.4/0.4,3.0,0.5/1,0,1/

rz/xyz/ 5,1,13/0.0,3.0,0.5/0.4,3.0,1.5/1,0,1/

rz/xyz/ 5,1, 4/0.0,3.0,1.5/0.4,3.0,1.6/1,0,1/

rz/xyz/9,1,7/0.0,3.0,1.6/0.4,3.0,1.9/1,0,1/

rz/xyz/ 5,1, 4/0.0,3.0,1.9/0.4,3.0,2.0/1,0,1/

rz/xyz/ 5,1, 7/0.4,3.0,0.0/0.6,3.0,0.5/1,0,1/

rz/xyz/ 9,1,13/0.4,3.0,0.5/0.6,3.0,1.5/1,0,1/

rz/xyz/ 5,1, 7/0.4,3.0,1.5/0.6,3.0,2.0/1,0,1/

rz/xyz/ 5,1, 7/0.6,3.0,0.0/0.7,3.0,0.5/1,0,1/

rz/xyz/ 5,1,13/0.6,3.0,0.5/0.7,3.0,1.5/1,0,1/

rz/xyz/ 5,1, 7/0.6,3.0,1.5/0.7,3.0,2.0/1,0,1/

rz/xyz/ 5,1,13/0.7,3.0,0.0/1.0,3.0,2.0/1,0,1/

pset/rays/seq/1,0,0/

regnpts/reg1a/
7/pset,get,rays/xyz/0.0,-1.0,2.0/1.0,-1.0,2.0/1.0,-1.0,0.0/1,2./

regnpts/reg1b/
7/pset,get,rays/xyz/0.0,-1.0,2.0/1.0,-1.0,2.0/1.0,-1.0,0.0/1,2./

regnpts/reg1c/
7/pset,get,rays/xyz/0.0,-1.0,2.0/1.0,-1.0,2.0/1.0,-1.0,0.0/1,2./

regnpts/reg1d/
7/pset,get,rays/xyz/0.0,-1.0,2.0/1.0,-1.0,2.0/1.0,-1.0,0.0/1,2./

regnpts/reg2a/
2/pset,get,rays/xyz/0.0,-1.0,2.0/1.0,-1.0,2.0/1.0,-1.0,0.0/

regnpts/reg2b/
2/pset,get,rays/xyz/0.0,-1.0,2.0/1.0,-1.0,2.0/1.0,-1.0,0.0/

regnpts/reg2c/
4/pset,get,rays/xyz/0.0,-1.0,2.0/1.0,-1.0,2.0/1.0,-1.0,0.0/

regnpts/reg2d/
4/pset,get,rays/xyz/0.0,-1.0,2.0/1.0,-1.0,2.0/1.0,-1.0,0.0/

regnpts/reg3a/
5/pset,get,rays/xyz/0.0,-1.0,2.0/1.0,-1.0,2.0/1.0,-1.0,0.0/

regnpts/reg3b/
3/pset,get,rays/xyz/0.0,-1.0,2.0/1.0,-1.0,2.0/1.0,-1.0,0.0/

regnpts/reg3c/
5/pset,get,rays/xyz/0.0,-1.0,2.0/1.0,-1.0,2.0/1.0,-1.0,0.0/

regnpts/reg3d/
3/pset,get,rays/xyz/0.0,-1.0,2.0/1.0,-1.0,2.0/1.0,-1.0,0.0/

regnpts/reg3e/
5/pset,get,rays/xyz/0.0,-1.0,2.0/1.0,-1.0,2.0/1.0,-1.0,0.0/

regnpts/reg3f/
3/pset,get,rays/xyz/0.0,-1.0,2.0/1.0,-1.0,2.0/1.0,-1.0,0.0/

regnpts/reg3g/
5/pset,get,rays/xyz/0.0,-1.0,2.0/1.0,-1.0,2.0/1.0,-1.0,0.0/

regnpts/reg3h/
5/pset,get,rays/xyz/0.0,-1.0,2.0/1.0,-1.0,2.0/1.0,-1.0,0.0/

regnpts/reg3i/
5/pset,get,rays/xyz/0.0,-1.0,2.0/1.0,-1.0,2.0/1.0,-1.0,0.0/

regnpts/reg3j/
5/pset,get,rays/xyz/0.0,-1.0,2.0/1.0,-1.0,2.0/1.0,-1.0,0.0/

regnpts/reg3k/
5/pset,get,rays/xyz/0.0,-1.0,2.0/1.0,-1.0,2.0/1.0,-1.0,0.0/

regnpts/reg3l/
5/pset,get,rays/xyz/0.0,-1.0,2.0/1.0,-1.0,2.0/1.0,-1.0,0.0/

regnpts/reg6a/
5/pset,get,rays/xyz/0.0,-1.0,2.0/1.0,-1.0,2.0/1.0,-1.0,0.0/

regnpts/reg6b/
5/pset,get,rays/xyz/0.0,-1.0,2.0/1.0,-1.0,2.0/1.0,-1.0,0.0/

regnpts/reg6c/
5/pset,get,rays/xyz/0.0,-1.0,2.0/1.0,-1.0,2.0/1.0,-1.0,0.0/

regnpts/reg7/
5/pset,get,rays/xyz/0.0,-1.0,2.0/1.0,-1.0,2.0/1.0,-1.0,0.0/

regnpts/reg4 /
5/pset,get,rays/xyz/0.0,-1.0,2.0/1.0,-1.0,2.0/1.0,-1.0,0.0/

regnpts/reg5 /
5/pset,get,rays/xyz/0.0,-1.0,2.0/1.0,-1.0,2.0/1.0,-1.0,0.0/

zq/itp/pset,get/rays/21

filter/1,0,0/

setpts

dump/x3d/x3d.mos3d.1

search

dump/x3d/x3d.mos3d.2

settets

dump/x3d/x3d.mos3d.3

dump/gmv/gmv.mos3d

finish

 

 

[Return to LaGriT Home Page](../index.md)

 

