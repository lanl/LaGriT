---
GENERATOR: 'Mozilla/4.7 
[en
] (X11; I; IRIX 6.5 IP32) 
[Netscape
]'
---

 
*input.hex

 
* create a hexaheral grid

 cmo create abc///hex

 quadxyz/5 25 7/ 0. 0. 0. /1. 0. 0. /1. 2. 0. /0. 2. 0. /&

 0. 0. 10. /1. 0. 10. /1. 2. 10. /0. 2. 10. /

 
*cmo/printatt//-xyz-

 setpts

 creatpts/brick/xyz/5 25 7/1,0,0/connect/

 settets

 dump/gmv/gmv.myhex

 dump/avs/avs.myhex/abc/

 finish


