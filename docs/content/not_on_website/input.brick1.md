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

quadxyz 5 7 5 0. 0. 0. 1. 0. 0. 1.5 .5 2. .5 .2 2.5 -1. 1.5 0. 2. 2. 0.
&

         2.1 1.9 2.4 -.2 1.8 2.3 /

setpts

creatpts/brick/xyz/5 7 5/1,0,0/connect/

settets

dump/gmv/gmv.hex

finish

