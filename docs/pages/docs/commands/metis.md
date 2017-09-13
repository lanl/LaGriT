---
GENERATOR: 'Mozilla/4.7C-SGI 
[en
] (X11; I; IRIX64 6.5 IP30) 
[Netscape
]'
---

 METIS

 

 Interface METIS graph partition and reorder package with LaGriT. For
 details of METIS algorithms and descriptions of the third command line
 argument see:

 <http://glaros.dtc.umn.edu/gkhome/views/metis

 Partition algorithm divides mesh into npartition parts attempting to
 assign an equal number of graph vertices (nodes or elements) to each
 partition and also the number of adjacent elements assigned to
 different processors is minimized. New attributes are created to hold
 partition information. Node partition number is in inodeprt, element
 partition number is in ielemprt.

 Reorder algorithm computes a permutation vector, iperm, and an inverse
 permutation vector, invperm. The mesh can be reordered using the
 permutation vector in reorder. The reorder algorithms will compute
 fill-reducing orderings of sparse matrices.

  

METIS Interface to LaGriT

 METIS can be freely distributed provided that:

 -   A reference to the following paper is included: *“A Fast and
     Highly Quality Multilevel Scheme for Partitioning Irregular
     Graphs”. George Karypis and Vipin Kumar. SIAM Journal on
     Scientific Computing, Vol. 20, No. 1, pp. 359—392, 1999.*
 -   The original documentation ([PDF file of the
     manual](http://glaros.dtc.umn.edu/gkhome/fetch/sw/metis/manual.pdf" download> </a>)
     and copyright notice is include
 -   METIS 4.0 Copyright 2001-06, Regents of the University of
     Minnesota


FORMAT (partition):

 

 metis / partition / metis\_partmeshnodal  metis\_partmeshdual/ node 
 dual / npartitions / inodeprt / ielemprt

FORMAT (reorder):

 metis / reorder / metis\_edgend  metis\_nodend / node  dual /
 
[iperm
] / 
[invperm
]

FORMAT (reorder with weights):

 metis / reorder / mmetis\_nodewnd / node  dual / 
[iperm
] /
 
[invperm
] / ivert\_weight

EXAMPLES (partition):

 metis / partition / metis\_partmeshnodal / node / 32 / inodeprt /
 ielemprt

 metis / partition / metis\_partmeshdual / dual / 32 / inodeprt /
 ielemprt

 metis / partition / metis\_partmeshnodal / node / 32 / -def- / -def-

 metis / partition / metis\_partmeshnodal / node / 32

EXAMPLES (reorder):

 metis / reorder / metis\_edgend / dual

 metis / reorder / metis\_nodend / node

 metis / reorder / metis\_nodend / dual

 metis / reorder / metis\_nodend / dual / -def- / -def-

 metis / reorder / metis\_nodend / dual / ieprm1 / ieinvprm1

 metis / reorder / metis\_edgend / node

 reorder/-def-/ieprm1

EXAMPLE LaGriT Control File:

     *
     * Build some meshes, tri,quad,hex,tet
     *
     define / NP / 21
     cmo / create / mo_hex / / / hex
     createpts / brick / xyz / NP NP NP / 0. 0. 0. / 1. 1. 1. / 1 1 1
     cmo / setatt / mo_hex / itetclr / 1 0 0 / 1
     resetpts / itp

     define / NP / 101
     cmo/create/mo_qua/ / / quad
     quadxy/ NP NP / 0. 0. 0. / 1. 0. 0. / 1. 1. 0. / 0. 1. 0.
     createpts/brick/xyz/NP NP 1/1 0 0 / connect
     cmo / setatt / mo_qua / itetclr / 1 0 0 / 1
     resetpts / itp

     hextotet / 4 / mo_tri / mo_qua
     resetpts / itp

     hextotet / 24 / mo_tet / mo_hex
     resetpts / itp

     cmo / status / brief
     *
     * Run Metis partition into 16, 33 and 128
     *
     cmo / select / mo_qua
     metis/partition/metis_partmeshnodal/node/ 16 idn016 ide016                         
     metis/partition/metis_partmeshnodal/node/ 33 idn033 ide033                         
     metis/partition/metis_partmeshnodal/node/128 idn128 ide128    
     metis/partition/metis_partmeshdual/dual/ 16  iddn016 idde016
     metis/partition/metis_partmeshdual/dual/ 33  iddn033 idde033
     metis/partition/metis_partmeshdual/dual/128  iddn128 idde128
     metis / reorder / metis_nodend / node / iperm1 / invperm1

     cmo / select / mo_tri
     metis/partition/metis_partmeshnodal/node/ 16 idn016 ide016                         
     metis/partition/metis_partmeshnodal/node/ 33 idn033 ide033                         
     metis/partition/metis_partmeshnodal/node/ 128 idn128 ide128    
     metis/partition/metis_partmeshdual/dual/ 16  iddn016 idde016
     metis/partition/metis_partmeshdual/dual/ 33  iddn033 idde033
     metis/partition/metis_partmeshdual/dual/128  iddn128 idde128
     metis / reorder / metis_nodend / node / iperm1 / invperm1

     cmo / select / mo_hex
     metis/partition/metis_partmeshnodal/node/ 16 idn016 ide016                         
     metis/partition/metis_partmeshnodal/node/ 33 idn033 ide033                         
     metis/partition/metis_partmeshnodal/node/ 128 idn128 ide128    
     metis/partition/metis_partmeshdual/dual/ 16  iddn016 idde016
     metis/partition/metis_partmeshdual/dual/ 33  iddn033 idde033
     metis/partition/metis_partmeshdual/dual/128  iddn128 idde128
     metis / reorder / metis_nodend / node / iperm1 / invperm1

     cmo / select / mo_tet
     metis/partition/metis_partmeshnodal/node/ 16 idn016 ide016                         
     metis/partition/metis_partmeshnodal/node/ 33 idn033 ide033                         
     metis/partition/metis_partmeshnodal/node/ 128 idn128 ide128    
     metis/partition/metis_partmeshdual/dual/ 16  iddn016 idde016
     metis/partition/metis_partmeshdual/dual/ 33  iddn033 idde033
     metis/partition/metis_partmeshdual/dual/128  iddn128 idde128
     metis / reorder / metis_nodend / node / iperm1 / invperm1

     dump /  gmv / metis_qua.gmv / mo_qua
     dump /  gmv / metis_tri.gmv / mo_tri
     dump /  gmv / metis_hex.gmv / mo_hex
     dump /  gmv / metis_tet.gmv / mo_tet
     *
     * Create an attribute which is the node number
     *
     cmo / set_id / mo_qua / node / idnode0
     cmo / set_id / mo_tri / node / idnode0
     cmo / set_id / mo_hex / node / idnode0
     cmo / set_id / mo_tet / node / idnode0
     *
     * Reorder each mesh using the Metis permutation vector
     *
     reorder / mo_qua / iperm1
     reorder / mo_tri / iperm1
     reorder / mo_hex / iperm1
     reorder / mo_tet / iperm1
     *
     * Create an attribute which is the node number after reorder
     *
     cmo / set_id / mo_qua / node / idnode1
     cmo / set_id / mo_tri / node / idnode1
     cmo / set_id / mo_hex / node / idnode1
     cmo / set_id / mo_tet / node / idnode1

     dump /  gmv / metis_qua_reorder.gmv / mo_qua
     dump /  gmv / metis_tri_reorder.gmv / mo_tri
     dump /  gmv / metis_hex_reorder.gmv / mo_hex
     dump /  gmv / metis_tet_reorder.gmv / mo_tet

     finish                                                                          

 

