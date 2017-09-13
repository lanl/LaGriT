---
GENERATOR: 'Mozilla/4.7C-SGI 
[en
] (X11; I; IRIX64 6.5 IP30) 
[Netscape
]'
---

 METIS

 

 Interface METIS graph partition and reorder package with LAGriT. For
 details of METIS algorithms and descriptions of the third command line
 argument see:

 [http://www-users.cs.umn.edu/
~karypis/metis](http://www-users.cs.umn.edu/~karypic/metis)[](http://www-users.cs.umn.edu/~karypic/metis)

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

  

METIS Interface to LAGriT

  

 The standard libraries, liblagrit.a and libutil.a do not contain
 METIS. In order to utilize the METIS functions one must download the
 METIS package, build the METIS libraries on your local system and link
 them with the LAGriT libraries. One must also remove the dummy metis
 routines from the liblagrit.a library.

 For example, on an SGI the following steps are required. Different
 compile and link flags are required for different operating systems.

 Remove the dummy metis routines from the library: ar -dlv
 liblagrit\_sgi.a metis\_lg.o

 Compile the main program:

 f77 -g -c -n32 -r10000 -avoid\_gp\_overflow

 
$LAGRIT\_SRC\_PATH/adrivgen.f

 Link the LaGriT and METIS libraries to build an executable:

 f90 -g -n32 -r10000 -avoid\_gp\_overflow -o xlagrit\_g\_metis 



 adrivgen.o liblagrit\_sgi.a libmetis.a libutil\_sgi.a

  

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
