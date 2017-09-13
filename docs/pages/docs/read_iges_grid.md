 **read/iges\_grid**

  reads and processes an IGES file containing surfaces, curves and
  points in IGES nurbs format.

 FORMAT:

  **read****/iges\_grid**/file\_name/
[iopt\_nurbs
]/
[iopt\_nurbl
]/
[iopt\_nurp
]/
[ksi
]/
[ksj
]/
[kli
]

    
    Argument    | Default | Description
    ------------ ----------- -----------------------------------
    iopt-nurbs |  default=1 |  read quad nurb surface if =1
    iopt-nurbl |  default=0 | read triangle nurb surface if = 2
    iopt-nurbp |  default=0  |  read triangle nurb curves if = 1
    ksi, ksj |  |              dimensions of surface nurb
    kli   |  |                 dimension of line nurb
 
