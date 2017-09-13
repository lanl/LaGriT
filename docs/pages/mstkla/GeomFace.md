---
GENERATOR: 'Mozilla/4.05C-SGI 
[en
] (X11; I; IRIX64 6.5 IP28) 
[Netscape
]'
---

[<img height="300" width="300" src="../images/arrow2.gif">"30"
"30"](mstkla.md#MODEL%20FACE:) [<img height="300" width="300" src="../images/arrow3.gif">"30"
"30"](GeomRegion.md) [<img height="300" width="300" src="../images/arrow4.gif">"30"
"30"](GeomEdge.md)

<img height="300" width="300" src="../images/construction14.gif">"169" "131"

------------------------------------------------------------------------

------------------------------------------------------------------------

   **MODEL FACE OPERATORS:**

------------------------------------------------------------------------

 *void* **GF\_Regions**(*PGeomFace* gf, *PGeomRegn* gr
[2
]);

Get the regions on either side of the face. gr
[0
] is the region on
the

opposite side of the normal while gr
[1
] is the region on the same
side

of the normal.  In principle (not in LaGriT), both entries of gr may

be filled \_AND\_ gr
[0
] may be equal to gr
[1
]. Also in principle,
both

entries of gr may be NULL.

 

 

[<img height="300" width="300" src="../images/arrow2.gif">"30"
"30"](mstkla.md#MODEL%20REGION:) [<img height="300" width="300" src="../images/arrow3.gif">"30"
"30"](GeomRegion.md) [<img height="300" width="300" src="../images/arrow4.gif">"30"
"30"](GeomEdge.md)
