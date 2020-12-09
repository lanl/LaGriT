---
GENERATOR: 'Mozilla/4.05C-SGI [en] (X11; I; IRIX64 6.5 IP28) [Netscape]'
---
<img height="300" width="300" src="https://lanl.github.io/LaGriT/assets/images/arrow2.gif">"30"
"30"](mstkla.md#MODEL%20REGION:) <img height="300" width="300" src="https://lanl.github.io/LaGriT/assets/images/arrow3.gif">"30"
"30"](GeomEntity.md) <img height="300" width="300" src="https://lanl.github.io/LaGriT/assets/images/arrow4.gif">"30"
"30"](GeomFace.md)

<img height="300" width="300" src="https://lanl.github.io/LaGriT/assets/images/construction14.gif">"169" "131"

------------------------------------------------------------------------

------------------------------------------------------------------------

   **MODEL REGION OPERATORS:** 

------------------------------------------------------------------------

*void* **GR\_Faces**(*PGeomRegn* gr, *int* 
*nf, *PGeomFace* 
*faces);

Get the model faces that form the boundary of a model region.

------------------------------------------------------------------------

*int* **GR\_FaceDir**(*PGeomRegn* gr, *PGeomFace* gf);

Get the sense in which a model region is using a face, i.e., is the

face normal pointing into the region (return value 0), out of the

region (return value 1), both (return value 2) or none (return value

-1). 
*
*
*
*
* MUST VERIFY THiS WITH CODE VALUES 
*
*
*
*
*
*

------------------------------------------------------------------------

*int* **GR\_OnBoundary**(*PGeomRegn* this, *PGeomEntity* gent);

 

 

 
<img height="300" width="300" src="https://lanl.github.io/LaGriT/assets/images/arrow2.gif">"30"
"30"](mstkla.md#MODEL%20REGION:) <img height="300" width="300" src="https://lanl.github.io/LaGriT/assets/images/arrow3.gif">"30"
"30"](GeomEntity.md) <img height="300" width="300" src="https://lanl.github.io/LaGriT/assets/images/arrow4.gif">"30"
"30"](GeomFace.md)
