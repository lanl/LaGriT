---
GENERATOR: 'Mozilla/4.05C-SGI [en] (X11; I; IRIX64 6.5 IP28) [Netscape]'
---
[<img height="300" width="300" src="../images/arrow2.gif">"30"
"30"](mstkla.md#GMTKLA_Init) [<img height="300" width="300" src="../images/arrow3.gif">"30"
"30"](GenRegion.md) [<img height="300" width="300" src="../images/arrow4.gif">"30"
"30"](GeomEntity.md)

------------------------------------------------------------------------

------------------------------------------------------------------------

 **GEOMETRIC MODEL INITIALIZATION:**

 

------------------------------------------------------------------------



*void* **GMTKLA\_Init**(*void*);

Initialize the geometric model interface (does nothing for now but

include it for completeness sake).

------------------------------------------------------------------------

------------------------------------------------------------------------

   **MODEL OPERATORS:**

 

------------------------------------------------------------------------

 *int* **GM\_NumRegions**(*void*);

Number of model regions. Note this has nothing to do with number of

material attributes. That is the analysis preprocessing concern not

the mesh generator's or the geometric modeler's. The number of model

regions refers to the number of distinct volumes in the geometric

model.

------------------------------------------------------------------------

*int* **GM\_NumFaces**(*void*);

Number of model faces. For now, this just returns the number of

surfaces in the geometric model although they are strictly not the

same.

------------------------------------------------------------------------

*PGeomRegn* **GM\_Region**(*int* i);

Return the i'th geometric model region

------------------------------------------------------------------------

*PGeomFace* **GM\_Face**(*int* i);

Return the i'th geometric model face

------------------------------------------------------------------------

*void* **GM\_Regions**(*int* 
*nr, *PGeomRegn* 
*
*gregs);

Get all the model regions

------------------------------------------------------------------------

*void* **GM\_Faces**(*int* 
*nf, *PGeomFace* 
*
*gfaces);

Get all the model faces
[<img height="300" width="300" src="../images/arrow2.gif">"30"
"30"](mstkla.md#GMTKLA_Init) [<img height="300" width="300" src="../images/arrow3.gif">"30"
"30"](GenRegion.md) [<img height="300" width="300" src="../images/arrow4.gif">"30"
"30"](GeomEntity.md)
