---
GENERATOR: 'Mozilla/4.05C-SGI [en] (X11; I; IRIX64 6.5 IP28) [Netscape]'
---

 [<img height="300" width="300" src="../images/arrow2.gif">"30"
"30"](http://www.ees.lanl.gov/staff/rao/mstkla/mstkla.md#UTILITIES:) [<img height="300" width="300" src="../images/arrow3.gif">"30"
"30"](http://www.ees.lanl.gov/staff/rao/mstkla/GeomVertex.md) [<img height="300" width="300" src="../images/arrow4.gif">"30"
"30"](http://www.ees.lanl.gov/staff/rao/mstkla/example.md)

<img height="300" width="300" src="../images/construction14.gif">"169" "131"

------------------------------------------------------------------------

------------------------------------------------------------------------

   **UTILITIES:**

------------------------------------------------------------------------

 

*void* **ReportError**(*char* 
*modulename, *char* 
*message, *ErrType*
severity);

Report an error message. The severity may be specified as

**MESSG**: Print a message to the screen

**WARNING**: Print message with additional attention grabbing text

**ERROR**: Print message with additional attention grabbing text

**FATAL**: Print attention grabbing message and exit.

 

 

------------------------------------------------------------------------

------------------------------------------------------------------------

 <img height="300" width="300" src="../images/new1.gif">"31" "12" **VECTOR FUNCTIONS:**

------------------------------------------------------------------------

*void* **vdiff**(*double* 
*a, *double* 
*b, *double* 
*a\_minus\_b);

------------------------------------------------------------------------

*void* **vsum**(*double* 
*a, *double* 
*b, *double* 
*a\_plus\_b);

------------------------------------------------------------------------

*double* **vmag2**(*double* 
*v);

------------------------------------------------------------------------

*double* **vmag**(*double* 
*v);

------------------------------------------------------------------------

*void* **vcross**(*double* 
*a, *double* 
*b, *double* 
*a\_cross\_b);

------------------------------------------------------------------------

*double* **vdot**(*double* 
*a, *double* 
*b);

------------------------------------------------------------------------

*void* **vnorm**(*double* 
*a, *double* 
*unita);

------------------------------------------------------------------------

------------------------------------------------------------------------

  ** <img height="300" width="300" src="../images/new1.gif">"31" "12"REAL NUMBER
COMPARISON FUNCTIONS:**

------------------------------------------------------------------------

 

*int* **REAL\_le**(*double* a, *double* b);

------------------------------------------------------------------------

*int* **REAL\_ge**(*double* a, *double* b);

------------------------------------------------------------------------

*int* **REAL\_eq**(*double* a, *double* b);

------------------------------------------------------------------------

 

------------------------------------------------------------------------

------------------------------------------------------------------------

 <img height="300" width="300" src="../images/new1.gif">"31" "12" **COMP GEOM
FUNCTIONS:**

------------------------------------------------------------------------

*int* **pntInTriangle2**(*double* 
*pnt, *double* (
*tri)[3], *double*
tol, *int* flag, *int* 
*mtype, *int* 
*locID);

 

Check if a point that is guaranteed to be in the plane of a triangle is
inside the triangle itself.

If **flag = 1**, check if point is strictly inside

    **flag = 0**, check if point is inside or on the boundary within a
real tolerance 'tol'

Return **value =  1 (inside), 0 (outside), -1 (on boundary)**

If point is on the boundary, **mtype** indicates type of entity it is
on

**locID** indicates the local ID number of the vertex or edge it is on

 

------------------------------------------------------------------------

*int* **pntInTet**(*double* 
*pnt, *double* (
*tet)[3], *double* tol,
*int* flag, *int* 
*mtype, *int* 
*locID);

Check if a point is inside a tet

If **flag = 1**, check if point is strictly inside

    **flag = 0**, check if point is inside or on the booundary

              within a real tolerance 'tol'

Return **value =  1 (inside), 0 (outside), -1 (on boundary)**

If point is on the boundary, **mtype** indicates type of entity it is
on

**locID** indicates the local ID number of the vertex or edge it is on

 

------------------------------------------------------------------------

*void* **Tri\_CircumCen**(*double* (
*xyz)[3], *double* 
*cen);

------------------------------------------------------------------------

*double* **Tri\_Area**(*double* (
*xyz)[3]);

------------------------------------------------------------------------

*void*   **Tri\_Normal**(*double* (
*xyz)[3], *double* 
*normal);

------------------------------------------------------------------------

*double* **Tet\_Volume**(*double* (
*xyz)[3]);

------------------------------------------------------------------------

*void*   **Tet\_CircumCen**(*double* (
*xyz)[3], *double* 
*cen);

------------------------------------------------------------------------

 

 

 

 

 [<img height="300" width="300" src="../images/arrow2.gif">"30"
"30"](http://www.ees.lanl.gov/staff/rao/mstkla/mstkla.md#UTILITIES:) [<img height="300" width="300" src="../images/arrow3.gif">"30"
"30"](http://www.ees.lanl.gov/staff/rao/mstkla/GeomVertex.md) [<img height="300" width="300" src="../images/arrow4.gif">"30"
"30"](http://www.ees.lanl.gov/staff/rao/mstkla/example.md)
