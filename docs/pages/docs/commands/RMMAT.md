 **RMMAT**

  This routine is used to remove points that are of a specified
  material type.  Elements with the specified material types are
  flagged by setting the element material type negative.  After using
  **rmmat**, **[rmpoints/](RMPOINT.md)ompress will delete
  elements whose material type is negative and the dudded nodes.

   **FORMAT**

  **rmmat**/material
  number/
[**allnodeelement**
]/
[**exclusive**
]

  default is: **rmmat**/material number or **rmmat**/material
  number**/all**

  removes nodes with imt = material number and removes elements with
  itetclr= material number
 
  other options are:
 
  **rmmat**/material number**/node**

  removes nodes  with imt = material number
 
  **rmmat**/material number**/element**

  removes elements with itetclr= material number
 
  **rmmat**/material number/**/exclusive or rmmat**/material
  number**/all/exclusive**

  removes everything except nodes with imt =material and removes
  everything except elements with itetclr= material


 [Click here for demos](../demos/main_rmmat.md)

 
