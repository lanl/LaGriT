
 **ELTSET**

  This command creates eltsets or element sets with membership
  criteria:
 
  1.  **eq**:equal to, gt:greater than, **lt**:less than, **ge**:
      greater than or equal, **le**: less than or equal to, **ne**: no
      equal to value of an element attribute
  2.  **inclusive** pset membership - all elements any of whose nodes
      is in pset
  3.  **exclusive** pset membership - all elements all of whose nodes
      are in pset
  4.  **face** pset membership - elements if all nodes of a face are
      in pset
  5.  **union, inter, not**, boolian comparison with other eltsets
  6.  **region** or **mregion** membership
  7.  quality criteria (**volume** or **aspect** ratio)
  8.  **list** - list element set members to screen, or list the names
      of element sets if no 'eltset\_name' is specified.
  9.  **write** - write element set members to a file
  10. **delete** - remove element set from mesh object
 
  The **mregion** and **region** form of this command calculate the
  center of mass of the element and determine which mregion or region
  the center lies in. It is possible if the interface surfaces are
  curved that the center will not lie in the same mregion or region as
  the vertices. Using itetclr will give the better result.

 FORMAT:

  **eltset**/eset\_name/element\_attribute\_name**/eq**|**ne**|**lt**|**gt**|**le**|**ge**/value/

  **eltset**/eset\_nam**e/unioninternotdelete**/eset\_list/

  **eltset**/eset\_name**/inclusiveexclusiveface/pset/get**/pset\_name/

  **eltset**/eset\_name/regionmregion/region\_name|mregion\_name/

  **eltset**/eset\_name **/volume/ eqneltgtlege** /value

  **eltset**/eset\_name **/aspect/ eqneltgtlege** /value

  **eltset**/eset\_name **/list**

  **eltset**/eset\_name
  **/write**/file\_name
[.cellset
]/
[**ascii**|**binary**
]

  **eltset**/-all-
  **/write**/file\_name
[.cellset
]/
[**ascii**|**binary**
]

 EXAMPLES:

      eltset/element_set1/itetclr/eq/4 
      eltset/element_set2/face/pset/get/mypset
      eltset/element_set3/inclusive/pset/get/mypset
      eltset/element_set4/region/upper 
      eltset/element_set5/volume/lt/3.0 
      eltset/element_set5/delete
      eltset / /list (list the names of element sets which have been defined) 
