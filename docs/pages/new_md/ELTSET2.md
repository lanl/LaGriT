---
GENERATOR: 'Mozilla/4.7 [en] (X11; I; IRIX 6.5 IP32) [Netscape]'
Generator: Microsoft Word 98
title: ELTSET
---

 

 **ELTSET**

  This command creates eltsets or element sets with membership
  criteria:
 
   1
. equal to, greater than or less than value of an element attribute

   2. inclusive pset membership - all elements any of whose nodes is in
   pset

   3. exclusive pset membership - all elements all of whose nodes are in
   pset

   4. union, intersection, not, delete other eltsets

   5. region or mregion membership

   6. quality criteria (volume or aspect ratio)

  The **mregion** and **region** form of this command calculate the
  center of mass of the element and determine which mregion or region
  the center lies in. It is possible if the interface surfaces are
  curved that the center will not lie in the same mregion or region as
  the vertices. Using itetclr will give the better result.

 **FORMAT:**

  **eltset**/eset\_name/element\_attribute\_name**/eq****ne****lt****gt****le****ge**/value/

  **eltset**/eset\_nam**e/unioninternotdelete**/eset\_list/

  **eltset**/eset\_name**/inclusiveexclusive/pset/get**/pset\_name/

  **eltset**/eset\_name**/regionmregion**/region\_name****mregion\_name****/

  **eltset**/eset\_name **/volume/ eqneltgtlege** /value

  **eltset**/eset\_name /

  **aspect**/ **eqneltgtlege** /value

 **EXAMPLES:**

  **eltset**/element\_set1**/itetclr****/eq**/4

  **eltset**/element\_set2**/inclusive****/pset****/get**/mypset

  **eltset**/element\_set3**/region**/upper

  **eltset**/element\_set4**/volume****/lt**/3.0
