---
Author: Jan Wills
GENERATOR: 'Mozilla/4.7 [en] (X11; I; IRIX 6.5 IP32) [Netscape]'
---

 
**setatt**/mo\_name/attribute\_name/ifirst,ilast,istride/value
mo\_name is type character, required.

attribute\_name is type character.  If blank, the active mesh object
will be used.

ifirst,ilast,istride
for attributes with length = 'nnodes', pset can be supplied

for attributes with length = 'nelements', eltset can be supplied

for other attributes ifirst,ilast,istride must be numbers; if blank then
all members of the attribute will be modified.
value is type integer or real depending on the type of the attribute.
Sets the value of the specified attribute in the given range to the
supplied value.
Note:  This command requires that the mesh contains one or more nodes.
**EXAMPLES:**
mo/setatt/mo/itp1/1,0,0/0

mo/setatt**/ 3dmesh**/itetclr** **/eltset**,**get**,blue/3

Will set all elements in the element set 'blue' to have the value of
itetclr to 3.

mo/setatt**/**/ndimensions\_geom**////2

Will reset the ndimensions geometry attribute of the active mesh object
to 2.
