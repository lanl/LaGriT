# cmo / setatt

Operation to set mesh object attribute values.

## SYNTAX

**cmo / setatt** /mo_name / attribute_name /ifirst,ilast,istride/ value

- mo_name is the name of the mesh oject to set values

- attribute_name is the name in the mesh object to operate on

- ifirst,ilast,istride is the selection, default 1,0,0 will operate on all members of the attribute. The node or element set syntax can be used instead of the integer range. See **`pset`** and **`eltset`** commands.

- value is type integer or real depending on the type of the attribute.
Sets the value of the specified attribute in the given range to the
supplied value.
Note: This command requires that the mesh contains one or more nodes.


## EXAMPLES

```
cmo/setatt/mo/itp1/1,0,0/0
```
Will set all values of node attribute itp1 to 0

```
cmo/setatt/ 3dmesh /itetclr /eltset,get,blue/ 3
```
Will set all elements of attribute itetclr and in the element set 'blue' to have the value of 3

```
cmo/setatt // ndimensions_geom/ 2
```
Will reset the ndimensions geometry attribute of the active mesh object to 2
