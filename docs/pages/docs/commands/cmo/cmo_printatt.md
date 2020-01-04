---
Title: "cmo/printatt"
Tags: cmo printatt
---


# cmo/printatt

---------------

## SYNTAX

<pre>
<b>cmo/printatt</b>/ mo_name/ attribute_name

<b>cmo/printatt</b>/ mo_name/ attribute_name / [ print_option ] [ index_set ]

<b>cmo/printatt</b>/ mo_name/ [<b>-all- -xyz- node element</b> ] / [ <b>minmax list value</b> ] / [ index_set ]
</pre>

 
`attribute_name` -  name of valid cmo attribute or category of attributes. The category selections are:
- **-all-** will printall attributes in the mesh object
- **-xyz-** will print attributes xic,yic,zic
- **node** will print all attributes of length nnodes
- **element** will print all attributes of length nelements

`print_option` is the print display option.
- **value**  this is default, prints attribute values
- **list**   will print attribute name along with its length
- **minmax** will print name and the min and max of the attribute field and length

`index_set` is the range istart,istride,ilast using pset, eltset, or index numbers where 1,0,0 are all.
 
   

## EXAMPLES

 
```
cmo/printatt/mo1/zic/3,8,0

cmo/printatt/mo1/itetclr/eltset,get,eset1
```
print values for attribute zic for nodes between index 3 and 8

print values for attribute itetclr for elements in the element set eset1


 
```
cmo printatt/mo1/-all-/list

cmo printatt/mo1/node/list
```
print attribute names for all attributes in the mesh object mo1

print attribute names for all attributes of length nnodes

 
```
cmo/printatt/mo1/-all-/minmax

cmo/printatt/mo1/-xyz-/minmax

cmo/printatt/mo1/xic/ minmax/7,10,0

cmo/printatt/mo1/itp1/minmax/pset,get,pset1

cmo/printatt/mo1/itetclr/minmax/eltset,get,e_small
```
print min and max of attributes

  

