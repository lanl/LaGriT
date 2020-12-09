---
title: cmo modatt 
tags: cmo, modatt, attibute
---

# CMO/MODATT

Modifies a field for a mesh object attribute.
For more descriptions of the mesh object attributes, see [Mesh Object Definition](../../meshobject.md)

<pre>
<b>modatt</b>/ cmo_name/ cmo_att_name /cmo_field_name/ new_values
</pre>


`cmo_name` is the mesh object name. 

`cmo_att_name` is the mesh object attribute. 

`cmo_field_name` is type character, required

`new_values` is new field or fields as appropriate for that field. 



## Mesh Object Fields


**`name`**            (character) Attribute name


**`type`**            (character) Attribute type

- **INT** Integer number

- **REAL**  Real number

- **CHARACTER**  character variable of length 32

- **VINT**  Vector of integer

- **VDOUBLE**  Vector of real *8 (this is the default)

- **VCHAR**  Vector of character *32



**`rank`**            (character) Attribute rank (must be an attribute for this Mesh object), default is scalar


**`length`**          (character) Attribute length (must be an attribute for this Mesh object), default is nnodes


**`interpolation`**    (character) Interpolation option:

- **constant**        Constant value

- **sequence**        Set to the node number

- **copy**           Copy values

- **linear**       Linear interpolation, this is the default

- **user**           User provides a subroutine named user_interpolate

- **log**             Logarithmic interpolation

- **asinh**           Asinh interpolation

- **min**           Set to the minimum

- **max**              Set to the maximum

- **incmin**           Set to the minimum plus one (vint attribute only)

- **incmax**           Set to the maximum plus one (vint attribute only)

- **and**              'and' the bits

- **or**             'or' the bits



**`persistence`**      (character) Attribute persistence:

- **permanent**       Can not be deleted.

- **temporary**        Temporary attribute, this is the default



**`ioflag`**         (character) Attribute IO flag: default is **agl** (avs,gmv, and lagrit)

- **a**                Put this attribute on avs dumps

- **g**                Put this attribute on gmv dumps

- **f**                 Put this attribute on fehm dumps

- **l**                 Put this attribute on LaGriT dumps

- **L**                 Do not write this attribute to LaGriT dumps


**`default`**          (real) Attribute value




## EXAMPLES

```
define CMO cmo1
cmo / modatt / CMO / itp1 / ioflag / l
cmo / modatt / CMO / icr1 / ioflag / l
cmo / modatt / CMO / isn1 / ioflag / l
```
This is often used to reduce the amount of data written to an AVS format file. By default, the attributes **imt,itp,icr,isn** are written to the file.If not needed, modify the IO output so attributes will not be written unless it is an **l** type for lagrit file formats.
This example uses a define variable to set the mesh object variable CMO to "cmo1". Note the command parser recogizes these AVS attribute names with or without the letter "1" on the name.


```
cmo/select/ mo_tet

cmo/modatt/-cmo-/boron/length/ nnodes

cmo/modatt/-def-/boron/default/ 10.0

cmo/modatt/-def-/boron/interp/ user
```
These set of commands are used to modify the attribute "boron" in the mesh object named "mo_tet". 
The attribute length is changed from **nelements** to **nnodes**. The default value is set to 10. and the interpolation type is set to **user**.
