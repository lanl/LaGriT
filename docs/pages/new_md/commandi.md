---
GENERATOR: 'Mozilla/4.05C-SGI [en] (X11; I; IRIX 6.5 IP32) [Netscape]'
Generator: Microsoft Word 98
title: b
---

 

 **b. Command Interface**

The default Mesh Object is named *3dmesh* For simple problems the user
must supply only
a[**cmo** **/create**/](cmo_create.md)mesh\_object\_name command. There
is no limit on the number of Mesh Objects that can be defined, but at
any time there is only one 'current' or 'active' Mesh Object. For more
advanced problems, such as those requiring more than one Mesh Object or
requiring extensions to the basic Mesh Object template, the Mesh
Object(s) is(are) manipulated via the [**cmo**](CMO2.md)commands which
are described in the next section. For example, additional user defined
attributes may be added to a Mesh Object by using
the[**cmo** **/addatt**](cmo_addatt.md) command, or the 'active' Mesh
Object can be changed using the[**cmo/select**](cmo_select.md)command.
