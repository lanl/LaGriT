**b. Command Interface**

The default Mesh Object is named *3dmesh*. For simple problems the user
must supply only a [**cmo****/create**/](commands/cmo/cmo_create.md) mesh\_object\_name
command. There is no limit on the number of Mesh Objects that can be
defined, but at any time there is only one 'current' or 'active' Mesh
Object. For more advanced problems, such as those requiring more than
one Mesh Object or requiring extensions to the basic Mesh Object
template, the Mesh Object(s) is(are) manipulated via the
[**cmo**](commands/CMO2.md) commands which are described in the next
section. For example, additional user defined attributes may be added to
a Mesh Object by using the[**cmo****/addatt**](commands/cmo/cmo_addatt.md) command, or the
'active' Mesh Object can be changed using the [**cmo/select**](commands/cmo/cmo_select.md) command.
