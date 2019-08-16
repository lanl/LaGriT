
# FORTRAN Interface

Mesh Objects and their attribute data are accessed through a set of subroutines. An
example of accessing an existing Mesh Object and creating a new mesh
object is given in [Fortran Mesh Object Access](accessing.md); that example should
be used as a template when operating with Mesh Objects. The subroutine
set includes:


[cmo_get_name](meshob.md#cmo_get_name): retrieve active mesh object
name

[cmo_set_name](meshob.md#cmo_set_name): set active mesh object name

[cmo_get_info](meshob.md#cmo_get_info):retrieve mesh object pointer

[cmo_get_intinfo](meshob.md#cmo_get_intinfo): retrieve mesh object
integer data

[cmo_get_attinfo](meshob.md#cmo_get_attinfo): retrieve mesh real or
character data

[cmo_set_info](meshob.md#cmo_set_info): set mesh object integer data

[cmo_set_attinfo](meshob.md#cmo_set_attinfo): set mesh real or
character data

[cmo_newlen](meshob.md#cmo_newlen): adjust the lengths of mesh object
data based on the values of number of nodes and number of elements which
are stored as integer mesh object attributes (nnodes, nelements)

Only data from the active Mesh Object may be retrieved; calling
**cmo_set_name** will make the referenced Mesh Object active. 

Scalar quantities are retrieved and stored using **cmo_get_intinfo**,
**cmo_get_attinfo**,  and **cmo_set_info**. 

Vector quantities are referred to by their pointers. The length of the vectors is calculated
internal to LaGriT based on the values of the scalar mesh object
attributes. 

Memory allocation for a new mesh object or for a mesh object
which will grow in size is accommodated by first setting the appropriate
scalars for the Mesh Object by using  **cmo_set_intinfo** with the new
number of elements and/or nodes and then calling **cmo_newlen**. These
two steps must be taken before adding to the size of a Mesh Object.


Mesh object parameters are retrieved with the [cmo_get_attparam](meshob.md#cmo_get_attparam) subroutine.

See [Mesh Object Subroutines](meshob.md) for a list of  mesh object subroutines .
