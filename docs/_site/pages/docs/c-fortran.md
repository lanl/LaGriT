# C-FORTRAN Interface

Mesh Objects and their attribute data are accessed through a set of fortran subroutines.
A set of c-fortran wrappers enable C++ codes to create and access a mesh object.
The main drivers and interface is written in Fortran, these are used to parse command lines and create mesh objects.  Once setup, the mesh data is passed to Fortran, C, or C++ routines. 


As an example of how these c-fortran routines are used, see the poisson files in the LaGriT/src directory. 
```
poi_driver.f = poisson command, parsing, data prep for polygon
               calls  poisson_2d_()

poi_routine_2D.cpp = sets polygon and calls poi_ routines
poi_polygon.cpp and poi_polygon.h = definitions and routines for polygon object
```

## C++ Routines to Access Mesh Object

These files are used for c-fortran codes:
```
lg_c_wrappers.cpp  = exposes the raw fortran subroutines to C/C++ codes 
                     wrappers for dotask and cmo_get_info routines
lg_fc_wrappers.f90 = fortran calls for C to get mesh object pointers
                     assigns mesh data cray pointer to C pointer
lg_c_interface.h   = C++ declarations for lg_c_wrappers.cpp
lg_f_interface.h   = fortran declarations 
fc_mangle.h (name mangling created by cmake with names for c-fortran)

lg_example.cpp = test lg c and fc wrappers
lg_example_fortran.f90 = test various fotran commands used as wrappers

In general:
lg_routines are C++ wrappers calling fortran using C arguments
fc_routines are f90 wrappers calling fortran using c-fortran arguments
```



## C++ Wrappers for Fortran Subroutines

``` 
LG_ERR lg_cmo_get_name(char* name_buffer, int name_buffer_size);

example:
    char cmo_name[32];
    err = lg_cmo_get_name(cmo_name, 32);
```
Get the name of the current mesh object. (Only one mesh object is current at any time.)
Note the length of the string must be included as the last argument.


```
LG_ERR lg_dotask(const char* cmd);

example: 
    const char* cmds[] = {
        "cmo/create/tmp_hex/ / /hex",
        "createpts/brick/xyz/3,3,3 /1.,2.,3./1.5,2.5,3.5/"};

    for (int i = 0; i < sizeof(cmds)/sizeof(cmds[0]); ++i) {
        err = lg_dotask(cmds[i]);
    }
    err = lg_dotask("cmo/status/tmp_hex/");
```
The lg_dotask() routine uses a char buffer to send LaGriT commands for processing. This is similar to calling commands during LaGriT runs.


```
int lg_cmo_get_intinfo(const char* ioption, const char* cmo_name);

example: int nnodes = lg_cmo_get_intinfo("nnodes", cmo_name);
```
Get mesh object attribute, returns the integer value of the named attribute in named mesh object. In this example the number of nodes in a mesh is returned.

```
void lg_cmo_get_int(const char* ioption, const char* cmo_name, long ival);
void lg_cmo_get_vint(const char* ioption, const char* cmo_name, long* ival);

example:
    char att1[ ]="imt";
    cmolen = strlen(cmo_name);
    iattlen = strlen(att1);
    fc_cmo_get_vint_(cmo_name,att1,&iptr,&nlen,&ierr,icmolen,iattlen);
    printf("return imt nlength: %d\n", nlen);
```
Get a pointer to the scaler or vector array of mesh object attribute of type int. In this example the pointer to the mesh array for "imt" is assigned to iptr, with the length in variable nlen.


## Fortran Wrappers for data pointers

```
void fc_cmo_get_double(const char* cmo, const char* att, double xval, integer ierr);
void fc_cmo_get_vdouble(const char* cmo, const char* att, double** iptr, long* nlen, size_t cmolen, size_t attlen);

example:
   icmolen = strlen(mo_poly_name);
   iattlen = 4;
   fc_cmo_get_double_(mo_poly_name,"xmin",&xmin,&ierr,icmolen,iattlen);
   iattlen = 3;
   fc_cmo_get_vdouble_(mo_poly_name,"xic",&xptr,&nlen,&ierr,icmolen,iattlen);
```
Get a pointer to the scaler or vector array of mesh object attribute of type double. These examples show the mesh attribute xmin assigned to the variable xmin scalar value and the xptr pointer to the xic attribute with length nlen.


## Rules for C++ calling fortran codes 

C++ prototype declarations must be extern "C" (this is not used for C codes)

Must match the Fortran and C++ types, for lagrit integers are 8 bytes
So make sure all are 8 bytes for integer=long, real*8=double

All Fortran arguments are passed as pointers, whether they are input or output values.
Array names such as cmd_buffer are already pointers, so argument is cmd_buffer.
but use & to pass values such as &size

Arrays in C and C++ index starts at 0. In Fortran the index starts at 1.

Fortran matrix element A(3,5) translates to C/C++ matrix element a[4][2].
(Subtract 1 for zero base indexing and reverse the order of the subscripts.)

All arguments are passed as pointers except char string which is an array of characters
A Char string must pass the lenth at end of argument list, this is a hidden value in the fortran call.


