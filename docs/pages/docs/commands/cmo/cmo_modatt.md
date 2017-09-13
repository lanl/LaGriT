---
Author: Jan Wills
GENERATOR: 'Mozilla/4.75 [en] (X11; U; IRIX 6.5 IP32) [Netscape]'
---

   

 ** modatt**/ mo\_name/att\_name/field\_name/new\_field/

 Modifies the field field\_name for attribute att\_name in Mesh Object
 mo\_name.

  mo\_name required.

  att\_name required.

  field\_name is type character, required

  new\_field is the type of the field, required

  Field\_names (may be lower or upper case):

  --------------------- --------------------------------------------------------------------------------------------
  **name** -            (character) Attribute name 

  t**ype -**            character) Attribute type

                        **INT**- Integer

                        **REAL** - Real number

                        **CHARACTER** - character variable of length 32

                        **VINT** - Vector of integer 

                        **VDOUBLE** - Vector of real
*8 (this is the default)

                        **VCHAR** - Vector of character
*32

  **rank** -            (character) Attribute rank (must be an attribute for this Mesh object)  default is scalar

  **length** -          (character) Attribute length (must be an attribute for this Mesh object) default is nnodes

  **interpolation** -   (character) Interpolation option: 

  onstant** -        Constant value 

  **sequence** -        Set to the node number

  opy** -            Copy values

  **linear** -          Linear interpolation  - this is the default

   **user** -           User provides a subroutine named user\_interpolate ([see IV. e.8](../../miscell.md))

  **log** -             Logarithmic interpolation

  **asinh** -           Asinh interpolation

  ** min** -             Set to the minimum

  ** max** -             Set to the maximum

  **incmin** -          Set to the minimum plus one (vint attribute only)

  **incmax** -          Set to the maximum plus one (vint attribute only)

  **and** -             'and' the bits

  **or** -              'or' the bits

  **persistence** -     (character) Attribute persistence:

  **permanent -**       Can not be deleted 

  emporary** -       Temporary attribute - this is the default

  **ioflag** -          (character) Attribute IO flag:

                        default is **ag**

  **a **                Put this attribute on avs dumps

  **g **                Put this attribute on gmv dumps

  **f**                 Put this attribute on fehm dumps 

  **l**                 Put this attribute on LaGriT dumps

  **L**                 Do not write this attribute to LaGriT dumps

  ** default**          (real) Attribute value
  --------------------- --------------------------------------------------------------------------------------------



 

 ** ** **EXAMPLES:**

  mo/modatt**/ mo\_tet2/boron**/length**

  mo/modatt/-cmo-**/boron**/length/nnodes**

  mo/modatt/-cmo-**/boron**/default**/10.0

  mo/modatt/-def-**/boron**/default**/10.0

  mo/modatt**/-def-boron**/interp/user**
