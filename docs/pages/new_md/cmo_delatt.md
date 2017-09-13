---
Author: Jan Wills
GENERATOR: 'Mozilla/4.7C-SGI [en] (X11; I; IRIX64 6.5 IP30) [Netscape]'
---

 

**delatt**/mo\_name/att\_name

Deletes the attribute att\_name from Mesh Object, mo\_name. Will not
delete an attribute with a persistence of 'permanent'.

 mo\_name must be specified.

att\_name must be specified.

 **EXAMPLES:**

  **cmo/delatt**/mo\_tet2/boron

  **cmo/delatt/-cmo-**/boron

  **cmo/delatt/-default-**/boron

  **cmo/delatt/-cmo-/-all-** (this will delete all attributes with
  persistence of temporary)

 **DELATT**/mo\_name/att\_name/

  Deletes the attribute att\_name from Mesh Object, mo\_name even if
  the attribute has a persistence of 'permanent'.

   mo\_name must be specified.

  att\_name must be specified.

 **EXAMPLES:**

  **cmo/DELATT**/mo\_tet2/xic
