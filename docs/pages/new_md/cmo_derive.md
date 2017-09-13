---
Author: Jan Wills
GENERATOR: 'Mozilla/4.05C-SGI [en] (X11; I; IRIX 6.5 IP32) [Netscape]'
---

 

**derive**/mo\_name/master\_mo/
mo\_name is type character, required.

master\_mo is type character, default is -cmo-

Uses Mesh Object, master\_mo, as the template for deriving Mesh Object,
mo\_name. Mesh Object, mo\_name, will be an image of master\_mo but will
contain no data. The output Mesh Object, mo\_name, will become the
Current Mesh Object. If mo\_name is the same as master\_mo nothing
happens. If mo\_name exists it is over written.

 
** ** **EXAMPLES:**
**cmo/derive**/mo\_tet2/mo\_tet1

**cmo/derive/-cmo-**/mo\_tet1

**cmo/derive**/mo\_tet2

**cmo/derive**/mo\_tet2**/-cmo-**

**cmo/derive/-default-/-cmo-**

**cmo/derive**/mo\_tet2**/-default-**

**cmo/derive/-default-**/mo\_tet1
