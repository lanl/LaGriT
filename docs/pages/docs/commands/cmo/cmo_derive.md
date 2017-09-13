---
Author: Jan Wills
GENERATOR: 'Mozilla/4.05C-SGI 
[en
] (X11; I; IRIX 6.5 IP32) 
[Netscape
]'
---

 

**derive**/ mo\_name/master\_mo/
mo\_name is type character, required.

master\_mo is type character, default is -cmo-

Uses Mesh Object, master\_mo, as the template for deriving Mesh Object,
mo\_name. Mesh Object, mo\_name, will be an image of master\_mo but will
contain no data. The output Mesh Object, mo\_name, will become the
Current Mesh Object. If mo\_name is the same as master\_mo nothing
happens. If mo\_name exists it is over written.

 
** **EXAMPLES:
mo/derive**/ mo\_tet2/mo\_tet1

mo/derive/-cmo-**/ mo\_tet1

mo/derive**/ mo\_tet2

mo/derive**/ mo\_tet2**/-cmo-**

mo/derive/-default-/-cmo-**

mo/derive**/ mo\_tet2**/-default-**

mo/derive/-default-**/ mo\_tet1
