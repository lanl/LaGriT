---
Author: Jan Wills
GENERATOR: 'Mozilla/4.05C-SGI [en] (X11; I; IRIX 6.5 IP32) [Netscape]'
---

 

**length**/ mo\_name/att\_name

mo\_name **** is type character, default is **-all-**

att\_name **** is type character, default is **-all-**

Returns the memory length of attribute att\_name for Mesh Object,
mo\_name.

 **EXAMPLES:**

  mo/length**/ mo\_tet2/boron

  mo/length/-cmo-**/boron

  mo/length/-default-**/boron

  mo/length/-cmo-/-all-**

  mo/length**/ mo\_tet2**/-all-**

  mo/length**

  mo/length/-all-/-all-**

  mo/length/-all-**/boron
