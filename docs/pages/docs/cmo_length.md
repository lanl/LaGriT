---
Author: Jan Wills
GENERATOR: 'Mozilla/4.05C-SGI [en] (X11; I; IRIX 6.5 IP32) [Netscape]'
---

 

**length**/mo\_name/att\_name

mo\_name **** is type character, default is **-all-**

att\_name **** is type character, default is **-all-**

Returns the memory length of attribute att\_name for Mesh Object,
mo\_name.

 **EXAMPLES:**

  **cmo/length**/mo\_tet2/boron

  **cmo/length/-cmo-**/boron

  **cmo/length/-default-**/boron

  **cmo/length/-cmo-/-all-**

  **cmo/length**/mo\_tet2**/-all-**

  **cmo/length**

  **cmo/length/-all-/-all-**

  **cmo/length/-all-**/boron
