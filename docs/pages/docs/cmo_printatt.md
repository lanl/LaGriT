---
Author: Jan Wills
GENERATOR: 'Mozilla/4.05C-SGI 
[en
] (X11; I; IRIX 6.5 IP32) 
[Netscape
]'
---

  

 **printatt**/cmo\_name/attribute\_name

 **printatt**/cmo\_name/attribute\_name/ 
[print\_opt
] /
 
[index\_set
]

  **attribute\_name -  ** name of valid cmo attribute or category of
  attributes

 **        node       ** = all attributes of nnodes length

        **  element  **  = all attributes of nelements length

  **-all-**         = all attributes of the cmo

  **-xyz- **       = xic, yic, and zic attributes of the cmo

 **        print\_opt**   ='value' or 'list' or 'minmax'

  **value**          = this is default, prints attribute values

  l**ist**              =print attribute name along with its length

  **minmax**      = print name, min and max of the attribute field,
  and its l length

   

 **EXAMPLES:**

 print field values:

 **cmo/printatt**/cmo1/zic/3,8,0

 **cmo/printatt**/cmo1/itetclr**/eltset,get**,eset1

 print attribute names:

 **cmo printatt**/cmo1**/-all-/list**

 **cmo printatt**/cmo1**/node/list**

 print min and max of attribute fields:

 **cmo printatt**/cmo1**/-all-/minmax**

 **cmo printatt**/cmo1**/-xyz-/minmax**

 **cmo printatt**/cmo1/xic**/minmax**/7,10,0

 **cmo printatt**/cmo1****/itp1**/minmax/pset,get**,psetr1

 attribute itp1 has length 'nnodes' hence the pset syntax is valid

 **cmo printatt**/cmo1****/itetclr**/minmax/eset,get**,smallets

 attribute itetclr has length 'nelements' hence the eset syntax is
 valid

  


