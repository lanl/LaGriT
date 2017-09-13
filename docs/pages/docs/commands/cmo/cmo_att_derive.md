---
GENERATOR: 'Mozilla/4.79C-SGI 
[en
] (X11; U; IRIX64 6.5 IP30) 
[Netscape
]'
title: 'cmo/addatt - add an attribute to a mesh object'
---

cmo/attribute\_derive
---------------------

 The mo/attribute\_derive** command is used to give one mesh object
 (at least) the same set of attributes as another mesh object. This is
 useful, for example, for merging two mesh objects. Specifically, it
 looks at the set of attributes present in the source mesh, compares it
 to the set of attributes in the sink mesh, and adds to the sink mesh
 any attributes that it is missing.

 

 The command needs a target mesh to be designated. If no source mesh is
 given, it will use the current mesh object.

 **FORMAT:**

  mo/attribute\_derive** / sink\_mo\_name /  
[ src\_mo\_name 
]

 

 **EXAMPLES:**

  [mo****/attribute\_derive**/cmo\_sink/cmo\_src]{.style1
  [mo****/attribute\_derive**/empty\_cmo]{.style1


cmo/attribute\_union
--------------------

 The mo/attribute\_union** command is used to give two mesh objects
 the same set of attributes as each other. This is useful, for example,
 for merging two mesh objects. Specifically, it looks at the set of
 attributes present in each mesh, compares it to the set of attributes
 in the other mesh, and makes it so each mesh posesses the union of the
 two sets of attributes. In particular, this is just a wrapper for the
 above call - it executes
 [attribute\_derive]{style="font-weight: bold;" twice, once in each
 direction.

 

 The command needs both meshes to be designated.

 **FORMAT:**

  mo/attribute\_union** / sink\_mo\_name /  src\_mo\_name


 

 **EXAMPLES:**

  [mo****/attribute\_derive**/cmo2/cmo1]{.style1
  []{.style1
