---
Generator: Microsoft Word 98
---

****

[]{#_Toc374864041[]{#_Toc375390638[]{#_Toc390510541[]{#_Toc390570210[]{#_Toc390573616[]{#_Toc390586[]{#_Toc397506926[]{#_Toc397507074[]{#_Toc397507199[]

The **region** command is used to divide the enclosing volume into
regions. The directional operators **lt**, **le**, **gt**, **** and
**ge** are applied to previously defined surfaces ac

**lt --** if the surface following is a volume then **lt** means inside
not including the surface of the volume. If the surface is a plane or a
sheet **lt** means the space on t ****

le -- if the surface following is a volume then **le** means inside
including the surface of the volume. If the surface is a plane or a
sheet **le** means the space on the

**gt --** if the surface following is a volume then **gt** means outside
not including the surface of the volume. If the surface is a plane or a
sheet **gt** means the space on the same side of the plane or sheet as
the normal not in ****

ge -- if the surface following is a volume then **ge** means outside
including the surface of the volume. If the surface is a plane or a
sheet **ge** means the spaon on the

In region comands, surface names must be preceeded by a directional
operator. The logical operators **or**, **and**, and **not** mean union,
intersection and complement respecti

**region**/top/ **le** cube **and** **gt** cutplane 

**region**/bottom/ **le** cube **and** **le** cutplane 

The region *bottom* contains the interface *cutplane;* *top* contains
none of the interface. Interior interfaces must be included in one and
only one region.

If a region touches an external boundary, include the surface that
defines the enclosing volume in **region** and **mregion** commands. For
example, the regions *top* and *bottom* are enclosed by the surface cube

<img height="300" width="300" src="Image224.gif">"344" "239"
