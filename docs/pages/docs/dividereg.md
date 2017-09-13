 **4. Divide the enclosing volumes into regions**

The [**region**](commands/REGION.md) command is used to divide the
enclosing volume into regions. The directional operators **lt**, **le**,
**gt**, ** ** and **ge** are applied to previously defined surfaces
according to the following rules.

**lt --** if the surface following is a volume then **lt** means inside
not including the surface of the volume. If the surface is a plane or a
sheet **lt** means the space on the side of the plane or sheet opposite
to the normal not including the plane or sheet itself.

**le --** if the surface following is a volume then **le** means inside
including the surface of the volume. If the surface is a plane or a
sheet **le** means the space on the side of the plane or sheet opposite
to the normal including the plane or sheet itself.

**gt --** if the surface following is a volume then **gt** means outside
not including the surface of the volume. If the surface is a plane or a
sheet **gt** means the space on the same side of the plane or sheet as
the normal not including the plane or sheet itself.

**ge --** if the surface following is a volume then **ge** means outside
including the surface of the volume. If the surface is a plane or a
sheet **ge** means the space on the same side of the plane or sheet as
the normal including the plane or sheet itself.

In region comands, surface names must be preceeded by a directional
operator. The logical operators **or**, **and**, and **not** mean union,
intersection and complement respectively. Parentheses are operators and
are used for nesting. Spaces are required as delimiters to separate
operators and operands. To define the two regions created by the plane
bisecting the unit cube:

**region**/top/ **le** cube **and** **gt** cutplane 

**region**/bottom **/ le** cube **and le** cutplane 

The region *bottom* contains the interface *cutplane*; top contains none
of the interface. Interior interfaces must be included in one and only
one region.

If a region touches an external boundary, include the surface that
defines the enclosing volume in region and mregion commands. For
example, the regions *top* and *bottom* are enclosed by the surface
*cube*

<img height="300" width="300" src="https://lanl.github.io/docs/assets/images/Image224.gif">
