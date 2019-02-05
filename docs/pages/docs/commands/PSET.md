---
title: "PSET"
categories: pset point selection command
---

# PSET (Point Set) #

 Associate a name with a point set based on various geometric and logical operators. Manipulate point sets. Output point sets.
 
 ## SYNTAX ##
 
 <pre>
 <b>pset</b>/ pset_name / select_type / select_type_options
 </pre>
 
 The following are definitions for the various selection types:
 
**`attribute`** forms a pset from all points in ifirst,ilast,istride which have the specified value for a node based
  attribute. If the optional comparator field is given; that operation is used to compare the attribute value to the requested value. *This option was previously named* **zq.** in old releases.
  <pre>
  pset / [name or -all-] / attribute_name / ifirst,ilast,istride / [eq or ne or lt or gt ] / [value]
  pset / [name or -all-] / attribute_name / ifirst,ilast,istride / [value] / [eq or ne or lt or gt ]
  
  replaces:
  zq / attribute_name / ifirst,ilast,istride / [value] / [eq or ne or lt or gt ]
  </pre>

**`constraints`** forms a pset of nodes having the specified number of constraints.  The node's **icr** value is used as an index to the **icontab** attribute which gives the number of constraints.  [See chapter III, A](../meshobject.md) for an explanation of the **icontab** entries.

**`delete`** deletes a previously defined pset
<pre>
pset/ pset_name / delete
</pre>

**`eltset`** forms a pset of nodes in the element set named.
<pre>
pset/ pset_name /eltset/element_set_name
</pre>


**`geom`** / shape_type / forms a pset from all points inside a geometric shape as defined in the following options:
<pre>
pset/ pset_name /geom / xyz /ifirst,ilast,istride/ xl,yl,zl / xu,yu,zu/ xcen,ycen,zcen

pset/ pset_name /geom / rtz /ifirst,ilast,istride/ r1,t1,z1 / r2,t2,z2/ xcen,ycen,zcen

pset/ pset_name /geom / rtp /ifirst,ilast,istride/ r1,t1,p1 / r2/t2/p2/ xcen,ycen,zcen
</pre>

    **`xyz`** forms a pset from all points inside a box whose corners are xl,yl,zl and xu,yu,zu relative to the geometry center at xc,yc,zc.

    **`rtz`** forms a pset of nodes within the cylinder or cylindrical shell given by radius r1 to r2, angle theta t1 to t2 and height z1 to z2.

    **`rtp`** forms a pset of nodes within the sphere, sperical shell or sperical section given by radius r1 to r2, and angles theta t1 to t2 and angles phi p1 to p2.  [See chapter II, A. Conventions](../conventions.md) for an explanation of angles theta and phi.
    

**`list`** lists nodes in a pset or names of all psets for the mesh object.

logical operations **`union`**, **`inter`** and **`not`** act on previously defined psets.  The definition of the unary operator **`not`** is extended such that **`not`**/p1/p2 means p1 and (not(p2)).
<p>
pset/pset_name/ union inter not /pset1[/ pset2/…/psetn ]
</p>

**`region`** or **`mregion`** will return all nodes
  that are in the specified region/mregion - the definition of the
  region/mregion is evaluated to determine membership.  Hence the
  result may vary from what would be returned if the 'imt1' value of
  the nodes had been queried using the **attribute** option
<pre>
pset/ pset_name /region or mregion / region_name / ifirst,ilast,istride
</pre>

 **`seq`** forms a pset of the nodes defined by ifirst, ilast, istride;
the special syntax,: 1,0,0 refers to all nodes and 0,0,0 refers to the last set of nodes created.
<pre>
pset/ pset_name / seq /ifirst,ilast,istride
</pre>

**`surface`** identifies nodes on the specified surface.  Keyword surface names have the following meaning:
<pre>
pset/ pset_name /surface/surface_name/[ifirst,ilast,istride]
</pre>

    -**all**- will identify nodes on any surface.

    -**interface**- will identify nodes on any interface surface.

    -**boundary**- will idendify nodes on exterior surfaces.
   

 **`write`** write pset node list to a file, options are:
<pre>
  pset / [pset_name or -all-] / write/ file_name[.vertexset] / [ascii or binary]
 </pre>

 **`zone`** or **`zonn`** write pset node list to a file (FEHM Flow and Transport code zone file format). By default the zone_number is a number 1-n where n is the number of psets defined in the mesh object. Specify a number value for a single zone file with the zone_number option. 
<pre>
  pset / [name or -all-] / zone or zonn / file_name[.zone or .zonn] / [ascii] [zone_number]
  </pre>

  
## EXAMPLES ##

    pset/apset/seq/1,0,0/

associate the pset name apsetwith all points.

    pset/apset/seq/0,0,0/

associate the pset name apsetwith the last set of nodes created.

    pset/apset/union/pset1,pset2,pse

associate the pset name apset with the set of nodes which belong to at least one of pset1, pset2, pset3.

    pset/apset/inter/pset1,pset2,pset3

 associate the pset name apset with the set of nodes which belong to pset1, and pset2, and pset3.

     pset/apset/not/pset1,pset2,pset3

 associate the pset name apset with the set of nodes which belong to pset1, and do not belong to pset2, and do not belong to pset3
pset/apset/not/pset1
 associate the pset name apset with the set of nodes which do not belong to pset1

      pset/list/

 list the names of all psets
pset/mypset/list
 output the list the node numbers of the members of mypset to the screen and the log file outx3dgen

      pset/mypset/write/file_name.vertexset/ascii

 Write list of nodes in pset mypset to an ascii file named file_name.vertexset
pset/-all-/write/root_name/ascii
Write list of nodes in all psets. root_name is treated as a root name and each pset is written to a separate file beginning with that root. For example, if you have psets named pset1 and pset2, they will be written to files called root_name_pset1.vertexset and root_name_pset2.vertexset.

    pset/mypset/zone/file_name.zone/ascii

 Write list of nodes in pset mypset to an ascii file named file_name.zone in FEHM zone file format. The zone number will be 1 if this is the first pset for the mesh object. 
 
         pset/mypset/zone/file_name.zone/ascii 42

Write list of nodes in pset mypset to an ascii file named file_name.zone in FEHM zone file format. The zone number will be 42 as specified.

        pset/apset/attribute/itp/1,0,0/10/ge
        
 associate the name apset with the points whose type field(itp1) has value greater than or equal to 10 (these would be boundary nodes).

      pset/mypset/geom/xyz/1,0,0/1.,1.,-5./10.,20.,10./

associate the name mypset with all nodes that fall with the box with corners at (1,1,-5) and (10,20,10)
  
    pset/mypset/geom/rtz/pset,get,oldpset/0.,0.,0./10.,360.,10.  

associate the name mypset with the nodes that are members of the pset oldpset and which fall inside the cylinder of radius 10 and height 10 and whose axis is the z-axis.

    pset/spset/surface/s1/1,0,0

associate the name spset with the set of nodes that lie on the surface s1.
pset/spseta/surface/s2/pset,get,spset
associate the name spseta with the set of nodes that lie on the surface s2 and which are members of the pset spset  This command and the previous command would identify the nodes that are on the intersection of surfaces s1 and s2 and give the name spseta to these nodes.

    pset/mypset/constraints/3

associate the name mypset with the set of nodes that have 3 constraints ( normally the set of nodes that lie on 3  constrained surfaces -- surfaces of type reflect or intrcons)

[Click here for demos](../demos/main_pset.md)
