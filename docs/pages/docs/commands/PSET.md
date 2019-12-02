---
title: "PSET"
tags: pset node selection
---

# PSET (Point Set) 

-------------------

Associate a name with a point set based on various geometric and logical operators. Manipulate point sets. Output point sets.
 

By convention, `ifirst,ilast,istride` syntax represents a range selection defined either by a set of points from ifirst to ilast, with increments of istride (1 by default). A set selection can also be defined with **pset,get,** `pset_name` where `pset_name` has been defined by the following **`PSET`** commands. Most commands with the syntax `ifirst,ilast,istride` can also use **pset,get,** `pset_name`.
 
The following are the syntax and parameter definitions:

 
### attribute
forms a pset from points in selected range which have the specified value for a node based attribute. 
If the optional comparator field is given; that operation is used to compare the attribute value to the requested value. 
Note the comparator fields and the value can swap argument order.
*This option was previously named zq in old releases*.
<pre>
<b>pset</b>/pset_name/ <b>attribute</b> / attribute_name /ifirst,ilast,istride/[<b>eq</b> or <b>ne</b> or <b>lt</b> or <b>gt</b> ] / [value]
</pre>


### constraints
forms a pset of nodes having the specified number of constraints.  The node's **icr** value is used as an index to the **icontab** attribute which gives the number of constraints.  [See Mesh Object](../meshobject.md) for an explanation of the **icontab** entries.
<pre>
<b>pset</b>/pset_name/<b>constraints</b>/ num_constraints 
</pre>


### delete
removes a previously defined pset
<pre>
<b>pset</b>/pset_name/ <b>delete</b>
</pre>


### eltset
forms a pset of nodes in the element set named.
<pre>
<b>pset</b>/pset_name/ <b>eltset</b> / element_set_name
</pre>


### geom
forms a pset from all points inside a geometric shape as defined in the following options:
<pre>
<b>pset</b>/pset_name/<b>geom / xyz</b> /ifirst,ilast,istride/ xl,yl,zl / xu,yu,zu/ xcen,ycen,zcen

<b>pset</b>/pset_name/<b>geom / rtz</b> /ifirst,ilast,istride/ r1,t1,z1 / r2,t2,z2/ xcen,ycen,zcen

<b>pset</b>/pset_name/<b>geom / rtp</b> /ifirst,ilast,istride/ r1,t1,p1 / r2/t2/p2/ xcen,ycen,zcen
</pre>

-  **xyz** forms a pset from all points inside a box whose corners are xl,yl,zl and xu,yu,zu relative to the geometry center at xc,yc,zc.

-  **rtz** forms a pset of nodes within the cylinder or cylindrical shell given by radius r1 to r2, angle theta t1 to t2 and height z1 to z2.

-  **rtp** forms a pset of nodes within the sphere, sperical shell or sperical section given by radius r1 to r2, and angles theta t1 to t2 and angles phi p1 to p2.  [See  Conventions](../conventions.md) for an explanation of angles theta and phi.
    


### list
lists nodes in a pset or names of all psets for the mesh object.
<pre>
<b>pset</b>/pset_name/<b>list</b> </pre>


### union inter not
logical operations **`union`**, **`inter`** and **`not`** act on previously defined psets.  The definition of the unary operator **`not`** is extended such that **`not`**/p1/p2 means p1 and (not(p2)).
<pre>
<b>pset</b>/pset_name/ <b>union</b> or <b>inter</b> or <b>not</b> / pset1[  ,pset2, … psetn ]
</pre>


### region mregion 
will return all nodes that are in the specified region or mregion.
<pre>
<b>pset</b>/pset_name/<b>region</b> or <b>mregion</b> / region_name / ifirst,ilast,istride
</pre>



### seq 
forms a pset of the nodes defined by ifirst, ilast, istride;
the special syntax,: 1,0,0 refers to all nodes and 0,0,0 refers to the last set of nodes created.
<pre>
<b>pset</b>/pset_name/ <b>seq</b> /ifirst,ilast,istride
</pre>


### surface 
identifies nodes on the specified surface as indicated by defining surface_name.  The following keywords for surface_name can be used:

- **-all-** will identify nodes on all and any surfaces.
- **-interface-** will identify all nodes on interfaces.
- **-boundary-** will idendtify nodes on external boundary surfaces.

<pre>
<b>pset</b>/pset_name/ <b>surface</b> / surface_name / [ifirst,ilast,istride]
</pre>



### write
write or dump a pset node list to a file, options are **ascii** or **binary**.
<pre>
  <b>pset</b>/ [pset_name or <b>-all-</b>] / <b>write</b> / file_name[.vertexset] / [<b>ascii</b> or <b>binary</b>]
</pre>


### zone zonn
write pset node list to a file (FEHM Flow and Transport code zone file format). By default the zone_number is a number 1-n where n is the number of psets defined in the mesh object. Specify a number value for a single zone file with the zone_number option. 
<pre>
<b>pset</b> / [name or <b>-all-</b>] / <b>zone</b> or <b>zonn</b> / file_name[.zone or .zonn] / [<b>ascii</b>] [zone_number]
</pre>



### zq (deprecrated)
<pre>
<b>pset</b>/pset_name / attribute_name/ ifirst,ilast,istride / [value] / [<b>eq</b> or <b>ne</b> or <b>lt</b> or <b>gt</b> ]
  
replaces:
<b>zq</b> / attribute_name / ifirst,ilast,istride / [value] / [<b>eq</b> or <b>ne</b> or <b>lt</b> or <b>gt</b> ]
</pre>

  
  
## EXAMPLES

    pset/apset/seq/1,0,0/

associate the pset name apsetwith all points.

    pset/apset/seq/0,0,0/

associate the pset name apset with the last set of nodes created.

    pset/apset/ union /pset1,pset2,pset3

associate the pset name apset with the set of nodes which belong to at least one of pset1, pset2, pset3.

    pset/apset/ inter / pset1,pset2,pset3

 associate the pset name apset with the set of nodes which belong to pset1, and pset2, and pset3.

     pset/apset/ not /pset1,pset2,pset3

 associate the pset name apset with the set of nodes which belong to pset1, and do not belong to pset2, and do not belong to pset3 pset/apset/not/pset1 associate the pset name apset with the set of nodes which do not belong to pset1


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
 
      pset/apset/ inter / pset1,pset2,pset3

 associate the pset name apset with the set of nodes which belong to pset1, and pset2, and pset3.

      pset/p1/attribute/zic/1,0,0/gt 1.0
      pset/p2/attribute/zic/1,0,0/lt 10.0
      pset/pboth/ inter / p1 p2
      
      pset/p1/attribute/zic/ 1,0,0 /gt 1.0
      pset/pboth/attribute/zic/ pset,get,p1 / lt 10.0
      

associate the name pboth with all nodes that have zic values between 1.0 and 10.0, note both sets of commands have the same result.
  
    pset/mypset/geom/rtz/pset,get,oldpset/0.,0.,0./10.,360.,10.  

associate the name mypset with the nodes that are members of the pset oldpset and which fall inside the cylinder of radius 10 and height 10 and whose axis is the z-axis.

    pset/spset/surface/s1/1,0,0

associate the name spset with the set of nodes that lie on the surface s1.

    pset/spseta/surface/s2/pset,get,spset
    
associate the name spseta with the set of nodes that lie on the surface s2 and which are members of the pset spset  This command and the previous command would identify the nodes that are on the intersection of surfaces s1 and s2 and give the name spseta to these nodes.

    pset/mypset/constraints/3

associate the name mypset with the set of nodes that have 3 constraints ( normally the set of nodes that lie on 3  constrained surfaces -- surfaces of type reflect or intrcons)


[Click here for demos](../demos/main_pset.md)
