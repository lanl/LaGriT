---
GENERATOR: 'Mozilla/4.75 
[en
] (X11; U; IRIX 6.5 IP32) 
[Netscape
]'
---

[]{#cmo/set_idmo/set\_id**

 mo/set\_id** /mo\_name/ **both**  **node**  **element**
 /
[attribute\_name1
]/
[attribute\_name2
]
 creates integer attributes that contain the node or element number.

 if later operations delete nodes or elements causing renumbering,
 these attributes will contain the original node or element number.

  

**EXAMPLES:**

 mo/set\_id**

 mo/set\_id**/cmo1**/node**

 mo/set\_id**/cmo1**/element**

 mo/set\_id**/cmo1**/both**

 mo/set\_id**/cmo1**/node**/id\_node

 mo/set\_id**/cmo1**/element**/id\_elem

 mo/set\_id**/cmo1**/both**/id\_node1/id\_elem1

 mo**/reate**/cmo1//**/hex**

 reatepts****/brick****/xyz**/11,11,11/0.,0.,0./1.,1.,1./1,1,1

 mo/set\_id**

 mo/set\_id**/cmo1**/node**

 mo/set\_id**/cmo1**/element**

 mo/set\_id**/cmo1**/both**

 mo/set\_id**/cmo1**/node**/id\_node

 mo/set\_id**/cmo1**/element**/id\_elem

 mo/set\_id**/cmo1**/both**/id\_node1/id\_elem1

 **pset**/p\_xgthalf**/geom****/xyz**/1,0,0/.5,0,0/1,1,1

 **rmpoint****/pset,get,**p\_xgthalf**/inclusive**

 **rmpoint**/ompress**

 **dump****/gmv**/set\_id.gmv/cmo1

  

  

  


