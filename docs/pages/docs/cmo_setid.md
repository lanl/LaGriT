---
GENERATOR: 'Mozilla/4.75 [en] (X11; U; IRIX 6.5 IP32) [Netscape]'
---
[]{#cmo/set_id**cmo/set\_id**

 **cmo/set\_id** /mo\_name/ **both**  **node**  **element**
 /[attribute\_name1]/[attribute\_name2]
 creates integer attributes that contain the node or element number.

 if later operations delete nodes or elements causing renumbering,
 these attributes will contain the original node or element number.

  

**EXAMPLES:**

 **cmo/set\_id**

 **cmo/set\_id**/cmo1**/node**

 **cmo/set\_id**/cmo1**/element**

 **cmo/set\_id**/cmo1**/both**

 **cmo/set\_id**/cmo1**/node**/id\_node

 **cmo/set\_id**/cmo1**/element**/id\_elem

 **cmo/set\_id**/cmo1**/both**/id\_node1/id\_elem1

 **cmo****/create**/cmo1//**/hex**

 **createpts****/brick****/xyz**/11,11,11/0.,0.,0./1.,1.,1./1,1,1

 **cmo/set\_id**

 **cmo/set\_id**/cmo1**/node**

 **cmo/set\_id**/cmo1**/element**

 **cmo/set\_id**/cmo1**/both**

 **cmo/set\_id**/cmo1**/node**/id\_node

 **cmo/set\_id**/cmo1**/element**/id\_elem

 **cmo/set\_id**/cmo1**/both**/id\_node1/id\_elem1

 **pset**/p\_xgthalf**/geom****/xyz**/1,0,0/.5,0,0/1,1,1

 **rmpoint****/pset,get,**p\_xgthalf**/inclusive**

 **rmpoint****/compress**

 **dump****/gmv**/set\_id.gmv/cmo1

  

  

  


