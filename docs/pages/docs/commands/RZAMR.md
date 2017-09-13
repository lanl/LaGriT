---
Author: Jan Wills
GENERATOR: 'Mozilla/4.05C-SGI [en] (X11; I; IRIX 6.5 IP32) [Netscape]'
---

**RZAMR**

 **RZAMR** uses an octree type refinement applied to an existing
 hexahedral mesh to all nodes in a specified region.  No additional
 elements are kept,  the intention is that the resulting node
 distribution will be passed to **** connect to generate a tetrahedra
 mesh.

 **FORMAT:**

  **rzamr**/region\_name/number\_of\_levels

  
*  region\_name    name of region to refine.  If blank,  all
  regions will be refined.  An element will be refined if any node of
  the element is in the specified region.

  
*  number\_of\_levels    is the number of times the refinement will
  be performed.  After each level, the code will determine which of
  the new nodes are in the specified region and will refine the
  associated elements.  Default is 1.

   

 **EXAMPLES:**

  **rzamr**    refine the entire mesh

  **rzamr** /r1/3    refine elements with nodes in the region r1 three
  times
 
  examples of use of **rzamr**:
 
  
*create the hex mesh

  cmo/create/cmo///hex

  
* define geometry

  surface/inside/reflect/box/0,0,0/1,1,1

  surface/diag/intrface/plane/0,0,0/1,0,1/1,1,1

  region/lin/ le inside and ge diag /

  region/rin/ le inside and lt diag /

  mregion/mlin/ le inside and gt diag /

  mregion/mrin/ le inside and lt diag /

  
* distribute nodes

  quadxyz/2,2,2/0.,0.,0./1.,0.,0./1.,1.,0./0.,1.,0./ &

  0.,0.,1./1.,0.,1./1.,1.,1./0.,1.,1./

  
* set node types and materials

  setpts

  
* connect up the hex mesh

  rzbrick/xyz/2,2,2/1,0,0/connect

  
* refine the hex mesh

  rzamr/lin/1

  rzamr/rin/3

  
* create the tet mesh

  cmo/create/cmot///tet

  
* define geometry again for tet mesh

  surface/inside/reflect/box/0,0,0/1,1,1

  surface/diag/intrface/plane/0,0,0/1,0,1/1,1,1

  region/lin/ le inside and ge diag /

  region/rin/ le inside and lt diag /

  mregion/mlin/ le inside and gt diag /

  mregion/mrin/ le inside and lt diag /

  
* copy in the nodes from the hex mesh to the tet mesh

  copypts/cmot/cmo

  cmo/select/cmot

  cmo/release/cmo

  
* reset node types and materials so that setpts will use

  
* geometry to figure out the correct values

  cmo/setatt/cmot/itp/1,0,0/0

  cmo/setatt/cmot/imt/1,0,0/0

  
* set node types and materials

  setpts

  
* connect up the tet mesh

  connect

  
* set element materials

  
* and create parent/child nodes on interfaces

  settets

  dump/gmv/gmvtet

  finish

   

 
