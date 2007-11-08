*dk getedges
      subroutine getedges(mpary,mpno,nnodes,nelements,itet,itetoff,
     &   itettyp,iparent,isubname1,inclusive,ipiedges,iedges_first)
C #####################################################################
C
C     PURPOSE -
C
C        Get the parent mass point - parent mass point relation.
C        That is, we compile the list of edges whose endpoints
C        are both in the mass point array MPARY.  It is assumed
C        that MPARY only contains parent nodes.
C
C     INPUT ARGUMENTS -
C
C        MPARY  - Mass point array.
C        NNODES    - No. of nodes in the mesh
C        NELEMENTS - No. of elements in the mesh
C        ITET      - The element-node relation
C        ITETOFF   - Offset array for ITET
C        ITETTYP   - Array of element types
C        IPARENT   - Array giving parent nodes
C        ISUBNAME1 - Partition that output IEDGES array belongs to.
C        INCLUSIVE - 1 means inclusive - edge is a refine candidate
C                    if either node is in pset,
C                  - 0 means exclusive - edge is a refine candidate
C                    if both nodes are in pset
C
C     OUTPUT ARGUMENTS -
C
C        IPIEDGES  - Pointer to IEDGES array which will be filled with
C                    parent mass points that neighbour a given parent
C                    mass point.
C        IEDGES_FIRST - Mapping from nodes into the IEDGES array.  For a
C                       node N, IEDGES_FIRST(N) gives the position in 
C                       IEDGES of the first neighbour mass point to N.
C $Log:   /pvcs.config/t3d/src/getedges.f_a  $
CPVCS    
CPVCS       Rev 1.3   22 Aug 2007 15:02:06   gable
CPVCS    Added getedges_d subroutine to get all edges around each node.
CPVCS    
CPVCS       Rev 1.2   Tue Jun 23 15:17:52 1998   dcg
CPVCS    changes for refine/rivara command
CPVCS    
CPVCS       Rev 1.1   Fri Oct 03 11:01:26 1997   dcg
CPVCS    reorder declarations as per DEC compiler
CPVCS    
CPVCS       Rev 1.0   Tue Jun 03 00:22:04 1997   kuprat
CPVCS    Initial revision.
C
C #####################################################################

      implicit none 
      include 'consts.h'
      include 'local_element.h'
      include 'chydro.h'

      integer lenptr, inclusive
      parameter (lenptr=1000000)

      pointer (ipiedges,iedges),(ipinvmpary,invmpary),
     &   (ipiedges_first_,iedges_first_),(ipiedges_,iedges_),
     &   (iplink,link)
      integer mpno,nelements,nnodes,lenlist,icscode,i,k,next,j,
     &   lenorig,inc,ityp,ic1,ic2,ip1,ip2,maxpar,minpar
      
      integer iedges(lenptr),iedges_first(lenptr),itet(lenptr),
     &   itetoff(lenptr),invmpary(nnodes),iedges_first_(nnodes),
     &   iedges_(lenptr),link(lenptr),itettyp(lenptr),iparent(nnodes),
     &   mpary(mpno)
      character*32 isubname, isubname1
      logical isedge

      isubname='getedges'
      
c.... Get local array storage.

      lenlist=100+mpno
      call mmgetblk('invmpary',isubname,ipinvmpary,nnodes,1,icscode)
      call mmgetblk('iedges_first_',isubname,ipiedges_first_,nnodes,1
     &   ,icscode)
      call mmgetblk('iedges_',isubname,ipiedges_,
     &   lenlist,1,icscode)
      call mmgetblk('link',isubname,iplink,lenlist,1,icscode)
      
c.... Compute the inverse mass point array map, and initialize all
c.... the linked lists to 'empty'.

      do i=1,nnodes
         invmpary(i)=0
         iedges_first_(i)=0
      enddo

c.... For parent nodes in the mass point array we define the
c.... inverse map.

      do k=1,mpno
         if (iparent(mpary(k)).eq.mpary(k)) then
            invmpary(mpary(k))=k
         endif
      enddo
      
c.... Loop thru all elements, and for each local edge with endpoints
c.... being parent mass points, put the larger index parent in the
c.... linked list for the lower index parent.  (If we put each node
c.... in the other node's linked list, then we would compute DOUBLE
c.... the number of edges.)

      next=1
      do i=1,nelements
         ityp=itettyp(i)
         do j=1,nelmnee(ityp)
            ic1=itet(ielmedge1(1,j,ityp)+itetoff(i))
            ic2=itet(ielmedge1(2,j,ityp)+itetoff(i))
            ip1=iparent(ic1)
            ip2=iparent(ic2)
            maxpar=max(ip1,ip2)
            minpar=min(ip1,ip2)
            if(inclusive.eq.0) then
               isedge = invmpary(maxpar).ne.0.and.invmpary(minpar).ne.0
            else
               isedge =invmpary(maxpar).ne.0.or.invmpary(minpar).ne.0
            endif
            if (isedge) then
               inc=mpno+100
               call insertll(iedges_first_,ipiedges_,iplink,lenlist,inc,
     &            minpar,maxpar,next)
            endif
         enddo
      enddo

c.... Increment storage in IEDGES if necessary.

      call mmblklen('iedges',isubname1,ipiedges,lenorig,icscode)
      if (lenorig.lt.next) then
         if (icscode.eq.0) then
            inc=next-lenorig
            call mmincblk('iedges',isubname1,ipiedges,inc,icscode)
         else
            call mmgetblk('iedges',isubname1,ipiedges,next,1,icscode)
         endif
      endif

c.... Compress all the individual linked lists into the output arrays.

      call orderll(iedges_first_,iedges_,link,nnodes,iedges_first,iedges
     &   )

      call mmrelprt(isubname,icscode)
      return
      end
      subroutine getedges_d(mpary,mpno,nnodes,nelements,itet,itetoff,
     &   itettyp,iparent,isubname1,inclusive,ipiedges,iedges_first)
C #####################################################################
C
C     PURPOSE -
C
C        This is almost exactly the same as getedges( ) except that
C        this version creates and edge list with all edges counted
C        twice. The getedges( ) version only reports nodes connected
C        to a node when the node number of the connection is larger.
C        This version reports larger and small node number connections.
C
C        Get the parent mass point - parent mass point relation.
C        That is, we compile the list of edges whose endpoints
C        are both in the mass point array MPARY.  It is assumed
C        that MPARY only contains parent nodes.
C
C     INPUT ARGUMENTS -
C
C        MPARY  - Mass point array.
C        NNODES    - No. of nodes in the mesh
C        NELEMENTS - No. of elements in the mesh
C        ITET      - The element-node relation
C        ITETOFF   - Offset array for ITET
C        ITETTYP   - Array of element types
C        IPARENT   - Array giving parent nodes
C        ISUBNAME1 - Partition that output IEDGES array belongs to.
C        INCLUSIVE - 1 means inclusive - edge is a refine candidate
C                    if either node is in pset,
C                  - 0 means exclusive - edge is a refine candidate
C                    if both nodes are in pset
C
C     OUTPUT ARGUMENTS -
C
C        IPIEDGES  - Pointer to IEDGES array which will be filled with
C                    parent mass points that neighbour a given parent
C                    mass point.
C        IEDGES_FIRST - Mapping from nodes into the IEDGES array.  For a
C                       node N, IEDGES_FIRST(N) gives the position in 
C                       IEDGES of the first neighbour mass point to N.
C
C #####################################################################

      implicit none 
      include 'consts.h'
      include 'local_element.h'
      include 'chydro.h'

      integer lenptr, inclusive
      parameter (lenptr=1000000)

      pointer (ipiedges,iedges),(ipinvmpary,invmpary),
     &   (ipiedges_first_,iedges_first_),(ipiedges_,iedges_),
     &   (iplink,link)
      integer mpno,nelements,nnodes,lenlist,icscode,i,k,next,j,
     &   lenorig,inc,ityp,ic1,ic2,ip1,ip2,maxpar,minpar
      
      integer iedges(lenptr),iedges_first(lenptr),itet(lenptr),
     &   itetoff(lenptr),invmpary(nnodes),iedges_first_(nnodes),
     &   iedges_(lenptr),link(lenptr),itettyp(lenptr),iparent(nnodes),
     &   mpary(mpno)
      character*32 isubname, isubname1
      logical isedge

      isubname='getedges_d'
      
c.... Get local array storage.

      lenlist=100+mpno
      call mmgetblk('invmpary',isubname,ipinvmpary,nnodes,1,icscode)
      call mmgetblk('iedges_first_',isubname,ipiedges_first_,nnodes,1
     &   ,icscode)
      call mmgetblk('iedges_',isubname,ipiedges_,
     &   lenlist,1,icscode)
      call mmgetblk('link',isubname,iplink,lenlist,1,icscode)
      
c.... Compute the inverse mass point array map, and initialize all
c.... the linked lists to 'empty'.

      do i=1,nnodes
         invmpary(i)=0
         iedges_first_(i)=0
      enddo

c.... For parent nodes in the mass point array we define the
c.... inverse map.

      do k=1,mpno
         if (iparent(mpary(k)).eq.mpary(k)) then
            invmpary(mpary(k))=k
         endif
      enddo
      
c.... Loop thru all elements, and for each local edge with endpoints
c.... being parent mass points, put the larger index parent in the
c.... linked list for the lower index parent.  (If we put each node
c.... in the other node's linked list, then we would compute DOUBLE
c.... the number of edges.)

      next=1
      do i=1,nelements
         ityp=itettyp(i)
         do j=1,nelmnee(ityp)
            ic1=itet(ielmedge1(1,j,ityp)+itetoff(i))
            ic2=itet(ielmedge1(2,j,ityp)+itetoff(i))
            ip1=iparent(ic1)
            ip2=iparent(ic2)
            maxpar=max(ip1,ip2)
            minpar=min(ip1,ip2)
            if(inclusive.eq.0) then
               isedge = invmpary(maxpar).ne.0.and.invmpary(minpar).ne.0
            else
               isedge =invmpary(maxpar).ne.0.or.invmpary(minpar).ne.0
            endif
            if (isedge) then
               inc=mpno+100
               call insertll(iedges_first_,ipiedges_,iplink,lenlist,inc,
     &            minpar,maxpar,next)
               call insertll(iedges_first_,ipiedges_,iplink,lenlist,inc,
     &            maxpar,minpar,next)
            endif
         enddo
      enddo

c.... Increment storage in IEDGES if necessary.

      call mmblklen('iedges',isubname1,ipiedges,lenorig,icscode)
      if (lenorig.lt.next) then
         if (icscode.eq.0) then
            inc=next-lenorig
            call mmincblk('iedges',isubname1,ipiedges,inc,icscode)
         else
            call mmgetblk('iedges',isubname1,ipiedges,next,1,icscode)
         endif
      endif

c.... Compress all the individual linked lists into the output arrays.

      call orderll(iedges_first_,iedges_,link,nnodes,iedges_first,iedges
     &   )

      call mmrelprt(isubname,icscode)
      return
      end
