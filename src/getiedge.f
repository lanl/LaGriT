*dk,getiedge
      subroutine getiedge(mpno,mpary,ieltno,ieltary,nnodes,itet,itetoff,
     &   itetclr,itettyp,isubname1,ipiedge,ipiedgeoff,ipiedgemat)
C #####################################################################
C
C     PURPOSE -
C
C        GETIEDGE gets the node-edge relation for a 3D mesh.
C
C     INPUT ARGUMENTS -
C
C         mpno      - number of nodes for which we need relation.
C         mpary     - array of nodes needing relation.
C         ieltno    - number of tets involved in this calculation.
C         ieltary   - array of relevant tets from which we will 
C                     derive the node-edge relation.
C         nnodes    - total number of nodes in mesh.
C         itet      - array containing element-node relation for
C                     whole mesh.
C         itetoff   - offset array for itet.
C         itetclr   - array giving material numbers for all the
C                     elements in the mesh.
C         itettyp   - array giving element types for all elements
C                     in the mesh.
C         isubname1 - name of partition which contains the output
C                     arrays.
C
C     INPUT/OUTPUT ARGUMENTS -
C         
C         ipiedgeoff  - Pointer to IEDGEOFF array which gives, for each
C                       node, the offset into the IEDGE array where the edges
C                       for that node begin.
C         ipiedge     - Pointer to IEDGE array which gives all the node edges.
C         ipiedgemat  - Pointer to IEDGEMAT array which gives the material
C                       numbers corresponding to the edges in IEDGE.
C
C     CHANGE HISTORY -
C
C        $Log:   /pvcs.config/t3d/src/getiedge.f_a  $
CPVCS    
CPVCS       Rev 1.6   06 Nov 2001 15:41:22   kuprat
CPVCS    We now distinguish between coincident but differently
CPVCS    colored edges.
CPVCS    
CPVCS       Rev 1.5   Mon Apr 14 16:49:48 1997   pvcs
CPVCS    No change.
CPVCS    
CPVCS       Rev 1.4   Thu Nov 21 09:41:00 1996   kuprat
CPVCS    Changed MMGETLEN calls to MMBLKLEN calls.
CPVCS    
CPVCS       Rev 1.3   Wed Oct 30 16:48:26 1996   kuprat
CPVCS    Large revision.  We now generate edges that occur in the
CPVCS    IELTARY list of (possibly hybrid) elements.
CPVCS    
CPVCS       Rev 1.2   11/07/95 17:18:04   dcg
CPVCS    change flag to 2 in mmgetblk calls
CPVCS    
CPVCS       Rev 1.1   10/13/95 00:51:16   kuprat
CPVCS    Now we pass ITET instead of IPITET
CPVCS    
CPVCS       Rev 1.0   08/24/95 16:38:52   kuprat
CPVCS    Initial revision.
C
c#######################################################################
c     Get all edges in a 3d tet mesh.  That is, get all ordered
c     node pairs (a,b) where a is a neighbour of b, AND
c     a<b.  (Note: if we drop the last condition, then we would
c     get DOUBLE the number of edges in the mesh.)
c#######################################################################

      implicit none
      include 'local_element.h'
      include 'consts.h'
      include 'smooth.h'

      integer lenptr
      parameter (lenptr=1000000)

      integer mpno,mpary(lenptr),ieltno,ieltary(lenptr),nnodes,
     &   itet(lenptr),itetoff(lenptr),itetclr(lenptr),
     &   itettyp(lenptr)

      pointer (ipiedgeoff,iedgeoff), (ipiedge,iedge), 
     &   (ipifirst1,ifirst1), (ipiedge1,iedge1), (iplink,link),
     &   (ipiedgemat1,iedgemat1), (ipiedgemat,iedgemat)
      integer iedgeoff(lenptr), iedge(lenptr), 
     &   ifirst1(lenptr), iedge1(lenptr), link(lenptr),
     &   iedgemat1(lenptr), iedgemat(lenptr)

      integer length,icscode,i,next,k,j,ivi,ivj,itemp,
     &   inc,ind,lenorig,ihyb

      character*32 isubname, isubname1
      
      isubname='getiedge'

c.... Get initial storage for temporary arrays used in compiling
c.... the topological relation as a set of linked lists.
c.... IFIRST1, for each node, points to the beginning of each linked
c.... list.  If the I'th list is empty, IFIRST(I)=0.
c.... LINK contains the links.  (We make an initial (low) estimate
c.... that the total number of edges emanating from MPNO mass points
c.... is MPNO.  As more storage is needed, it is assigned in blocks
c.... of length MPNO.  Since MPNO could be very small, we make
c.... sure that memory is incremented by at least 1000.)
c.... IEDGE1, IEDGEMAT1 hold the data fields.

      length=max(mpno,1000)
      call mmgetblk('iedge1',isubname,ipiedge1,length,1,icscode)
      call mmgetblk('iedgemat1',isubname,ipiedgemat1,length,1,icscode)
      call mmgetblk('link',isubname,iplink,length,1,icscode)

      call mmgetblk('ifirst1',isubname,ipifirst1,nnodes,1,icscode)

      do i=1,nnodes
         ifirst1(i)=0
      enddo
      next=1

c.... Loop through all (possibly hybrid) elements in IELTARY.

      do i=1,ieltno
         ihyb=ieltary(i)

c.... Loop through all virtual tetrahedra corresponding to this type
c.... of element.

         do j=1,ihybnumtetv(itettyp(ihyb))

c.... Loop through the six edges of this virtual tetrahedron.

            do 20 k=1,6
               ivi=itet(itetv(ielmedge1(1,k,ifelmtet),j,itettyp(ihyb))
     &            +itetoff(ihyb))
               ivj=itet(itetv(ielmedge1(2,k,ifelmtet),j,itettyp(ihyb))
     &            +itetoff(ihyb))

c.... Make sure IVI < IVJ, to eliminate double counting of edges.

               if (ivi.gt.ivj) then
                  itemp=ivj
                  ivj=ivi
                  ivi=itemp
               endif

c.... If memory length might be exceeded by addition of another edge,
c.... allocate more.

               if (length.le.next) then
                  inc=max(mpno,1000)
                  length=length+inc
                  call mmincblk('iedge1',isubname,ipiedge1,
     &               inc,icscode)
                  call mmincblk('iedgemat1',isubname,ipiedgemat1,
     &               inc,icscode)
                  call mmincblk('link',isubname,iplink,inc,icscode)
               endif

c.... If IFIRST1(IVI) is 0, a new linked list is started for IVI,
c.... with first datum IVJ and tet color of the element.

               if (ifirst1(ivi).eq.0) then
                  ifirst1(ivi)=next
                  iedge1(next)=ivj
                  iedgemat1(next)=itetclr(ihyb)
                  link(next)=0
                  next=next+1
               else

c.... Follow links until we either find IVJ (in which case the
c.... datum is already in the list), or we find the end of the
c.... list, in which case we add IVJ to the list.

                  ind=ifirst1(ivi)
 10               if ((iedge1(ind).eq.ivj).and.(iedgemat1(ind).eq
     &               .itetclr(ihyb))) goto 20
                  if (link(ind).ne.0) then
                     ind=link(ind)
                     goto 10
                  endif
                  link(ind)=next
                  iedge1(next)=ivj
                  iedgemat1(next)=itetclr(ihyb)
                  link(next)=0
                  next=next+1
               endif
 20         continue
         enddo
      enddo

c.... Convert temporary arrays containing linked lists to 
c.... packed arrays in the ISUBNAME1 partition.


c.... For each array in ISUBNAME1, check if it has sufficient
c.... length or even exists.  If not, fix the problem.

      call mmblklen('iedge',isubname1,ipiedge,lenorig,icscode)
      if (length.gt.lenorig) then
         if (icscode.eq.0) then
            inc=length-lenorig
            call mmincblk('iedge',isubname1,ipiedge,inc,icscode)
         else
            call mmgetblk('iedge',isubname1,ipiedge,length,1,icscode)
         endif      
      endif

      call mmblklen('iedgemat',isubname1,ipiedgemat,lenorig,icscode)
      if (length.gt.lenorig) then
         if (icscode.eq.0) then
            inc=length-lenorig
            call mmincblk('iedgemat',isubname1,ipiedgemat,inc,icscode)
         else
            call mmgetblk('iedgemat',isubname1,ipiedgemat,length,1
     &         ,icscode)
         endif      
      endif

      call mmblklen('iedgeoff',isubname1,ipiedgeoff,lenorig,icscode)
      if (nnodes+1.gt.lenorig) then
         if (icscode.eq.0) then
            inc=nnodes+1-lenorig
            call mmincblk('iedgeoff',isubname1,ipiedgeoff,inc,icscode)
         else
            call mmgetblk('iedgeoff',isubname1,ipiedgeoff,nnodes+1,1
     &         ,icscode)
         endif      
      endif

c.... Dump out all NNODES linked lists into compressed arrays.

      next=1
      do i=1,nnodes
         iedgeoff(i)=next-1
         if (ifirst1(i).ne.0) then
            ind=ifirst1(i)
            iedge(next)=iedge1(ind)
            iedgemat(next)=iedgemat1(ind)
            next=next+1
            do while (link(ind).ne.0)
               ind=link(ind)
               iedge(next)=iedge1(ind)
               iedgemat(next)=iedgemat1(ind)
               next=next+1
            enddo
         endif
      enddo
      iedgeoff(nnodes+1)=next-1

c.... Release temporary arrays.

      call mmrelprt(isubname,icscode)
      return
      end

