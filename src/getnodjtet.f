*dk getnodjtet
      subroutine getnodjtet(itet,mpary,mpno,nelements,nnodes,
     &   isubname1,ipnodjtet,njfirst)
C #####################################################################
C
C     PURPOSE -
C
C        Get the Node-JTET relation.
C
C     INPUT ARGUMENTS -
C
C        ITET      - The usual ITET array for the current mesh.
C        MPARY     - Mass point array.
C        MPNO      - Length of mass point array.
C        NELEMENTS - Number of tets in current mesh.
C        NNODES    - Number of nodes in current mesh.
C        ISUBNAME1 - Partition that output NODJTET array belongs to.
C
C     OUTPUT ARGUMENTS -
C
C        IPNODJTET - Pointer to NODJTET array which will be filled with
C                    the set of JTETs that a given node shares.
C        NJFIRST   - NJFIRST is a mapping from nodes to the NODJTET
C                    array which gives the position of the first JTET
C                    in NODJTET that corresponds to a given node.
C
C     CHANGE HISTORY -
C$Log: getnodjtet.f,v $
CRevision 2.00  2007/11/05 19:45:57  spchu
CImport to CVS
C
CPVCS    
CPVCS       Rev 1.4   Fri Oct 03 11:01:48 1997   dcg
CPVCS    reorder declarations as per DEC compiler
CPVCS    
CPVCS       Rev 1.3   Mon Apr 14 16:49:50 1997   pvcs
CPVCS    No change.
CPVCS    
CPVCS       Rev 1.2   Fri Apr 11 11:02:22 1997   kuprat
CPVCS    Replaced MMGETLEN call with MMBLKLEN call.
CPVCS    
CPVCS       Rev 1.1   11/07/95 17:18:06   dcg
CPVCS    change flag to 2 in mmgetblk calls
CPVCS    
CPVCS       Rev 1.0   02/15/95 13:37:34   dcg
CPVCS    Original version
C
C ######################################################################
 
c    Get the 'node-jtet relation':
c    Given a node I in the mass point array MPARY, we list all the
c    JTET values that correspond to node I.  That is, if tet JT has one of
c    its four nodes equal to node I, and if the local node number for that
c    node is JI, then we will provide a JTET value such that
c
c    JT=1+(JTET-1)/4  (integer divide) and
c    JI=JTET-4*(JT-1).
c
c    The relation actually consists of two arrays, NJFIRST and NODJTET.
c    For node I=MPARY(K), the first JTET value occurs at position NJFIRST(K)
c    in NODJTET, and the last one occurs at NJFIRST(K+1)-1.  (Hence NJFIRST
c    must be dimensioned to have length at least MPNO+1.)
 
      implicit none 

      integer lenptr
      parameter (lenptr=1000000)

      pointer (ipnodjtet,nodjtet),(ipinvmpary,invmpary),
     &   (ipnjfirst_,njfirst_),(ipnodjtet_,nodjtet_),(iplink,link)
      integer mpno,nelements,nnodes,lenlist,icscode,i,k,next,j,jteti,
     &   lenorig,inc
       integer nodjtet(lenptr),njfirst(mpno+1),itet(4,nelements),
     &   mpary(mpno),invmpary(nnodes),njfirst_(mpno),
     &   nodjtet_(lenptr),link(lenptr)
      character*32 isubname, isubname1

      isubname='getnodjtet'
 
c.... Get local array storage.

      lenlist=mpno
      call mmgetblk('invmpary',isubname,ipinvmpary,nnodes,1,icscode)
      call mmgetblk('njfirst_',isubname,ipnjfirst_,mpno,1,icscode)
      call mmgetblk('nodjtet_',isubname,ipnodjtet_,
     &   lenlist,1,icscode)
      call mmgetblk('link',isubname,iplink,lenlist,1,icscode)
 
c.... Compute the inverse mass point array map, and initialize all
c.... the linked lists to 'empty'.

      do i=1,nnodes
         invmpary(i)=0
      enddo
      do k=1,mpno
         invmpary(mpary(k))=k
         njfirst_(k)=0
      enddo
 
c.... Loop thru all elements, and for each node it contains, determine
c.... if it is in the mass point array.  If so add this element to
c.... the linked list for that mass point.

      next=1
      do i=1,nelements
         do j=1,4
            k=invmpary(itet(j,i))
            if (k.ne.0) then
               jteti=4*(i-1)+j
               call insertll(njfirst_,ipnodjtet_,iplink,lenlist,mpno,
     &            k,jteti,next)
            endif
         enddo
      enddo
 
c.... Increment storage in NODJTET if necessary.

      call mmblklen('nodjtet',isubname1,ipnodjtet,lenorig,icscode)
      if (lenorig.lt.next) then
         if (icscode.eq.0) then
            inc=next-lenorig
            call mmincblk('nodjtet',isubname1,ipnodjtet,inc,icscode)
         else
            call mmgetblk('nodjtet',isubname1,ipnodjtet,next,1,icscode)
         endif
      endif

c.... Compress all the individual linked lists into NJFIRST/NODJTET.

      call orderll(njfirst_,nodjtet_,link,mpno,njfirst,nodjtet)

      call mmrelprt(isubname,icscode)
      return
      end
