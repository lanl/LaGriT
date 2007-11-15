*dk,getnodhyb
      subroutine getnodhyb(mpno,mpary,ieltno,ieltary,nnodes,itet,
     &   itetoff,itettyp,iparent,invmpary,isubname1,ipnodhyb,
     &   ipnodhyboff)
C #####################################################################
C
C     PURPOSE -
C
C        GETNODHYB gets the node-hyb relation for a 3D mesh.
C          For a given node, the node-hyb relation
C          is a list of numbers that give the hybrid elements that the
C          the node belongs to AND the local node number within each element.
C
C     INPUT ARGUMENTS -
C
C         mpno      - number of nodes for which we need relation.
C         mpary     - array of nodes needing relation.
C         ieltno    - number of elements involved in this calculation.
C         ieltary   - array of relevant elements from which we will
C                     derive the node-edge relation.
C         nnodes    - total number of nodes in mesh.
C         itet      - array containing element-node relation for
C                     whole mesh.
C         itetoff   - offset array for itet.
C         itettyp   - array giving element types for all elements
C                     in the mesh.
C         iparent   - array giving the parent of each node.
C         invmpary  - inverse array of MPARY.
C         isubname1 - name of partition which contains the output
C                     arrays.
C
C     INPUT/OUTPUT ARGUMENTS -
C
C         ipnodhyb  - Pointer to NODHYB array which gives, for each node,
C                     a list of elements that the node touches, encoded
C                     with local node numbering information.
C         ipnodhyboff   - Pointer to NODHYBOFF array giving offsets into
C                         NODHYB array.
C
C     CHANGE HISTORY -
C
C        $Log: getnodhyb.f,v $
C        Revision 2.00  2007/11/05 19:45:57  spchu
C        Import to CVS
C
CPVCS    
CPVCS       Rev 1.3   24 Feb 2000 11:20:36   jtg
CPVCS    numlocnod count was incorrect if qud, lin, or pnt elements
CPVCS    occurred in the mesh. This has beend fixed.
CPVCS    
CPVCS       Rev 1.2   Thu Sep 24 15:52:48 1998   kuprat
CPVCS    Fixed bug where NTRI was not initialized to zero.
CPVCS    
CPVCS       Rev 1.1   Fri Aug 21 16:46:48 1998   dcg
CPVCS    make changes to allow for 2d massage
CPVCS
CPVCS       Rev 1.0   Wed Oct 29 16:59:12 1997   kuprat
CPVCS    Initial revision.
C
c#######################################################################
c     Get nod-hyb relation.  Thus for node NODE=MPARY(K), we have
c     that NODHYBOFF(K) points into a set of values in NODHYB, such that for
c     each hybrid element IHYB=IELTARY(I) (with node NODE matching with
c     the J'th local node in element IHYB), we have a value
c     J+MAXNEN*I.  This method of encoding two pieces of information
c     is patterned after 'JTET' encoding--MAXNEN is the maximum number
c     of local vertices that an element can have, assuring unambiguous
c     encoding.
c#######################################################################
 
      implicit none
      include 'local_element.h'
      include 'consts.h'
      include 'smooth.h'
 
      integer lenptr
      parameter (lenptr=1000000)
 
      integer mpno,mpary(lenptr),ieltno,ieltary(lenptr),nnodes,
     &   itet(lenptr),itetoff(lenptr),itettyp(lenptr),
     &   iparent(lenptr),invmpary(lenptr)
 
      pointer (ipnodhyb1,nodhyb1), (iplinknodhyb,linknodhyb),
     &   (ipifirstnodhyb,ifirstnodhyb), (ipnodhyb,nodhyb),
     &   (ipnodhyboff,nodhyboff)
 
      integer nodhyb1(lenptr),linknodhyb(lenptr),
     &   ifirstnodhyb(lenptr),nodhyb(lenptr),nodhyboff(lenptr)
 
      integer icscode,i,next,j,inc,ind,lenorig,ihyb,
     &   ii,numlocnod,nextnodhyb,node
 
      character*32 isubname,  isubname1
 
      isubname='getnodhyb'
 
c.... Compute the total number of 'local nodes' in the IELTARY
c.... array of IELTNO elements.  That is, for each element count
c....  the number of nodes with non-zero invmpary.
 
      numlocnod=0
      do i=1,ieltno
         ihyb=ieltary(i)
         do j=1,nelmnen(itettyp(ihyb))
            node=itet(j + itetoff(ihyb))
            ii=invmpary(iparent(node))
            if (ii.ne.0) numlocnod=numlocnod+1
         enddo
      enddo
 
c.... Allocate temporary storage for linked lists needed to derive
c.... this topological relation.  The amount of storage needed is
c.... NUMLOCNOD (total no. of data items) for the data and link
c.... array, and is MPNO (size of index set) for the IFIRSTNODHYB
c.... array which points into the data array.
 
      call mmgetblk('nodhyb1',isubname,ipnodhyb1,numlocnod,1,icscode)
      call mmgetblk('linknodhyb',isubname,iplinknodhyb,numlocnod,1
     &   ,icscode)
      call mmgetblk('ifirstnodhyb',isubname,ipifirstnodhyb,mpno,1
     &   ,icscode)
 
      do i=1,mpno
         ifirstnodhyb(i)=0
      enddo
      nextnodhyb=1
 
c.... Loop over array of elements and insert local nodes into
c.... linked lists.
 
      do i=1,ieltno
         ihyb=ieltary(i)
         do j=1,nelmnen(itettyp(ihyb))
 
            node=itet(j + itetoff(ihyb))
            ii=invmpary(iparent(node))
 
            if (ii.ne.0) then
               if (ifirstnodhyb(ii).eq.0) then
                  ifirstnodhyb(ii)=nextnodhyb
               else
                  ind=ifirstnodhyb(ii)
                  do while(linknodhyb(ind).ne.0)
                     ind=linknodhyb(ind)
                  enddo
                  linknodhyb(ind)=nextnodhyb
               endif
               nodhyb1(nextnodhyb)=j+maxnen*(i-1)
               linknodhyb(nextnodhyb)=0
               nextnodhyb=nextnodhyb+1
            endif
         enddo
      enddo
 
c.... Allocate memory if necessary for data arrays which
c.... will reside in the ISUBNAME1 partition.
 
      call mmblklen('nodhyb',isubname1,ipnodhyb,lenorig,icscode)
      if (numlocnod.gt.lenorig) then
         if (icscode.eq.0) then
            inc=numlocnod-lenorig
            call mmincblk('nodhyb',isubname1,ipnodhyb,inc,icscode)
         else
            call mmgetblk('nodhyb',isubname1,ipnodhyb,numlocnod,1
     &         ,icscode)
         endif
      endif
      call mmblklen('nodhyboff',isubname1,ipnodhyboff,lenorig,icscode)
      if (mpno+1.gt.lenorig) then
         if (icscode.eq.0) then
            inc=mpno+1-lenorig
            call mmincblk('nodhyboff',isubname1,ipnodhyboff,inc,icscode)
         else
            call mmgetblk('nodhyboff',isubname1,ipnodhyboff,mpno+1,1
     &         ,icscode)
         endif
      endif
 
c.... Loop through linked lists and write them out in a sequential
c.... manner into the target arrays which hold the output
c.... topological relation.
 
      next=1
      do ii=1,mpno
         nodhyboff(ii)=next-1
         ind=ifirstnodhyb(ii)
         do while (ind.ne.0)
            nodhyb(next)=nodhyb1(ind)
            ind=linknodhyb(ind)
            next=next+1
         enddo
      enddo
      nodhyboff(mpno+1)=next-1
 
c.... Release temporary memory.
 
      call mmrelprt(isubname,icscode)
      return
      end
 
