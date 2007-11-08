*dk,get_elements_around_node
      subroutine get_elements_around_node(itin,ipin,nelts,ipelts,
     *   itetoff,jtetoff,itet1,jtet1,itettyp,iparent,
     *   nef,mbndry)
C
C #####################################################################
C
C     PURPOSE -
C
C        This routine finds all the elements that contain a given
C        edge.  The seed element is it and the designated node is
C        local node ipin of element it.  The elements(including the seed)
C        are returned in elts.
C        (pointed to by ipelts)
C
C     INPUT ARGUMENTS -
C
C       itin   -- seed element
C       ipin   -- local (not global) node number of node to be tracked around (parent node)
C       (ip)elts - Pointer supplied as input - array filled on exit
C       itetoff,jtetoff,itet1,jtet1,itettyp - cmo connectivity vectors
C       ipitetoff,ipjtetoff,ipitet,ipjtet,ipitettyp - cmo connectivity pointers
C       (ip)iparent - Pointer to array of parent points of all nodes
C       nef - number of element faces for this cmo
C       mbndry - boundary flag value
C
C     OUTPUT ARGUMENTS -
C        nelts - number of elements containing the node
C        (ip)elts - elements containing node
C
C     CHANGE HISTORY -
C
C        $Log:   /pvcs.config/t3d/src/get_elements_around_node.f_a  $
CPVCS    
CPVCS       Rev 1.7   30 Oct 2006 13:55:02   gable
CPVCS    Minor changes to IO format and clean up of comments.
CPVCS    
CPVCS       Rev 1.6   15 Apr 2003 17:13:50   dcg
CPVCS    corrected comments
CPVCS
CPVCS       Rev 1.5   02 Feb 2000 20:48:24   jtg
CPVCS    should now work for mbndry=0, and also I think it works for
CPVCS    jtet loops without modification
CPVCS
CPVCS       Rev 1.4   Fri Sep 25 11:45:40 1998   dcg
CPVCS    supply missing argument in mmincblk call
CPVCS
CPVCS       Rev 1.3   Wed Jun 03 11:29:14 1998   dcg
CPVCS    fix tet specific code to become hybrid code
CPVCS
CPVCS       Rev 1.2   Sat May 23 23:30:28 1998   kuprat
CPVCS    Fixed typo so that version logging works now.
C ######################################################################
C
      implicit none
C
      character*132 logmess
C
      include "local_element.h"
C
C ######################################################################
C
C
      integer nelts,itin,ipin
      integer itetoff(*), jtetoff(*),itettyp(*)
C
      integer itet1(*), jtet1(*)
      integer iparent(*)
      pointer (ipstack,stack)
      integer stack(*)
      pointer (ipelts, elts)
      integer elts(*)
C
      integer ilen,icscode,ierror,i,ip1,nef,
     *        i1,itt,iff,j,mbndry,numinc,
     *        ittyp,nf,lenstk,lstack,stkptr,jt,nf1
      character*32 isubname,blkelts,prtelts
C
C#######################################################################
C
C
      isubname='get_elements_around_node'
      numinc=100
      call mmgetlen(ipelts,ilen,ierror)
      if(ierror.ne.0) go to 19
      call mmgetnam(ipelts,blkelts,prtelts,ierror)
      if(ierror.ne.0) go to 19
 
C
C  create a stack of elements that contain the node
C  the push operation adds an element to the stack
C  the pop operation takes it off
C  after a pop the element is written to the result list
C  and we check all neighbors of the popped element  --
C  if the neigbor element contains the node and
C  is neither already on the stack or on the result list
C  it is pushed on the stack
C
C  make a temp array for the stack
      lenstk=100
      call mmgetblk('stack',isubname,ipstack,lenstk,1,ierror)
      if(ierror.ne.0) go to 19
C
      lstack=0
      nelts = 0
      call epush(itin,lstack,stack)
      stkptr=1
      ip1=iparent(itet1(itetoff(itin)+ipin))
5     call epop(itt,stkptr,stack)
      nelts = nelts+1
      if(nelts.gt.ilen)
     *          call mmincblk(blkelts,prtelts,ipelts,numinc,ierror)
      elts(nelts)=itt
      ittyp=itettyp(itt)
      nf1=1
      do nf =nf1,nelmnef(ittyp)
          do j=1,ielmface0(nf,ittyp)
             i1=itet1(itetoff(itt)+ielmface1(j,nf,ittyp))
             i1=iparent(i1)
C
C  see if this face contains the requested node
C
             if(i1.eq.ip1) then
               iff=nf
C
C  check if interface face
C
               jt=abs(jtet1(jtetoff(itt)+iff))
               if(jt.eq.mbndry) go to 15
               if(jt.gt.mbndry) jt=jt-mbndry
               jt=1+(jt-1)/nef
C
C  see if this tet has been written or if it is already in the
C  stack - if so go to next face neighbor
C
               do i=1,nelts
                 if(elts(i).eq.jt) go to 15
               enddo
               do i=1,lstack
                 if(stack(i).eq.jt) go to 15
               enddo
C
C  add it to the stack
C
               if(lstack+1.gt.lenstk)
     *            call mmincblk('stack',isubname,ipstack,numinc,ierror)
               call epush(jt,lstack,stack)
               go to 15
             endif
C end loop on nodes
          enddo
C end loop on faces
 15   continue
      enddo
      if(stkptr.le.lstack) go to 5
      go to 9999
C  shouldn't ever get here
C
  19  write(logmess, 20) itin,ipin
  20  format (' ATTENTION:',
     *        ' error in get_elements_around_node for element',
     *         i10, ' node ',i10 )
      call writloga('default',0,logmess,0,icscode)
C
9999  continue
      call mmrelblk('stack',isubname,ipstack,ierror)
      return
      end
 
C|C|C|C|C|C|C|C|C|C|C|C|C|C|C|C|C|C|C|C|C|C|C|C|C|C|C|C|C|C|C|C|C|C|
 
      subroutine epush(it,lstack,stack)
      implicit none
      integer it,lstack,stack(*)
      lstack=lstack+1
      stack(lstack)=it
      return
      end
 
C|C|C|C|C|C|C|C|C|C|C|C|C|C|C|C|C|C|C|C|C|C|C|C|C|C|C|C|C|C|C|C|C|C|
 
      subroutine epop(it,stkptr,stack)
      implicit none
      integer it,stkptr,stack(*)
      it=stack(stkptr)
      stkptr=stkptr+1
      return
      end
 
C|C|C|C|C|C|C|C|C|C|C|C|C|C|C|C|C|C|C|C|C|C|C|C|C|C|C|C|C|C|C|C|C|C|
