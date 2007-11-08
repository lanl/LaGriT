      subroutine get_materials_around_node(itin,ipin,nmtrls,ipmtrls,
     *   lenmtrl,ipstack,lenstk,
     *   itetoff,jtetoff,itet1,jtet1,itettyp,itetclr,iparent,
     *   nef,mbndry)
C
C #####################################################################
C
C     PURPOSE -
C
C        This routine finds all the material that surround a given
C        node.  The seed element is it and the designated node is
C        local node ipin of element it.  The materials
C        are returned in mtrls.
C
C     INPUT ARGUMENTS -
C
C       itin   -- seed element
C       ipin   -- local node # to be tracked around 
C       (ip)mtlrs - Pointer supplied as input - array filled on exit
C       lenmtrl -- length of mtlrs array
C       (ip)stack  -- Pointer to stack work array
C       lenstk -- length of stack array
C       itetoff,jtetoff,itet,jtet,itettyp,itetclr - cmo attributes
C       iparent - Array of parent points of all nodes
C       nef - number of element faces for this cmo
C       mbndry - value signifying boundary or interface face
C
C     OUTPUT ARGUMENTS -
C        nmtrls - number of materials surrounding the node
C        (ip)mtrls - materials surrounding the node
C
C     CHANGE HISTORY -
C
C$Log: get_materials_around_node.f,v $
CRevision 2.00  2007/11/05 19:45:56  spchu
CImport to CVS
C
CPVCS    
CPVCS       Rev 1.1   Thu Jan 14 09:27:04 1999   dcg
CPVCS    move mmget calls outside subroutine for
CPVCS    efficiency considerations
CPVCS    
CPVCS       Rev 1.0   Wed Jan 13 08:57:08 1999   dcg
CPVCS    Initial revision.
c
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
      integer nmtrls,itin,ipin
      pointer (ipmtrls, mtrls)
      integer mtrls(*)
      integer  itetoff(*), jtetoff(*),itettyp(*),itetclr(*)
C
c      pointer (ipitet, itet1)
c      pointer (ipjtet, jtet1)
      integer itet1(*), jtet1(*)
c      pointer (ipiparent, iparent)
      integer iparent(*)
      pointer (ipstack,stack)
      integer stack(*)
C
      integer icscode,ierror,i,ip1,nef,
     *        i1,itt,iff,j,mbndry,numinc,lenmtrl,
     *        ittyp,nf,lenstk,lstack,stkptr,jt,nf1
      character*32 isubname,blkelts,prtelts
C
C#######################################################################
C
C
      isubname='get_materials_around_node'
      numinc=100
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
      lstack=0
      nmtrls = 0
C push on stack
      lstack=lstack+1
      stack(lstack)=itin
      stkptr=1
      ip1=iparent(itet1(itetoff(itin)+ipin))
c pop from stack
5     itt=stack(stkptr)
      stkptr=stkptr+1
      do i=1,nmtrls
         if(mtrls(i).eq.itetclr(itt)) go to 7
      enddo
      nmtrls = nmtrls+1
      if(nmtrls.gt.lenmtrl) then
         call mmgetnam(ipmtrls,blkelts,prtelts,ierror)
         call mmincblk(blkelts,prtelts,ipmtrls,numinc,ierror)
         lenmtrl=lenmtrl+numinc
      endif
      mtrls(nmtrls)=itetclr(itt)
7     ittyp=itettyp(itt)
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
               jt=jtet1(jtetoff(itt)+iff)
               if(jt.eq.mbndry) go to 15
               if(jt.gt.mbndry) jt=jt-mbndry
               jt=1+(jt-1)/nef
C
C  see if this tet has been written or if it is already in the
C  stack - if so go to next face neighbor
C
               do i=1,lstack
                 if(stack(i).eq.jt) go to 15
               enddo
C
C push onto the stack
C
               if(lstack+1.gt.lenstk) then
                  call mmgetnam(ipstack,blkelts,prtelts,ierror)
                  call mmincblk(blkelts,prtelts,ipstack,numinc,ierror)
                  lenstk=lenstk+numinc
               endif
               lstack=lstack+1
               stack(lstack)=jt
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
     *         i10 ' node ',i10 )
      call writloga('default',0,logmess,0,icscode)
C
9999  continue
      return
      end
