      subroutine get_elements_on_edge(itin,iein,nelts,ipelts,ipedges,
     *  ipitetoff,ipjtetoff,ipitet,ipjtet,ipitettyp,ipiparent,nef,
     *  mbndry)
C
C #####################################################################
C
C     PURPOSE -
C
C        This routine finds all the elements that contain a given
C        edge.  The seed element is it and the designated edge is
C        edge ie of element it.  The elements(including the seed)
C        are returned in elts, the local edge numbers in edges
C        (pointed to by ipelts and ipedges)
C
C     INPUT ARGUMENTS -
C
C       it   -- seed element
C       ie   -- local edge # of it that is edge to be traced
C       (ip)elts - Pointer supplied as input - array filled on exit
C       (ip)edges _ Pointer supplied as input - array filled on exit
C       ipitetoff,ipjtetoff,ipitet,ipjtet,ipitettyp - cmo pointers
C       (ip)iparent - Pointer to array of parent points of all nodes
C       nef - number of element faces for this cmo
C
C     OUTPUT ARGUMENTS -
C        nelts - number of elements containing the edge
C        (ip)elts - elements containing edge
C        (ip)edges _ local edge numbers
C
C     CHANGE HISTORY -
C
C        $Log: get_elements_on_edge.f,v $
C        Revision 2.00  2007/11/05 19:45:56  spchu
C        Import to CVS
C
CPVCS    
CPVCS       Rev 1.7   19 Dec 2001 13:47:58   dcg
CPVCS    add subroutine get_elements_on_edge_test
CPVCS    
CPVCS       Rev 1.6   Mon Jun 28 18:17:54 1999   jtg
CPVCS    fixed error for hybrid meshes that ittyp not reset when changing idir
CPVCS    
CPVCS       Rev 1.5   Sun Jun 27 12:37:50 1999   jtg
CPVCS    fixed error that len,len1 not increased if mmincblk called
CPVCS    and len,len1 not checked (increased) in 2D version
CPVCS    
CPVCS       Rev 1.4   Fri Sep 25 11:45:48 1998   dcg
CPVCS    supply missing argument in mmincblk call
CPVCS
CPVCS       Rev 1.3   Wed Aug 19 15:14:56 1998   dcg
CPVCS    add subroutine get_elements_on_edge2d for 2D applications
C ######################################################################
C
      implicit none
C
      include "local_element.h"

C arguments (itin,iein,nelts,ipelts,ipedges,
C  ipitetoff,ipjtetoff,ipitet,ipjtet,ipitettyp,ipiparent,nef,mbndry)

      integer itin,iein,nelts,nef,mbndry
      pointer (ipelts, elts)
      integer elts(*)
      pointer (ipedges, edgs)
      integer edgs(*)
      pointer (ipitetoff, itetoff)
      pointer (ipjtetoff, jtetoff)
      pointer (ipitettyp, itettyp)
      integer  itetoff(*), jtetoff(*),itettyp(*)
      pointer (ipitet, itet1)
      pointer (ipjtet, jtet1)
      integer itet1(*), jtet1(*)
      pointer (ipiparent, iparent)
      integer iparent(*)

C
C ######################################################################
C
      integer ilen,icscode,ierror,iip1,iip2,
     *        ie,ip1,ip2,ilen1,
     *        i1,i2,itstart,itt,itold,itnew,
     *        iff,ifold,ifnew,j,
     *        ittyp,nf,k,ittypst,
     *        jt,idir,nfstart,nf1,numinc

      character*132 logmess
      character*32 isubname,blkelts,prtelts,blkedges,prtedges
C
C#######################################################################
C BEGIN begin
C
      isubname='get_elements_on_edge'
      numinc=100
      call mmgetlen(ipelts,ilen,ierror)
      if(ierror.ne.0) go to 19
      call mmgetnam(ipelts,blkelts,prtelts,ierror)
      if(ierror.ne.0) go to 19
      call mmgetlen(ipedges,ilen1,ierror)
      if(ierror.ne.0) go to 19
      call mmgetnam(ipedges,blkedges,prtedges,ierror)
      if(ierror.ne.0) go to 19
C
C  nodes on edge (itin,iein)
C
      iip1=itet1(itetoff(itin)+ielmedge1(1,iein,itettyp(itin)))
      iip2=itet1(itetoff(itin)+ielmedge1(2,iein,itettyp(itin)))
      ip1=iparent(iip1)
      ip2=iparent(iip2)
C
C  Loop through all neigbors of element to find one that shares
C  this edge
C
C iflag  = 0  only one material around edge
C        = 1  more than one material around edge
C iflag1 = 2  on and exterior boundary
C
      nelts = 1
      elts(1)=itin
      edgs(1)=iein
      itstart=itin
      nfstart=1
      itold=0
      ifold=0
      itt=itin
      idir=0
      ittyp=itettyp(itt)
      ittypst=ittyp
      nf1=1
 10   do nf =nf1,nelmnef(ittyp)
           do j=1,ielmface0(nf,ittyp)
             ie=ielmface2(j,nf,ittyp)
             i1=itet1(itetoff(itt)+ielmedge1(1,ie,ittyp))
             i2=itet1(itetoff(itt)+ielmedge1(2,ie,ittyp))
             i1=iparent(i1)
             i2=iparent(i2)
             if(i1.eq.ip1.and.i2.eq.ip2.or.
     *         i2.eq.ip1.and.i1.eq.ip2) then
               iff=nf
               if(itt.eq.itstart) nfstart=nf
C
C  check if external boundary face
C  make sure to check both directions for material
C  interfaces (use idir)
C
               jt=jtet1(jtetoff(itt)+iff)
               if(jt.eq.mbndry) then
                  idir=idir+1
                  if(idir.eq.2) go to 9999
                  itt=itstart
                  itold=0
                  ifold=0
                  nf1=nfstart+1
                  ittyp=ittypst
                  go to 10
               endif
               nf1=1
C
C  check if interface face - if so both edges
C  and node to be added are on an internal interface
C
               if(jt.gt.mbndry)then
                  jt=jt-mbndry
               endif
C
C  check that we didn't go backwards
C
               itnew=1+(jt-1)/nef
               ifnew=jt-nef*(itnew-1)
               if (itnew.ne.itold.or.ifnew.ne.ifold) then
C
C  see if back to starting element
C
                  if( itnew.eq.itstart) then
                      go to 9999
                  endif
C
C  found next element in chain
C  add it to list
C
                  nelts=nelts+1
                  if(nelts.gt.ilen) then
                     call mmincblk(blkelts,prtelts,ipelts,numinc,
     *                         ierror)
                     if(ierror.ne.0) go to 19
                     ilen=ilen+numinc
                  endif
                  if(nelts.gt.ilen1) then
                     call mmincblk(blkedges,prtedges,ipedges,numinc,
     *                         ierror)
                     if(ierror.ne.0) go to 19
                     ilen1=ilen1+numinc
                  endif
                  elts(nelts)=itnew
C
C  find edge for this element
C
                  ittyp=itettyp(itnew)
                  do k=1,nelmnee(ittyp)
                     i1=itet1(itetoff(itnew)+ielmedge1(1,k,ittyp))
                     i2=itet1(itetoff(itnew)+ielmedge1(2,k,ittyp))
                     i1=iparent(i1)
                     i2=iparent(i2)
                     if(i1.eq.ip1.and.i2.eq.ip2.or.
     *                i2.eq.ip1.and.i1.eq.ip2) then
                         ie=k
                         goto 30
                     endif
                  enddo
                  goto 19
30                edgs(nelts)=ie
C
C  still looking for more tets surrounding the edge
C
                  ittyp=itettyp(itnew)
                  itold=itt
                  ifold=iff
                  itt=itnew
                  go to 10
               endif
             endif
C end loop on edges
           enddo
C end loop on faces
      enddo
C
C  shouldn't ever get here
C
  19  write(logmess, 20)itin,iein
  20  format (' ATTENTION: error in get_elements_on_edge for element',
     *         i10,' edge ',i10 )
      call writloga('default',0,logmess,0,icscode)
C
9999  continue
      return
      end
c
C #####################################################################
      subroutine get_elements_on_edge_test
     *  (itin,iein,nelts,ipelts,ipfaces,ipedges,
     *  ipitetoff,ipjtetoff,ipitet,ipjtet,ipitettyp,ipiparent,nef,
     *  mbndry,flag)
C
C #####################################################################
C
C     PURPOSE -
C
C        This routine finds all the elements that contain a given
C        edge.  The seed element is it and the designated edge is
C        edge ie of element it.  The elements(including the seed)
C        are returned in elts, the local edge numbers in edges
C        (pointed to by ipelts and ipedges)
C
C     INPUT ARGUMENTS -
C
C       it        - seed element
C       ie        - local edge # of it that is edge to be traced
C       (ip)elts  - Pointer supplied as input - array filled on exit
C       (ip)edges - Pointer supplied as input - array filled on exit
C       (ip)faces - Pointer supplied as input - array filled on exit
C       ipitetoff,ipjtetoff,ipitet,ipjtet,ipitettyp - cmo pointers
C       (ip)iparent - Pointer to array of parent points of all nodes
C       nef - number of element faces for this cmo
C
C     OUTPUT ARGUMENTS -
C        nelts     - number of elements containing the edge
C        (ip)elts  - elements containing edge
C        (ip)edges - local edge numbers
C        (ip)faces - local face numbers
c        flag      - 0 means interior edge, 1 means interface edge
c                    2 means boundary edge, 3 means boundary+interface
C
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
      integer nelts,itin,iein,numinc
      pointer (ipelts, elts)
      integer elts(*)
      pointer (ipedges, edgs)
      integer edgs(*)
      pointer (ipfaces, faces)
      integer faces(*)
      pointer (ipitetoff, itetoff)
      pointer (ipjtetoff, jtetoff)
      pointer (ipitettyp, itettyp)
      integer  itetoff(*), jtetoff(*),itettyp(*)
C
      pointer (ipitet, itet1)
      pointer (ipjtet, jtet1)
      integer itet1(*), jtet1(*)
      pointer (ipiparent, iparent)
      integer iparent(*)
C
      integer ilen,icscode,ierror,iip1,iip2,
     *        ie,ip1,ip2,nef,ilen1,
     *        i1,i2,itstart,itt,itold,itnew,
     *        iff,ifold,ifnew,j,mbndry,
     *        ittyp,nf,k,ittypst,
     *        jt,idir,nfstart,nf1,flag
      character*32 isubname,blkelts,prtelts,blkedges,prtedges
C
C#######################################################################
C
C
      isubname='get_elements_on_edge'
      flag=0
      numinc=100
      call mmgetlen(ipelts,ilen,ierror)
      if(ierror.ne.0) go to 19
      call mmgetnam(ipelts,blkelts,prtelts,ierror)
      if(ierror.ne.0) go to 19
      call mmgetlen(ipedges,ilen1,ierror)
      if(ierror.ne.0) go to 19
      call mmgetnam(ipedges,blkedges,prtedges,ierror)
      if(ierror.ne.0) go to 19
C
C  nodes on edge (itin,iein)
C
      iip1=itet1(itetoff(itin)+ielmedge1(1,iein,itettyp(itin)))
      iip2=itet1(itetoff(itin)+ielmedge1(2,iein,itettyp(itin)))
      ip1=iparent(iip1)
      ip2=iparent(iip2)
C
C  Loop through all neigbors of element to find one that shares
C  this edge
C
C iflag  = 0  only one material around edge
C        = 1  more than one material around edge
C iflag1 = 2  on and exterior boundary
C
      nelts = 1
      elts(1)=itin
      edgs(1)=iein
      itstart=itin
      nfstart=1
      itold=0
      ifold=0
      itt=itin
      idir=0
      ittyp=itettyp(itt)
      ittypst=ittyp
      nf1=1
 10   do nf =nf1,nelmnef(ittyp)
           do j=1,ielmface0(nf,ittyp)
             ie=ielmface2(j,nf,ittyp)
             i1=itet1(itetoff(itt)+ielmedge1(1,ie,ittyp))
             i2=itet1(itetoff(itt)+ielmedge1(2,ie,ittyp))
             i1=iparent(i1)
             i2=iparent(i2)
             if(i1.eq.ip1.and.i2.eq.ip2.or.
     *         i2.eq.ip1.and.i1.eq.ip2) then
               iff=nf
               if(itt.eq.itstart) then
                  nfstart=nf
                  faces(1)=nf
               endif
C
C  check if external boundary face
C  make sure to check both directions for material
C  interfaces (use idir)
C
               jt=jtet1(jtetoff(itt)+iff)
               if(jt.eq.mbndry) then
                  idir=idir+1
                  if(idir.eq.2) go to 9999
                  itt=itstart
                  itold=0
                  ifold=0
                  nf1=nfstart+1
                  ittyp=ittypst
                  if(flag.eq.0) then
                     flag=2
                  elseif(flag.eq.1) then
                     flag=3
                  endif
                  go to 10
               endif
               nf1=1
C
C  check if interface face - if so both edges
C  and node to be added are on an internal interface
C
               if(jt.gt.mbndry)then
                  if(flag.eq.0) then
                     flag=1
                  elseif(flag.eq.2) then
                     flag=3
                  endif
                  jt=jt-mbndry
               endif
C
C  check that we didn't go backwards
C
               itnew=1+(jt-1)/nef
               ifnew=jt-nef*(itnew-1)
               if (itnew.ne.itold.or.ifnew.ne.ifold) then
C
C  see if back to starting element
C
                  if( itnew.eq.itstart) then
                      go to 9999
                  endif
C
C  found next element in chain
C  add it to list
C
                  nelts=nelts+1
                  if(nelts.gt.ilen) then
                     call mmincblk(blkelts,prtelts,ipelts,numinc,
     *                         ierror)
                     if(ierror.ne.0) go to 19
                     ilen=ilen+numinc
                  endif
                  if(nelts.gt.ilen1) then
                     call mmincblk(blkedges,prtedges,ipedges,numinc,
     *                         ierror)
                     if(ierror.ne.0) go to 19
                     ilen1=ilen1+numinc
                  endif
                  elts(nelts)=itnew
                  faces(nelts)=ifnew
C
C  find edge for this element
C
                  ittyp=itettyp(itnew)
                  do k=1,nelmnee(ittyp)
                     i1=itet1(itetoff(itnew)+ielmedge1(1,k,ittyp))
                     i2=itet1(itetoff(itnew)+ielmedge1(2,k,ittyp))
                     i1=iparent(i1)
                     i2=iparent(i2)
                     if(i1.eq.ip1.and.i2.eq.ip2.or.
     *                i2.eq.ip1.and.i1.eq.ip2) then
                         ie=k
                         goto 30
                     endif
                  enddo
                  goto 19
30                edgs(nelts)=ie
C
C  still looking for more tets surrounding the edge
C
                  ittyp=itettyp(itnew)
                  itold=itt
                  ifold=iff
                  itt=itnew
                  go to 10
               endif
             endif
C end loop on edges
           enddo
C end loop on faces
      enddo
C
C  shouldn't ever get here
C
  19  write(logmess, 20)itin,iein
  20  format (' ATTENTION: error in get_elements_on_edge for element',
     *         i10,' edge ',i10 )
      call writloga('default',0,logmess,0,icscode)
C
9999  continue
      return
      end
C #####################################################################

C #####################################################################
*dk,get_elements_on_edge
      subroutine get_elements_on_edge2d(itin,iein,nelts,ipelts,ipedges,
     *  ipitetoff,ipjtetoff,ipitet,ipjtet,ipitettyp,ipiparent,nef,
     *  mbndry)
C
C #####################################################################
C
C     PURPOSE -
C
C        This routine finds all the elements that contain a given
C        edge.  The seed element is it and the designated edge is
C        edge ie of element it.  The elements(including the seed)
C        are returned in elts, the local edge numbers in edges
C        (pointed to by ipelts and ipedges)
C
C     INPUT ARGUMENTS -
C
C       it   -- seed element
C       ie   -- local edge # of it that is edge to be traced
C       (ip)elts - Pointer supplied as input - array filled on exit
C       (ip)edges _ Pointer supplied as input - array filled on exit
C       ipitetoff,ipjtetoff,ipitet,ipjtet,ipitettyp - cmo pointers
C       (ip)iparent - Pointer to array of parent points of all nodes
C       nef - number of element faces for this cmo
C
C     OUTPUT ARGUMENTS -
C        nelts - number of elements containing the edge
C        (ip)elts - elements containing edge
C        (ip)edges _ local edge numbers
C
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
      integer nelts,itin,iein
      pointer (ipelts, elts)
      integer elts(*)
      pointer (ipedges, edgs)
      integer edgs(*)
      pointer (ipitetoff, itetoff)
      pointer (ipjtetoff, jtetoff)
      pointer (ipitettyp, itettyp)
      integer  itetoff(*), jtetoff(*),itettyp(*)
C
      pointer (ipitet, itet1)
      pointer (ipjtet, jtet1)
      integer itet1(*), jtet1(*)
      pointer (ipiparent, iparent)
      integer iparent(*)
C
      integer ilen,icscode,ierror,iip1,iip2,
     *        ie,ip1,ip2,nef,ilen1,numinc,
     *        i1,i2,itt,itnew,
     *        iff,j,mbndry,
     *        ittyp,nf,k,
     *        jt
      character*32 isubname,blkelts,prtelts,blkedges,prtedges
C
C#######################################################################
C
C
      isubname='get_elements_on_edge'
      numinc=100
      call mmgetlen(ipelts,ilen,ierror)
      if(ierror.ne.0) go to 19
      call mmgetnam(ipelts,blkelts,prtelts,ierror)
      if(ierror.ne.0) go to 19
      call mmgetlen(ipedges,ilen1,ierror)
      if(ierror.ne.0) go to 19
      call mmgetnam(ipedges,blkedges,prtedges,ierror)
      if(ierror.ne.0) go to 19
C
C  nodes on edge (itin,iein)
C
      iip1=itet1(itetoff(itin)+ielmedge1(1,iein,itettyp(itin)))
      iip2=itet1(itetoff(itin)+ielmedge1(2,iein,itettyp(itin)))
      ip1=iparent(iip1)
      ip2=iparent(iip2)
C
C  Loop through all neigbors of element to find one that shares
C  this edge
C  NOTE 2D has only one face so looping gives unpredictable result
C  replaced do j=1,ielmface0(nf,ittyp) with j = 1
C
      nelts = 1
      elts(1)=itin
      edgs(1)=iein
      itt=itin
      ittyp=itettyp(itt)
 10   do nf =1,nelmnef(ittyp)
         j = 1
         ie=ielmface2(j,nf,ittyp)
         i1=itet1(itetoff(itt)+ielmedge1(1,ie,ittyp))
         i2=itet1(itetoff(itt)+ielmedge1(2,ie,ittyp))
         i1=iparent(i1)
         i2=iparent(i2)
         if(i1.eq.ip1.and.i2.eq.ip2.or.
     *      i2.eq.ip1.and.i1.eq.ip2) then
            iff=nf
C
C  check if external boundary face
C
               jt=jtet1(jtetoff(itt)+iff)
               if(jt.eq.mbndry)  go to 9999
C
C  check if interface face
C
               if(jt.gt.mbndry)  jt=jt-mbndry
               itnew=1+(jt-1)/nef
C
C  add to element list (increase if necessary)
C
               nelts=nelts+1
               if(nelts.gt.ilen) then
                  call mmincblk(blkelts,prtelts,ipelts,numinc,
     *                         ierror)
                  if(ierror.ne.0) go to 19
                  ilen=ilen+numinc
               endif
               if(nelts.gt.ilen1) then
                  call mmincblk(blkedges,prtedges,ipedges,numinc,
     *                         ierror)
                  if(ierror.ne.0) go to 19
                  ilen1=ilen1+numinc
               endif
               elts(nelts)=itnew
C
C  find edge for this element
C
               ittyp=itettyp(itnew)
               do k=1,nelmnee(ittyp)
                     i1=itet1(itetoff(itnew)+ielmedge1(1,k,ittyp))
                     i2=itet1(itetoff(itnew)+ielmedge1(2,k,ittyp))
                     i1=iparent(i1)
                     i2=iparent(i2)
                     if(i1.eq.ip1.and.i2.eq.ip2.or.
     *                i2.eq.ip1.and.i1.eq.ip2) ie=k
               enddo
               edgs(nelts)=ie
               go to 9999
            endif
C end loop on edges
         enddo
C end loop on faces
C     enddo - loop removed, there is just one face for 2d
C
C  shouldn't ever get here
C
  19  write(logmess, 20)itin,iein
  20  format(' ATTENTION: error in get_elements_on_edge2d for element',
     *         i10,' edge ',i10 )
      call writloga('default',0,logmess,0,icscode)
C
9999  continue
      return
      end
C #####################################################################
