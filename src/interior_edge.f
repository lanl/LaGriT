      logical function interior_edge(itin,iein,
     *  ipitetoff,ipjtetoff,ipitet,ipjtet,ipitettyp,ipiparent,nef,
     *  mbndry)
C
C #####################################################################
C
C     PURPOSE -
C
C        This routine finds returns true if the edge is an interior
C        edge false if it is a boundary edge
C
C     INPUT ARGUMENTS -
C
C       it   -- seed element
C       ie   -- local edge # of it that is edge to be tested
C       (ip)itetoff (ip)jtetoff (ip)itet (ip)jtet (ip)itettyp
c            standard mesh object pointers
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
C        $Log: interior_edge.f,v $
C        Revision 2.00  2007/11/05 19:45:59  spchu
C        Import to CVS
C
CPVCS
CPVCS       Rev 1.21   02 Oct 2007 12:40:28   spchu
CPVCS    original version
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
      integer icscode,iip1,iip2,
     *        ie,ip1,ip2,nef,
     *        i1,i2,itstart,itt,itold,itnew,
     *        iff,ifold,ifnew,j,mbndry,
     *        ittyp,nf,
     *        jt,idir,nfstart,nf1
      character*32 isubname
C
C#######################################################################
C
C
      isubname='interior_edge'
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
      interior_edge=.true.
      nelts = 1
      itstart=itin
      nfstart=1
      itold=0
      ifold=0
      itt=itin
      idir=0
      ittyp=itettyp(itt)
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
                  interior_edge=.false.
                  go to 9999
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
C
                  ittyp=itettyp(itnew)
                  ittyp=itettyp(itnew)
                  itold=itt
                  ifold=iff
C
C  still looking for more tets surrounding the edge
C
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
  20  format (' ATTENTION: error in interior_edge',
     *         i10,' edge ',i10 )
      call writloga('default',0,logmess,0,icscode)
C
9999  continue
      return
      end
