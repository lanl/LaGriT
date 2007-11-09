      subroutine refine_edge_list_lg(nwds,imsgin,xmsgin,cmsgin,ierror)
C
C #####################################################################
C
C     PURPOSE -
C
C     This routine takes pairs of nodes - determines the edges that
C     contain them and refines these edges
C
C     INPUT ARGUMENTS -
C
C        imsgin()  - Integer array of command input tokens
C        xmsgin()  - Real array of command input tokens
C        cmsgin()  - Character array of command input tokens
C        msgtype() - Integer array of command input token types
C        nwds      - Number of command input tokens
C
C     OUTPUT ARGUMENTS -
C
C        ierror - Error Return Code (==0 ==> OK, <>0 ==> Error)
C
C     CHANGE HISTORY -
C
C$Log: refine_edge_list_lg.f,v $
CRevision 2.00  2007/11/09 20:04:00  spchu
CImport to CVS
C
CPVCS    
CPVCS       Rev 1.5   04 May 2004 16:22:18   gable
CPVCS    B. Aagaard: Fixed bug, ityp = itettyp(it) was not being set correctly.
CPVCS    
CPVCS       Rev 1.4   Mon Feb 22 20:40:38 1999   nnc
CPVCS    Added missing initialization of ipipt2.
CPVCS    
CPVCS       Rev 1.3   Wed Feb 10 09:14:34 1999   dcg
CPVCS    add missing return  - adjust temp memory allocation
CPVCS
CPVCS       Rev 1.2   Fri Jan 22 16:53:08 1999   dcg
CPVCS    remove duplicate declaration
CPVCS
CPVCS       Rev 1.1   Wed Dec 23 15:55:36 1998   dcg
CPVCS    add error message
CPVCS
CPVCS       Rev 1.0   Wed Dec 23 14:07:36 1998   dcg
CPVCS    Initial revision.
C
C
C ######################################################################
C
      implicit none
      integer nwds, imsgin(nwds), ierror
      real*8 xmsgin(nwds)
      character*(*) cmsgin(nwds)
C
      character*132 logmess
C
      pointer (ipitp1, itp1(*))
      pointer (ipisn1, isn1(*))
      pointer (ipxic, xic(*))
      pointer (ipyic, yic(*))
      pointer (ipzic, zic(*))
      integer itp1,isn1
      real*8 xic,yic,zic
C
      pointer (ipitet, itet1)
      pointer (ipjtet, jtet1)
      integer itet1(*), jtet1(*)
C
      pointer (ipitettyp, itettyp)
      pointer (ipitetoff, itetoff)
      pointer (ipjtetoff, jtetoff)
      integer itettyp(*),
     *        itetoff(*), jtetoff(*)
      pointer (ipitadd, itadd)
      pointer (ipieadd, ieadd)
      integer itadd(*), ieadd(*)
      pointer (ipiadd, iadd)
      integer iadd(*)
      pointer (ipipt1, ipt1)
      pointer (ipipt2, ipt2)
      integer ipt1(*),ipt2(*)
      pointer (ipxadd, xadd)
      pointer (ipyadd, yadd)
      pointer (ipzadd, zadd)
      real*8 xadd(*), yadd(*), zadd(*)
      pointer (ipiseedtet, iseedtet)
      integer iseedtet(*)
      pointer (ipiparent, iparent)
      integer iparent(*)
      pointer (ipielts, ielts)
      integer ielts(*)
C
      integer length,icscode,itype,npoints,ntets,ilen,mbndry,
     *   nef,i,nf,it,k,npairs,loc1,loc2,i1,i2,ie,
     *   j1,j2,it1,ityp1,ityp,nn,nadd,nelts,j
C
      include "local_element.h"
C
      character*32 cmo,isubname
      logical ifound
c
      isubname='refine_edge_list'
c
c  pick up pairs of nodes and find tet and edges they belong to
c  will use call to refine_edge_add
c
c     get memory for refine subroutine
      length=(nwds-4)/2
      npairs=length
      call mmgetblk('itadd',isubname,ipitadd,length,1,icscode)
      call mmgetblk('ieadd',isubname,ipieadd,length,1,icscode)
      call mmgetblk('iadd',isubname,ipiadd,length,1,icscode)
      call mmgetblk('xadd',isubname,ipxadd,length,2,icscode)
      call mmgetblk('yadd',isubname,ipyadd,length,2,icscode)
      call mmgetblk('zadd',isubname,ipzadd,length,2,icscode)
      call mmgetblk('pt1',isubname,ipipt1,length,1,icscode)
      call mmgetblk('pt2',isubname,ipipt2,length,1,icscode)
 
c  get mesh object information
      call cmo_get_name(cmo,icscode)
      call cmo_get_info('nnodes',cmo,npoints,ilen,itype,icscode)
      call cmo_get_info('nelements',cmo,ntets,ilen,itype,icscode)
      call cmo_get_info('mbndry',cmo,mbndry,ilen,itype,icscode)
      call cmo_get_info('faces_per_element',cmo,nef,ilen,itype,icscode)
      call cmo_get_info('itp1',cmo,ipitp1,ilen,ityp,icscode)
      call cmo_get_info('isn1',cmo,ipisn1,ilen,ityp,icscode)
      call cmo_get_info('xic',cmo,ipxic,ilen,ityp,icscode)
      call cmo_get_info('yic',cmo,ipyic,ilen,ityp,icscode)
      call cmo_get_info('zic',cmo,ipzic,ilen,ityp,icscode)
      call cmo_get_info('itettyp',cmo,
     *                  ipitettyp,ilen,ityp,icscode)
      call cmo_get_info('itetoff',cmo,
     *                  ipitetoff,ilen,ityp,icscode)
      call cmo_get_info('jtetoff',cmo,
     *                  ipjtetoff,ilen,ityp,icscode)
      call cmo_get_info('itet',cmo,ipitet,ilen,ityp,icscode)
      call cmo_get_info('jtet',cmo,ipjtet,ilen,ityp,icscode)
c
c  get memory for seed array and parent array and tets surrounding node
      length = ntets
      call mmgetblk('iseedtet',isubname,ipiseedtet,length,1,icscode)
      length=npoints
      call mmgetblk('iparent',isubname,ipiparent,
     &      length,1,icscode)
      length=100
      call mmgetblk('ielts',isubname,ipielts,
     &      length,1,icscode)
c  get parents
      call unpackpc(npoints,itp1,isn1,iparent)
 
c  fill iseedtet
      do i=1,ntets
         ityp = itettyp(i)
         do nf = 1, nelmnef(ityp)
            k=itet1(itetoff(i)+nf)
            iseedtet(k)=i
            iseedtet(iparent(k))=i
         enddo
      enddo
c  fill arrays of pairs of nodes
      do i=1,npairs
         ipt1(i)=imsgin(4+i*2)
         ipt2(i)=imsgin(5+i*2)
      enddo
 
c  find tet and edge for each pair of nodes
      nadd=0
      do i=1,npairs
         i1=min(ipt1(i),ipt2(i))
         i2=max(ipt1(i),ipt2(i))
         it=iseedtet(i1)
         ityp = itettyp(it)
         do nn = 1, nelmnen(ityp)
            if(i1.eq.itet1(itetoff(it)+nn)) then
               call get_elements_around_node(it,nn,nelts
     &           ,ipielts,itetoff,jtetoff,itet1,jtet1,itettyp,iparent,
     &            nef,mbndry)
               ifound= .false.
               do j=1,nelts
                 it1=ielts(j)
                 ityp1 = itettyp(it1)
                 do ie = 1, nelmnee(ityp1)
                    loc1=ielmedge1(1,ie,ityp1)
                    loc2=ielmedge1(2,ie,ityp1)
                    j1=itet1(itetoff(it1)+loc1)
                    j2=itet1(itetoff(it1)+loc2)
                    if(i1.eq.min(j1,j2).and.i2.eq.max(j1,j2)) then
                       ifound=.true.
                       nadd=nadd+1
                       itadd(nadd)=it1
                       ieadd(nadd)=ie
                       iadd(nadd)=0
                       xadd(nadd)=(xic(i1)+xic(i2))/2.
                       yadd(nadd)=(yic(i1)+yic(i2))/2.
                       zadd(nadd)=(zic(i1)+zic(i2))/2.
                       go to 10
                    endif
                 enddo
               enddo
            endif
         enddo
         if(.not.ifound) write(logmess,8) i1,i2
 8       format ('cannot refine pair ',2i10, ' no edge')
         call writloga('default',0,logmess,0,icscode)
 10      continue
      enddo
      if(nadd.gt.0) call refine_edge_add(cmo,nadd,ipitadd,ipieadd,
     *      ipiadd,ipxadd,ipyadd,ipzadd)
      call mmrelprt(isubname,icscode)
      return
      end
