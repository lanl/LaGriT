      subroutine refine_cc_rivara()
c#######################################################################
c
c     purpose -
c
C  This routine takes the information in ietet_aij (made by test_coupling_
C  coef and attempts to refine the 'bad' edges by using the rivara refine
c  routine
C  currently it will do this one edge at a time by creating a pset
c  of exactly the nodes in the edge and requesting the refinement
c  to a length 1/2 of the current length
C
C $Log: refine_cc_rivara.f,v $
C Revision 2.00  2007/11/09 20:04:00  spchu
C Import to CVS
C
CPVCS    
CPVCS       Rev 1.2   Thu Apr 06 13:47:14 2000   dcg
CPVCS    replace get_info_i call
CPVCS
CPVCS       Rev 1.1   Fri Jul 24 15:02:32 1998   dcg
CPVCS    delete unused variables
CPVCS
CPVCS       Rev 1.0   Fri Jul 24 14:53:08 1998   dcg
CPVCS    Initial revision.
      implicit none
      include 'local_element.h'
      include 'chydro.h'
      pointer (ipiadd,iadd)
      pointer (ipieadd,ieadd)
      pointer (ipitadd,itadd)
      pointer (ipxadd,xadd)
      pointer (ipyadd,yadd)
      pointer (ipzadd,zadd)
      pointer (ipitpadd,itpadd)
      pointer (ipicradd,icradd)
      integer ieadd(*),iadd(*),itadd(*),itpadd(*),icradd(*)
      real*8 xadd(*),yadd(*),zadd(*)
      pointer (ipietet,ietet)
      integer ietet(3,10000000)
      pointer(ipncc,ncc)
      real*8 ncc(10000000)
      pointer (ipxic,xic)
      pointer (ipyic,yic)
      pointer (ipzic,zic)
      real*8 xic(*),yic(*),zic(*)
      pointer (ipiparent,iparent)
      pointer (ipitp,itp1)
      pointer (ipisn,isn1)
      pointer (ipitet,itet)
      pointer (ipitetoff,itetoff)
      pointer (ipitettyp,itettyp)
      integer iparent(*),itp1(*),isn1(*),itet(*),itetoff(*),itettyp(*)
      integer ierror,ilen,itype,npoints,ntets,numneg,len_elist,isave,
     *   icscode,nadd,it,i,ittyp,i1,i2,iedge
      character*32 cmo,isubname
      real*8 dist,maxcc
      character*132 logmess
C
      isubname='refine_coup_coef'
C
C     Get mesh object data.
C
      call cmo_get_name(cmo,ierror)
      call cmo_get_info('nnodes', cmo, npoints, ilen, itype, ierror)
      if(ierror.ne.0) call x3d_error(isubname,'cmo_get_info')
      call cmo_get_info('nelements', cmo, ntets, ilen, itype, ierror)
      if(ierror.ne.0) call x3d_error(isubname,'cmo_get_info')
      call cmo_get_info('ietet_aij',cmo,ipietet,ilen,itype,ierror)
      call cmo_get_info('neg_coup_coeff',cmo,ipncc,ilen,itype,ierror)
      call cmo_get_info('num_neg_coup_coeff',cmo,numneg,ilen,itype,
     *      ierror)
      call cmo_get_info('xic',cmo,ipxic,ilen,itype,ierror)
      call cmo_get_info('yic',cmo,ipyic,ilen,itype,ierror)
      call cmo_get_info('zic',cmo,ipzic,ilen,itype,ierror)
      call cmo_get_info('itp1',cmo,ipitp,ilen,itype,ierror)
      call cmo_get_info('isn1',cmo,ipisn,ilen,itype,ierror)
      call cmo_get_info('itet',cmo,ipitet,ilen,itype,ierror)
      call cmo_get_info('itetoff',cmo,ipitetoff,ilen,itype,ierror)
      call cmo_get_info('itettyp',cmo,ipitettyp,ilen,itype,ierror)
c
      call cmo_get_info('idebug',cmo,idebug,ilen,itype,ierror)
C
C
C    Get temporary memory
C
      len_elist=numneg
      call mmgetblk('itadd',isubname,ipitadd,len_elist,1,icscode)
      call mmgetblk('ieadd',isubname,ipieadd,len_elist,1,icscode)
      call mmgetblk('iadd',isubname,ipiadd,len_elist,1,icscode)
      call mmgetblk('xadd',isubname,ipxadd,len_elist,2,icscode)
      call mmgetblk('yadd',isubname,ipyadd,len_elist,2,icscode)
      call mmgetblk('zadd',isubname,ipzadd,len_elist,2,icscode)
      call mmgetblk('itpadd',isubname,ipitpadd,len_elist,1,icscode)
      call mmgetblk('icradd',isubname,ipicradd,len_elist,1,icscode)
C
C  put parent nodes in iparent
C
      call mmgetblk('iparent',isubname,ipiparent,npoints,1,ierror)
      call unpackpc(npoints,itp1,isn1,iparent)
C
C  Loop through bad edges and make point to send to the
C  refine routine
C
      nadd=0
      isave=1
      maxcc=abs(ncc(1))
      if(numneg.eq.1) then
         call dotaskx3d('dump/gmv/gmv1 ; finish',ierror)
      endif
      do i=2,numneg
         if(abs(ncc(i)).gt.maxcc) then
            maxcc=abs(ncc(i))
            isave=i
         endif
      enddo
         write(logmess,"('number of bad coef ',i5,' max coef ',e14.6)")
     *      numneg,maxcc
         call writloga('default',0,logmess,0,ierror)
         it=ietet(1,isave)
         ittyp=itettyp(it)
C get the nodes on the bad edge and make a pset that contains only
C those two
         iedge=ietet(3,isave)
         i1=itet(itetoff(it)+ielmedge1(1,iedge,ittyp))
         i2=itet(itetoff(it)+ielmedge1(2,iedge,ittyp))
         write(logmess,12) i1,i1
 12      format ('pset/..pt1../seq/',i10,'/',i10,'/1;finish')
         call dotaskx3d(logmess,ierror)
         write(logmess,13) i2,i2
 13      format ('pset/..pt2../seq/',i10,'/',i10,'/1;finish')
         call dotaskx3d(logmess,ierror)
         call dotaskx3d('pset/..pts../union/..pt1../..pt2.. ;finish',
     *     ierror)
C  get length of edge
         dist = sqrt((xic(i2)-xic(i1))**2+(yic(i2)-yic(i1))**2+
     *               (zic(i2)-zic(i1))**2)/1.9
C  send command off to refine
         write(logmess,14) dist
14       format('refine/rivara_boundary///edge/pset,get,..pts../',e14.6,
     *           '//exclusive ; finish')
         call dotaskx3d(logmess,ierror)
         call dotaskx3d('pset/..pt1../delete;finish',ierror)
         call dotaskx3d('pset/..pt2../delete;finish',ierror)
         call dotaskx3d('pset/..pts../delete;finish',ierror)
c  refresh pointers
         call cmo_get_info('xic',cmo,ipxic,ilen,itype,ierror)
         call cmo_get_info('yic',cmo,ipyic,ilen,itype,ierror)
         call cmo_get_info('zic',cmo,ipzic,ilen,itype,ierror)
         call cmo_get_info('itet',cmo,ipitet,ilen,itype,ierror)
         call cmo_get_info('itetoff',cmo,ipitetoff,ilen,itype,ierror)
         call cmo_get_info('itettyp',cmo,ipitettyp,ilen,itype,ierror)
         call cmo_get_info('ietet_aij',cmo,ipietet,ilen,itype,ierror)
         call cmo_get_info('num_neg_coup_coeff',cmo,numneg,ilen,itype,
     *      ierror)
200   call mmrelprt(isubname,icscode)
      return
      end
 
 
 
 
 
 
 
 
 
