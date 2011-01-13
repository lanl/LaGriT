*dk,readdcm
      subroutine readdcm(ifile,ierror)
C
C$Log: readdcm.f,v $
CRevision 2.00  2007/11/09 20:03:59  spchu
CImport to CVS
C
CPVCS    
CPVCS       Rev 1.2   30 Sep 2004 09:21:42   dcg
CPVCS    replace calls to real( with calls to dble(
CPVCS
CPVCS       Rev 1.1   Mon Apr 14 16:57:34 1997   pvcs
CPVCS    No change.
CPVCS
CPVCS       Rev 1.0   Tue Jan 30 15:20:40 1996   dcg
CPVCS    Initial revision.
C ######################################################################
C
      implicit real*8 (a-h, o-z)
C
C ######################################################################
C
      include "local_element.h"
C
C ######################################################################
C
      pointer (ipimt1, imt1(1000000))
      pointer (ipitp1, itp1(1000000))
      pointer (ipxic, xic(1000000))
      pointer (ipyic, yic(1000000))
      pointer (ipzic, zic(1000000))
      pointer (ippic, pic(1000000))
      pointer (ipric, ric(1000000))
      pointer (ipeic, eic(1000000))
      pointer (ipitetclr, itetclr(1000000))
      pointer (ipitettyp, itettyp(1000000))
      pointer (ipitetoff, itetoff(1000000))
      pointer (ipjtetoff, jtetoff(1000000))
      pointer (ipitet, itet1(1000000))
      pointer (ipjtet, jtet1(1000000))
      pointer (ipjtet2, jtet2(1000000))
C
C ######################################################################
C
      character*80 iline
      character*32 ifile
      character*32 isubname, cmo
C
      data imatsel / 0 /
C
C ######################################################################
C
      crosx(a,b,c,d,e,f)=b*f-c*e
      crosy(a,b,c,d,e,f)=c*d-a*f
      crosz(a,b,c,d,e,f)=a*e-b*d
C
C ######################################################################
C
C
      isubname='readdcm'
C
      iunit=-1
      call hassign(iunit,ifile,ierror)
      if (iunit.lt.0 .or. ierror.lt.0) then
        call x3d_error(isubname,'hassign bad file unit')
        goto 9999 
      endif 
C
      read(iunit,'(a80)') iline
      read(iunit,'(a80)') iline
      read(iunit,'(a80)') iline
C
      npoints=0
      nplanes=0
      nsum=0
 100  continue
         read(iunit,*,err=200) i1,x,y,z
         npoints=npoints+1
         goto 100
 200  continue
         read(iunit,*,end=300) i1,n,iclr,p1,p2,p3
         nplanes=nplanes+1
         nsum=nsum+n
         goto 200
 300  continue
      close(iunit)
C
      call cmo_get_name(cmo,ierr)
         if(ierr.ne.0) call x3d_error('readdcm','cmo_get_name')
C
      nnodes=npoints
      mbndry=16000000
      nsdtopo=2
      nsdgeom=3
      nen=3
      nef=3
C
      call cmo_set_info('nnodes',cmo,nnodes,1,1,ierr)
         if(ierr.ne.0) call x3d_error('readdcm','cmo_set_info')
      call cmo_set_info('mbndry',cmo,mbndry,1,1,ierr)
         if(ierr.ne.0) call x3d_error('readdcm','cmo_set_info')
      call cmo_set_info('ndimensions_geom',cmo,nsdgeom,1,1,ierr)
         if(ierr.ne.0) call x3d_error('readdcm','cmo_set_info')
      call cmo_set_info('ndimensions_topo',cmo,nsdtopo,1,1,ierr)
         if(ierr.ne.0) call x3d_error('readdcm','cmo_set_info')
      call cmo_set_info('nodes_per_element',cmo,nen,1,1,ierr)
         if(ierr.ne.0) call x3d_error('readdcm','cmo_set_info')
      call cmo_set_info('faces_per_element',cmo,nef,1,1,ierr)
         if(ierr.ne.0) call x3d_error('readdcm','cmo_set_info')
C
      call cmo_newlen(cmo,ierr)
         if(ierr.ne.0) call x3d_error('readdcm','cmo_newlen')
C
      call cmo_get_info('imt1',cmo,ipimt1,ilen,itype,ierr)
         if(ierr.ne.0) call x3d_error('readdcm','cmo_get_info')
      call cmo_get_info('itp1',cmo,ipitp1,ilen,itype,ierr)
         if(ierr.ne.0) call x3d_error('readdcm','cmo_get_info')
      call cmo_get_info('xic',cmo,ipxic,ilen,itype,ierr)
         if(ierr.ne.0) call x3d_error('readdcm','cmo_get_info')
      call cmo_get_info('yic',cmo,ipyic,ilen,itype,ierr)
         if(ierr.ne.0) call x3d_error('readdcm','cmo_get_info')
      call cmo_get_info('zic',cmo,ipzic,ilen,itype,ierr)
         if(ierr.ne.0) call x3d_error('readdcm','cmo_get_info')
C
      iunit=-1
      call hassign(iunit,ifile,ierror)
      read(iunit,'(a80)') iline
      read(iunit,'(a80)') iline
      read(iunit,'(a80)') iline
      npoints=0
 110  continue
         read(iunit,*,err=120) i1,x,y,z
         npoints=npoints+1
         imt1(i1)=0
         itp1(i1)=0
         xic(i1)=x
         yic(i1)=y
         zic(i1)=z
         goto 110
 120  continue
C
      nnodes=npoints+nplanes
      nelements=nsum
      call cmo_set_info('nnodes',cmo,nnodes,1,1,ierr)
         if(ierr.ne.0) call x3d_error('readdcm','cmo_set_info')
      call cmo_set_info('nelements',cmo,nelements,1,1,ierr)
         if(ierr.ne.0) call x3d_error('readdcm','cmo_set_info')
C
      call cmo_newlen(cmo,ierr)
         if(ierr.ne.0) call x3d_error('readdcm','cmo_newlen')
C
      call cmo_get_info('imt1',cmo,ipimt1,ilen,itype,ierr)
         if(ierr.ne.0) call x3d_error('readdcm','cmo_get_info')
      call cmo_get_info('itp1',cmo,ipitp1,ilen,itype,ierr)
         if(ierr.ne.0) call x3d_error('readdcm','cmo_get_info')
      call cmo_get_info('xic',cmo,ipxic,ilen,itype,ierr)
         if(ierr.ne.0) call x3d_error('readdcm','cmo_get_info')
      call cmo_get_info('yic',cmo,ipyic,ilen,itype,ierr)
         if(ierr.ne.0) call x3d_error('readdcm','cmo_get_info')
      call cmo_get_info('zic',cmo,ipzic,ilen,itype,ierr)
         if(ierr.ne.0) call x3d_error('readdcm','cmo_get_info')
      call cmo_get_info('pic',cmo,ippic,ilen,itype,ierr)
         if(ierr.ne.0) call x3d_error('readdcm','cmo_get_info')
      call cmo_get_info('ric',cmo,ipric,ilen,itype,ierr)
         if(ierr.ne.0) call x3d_error('readdcm','cmo_get_info')
      call cmo_get_info('eic',cmo,ipeic,ilen,itype,ierr)
         if(ierr.ne.0) call x3d_error('readdcm','cmo_get_info')
      call cmo_get_info('itetclr',cmo,ipitetclr,ilen,itype,ierr)
         if(ierr.ne.0) call x3d_error('readdcm','cmo_get_info')
      call cmo_get_info('itettyp',cmo,ipitettyp,ilen,itype,ierr)
         if(ierr.ne.0) call x3d_error('readdcm','cmo_get_info')
      call cmo_get_info('itetoff',cmo,ipitetoff,ilen,itype,ierr)
         if(ierr.ne.0) call x3d_error('readdcm','cmo_get_info')
      call cmo_get_info('jtetoff',cmo,ipjtetoff,ilen,itype,ierr)
         if(ierr.ne.0) call x3d_error('readdcm','cmo_get_info')
      call cmo_get_info('itet',cmo,ipitet,ilen,itype,ierr)
         if(ierr.ne.0) call x3d_error('readdcm','cmo_get_info')
      call cmo_get_info('jtet',cmo,ipjtet,ilen,itype,ierr)
         if(ierr.ne.0) call x3d_error('readdcm','cmo_get_info')
C
      nplanes=0
      ntris=0
      nsum=0
 210  continue
      read(iunit,*,end=310) i1,n,iclr,p1,p2,p3
C
      xavg=0.0
      yavg=0.0
      zavg=0.0
      do i=nsum+2,nsum+n
         xavg=xavg+xic(i)
         yavg=yavg+yic(i)
         zavg=zavg+zic(i)
      enddo
      xavg=xavg/dble(n-1)
      yavg=yavg/dble(n-1)
      zavg=zavg/dble(n-1)
C
      nplanes=nplanes+1
      imt1(npoints+nplanes)=iclr
      itp1(npoints+nplanes)=0
      xic(npoints+nplanes)=xavg
      yic(npoints+nplanes)=yavg
      zic(npoints+nplanes)=zavg
      pic(npoints+nplanes)=p1
      ric(npoints+nplanes)=p2
      eic(npoints+nplanes)=p3
C
      nstart=nsum+2
      nend=nsum+n+1
      do i=nstart,nend
         if(i.eq.nend) then
            i2=nstart
         else
            i2=i+1
         endif
         if(imatsel.eq.0.or.iclr.eq.imatsel) then
            ntris=ntris+1
            itetclr(ntris)=iclr
            itettyp(ntris)=ifelmtri
            itetoff(ntris)=nen*(ntris-1)
            jtetoff(ntris)=nef*(ntris-1)
            itet1(nen*(ntris-1)+1)=i
            itet1(nen*(ntris-1)+2)=i2
            itet1(nen*(ntris-1)+3)=npoints+nplanes
         endif
      enddo
C
      nsum=nsum+n+1
      goto 210
 310  continue
C
      nelements=ntris
      call cmo_set_info('nelements',cmo,nelements,1,1,ierr)
         if(ierr.ne.0) call x3d_error('readdcm','cmo_set_info')
C
      do i=1,npoints
         imt1(i)=0
         itp1(i)=0
      enddo
      do j=1,ntris
         index=1+nef*(j-1)
         imt1(itet1(index))= itetclr(j)
      enddo
      do i=1,npoints
         if(imt1(i).eq.0) itp1(i)=21
      enddo
C
      length=nef*ntris
      call mmgetblk('jtet2',isubname,ipjtet2,length,2,icscode)
      do it=1,nef*ntris
         jtet1(it)=-1
         jtet2(it)=-1
      enddo
C
      print *,"Generate connectivity"
      call geniee(itet1,jtet1,jtet2,nen,nef,
     *            ntris,nnodes,nsdtopo,nnodes,ntris)
      do it=1,ntris
         do i=1,nef
            index=i+nef*(it-1)
            jt=jtet1(index)
            jf=jtet2(index)
            if(jt.gt.0.and.jt.le.ntris) then
               jtet1(index)=nef*(jtet1(index)-1)+
     *                           jtet2(index)
            else
               jtet1(i+nef*(it-1))=mbndry
            endif
         enddo
      enddo
C
      close(iunit)
C
      xareamin=1.0d+30
      xareamax=0.0
      do it=1,ntris
         index=nen*(it-1)
         x1=xic(itet1(index+1))
         y1=yic(itet1(index+1))
         z1=zic(itet1(index+1))
         x2=xic(itet1(index+2))
         y2=yic(itet1(index+2))
         z2=zic(itet1(index+2))
         x3=xic(itet1(index+3))
         y3=yic(itet1(index+3))
         z3=zic(itet1(index+3))
         xa=-crosx((x2-x1),(y2-y1),(z2-z1),(x3-x1),(y3-y1),(z3-z1))
         ya=-crosy((x2-x1),(y2-y1),(z2-z1),(x3-x1),(y3-y1),(z3-z1))
         za=-crosz((x2-x1),(y2-y1),(z2-z1),(x3-x1),(y3-y1),(z3-z1))
         xarea=0.5*sqrt(xa**2+ya**2+za**2)
         xareamin=min(xareamin,xarea)
         xareamax=max(xareamax,xarea)
      enddo
      print *,"Area min/max from DCM data: ",xareamin,xareamax
C
      goto 9999
 9999 continue
      call mmrelprt(isubname,icscode)
      return
      end
