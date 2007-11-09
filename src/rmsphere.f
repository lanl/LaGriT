*dk,rmsphere
      subroutine rmsphere(imsgin,xmsgin,cmsgin,msgtype,nwds,ierr1)
C
C#######################################################################
C
C      PURPOSE -
C
C         THIS ROUTINE IS USED TO REMOVE POINTS FROM A SPHERICAL
C            SHELL REGION GIVEN AN INNER-OUTER RADIUS AND CENTER POINT.
C
C
C          FORMAT: RMSPHERE/inner radius/outer radius/xcen/ycen/zcen
C
C
C      INPUT ARGUMENTS -
C
C         NONE
C
C
C      OUTPUT ARGUMENTS -
C
C         NONE
C
C
C      CHANGE HISTORY -
C
C         $Log:   /pvcs.config/t3d/src/rmsphere.f_a  $
CPVCS    
CPVCS       Rev 1.1   Mon Apr 14 17:00:00 1997   pvcs
CPVCS    No change.
CPVCS    
CPVCS       Rev 1.0   09/20/95 09:46:44   dcg
CPVCS    Initial revision.
C
C
C#######################################################################
C
      implicit real*8 (a-h, o-z)
C
      include 'chydro.h'
C
C#######################################################################
C
      integer nwds, imsgin(nwds), msgtype(nwds)
      real*8 xmsgin(nwds)
      character*(*) cmsgin(nwds)
C
      integer ierror
C
C#######################################################################
C
      character*132 logmess
C
C
      character*40 cmo
C
C     *****************************************************************
C
      pointer ( ipimt1 , imt1 )
      integer imt1(10000000)
      pointer ( ipitp1 , itp1 )
      integer itp1(10000000)
C
      pointer (ipxic, xic)
      pointer (ipyic, yic)
      pointer (ipzic, zic)
      REAL*8 xic(1000000), yic(1000000), zic(1000000)
C
C#######################################################################
C
C
C     ******************************************************************
C
C
      call cmo_get_name(cmo,icscode)
C
      call cmo_get_info('nnodes',cmo,nnodes,ilencmo,itypcmo,icscode)
      call cmo_get_info('nelements',cmo,
     *                   nelements,ilencmo,itypcmo,icscode)
      call cmo_get_info('mbndry',cmo,mbndry,ilencmo,itypcmo,icscode)
C
      call cmo_get_info('imt1',cmo,ipimt1,ilen,ityp,icscode)
      if (icscode .ne. 0) call x3d_error(isubname,'cmo_get_info')
      call cmo_get_info('itp1',cmo,ipitp1,ilen,ityp,icscode)
      if (icscode .ne. 0) call x3d_error(isubname,'cmo_get_info')
C
      call cmo_get_info('xic',cmo,ipxic,ilen,ityp,icscode)
      if (icscode .ne. 0) call x3d_error(isubname,'cmo_get_info')
      call cmo_get_info('yic',cmo,ipyic,ilen,ityp,icscode)
      if (icscode .ne. 0) call x3d_error(isubname,'cmo_get_info')
      call cmo_get_info('zic',cmo,ipzic,ilen,ityp,icscode)
      if (icscode .ne. 0) call x3d_error(isubname,'cmo_get_info')
C
      ipointf=nnodes
C
      radin=xmsgin(1)
      radot=xmsgin(2)
      xcen=xmsgin(3)
      ycen=xmsgin(4)
      zcen=xmsgin(5)
      radinsq=0.999999*radin*radin
      radotsq=1.000001*radot*radot
C
C
      ndel=0
      do 100 i1=1,ipointf
      if(i1.eq.i1) goto 101
 101  continue
      radsq=(xic(i1)-xcen)**2+(yic(i1)-ycen)**2+(zic(i1)-zcen)**2
      if(radsq.ge.radinsq.and.radsq.le.radotsq) then
         ndel=ndel+1
         isq=0
         itp=ifitpdud
         itp1(i1)=itp
      endif
 100  continue
C
      ierr1=0
      write(logmess,6000) ndel
 6000 format( 'RMSPHERE DUDDED ',i6,' POINTS')
      call writloga('default',0,logmess,0,ierrw)
C
C
      goto 9999
 9999 continue
      return
      end
