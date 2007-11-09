      subroutine sphere1(itype,icount,nrt,radius,xc,yc,zc,uradius)
C
C#######################################################################
C
C     PURPOSE -
C
C     THIS ROUTINE GENERATES A SPHERE OF A SPECIFIED RADIUS
C
C
C     FORMAT: SPHERE/ITYPE=1/NPT/RADIUS/XCEN/YCEN/ZCEN/VELOCITY
C
C
C     INPUT:
C
C           NRT - NUMBER OF POINTS ON RADIUS SPHERE (THIS WILL BE ROUNDED
C                 TO THE NEAREST SQRT(INT(NUMBER OF POINTS/6 SIDES))
C
C        RADIUS - RADIUS OF THE SPHERE
C
C            XC - X COORDINATE OF THE CENTER OF THE SPHERE
C            YC - Y COORDINATE OF THE CENTER OF THE SPHERE
C            ZC - Z COORDINATE OF THE CENTER OF THE SPHERE
C
C       URADIUS - VELOCITY (RELATIVE TO THE CENTER )
C
C
C     INPUT ARGUMENTS -
C
C
C
C     OUTPUT ARGUMENTS -
C
C
C
C     CHANGE HISTORY -
C
C        $Log: sphere1.f,v $
C        Revision 2.00  2007/11/09 20:04:03  spchu
C        Import to CVS
C
CPVCS    
CPVCS       Rev 1.1   Wed Apr 05 13:35:04 2000   nnc
CPVCS    Minor source modifications required by the Absoft compiler.
CPVCS    
CPVCS       Rev 1.0   Mon Jan 31 11:14:18 2000   dcg
CPVCS    Initial revision.
CPVCS
CPVCS       Rev 1.7   Mon Apr 14 17:02:04 1997   pvcs
CPVCS    No change.
CPVCS
CPVCS       Rev 1.6   Tue Mar 05 12:35:36 1996   het
CPVCS    Correct an error with the zero point.
CPVCS
CPVCS       Rev 1.5   Thu Feb 08 15:55:22 1996   dcg
CPVCS    replace uic,vic,wic with user defined velocity vector
CPVCS
CPVCS       Rev 1.4   11/16/95 15:22:46   dcg
CPVCS    replace character literals in calls
CPVCS
CPVCS       Rev 1.3   10/22/95 13:19:00   het
CPVCS    Fix the call to rz because the calling sequence changed
CPVCS
CPVCS       Rev 1.2   08/29/95 12:17:02   het
CPVCS    Add the cmowrk storage block for each CMO
CPVCS
CPVCS       Rev 1.1   08/15/95 18:19:50   het
CPVCS    Cleanup code and correct errors
CPVCS
CPVCS       Rev 1.0   01/17/95 16:39:34   pvcs
CPVCS     Original version
C
C
C#######################################################################
C
C
      implicit none
C
C#######################################################################
C
      include 'cmo.h'
      include 'chydro.h'
C
      character*4096 ibuff
      character*32 cvelnm,isubname
      pointer(ipout,out)
      real*8 out(*)
      pointer(ipvels,vels)
      real*8 vels(3,*)
      integer ipointj,length,icmotype,icscode,ierror,nrt,icts,nrt1,
     *  iout,lout,icount,itype,ier,i1,nrtx,nrty,nrtz,ntets,npoints,
     *  lenxic,ityp
      real*8 xc,yc,zc,rout,radius,uradius,xdiff,ydiff,zdiff,radxyz,
     * xradius,yradius,zradius
C
C#######################################################################
 
      isubname='shpere1'
 
      call cmo_get_info('ipointj',cmo,
     *                ipointj,length,icmotype,icscode)
      if (icscode .ne. 0) call x3d_error(isubname,'ipointj')
C
C
      call cmo_get_name(cmo,ierror)
C
      call cmo_get_info('xic',cmo,ipxic,lenxic,icmotype,ierror)
      length=ipointj+nrt
      if(length.gt.lenxic) then
         call cmo_set_info('nnodes',cmo,length,1,1,ierror)
         call cmo_newlen(cmo,ierror)
      endif
C
      call cmo_get_info('nnodes',cmo,npoints,length,icmotype,ierror)
      call cmo_get_info('nelements',cmo,ntets,length,icmotype,ierror)
      call cmo_get_info('mbndry',cmo,mbndry,length,icmotype,ierror)
      call cmo_get_info('xic',cmo,ipxic,length,icmotype,ierror)
      call cmo_get_info('yic',cmo,ipyic,length,icmotype,ierror)
      call cmo_get_info('zic',cmo,ipzic,length,icmotype,ierror)
C
C.......................................................................
C
      icts=ipointj+1
      nrt1=int(sqrt(float(nrt+1)/6.0))
      if(radius.le.1.0e-10) then
         ipointj=ipointj+1
         xic(ipointj)=0.0+xc
         yic(ipointj)=0.0+yc
         zic(ipointj)=0.0+zc
         call cmo_set_info('ipointj',cmo,
     *                   ipointj,1,1,icscode)
         if (icscode .ne. 0) call x3d_error(isubname,'set_ipointj')
         goto 9999
      endif
C
      nrtx=nrt1
      nrty=nrt1
      nrtz=nrt1
      xradius=radius
      yradius=radius
      zradius=radius
C
      ibuff=' '
      write(ibuff,9000) 'rz/xyz/',
     *     nrtx ,     nrty ,        1 ,
     *  -xradius , -yradius , -zradius ,
     *  +xradius , +yradius , -zradius
      call dotaskx3d(ibuff,ierror)
C
      ibuff=' '
      write(ibuff,9000) 'rz/xyz/',
     *      nrtx ,     nrty ,        1 ,
     *  -xradius , -yradius , +zradius ,
     *  +xradius , +yradius , +zradius
      call dotaskx3d(ibuff,ierror)
C
C
      ibuff=' '
      write(ibuff,9000) 'rz/xyz/',
     *      nrtx ,        1 ,     nrtz ,
     *  -xradius , -yradius , -zradius ,
     *  +xradius , -yradius , +zradius
      call dotaskx3d(ibuff,ierror)
C
      ibuff=' '
      write(ibuff,9000) 'rz/xyz/',
     *      nrtx ,        1 ,     nrtz ,
     *  -xradius , +yradius , -zradius ,
     *  +xradius , +yradius , +zradius
      call dotaskx3d(ibuff,ierror)
C
      ibuff=' '
      write(ibuff,9000) 'rz/xyz/',
     *         1 ,     nrty ,     nrtz ,
     *  -xradius , -yradius , -zradius ,
     *  -xradius , +yradius , +zradius
      call dotaskx3d(ibuff,ierror)
C
      ibuff=' '
      write(ibuff,9000) 'rz/xyz/',
     *         1 ,     nrty ,     nrtz ,
     *  +xradius , -yradius , -zradius ,
     *  +xradius , +yradius , +zradius
      call dotaskx3d(ibuff,ierror)
C
      call cmo_get_info('ipointj',cmo,
     *                icount,length,icmotype,icscode)
      if (icscode .ne. 0) call x3d_error(isubname,'get_ipointj')
C
      call cmo_get_info('nnodes',cmo,npoints,length,icmotype,ierror)
      call cmo_get_info('nelements',cmo,ntets,length,icmotype,ierror)
      call cmo_get_info('mbndry',cmo,mbndry,length,icmotype,ierror)
      call cmo_get_info('xic',cmo,ipxic,length,icmotype,ierror)
      call cmo_get_info('yic',cmo,ipyic,length,icmotype,ierror)
      call cmo_get_info('zic',cmo,ipzic,length,icmotype,ierror)
C
      do 100 i1=icts,icount
         radxyz=sqrt(xic(i1)*xic(i1)+yic(i1)*yic(i1)+zic(i1)*zic(i1))
         xic(i1)=xic(i1)*radius/radxyz+xc
         yic(i1)=yic(i1)*radius/radxyz+yc
         zic(i1)=zic(i1)*radius/radxyz+zc
 100  continue
C
      if(abs(uradius).lt.1.0e-15) goto 9999
      call cmo_get_attinfo('velname',cmo,iout,rout,cvelnm,
     *   ipout,lout,ityp,ierror)
      call cmo_get_info(cvelnm,cmo,ipvels,length,icmotype,ier)
      if(ier.ne.0) go to 9999
      do 200 i1=icts,icount
         xdiff=xic(i1)-xc
         ydiff=yic(i1)-yc
         zdiff=zic(i1)-zc
         radxyz=sqrt(xdiff*xdiff+ydiff*ydiff+zdiff*zdiff)
         vels(1,i1)=uradius*xdiff/radxyz
         vels(2,i1)=uradius*ydiff/radxyz
         vels(3,i1)=uradius*zdiff/radxyz
 200  continue
C
 
 9000 format(a7,3(i3,'/'),6(1pe15.7,'/'),' ; finish')
 9999 return
      end
