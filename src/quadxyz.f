*DK quadxyz
      subroutine quadxyz(imsgin,xmsgin,cmsgin,msgtype,nwds,ierr2)
C
C#######################################################################
C
C      PURPOSE -
C
C         THIS SUBROUTINE DEFINES AN ARBITRARY, LOCICAL hexahedron OF
C           POINTS IN 3D(XYZ) SPACE.
C
C
C     FORMAT: QUADXYZ/# points in x/# points in y/                      &
C                    x1,y1,z1/x2,y2,z2/x3,y3,z3/x4,y4,z4 (counter clockwise)
C                    x5,y5,z5/x6,y6,z6/x7,y7,z7/x8,y8,z8 (corners of hex)
C                    ixz,iyz,izz/ (cell centered=0, end points =1)
C                    ixratio,iyratio,izratio/(regular spacing=1, ratio spacing=0)
C                    xrz,yrz,zrz (ratio spacing - xrz is ratio of second
C                              interval to first interval along
C                              line from point 1 to point2
C                              yrz -- ditto along line from point2
C                                     to point3)
C                              zrz -- ditto along line from point1
C                                     to point5)
C          Note:  the direction from point 1 to point 2 is the 'x' direction
C          Note:  the direction from point 2 to point 3 is the 'y' direction
C          Note:  the direction from point 1 to point 5 is the 'z' direction
C
C      INPUT ARGUMENTS -
C
C        imsgin()  - Integer array of command input tokens
C        xmsgin()  - Real array of command input tokens
C        cmsgin()  - Character array of command input tokens
C        msgtype() - Integer array of command input token types
C        nwds      - Number of command input tokens
C
C      OUTPUT ARGUMENTS -
C
C         ierr2 - Error Return Code (==0 ==> OK, <>0 ==> Error)
C
C
C      CHANGE HISTORY -
C
C         $Log: quadxyz.f,v $
C         Revision 2.00  2007/11/09 20:03:58  spchu
C         Import to CVS
C
CPVCS    
CPVCS       Rev 1.6   05 Mar 2002 13:06:00   gable
CPVCS    Changed log file output format from i6 to i9
CPVCS    
CPVCS       Rev 1.5   Thu Apr 06 13:34:16 2000   dcg
CPVCS    replace get_info_i and set_info_i calls
CPVCS
CPVCS       Rev 1.4   Wed Oct 08 16:54:32 1997   dcg
CPVCS    fix number of arguments in calls to x3d_error
CPVCS
CPVCS       Rev 1.3   Mon Apr 14 16:57:06 1997   pvcs
CPVCS    No change.
CPVCS
CPVCS       Rev 1.2   Thu Oct 10 11:23:52 1996   dcg
CPVCS    take care of special case with nz=2
CPVCS
CPVCS       Rev 1.0   Mon Jul 29 15:17:34 1996   dcg
CPVCS    Initial revision.
C
C#######################################################################
C
      implicit none
C
      character*132 logmess
C
C#######################################################################
C
      integer nwds, imsgin(nwds), msgtype(nwds)
      REAL*8 xmsgin(nwds)
      character*(*) cmsgin(nwds)
C
      integer ierr,ixratio,iyratio,izratio,ixz,iyz,izz,nx,ny,nz,iprint
      integer npoints,ilen,ityp,istart,ipointi,ipointj,npt,ic1,iz,
     x        ierr2,icount
 
C
C#######################################################################
C
      character*32 cmo
      character*8 isubname
      pointer (ipxic, xic)
      pointer (ipyic, yic)
      pointer (ipzic, zic)
      real*8 xic(1000000), yic(1000000), zic(1000000)
      real*8 x1,x2,x3,x4,y1,y2,y3,y4,z1,z2,z3,z4,xrz,yrz,zrz,d,
     x       x5,y5,z5,x6,y6,z6,x7,y7,z7,x8,y8,z8,x(4),y(4),z(4),
     X       deltax,deltay,deltaz,factor
C
 
C
C#######################################################################
C
      isubname='quadxyz'
C
      ierr2=0
C
      call cmo_get_name(cmo,ierr2)
      call cmo_get_info('nnodes',cmo,npoints,ilen,ityp,ierr2)
C
      call cmo_get_info('ipointi',cmo,
     *                ipointi,ilen,ityp,ierr)
      if (ierr .ne. 0) call x3d_error(isubname,'cmo_get_info')
      call cmo_get_info('ipointj',cmo,
     *                ipointj,ilen,ityp,ierr)
      if (ierr .ne. 0) call x3d_error(isubname,'cmo_get_info')
C
      istart=1
      nx=imsgin(istart+1)
         istart=istart+1
      ny=imsgin(istart+1)
         istart=istart+1
      nz=imsgin(istart+1)
         istart=istart+1
      x1=xmsgin(istart+1)
         istart=istart+1
      y1=xmsgin(istart+1)
         istart=istart+1
      z1=xmsgin(istart+1)
         istart=istart+1
      x2=xmsgin(istart+1)
         istart=istart+1
      y2=xmsgin(istart+1)
         istart=istart+1
      z2=xmsgin(istart+1)
         istart=istart+1
      x3=xmsgin(istart+1)
         istart=istart+1
      y3=xmsgin(istart+1)
         istart=istart+1
      z3=xmsgin(istart+1)
         istart=istart+1
      x4=xmsgin(istart+1)
         istart=istart+1
      y4=xmsgin(istart+1)
         istart=istart+1
      z4=xmsgin(istart+1)
         istart=istart+1
      x5=xmsgin(istart+1)
         istart=istart+1
      y5=xmsgin(istart+1)
         istart=istart+1
      z5=xmsgin(istart+1)
         istart=istart+1
      x6=xmsgin(istart+1)
         istart=istart+1
      y6=xmsgin(istart+1)
         istart=istart+1
      z6=xmsgin(istart+1)
         istart=istart+1
      x7=xmsgin(istart+1)
         istart=istart+1
      y7=xmsgin(istart+1)
         istart=istart+1
      z7=xmsgin(istart+1)
         istart=istart+1
      x8=xmsgin(istart+1)
         istart=istart+1
      y8=xmsgin(istart+1)
         istart=istart+1
      z8=xmsgin(istart+1)
         istart=istart+1
      if(nwds.le.istart) then
         ixz=1
         iyz=1
         izz=1
      else
         ixz=imsgin(istart+1)
            istart=istart+1
         iyz=imsgin(istart+1)
            istart=istart+1
         izz=imsgin(istart+1)
            istart=istart+1
      endif
      if(nwds.le.istart) then
        ixratio=0
        iyratio=0
        izratio=0
        xrz=0.
        yrz=0.
        zrz=0.
      else
            istart=istart+1
         ixratio=imsgin(istart)
            istart=istart+1
         iyratio=imsgin(istart)
            istart=istart+1
         izratio=imsgin(istart)
            istart=istart+1
         xrz=xmsgin(istart)
            istart=istart+1
         yrz=xmsgin(istart)
            istart=istart+1
         zrz=xmsgin(istart)
            istart=istart+1
      endif
C
C     ******************************************************************
C
C     FILTER THE ZONING FLAGS AND MULTIPLIES TO ALLOW ONLY THE
C        PROPER VALUES.
C
      if(ixz.ne.0.and.ixz.ne.1) ixz=1
      if(iyz.ne.0.and.iyz.ne.1) iyz=1
      if(izz.ne.0.and.izz.ne.1) izz=1
      if(xrz.le.0.0) xrz=1.0
      if(yrz.le.0.0) yrz=1.0
      if(zrz.le.0.0) zrz=1.0
      iprint=0
      if(ixz.eq.0.and.ixratio.ne.0) then
         iprint=1
         ixratio=0
         xrz=1.
      endif
      if(iyz.eq.0.and.iyratio.ne.0) then
         iprint=1
         iyratio=0
         yrz=1.
      endif
      if(izz.eq.0.and.izratio.ne.0) then
         iprint=1
         izratio=0
         zrz=1.
      endif
      if(iprint.eq.1) then
         logmess='Ratio spacing not allowed with cell centered points'
         call writloga('default',0,logmess,0,ierr)
      endif
C
C     Adjust end points for cell centered data
C
      if(ixz.eq.0) then
         d=0.5*(x2-x1)/nx
         x1=x1+d
         x2=x2-d
         d=0.5*(y2-y1)/nx
         y1=y1+d
         y2=y2-d
         d=0.5*(z2-z1)/nx
         z1=z1+d
         z2=z2-d
         d=0.5*(x3-x4)/nx
         x4=x4+d
         x3=x3-d
         d=0.5*(y3-y4)/nx
         y4=y4+d
         y3=y3-d
         d=0.5*(z3-z4)/nx
         z4=z4+d
         z3=z3-d
         d=0.5*(x6-x5)/nx
         x5=x5+d
         x6=x6-d
         d=0.5*(y6-y5)/nx
         y5=y5+d
         y6=y6-d
         d=0.5*(z6-z5)/nx
         z5=z5+d
         z6=z6-d
         d=0.5*(x7-x8)/nx
         x8=x8+d
         x7=x7-d
         d=0.5*(y7-y8)/nx
         y8=y8+d
         y7=y7-d
         d=0.5*(z7-z8)/nx
         z8=z8+d
         z7=z7-d
      endif
      if(iyz.eq.0) then
         d=0.5*(x3-x2)/ny
         x2=x2+d
         x3=x3-d
         d=0.5*(y3-y2)/ny
         y2=y2+d
         y3=y3-d
         d=0.5*(z3-z2)/ny
         z2=z2+d
         z3=z3-d
         d=0.5*(x4-x1)/ny
         x4=x4-d
         x1=x1+d
         d=0.5*(y4-y1)/ny
         y4=y4-d
         y1=y1+d
         d=0.5*(z4-z1)/ny
         z4=z4-d
         z1=z1+d
         d=0.5*(x7-x6)/ny
         x6=x6+d
         x7=x7-d
         d=0.5*(y7-y6)/ny
         y6=y6+d
         y7=y7-d
         d=0.5*(z7-z6)/ny
         z6=z6+d
         z7=z7-d
         d=0.5*(x8-x5)/ny
         x8=x8-d
         x5=x5+d
         d=0.5*(y8-y5)/ny
         y8=y8-d
         y5=y5+d
         d=0.5*(z8-z5)/ny
         z8=z8-d
         z5=z5+d
      endif
      if(izz.eq.0) then
         d=0.5*(x5-x1)/nz
         x1=x1+d
         x5=x5-d
         d=0.5*(y5-y1)/nz
         y1=y1+d
         y5=y5-d
         d=0.5*(z5-z1)/nz
         z1=z1+d
         z5=z5-d
         d=0.5*(x6-x2)/nz
         x6=x6-d
         x2=x2+d
         d=0.5*(y6-y2)/nz
         y6=y6-d
         y2=y2+d
         d=0.5*(z6-z2)/nz
         z6=z6-d
         z2=z2+d
         d=0.5*(x7-x3)/nz
         x3=x3+d
         x7=x7-d
         d=0.5*(y7-y3)/nz
         y3=y3+d
         y7=y7-d
         d=0.5*(z7-z3)/nz
         z3=z3+d
         z7=z7-d
         d=0.5*(x8-x4)/nz
         x8=x4-d
         x4=x4+d
         d=0.5*(y8-y4)/nz
         y8=y8-d
         y4=y4+d
         d=0.5*(z8-z4)/nz
         z8=z8-d
         z4=z4+d
      endif
C
C     Set nnodes in mesh object and get pointers
C
      call cmo_set_info('nnodes',cmo,npoints+nx*ny*nz,1,1,ierr2)
      call cmo_newlen(cmo,ierr2)
      call cmo_get_info('xic',cmo,ipxic,ilen,ityp,ierr2)
      call cmo_get_info('yic',cmo,ipyic,ilen,ityp,ierr2)
      call cmo_get_info('zic',cmo,ipzic,ilen,ityp,ierr2)
C
C     CREATE the points.
C     Do base first (quad defined by first four points)
C     The do top (quad defined by last four points)
C
      call quadpts(x1,y1,z1,x2,y2,z2,x3,y3,z3,x4,y4,z4,nx,ny,xrz,yrz,
     x             xic(npoints+1),yic(npoints+1),zic(npoints+1))
      npt=npoints+1+(nx*ny)*(nz-1)
      call quadpts(x5,y5,z5,x6,y6,z6,x7,y7,z7,x8,y8,z8,nx,ny,xrz,yrz,
     x             xic(npt),yic(npt),zic(npt))
C
C     Now fill in the middle use call to getdelta to get z spacing
C     Then call quadpts to fill each intermediate quad.
C
      if(nz.le.2) go to 5000
      call getdelta(x1,y1,z1,x5,y5,z5,nz-1,zrz,deltax,deltay,
     *  deltaz)
      factor=1.0
      do iz=2,nz-1
         if(iz.ge.3) factor=factor*zrz
         icount=npoints+nx*ny*(iz-1)+1
         xic(icount)=xic(icount-nx*ny)+deltax*factor
         yic(icount)=yic(icount-nx*ny)+deltay*factor
         zic(icount)=zic(icount-nx*ny)+deltaz*factor
      enddo
      call getdelta(x2,y2,z2,x6,y6,z6,nz-1,zrz,deltax,deltay,
     *  deltaz)
      factor=1.0
      do iz=2,nz-1
         if(iz.ge.3) factor=factor*zrz
         icount=npoints+nx*ny*(iz-1)+nx
         xic(icount)=xic(icount-nx*ny)+deltax*factor
         yic(icount)=yic(icount-nx*ny)+deltay*factor
         zic(icount)=zic(icount-nx*ny)+deltaz*factor
      enddo
      call getdelta(x4,y4,z4,x8,y8,z8,nz-1,zrz,deltax,deltay,
     *  deltaz)
      factor=1.0
      do iz=2,nz-1
         if(iz.ge.3) factor=factor*zrz
         icount=npoints+nx*ny*(iz-1)+1+nx*(ny-1)
         xic(icount)=xic(icount-nx*ny)+deltax*factor
         yic(icount)=yic(icount-nx*ny)+deltay*factor
         zic(icount)=zic(icount-nx*ny)+deltaz*factor
      enddo
      call getdelta(x3,y3,z3,x7,y7,z7,nz-1,zrz,deltax,deltay,
     *  deltaz)
      factor=1.0
      do iz=2,nz-1
         if(iz.ge.3) factor=factor*zrz
         icount=npoints+nx*ny*(iz)
         xic(icount)=xic(icount-nx*ny)+deltax*factor
         yic(icount)=yic(icount-nx*ny)+deltay*factor
         zic(icount)=zic(icount-nx*ny)+deltaz*factor
      enddo
C
C     We have all four corners of all intermediate quads - fill them in
C
      do iz=2,nz-1
         ic1=npoints+nx*ny*(iz-1)+1
         x(1)=xic(ic1)
         y(1)=yic(ic1)
         z(1)=zic(ic1)
         x(2)=xic(ic1+nx-1)
         y(2)=yic(ic1+nx-1)
         z(2)=zic(ic1+nx-1)
         x(4)=xic(ic1+nx*(ny-1))
         y(4)=yic(ic1+nx*(ny-1))
         z(4)=zic(ic1+nx*(ny-1))
         x(3)=xic(npoints+nx*ny*iz)
         y(3)=yic(npoints+nx*ny*iz)
         z(3)=zic(npoints+nx*ny*iz)
         call quadpts(x(1),y(1),z(1),
     x                x(2),y(2),z(2),
     x                x(3),y(3),z(3),
     x                x(4),y(4),z(4),nx,ny,xrz,yrz,
     x                xic(ic1),yic(ic1),zic(ic1))
      enddo
 
C     ******************************************************************
C     PRINT OUT THE POINT NUMBERS GENERATED
C
 5000 write(logmess,6000) npoints+1,npoints+nx*ny*nz
 6000 format('  QUADXY GENERATED POINTS ',i9,' TO ',i9)
      call writloga('default',0,logmess,0,ierr)
C
C
      ipointi=npoints+1
      ipointj=npoints+nx*ny*nz
      call cmo_get_name(cmo,ierr2)
C
      call cmo_set_info('ipointi',cmo,
     *                ipointi,ilen,ityp,ierr)
      if (ierr .ne. 0) call x3d_error(isubname,'cmo_set_info')
      call cmo_set_info('ipointj',cmo,
     *                ipointj,ilen,ityp,ierr)
      if (ierr .ne. 0) call x3d_error(isubname,'cmo_set_info')
C
      goto 9999
 9999 continue
C
      return
      end
