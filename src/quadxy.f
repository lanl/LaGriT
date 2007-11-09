*DK quadxy
      subroutine quadxy(imsgin,xmsgin,cmsgin,msgtype,nwds,ierr2)
C
C#######################################################################
C
C      PURPOSE -
C
C         THIS SUBROUTINE DEFINES AN ARBITRARY, LOCICAL QUAD OF POINTS
C            IN 2D(XY) SPACE.
C
C
C     FORMAT: QUADXY/# points in x/# points in y/                      &
C                    x1,y1,z1/x2,y2,z2/x3,y3,z3/x4,y4,z4/
C                             (counter clockwise)
C                    ixz,iyz/ (cell centered=0, end points =1)
C                    ixratio,iyratio/(regular spacing=1, ratio spacing=0)
C                    xrz,yrz (ratio spacing - xrz is ratio of second
C                              interval to first interval along
C                              line from point 1 to point2
C                              yrz -- ditto along line from point2
C                                     to point3)
C          Note:  the direction from point 1 to point 2 is the 'x' direction
C          Note:  the direction from point 2 to point 3 is the 'y' direction
C      INPUT ARGUMENTS -
C
C        imsgin()  - Integer array of command input tokens
C        xmsgin()  - Real array of command input tokens
C        cmsgin()  - Character array of command input tokens
C        msgtype() - Integer array of command input token types
C        nwds      - Number of command input tokens
C
C
C      OUTPUT ARGUMENTS -
C
C         ierr2 - Error Return Code (==0 ==> OK, <>0 ==> Error)
C
C
C      CHANGE HISTORY -
C
C         $Log:   /pvcs.config/t3d/src/quadxy.f_a  $
CPVCS    
CPVCS       Rev 1.6   05 Mar 2002 13:06:22   gable
CPVCS    Changed log file output format from i6 to i9
CPVCS    
CPVCS       Rev 1.5   Thu Apr 06 09:28:08 2000   dcg
CPVCS    replace get_info_i and set_info_i calls
CPVCS
CPVCS       Rev 1.4   Mon Apr 14 16:57:04 1997   pvcs
CPVCS    No change.
CPVCS
CPVCS       Rev 1.3   Tue Nov 05 09:38:58 1996   jan
CPVCS    modified line 171 - added error flag to arguments
CPVCS
CPVCS       Rev 1.2   Thu Oct 31 14:21:36 1996   dcg
CPVCS    add missing end statement
CPVCS
CPVCS       Rev 1.1   Wed Oct 09 10:25:44 1996   dcg
CPVCS    add ratio spacing to quad routines
CPVCS
CPVCS       Rev 1.0   Mon Jul 29 15:17:30 1996   dcg
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
      integer nwds, imsgin(nwds), msgtype(nwds),ierr2
      REAL*8 xmsgin(nwds)
      character*(*) cmsgin(nwds)
C
      integer ierr,ixratio,iyratio,ixz,iyz,nx,ny,iprint
      integer npoints,ilen,ityp,istart,ipointi,ipointj
C
C#######################################################################
C
      character*32 cmo
      character*8 isubname
C
      pointer (ipxic, xic)
      pointer (ipyic, yic)
      pointer (ipzic, zic)
      real*8 xic(1000000), yic(1000000), zic(1000000)
      real*8 x1,x2,x3,x4,y1,y2,y3,y4,z1,z2,z3,z4,xrz,yrz,d
C
C#######################################################################
C
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
      istart=2
      nx=imsgin(istart)
         istart=istart+1
      ny=imsgin(istart)
         istart=istart+1
      x1=xmsgin(istart)
         istart=istart+1
      y1=xmsgin(istart)
         istart=istart+1
      z1=xmsgin(istart)
         istart=istart+1
      x2=xmsgin(istart)
         istart=istart+1
      y2=xmsgin(istart)
         istart=istart+1
      z2=xmsgin(istart)
         istart=istart+1
      x3=xmsgin(istart)
         istart=istart+1
      y3=xmsgin(istart)
         istart=istart+1
      z3=xmsgin(istart)
         istart=istart+1
      x4=xmsgin(istart)
         istart=istart+1
      y4=xmsgin(istart)
         istart=istart+1
      z4=xmsgin(istart)
         istart=istart+1
      if(nwds.le.15) then
         ixz=1
         iyz=1
      else
         ixz=imsgin(istart)
            istart=istart+1
         iyz=imsgin(istart)
            istart=istart+1
      endif
      if(nwds.le.17) then
        ixratio=0
        iyratio=0
        xrz=1.
        yrz=1.
      else
         ixratio=imsgin(istart)
            istart=istart+1
         iyratio=imsgin(istart)
            istart=istart+1
         xrz=xmsgin(istart)
            istart=istart+1
         yrz=xmsgin(istart)
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
      if(xrz.le.0.0) xrz=1.0
      if(yrz.le.0.0) yrz=1.0
      iprint=0
      if(ixz.eq.0.and.ixratio.ne.0) then
         iprint=1
         ixratio=0
         xrz=1.
      endif
         if(iyz.eq.0.and.iyratio.ne.0) then
         iprint=1
         ixratio=0
         yrz=1.
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
      endif
C
C     Set nnodes in mesh object and get pointers
C
      call cmo_set_info('nnodes',cmo,npoints+nx*ny,1,1,ierr2)
      call cmo_newlen(cmo,ierr2)
      call cmo_get_info('xic',cmo,ipxic,ilen,ityp,ierr2)
      call cmo_get_info('yic',cmo,ipyic,ilen,ityp,ierr2)
      call cmo_get_info('zic',cmo,ipzic,ilen,ityp,ierr2)
C
C     CREATE the points.
C
      call quadpts(x1,y1,z1,x2,y2,z2,x3,y3,z3,x4,y4,z4,nx,ny,xrz,yrz,
     x             xic(npoints+1),yic(npoints+1),zic(npoints+1))
 
C
C     ******************************************************************
C     PRINT OUT THE POINT NUMBERS GENERATED
C
      write(logmess,6000) npoints+1,npoints+nx*ny
 6000 format('  QUADXY GENERATED POINTS ',i9,' TO ',i9)
      call writloga('default',0,logmess,0,ierr)
C
      ipointi=npoints+1
      ipointj=npoints+nx*ny
      call cmo_get_name(cmo,ierr2)
C
      call set_info_i('ipointi',cmo,
     *                ipointi,1,1,ierr)
      if (ierr .ne. 0) call x3d_error(isubname,'cmo_get_info')
      call set_info_i('ipointj',cmo,
     *                ipointj,1,1,ierr)
      if (ierr .ne. 0) call x3d_error(isubname,'ccmo_get_info')
C
      goto 9999
 9999 continue
      return
      end
      subroutine getdelta (xi,yi,zi,xf,yf,zf,n,r,deltax,
     x                     deltay,deltaz)
C   ******************************************************
C
C   PURPOSE:
C     return proportional spacing delta
C
C  INPUT:
C     xi,yi,zi  coordinates of first point
C     xf,yf,zf  coordinates of last point
C     n         number of interval
C     r         ratio of i+1 interval distance to i interval
C
C  OUTPUT:
C     deltax,deltay,deltaz incremental distances to add to
C                          first point to get next point
C
C   *******************************************************
      implicit none
      real*8 xi,yi,zi,xf,yf,zf,r,deltax,deltay,deltaz
      integer n,i
      real*8 sum,xd,yd,zd
C
      xd=xf-xi
      yd=yf-yi
      zd=zf-zi
      if (n.le.1) then
         deltax=xd
         deltay=yd
         deltaz=zd
      else
         sum=0.0
         do i=1,n-1
            sum=sum+r**i
         enddo
         sum=sum+1.0
         deltax=xd/sum
         deltay=yd/sum
         deltaz=zd/sum
      endif
      return
      end
      subroutine quadpts (x1,y1,z1,x2,y2,z2,
     x                    x3,y3,z3,x4,y4,z4,
     x                    nx,ny,rx,ry,x,y,z)
C   ******************************************************
C
C   PURPOSE:
C     create nx by ny set of points using ratio spacing.
C
C  INPUT:
C     x1,y1,z1  coordinates of first point
C     x2,y2,z2  coordinates of second point (counterclockwise)
C     x3,y3,z3  coordinates of third point
C     x4,y4,z4  coordinates of last point
C     nx        number of points in x direction
C     ny        number of points in y direction
C     rx        ratio of i+1 interval distance to i interval
C                x-direction
C     rx        ratio of i+1 interval distance to i interval
C                y-direction
C
C  OUTPUT:
C     x,y,z	    coordinates of created points
C
C   ******************************************************
      implicit none
      real*8 x1,y1,z1,x2,y2,z2,x3,y3,z3,x4,y4,z4,rx,ry,
     x       x(*),y(*),z(*),
     x       deltax,deltay,deltaz,factor
      integer nx,ny,i,j,icount,icount1,icount2
C
C   get delta spacing for first nx points
C
      x(1)=x1
      y(1)=y1
      z(1)=z1
      icount=1
      if(nx.ge.2) then
         call getdelta(x1,y1,z1,x2,y2,z2,nx-1,rx,
     *              deltax,deltay,deltaz)
         factor=1.0
         do i=2,nx-1
            if(i.ge.3) factor=factor*rx
            icount=icount+1
            x(icount)=x(icount-1)+deltax*factor
            y(icount)=y(icount-1)+deltay*factor
            z(icount)=z(icount-1)+deltaz*factor
         enddo
         icount=icount+1
         x(icount)=x2
         y(icount)=y2
         z(icount)=z2
      endif
C
C   get delta spacing for last nx points
C
      if(ny.le.1) go to 9999
      if(nx.ge.2) then
         call getdelta(x4,y4,z4,x3,y3,z3,nx-1,rx,
     *              deltax,deltay,deltaz)
      else
         deltax=0.
         deltay=0.
         deltaz=0.
      endif
      icount=nx*(ny-1)+1
      x(icount)=x4
      y(icount)=y4
      z(icount)=z4
      if(nx.ge.2) then
         factor=1.0
         do i=2,nx-1
            if(i.ge.3) factor=factor*rx
            icount=icount+1
            x(icount)=x(icount-1)+deltax*factor
            y(icount)=y(icount-1)+deltay*factor
            z(icount)=z(icount-1)+deltaz*factor
         enddo
         icount=icount+1
         x(icount)=x3
         y(icount)=y3
         z(icount)=z3
      endif
C
C   fill in middle row by using getdelta with y ratio to get
C   end points of rows -- then use getdelta with x ratio
C   for x spacing.
C
      if(ny.gt.1) then
         call getdelta(x1,y1,z1,x4,y4,z4,ny-1,ry,
     *              deltax,deltay,deltaz)
         factor=1.0
         do i=2,ny-1
            if(i.ge.3) factor=factor*ry
            icount=nx*(i-1)+1
            x(icount)=x(icount-nx)+deltax*factor
            y(icount)=y(icount-nx)+deltay*factor
            z(icount)=z(icount-nx)+deltaz*factor
         enddo
         call getdelta(x2,y2,z2,x3,y3,z3,ny-1,ry,
     *              deltax,deltay,deltaz)
         factor=1.0
         do i=2,ny-1
            if(i.ge.3) factor=factor*ry
            icount=nx*i
            x(icount)=x(icount-nx)+deltax*factor
            y(icount)=y(icount-nx)+deltay*factor
            z(icount)=z(icount-nx)+deltaz*factor
         enddo
         do i=2,ny-1
            icount1=nx*(i-1)+1
            icount2=nx*i
            call getdelta(x(icount1),y(icount1),z(icount1),
     *              x(icount2),y(icount2),z(icount2),
     *              nx-1,rx,deltax,deltay,deltaz)
            factor=1.0
            do j=2,nx-1
               if(j.ge.3) factor=factor*rx
               icount1=icount1+1
               x(icount1)=x(icount1-1)+deltax*factor
               y(icount1)=y(icount1-1)+deltay*factor
               z(icount1)=z(icount1-1)+deltaz*factor
            enddo
         enddo
      endif
 9999 return
      end
