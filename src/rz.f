c
      subroutine rz(imsgin,xmsgin,cmsgin,msgtype,nwds,ierr2)
C
C
C#######################################################################
C
C      PURPOSE -
C
C     this routine is used to ratio zone a region of space
C
C
C     FORMAT: RZ/igeom/nx,ny,nz/xi,yi,zi/xf,yf,zf                      &
C     FORMAT:         /ixz/iyz/izz/ixratio/iyratio/izratio/xrz/yrz/zr
C
C
C
C      INPUT ARGUMENTS -
C        imsgin()  - Integer array of command input tokens
C        xmsgin()  - Real array of command input tokens
C        cmsgin()  - Character array of command input tokens
C        msgtype() - Integer array of command input token types
C        nwds      - Number of command input tokens
C
C     OUTPUT ARGUMENTS -
C
C        ierr2 - Error Return Code (==0 ==> OK, <>0 ==> Error)
C
C    LOCAL VARIABLES
C
C      igeom =  xyz ==> x , y , z cartesian geometry
C      igeom =  rtz ==> r , theta , z ==> cylindrical geometry
C      igeom =  rtp ==> r , theta , phi  ==> spherical geometry
C      igeom = line ==> interpolate nx points along line from
C               (xi,yi,zi)  to  (xf,yf,z)
C
C
C
C      nx -- IS THE NUMBER OF NODES (POINTS) IN THE X-DIRECTION.
C               > 0 ==> GENERATE POINTS IN A LINED UP FASHION.
C               < 0 ==> GENERARE POINTS IN A STAGGERED PATTEREN.
C      ny -- IS THE NUMBER OF NODES (POINTS) IN THE Y-DIRECTION.
C               > 0 ==> GENERATE POINTS IN A LINED UP FASHION.
C               < 0 ==> GENERARE POINTS IN A STAGGERED PATTEREN.
C      nz -- IS THE NUMBER OF NODES (POINTS) IN THE Z-DIRECTION.
C               > 0 ==> GENERATE POINTS IN A LINED UP FASHION.
C               < 0 ==> GENERARE POINTS IN A STAGGERED PATTEREN.
C
C
C                   cartesian  cylinderical       spherical
C                     x-y-z      r-theta-z       r-theta-phi
C                   *********  ************      ***********
C
C                          NOTE: ANGLES ARE IN DEGREES
C
C      xi -- minimum    x            r               r
C      yi -- minimum    y      theta(xy-plane)   theta(z-axis)
C      zi -- minimum    z            z           phi(x-axis,xy-plane)
C
C
C      xf -- maximum    x            r               r
C      yf -- maximum    y      theta(xy-plane)   theta(z-axis)
C      zf -- maximum    z            z           phi(x-axis,xy-plane)
C
C
C
C
C     THE REFLECTIVE OR ABSOLUTE ZONING SWITCHES:
C         THESE SWITCH INDICATE WHETHER POINTS ARE GENERATED AT
C            CELL CENTERES (REFLECTIVE POINTS) OR AT CELL VERTICES
C            (ABSOLUTE POINTS).
C
C         ixz = 0 ==> MIN AND MAX X-VALULE ARE USED AS REFLECTIVE
C                        BEGINNING AND ENDDING POINTS.
C         ixz = 1 ==> MIN AND MAX X-VALUES ARE USED AS ABSOLUTE
C                        BEGINNING AND ENDDING POINTS.
C
C         iyz = 0 ==> MIN AND MAX Y-VALULE ARE USED AS REFLECTIVE
C                        BEGINNING AND ENDDING POINTS.
C         iyz = 1 ==> MIN AND MAX Y-VALUES ARE USED AS ABSOLUTE
C                        BEGINNING AND ENDDING POINTS.
C
C         izz = 0 ==> MIN AND MAX Z-VALULE ARE USED AS REFLECTIVE
C                        BEGINNING AND ENDDING POINTS.
C         izz = 1 ==> MIN AND MAX Z-VALUES ARE USED AS ABSOLUTE
C                        BEGINNING AND ENDDING POINTS.
C
C     THE RATIO ZONING SWITCHES.
C
C         ixratio = 0  ==> NO RATIO ZONING IN THE X-DIRECTION.
C         ixratio = 1  ==> RATIO ZONING IN THE X-DIRECTION.
C
C         iyratio = 0  ==> NO RATIO ZONING IN THE Y-DIRECTION.
C         iyratio = 1  ==> RATIO ZONING IN THE Y-DIRECTION.
C
C         izratio = 0  ==> NO RATION ZONING IN THE Z-DIRECTION.
C         izratio = 1  ==> RATIO ZONING IN THE Z-DIRECTION.
C
C
C     THE RATIO ZONE VALUES (I.E., THE ZONING INCREASES OR DECREASES
C                                  BY THIS PERCENTAGE FROM ZONE TO
C                                  ZONE.)
C
C        xrz = THE RATIO ZONING VALUE FOR THE X-DIRECTION.
C        yrz = THE RATIO ZONING VALUE FOR THE Y-DIRECTION.
C        zrz = THE RATIO ZONING VALUE FOR THE Z-DIRECTION.
C
C
C
C      CHANGE HISTORY -
C
C        $Log:   /pvcs.config/t3d/src/rz.f_a  $
CPVCS    
CPVCS       Rev 1.27   29 Sep 2004 17:00:30   dcg
CPVCS    remove line that set value for pie - this is set in initlagrit
CPVCS
CPVCS       Rev 1.26   07 May 2003 10:13:00   gable
CPVCS    Fixed error that occured when createpts/line was a
CPVCS    line aligned with the y or z axis. Error resulted
CPVCS    in only cell centered point distributions. Now one
CPVCS    can get correct results when ijz=1 and ikz=1.
CPVCS
CPVCS       Rev 1.25   21 Mar 2002 09:15:40   dcg
CPVCS    fix errors in line mode
CPVCS
CPVCS       Rev 1.24   19 Mar 2001 13:49:08   dcg
CPVCS    make variable pie explicitly double precision
CPVCS
CPVCS       Rev 1.23   Thu Apr 06 13:59:24 2000   dcg
CPVCS    replace get_info_i set_info_i calls
CPVCS
CPVCS       Rev 1.22   26 Jan 2000 13:34:06   dcg
CPVCS    set nx, ny, nz to max (1,input value) to avoid
CPVCS    setting npoint_new to zero (nx*ny*nz)
CPVCS
CPVCS       Rev 1.21   Tue Oct 19 09:02:48 1999   dcg
CPVCS    change print format to allow for more nodes
CPVCS
CPVCS       Rev 1.20   Fri Sep 03 10:02:56 1999   gable
CPVCS    Changed i6 format to i10
CPVCS
CPVCS       Rev 1.19   Thu Jun 24 13:56:50 1999   dcg
CPVCS    get rid of unused variables
CPVCS
CPVCS       Rev 1.17   Fri Jan 22 16:41:34 1999   dcg
CPVCS    add include 'consts.h for subroutine gencurve
CPVCS
CPVCS       Rev 1.16   Fri Jun 19 09:40:12 1998   dcg
CPVCS    remove duplicate declarations
CPVCS
CPVCS       Rev 1.15   Wed May 20 11:12:40 1998   dcg
CPVCS    fix 'line' option
CPVCS
CPVCS       Rev 1.13   Mon Apr 14 17:00:22 1997   pvcs
CPVCS    No change.
CPVCS
CPVCS       Rev 1.12   Mon Feb 26 16:48:08 1996   dcg
CPVCS    IBM changes
CPVCS
CPVCS       Rev 1.11   Fri Feb 16 21:50:38 1996   het
CPVCS    Add the curve option.
CPVCS
CPVCS       Rev 1.10   Fri Dec 22 14:17:26 1995   het
CPVCS    Correct errors for inside_ routines.
CPVCS
CPVCS       Rev 1.9   11/20/95 09:09:50   dcg
CPVCS    fix argument type checks
CPVCS
CPVCS       Rev 1.8   11/17/95 15:22:20   dcg
CPVCS    replace literal character strings in calls
CPVCS
CPVCS       Rev 1.7   11/16/95 17:02:48   het
CPVCS    Fix an error with the ipointi/ipointj
CPVCS
CPVCS       Rev 1.6   09/20/95 15:35:00   dcg
CPVCS    HP changes
CPVCS
CPVCS       Rev 1.5   09/18/95 19:44:00   dcg
CPVCS    add argument type test
CPVCS
CPVCS       Rev 1.4   08/23/95 12:56:30   dcg
CPVCS    move cmo calls to subroutine rz from msgtty
CPVCS
CPVCS       Rev 1.3   07/17/95 16:02:08   dcg
CPVCS    activate coordsys changes for rm, rz commands
CPVCS
CPVCS       Rev 1.2   05/26/95 13:13:28   het
CPVCS    Replace subroutine parameter list with subroutine calles.
CPVCS
CPVCS       Rev 1.1   01/04/95 22:05:20   llt
CPVCS    unicos changes (made by het)
CPVCS
CPVCS       Rev 1.0   11/10/94 12:18:28   pvcs
CPVCS    Original version.
C
C#######################################################################
C
      implicit none
C
      include 'chydro.h'
      include 'consts.h'
C
      integer nwds, imsgin(nwds), msgtype(nwds)
      real*8 xmsgin(nwds)
      character*(*) cmsgin(nwds)
      character*132 logmess
C
      pointer (ipxic, xic)
      pointer (ipyic, yic)
      pointer (ipzic, zic)
      real*8 xic(*), yic(*), zic(*)
      pointer (ipitp1, itp1)
      pointer (ipimt1, imt1)
      pointer (ipicr1, icr1)
      integer itp1(*),imt1(*),icr1(*)
C
      character*32 cgeom, cmo, isubname
      integer ierr2,icscode,ipointi,ipointj,ilen,ityp,igeom2,
     * nx,ny,nz,ixratio,iyratio,izratio,ixz,iyz,izz,icount,
     * npoints,i,i1,nx1,ny1,nz1,npoints_new,npoints_save,
     * nx2,ny2,nz2,ix,iy,iz,icntin
      real*8 xi,yi,zi,xf,yf,zf,xrz,yrz,zrz,rsum,dx1,dy1,dz1,
     * x1,y1,z1,x11,y11,z11,x12,y12,z12,
     * r1,z2,xcen,ycen,zcen,ph1,dph,r2,r12,phf,ph11,ph12,dph1,
     *  phi,dth,th1,dth1,th11,dr,dr1,th12,r11,ri,rf,dx,dy,
     *  dz,d1,d2,dzrel1,rsumz,dyrel1,rsumy,rsumx,radtodeg,
     *  radius,angle,degtorad,thi,thf,dxabs1,dyabs1,dzabs1,dxrel1
      real*8 cvmgtr
C
C#######################################################################
C
      isubname='rz'
      ierr2=0
      call cmo_get_name(cmo,ierr2)
      call cmo_get_info('ipointi',cmo,
     *                ipointi,ilen,ityp,icscode)
      if (icscode .ne. 0) call x3d_error(isubname,'cmo_get_info')
      call cmo_get_info('ipointj',cmo,
     *                ipointj,ilen,ityp,icscode)
      if (icscode .ne. 0) call x3d_error(isubname,'cmo_get_info')
      call cmo_get_info('nnodes',cmo,npoints,ilen,ityp,ierr2)
C
      cgeom=cmsgin(2)
C
C     ******************************************************************
C
C     TO ALLOW FOR AN "OLD" GENERATION OPTION WE CAN SPECIFY THE
C        GENERATION COORDINATE SYSTEM BY NUMBER (1=XYZ, 2=RTZ, 3=RTP).
C
      igeom2=1
      if(cgeom(1:3).eq.'xyz') igeom2=1
      if(cgeom(1:3).eq.'rtz') igeom2=2
      if(cgeom(1:3).eq.'rtp') igeom2=3
      if(cgeom(1:5).eq.'curve') igeom2=4
      if(cgeom(1:5).eq.'line') igeom2=5
 
C
C     ******************************************************************
C
C     TRANSFORM POINTS AND VELOCITIES TO LOCAL COORD. SYSTEM
C
      if (normflgc .gt. 0) call chglocl(1,npoints,1)
C
      icount=ipointj
      call test_argument_type(3,1,3,imsgin,xmsgin,cmsgin,
     *                         msgtype,nwds)
      call test_argument_type(6,2,6,imsgin,xmsgin,cmsgin,
     *                         msgtype,nwds)
      call test_argument_type(3,1,12,imsgin,xmsgin,cmsgin,
     *                         msgtype,nwds)
      nx=max(1,imsgin(3))
      ny=max(1,imsgin(4))
      nz=max(1,imsgin(5))
      xi=xmsgin(6)
      yi=xmsgin(7)
      zi=xmsgin(8)
      xf=xmsgin(9)
      yf=xmsgin(10)
      zf=xmsgin(11)
      ixz=imsgin(12)
      iyz=imsgin(13)
      izz=imsgin(14)
      if(nwds.le.14) then
        ixratio=0
        iyratio=0
        izratio=0
        xrz=0.
        yrz=0.
        zrz=0.
      else
         call test_argument_type(3,2,18,imsgin,xmsgin,cmsgin,
     *                         msgtype,nwds)
         call test_argument_type(3,1,15,imsgin,xmsgin,cmsgin,
     *                         msgtype,nwds)
         ixratio=imsgin(15)
         iyratio=imsgin(16)
         izratio=imsgin(17)
         xrz=xmsgin(18)
         yrz=xmsgin(19)
         zrz=xmsgin(20)
      endif
C
      if(igeom2.eq.4) then
         radius=xmsgin(21)
         angle=xmsgin(22)
      endif
C
      npoints_new=iabs(nx)*iabs(ny)*iabs(nz)
      npoints_save=ipointj
      npoints=npoints+npoints_new
      call cmo_set_info('nnodes',cmo,npoints,1,1,ierr2)
      call cmo_newlen(cmo,ierr2)
      call cmo_get_info('xic',cmo,ipxic,ilen,ityp,ierr2)
      call cmo_get_info('yic',cmo,ipyic,ilen,ityp,ierr2)
      call cmo_get_info('zic',cmo,ipzic,ilen,ityp,ierr2)
      call cmo_get_info('itp1',cmo,ipitp1,ilen,ityp,ierr2)
      call cmo_get_info('imt1',cmo,ipimt1,ilen,ityp,ierr2)
      call cmo_get_info('icr1',cmo,ipicr1,ilen,ityp,ierr2)
C
C     ******************************************************************
C
C     CONVERT ANGLES TO RADIANS.
C
      icntin=icount
      degtorad=pie/180.0
      radtodeg=180.0/pie
C
C     ******************************************************************
C
C     FOR CYLINDRICAL COORDINATES WE CONVERT THE THETA ANGLE FROM
C        DEGREES TO RADIANS.
C
      if(igeom2.eq.2) then
         yi=yi*pie/180.0
         yf=yf*pie/180.0
      endif
C
C     ******************************************************************
C
C     FOR SPHERICAL COORDINATES WE CONVERT THE THETA AND PHI ANGLES FROM
C        DEGREES TO RADIANS.
C
      if(igeom2.eq.3) then
         yi=yi*pie/180.0
         yf=yf*pie/180.0
         zi=zi*pie/180.0
         zf=zf*pie/180.0
      endif
C
C     ******************************************************************
C
C     FILTER THE ZONING FLAGS AND MULTIPLIES TO ALLOW ONLY THE
C        PROPER VALUES.
C
      if(ixz.ne.0.and.ixz.ne.1) ixz=0
      if(iyz.ne.0.and.iyz.ne.1) iyz=0
      if(izz.ne.0.and.izz.ne.1) izz=0
      if(xrz.le.0.0) xrz=1.0
      if(yrz.le.0.0) yrz=1.0
      if(zrz.le.0.0) zrz=1.0
C
C     Do not reset ixz value if point distribution is along a line
C
      if(igeom2 .ne. 5)then
      if(xi.eq.xf.and.ixz.eq.1) ixz=0
      if(yi.eq.yf.and.iyz.eq.1) iyz=0
      if(zi.eq.zf.and.izz.eq.1) izz=0
      endif
C
C     ******************************************************************
C
C     SETUP THE STARTING VALUE AND DELTA FOR THE X-DIRECTION.
C
      nx1=iabs(nx)
      if(nx1.lt.2.and.ixz.gt.0) ixz=0
      if(ixz.eq.0) then
         if(nx1.eq.1) then
            rsum=0.0
         else
            rsum=0.0
            do 10 i1=1,nx1-1
               rsum=rsum+xrz**float(i1)
 10         continue
         endif
         rsumx=0.5+rsum+0.5*xrz**float(nx1+1)
         dxrel1=(xf-xi)/rsumx
         dx1=dxrel1
         x11=xi-0.5*dx1
      else
         if(nx1.eq.2) then
            rsum=0.0
         else
            rsum=0.0
            do 15 i1=1,nx1-2
               rsum=rsum+xrz**float(i1)
 15         continue
         endif
         rsumx=1.0+rsum
         dxabs1=(xf-xi)/rsumx
         dx1=dxabs1/xrz
         x11=xi-dx1
      endif
      x12=x11+0.5*dx1
C
C     ******************************************************************
C
C     SETUP THE STARTING VALUE AND DELTA FOR THE Y-DIRECTION.
C
      ny1=iabs(ny)
      if(ny1.lt.2.and.iyz.gt.0) iyz=0
      if(iyz.eq.0) then
         if(ny1.eq.1) then
            rsum=0.0
         else
            rsum=0.0
            do 20 i1=1,ny1-1
               rsum=rsum+yrz**float(i1)
 20         continue
         endif
         rsumy=0.5+rsum+0.5*yrz**float(ny1+1)
         dyrel1=(yf-yi)/rsumy
         dy1=dyrel1
         y11=yi-0.5*dy1
      else
         if(ny1.eq.2) then
            rsum=0.0
         else
            rsum=0.0
            do 25 i1=1,ny1-2
               rsum=rsum+yrz**float(i1)
 25         continue
         endif
         rsumy=1.0+rsum
         dyabs1=(yf-yi)/rsumy
         dy1=dyabs1/yrz
         y11=yi-dy1
      endif
      y12=y11+0.5*dy1
C
C     ******************************************************************
C
C     SETUP THE STARTING VALUE AND DELTA FOR THE Z-DIRECTION.
C
      nz1=iabs(nz)
      if(nz1.lt.2.and.izz.gt.0) izz=0
      if(izz.eq.0) then
         if(nz1.eq.1) then
            rsum=0.0
         else
            rsum=0.0
            do 30 i1=1,nz1-1
               rsum=rsum+zrz**float(i1)
 30         continue
         endif
         rsumz=0.5+rsum+0.5*zrz**float(nz1+1)
         dzrel1=(zf-zi)/rsumz
         dz1=dzrel1
         z11=zi-0.5*dz1
      else
         if(nz1.eq.2) then
            rsum=0.0
         else
            rsum=0.0
            do 35 i1=1,nz1-2
               rsum=rsum+zrz**float(i1)
 35         continue
         endif
         rsumz=1.0+rsum
         dzabs1=(zf-zi)/rsumz
         dz1=dzabs1/zrz
         z11=zi-dz1
      endif
      z12=z11+0.5*dz1
C
C     ******************************************************************
C
C     CHECK TO SEE IF THIS A 1D LINE IN 3D SPACE. IF SO THEN JUST
C        INTERPOLATE ALONG THE LINE.
      if(igeom2.eq.5) then
C
C
         nx1=iabs(nx)
      endif
C
C     ******************************************************************
C
C     NOW GO TO THE CORRECT GEOMETRY ROUTINE.
C
C
      goto (100 , 200 , 300, 400,500) igeom2
      goto 9999
C
C     ******************************************************************
C
C     DEFINE THE XYZ- COORDINATE SETUP. THE GENERATION ORDER IS:
C        1) INNER  LOOP IS FOR THE X-COORDINATES.
C        2) MIDDLE LOOP IS FOR THE Y-COORDINATES.
C        3) OUTER  LOOP IS FOR THE Z-COORDINATES.
C
 100  continue
      d1=0.0
      d2=0.0
      z1=z11
      z2=z12
      dz=dz1
      do 110 iz=1,nz1
         z1=z1+dz
         if(ny.lt.0) then
            if(mod(iz,2).ne.0) then
               ny2=ny1
               y1=y11
            else
               ny2=ny1-1
               y1=y12
            endif
         else
            ny2=ny1
            y1=y11
         endif
         dy=dy1
         do 120 iy=1,ny2
            y1=y1+dy
            if(nx.lt.0) then
               if(mod(iy,2).ne.0) then
                  nx2=nx1
                  x1=x11
               else
                  nx2=nx1-1
                  x1=x12
               endif
            else
               nx2=nx1
               x1=x11
            endif
            dx=dx1
            do 130 ix=1,nx2
               x1=x1+dx
               icount=icount+1
               xic(icount)=x1
               yic(icount)=y1
               zic(icount)=z1
               dx=cvmgtr(dx*xrz,dx,ixratio.eq.1)
 130        continue
            dy=cvmgtr(dy*yrz,dy,iyratio.eq.1)
 120     continue
         dz=cvmgtr(dz*zrz,dz,izratio.eq.1)
 110  continue
      goto 9998
C
C     ******************************************************************
C
C     DEFINE THE RTZ-COORDINATE SETUP FOR CYLINDRICAL GEOMETRY. THE
C        GENERATION ORDER IS:
C           1) INNER  LOOP IS FOR THE RADIAL-COORDINATES.
C           2) MIDDLE LOOP IS FOR THE  THETA-COORDINATES.
C           3) OUTER  LOOP IS FOR THE      Z-COORDINATES.
C
 200  continue
      d1=0.0
      d2=0.0
      ri=xi
      rf=xf
      thi=yi
      thf=yf
      r11 =x11
      r12 =x12
      dr1 =dx1
      th11=y11
      th12=y12
      dth1=dy1
      z1  =z11
      z2  =z12
      dz  =dz1
      do 210 iz=1,nz1
         z1=z1+dz
         if(nx.lt.0) then
            if(mod(iz,2).ne.0) then
               nx2=nx1
               r1=r11
            else
               nx2=nx1-1
               r1=r12
            endif
         else
            nx2=nx1
            r1=r11
         endif
         dr=dr1
         do 220 ix=1,nx2
            r1=r1+dr
            if(ny.lt.0) then
               if(mod(ix,2).ne.0) then
                  ny2=ny1
                  th1=th11
               else
                  ny2=ny1-1
                  if(thf.eq.2.0*pie) ny2=ny1
                  th1=th12
               endif
            else
               ny2=ny1
               th1=th11
            endif
            dth=dth1
            do 230 iy=1,ny2
               th1=th1+dth
               x1=r1*cos(th1)
               y1=r1*sin(th1)
               icount=icount+1
C*****         call flgrid(2,icount,x1,y1,z1,0.0,0.0,0.0)
               xic(icount)=x1
               yic(icount)=y1
               zic(icount)=z1
               dth=cvmgtr(dth*yrz,dth,iyratio.eq.1)
 230        continue
            dr=cvmgtr(dr*xrz,dr,ixratio.eq.1)
 220     continue
         dz=cvmgtr(dz*zrz,dz,izratio.eq.1)
 210  continue
      goto 9998
C
C     ******************************************************************
C
C     DEFINE THE RTP-COORDINATE SETUP FOR SPHERICAL GEOMETRY. THE
C        GENERATION ORDER IS:
C           1) INNER  LOOP IS FOR THE RADIAL-COORDINATES.
C           2) MIDDLE LOOP IS FOR THE  THETA-COORDINATES.
C           3) OUTER  LOOP IS FOR THE    PHI-COORDINATES.
C
 300  continue
      d1=0.0
      d2=0.0
      ri=xi
      rf=xf
      thi=yi
      thf=yf
      phi=zi
      phf=zf
      r11 =x11
      dr1 =dx1
      th11=y11
      th12=y12
      dth1=dy1
      ph11=z11
      ph12=z12
      dph1=dz1
      r1=r11
      r2=r12
      dr=dr1
      do 310 ix=1,nx1
         r1=r1+dr
         if(ny.lt.0) then
            if(mod(ix,2).ne.0) then
               ny2=ny1
               th1=th11
            else
               ny2=ny1-1
               if(thf.eq.2.0*pie) ny2=ny1
               th1=th12
            endif
         else
            ny2=ny1
            th1=th11
         endif
         dth=dth1
         do 320 iy=1,ny2
            th1=th1+dth
            if(nz.lt.0) then
               if(mod(iy,2).ne.0) then
                  nz2=nz1
                  ph1=ph11
               else
                  nz2=nz1-1
                  if(phf.eq.2.0*pie) nz2=nz1
                  ph1=ph12
               endif
            else
               nz2=nz1
               ph1=ph11
            endif
            dph=dph1
            do 330 iz=1,nz2
               ph1=ph1+dph
               x1=r1*sin(th1)*cos(ph1)
               y1=r1*sin(th1)*sin(ph1)
               z1=r1*cos(th1)
               icount=icount+1
C*****         call flgrid(2,icount,x1,y1,z1,0.0,0.0,0.0)
               xic(icount)=x1
               yic(icount)=y1
               zic(icount)=z1
               dph=cvmgtr(dph*zrz,dph,izratio.eq.1)
 330        continue
            dth=cvmgtr(dth*yrz,dth,iyratio.eq.1)
 320     continue
         dr=cvmgtr(dr*xrz,dr,ixratio.eq.1)
 310  continue
      goto 9998
C
C     ******************************************************************
C
C
 400  continue
      call gencurve(icount,nx,
     *              xic,yic,zic,
     *              xcen,ycen,zcen,
     *              xi,yi,zi,xf,yf,zf,
     *              radius,angle)
      icount=icount+nx
      goto 9998
C
C     ******************************************************************
C    Distribute points along the line from (xi,yi,zi) to (xf,yf,zf)
C
 500  if(nx1.eq.1) then
        icount=icount+1
        if(ixratio.eq.1) then
           xic(icount)=0.5*(xi+xf)
           yic(icount)=0.5*(yi+yf)
           zic(icount)=0.5*(zi+zf)
        else
           xic(icount)= xi
           yic(icount)= yi
           zic(icount)= zi
        endif
      else
C
C  check for ratio spacing
C
         if(ixratio.eq.0) then
c
C  no ratio spacing check for end point or cell centered
C
            if (ixz.eq.1) then
C  end points
               dx1=(xf-xi)/(nx1-1)
               dy1=(yf-yi)/(nx1-1)
               dz1=(zf-zi)/(nx1-1)
               icount=icount+1
               xic(icount)= xi
               yic(icount)= yi
               zic(icount)= zi
               do i=1,nx1-2
                  icount=icount+1
                  xic(icount)=xi+i*dx1
                  yic(icount)=yi+i*dy1
                  zic(icount)=zi+i*dz1
               enddo
               icount=icount+1
               xic(icount)= xf
               yic(icount)= yf
               zic(icount)= zf
            else
C  cell centered
               dx1=(xf-xi)/(nx1)
               dy1=(yf-yi)/(nx1)
               dz1=(zf-zi)/(nx1)
               icount=icount+1
               xic(icount)= xi+0.5*dx1
               yic(icount)= yi+0.5*dy1
               zic(icount)= zi+0.5*dz1
               do i=1,nx1-2
                  icount=icount+1
                  xic(icount)=xi+(i+0.5)*dx1
                  yic(icount)=yi+(i+0.5)*dy1
                  zic(icount)=zi+(i+0.5)*dz1
               enddo
               icount=icount+1
               xic(icount)= xf-0.5*dx1
               yic(icount)= yf-0.5*dy1
               zic(icount)= zf-0.5*dz1
            endif
         else
c
C  ratio spacing
C
            if(ixz.eq.1) then
C  end points
C  get length of first space
               icount=icount+1
               xic(icount)= xi
               yic(icount)= yi
               zic(icount)= zi
               x1=xi
               y1=yi
               z1=zi
               rsum=1.0
               do i=1,nx1-2
                  rsum=rsum+xrz**float(i)
               enddo
               dx1=(xf-xi)/rsum
               dy1=(yf-yi)/rsum
               dz1=(zf-zi)/rsum
               do i=1,nx1-2
                  icount=icount+1
                  xic(icount)=x1+dx1
                  yic(icount)=y1+dy1
                  zic(icount)=z1+dz1
                  x1=  xic(icount)
                  y1=  yic(icount)
                  z1=  zic(icount)
                  dx1=xrz*dx1
                  dy1=xrz*dy1
                  dz1=xrz*dz1
               enddo
               icount=icount+1
               xic(icount)= xf
               yic(icount)= yf
               zic(icount)= zf
           else
C  cell centered
               rsum=1.0
               do i=1,nx1-2
                  rsum=rsum+xrz**float(i)
               enddo
               rsum=0.5+rsum+xrz**float(nx1-1)
               dx1=(xf-xi)/rsum
               dy1=(yf-yi)/rsum
               dz1=(zf-zi)/rsum
               icount=icount+1
               xic(icount)= xi+0.5*dx1
               yic(icount)= yi+0.5*dy1
               zic(icount)= zi+0.5*dz1
               x1=xi+0.5*dx1
               y1=yi+0.5*dy1
               z1=zi+0.5*dz1
               do i=1,nx1-1
                  icount=icount+1
                  xic(icount)=x1+dx1
                  yic(icount)=y1+dy1
                  zic(icount)=z1+dz1
                  x1=  xic(icount)
                  y1=  yic(icount)
                  z1=  zic(icount)
                  dx1=xrz*dx1
                  dy1=xrz*dy1
                  dz1=xrz*dz1
               enddo
            endif
         endif
      endif
      go to 9998
 
C
C     ******************************************************************
C     PRINT OUT THE POINT NUMBERS GENERATED
C
 9998 write(logmess,6000) icntin+1,icount
 6000 format('  RZ GENERATED POINTS ',i20,' TO ',i20)
      call writloga('default',0,logmess,0,ierr2)
c
c     Initialize ipt1, imt1 and icr1 to zero for new nodes.
C
      do i=icntin+1,icount
         itp1(i)=0
         imt1(i)=0
         icr1(i)=0
      enddo
      goto 9999
 9999 continue
      ipointi=npoints_save+1
      ipointj=icount
      call cmo_get_name(cmo,ierr2)
C
      npoints=ipointj
      call cmo_set_info('nnodes',cmo,npoints,1,1,ierr2)
C
      call cmo_set_info('ipointi',cmo,
     *                ipointi,1,1,icscode)
      if (icscode .ne. 0) call x3d_error(isubname,'cmo_set_info')
      call cmo_set_info('ipointj',cmo,
     *                ipointj,1,1,icscode)
      if (icscode .ne. 0) call x3d_error(isubname,'cmo_set_info')
C
C     TRANSFORM POINTS AND VELOCITIES TO NORMAL COORD. SYSTEM
C
      if (normflgc .gt. 0) call chgnorm(1,npoints,1)
C
C     ******************************************************************
C
      return
      end
      subroutine gencurve(icount,npoints,xp,yp,zp,xcen,ycen,zcen,
     *                    xstart,ystart,zstart,xend,yend,zend,
     *                    radius,angle)
C
C#######################################################################
C
      implicit real*8 (a-h, o-z)
      include 'consts.h'
C
      real*8 xp(npoints), yp(npoints), zp(npoints)
      x_deg_to_rad = acos(-1.0d+00)/180.0
      dz=(zend-zstart)/(npoints-1)
      z1=-dz
      dangle=angle/(npoints-1)
      a1=-dangle
      do i=1,npoints
         z1=z1+dz
         a1=a1+dangle
         xp(icount+i)=radius*cos(a1*x_deg_to_rad)
         yp(icount+i)=radius*sin(a1*x_deg_to_rad)
         zp(icount+i)=z1
      enddo
      do i=1,npoints
         xp(icount+i)=xp(icount+i)-radius
      enddo
      amag=sqrt((xend-xstart)**2+(yend-ystart)**2)
      bmag=sqrt((xp(icount+npoints)-xp(icount+1))**2+
     *          (yp(icount+npoints)-yp(icount+1))**2)
      xdot=(xend-xstart)*(xp(icount+npoints)-xp(icount+1)) +
     *     (yend-ystart)*(yp(icount+npoints)-yp(icount+1))
      theta=acos(min(one,max(-one,xdot/(amag*bmag))))
      x1=0.0
      y1=0.0
      z1=1.0d+06
      do i=1,npoints
         call rotatelo(xp(icount+i),yp(icount+i),zp(icount+i),
     *                 xcp,ycp,zcp,
     *                 x1,y1,+z1,
     *                 x1,y1,-z1,
     *                 theta)
         xp(icount+i)=xcp
         yp(icount+i)=ycp
         zp(icount+i)=zcp
      enddo
      do i=1,npoints
         xp(icount+i)=xp(icount+i)+xstart
         yp(icount+i)=yp(icount+i)+ystart
         zp(icount+i)=zp(icount+i)+zstart
      enddo
      xcen=-radius
      ycen=0.0
      zcen=0.0
      x1=0.0
      y1=0.0
      z1=1.0
      call rotatelo(xcen,ycen,zcen,
     *              xcp,ycp,zcp,
     *              x1,y1,+z1,
     *              x1,y1,-z1,
     *              theta)
      xcen=xcp+xstart
      ycen=ycp+ystart
      zcen=zcp
      goto 9999
 9999 continue
      return
      end
