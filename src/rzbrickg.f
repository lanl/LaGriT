*DK rzbrickg
      subroutine rzbrickg(cgeom , icount ,
     *  nx , ny , nz ,
     *  xi , yi , zi ,
     *  xf , yf , zf ,
     *  ixz , iyz , izz ,
     *  ixratio , iyratio , izratio ,
     *  xrz , yrz , zrz ,
     *  ipxic, ipyic, ipzic)
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
C
C      igeom = 0(or xyz) ==> x , y , z cartesian geometry
C      igeom = 1(or rtz) ==> r , theta , z ==> cylindrical geometry
C      igeom = 2(or rtp) ==> r , theta , phi  ==> spherical geometry
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
C      OUTPUT ARGUMENTS -
C
C         NONE
C
C
C      CHANGE HISTORY -
C
C         HT0106AC-87, HT0404AB-87, JP0803AA-87, HT1023AA-87
C         HT1117AA-87
C
C
C#######################################################################
C
      implicit real*8 (a-h, o-z)
      include 'chydro.h'
C
C
      character cgeom*(*)
C
      pointer (ipxic, xic)
      pointer (ipyic, yic)
      pointer (ipzic, zic)
      dimension xic(10000000), yic(10000000), zic(10000000)
C
C#######################################################################
C
C
C     ******************************************************************
C
C     CONVERT ANGLES TO RADIANS.
C
      degtorad=pie/180.0
      radtodeg=180.0/pie
C
C     ******************************************************************
C
C     TO ALLOW FOR AN "OLD" GENERATION OPTION WE CAN SPECIFY THE
C        GENERATION COORDINATE SYSTEM BY NUMBER (1=XYZ, 2=RTZ, 3=RTP).
C
      igeom2=0
      if(cgeom(1:3).eq.'xyz') igeom2=1
      if(cgeom(1:3).eq.'rtz') igeom2=2
      if(cgeom(1:3).eq.'rtp') igeom2=3
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
      if(ixratio.eq.0) xrz=1.0
      if(iyratio.eq.0) yrz=1.0
      if(izratio.eq.0) zrz=1.0
 
C
C     ******************************************************************
C
C     SETUP THE STARTING VALUE AND DELTA FOR THE X-DIRECTION.
C
      nx1=iabs(nx)
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
         if(nx1.lt.2) nx1=2
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
         if(ny1.lt.2) ny1=2
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
         if(nz1.lt.2) nz1=2
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
C     NOW GO TO THE CORRECT GEOMETRY ROUTINE.
C
      goto (100 , 200 , 300) igeom2
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
C*****         call flgrid(2,icount,x1,y1,z1,0.0,0.0,0.0)
               xic(icount)=x1
               yic(icount)=y1
               zic(icount)=z1
               dx=cvmgtr(dx*xrz,dx,ixratio.eq.1)
 130        continue
            dy=cvmgtr(dy*yrz,dy,iyratio.eq.1)
 120     continue
         dz=cvmgtr(dz*zrz,dz,izratio.eq.1)
 110  continue
      goto 9999
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
         if(ny.lt.0) then
            if(mod(iz,2).ne.0) then
               ny2=ny1
               th1=th11
            else
               ny2=ny1-1
               if(thf.eq.2.0*pie) nx2=nx1
               th1=th12
            endif
         else
            ny2=ny1
            th1=th11
         endif
         dth=dth1
         do 220 iy=1,ny2
            th1=th1+dth
            if(nx.lt.0) then
               if(mod(iy,2).ne.0) then
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
            do 230 ix=1,nx2
               r1=r1+dr
               x1=r1*cos(th1)
               y1=r1*sin(th1)
               icount=icount+1
C*****         call flgrid(2,icount,x1,y1,z1,0.0,0.0,0.0)
               xic(icount)=x1
               yic(icount)=y1
               zic(icount)=z1
               dr=cvmgtr(dr*xrz,dr,ixratio.eq.1)
 230        continue
            dth=cvmgtr(dth*yrz,dth,iyratio.eq.1)
 220     continue
         dz=cvmgtr(dz*zrz,dz,izratio.eq.1)
 210  continue
      goto 9999
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
      ph1=z11
      dph1=dz1
      r1=r11
      r2=r12
      dph=dph1
      do 310 iz=1,nz1
         ph1=ph1+dph
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
            if(nx.lt.0) then
               if(mod(iy,2).ne.0) then
                  nx2=nx1
                  r1=r11
               else
                  nx2=nx1-1
C*****************if(phf.eq.2.0*pie) nz2=nz1
                  r1=r12
               endif
            else
               nx2=nx1
               r1=r11
            endif
            dr=dr1
            do 330 ix=1,nx2
               r1=r1+dr
               x1=r1*sin(th1)*cos(ph1)
               y1=r1*sin(th1)*sin(ph1)
               z1=r1*cos(th1)
               icount=icount+1
C*****         call flgrid(2,icount,x1,y1,z1,0.0,0.0,0.0)
               xic(icount)=x1
               yic(icount)=y1
               zic(icount)=z1
               dr=cvmgtr(dr*xrz,dr,ixratio.eq.1)
 330        continue
            dth=cvmgtr(dth*yrz,dth,iyratio.eq.1)
 320     continue
         dph=cvmgtr(dph*zrz,dph,izratio.eq.1)
 310  continue
C
C     ******************************************************************
C
C     SET UP THE USUAL CFT IMMUNE STATEMENT 9999 IN CASE DDT IS NEEDED.
C
      goto 9999
 9999 continue
C
C     ******************************************************************
C
      return
      end
