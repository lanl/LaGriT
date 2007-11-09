*DK rotatepr
      subroutine rotatepr(xa,ya,za,x1,y1,z1,th1,ph1,xp,yp,zp)
C
C
C #####################################################################
C
C     PURPOSE -
C
C        None
C
C     INPUT ARGUMENTS -
C
C        None
C
C     OUTPUT ARGUMENTS -
C
C        None
C
C     CHANGE HISTORY -
C
C        $Log:   /pvcs.config/t3d/src/rotatepr.f_a  $
CPVCS    
CPVCS       Rev 1.3   30 Sep 2004 11:08:36   dcg
CPVCS    make implicit none
CPVCS    use value of pie from chydro.h
CPVCS
CPVCS       Rev 1.2   Mon Apr 14 17:00:08 1997   pvcs
CPVCS    No change.
CPVCS
CPVCS       Rev 1.1   01/04/95 22:05:12   llt
CPVCS    unicos changes (made by het)
CPVCS
CPVCS       Rev 1.0   11/10/94 12:18:24   pvcs
CPVCS    Original version.
C
C ######################################################################
C
      implicit none
      include 'chydro.h'
      real*8 xa,ya,za,x1,y1,z1,th1,ph1,xp,yp,zp,
     *   phi,thi,phir,thir,ph2,th2,radius,xr,yr,zr
C
ccht
ccht
ccht this routine is used to rotate point '1' about point 'a'
ccht through the angles 'th1' and 'ph1' to find the new point 'p'.
ccht
ccht   note: th1 is with respect to the z-axes
ccht         ph1 is with restpec to the x-axis in the xy-plane
ccht
ccht
      phi=ph1*pie/180.0
      thi=th1*pie/180.0
      call angle3v(xa,ya,za,x1,y1,z1,phir,thir)
      ph2=phi+phir
      th2=thi+thir
      radius=sqrt((x1-xa)**2+(y1-ya)**2+(z1-za)**2)
      xr=radius*cos(phir)*sin(thir)
      yr=radius*sin(phir)*sin(thir)
      zr=radius*cos(thir)
      xp=radius*cos(ph2)*sin(th2)+xa
      yp=radius*sin(ph2)*sin(th2)+ya
      zp=radius*cos(th2)+za
      return
      end
