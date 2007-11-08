*deck aratio_tet
      subroutine aratio_tet(xl1,yl1,zl1,
     *                      xl2,yl2,zl2,
     *                      xl3,yl3,zl3,
     *                      xl4,yl4,zl4,
     *                      arattet,epsilon_notused)
C
C
C #####################################################################
C
C     PURPOSE -
C
C        Finds the aspect ratio of a tet.
C
C     INPUT ARGUMENTS -
C
C        (x1,y1,z1),...,(x4,y4,z4) - The coordinates of the tet.
C        epsilon_notused                   - result if calculation is singular.
C
C     OUTPUT ARGUMENTS -
C
C        arattet : The aspect ratio of the tet.
C
C     CHANGE HISTORY -
C
C $Log: aratio_tet.f,v $
C Revision 2.00  2007/11/05 19:45:46  spchu
C Import to CVS
C
CPVCS    
CPVCS       Rev 1.2   Fri Jan 08 16:59:12 1999   kuprat
CPVCS    We now prevent zero divides using SAFE.
CPVCS    
CPVCS       Rev 1.1   Wed Dec 23 11:50:24 1998   kuprat
CPVCS    We now properly handle singular tets.
CPVCS    
CPVCS       Rev 1.0   Fri Aug 29 14:11:32 1997   dcg
CPVCS    Initial revision.
C
C ######################################################################
C
C
      implicit none
C
      include "consts.h"
      include "local_element.h"
C
C      character*132 logmess
C      integer icscode
 
      real*8 xl1,yl1,zl1,xl2,yl2,zl2,xl3,yl3,zl3,xl4,yl4,zl4
      real*8 arattet
      real*8 xb,yb,zb,xc,yc,zc,xd,yd,zd,xn,yn,zn,x2,y2,z2,q,
     * xa,ya,za,dvor,qvor2,rcir
      real*8 ac1,bc1,cc1,dc1,ac2,bc2,cc2,dc2,ac3,bc3,cc3,dc3,
     *   ac4,bc4,cc4,dc4,dn1,dn2,dn3,dn4
      real*8 a11,a12,a13,d1,a21,a22,a23,a31,a32,a33,d3,d2
      real*8 qdet,rx,ry,rz,rinsc
 
      real*8 epsilon_notused
      real*8 a,d
      real*8 x1,x3,y1,y3,z1,z3
 
C  MACROS.
C
      include "statementfunctions.h"
      a(x1,x2,x3,y1,y2,y3) = x1*(y2-y3) - y1*(x2-x3) + x2*y3 - y2*x3
      d(x1,x2,x3,y1,y2,y3,z1,z2,z3) = y1*(x2*z3 - z2*x3)
     &         - x1*(y2*z3 - z2*y3) - z1*(x2*y3 - y2*x3)
C
C ######################################################################
C
C
C  Calculation of "rcir", the radius of the
C  circumscribed sphere of the tet.
C
      xb = xl3 - xl2
      yb = yl3 - yl2
      zb = zl3 - zl2
      xc = xl4 - xl2
      yc = yl4 - yl2
      zc = zl4 - zl2
      xd = xl1 - xl2
      yd = yl1 - yl2
      zd = zl1 - zl2
      xn =   yb*zc - yc*zb
      yn = -(xb*zc - xc*zb)
      zn =   xb*yc - xc*yb
      x2 =   yn*zb - yb*zn
      y2 = -(xn*zb - xb*zn)
      z2 =   xn*yb - xb*yn
      q = -0.5*(xc*xb+yc*yb+zc*zb-xc*xc-yc*yc-zc*zc)/
     *          safe(x2*xc+y2*yc+z2*zc)
      xa = q*x2 + 0.5*xb
      ya = q*y2 + 0.5*yb
      za = q*z2 + 0.5*zb
      dvor = -0.5*(xd*xd + yd*yd + zd*zd)
      qvor2 = -(xd*xa+yd*ya+zd*za+dvor)/
     *         safe(xd*xn+yd*yn+zd*zn)
 
      rcir = sqrt( (qvor2*xn + xa)**2
     *           + (qvor2*yn + ya)**2
     *           + (qvor2*zn + za)**2 )
C
C  Calculation of "rinsc", the radius of the inscribed
C  sphere of the tet.
C
      ac1 = a(yl2,yl4,yl3,zl2,zl4,zl3)
      bc1 = a(zl2,zl4,zl3,xl2,xl4,xl3)
      cc1 = a(xl2,xl4,xl3,yl2,yl4,yl3)
      dc1 = d(xl2,xl4,xl3,yl2,yl4,yl3,zl2,zl4,zl3)
      ac2 = a(yl1,yl4,yl2,zl1,zl4,zl2)
      bc2 = a(zl1,zl4,zl2,xl1,xl4,xl2)
      cc2 = a(xl1,xl4,xl2,yl1,yl4,yl2)
      dc2 = d(xl1,xl4,xl2,yl1,yl4,yl2,zl1,zl4,zl2)
      ac3 = a(yl1,yl2,yl3,zl1,zl2,zl3)
      bc3 = a(zl1,zl2,zl3,xl1,xl2,xl3)
      cc3 = a(xl1,xl2,xl3,yl1,yl2,yl3)
      dc3 = d(xl1,xl2,xl3,yl1,yl2,yl3,zl1,zl2,zl3)
      ac4 = a(yl1,yl3,yl4,zl1,zl3,zl4)
      bc4 = a(zl1,zl3,zl4,xl1,xl3,xl4)
      cc4 = a(xl1,xl3,xl4,yl1,yl3,yl4)
      dc4 = d(xl1,xl3,xl4,yl1,yl3,yl4,zl1,zl3,zl4)
      dn1 = sqrt(ac1**2 + bc1**2 + cc1**2)
      dn2 = sqrt(ac2**2 + bc2**2 + cc2**2)
      dn3 = sqrt(ac3**2 + bc3**2 + cc3**2)
      dn4 = sqrt(ac4**2 + bc4**2 + cc4**2)
 
      ac1 = ac1/safe(dn1)
      bc1 = bc1/safe(dn1)
      cc1 = cc1/safe(dn1)
      dc1 = dc1/safe(dn1)
      ac2 = ac2/safe(dn2)
      bc2 = bc2/safe(dn2)
      cc2 = cc2/safe(dn2)
      dc2 = dc2/safe(dn2)
      ac3 = ac3/safe(dn3)
      bc3 = bc3/safe(dn3)
      cc3 = cc3/safe(dn3)
      dc3 = dc3/safe(dn3)
      ac4 = ac4/safe(dn4)
      bc4 = bc4/safe(dn4)
      cc4 = cc4/safe(dn4)
      dc4 = dc4/safe(dn4)
 
      a11 = ac1 - ac2
      a12 = bc1 - bc2
      a13 = cc1 - cc2
      d1  = dc2 - dc1
      a21 = ac1 - ac3
      a22 = bc1 - bc3
      a23 = cc1 - cc3
      d2  = dc3 - dc1
      a31 = ac1 - ac4
      a32 = bc1 - bc4
      a33 = cc1 - cc4
      d3  = dc4 - dc1
 
      qdet = (a12*a23 - a13*a22)*a31
     *     + (a13*a21 - a11*a23)*a32
     *     + (a11*a22 - a12*a21)*a33
 
      rx = ( (a22*a33 - a23*a32)*d1
     *   +   (a13*a32 - a12*a33)*d2
     *   +   (a12*a23 - a13*a22)*d3 )/safe(qdet)
 
      ry = ( (a23*a31 - a21*a33)*d1
     *   +   (a11*a33 - a13*a31)*d2
     *   +   (a13*a21 - a11*a23)*d3 )/safe(qdet)
 
      rz = ( (a21*a32 - a22*a31)*d1
     *     + (a12*a31 - a11*a32)*d2
     *     + (a11*a22 - a12*a21)*d3 )/safe(qdet)
 
      rinsc =  ac1*rx + bc1*ry + cc1*rz + dc1
 
      arattet = dabs(3.d0*rinsc/safe(rcir))
 
c$$$      if (.not.(arattet.ge.epsilon_notused.and.arattet.le.1.d50)) then
c$$$         arattet=epsilon_notused
c$$$      endif 

      return
      end
