*deck aratio_tri
      subroutine aratio_tri(xl1,yl1,zl1,
     *                      xl2,yl2,zl2,
     *                      xl3,yl3,zl3,
     *                      arattri)
C
C
C #####################################################################
C
C     PURPOSE -
C
C        Finds the aspect ratio of a tri.
C
C     INPUT ARGUMENTS -
C
C        (x1,y1,z1),...,(x3,y3,z3) - The coordinates of the tri.
C
C     OUTPUT ARGUMENTS -
C
C        arattri : The aspect ratio of the tri.
C
C     CHANGE HISTORY -
C
C $Log: aratio_tri.f,v $
C Revision 2.00  2007/11/05 19:45:46  spchu
C Import to CVS
C
CPVCS    
CPVCS       Rev 1.1   Fri Jan 08 16:58:28 1999   kuprat
CPVCS    We now prevent zero divides using SAFE.
CPVCS    
CPVCS       Rev 1.0   Fri Aug 29 14:11:34 1997   dcg
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
      real*8 xl1,yl1,zl1,xl2,yl2,zl2,xl3,yl3,zl3
      real*8 arattri
      real*8 rcir
      real*8 rinsc
      real*8 ax4,ay4,az4,farea
      real*8 ds1,ds2,ds3

      include "statementfunctions.h"
C
C ######################################################################
C
      ax4 = (yl3 - yl1)*(zl2 - zl1)-(zl3-zl1)*(yl2-yl1)
      ay4 = -((xl3 - xl1)*(zl2 - zl1)-(zl3 - zl1)*
     *        (xl2 - xl1))
      az4 = (xl3 - xl1)*(yl2 - yl1)-(yl3 - yl1)*
     *      (xl2 - xl1)
C  radius of inscribed circle is 2*area of triangle/perimeter
C  radius of circumscribed circle is l1*l2*l3/4*area where l1,l2,l3 are edge
C  lengths
C
      farea=.5*sqrt(ax4**2+ay4**2+az4**2)
      ds1 = sqrt((xl3 - xl2)**2+(yl3 - yl2)**2+
     *           (zl3 - zl2)**2)
      ds2 = sqrt((xl1 - xl3)**2+(yl1 - yl3)**2+
     *           (zl1 - zl3)**2)
      ds3 = sqrt((xl2 - xl1)**2+(yl2 - yl1)**2 +
     *           (zl2 - zl1)**2)
      rinsc = 2.*farea/safe(ds1+ds2+ds3)
      rcir= ds1*ds2*ds3/safe(4.*farea)
 
      arattri= 2.*rinsc/safe(rcir)
 
      return
      end
