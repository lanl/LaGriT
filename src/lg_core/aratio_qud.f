*deck aratio_qud
      subroutine aratio_qud(xl1,yl1,zl1,
     *                      xl2,yl2,zl2,
     *                      xl3,yl3,zl3,
     *                      xl4,yl4,zl4,
     *                      aratqud)
C
C
C #####################################################################
C
C     PURPOSE -
C
C        Finds the aspect ratio of a quad.
C
C     INPUT ARGUMENTS -
C
C        (x1,y1,z1),...,(x4,y4,z4) - The coordinates of the quad.
C
C     OUTPUT ARGUMENTS -
C
C        aratqud : The aspect ratio of the quad.
C
C     CHANGE HISTORY -
C
C $Log: aratio_qud.f,v $
C Revision 2.00  2007/11/05 19:45:46  spchu
C Import to CVS
C
CPVCS    
CPVCS       Rev 1.0   Fri Aug 29 14:11:36 1997   dcg
CPVCS    Initial revision.
C
C ######################################################################
C
C
      implicit none
C
      include "local_element.h"
C
      real*8 xl1,yl1,zl1,xl2,yl2,zl2,xl3,yl3,zl3,xl4,yl4,zl4
      real*8 aratqud
      real*8 xb,yb,zb,xc,yc,zc
      real*8 smdiag,lardiag,dist1,dist2
C
C ######################################################################
C
C
C  Calculation of "dist", the length of the
C  diagonals of the quad.
C
      xb = xl1 - xl3
      yb = yl1 - yl3
      zb = zl1 - zl3
      xc = xl2 - xl4
      yc = yl2 - yl4
      zc = zl2 - zl4
      dist1 = sqrt ( xb*xb + yb*yb + zb*zb )
      dist2 = sqrt ( xc*xc + yc*yc + zc*zc )
      if (dist1 .lt. dist2) then
         smdiag = dist1
         lardiag = dist2
      else
         lardiag = dist1
         smdiag = dist2
      endif
 
      aratqud = smdiag/lardiag
 
      return
      end
