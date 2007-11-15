*deck aratio_hex
      subroutine aratio_hex(xl1,yl1,zl1,
     *                      xl2,yl2,zl2,
     *                      xl3,yl3,zl3,
     *                      xl4,yl4,zl4,
     *                      xl5,yl5,zl5,
     *                      xl6,yl6,zl6,
     *                      xl7,yl7,zl7,
     *                      xl8,yl8,zl8,
     *                      arathex)
C
C
C #####################################################################
C
C     PURPOSE -
C
C        Finds the aspect ratio of a hex.
C
C     INPUT ARGUMENTS -
C
C        (x1,y1,z1),...,(x8,y8,z8) - The coordinates of the hex.
C
C     OUTPUT ARGUMENTS -
C
C        arathex : The aspect ratio of the hex.
C
C     CHANGE HISTORY -
C
C$Log: aratio_hex.f,v $
CRevision 2.00  2007/11/05 19:45:46  spchu
CImport to CVS
C
CPVCS    
CPVCS       Rev 1.0   Fri Aug 29 14:11:30 1997   dcg
CPVCS    Initial revision.
C
C ######################################################################
C
C
      implicit none
C
      include "local_element.h"
C
      real*8 xl1,yl1,zl1,xl2,yl2,zl2,xl3,yl3,zl3,xl4,yl4,zl4,
     *       xl5,yl5,zl5,xl6,yl6,zl6,xl7,yl7,zl7,xl8,yl8,zl8
      real*8 arathex
      real*8 xb,yb,zb,xc,yc,zc,xd,yd,zd,xe,ye,ze
      real*8 smdiag,lardiag,dist
 
      integer i
 
      dimension dist(4)
 
C
C ######################################################################
C
C
C  Calculation of "dist", the length of the
C  diagonals of the quad.
C
 
      xb = xl1 - xl7
      yb = yl1 - yl7
      zb = zl1 - zl7
      xc = xl2 - xl8
      yc = yl2 - yl8
      zc = zl2 - zl8
      xd = xl3 - xl5
      yd = yl3 - yl5
      zd = zl3 - zl5
      xe = xl4 - xl6
      ye = yl4 - yl6
      ze = zl4 - zl6
 
      dist(1) = sqrt ( xb*xb + yb*yb + zb*zb )
      dist(2) = sqrt ( xc*xc + yc*yc + zc*zc )
      dist(3) = sqrt ( xd*xd + yd*yd + zd*zd )
      dist(4) = sqrt ( xe*xe + ye*ye + ze*ze )
 
      smdiag = dist(1)
      lardiag = dist(1)
      do 21 i=2,4
         if(dist(i) .ge. lardiag) lardiag = dist(i)
         if(dist(i) .le. smdiag) smdiag = dist(i)
 21   continue
 
      arathex = smdiag/lardiag
 
      return
      end
