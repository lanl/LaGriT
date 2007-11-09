*deck xyznorm
      subroutine xyznorm(x1,y1,z1,x2,y2,z2)
C
C#######################################################################
C
C     PURPOSE -
C
C     THIS ROUTINE TRANSLATES POINTS FROM THE CURRENT LOCAL COORDINATE
C     SYSTEM TO THE NORMAL COORDINATE SYSTEM.  THE CURRENT ORIGIN AND
C     ROTATION MATRIX ARE USED FOR THE TRANSALTION.
C
C
C     INPUT ARGUMENTS -
C
C        x1 - CURRENT X COORDINATE
C        y1 - CURRENT Y COORDINATE
C        z1 - CURRENT Z COORDINATE
C
C
C     OUTPUT ARGUMENTS -
C
C        x2 - NORMAL X COORDINATE
C        y2 - NORMAL Y COORDINATE
C        z2 - NORMAL Z COORDINATE
C
C
C     CHANGE HISTORY -
C
C        $Log: xyznorm.f,v $
C        Revision 2.00  2007/11/09 20:04:06  spchu
C        Import to CVS
C
CPVCS    
CPVCS       Rev 1.2   Mon Apr 14 17:06:16 1997   pvcs
CPVCS    No change.
CPVCS    
CPVCS       Rev 1.1   05/11/95 13:14:44   het
CPVCS    Add diffusion solver
CPVCS    
CPVCS       Rev 1.0   11/10/94 12:20:42   pvcs
CPVCS    Original version.
C
C
C
C
C#######################################################################
C
      implicit real*8 (a-h,o-z)
C
      include "chydro.h"
C
C#######################################################################
C
C
C     ******************************************************************
C
C     GET THE CURRENT ORIGIN
C
      x0=origc(1)
      y0=origc(2)
      z0=origc(3)
C
C     ******************************************************************
C
C     GET THE CURRENT ROTATION MATRIX
C
      ax=rotatc(1,1)
      bx=rotatc(1,2)
      cx=rotatc(1,3)
      ay=rotatc(2,1)
      by=rotatc(2,2)
      cy=rotatc(2,3)
      az=rotatc(3,1)
      bz=rotatc(3,2)
      cz=rotatc(3,3)
C
C     ******************************************************************
C     TRANSPOSE AND ROTATE THE POINT TO THE REAL ORIGIN AND AXIS
C
      x2=ax*x1 + ay*y1 + az*z1 + x0
      y2=bx*x1 + by*y1 + bz*z1 + y0
      z2=cx*x1 + cy*y1 + cz*z1 + z0
C
C
C     ******************************************************************
C     SET UP THE CFT IMMUNE STATEMENT FOR DDT
C
      goto 9999
 9999 continue
C
      return
      end
