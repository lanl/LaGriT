*DK ocmidpt
      subroutine ocmidpt(x,x1,x2,nn)
C
C
C#######################################################################
C
C      PURPOSE -
C
C         THIS ROUTINE
C
C      INPUT ARGUMENTS -
C
C         input_message - CHARACTER STRING CONTAINING THE INPUT
C                            MESSAGE.
C
C      OUTPUT ARGUMENTS -
C
C         ierror_return - ERROR RETURN CODE (==0 ==> OK, <>0 ==> ERROR)
C
C      CHANGE HISTORY -
C
C        $Log:   /pvcs.config/t3d/src/ocmidpt.f_a  $
CPVCS    
CPVCS       Rev 1.1   Mon Apr 14 16:56:12 1997   pvcs
CPVCS    No change.
CPVCS    
CPVCS       Rev 1.0   01/17/95 16:39:08   pvcs
CPVCS     Original version
C
C#######################################################################
C
      implicit real*8 (a-h, o-z)
      character*132 logmess
C
C#######################################################################
C
c
c        This routine finds the midpoint x along the shorter great
c        circle arc between points x1 and x2 on the unit sphere.
c
      real x(nn,3), x1(nn,3), x2(nn,3)
c
      do 20 j=1,3
 20   x(1,j) = x1(1,j) + x2(1,j)
      xnorm = 1./sqrt(x(1,1)**2 + x(1,2)**2 + x(1,3)**2)
      do 40 j=1,3
 40   x(1,j) = xnorm*x(1,j)
c
      goto 9999
 9999 continue
      return
      end
