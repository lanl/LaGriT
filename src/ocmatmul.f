*DK ocmatmul
      subroutine ocmatmul(z,a,r,n)
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
C        $Log:   /pvcs.config/t3d/src/ocmatmul.f_a  $
CPVCS    
CPVCS       Rev 1.1   Mon Apr 14 16:56:06 1997   pvcs
CPVCS    No change.
CPVCS    
CPVCS       Rev 1.0   01/17/95 16:38:58   pvcs
CPVCS     Original version
C
C#######################################################################
C
      implicit real*8 (a-h, o-z)
      character*132 logmess
C
C#######################################################################
C
      real z(n), a(n,n), r(n)
      do 100 i=1,n
      z(i) = 0.0
      do 100 j=1,n
 100  z(i) = z(i) + a(i,j)*r(j)
      return
      end
