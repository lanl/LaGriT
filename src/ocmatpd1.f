*DK ocmatpd1
      subroutine ocmatpd1(a,b,c,n,m)
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
C        $Log:   /pvcs.config/t3d/src/ocmatpd1.f_a  $
CPVCS    
CPVCS       Rev 1.1   Mon Apr 14 16:56:08 1997   pvcs
CPVCS    No change.
CPVCS    
CPVCS       Rev 1.0   01/17/95 16:39:02   pvcs
CPVCS     Original version
C
C#######################################################################
C
      implicit real*8 (a-h, o-z)
      character*132 logmess
C
C#######################################################################
C
      real a(n,1), b(n,1), c(n,1)
c
      do 100 i=1,m
      do 100 j=1,m
      a(i,j) = 0.0
      do 100 k=1,m
 100  a(i,j) = a(i,j) + b(i,k)*c(k,j)
c
      return
      end
