      subroutine erradpt(ieltary,ieltno,range,errvec)
C #####################################################################
C
C     PURPOSE -
C
C        Subroutine that provides edge errors for purposes of
C        MEGA adaption.  (Replace this default subroutine  with
C        user-supplied subroutine with real edge errors for nontrivial smoothing.)
C        This default subroutine should create a uniform grid when used
C        with MEGA type r-adaption.
C
C     INPUT ARGUMENTS -
C
C        IELTARY - List of tetrahedra that need edge errors specified.
C        IELTNO - Length of IELTARY.
C
C     OUTPUT ARGUMENTS -
C
C        ERRVEC - Array of dimension (6,IELTNO) that gives six edge
C           errors for each element in IELTARY.
C        RANGE - The difference between the maximum and the minimum
C           values of the function being represented.  This number
C           should be nonnegative, and need only be a rough estimate.
C
C     CHANGE HISTORY -
C
C     $Log:   /pvcs.config/t3d/src/erradpt.f_a  $
CPVCS    
CPVCS       Rev 1.1   08 Jan 2001 16:33:02   dcg
CPVCS    change for alpha compilers 
CPVCS    
CPVCS       Rev 1.0   23 Dec 2000 17:28:00   kuprat
CPVCS    Initial revision.
C
C ######################################################################
 
      implicit none
 
      integer ieltno
      real*8 range,errvec(6,ieltno)
      integer ieltary(ieltno),i,j
 
      do i=1,ieltno
         do j=1,6
            errvec(j,i)=0.
         enddo
      enddo
      range=1.
 
      return
      end
 
