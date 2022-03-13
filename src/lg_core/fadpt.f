      subroutine fadpt(x,y,z,mat,nvec,time,f)
C #####################################################################
C
C     PURPOSE -
C
C        Adaption function for smoothing algorithms.  (Replace this
C        code with a user-supplied function for nontrivial smoothing.)
C        This default function should create a uniform grid when used
C        with MEGA or ESUG type smoothing.
C
C     INPUT ARGUMENTS -
C
C        X,Y,Z - Input spatial coordinate arrays.
C        MAT  - Input array of material types.  (This is only
C               relevant in the case of child points where a 
C               function value might depend on position, time, 
C               AND material type.)
C        NVEC - Length of spatial arrays.  (Evaluate function at each
C             spatial coordinate.)
C        TIME  - Current time (for time dependent adaption).
C
C     OUTPUT ARGUMENTS -
C
C        F - Array of adaption function values.
C
C     CHANGE HISTORY -
C
C     $Log: fadpt.f,v $
C     Revision 2.00  2007/11/05 19:45:54  spchu
C     Import to CVS
C
CPVCS    
CPVCS       Rev 1.8   Mon Apr 14 16:45:12 1997   pvcs
CPVCS    No change.
CPVCS    
CPVCS       Rev 1.7   Thu Oct 31 11:52:40 1996   kuprat
CPVCS    Added MAT argument which gives the user access to the 
CPVCS    material type of a point for those cases where function 
CPVCS    value depends on material type.
C
C ######################################################################

      implicit none
 
      integer lenptr
      parameter (lenptr=1000000)

      real*8 x(lenptr),y(lenptr),z(lenptr),time
      integer mat(lenptr),nvec,i
      real*8 f(lenptr)

      do i=1,nvec
         f(i)=0.
      enddo

      return
      end

