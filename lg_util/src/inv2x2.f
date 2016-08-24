*dk,inv2x2
      subroutine inv2x2(a11,a21,a12,a22,x1,x2,b1,b2,epsilon)
C                                                                       
C ######################################################################
C                      
C     PURPOSE -
C
C        Solv the 2x2 problem a x = b for x
C
C     INPUT ARGUMENTS -
C
C        a11,a21,a12,a22 - the a matrix
C        b1,b2           - the b vector
C        epsilon         - relative epsilon for small tests
C
C     OUTPUT ARGUMENTS -
C
C        x1,x2 - the solution vector
C
C     PROGRAMMING NOTES -
C
C        to handle "nearly singular" matrix, the
C        Cramers Rule soln is compared to
C        the Schmidt-Hilbert soln.
C        If the magnitudes differ widly
C        the Schmidt-Hilbert soln is used.
C
C     CHANGE HISTORY -
C
C        $Log: inv2x2.f,v $
C        Revision 2.00  2007/11/03 00:49:11  spchu
C        Import to CVS
C
CPVCS    
CPVCS       Rev 1.3   Tue Oct 19 14:58:56 1999   jtg
CPVCS    fixed local_debug flag (was non-zero)
CPVCS    
CPVCS       Rev 1.2   Tue Oct 19 12:59:42 1999   jtg
CPVCS    If singular, it now finds the best solution instead of
CPVCS    returning "1/epsilon", using Schmidt-Hilbert to
CPVCS    solve the singular problem.
CPVCS
CPVCS       Rev 1.1   01/04/95 21:55:30   llt
CPVCS    unicos changes (made by het)
CPVCS
CPVCS       Rev 1.0   11/15/94 16:50:24   llt
CPVCS    Original version
C
C ######################################################################
C
      implicit none
      real*8 a11,a12,a21,a22,b1,b2,
     &                  x1,x2,epsilon

      integer iflag,i1,i,local_debug

      real*8 mat(2,2),matinv(2,2),s1,s2,xxx,sss
     &       ,iarb(2),arb(2,2),work(2,2),scale,det

C-----------------------------------------------------------------------

      xxx=abs(b1)
      if (xxx.lt.abs(b2)) xxx=abs(b2)

      ! do nothing if vec==0
      if (xxx.eq.0.d0) then
         x1=0.d0
         x2=0.d0
         goto 100
      endif

      mat(1,1)=a11
      mat(2,1)=a21
      mat(1,2)=a12
      mat(2,2)=a22

      ! calculate determinant and adjoint
      scale=0.d0
      det=0.d0
      i=1
      i1=2
      xxx=mat(1,i)*mat(2,i1)
      if (abs(xxx).gt.scale) scale=abs(xxx)
      det=det+xxx
      xxx=mat(1,i1)*mat(2,i)
      if (abs(xxx).gt.scale) scale=abs(xxx)
      det=det-xxx
      matinv(1,1)=mat(2,2)
      matinv(2,2)=mat(1,1)
      matinv(1,2)=-mat(1,2)
      matinv(2,1)=-mat(2,1)

      ! calculate soln using inverse=adjoint/determinant (Cramer's rule)

      if (det.ne.0.d0) then
         x1=matinv(1,1)*b1+matinv(1,2)*b2
         x1=x1/det
         x2=matinv(2,1)*b1+matinv(2,2)*b2
         x2=x2/det
      endif

      if (abs(det).le.epsilon*scale.or.det.eq.0.d0) then

         ! if (nearly) singular, compare to Schmidt-Hilbert soln and
         ! use that instead if magnitudes vastly different

         call inv_schmidt_hilbert_lg
     &          (epsilon,2,2,2,2
     &           ,mat,matinv,iarb,arb,work)

         if (det.ne.0.d0) then
            s1=matinv(1,1)*b1+matinv(1,2)*b2
            s2=matinv(2,1)*b1+matinv(2,2)*b2
            xxx=abs(x1)+abs(x2)
            sss=abs(s1)+abs(s2)
            if (xxx*epsilon.gt.sss) then
               ! presume that what the user really wanted was
               ! the schmidt_hilbert soln of the (nearly) singular matrix
               x1=s1
               x2=s2
               iflag=1
            endif
         else
            x1=matinv(1,1)*b1+matinv(1,2)*b2
            x2=matinv(2,1)*b1+matinv(2,2)*b2
         endif

      endif

      !local_debug=0
      !if (local_debug.gt.0) then
      !   write(*,*) x1,x2,a11*x1+a12*x2-b1,a21*x1+a22*x2-b2
      !endif

100   return
      end
