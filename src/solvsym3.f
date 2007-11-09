*dk solvsym3
      subroutine solvsym3(a11,a12,a13,a22,a23,a33,
     &   b1,b2,b3,x1,x2,x3)
C #####################################################################
C
C     PURPOSE -
C
C        Solve 3x3 linear system "AX=B" for symmetric A using
C        Cramer's Rule.
C
C     INPUT ARGUMENTS -
C
C        A11,A12,A13,A22,A23,A33 - Coefficients of a symmetric 3x3
C                                  matrix.
C        B1,B2,B3                - Righthand side vector.
C
C
C     OUTPUT ARGUMENTS -
C
C        X1,X2,X3                - Solution vector.
C
C     CHANGE HISTORY -
C$Log:   /pvcs.config/t3d/src/solvsym3.f_a  $
CPVCS    
CPVCS       Rev 1.2   Mon Apr 14 17:02:00 1997   pvcs
CPVCS    No change.
CPVCS    
CPVCS       Rev 1.1   06/02/95 23:45:38   kuprat
CPVCS    Commented out print statement
CPVCS    
CPVCS       Rev 1.0   02/15/95 13:37:56   dcg
CPVCS    Original version
C
C ######################################################################
      implicit none
      real*8 a11,a12,a13,a22,a23,a33,b1,b2,b3,x1,x2,x3
      real*8 c11,c12,c13,c22,c23,c33,det
 
      c11=a22*a33-a23**2
      c12=a13*a23-a12*a33
      c13=a12*a23-a22*a13
      c22=a11*a33-a13**2
      c23=a13*a12-a11*a23
      c33=a11*a22-a12**2
 
      det=a11*c11+a12*c12+a13*c13
 
      if (det.eq.0.) then
ccccc         print*,'Solvsym3: Zero determinant!!'
         x1=0.
         x2=0.
         x3=0.
      else
         x1=(b1*c11+b2*c12+b3*c13)/det
         x2=(b1*c12+b2*c22+b3*c23)/det
         x3=(b1*c13+b2*c23+b3*c33)/det
      endif
      return
      end
