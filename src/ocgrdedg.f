*DK ocgrdedg
      subroutine ocgrdedg(xn,n)
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
C        $Log:   /pvcs.config/t3d/src/ocgrdedg.f_a  $
CPVCS    
CPVCS       Rev 1.1   Mon Apr 14 16:56:02 1997   pvcs
CPVCS    No change.
CPVCS    
CPVCS       Rev 1.0   01/17/95 16:38:50   pvcs
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
c        This routine fills the column zero and row n+1 of the grid
c        array xn for a single diamond.
c
      real xn(0:n+1,0:n+1,3), r(3,3), s(3,3), t(3,3), u(3), v(3)
c
      phi = -0.4*3.1415926535898
c
c        Fill column zero.
c
      call ocnulvec(s,9)
      s(1,1) =  cos(phi)
      s(2,2) =  s(1,1)
      s(2,1) =  sin(phi)
      s(1,2) = -s(2,1)
      s(3,3) =  1.0
      do 20 i=1,n
      do 10 j=1,3
 10   u(j) = xn(1,i,j)
      call ocmatmul(v,s,u,3)
      do 20 j=1,3
 20   xn(i,0,j) = v(j)
c
c        Fill row n+1.
c
      psi = atan2(xn(n,1,2),xn(n,1,1))
      cosa = cos(psi)
      sina = sin(psi)
      cosb = xn(n,1,3)
      sinb = sqrt(1.-cosb**2)
      t(1,1) =  cosb*cosa
      t(1,2) =  cosb*sina
      t(1,3) = -sinb
      t(2,1) = -sina
      t(2,2) =  cosa
      t(2,3) =  0.0
      t(3,1) =  sinb*cosa
      t(3,2) =  sinb*sina
      t(3,3) =  cosb
      call ocnulvec(s,9)
      chi = 2.*phi*xn(0,1,3)
      s(1,1) =  cos(chi)
      s(2,2) =  s(1,1)
      s(2,1) =  sin(chi)
      s(1,2) = -s(2,1)
      s(3,3) =  1.0
      call ocmatpd1(r,s,t,3,3)
      call ocmatpd2(s,t,r,3,3)
      do 40 i=0,n
      do 30 j=1,3
 30   u(j) = xn(i,2,j)
      call ocmatmul(v,s,u,3)
      do 40 j=1,3
 40   xn(n+1,n-i,j) = v(j)
c
      do 50 j=1,3
      xn(0,  0,  j) = 0.0
      xn(n+1,n+1,j) = 0.0
 50   xn(n+1,0,  j) = xn(n+1,1,j)
c
      return
      end
