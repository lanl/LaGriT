      subroutine fadpt(x,y,z,mat,nvec,time,f)
C #####################################################################
C
C     PURPOSE -
C
C        Adaption function for smoothing algorithms.  This is the
C        'gyroscope' function wherein the function has large
C        second derivatives near each of three rings in the
C        three coordinate planes.
C
C     INPUT ARGUMENTS -
C
C        X,Y,Z - Input spatial coordinate arrays.
C        MAT - Material type arrays.  (This is for cases where the
C              function value depends BOTH on position and material
C              type.)
C        NV - Length of spatial arrays.  (Evaluate function at each
C             spatial coordinate.)
C        TIME  - Current time (for time dependent adaption).
C
C     OUTPUT ARGUMENTS -
C
C        F - Array of adaption function values.
C
C     CHANGE HISTORY -
C
C ######################################################################
      implicit none
 
      integer lenptr
      parameter (lenptr=1000000)

      real*8 x(lenptr),y(lenptr),z(lenptr),f(lenptr)
      integer nvec, i, mat(lenptr)
      real*8 r0,z0,epssq,r,dsq,x0,y0,time

c.... Radius of rings

      r0=0.5

c.... Center of rings

      x0=0.
      y0=0.
      z0=0.
   
c.... Square of epsilon.  The function does not go to infinity
c.... on the ring because epsilon is nonzero.  More precisely, the
c.... function is 
c....                      f(x,y,z)=1/( d(x,y,z)**2 +epsilon**2 ).
c....
c.... That is, the function is 1 divided by the smallest distance
c.... to any of the three rings (squared) plus epsilon squared.  
c.... This implies that the 'characteristic length' of the function
c.... AT each of the rings is epsilon.

      epssq=.1**2

c.... Loop over vector of input values and compute function values.

      do i=1,nvec

         r=sqrt(x(i)**2+y(i)**2)
         dsq=(r-r0)**2+(z(i)-z0)**2

         r=sqrt(y(i)**2+z(i)**2)
         dsq=min(dsq,(r-r0)**2+(x(i)-x0)**2)

         r=sqrt(z(i)**2+x(i)**2)
         dsq=min(dsq,(r-r0)**2+(y(i)-y0)**2)

         f(i)=1./(dsq+epssq)

      enddo

      return
      end

