*dk,angle3v
      subroutine angle3v(xa,ya,za,x1,y1,z1,phi,thi)
      implicit none
      include 'consts.h'
      include 'chydro.h'
      real*8 x1,y1,z1,xa,ya,za,sinph,cosph,sinth,costh,phi,thi
C
C
C#######################################################################
C
C      PURPOSE -
C
C        THIS ROUTINE CALCULATES REFERENCE ANGLES RELATIVE TO A
C           LINE SIGMENT.  FROM THE TWO PAIRS OF (X,Y,Z) COORDINATES
C           WE CAN DERIVE EITHER (radius,THETA,z) OR (radius,THETA,PHI)
C           TRIPLETS.
C        NOTE:  THE ANGLES ARE RETURN IN RADIANS (NOT DEGREES).
C
C
C      INPUT ARGUMENTS -
C
C         xa       - X-COORDINATE CENTER.
C
C         ya       - Y-COORDINATE CENTER.
C
C         za       - Z-COORDINATE CENTER.
C
C         x1       - X-COORDINATE OF THE TERMINATION POINT.
C
C         y1       - Y-COORDINATE OF THE TERMINATION POINT.
C
C         z1       - Z-COORDINATE OF THE TERMINATION POINT.
C
C
C
C      OUTPUT ARGUMENTS -
C
C         phi      - ANGLE IN RADIANS.  FOR CYLINDRICAL COORDINATES
C                       THIS IS THE ANGLE (CALLED THETA) IN THE XY-PLANE
C                       RELATIVE TO THE X-AXIS. IN SPHERICAL COORDINATES
C                       THIS ANGLE (CALLED PHI) IS IN THE XY-PLANE
C                       RELATIVE TO THE X-AXIS. THE RANGE OF THIS
C                       ANGLE IS FROM [0,2PIE].
C
C         theta    - ANGLE IN RADIANS.  FOR SPHERICAL COORDINATES
C                       THIS IS THE ANGLE (CALLED THETA) RELATIVE TO THE
C                       Z-AXIS. THE RANGE OF THIS ANGLE IS FROM [0,180].
C
C
C      CHANGE HISTORY -
C
C        $Log: angle3v.f,v $
C        Revision 2.00  2007/11/05 19:45:46  spchu
C        Import to CVS
C
CPVCS    
CPVCS       Rev 1.3   29 Sep 2004 15:54:52   dcg
CPVCS    use pie from common - make routine implicit none
CPVCS
CPVCS       Rev 1.2   Wed Apr 05 13:34:02 2000   nnc
CPVCS    Minor source modifications required by the Absoft compiler.
CPVCS
CPVCS       Rev 1.1   Mon Apr 14 16:39:00 1997   pvcs
CPVCS    No change.
CPVCS
CPVCS       Rev 1.0   11/10/94 12:10:54   pvcs
CPVCS    Original version.
C
C#######################################################################
C
C
C
C     ..................................................................
C
C     CHECK TO SEE IF BOTH ANGLES ARE ZERO ( BOTH POINTS ARE AT THE
C        SAME LOCATION).
C
      if(abs(x1-xa).lt.1.0d-10.and.abs(y1-ya).lt.1.0d-10.
     *   and.abs(z1-za).lt.1.0d-10) then
         phi=zero
         thi=zero
         goto 9999
      endif
C
C
      call angle3(xa,ya,za,x1,y1,z1,sinph,cosph,sinth,costh)
C
C
C
C
C     ..................................................................
C     CALCULATE PHI.
C
      cosph=min( one,cosph)
      cosph=max(-one,cosph)
      phi=acos(cosph)
      if(cosph.ge.zero.and.sinph.ge.zero) phi=phi+3.d0*pie/two
      if(cosph.le.zero.and.sinph.ge.zero) phi=phi-pie/two
      if(cosph.le.zero.and.sinph.le.zero) phi=pie-phi+pie/two
      if(cosph.ge.zero.and.sinph.le.zero) phi=pie/two-phi+pie
      if(abs(cosph-one).le.1.0d-10) phi=3.d0*pie/two
      if(abs(sinph-one).le.1.0d-10) phi=zero
      if(abs(cosph+one).le.1.0d-10) phi=pie/two
      if(abs(sinph+one).le.1.0d-10) phi=pie
C
C
C     ,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,
C     CHECK TO SEE IF THERE IS NO PHI ANGLE.
C
      if(abs(x1-xa).lt.1.0d-10.and.abs(y1-ya).lt.1.0d-10) then
         phi=zero
      endif
C
C     ..................................................................
C     CALCULATE THETA.
C
      costh=min( one,costh)
      costh=max(-one,costh)
      thi=acos(costh)
      if(sinth.lt.zero) then
         if(costh.lt.zero) then
            thi=thi+pie/two
         else
            thi=two*pie-thi
         endif
      endif
C
C     ..................................................................
C
      goto 9999
 9999 continue
      return
      end
