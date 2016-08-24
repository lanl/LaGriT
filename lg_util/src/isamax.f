*dk,isamax
      integer function isamax(N,SX,INCX)
C
C ######################################################################
C
C        $Log: isamax.f,v $
C        Revision 2.00  2007/11/03 00:49:11  spchu
C        Import to CVS
C
CPVCS
CPVCS       Rev 1.21   02 Oct 2007 12:40:28   spchu
CPVCS    original version
C
C ######################################################################
C
      implicit real*8 (a-h,o-z)
c  this function operates as double precision on short word machines
C***BEGIN PROLOGUE  isamax
C***DATE WRITTEN   791001   (YYMMDD)
C***REVISION DATE  861211   (YYMMDD)
C***CATEGORY NO.  D1A2
C***KEYWORDS  LIBRARY=SLATEC(BLAS),
C             TYPE=SINGLE PRECISION(isamax-S IDAMAX-D ICAMAX-C),
C             LINEAR ALGEBRA,MAXIMUM COMPONENT,VECTOR
C***AUTHOR  LAWSON, C. L., (JPL)
C           HANSON, R. J., (SNLA)
C           KINCAID, D. R., (U. OF TEXAS)
C           KROGH, F. T., (JPL)
C***PURPOSE  Find the smallest index of that component af a vector
C            having the maximum magnitude.
C***DESCRIPTION
C
C                B L A S  Subprogram
C    Description of Parameters
C
C     --Input--
C        N  number of elements in input vector(s)
C       SX  single precision vector with N elements
C     INCX  storage spacing between elements of SX
C
C     --Output--
C   isamax  smallest index (zero if N .LE. 0)
C
C     Find smallest index of maximum magnitude of single precision SX.
C     isamax =  first I, I = 1 to N, to minimize  ABS(SX(1-INCX+I*INCX)
C***REFERENCES  LAWSON C.L., HANSON R.J., KINCAID D.R., KROGH F.T.,
C                 *BASIC LINEAR ALGEBRA SUBPROGRAMS FOR FORTRAN USAGE*,
C                 ALGORITHM NO. 539, TRANSACTIONS ON MATHEMATICAL
C                 SOFTWARE, VOLUME 5, NUMBER 3, SEPTEMBER 1979, 308-323
C***ROUTINES CALLED  (NONE)
C***END PROLOGUE  isamax
C
      real*8 SX(N),SMAX,XMAG
C***FIRST EXECUTABLE STATEMENT  isamax
      isamax = 0
      IF(N.LE.0) RETURN
      isamax = 1
      IF(N.LE.1)RETURN
      IF(INCX.EQ.1)GOTO 20
C
C        CODE FOR INCREMENTS NOT EQUAL TO 1.
C
      SMAX = ABS(SX(1))
      NS = N*INCX
      II = 1
          DO 10 I=1,NS,INCX
          XMAG = ABS(SX(I))
          IF(XMAG.LE.SMAX) GO TO 5
          isamax = II
          SMAX = XMAG
    5     II = II + 1
   10     CONTINUE
      RETURN
C
C        CODE FOR INCREMENTS EQUAL TO 1.
C
   20 SMAX = ABS(SX(1))
      DO 30 I = 2,N
         XMAG = ABS(SX(I))
         IF(XMAG.LE.SMAX) GO TO 30
         isamax = I
         SMAX = XMAG
   30 CONTINUE
      RETURN
      END
