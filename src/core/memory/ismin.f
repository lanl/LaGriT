*dk,ismin
      integer function ismin(N,SX,INCX)
C
C ######################################################################
C
C        $Log: ismin.f,v $
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
C***BEGIN PROLOGUE  ismin
C***DATE WRITTEN   790614   (YYMMDD)
C***REVISION DATE  860401   (YYMMDD)
C***CATEGORY NO.  D1A2
C***KEYWORDS  VECTOR,MINIMUM,INDEX
C***AUTHOR  KAHANER, D. K., LOS ALAMOS NATIONAL LABORATORY
C***PURPOSE  Find the smallest index of a minimum element of a vector.
C***DESCRIPTION
C
C   This function finds the smallest index of a minimum element of a
C   real array SX whose N elements are stored sequentially with
C   spacing INCX >= 1.  If N <= 0, the value zero is returned.
C   Thus, if I = ismin(N,SX,1), then SX(I) is an element of array SX
C   of minimum value.
C
C   Description of Parameters
C
C    --Input--
C        N  number of elements in input vector
C       SX  single precision vector with N elements
C     INCX  storage spacing between elements of SX
C
C    --Output--
C    ismin  smallest index (zero if N .LE. 0)
C
C***REFERENCES  (NONE)
C***ROUTINES CALLED  (NONE)
C***END PROLOGUE  ismin
      real*8 SX(N),SMIN
      INTEGER I,INCX,IX,N
C***FIRST EXECUTABLE STATEMENT  ismin
      ismin = 0
      IF( N .LT. 1 ) RETURN
      ismin = 1
      IF(N.EQ.1)RETURN
      IF(INCX.EQ.1)GO TO 20
C
C        CODE FOR INCREMENT NOT EQUAL TO 1
C
      IX = 1
      SMIN = (SX(1))
      IX = IX + INCX
      DO 10 I = 2,N
         IF((SX(IX)).GE.SMIN) GO TO 5
         ismin = I
         SMIN = (SX(IX))
    5    IX = IX + INCX
   10 CONTINUE
      RETURN
C
C        CODE FOR INCREMENT EQUAL TO 1
C
   20 SMIN = (SX(1))
      DO 30 I = 2,N
         IF((SX(I)).GE.SMIN) GO TO 30
         ismin = I
         SMIN = (SX(I))
   30 CONTINUE
      RETURN
      END
