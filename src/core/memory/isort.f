      SUBROUTINE isort(X,Y,N,KFLAG)
C
C #####################################################################
C
C     PURPOSE -
C
C        None
C
C     INPUT ARGUMENTS -
C
C        None
C
C     OUTPUT ARGUMENTS -
C
C        None
C
C     CHANGE HISTORY -
C
C        $Log: isort.f,v $
C        Revision 2.00  2007/11/03 00:49:11  spchu
C        Import to CVS
C
CPVCS    
CPVCS       Rev 1.4   01 Jun 2001 16:29:42   dcg
CPVCS    make name lower case
CPVCS
CPVCS       Rev 1.3   08/03/95 13:53:22   dcg
CPVCS    replace print * with writloga calls
CPVCS
CPVCS       Rev 1.2   01/20/95 12:21:52   dcg
CPVCS     declare r to be real*4 for type compatibility
CPVCS
CPVCS       Rev 1.1   01/04/95 21:55:42   llt
CPVCS    unicos changes (made by het)
CPVCS
CPVCS       Rev 1.0   11/10/94 12:42:36   pvcs
CPVCS    Original version.
C
C ######################################################################
C
      implicit real*8 (a-h,o-z)
C***BEGIN PROLOGUE  ISORT
C***DATE WRITTEN   761118   (YYMMDD)
C***REVISION DATE  861211   (YYMMDD)
C***CATEGORY NO.  N6A2A
C***KEYWORDS  LIBRARY=SLATEC,TYPE=INTEGER(SSORT-S DSORT-D ISORT-I),
C             QUICKSORT,SINGLETON QUICKSORT,SORT,SORTING
C***AUTHOR  JONES, R. E., (SNLA)
C           KAHANER, D. K., (NBS)
C           WISNIEWSKI, J. A., (SNLA)
C***PURPOSE  ISORT sorts integer array X and optionally makes the same
C            interchanges in integer array Y.  The array X may be
C            sorted in increasing order or decreasing order.  A
C            slightly modified QUICKSORT algorithm is used.
C***DESCRIPTION
C
C     Written by Rondall E Jones
C     Modified by John A. Wisniewski to use the Singleton QUICKSORT
C     algorithm. Date 18 November 1976.
C
C     Further modified by David K. Kahaner
C     NATIONAL BUREAU OF STANDARDS
C     August, 1981
C
C     Abstract
C         ISORT sorts integer array X and optionally makes the same
C         interchanges in integer array Y.  The array X may be sorted in
C         INCREASING order or DECREASING order.  A slightly modified
C         QUICKSORT algorithm is used.
C
C     Reference
C         Singleton,R.C., Algorithm 347, An Efficient Algorithm For
C         Sorting With Minimal Storage, CACM,12(3),1969,185-7.
C
C     Description of Parameters
C         X - integer array of values to be sorted
C         Y - integer array to be (optionally) carried along
C         N - number of values in integer array X to be sorted
C     KFLAG - control parameter
C           = 2 means sort X in INCREASING order and carry Y along.
C           = 1 means sort X in INCREASING order (ignoring Y)
C           =-1 means sort X in DECREASING order (ignoring Y)
C           =-2 means sort X in DECREASING order and carry Y along.
C***REFERENCES  SINGLETON, R. C., ALGORITHM 347, AN EFFICIENT
C                 ALGORITHM FOR SORTING WITH MINIMAL STORAGE, CACM,
C                 VOL. 12, NO. 3, 1969, PP. 185-187.
C***ROUTINES CALLED  XERROR
C***END PROLOGUE  ISORT
      dimension IL(21),IU(21)
      real*4 r
      INTEGER X(N),Y(N),T,TT,TY,TTY
      character*80 logmess
C***FIRST EXECUTABLE STATEMENT  ISORT
      NN = N
      IF (NN.GE.1) GO TO 10
         write(logmess,'(a)')
     x      'SSORT- THE NUMBER OF VALUES TO BE SORTED IS NOT POSITIVE'
         call writloga('default',0,logmess,0,ierr)
      RETURN
   10 KK = IABS(KFLAG)
      IF ((KK.EQ.1).OR.(KK.EQ.2)) GO TO 15
      write(logmess,'(a)')
     x  'SSORT- THE SORT CONTROL PARAMETER, K, WAS NOT 2, 1, -1, OR -2.'
      call writloga('default',0,logmess,0,ierr)
      RETURN
C
C ALTER ARRAY X TO GET DECREASING ORDER IF NEEDED
C
   15 IF (KFLAG.GE.1) GO TO 30
      DO 20 I=1,NN
   20 X(I) = -X(I)
   30 GO TO (100,200),KK
C
C SORT X ONLY
C
  100 CONTINUE
      M=1
      I=1
      J=NN
      R=.375
  110 IF (I .EQ. J) GO TO 155
  115 IF (R .GT. .5898437) GO TO 120
      R=R+3.90625E-2
      GO TO 125
  120 R=R-.21875
  125 K=I
C                                  SELECT A CENTRAL ELEMENT OF THE
C                                  ARRAY AND SAVE IT IN LOCATION T
C*****IJ = I + IFIX (FLOAT (J-I) * sngl(R))
      IJ = I + IFIX (FLOAT (J-I) * R)
      T=X(IJ)
C                                  IF FIRST ELEMENT OF ARRAY IS GREATER
C                                  THAN T, INTERCHANGE WITH T
      IF (X(I) .LE. T) GO TO 130
      X(IJ)=X(I)
      X(I)=T
      T=X(IJ)
  130 L=J
C                                  IF LAST ELEMENT OF ARRAY IS LESS THAN
C                                  T, INTERCHANGE WITH T
      IF (X(J) .GE. T) GO TO 140
      X(IJ)=X(J)
      X(J)=T
      T=X(IJ)
C                                  IF FIRST ELEMENT OF ARRAY IS GREATER
C                                  THAN T, INTERCHANGE WITH T
      IF (X(I) .LE. T) GO TO 140
      X(IJ)=X(I)
      X(I)=T
      T=X(IJ)
      GO TO 140
  135 TT=X(L)
      X(L)=X(K)
      X(K)=TT
C                                  FIND AN ELEMENT IN THE SECOND HALF OF
C                                  THE ARRAY WHICH IS SMALLER THAN T
  140 L=L-1
      IF (X(L) .GT. T) GO TO 140
C                                  FIND AN ELEMENT IN THE FIRST HALF OF
C                                  THE ARRAY WHICH IS GREATER THAN T
  145 K=K+1
      IF (X(K) .LT. T) GO TO 145
C                                  INTERCHANGE THESE ELEMENTS
      IF (K .LE. L) GO TO 135
C                                  SAVE UPPER AND LOWER SUBSCRIPTS OF
C                                  THE ARRAY YET TO BE SORTED
      IF (L-I .LE. J-K) GO TO 150
      IL(M)=I
      IU(M)=L
      I=K
      M=M+1
      GO TO 160
  150 IL(M)=K
      IU(M)=J
      J=L
      M=M+1
      GO TO 160
C                                  BEGIN AGAIN ON ANOTHER PORTION OF
C                                  THE UNSORTED ARRAY
  155 M=M-1
      IF (M .EQ. 0) GO TO 300
      I=IL(M)
      J=IU(M)
  160 IF (J-I .GE. 1) GO TO 125
      IF (I .EQ. 1) GO TO 110
      I=I-1
  165 I=I+1
      IF (I .EQ. J) GO TO 155
      T=X(I+1)
      IF (X(I) .LE. T) GO TO 165
      K=I
  170 X(K+1)=X(K)
      K=K-1
      IF (T .LT. X(K)) GO TO 170
      X(K+1)=T
      GO TO 165
C
C SORT X AND CARRY Y ALONG
C
  200 CONTINUE
      M=1
      I=1
      J=NN
      R=.375
  210 IF (I .EQ. J) GO TO 255
  215 IF (R .GT. .5898437) GO TO 220
      R=R+3.90625E-2
      GO TO 225
  220 R=R-.21875
  225 K=I
C                                  SELECT A CENTRAL ELEMENT OF THE
C                                  ARRAY AND SAVE IT IN LOCATION T
C*****IJ = I + IFIX (FLOAT (J-I) *sngl(R))
      IJ = I + IFIX (FLOAT (J-I) * R)
      T=X(IJ)
      TY= Y(IJ)
C                                  IF FIRST ELEMENT OF ARRAY IS GREATER
C                                  THAN T, INTERCHANGE WITH T
      IF (X(I) .LE. T) GO TO 230
      X(IJ)=X(I)
      X(I)=T
      T=X(IJ)
       Y(IJ)= Y(I)
       Y(I)=TY
      TY= Y(IJ)
  230 L=J
C                                  IF LAST ELEMENT OF ARRAY IS LESS THAN
C                                  T, INTERCHANGE WITH T
      IF (X(J) .GE. T) GO TO 240
      X(IJ)=X(J)
      X(J)=T
      T=X(IJ)
       Y(IJ)= Y(J)
       Y(J)=TY
      TY= Y(IJ)
C                                  IF FIRST ELEMENT OF ARRAY IS GREATER
C                                  THAN T, INTERCHANGE WITH T
      IF (X(I) .LE. T) GO TO 240
      X(IJ)=X(I)
      X(I)=T
      T=X(IJ)
       Y(IJ)= Y(I)
       Y(I)=TY
      TY= Y(IJ)
      GO TO 240
  235 TT=X(L)
      X(L)=X(K)
      X(K)=TT
      TTY= Y(L)
       Y(L)= Y(K)
       Y(K)=TTY
C                                  FIND AN ELEMENT IN THE SECOND HALF OF
C                                  THE ARRAY WHICH IS SMALLER THAN T
  240 L=L-1
      IF (X(L) .GT. T) GO TO 240
C                                  FIND AN ELEMENT IN THE FIRST HALF OF
C                                  THE ARRAY WHICH IS GREATER THAN T
  245 K=K+1
      IF (X(K) .LT. T) GO TO 245
C                                  INTERCHANGE THESE ELEMENTS
      IF (K .LE. L) GO TO 235
C                                  SAVE UPPER AND LOWER SUBSCRIPTS OF
C                                  THE ARRAY YET TO BE SORTED
      IF (L-I .LE. J-K) GO TO 250
      IL(M)=I
      IU(M)=L
      I=K
      M=M+1
      GO TO 260
  250 IL(M)=K
      IU(M)=J
      J=L
      M=M+1
      GO TO 260
C                                  BEGIN AGAIN ON ANOTHER PORTION OF
C                                  THE UNSORTED ARRAY
  255 M=M-1
      IF (M .EQ. 0) GO TO 300
      I=IL(M)
      J=IU(M)
  260 IF (J-I .GE. 1) GO TO 225
      IF (I .EQ. 1) GO TO 210
      I=I-1
  265 I=I+1
      IF (I .EQ. J) GO TO 255
      T=X(I+1)
      TY= Y(I+1)
      IF (X(I) .LE. T) GO TO 265
      K=I
  270 X(K+1)=X(K)
       Y(K+1)= Y(K)
      K=K-1
      IF (T .LT. X(K)) GO TO 270
      X(K+1)=T
       Y(K+1)=TY
      GO TO 265
C
C CLEAN UP
C
  300 IF (KFLAG.GE.1) RETURN
      DO 310 I=1,NN
  310 X(I) = -X(I)
      RETURN
      END
