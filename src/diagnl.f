*dk diagnl
      subroutine diagnl( nrows, ncols, ncoefs, AFP,
     *                   INDEX_ROW, INDEX_COL,
     *                   IROWOFF, IROWCNT, IROWDAG,
     *                   dmat, bmat, xmat, umat )
      implicit double precision (a-h, o-z)
      dimension AFP(ncoefs)
      dimension INDEX_ROW(ncoefs)
      dimension INDEX_COL(ncoefs)
      integer IROWOFF(nrows), IROWCNT(nrows), IROWDAG(nrows)
C
C#######################################################################
C
C     PURPOSE -
C
c         this subroutine performs the diagonal scaling preconditioning
c         for the conjugate gradient equation solver.
C
C
C     INPUT ARGUMENTS -
C
c         nnrad   -->  column pointer array for the a matrix.
c         nrad    -->  contains number of active columns for each row
c                      of the a matrix.
c         dmat    -->  square root of the diagonalized a matrix.
c         bmat    -->  forcing vector or right hand side of the equation
c                      system.
c         xmat    -->  product of d x u.
c         umat    -->  u (unknown) vector.
c         nequat  -->  number of equations in the system.
C
C
C     OUTPUT ARGUMENTS -
C
C        NONE
C
C     CHANGE HISTORY -
C
C        HT06XXAA-89
C
C
C#######################################################################
C
c
c     declare the arrays.
c
      dimension dmat(nrows)
      dimension bmat(nrows)
      dimension xmat(nrows)
      dimension umat(nrows)
C
C#######################################################################
C
c
c.......................................................................
c
c
c.......................................................................
c
c     compute the d matrix values.
c
c
c.......................................................................
c
      do 1 irow=1,nrows
c
            sum = 0.
c
            ncol=IROWCNT(irow)
            mpnt=irow
c
          do 1 j=1,ncol
c
              jcol = INDEX_COL(IROWOFF(irow)+j)
c
              if(jcol.eq.irow) then
                  dmat(irow) = sqrt( AFP(IROWOFF(irow)+j) )
              endif
c
    1 continue
c
c.......................................................................
c
c     compute the a matrix values.
c
c
c,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,
c
      do 2 irow=1,nrows
c
            ncol=IROWCNT(irow)
            mpnt=irow
c
          do 3 j=1,ncol
c
              jcol = INDEX_COL(IROWOFF(irow)+j)
c
              AFP(IROWOFF(irow)+j) = AFP(IROWOFF(irow)+j) /
     *                               ( dmat(irow) * dmat(jcol) )
c
    3    continue
c
c
    2 continue
c
c
c.......................................................................
c
c     compute the x vector initial guess values.
c
      do 4 i=1,nrows
c
          xmat(i) = dmat(i) * umat(i)
c
    4 continue
c
c.......................................................................
c
c     compute the b vector initial guess values.
c
      do 5 i=1,nrows
c
          bmat(i) = bmat(i) / dmat(i)
c
    5 continue
c
c.......................................................................
c
      goto 9999
 9999 continue
      return
      end
