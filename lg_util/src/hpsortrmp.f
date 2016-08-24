*dk,hpsortrmp
      SUBROUTINE hpsortrmp(n,m,md,a,ascend,iprm)
C
C
C#######################################################################
C
C     PURPOSE -
C
C     HPSORTRMP ("HeaP SORT using a Real*8 M-fold key, generating
C     a Permution array") takes an integer N, a real*8 array A, with
C     first dimension length MD and second dimension length N, an integer
C     permutation array IPRM of length N, and a real*8 number ASCEND, and 
C     reorders IPRM so that A(1,IPRM(1)),...,A(1,IPRM(N))
C     is in ascending order if ASCEND is positive and
C     is in decreasing order if ASCEND is negative.  To 
C     break ties, we require also that A(2,IPRM(*)) is
C     in ascending (descending) order, and so on, until
C     possibly the M'th key A(M,*) is consulted
C     (i.e. ascending lexicographic order of M-tuples).
C     Of course this requires that M<=MD.
C
C     INPUT ARGUMENTS -
C
C        N - number of elements to be sorted.
C        M - we interpret array A as M-tuples
C        MD- actual first dimension of array A (M<=MD)
C        A - real*8 array of values which determine
C             how IPRM will be reordered, it has 'depth' M for 
C             purposes of tie-breaking
C        IPRM - integer array to be reordered.  Strictly
c             speaking, IPRM need not be a permutation of
c             the integers {1,..,N}, but may be simply
c             a mapping from {1,..,N} onto a set of N
c             distinct positive integers.
C        ASCEND - real*8 which controls whether we sort in ascending
C                 or descending order.
C
C     OUTPUT ARGUMENTS -
C
C        IPRM - reordered integer array.
C
C     CHANGE HISTORY -
C
C        $Log: hpsortrmp.f,v $
C        Revision 2.00  2007/11/03 00:49:11  spchu
C        Import to CVS
C
CPVCS    
CPVCS       Rev 1.0   22 Aug 2000 10:21:54   dcg
CPVCS    Initial revision.
CPVCS    
CPVCS       Rev 1.0   Mon Nov 15 13:22:40 1999   kuprat
CPVCS    Initial revision.
C
C#######################################################################
C
 
      implicit none
 
      INTEGER n,m,md
      REAL*8 ascend,a(md,*)
      INTEGER i,ir,j,l,iprm(n),irra,k
      if (n.lt.2) return
      l=n/2+1
      ir=n
10    continue
        if(l.gt.1)then
          l=l-1
          irra=iprm(l)
        else
          irra=iprm(ir)
          iprm(ir)=iprm(1)
          ir=ir-1
          if(ir.eq.1)then
            iprm(1)=irra
            return
          endif
        endif
        i=l
        j=l+l
20      if(j.le.ir)then
          if(j.lt.ir)then
            do k=1,m-1
              if(ascend*a(k,iprm(j)).lt.ascend*a(k,iprm(j+1))) then
                j=j+1
                goto 30
              elseif(ascend*a(k,iprm(j)).gt.ascend*a(k,iprm(j+1)))then
                goto 30
              endif
            enddo
            if(ascend*a(m,iprm(j)).lt.ascend*a(m,iprm(j+1)))j=j+1
          endif
 30       continue
          do k=1,m-1
            if(ascend*a(k,irra).lt.ascend*a(k,iprm(j))) then
              iprm(i)=iprm(j)
              i=j
              j=j+j
              goto 20
            elseif(ascend*a(k,irra).gt.ascend*a(k,iprm(j))) then
              j=ir+1
              goto 20
            endif
          enddo
          if(ascend*a(m,irra).lt.ascend*a(m,iprm(j))) then
            iprm(i)=iprm(j)
            i=j
            j=j+j
          else
            j=ir+1
          endif
          goto 20
        endif
        iprm(i)=irra
      goto 10
      END
