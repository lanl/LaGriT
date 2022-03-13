*dk,hpsortimp
      SUBROUTINE hpsortimp(n,m,md,ia,ascend,iprm)
C
C
C#######################################################################
C
C     PURPOSE -
C
C     HPSORTIMP ("HeaP SORT using an Integer M-fold key, generating
C     a Permution array") takes an integer N, an integer array IA, with
C     first dimension length MD and second dimension length N, an integer
C     permutation array IPRM of length N, and a real number ASCEND, and 
C     reorders IPRM so that IA(1,IPRM(1)),...,IA(1,IPRM(N))
C     is in ascending order if ASCEND is positive and
C     is in decreasing order if ASCEND is negative.  To 
C     break ties, we require also that IA(2,IPRM(*)) is
C     in ascending (descending) order, and so on, until
C     possibly the M'th key IA(M,*) is consulted
C     (i.e. ascending lexicographic order of M-tuples).
C     Of course this requires that M<=MD.
C
C     INPUT ARGUMENTS -
C
C        N - number of elements to be sorted.
C        M - we interpret array IA as M-tuples
C        MD- actual first dimension of array IA (M<=MD)
C        IA - integer array of values which determine
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
C        $Log: hpsortimp.f,v $
C        Revision 2.00  2007/11/03 00:49:11  spchu
C        Import to CVS
C
CPVCS    
CPVCS       Rev 1.0   08 May 2001 10:29:56   dcg
CPVCS    Initial revision.
CPVCS    
CPVCS       Rev 1.0   Tue Aug 24 21:57:34 1999   kuprat
CPVCS    Initial revision.
C
C#######################################################################
C
 
      implicit none
 
      INTEGER n,m,md
      INTEGER ia(md,*)
      REAL*8 ascend
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
              if(ascend*ia(k,iprm(j)).lt.ascend*ia(k,iprm(j+1))) then
                j=j+1
                goto 30
              elseif(ascend*ia(k,iprm(j)).gt.ascend*ia(k,iprm(j+1)))then
                goto 30
              endif
            enddo
            if(ascend*ia(m,iprm(j)).lt.ascend*ia(m,iprm(j+1)))j=j+1
          endif
 30       continue
          do k=1,m-1
            if(ascend*ia(k,irra).lt.ascend*ia(k,iprm(j))) then
              iprm(i)=iprm(j)
              i=j
              j=j+j
              goto 20
            elseif(ascend*ia(k,irra).gt.ascend*ia(k,iprm(j))) then
              j=ir+1
              goto 20
            endif
          enddo
          if(ascend*ia(m,irra).lt.ascend*ia(m,iprm(j))) then
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
