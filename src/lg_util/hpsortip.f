*dk,hpsortip
      SUBROUTINE hpsortip(n,ia,ascend,iprm)
C
C
C#######################################################################
C
C     PURPOSE -
C
C     HPSORTIP ("HeaP SORT an Integer array, producing a
C     Permutaion") takes an integer N, an integer array IA,
C     an integer array IPRM of length N, and a real
C     number ASCEND, and 
C     reorders IPRM so that IA(IPRM(1)),...,IA(IPRM(N))
C     is in ascending order if ASCEND is positive and
C     is in decreasing order if ASCEND is negative.
C
C     INPUT ARGUMENTS -
C
C        N - number of elements to be sorted.
C        IA - integer array of values which determine
C             how IPRM will be reordered.
C        IPRM - integer array to be reordered.  Strictly
c                        speaking, IPRM need not be a permutation of
c                        the integers {1,..,N}, but may be simply
c                        a mapping from {1,..,N} onto a set of N
c                        distinct positive integers.
C        ASCEND - real which controls whether we sort in ascending
C                 or descending order.
C
C     OUTPUT ARGUMENTS -
C
C        IPRM - reordered integer array.
C
C     CHANGE HISTORY -
C
C        $Log: hpsortip.f,v $
C        Revision 2.00  2007/11/03 00:49:11  spchu
C        Import to CVS
C
CPVCS    
CPVCS       Rev 1.0   22 Aug 2000 10:21:52   dcg
CPVCS    Initial revision.
CPVCS    
CPVCS       Rev 1.0   Tue Aug 24 21:57:50 1999   kuprat
CPVCS    Initial revision.
C
C#######################################################################
C
 
      implicit none
 
      INTEGER n,ia(*)
      REAL*8 ascend
      INTEGER i,ir,j,l,iprm(n),irra
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
            if(ascend*ia(iprm(j)).lt.ascend*ia(iprm(j+1)))j=j+1
          endif
          if(ascend*ia(irra).lt.ascend*ia(iprm(j)))then
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
