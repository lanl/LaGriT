*dk,hpsort1
      SUBROUTINE hpsort1(n,ra,ascend,iprm)
C
C
C#######################################################################
C
C     PURPOSE -
C
C     HPSORT1 takes an integer N, a real array RA,
C     an integer array IPRM of length N, and a real
C     number ASCEND, and 
C     reorders IPRM so that RA(IPRM(1)),...,RA(IPRM(N))
C     is in ascending order if ASCEND is positive and
C     is in decreasing order if ASCEND is negative.
C
C     INPUT ARGUMENTS -
C
C        N - number of elements to be sorted.
C        RA - real array of values which determine
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
C        $Log: hpsort1.f,v $
C        Revision 2.00  2007/11/03 00:49:11  spchu
C        Import to CVS
C
CPVCS    
CPVCS       Rev 1.0   22 Aug 2000 10:21:44   dcg
CPVCS    Initial revision.
CPVCS    
CPVCS       Rev 1.0   Tue Jun 03 00:22:48 1997   kuprat
CPVCS    Initial revision.
C
C#######################################################################
C
 
      implicit none
 
      INTEGER n
      REAL*8 ra(*),ascend
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
            if(ascend*ra(iprm(j)).lt.ascend*ra(iprm(j+1)))j=j+1
          endif
          if(ascend*ra(irra).lt.ascend*ra(iprm(j)))then
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
