*dk,select
      subroutine select(k,n,arr,iprm)
c
c #####################################################################
c
c     purpose -
c
c     SELECT takes an integer K, a real array ARR,
c     and an integer array IPRM of length N
c     and reorders IPRM so that ARR(IPRM(K)) is the K'th largest
c     element in the set {ARR(IPRM(i)), i=1,..,N}, and we
c     have in addition the partitioning properties:
c     i<K ==> ARR(IPRM(i)) <= ARR(IPRM(K)) and
c     i>K ==> ARR(IPRM(i)) >= ARR(IPRM(K)).
c
c     This routine is identical to the Numerical Recipes subroutine
c     ``select.f'', except they don't have the ``permutation'' array
c     IPRM; instead they reorder ARR directly.
c
c     Note that setting K=(1+N)/2, this subroutine computes the
c     MEDIAN VALUE of N values in ARR, and it does this in
c     Order(N) time.  This is thus a more efficient way of computing
c     the median than a heap sort which is Order(NlogN).
c
c     input arguments -
c
c         K      -       `pivotal' position, explained above.
c         N      -       number of elements in IPRM
c         ARR    -       real array of values which determine
c                        how IPRM will be reordered.
c         IPRM   -       integer array to be reordered.  Strictly
c                        speaking, IPRM need not be a permutation of
c                        the integers {1,..,N}, but may be simply
c                        a mapping from {1,..,N} onto a set of N
c                        distinct positive integers.
c
c     output arguments -
c
c         IPRM   -       reordered integer array.
c
c     change history -
c
c         $log$
c
c #####################################################################
c
      implicit none
      INTEGER k,n
      REAL*8 arr(*)
      INTEGER i,ir,j,l,mid,ia,itemp,iprm(n)
      l=1
      ir=n
1     if(ir-l.le.1)then
        if(ir-l.eq.1)then
          if(arr(iprm(ir)).lt.arr(iprm(l)))then
            itemp=iprm(l)
            iprm(l)=iprm(ir)
            iprm(ir)=itemp
          endif
        endif
        return
      else
        mid=(l+ir)/2
        itemp=iprm(mid)
        iprm(mid)=iprm(l+1)
        iprm(l+1)=itemp
        if(arr(iprm(l+1)).gt.arr(iprm(ir)))then
          itemp=iprm(l+1)
          iprm(l+1)=iprm(ir)
          iprm(ir)=itemp
        endif
        if(arr(iprm(l)).gt.arr(iprm(ir)))then
          itemp=iprm(l)
          iprm(l)=iprm(ir)
          iprm(ir)=itemp
        endif
        if(arr(iprm(l+1)).gt.arr(iprm(l)))then
          itemp=iprm(l+1)
          iprm(l+1)=iprm(l)
          iprm(l)=itemp
        endif
        i=l+1
        j=ir
        ia=iprm(l)
3       continue
          i=i+1
        if(arr(iprm(i)).lt.arr(ia))goto 3
4       continue
          j=j-1
        if(arr(iprm(j)).gt.arr(ia))goto 4
        if(j.lt.i)goto 5
        itemp=iprm(i)
        iprm(i)=iprm(j)
        iprm(j)=itemp
        goto 3
5       iprm(l)=iprm(j)
        iprm(j)=ia
        if(j.ge.k)ir=j-1
        if(j.le.k)l=i
      endif
      goto 1
      END
C  (C) Copr. 1986-92 Numerical Recipes Software $!{.9s21-.
