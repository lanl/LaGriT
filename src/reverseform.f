      subroutine reverseform(m,irowptr,icol,n,icolptr,irow)
C#######################################################################
C
C     PURPOSE -
C
c     Assume we have two sets of integers ROW={1,...,m} and COL={1,...n},
c     we say a relation between ROW and COL is a set of ordered pairs
c     (a_k,b_k), k=1,numentries.  The row-packed sparse form
c     of this relation consists of arrays IROWPTR, ICOL such that
c       (  (i,ICOL(k)), IROWPTR(i) <= k < IROWPTR(i+1) ), 1 <= i <= m
c     represents our relation.  The column-packed sparse form
c     consists of arrays ICOLPTR, IROW such that 
c       (  (IROW(k),j), ICOLPTR(j) <= k < ICOLPTR(j+1) ), 1 <= j <= n
c     also represents the relation.  This routine converts the 
c     row-packed form to the column-packed form.
c
C        $Log: reverseform.f,v $
C        Revision 1.1  2006/03/20 04:30:41  kuprat
C        Initial revision.
C
c
C#######################################################################
      implicit none
      integer m,irowptr(*),icol(*),n,icolptr(*),irow(*)
      pointer (ipikey,ikey)
      integer ikey(2,*),itemp(2),numentries,k,i,j,jcurr,icscode
      character*32 isubname

      isubname='reverseform'

      numentries=irowptr(m+1)-1
      call mmgetblk('ikey',isubname,ipikey,2*numentries,1,icscode)

      k=0
      do i=1,m
         do j=irowptr(i),irowptr(i+1)-1
            k=k+1
            ikey(1,k)=icol(k)
            ikey(2,k)=i
         enddo
      enddo

      call hpsortim(numentries,2,2,itemp,ikey)

      jcurr=0
      do k=1,numentries
c... Following loop will execute zero times within a column, once if we
c... skip to next column, and X times if we skip forward X columns.
c... Since we don't expect to skip an entire column, we expect this loop
c... to execute at most once.
         do j=jcurr+1,ikey(1,k)
            icolptr(j)=k
         enddo
         jcurr=ikey(1,k)
         irow(k)=ikey(2,k)
      enddo
c... Unless there are some empty columns, this loop should execute just once.
      do j=jcurr+1,n+1
         icolptr(j)=numentries+1
      enddo

      call mmrelprt(isubname,icscode)

      return
      end
