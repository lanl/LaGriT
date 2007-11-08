*dk, cr_add
       subroutine cr_add(n, isink, isource, mask, mask2)
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
C        $Log:   /pvcs.config/t3d/src/cr_add.f_a  $
CPVCS    
CPVCS       Rev 1.5   14 Mar 2001 13:18:12   dcg
CPVCS    get rid of upper case
CPVCS
CPVCS       Rev 1.4   14 Mar 2001 13:03:06   dcg
CPVCS    use implicit none
CPVCS
CPVCS       Rev 1.3   Mon Apr 14 16:43:00 1997   pvcs
CPVCS    No change.
CPVCS
CPVCS       Rev 1.2   11/07/95 17:15:52   dcg
CPVCS    change flag to 2 in mmgetblk calls
CPVCS
CPVCS       Rev 1.1   06/27/95 11:10:10   dcg
CPVCS    remove second literal argument in memory management calls
CPVCS
CPVCS       Rev 1.0   11/10/94 12:11:28   pvcs
CPVCS    Original version.
C
C ######################################################################
C
       implicit none
C$$$
C
C
C$$$
      integer n
       integer isink(n), isource(n)
       logical mask(n), mask2(n)
       pointer (ipitemp, itemp)
       integer itemp(n)
      integer i,isum,indx_max,length,icscode,indx,j
      character*8 isubname
 
      isubname='cr_add'
       length=n
       call mmgetblk('itemp',isubname,ipitemp,length,2,icscode)
       indx = 1
       do i=1,n
          if (mask(i)) then
             itemp(indx) = i
            indx = indx + 1
          endif
       enddo
 
       indx_max = indx
       itemp(indx) = n + 1
 
       do 20 i=1,indx_max-1
        isum=0
         do 10 j=itemp(i),itemp(i+1)-1
            if (mask2(j)) then
             isum=isum+isource(j)
            isink(j) = isum
            endif
 10      continue
 20     continue
 
       goto 9999
 9999  continue
       call mmrelblk('itemp',isubname,ipitemp,icscode)
       return
       end
