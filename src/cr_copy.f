*dk, cr_copy
       subroutine cr_copy(n, isink,isource,mask,mask2)
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
C        $Log: cr_copy.f,v $
C        Revision 2.00  2007/11/05 19:45:51  spchu
C        Import to CVS
C
CPVCS    
CPVCS       Rev 1.5   14 Mar 2001 13:20:38   dcg
CPVCS    get rid of upper case
CPVCS
CPVCS       Rev 1.4   14 Mar 2001 13:04:56   dcg
CPVCS    use implicit none
CPVCS
CPVCS       Rev 1.3   Mon Apr 14 16:43:02 1997   pvcs
CPVCS    No change.
CPVCS
CPVCS       Rev 1.2   11/07/95 17:15:54   dcg
CPVCS    change flag to 2 in mmgetblk calls
CPVCS
CPVCS       Rev 1.1   06/27/95 11:10:22   dcg
CPVCS    remove second literal argument in memory management calls
CPVCS
CPVCS       Rev 1.0   11/10/94 12:11:30   pvcs
CPVCS    Original version.
C
C ######################################################################
C
       implicit none
C$$$
C
C
C$$$
      integer n,indx,i,icscode,length,indx_max,j
       integer isink(n), isource(n)
       logical mask(n), mask2(n)
 
       pointer (ipitemp, itemp)
       integer itemp(n + 1)
      character*8 icrname
 
      icrname='cr_sum'
       length=n
       call mmgetblk('itemp',icrname,ipitemp,length + 1,2,icscode)
       indx = 1
       do i=1,n
          if (mask(i)) then
             itemp(indx) = i
            indx = indx + 1
          endif
       enddo
 
       indx_max = indx
       itemp(indx) = n + 1
 
       do 40 i=1,indx_max-1
        do 30 j=itemp(i),itemp(i+1)-1
           if(mask2(j)) then
            isink(j)=isource(itemp(i))
           endif
 30     continue
 40    continue
 
       goto 9999
 9999  continue
 
       call mmrelblk('itemp',icrname,ipitemp,icscode)
       return
       end
