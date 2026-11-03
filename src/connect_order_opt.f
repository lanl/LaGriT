      subroutine connect_order_opt(imsgin,xmsgin,cmsgin,msgtype,nwds)
C #####################################################################
C     PURPOSE -
C        Find the connect option old_order in any position, remove it
C        from the command (so the other options are read exactly as
C        before), and record it for the insertion (lg_brio_order.c).
C        Without it, connect (2D) inserts more than 100 points in a
C        BRIO order; with it, in the order of LaGriT V3.3.4 and
C        earlier.
C     INPUT ARGUMENTS -
C        imsgin,xmsgin,cmsgin,msgtype,nwds - the parsed command
C     OUTPUT ARGUMENTS -
C        the same, without the old_order tokens
C #####################################################################
      implicit none
      integer nwds
      integer imsgin(*),msgtype(*)
      real*8 xmsgin(*)
      character*(*) cmsgin(*)
      integer i,j,iold
C
      iold=0
      i=2
      do while (i.le.nwds)
         if (msgtype(i).eq.3 .and. cmsgin(i).eq.'old_order') then
            iold=1
            do j=i,nwds-1
               imsgin(j)=imsgin(j+1)
               xmsgin(j)=xmsgin(j+1)
               cmsgin(j)=cmsgin(j+1)
               msgtype(j)=msgtype(j+1)
            enddo
            nwds=nwds-1
         else
            i=i+1
         endif
      enddo
      call lg_connect_order_set(iold)
      return
      end
