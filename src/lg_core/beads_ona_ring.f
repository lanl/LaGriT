 
C#####################################################################
C
C      FILE - beads_ona_ring.f
C             subroutines once located in temptam.f
C             and used by the stack routine
C
C      CHANGE HISTORY -
C
C      Updates - T.A.Miller - May 2011
C      Original version - T.Cherry - 97
C
C#####################################################################

C     This is a sample driver for beads_ona_ring()
C     Note, it uses malloc instead of lagrit mmgetblk routines
C     This will cause issues if used withing the lagrit libraries
 
      subroutine beads_ona_ring_2d_sp (errmsg,
     >  z_out2d,z_in2d,d_pinch,d_min,id_move,npoint,nvec,iorder,ierr)

C     This is a driver routine for the beads_ona_ring program. It
C     is set up to take a 2d array as input and process the rows
C     through the beads_ona_ring subroutine. While
C     the calling program may treat the input as a 2d array a(i,j)
C     this driver program treats the input and output array as a
C     one dimensional array. To make this general the iorder parameter
C     is used to inform the driver if the input array to be processed
C     is row ordered a(i,j), a(i+1,j), a(i+2,j) ...   iorder = 1
C     column ordered a(i,j), a(i,j+1), a(i,j+2) ...   iorder = 2
C
C     For the above ordering to work correctly the input array must
C     have the same dimensions as npoint and nvec.

      implicit none

C     Passed in variables
      integer npoint, nvec, iorder, ierr
      character*82 errmsg
      real*8 z_in2d(npoint*nvec)
      real*8 z_out2d(npoint*nvec)
      real*8 d_pinch(npoint-1)
      real*8 d_min(npoint-1)
      integer id_move(npoint-1)
c
c     Declare malloc as integer function
      integer malloc
c
c     Local variables
      real*8 z_in, z_out
      pointer (ipz_in,  z_in(*))
      pointer (ipz_out, z_out(*))
      real*8 d_pinch_wk, d_min_wk
      pointer (ipd_pinch_wk, d_pinch_wk(*))
      pointer (ipd_min_wk,   d_min_wk(*))
 
      integer i, n, nbyte_r8, nbyte_r4
C
C     This version of the code assumes the reals that are passed in
C     z_out2d,z_in2d,d_pinch,d_min are single precision real*4
C
C
C     If the user wishes to overwrite the input array with the filtered
C     output array, the first two arguments of beads_ona_ring_2d can
C     be the same array (i.e. call beads_ona_ring_2d(a,a,...)).
C
C
c
c     Allocate Memory
c
      errmsg = '-undefined error-'
      nbyte_r8 = 8*npoint
      nbyte_r4 = 8*(npoint-1)
      ipz_in  = malloc(nbyte_r8)
      ipz_out = malloc(nbyte_r8)
      ipd_pinch_wk = malloc(nbyte_r4)
      ipd_min_wk = malloc(nbyte_r4)
c
      do i = 1, npoint-1
         d_pinch_wk(i) = 0.0d0
         d_pinch_wk(i) = d_pinch(i)
         d_min_wk(i)   = 0.0d0
         d_min_wk(i)   = d_min(i)
      enddo
 
      do n = 1,nvec
         if(iorder .eq. 1)then
            do i = 1, npoint
               z_in(i) = z_in2d(i+npoint*(n - 1))
            enddo
         elseif(iorder .eq. 2)then
            do i = 1, npoint
               z_in(i) = z_in2d(n+(i*nvec))
            enddo
         endif
c
         call beads_ona_ring
     1      (errmsg,z_out,z_in,d_pinch,d_min,id_move,npoint,ierr)
c
         if(iorder .eq. 1)then
            do i = 1, npoint
               z_out2d(i+npoint*(n - 1)) = z_out(i)
            enddo
         elseif(iorder .eq. 2)then
            do i = 1, npoint
               z_out2d(n+(i*nvec)) = z_out(i)
            enddo
         endif
      enddo
 
      call free(ipz_in)
      call free(ipz_out)
      return
      end

C############################################################## 
 
      subroutine beads_ona_ring_2d (errmsg,
     >  z_out2d,z_in2d,d_pinch,d_min,id_move,npoint,nvec,iorder,ierr)

C############################################################## 
      implicit none

C     Passed in variables
      character*82 errmsg
      integer npoint, nvec, iorder, ierr
      real*8 z_in2d(npoint*nvec)
      real*8 z_out2d(npoint*nvec)
      real*8 d_pinch(npoint-1)
      real*8 d_min(npoint-1)
      integer id_move(npoint-1)
c
c     Declare malloc as integer function
      integer malloc
c
c     Local variables
      real*8 z_in, z_out
      pointer (ipz_in,  z_in(*))
      pointer (ipz_out, z_out(*))
 
      integer i, n, nbyte_r
C
C
C     This version of the code assumes the reals that are passed in
C     z_out2d,z_in2d,d_pinch,d_min are single precision real*8
C
C     This is a driver routine for the beads_ona_ring program. It
C     is set up to take a 2d array as input and process the rows
C     through the beads_ona_ring subroutine. While
C     the calling program may treat the input as a 2d array a(i,j)
C     this driver program treats the input and output array as a
C     one dimensional array. To make this general the iorder parameter
C     is used to inform the driver if the input array to be processed
C     is row ordered a(i,j), a(i+1,j), a(i+2,j) ...   iorder = 1
C     column ordered a(i,j), a(i,j+1), a(i,j+2) ...   iorder = 2
C
C     For the above ordering to work correctly the input array must
C     have the same dimensions as npoint and nvec.
C
C     If the user wishes to overwrite the input array with the filtered
C     output array, the first two arguments of beads_ona_ring_2d can
C     be the same array (i.e. call beads_ona_ring_2d(a,a,...)).
C
C
c
c     Allocate Memory
c
      errmsg = '-undefined error-'
      nbyte_r = 8*npoint
      ipz_in  = malloc(nbyte_r)
      ipz_out = malloc(nbyte_r)
c
      do n = 1,nvec
         if(iorder .eq. 1)then
            do i = 1, npoint
               z_in(i) = z_in2d(i+npoint*(n - 1))
            enddo
         elseif(iorder .eq. 2)then
            do i = 1, npoint
               z_in(i) = z_in2d(n+(i*nvec))
            enddo
         endif
c
         call beads_ona_ring (errmsg,
     1       z_out,z_in,d_pinch,d_min,id_move,npoint,ierr)
c
         if(iorder .eq. 1)then
            do i = 1, npoint
               z_out2d(i+npoint*(n - 1)) = z_out(i)
            enddo
         elseif(iorder .eq. 2)then
            do i = 1, npoint
               z_out2d(n+(i*nvec)) = z_out(i)
            enddo
         endif
      enddo
 
      call free(ipz_in)
      call free(ipz_out)
      return
      end
 
C#######################################################
C     This is the routine used within the stack method
C
C     Modified May 2011 to use mmgetblk instead of malloc
 
      subroutine beads_ona_ring
     1   (errmsg,zout,z,d_pinch,d_min,id_move,npoint,ierr)

C#######################################################

      implicit none
c     Passed in variables
      character*82 errmsg
      integer npoint, ierr
      real*8 z(npoint)
      real*8 zout(npoint)
      real*8 d_pinch(npoint-1)
      real*8 d_min(npoint-1)
      integer id_move(npoint-1)
c
      integer malloc

c     Local variables
      real*8 d, d_change, d_up, d_dn
      integer id_status, ipt_up, ipt_dn
      pointer (ipd, d(*))
      pointer (ipd_change, d_change(*))
      pointer (ipd_up,     d_up(*))
      pointer (ipd_dn,     d_dn(*))
      pointer (ipid_status, id_status(*))
      pointer (ipipt_up,    ipt_up(*))
      pointer (ipipt_dn,    ipt_dn(*))
 
      integer i, itrue, ifalse, npoint_last_call, icall
      integer ndist, nbyte_r, nbyte_i
      real*8  d_total, z_start, z_end, d_min_sum
      data icall / 0 /
 
C     NAME: beads_ona_ring
C           (errmsg,zout,z,d_pinch,d_min,id_move,npoint,ierr)
C
C     errmsg         Is a character string that will contain any possible
C                    error messeges.
C     zout           output array with new elevation values
C     z              input array of zic values, ordered from bottom to top
C                    must be monotonic increasing
c     d_pinch        If interval length d <= d_pinch then set to zero
c     d_min          If interval length is d_pinch < d < d_min set to d_min
c     id_move   = 1  Get or put values equally up and down
c                 2  Get or put values up only
c                 3  Get or put values down only
c     id_status =  0  Interval has been set to zero or d_min, don't change.
c               =  1  Candidate for setting to zero
c               = -1  Interval has been set to zero but redistribution has
c                        has not been done.
c               =  2  Candidate for setting to d_min
c               = -2  Interval has been set to d_min but redistribution has
c                        has not been done.
c               =  3  Interval length is > d_min
C     npoint          number of elevations in arrays
C     ierr            returns 0 if no errors
c #######################################################
C     BEGIN
c     begin main code for beads_ona_ring

      icall = icall + 1
      errmsg = '-undefined error-'
      ierr = 0
      npoint_last_call = npoint
      ndist = npoint - 1
  
c
c     Note, in usage these are all real*8
c     I do not know why some were allocating real*4
c     tam - changed all to real*8

      nbyte_r = 8*ndist
      nbyte_i = 4*ndist
      ipd =        malloc(nbyte_r)
      ipd_change = malloc(nbyte_r)
      ipd_up =     malloc(nbyte_r)
      ipd_dn =     malloc(nbyte_r)
      ipid_status= malloc(nbyte_r)
      ipipt_up =   malloc(nbyte_r)
      ipipt_dn =   malloc(nbyte_r)

c     REPLACE ALL malloc calls with mmgetblk calls

C     allocate work arrays
C      ilen=ndist
C      call mmgetblk('d',isubname,ipd,ilen,2,ics)
C      if(ics.ne.0 ) then
C       call x3d_error("beads_ona_ring",'mmgetblk work arrays for sink')
C       goto 9000
C      endif

c
c     Initialize some vectors
c
      z_start = z(1)
      call set_pointers(ipt_up,ipt_dn,ndist)
      do i = 1,ndist
         d_change(i) = 0.0d0
         id_status(i) = 3
      enddo
 
C     Do some error checking on the points.
c
c     Check that input vector is monotonic increasing.
c     It may happen that a layer dips below another and needs
c     to be truncated before points are sent to this routine.
      call monotonic(ifalse,z,npoint)
      if(ifalse .ne. 0) then
        write(errmsg,'(a)')'Error 10: Input not monotonic increasing.'
        call z_to_d(d,z,npoint)
        call d_to_z(zout,d,z_start,npoint)
        ierr = 10
        goto 9991
      endif
c
c     Check that d_pinch input is < d_min input for all intervals
      call check_input(itrue,d_pinch,d_min,ndist)
      if(itrue .ne. 0) then
        write(errmsg,'(a)')'Error 20: d_pinch >= d_min'
        ierr = 20
        call z_to_d(d,z,npoint)
        call d_to_z(zout,d,z_start,npoint)
        goto 9991
      endif
c
c     Check that sum(d_min) < z(npoint) - z(1)
      d_total = z(npoint) - z(1)
      z_start = z(1)
      z_end   = z(npoint)
      call check_d_min(itrue,d_min_sum,d_total,d_min,ndist)

C     tam - comment out if this is too much information
C     this can happen if divisions become too many in relation to thickness
      if(itrue .ne. 0) then
         write(6,*)
     *  'Error 30: sum(d_min)>d_total ',itrue, d_min_sum, d_total
         write(6,*)
     *  'Error: sum(d_min) should be a small percentage of d_total'
         write(errmsg,'(a)')'Error 30: sum(d_min) > z(npoint) - z(1)'
         ierr = 30
         call z_to_d(d,z,npoint)
         call d_to_z(zout,d,z_start,npoint)
         goto 9991
      endif
 
 9991 if (ierr .ne. 0 ) then
          write(6,*)'Error: No Action Possible - Vector not processed'
          write(6,*)'       Output = Input'
          goto 9999
      endif
 
 
C     if we get this far, the arrays are set up as expected
      ierr = 0
c
c     Calculate distance vector from coordinate vector.
c
      call z_to_d(d,z,npoint)
c
c     Flag intervals which will be pinched out.
c
      call set_id_status(id_status,d,d_pinch,d_min,ndist)
c
c     Pinch out flagged intervals
c
      call pinch_d(d,d_change,d_up,d_dn,id_move,id_status,ndist)
c
c     Redistribute pinched out length among candidate neighbors.
c
      call re_distribute_pinch
     1  (d,d_change,d_up,d_dn,id_status,id_move,ipt_up,ipt_dn,ndist)
c
c     Reset interval flags to identify intervals which will have
c     lenght added to them.
c
      call set_id_status(id_status,d,d_pinch,d_min,ndist)
c
c     Add lenght to flagged intervals.
c
      call pop_d(d,d_change,d_up,d_dn,d_min,id_move,id_status,ndist)
c
c     Redistribute added length by subtracting from candidate neighbors.
c
      call re_distribute_pop
     1  (d,d_change,d_up,d_dn,d_min,
     2   id_status,id_move,ipt_up,ipt_dn,ndist,ierr)
      if (ierr.ne.0) then
        write(errmsg,'(a)')'Error from routine re_distribute_pop '
        goto 9999
      endif
c
c      do i = 1,ndist
c       write(6,805)i,d(i),d_change(i),id_status(i)
c 805   format('slide pop',i5,2e15.5,i5 )
c      enddo
c
c     Convert interval lenght vector to coordinate vector.
c
      call d_to_z(zout,d,z_start,npoint)
c
c     Free memory
c
      call free(ipd)
      call free(ipd_change)
      call free(ipd_up)
      call free(ipd_dn)
      call free(ipid_status)
      call free(ipipt_up)
      call free(ipipt_dn)
 
 9999 return
      end
 
      subroutine re_distribute_pinch
     1  (d,d_change,d_up,d_dn,id_status,id_move,ipt_up,ipt_dn,ndist)
      implicit none
      integer ndist
      real*8 d(ndist)
      real*8 d_up(ndist)
      real*8 d_dn(ndist)
      real*8 d_change(ndist)
      integer id_status(ndist)
      integer id_move(ndist)
      integer ipt_up(ndist)
      integer ipt_dn(ndist)
      integer i, i_up, i_dn
 
      do i = 1,ndist
        if(id_status(i) .lt. 0)then
           if((id_move(i) .eq. 1) .or. (id_move(i) .eq. 2))then
              i_up = ipt_up(i)
              dowhile(id_status(i_up) .le. 1)
              i_up = ipt_up(i_up)
              enddo
              d(i_up) = d(i_up) + d_up(i)
              d_change(i_up) = d_change(i_up) + d_up(i)
           endif
           if((id_move(i) .eq. 1) .or. (id_move(i) .eq. 3))then
              i_dn = ipt_dn(i)
              dowhile(id_status(i_dn) .le. 1)
              i_dn = ipt_dn(i_dn)
              enddo
              d(i_dn) = d(i_dn) + d_dn(i)
              d_change(i_dn) = d_change(i_dn) + d_dn(i)
           endif
           id_status(i) = 0
        endif
      enddo
 
      return
      end
 
      subroutine re_distribute_pop
     1  (d,d_change,d_up,d_dn,d_min,
     2    id_status,id_move,ipt_up,ipt_dn,ndist,ierr)
      implicit none
      integer ndist,ierr
      real*8 d(ndist)
      real*8 d_up(ndist)
      real*8 d_dn(ndist)
      real*8 d_min(ndist)
      real*8 d_change(ndist)
      integer id_status(ndist)
      integer id_move(ndist)
      integer ipt_up(ndist)
      integer ipt_dn(ndist)
      integer i, i_up, i_dn
 
      ierr = 0
      do i = 1,ndist
        if(id_status(i) .lt. 0)then
c
c       Found an interval which was poped and needs go gain
c       some room from above or below.
c
           if((id_move(i) .eq. 1) .or. (id_move(i) .eq. 2))then
c
c             Look up for an interval that can be shrunk.
c
              i_up = ipt_up(i)
c              dowhile(id_status(i_up) .le. 0)
              dowhile((id_status(i_up)   .le. 0).or.
     1                (d(i_up)           .lt. 2*d_up(i)).or.
     2                (d(i_up) + d_up(i) .lt. d_min(i_up)))
              i_up = ipt_up(i_up)
              enddo
c
c             Interval d(i_up) is made smaller by d_up(i). d_up(i) is < 0.
c
c   debug
c   comment out write statements if this is too much
              d(i_up) = d(i_up) + d_up(i)
              if(d(i_up) .lt. 0.0d0)then
                 write(6,*)'Error:pop up',i, i_up, d(i_up), d_up(i)
                 ierr = 1
              endif
              if(d(i_up) .lt. d_min(i_up))then
                 write(6,*)'Error:min up',i, i_up, d(i_up), d_min(i_up)
                 ierr = 1
              endif
              d_change(i_up) = d_change(i_up) + d_up(i)
           endif
           if((id_move(i) .eq. 1) .or. (id_move(i) .eq. 3))then
c
c             Look down for an interval that can be shrunk.
c
              i_dn = ipt_dn(i)
c              dowhile(id_status(i_dn) .le. 0)
              dowhile((id_status(i_dn)   .le.0).or.
     1                (d(i_dn)           .lt.2*d_dn(i)).or.
     2                (d(i_dn) + d_dn(i) .lt. d_min(i_dn)))
              i_dn = ipt_dn(i_dn)
              enddo
c
c             Interval d(i_dn) is made smaller by d_dn(i). d_dn(i) is < 0.
c
c   debug
c   comment out write statements if this is too much
              d(i_dn) = d(i_dn) + d_dn(i)
              if(d(i_dn) .lt. 0.0d0)then
              write(6,*)'Error:pop dn',i, i_dn, d(i_dn), d_dn(i)
                 ierr = 1
              endif
              if(d(i_dn) .lt. d_min(i_dn))then
              write(6,*)'Error:min dn',i, i_dn, d(i_dn), d_min(i_dn)
                 ierr = 1
              endif
              d_change(i_dn) = d_change(i_dn) + d_dn(i)
           endif
           id_status(i) = 0
        endif
      enddo
 
      return
      end
 
      subroutine pop_d
     1   (d,d_change,d_up,d_dn,d_min,id_move,id_status,ndist)
      implicit none
      integer ndist
      real*8 d(ndist)
      real*8 d_change(ndist)
      real*8 d_up(ndist)
      real*8 d_dn(ndist)
      real*8 d_min(ndist)
      integer id_move(ndist)
      integer id_status(ndist)
      integer i
      real*8  d_get
 
      do i = 1, ndist
        if(id_status(i) .eq. 2)then
           d_get = d_min(i) - d(i)
           d_change(i) = d_change(i) +  d_get
           id_status(i) = - id_status(i)
           if(id_move(i) .eq. 1)then
              d_up(i) = - 0.5d0*d_get
              d_dn(i) = d_up(i)
           elseif(id_move(i) .eq. 2)then
              d_up(i) = -d_get
              d_dn(i) = 0.0d0
           elseif(id_move(i) .eq. 3)then
              d_up(i) = 0.0d0
              d_dn(i) = -d_get
           endif
           d(i) = d_min(i)
        endif
      enddo
 
      return
      end
 
      subroutine pinch_d(d,d_change,d_up,d_dn,id_move,id_status,ndist)
      implicit none
      integer ndist
      real*8 d(ndist)
      real*8 d_change(ndist)
      real*8 d_up(ndist)
      real*8 d_dn(ndist)
      integer id_move(ndist)
      integer id_status(ndist)
      integer i
 
 
      do i = 1, ndist
        if(id_status(i) .eq. 1)then
           d_change(i) = d_change(i) - d(i)
           id_status(i) = - id_status(i)
           if(id_move(i) .eq. 1)then
              d_up(i) = 0.5d0*d(i)
              d_dn(i) = d_up(i)
           elseif(id_move(i) .eq. 2)then
              d_up(i) = d(i)
              d_dn(i) = 0.0d0
           elseif(id_move(i) .eq. 3)then
              d_up(i) = 0.0d0
              d_dn(i) = d(i)
           endif
           d(i) = 0.0d0
        endif
      enddo
 
      return
      end
 
      subroutine set_pointers(ipt_up,ipt_dn,ndist)
      implicit none
      integer ndist
      integer ipt_up(ndist)
      integer ipt_dn(ndist)
      integer i
 
      do i = 1, ndist
        ipt_up(i) = i+1
      enddo
      ipt_up(ndist) = 1
 
      do i = 1, ndist
         ipt_dn(i) = i-1
      enddo
      ipt_dn(1) = ndist
 
      return
      end
      subroutine set_id_status(id_status,d,d_pinch,d_min,ndist)
      implicit none
      integer ndist
      integer id_status(ndist)
      real*8 d(ndist)
      real*8 d_pinch(ndist)
      real*8 d_min(ndist)
      integer i
 
      do i = 1, ndist
 
         if(id_status(i) .ne. 0)then
         if(d(i) .le. d_pinch(i))then
            id_status(i) = 1
         elseif((d(i) .gt. d_pinch(i)) .and. (d(i) .le. d_min(i)))then
            id_status(i) = 2
         else
            id_status(i) = 3
         endif
         endif
      enddo
      return
      end
 
c     INPUT Z(npoint) elevations
c     OUTPUT d(npoint) with distance between upper and lower elevation
c            for each layer
c
      subroutine z_to_d(d,z,npoint)
      implicit none
      integer npoint
      real*8 z(npoint)
      real*8 d(npoint-1)
      integer i
 
      do i = 1, npoint-1
         d(i) = z(i+1) - z(i)
      enddo
      return
      end
 
c     INPUT z_start         original start elevations
c           d(npoint)       distance between two points
c     OUTPUT z(npoint)      fill output elevations
 
      subroutine d_to_z(z,d,z_start,npoint)
      implicit none
      integer npoint
      real*8 z(npoint)
      real*8 d(npoint-1)
      real*8  z_start
      integer i
 
      z(1) = z_start
      do i = 1, npoint-1
         z(i+1) = z(i) + d(i)
      enddo
      return
      end
 
      subroutine check_input(itrue,d_pinch,d_min,ndist)
      implicit none
      integer itrue, ndist
      real*8 d_pinch(ndist)
      real*8 d_min(ndist)
      integer i
 
      itrue = 0
      do i = 1, ndist
         if( d_pinch(i) .ge. d_min(i))then
            itrue = itrue + 1
         endif
      enddo
      return
      end
 
      subroutine check_d_min(itrue,d_min_sum,d_total,d_min,ndist)
      implicit none
      integer itrue, ndist
      real*8 d_min(ndist)
      real*8 d_total
      real*8 d_min_sum
      integer i
 
      itrue = 0
      d_min_sum = 0.0d0
      do i = 1, ndist
         d_min_sum = d_min_sum + d_min(i)
      enddo
 
      if(d_min_sum .gt. d_total)itrue = -1
 
      return
      end
 
C     check that values are increasing
      subroutine monotonic(ifalse,z,npoint)
      implicit none
      integer ifalse, npoint
      real*8 z(npoint)
      integer i
 
 
      ifalse = 0
      do i = 2, npoint
c        if ifalse set, then not monotonic
         if(z(i) .lt. z(i-1))then
            ifalse = ifalse + 1
         endif
      enddo
      return
      end

C end file beads_ona_ring.f
