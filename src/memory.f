*dk,memory
C   original file contained one routine called memory()
C   modified to include the memory() routine and wrappers
C   for user queries on memory usage

      subroutine memory(imsgin,xmsgin,cmsgin,msgtype,nwds,
     *                  ierror_return)
C
C
C#######################################################################
C
C      PURPOSE -
C
C         Create a new pset.
C
C      INPUT ARGUMENTS -
C
C         imsgin()  - Integer array of command input tokens
C         xmsgin()  - Real array of command input tokens
C         cmsgin()  - Character array of command input tokens
C         msgtype() - Integer array of command input token types
C         nwds      - Number of command input tokens
C
C      OUTPUT ARGUMENTS -
C
C         ierror_return - Error Return Code (==0 ==> OK, <>0 ==> Error)
C
C      CHANGE HISTORY -
C
C         $Log: memory.f,v $
C         Revision 2.00  2007/11/05 19:46:01  spchu
C         Import to CVS
C
CPVCS    
CPVCS       Rev 1.3   02 May 2001 10:18:40   dcg
CPVCS
CPVCS       Rev 1.2   Mon Apr 14 16:53:26 1997   pvcs
CPVCS    No change.
CPVCS
CPVCS       Rev 1.1   07/17/95 16:11:44   dcg
CPVCS    original version
C
C#######################################################################
C
      implicit none
C
C#######################################################################
C
      integer nwds, imsgin(nwds), msgtype(nwds)
      REAL*8 xmsgin(nwds)
      character*(*) cmsgin(nwds)
C
      integer ierror_return
C
C#######################################################################
C
      integer npoints, nelements
C
      character*32 cmo
C
C#######################################################################
C
C
C
      if(nwds.le.1) then
         npoints   = 1000
         nelements = 6*npoints
      elseif(nwds.le.2) then
         npoints   = imsgin(2)
         nelements = 6*npoints
      else
         npoints   = imsgin(2)
         nelements = imsgin(3)
      endif
C
C.... Get the Current Mesh Object.
C
      call cmo_get_name(cmo, ierror_return)
C
      if(ierror_return .eq. 0) then
C
C....    Adjust length of Memory Managed Arrays.
C
         call cmo_memory(cmo, npoints, nelements, ierror_return)
C
      endif
C
c
      return
      end

C#######################################################################
C     
      subroutine max_mmgetblk(blk_size,blk_num,ierror)
C
C      PURPOSE - make incremental calls to mmgetblk to find how much
C                memory can be allocated before malloc fails.
C
C                need allow user to set block size to test
C                but need some way of protecting against numbers
C                that may cause run time problems due to large
C                size on smaller machine.
C
C      Note: This is the rough version with parameters set at
C            good default numbers. Need to allow adjustment by user.
C       
C      INPUT ARGUMENTS -
C            isize - default = 2000000 - size of block
C            n_blk - number of of calls to mmgetblk each pass 
C            ierror - negative number of times error detected
C
C      OUTPUT ARGUMENTS - captured ierror and screen report
C      
C      This version uses print statements to avoid format errors in
C      reporting the sizes dependant on machine and platform
C      The variable maxsize is dependent on compile and machine
C
C#######################################################################
      implicit none

      integer blk_size, blk_num, ierror

      integer isize, n_blk, imax_err, i
      integer ier(12), ier_prt
      integer isize_min, isize_max, chunk_size,blk_cnt
      integer icount1, icount2, icount3
      integer i1,i2,i3,i4,i5
      integer testloop
  
C     This will have to change for 64 bit compile
      integer BYTES_PER_REAL
      parameter (BYTES_PER_REAL=8)

      character*32 prtnam, blk(12)

      pointer (ip01, a01), (ip02, a02), (ip03, a03), (ip04, a04)
      pointer (ip05, a05), (ip06, a06), (ip07, a07), (ip08, a08)
      pointer (ip09, a09), (ip10, a10), (ip11, a11), (ip12, a12)

      real*8 nbytes, maxsize, totsize,totmax, totlo,tothi

      real*8 a01(*), a02(*), a03(*), a04(*)
      real*8 a05(*), a06(*), a07(*), a08(*)
      real*8 a09(*), a10(*), a11(*), a12(*)

cccccc
c     variable to skip interval test portion
      testloop = 0

C     default settings
C     isize is called by mmgetblk so bytes = isize*sizeof(type)
C     we use type 3 real so nbytes=isize*BYTES_PER_REAL

c     default is a number bigger than 4 byte integer
c     set maxsize to largest number malloc can understand
c     max signed int    2147483647 (32 bit)
c     max unsigned int 2^32 = 4294967296 (32 bit)
c     max signed int  9223372036854775807 (64 bit)

      maxsize = 4000000000. 
      isize = 200000
      n_blk = 3
      if (blk_size .ne. 0) isize = blk_size 
      if (blk_num .ne. 0) n_blk = blk_num
      chunk_size = isize
      nbytes = real(isize*BYTES_PER_REAL)
      ierror = 0
      imax_err = 0
      totlo= 0.
      tothi= 0.
      totsize= 0.

      prtnam = 'part_name'
      blk(1) = 'array_01'
      blk(2) = 'array_02'
      blk(3) = 'array_03'
      blk(4) = 'array_04'
      blk(5) = 'array_05'
      blk(6) = 'array_06'
      blk(7) = 'array_07'
      blk(8) = 'array_08'
      blk(9) = 'array_09'
      blk(10) = 'array_10'
      blk(11) = 'array_11'
      blk(12) = 'array_12'

      icount1 = 1
      icount2 = 1
      icount3 = 1


CCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCC
C     First find an upper limit.
      print *,'Looking for malloc to fail, expect errors .... '
      print *, 'Allocate arrays of ', 
     *         isize,' values and ',nbytes,' bytes'
      print *, 'Max unsigned integer value for 32 bytes is 4294967296'
      print *, 'Max test value for allocated bytes is ',maxsize

      do while ((imax_err .eq. 0) .and. (nbytes.lt.maxsize))

      do i = 1, n_blk
         ier(i) = -1
      enddo
      nbytes = real(BYTES_PER_REAL*isize)
      totsize= 0. 
      print *, icount1,'>> bytes/block = ', nbytes,
     *     ' total = ',nbytes*n_blk 

      if(n_blk.gt. 0)call mmgetblk(blk(1),prtnam,ip01,isize,2,ier(1))
      if((n_blk.gt. 1).and.(ier(1).eq.0)) then
         call mmgetblk(blk(2),prtnam,ip02,isize,2,ier(2))
      endif
      if((n_blk.gt. 2).and.(ier(2).eq.0)) then
         call mmgetblk(blk(3),prtnam,ip03,isize,2,ier(3))
      endif
      if((n_blk.gt. 3).and.(ier(3).eq.0)) then
         call mmgetblk(blk(4),prtnam,ip04,isize,2,ier(4))
      endif
      if((n_blk.gt. 4).and.(ier(4).eq.0)) then
         call mmgetblk(blk(5),prtnam,ip05,isize,2,ier(5))
      endif
      if((n_blk.gt. 5).and.(ier(5).eq.0)) then
         call mmgetblk(blk(6),prtnam,ip06,isize,2,ier(6))
      endif
      if((n_blk.gt. 6).and.(ier(6).eq.0)) then
         call mmgetblk(blk(7),prtnam,ip07,isize,2,ier(7))
      endif
      if((n_blk.gt. 7).and.(ier(7).eq.0)) then
         call mmgetblk(blk(8),prtnam,ip08,isize,2,ier(8)) 
      endif
      if((n_blk.gt. 8).and.(ier(8).eq.0)) then
         call mmgetblk(blk(9),prtnam,ip09,isize,2,ier(9))
      endif
      if((n_blk.gt. 9).and.(ier(9).eq.0)) then
         call mmgetblk(blk(10),prtnam,ip10,isize,2,ier(10))
      endif
      if((n_blk.gt. 10).and.(ier(10).eq.0)) then
         call mmgetblk(blk(11),prtnam,ip11,isize,2,ier(11))
      endif
      if((n_blk.gt. 11).and.(ier(11).eq.0)) then
         call mmgetblk(blk(12),prtnam,ip12,isize,2,ier(12))
      endif

      do i = 1, n_blk
c        print *, 'array number = ',i, '  error flag = ', ier(i)
         if(ier(i) .ne. 0)imax_err = 1
      enddo

c     finish up if imax_err has been reached
      if (imax_err .ne. 0) then
        call mmprint()

        do i = 1, n_blk
c         print *, 'array number = ',i, '  error flag = ', ier(i)
        if (ier(i).eq.0) then
           blk_cnt= blk_cnt+1 
           totsize=totsize+nbytes
        endif
        enddo

      totsize=totsize+nbytes
      endif

      call mmrelprt(prtnam,ier_prt)
      isize = isize * 2
      icount1 = icount1 + 1
      icount3 = icount3 + 1
      
      enddo

c     totsize is the number of bytes for this iteration
c     totlo are number of successful allocations
c     tothi is the number of bytes where failure occured 
      totlo = blk_cnt* nbytes 
      tothi = totsize+nbytes 

      if (tothi.gt.maxsize) then
      print*,"Test stopped at number larger than ",maxsize 
      endif

      print*," "
      print*,"Malloc by increasing block sizes of ",chunk_size
      print*, "Succeeded at  ",totlo/1000000.0," MEGABYTES"
      Print*, "Failed before ",tothi/1000000.0," MEGABYTES"
      print*," "

      return
      end

*dk,memory
