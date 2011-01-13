
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
      include 'machine.h' 
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
      include 'machine.h'


      integer blk_size, blk_num, ierror

      integer isize, imax_err, i
      integer ier(12), ier_prt
      integer isize_min, isize_max, chunk_size,blk_cnt
      integer testloop

C     these are small integers to save on print spacing
      integer*4 icount1, icount2, icount3, n_blk
  
C     defined in include file memory.h
C     integer BYTES_PER_REAL
C     parameter (BYTES_PER_REAL=8)

      character*32 prtnam, blk(12)

      pointer (ip01, a01), (ip02, a02), (ip03, a03), (ip04, a04)
      pointer (ip05, a05), (ip06, a06), (ip07, a07), (ip08, a08)
      real*8 a01(*), a02(*), a03(*), a04(*)
      real*8 a05(*), a06(*), a07(*), a08(*)

      real*8 nbytes,rsize,maxsize,totsize,totmax,
     *       totlo,tothi,nbfail

cccccc
C begin

c     variable to skip interval test portion
      testloop = 0
      nbfail = 0.

C     default settings
C     isize is called by mmgetblk so bytes = isize*sizeof(type)
C     we use type 3 real so nbytes=isize*BYTES_PER_REAL

c     default is a number bigger than 4 byte integer
c     set maxsize to largest number malloc can understand
c     to protect against a bad happening
c
C Note that malloc will use the argument as unsigned int
C max unsigned int 2^32 =              4,294,967,295
C max unsigned int 2^64 = 18,446,744,073,709,551,615 
C
C write size values as real for large unsigned int values 
C
      nbytes = real(isize*BYTES_PER_REAL)
      chunk_size = nbytes

C     Allow larger maxsize to check for error catching
C     Subtract reasonable number for safe code
      maxsize = 4000000000. - (nbytes*n_blk)
      maxsize = 4000000000. 
      if (BYTES_PER_PTR .gt. 4) then
        maxsize = 1.8e+19 - (nbytes*n_blk)
        maxsize = 1.8e+19 
      endif


C     increase the block size and increment by 1
C     will run into restrictions in size to malloc call
C     so can tell where things fail for bad numbers 32 bit
C
C     decrease size and increase number of blocks for
C     increasing the total calls to malloc but larger
C     total bytes allocated in end
C     code allows up to 6 n_blk calls each loop
      isize = 200000
      rsize  = real(isize)
      nbytes = real(BYTES_PER_REAL*isize)

C     simplify this piece of code to increment until failure
C     then report
      n_blk = 3
      n_blk = 1

      if (blk_size .ne. 0) isize = blk_size 
      if (blk_num .ne. 0) n_blk = blk_num

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

      icount1 = 1
      icount2 = 1
      icount3 = 1


CCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCC
C     First find an upper limit.

      print*,'Looking for malloc to fail, expect errors .... '
      print*,'Allocate blocks of ', 
     *         isize,' real values and ',nbytes,' bytes'
      print*,'Max unsigned for 32 bit is              4,294,967,295'
      print*,'Max unsigned for 64 bit is 18,446,744,073,709,551,615'
      print*,'Stop test value for allocated bytes is ',maxsize

C---- LOOP continues until malloc error or maxsize reached ----
      do while ((imax_err .eq. 0) .and. (nbytes.lt.maxsize))

      do i = 1, n_blk
         ier(i) = -1
      enddo

      rsize  = real(isize)
      nbytes = real(BYTES_PER_REAL*isize)
      totsize= 0. 
      print *, icount1,' >> ',n_blk,' number reals = ', rsize,
     *     ' total bytes = ',nbytes*n_blk 


      if(n_blk.gt. 0) then
        call mmgetblk(blk(1),prtnam,ip01,isize,2,ier(1))
      if (ier(1).lt.0) then
        nbfail=real(BYTES_PER_REAL*isize)
      else
        totlo=real(BYTES_PER_REAL*isize)
        totsize = totsize+totlo
      endif
      endif

      if((n_blk.gt. 1).and.(ier(1).eq.0)) then
         call mmgetblk(blk(2),prtnam,ip02,isize,2,ier(2))
      if (ier(2).lt.0) then
        nbfail=real(BYTES_PER_REAL*isize)
      else
        totlo=real(BYTES_PER_REAL*isize)
        totsize = totsize+totlo
      endif
      endif

      if((n_blk.gt. 2).and.(ier(2).eq.0)) then
         call mmgetblk(blk(3),prtnam,ip03,isize,2,ier(3))
      if (ier(3).lt.0) then
        nbfail=real(BYTES_PER_REAL*isize)
      else
        totlo=real(BYTES_PER_REAL*isize)
        totsize = totsize+totlo
      endif
      endif

      if((n_blk.gt. 3).and.(ier(3).eq.0)) then
         call mmgetblk(blk(4),prtnam,ip04,isize,2,ier(4))
      if (ier(4).lt.0) then
        nbfail=real(BYTES_PER_REAL*isize)
      else
        totlo=real(BYTES_PER_REAL*isize)
        totsize = totsize+totlo
      endif
      endif

      if((n_blk.gt. 4).and.(ier(4).eq.0)) then
         call mmgetblk(blk(5),prtnam,ip05,isize,2,ier(5))
      if (ier(5).lt.0) then
        nbfail=real(BYTES_PER_REAL*isize)
      else
        totlo=real(BYTES_PER_REAL*isize)
        totsize = totsize+totlo
      endif
      endif

      if((n_blk.gt. 5).and.(ier(5).eq.0)) then
         call mmgetblk(blk(6),prtnam,ip06,isize,2,ier(6))
      if (ier(6).lt.0) then
        nbfail=real(BYTES_PER_REAL*isize)
      else
        totlo=real(BYTES_PER_REAL*isize)
        totsize = totsize+totlo
      endif
      endif

      do i = 1, n_blk
         if(ier(i) .ne. 0)imax_err = 1
      enddo

c     finish up if imax_err has been reached
      if (imax_err .ne. 0) then

        do i = 1, n_blk
c         print *, 'array number = ',i, '  error flag = ', ier(i)
        if (ier(i).eq.0) then
           blk_cnt= blk_cnt+1 
        endif
        enddo
      endif

      call mmrelprt(prtnam,ier_prt)
      isize = isize * 2
      icount1 = icount1 + 1
      icount3 = icount3 + 1
      
      enddo

c     totsize is the number of bytes for this iteration
c     totlo are number of successful allocations
c     tothi is the number of bytes where failure occured 
      tothi = totsize+(nbytes*n_blk) 

      if (isize.gt.maxsize) then
      print*,"Test stopped at number larger than ",maxsize 
      endif

      print*," "
      print*, "Succeeded at  ",totlo/1000000.0," MEGABYTES"
      Print*, "Failed at     ",nbfail/1000000.0," MEGABYTES"
      print*," "

      return
      end

c dk,memory
