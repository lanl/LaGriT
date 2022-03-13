C  Modified heavily for 64 bit version
C  changed malloc to util_malloc wrappers in mmsc.c 
C  which allows better capture of errors and warnings 
C  mm2000.h is compiled from mm2000_header.F for preprocess definitions
C  changed output to allow large numbers (bigger than int) for size reporting
C

!!
!!  Memory Manager 2000 (:-P)
!!
!!  Neil Carlson <Neil.Carlson@Motorola.com>
!!  Motorola Computational Materials Group @ LANL
!!  6 April 2000
!!
!!  This package is intended to be a drop-in replacement for the present
!!  memory management routines (mm* files) used by LaGriT.  It maintains
!!  the interface and aims to replicate the behavior of the previous
!!  routines, while reimplementing the internals and clearing away years
!!  of accumulated cruft.
!!
!!
!!  PROCEDURES
!!
!!  The following procedure arguments are used throughout.
!!
!!    character*(*) aname -- Name of the managed array
!!    character*(*) pname -- Partition name of the managed array
!!    pointer (aptr, array(*)) -- Cray-style pointer to managed array
!!    integer length -- Size of the managed array
!!    integer type -- Type of the managed array:
!!                    1 == integer
!!                    2 == real*8
!!                    3 == character*32
!!    integer status -- Return status variable.  STATUS = 0 indicates
!!       a successful operation.
!!     HISTORIC value of error status for mm routines:
!!     -1  null pointer return from util_malloc
!!     -6  util_malloc failure 
!!     -16 array does not exist
!!     -17 array already exists, exit 
!!     -21 invalid length or type request, exit
!!
!!  1. PUBLIC ALLOCATION PROCEDURES
!!
!!  MMGETBLK (ANAME, PNAME, APTR, LENGTH, TYPE, STATUS)
!!    Allocates a new managed array of type TYPE and size LENGTH.
!!    Input: ANAME, PNAME, LENGTH, TYPE
!!    Output: APTR, STATUS
!!
!!  MMNEWLEN (ANAME, PNAME, APTR, LENGTH, STATUS)
!!    Reallocates an existing managed array to size LENGTH.
!!    Input: ANAME, PNAME, LENGTH
!!    Output: APTR, STATUS
!!
!!  MMRELBLK (ANAME, PNAME, APTR, STATUS)
!!    Deallocates a managed array.
!!    Input: ANAME, PNAME
!!    Output: STATUS
!!    APTR is unused.
!!
!!  MMRELPRT (PNAME, STATUS)
!!    Deallocates all managed arrays in a partition.
!!    Input: PNAME; Output: STATUS
!!
!!  MMNAMPRT (NEWPNAME, OLDPNAME, STATUS)
!!    Changes the name of partition OLDPNAME to NEWPNAME.
!!    Input: NEWPNAME, OLDPNAME.  Output: STATUS.
!!
!!  MMINCBLK (ANAME, PNAME, APTR, INCR, STATUS)
!!    Reallocates an existing managed array incrementing its size
!!    by INCR.
!!    Input: ANAME, PNAME, INCR
!!    Output: APTR, STATUS
!!
!!  MMGGETBK (ANAME, PNAME, APTR, LENGTH, TYPE, STATUS)
!!    If the named array does not exist, allocate a new managed array
!!    of type TYPE and size LENGTH.  If the named array exists and
!!    LENGTH exceeds its current size, reallocate it with size LENGTH.
!!    Otherwise simply return the current array pointer.
!!    Input: ANAME, PNAME, LENGTH, TYPE
!!    Output: APTR, STATUS
!!
!!  MM_OVALL (ANAME, PNAME, APTR, NEED, EXTRA, LENGTH, TYPE, STATUS)
!!    If LENGTH < NEED and LENGTH = 0, a new managed array is allocated
!!    of type TYPE and size NEED + EXTRA.  IF LENGTH < NEED and
!!    LENGTH > 0, the existing named array is reallocated with size
!!    NEED + EXTRA.  In either case the new size is returned in LENGTH.
!!    For proper operation, APTR must point to the named array (if it
!!    exists) and LENGTH must be its size; LENGTH = 0 indicates the
!!    named array does not exist.
!!    Input: ANAME, PNAME, NEED, EXTRA, LENGTH, TYPE
!!    Output: APTR, LENGTH, STATUS
!!
!!  MMCMPRS (iholesize, status)
!!    This does absolutely nothing but return STATUS = 0.  The previous
!!    version compressed the array descriptor blocks, but in this new
!!    implementation this operation is moot.
!!
!!  2. PUBLIC INQUIRY PROCEDURES
!!
!!  MMBLKLEN (ANAME, PNAME, APTR, LENGTH, STATUS)
!!  MMFINDBK (ANAME, PNAME, APTR, LENGTH, STATUS)
!!    Returns a pointer to, and size of, the named managed array.
!!    Input: ANAME, PNAME.  Output: APTR, LENGTH
!!
!!  MMGETINDEX (ANAME, PNAME, INDEX, STATUS)
!!    Returns the index of the named managed array.  INDEX > 0 indicates
!!    the array exists, INDEX = 0 it does not.
!!    Input: ANAME, PNAME.  Output: INDEX, STATUS
!!
!!  MMGETLEN (APTR, LENGTH, STATUS)
!!    Returns the size of a managed array by address.
!!    Input: APTR.  Output: LENGTH, STATUS.
!!
!!  MMGETNAM (APTR, ANAME, PNAME, STATUS)
!!    Returns the name and partition of a managed array by address.
!!    Input: APTR.  Output: ANAME, PNAME, STATUS
!!
!!  MMGETPR (ANAME, PNAME, APTR, STATUS)
!!    Returns the pointer to a named managed array.
!!    Input: ANAME, PNAME.  Output: APTR, STATUS
!!
!!  MMGETTYP (APTR, TYPE, STATUS)
!!    Returns the type of a managed array by address.
!!    Input: APTR.  Output: TYPE, STATUS
!!
!!  3. DIAGNOSTIC PROCEDURES
!!
!!  MMVERIFY ()
!!    This aims to verify the integrity of the managed array storage
!!    by checking that the prolog and epilog blocks have not been
!!    overwritten.  The code will print out an array map and halt if
!!    it detects such memory corruption.
!!
!!  MMPRINT ()
!!    Print out an address map of the managed arrays.
!!
!!  4. PRIVATE PROCEDURES -- DO NOT USE
!!
!!  MM_NEW_AD (INDEX, STATUS)
!!    Return an index (in the AD array) for a new array descriptor.
!!
!!  MM_INDEX_BY_NAME (ANAME, PNAME, INDEX)
!!    Return the AD-array index of the named managed array.
!!
!!  MM_INDEX_BY_ADDR (APTR, INDEX)
!!    Return the AD-array index of the managed array by address.
!!
!!  MM_PANIC (PROC, MESG)
!!    Print error message and halt.
!!
!!  MM_WARN (PROC, MESG)
!!    Print warning message and return.
!!
!!
!!  CHANGE HISTORY
!!
!!  $Log: mm2000.f,v $
!!  Revision 2.00  2007/11/03 00:49:12  spchu
!!  Import to CVS
!!
CPVCS    
CPVCS       Rev 1.10   31 May 2007 14:40:28   gable
CPVCS    Modified error reporting to include partition, array and length.
CPVCS    
CPVCS       Rev 1.9   11 Aug 2005 09:15:32   dcg
CPVCS    change pointer addition to function call that adds
CPVCS    pointers as unsigned integers -- this avoids
CPVCS    problems with platforms where sign bit is part of
CPVCS    the address
CPVCS    
CPVCS       Rev 1.8   21 Mar 2003 15:29:10   gable
CPVCS    Added output at the end of mmprint to print out total
CPVCS    amount of memory associated with memory managed arrays.
CPVCS    
CPVCS       Rev 1.7   11 Mar 2003 07:54:04   gable
CPVCS    Changed format in mmprint to allow larger integers.
CPVCS    
CPVCS       Rev 1.6   14 Nov 2000 09:37:02   dcg
CPVCS    remove warning for block not found
CPVCS
CPVCS       Rev 1.5   12 Oct 2000 13:35:52   dcg
CPVCS    comment out warning from mmrelblk if array does not exist
CPVCS
CPVCS       Rev 1.4   24 Aug 2000 09:09:44   dcg
CPVCS    remove debug print statments
CPVCS
CPVCS       Rev 1.3   23 Aug 2000 14:16:08   dcg
CPVCS    change mmprint to print in increasing address order
CPVCS
CPVCS       Rev 1.0   23 Aug 2000 11:17:58   dcg
CPVCS    Initial revision.
CPVCS
CPVCS       Rev 1.2   Fri Apr 07 08:34:44 2000   dcg
CPVCS    rename blockdata
CPVCS    replace f90 syntax with f77 (specifically f77 does not recognize plain 'do')
CPVCS
CPVCS       Rev 1.1   Thu Apr 06 13:15:40 2000   nnc
CPVCS    Fixed typo in mm_ovall name.
CPVCS
CPVCS       Rev 1.0   Thu Apr 06 10:49:48 2000   nnc
CPVCS    Initial revision.
 
 
!!
!!  1. PUBLIC ALLOCATION PROCEDURES
!!
 
      subroutine mmgetblk (aname, pname, aptr, length, type, status)
 
      implicit none
 
      character*(*) aname, pname
      integer length, type, status, ierr, idone
 
      pointer (aptr,char_array)
      character*32 char_array(*)

      character*12 char_length
      character*12 char_type
 
      pointer (pptr,prolog), (eptr,epilog)
      integer prolog(*), epilog(*)
 
      include 'mm2000.h'
 
      integer array_bytes, block_bytes
      integer index, j, iptrsize

      real*8 rbytes

      integer icharlnf
 
      integer util_malloc, util_sum_addr, util_get_ptrsize
      external util_malloc, util_sum_addr, util_get_ptrsize

CCCCCCCCCCC
C begin mmgetblk
 
      char_length = '************'
      ierr = 0
      status = 0
      idone = 0

C debug
c     print*
c     print*,'Begin mmgetblk: array, partition, ptr, len, type, status:'
c     print*,'                ',aname(1:icharlnf(aname)),'  ',
c    & pname(1:icharlnf(pname)), aptr,length,type,status 

      call mmverify ()
 
      call mm_index_by_name (aname, pname, index)
      if (index .gt. 0) then  ! Array already exists
        status = -17
        call mm_error ('MMGETBLK',
     &    'Array ' // aname(1:icharlnf(aname)) // '   in ' // 
     &     pname(1:icharlnf(pname)) // '  already exists')
        goto 999
      end if

C     Cannot allocate memory with length less than 1
      if (length .lt. 1) then 
        if (length.lt.999999 .and. length.gt.-99999) then
           write(char_length,200)length
        else
           write(char_length,100)length
        endif
        status = -21
        call mm_error ('MMGETBLK', 'Invalid length request. Array: '
     1   // aname(1:icharlnf(aname)) // '    in partition: ' // 
     &      pname(1:icharlnf(pname)) // 
     2   ' with length: ' // char_length(1:icharlnf(char_length)) )
        goto 999
      end if
 
      if (type .lt. 1 .or. type .gt. 3) then ! Valid types are 1,2,3
        write(char_type, 100) type
        status = -21
        call mm_error ('MMGETBLK', 'Invalid type request. Array: '
     1   // aname(1:icharlnf(aname)) // '    in partition: ' // 
     &   pname(1:icharlnf(pname)) // 
     2   '   of type ' // char_type(1:icharlnf(char_type)) )
        goto 999
      end if
 100  format(i12)
 200  format(i6)
 
      !! Allocate memory for the array
      if (type .eq. 1) then
        array_bytes = BYTES_PER_INT * length
      elseif (type .eq. 2) then
        array_bytes = BYTES_PER_REAL * length
      elseif (type .eq. 3) then
        array_bytes = BYTES_PER_CHAR * length
      endif
      block_bytes = PROLOG_BYTES + array_bytes + EPILOG_BYTES

C     Check that prolog + length*byte size will be a valid number
C     can not try to allocate number larger than max unsigned int
C     malloc will allocate the nonsense number and seg fault will
C     occur later when the unallocated part of memory is used
      rbytes = real(PROLOG_BYTES) + real(EPILOG_BYTES)
      rbytes = rbytes + (real(BYTES_PER_CHAR)*real(length))

      if (rbytes .ge. RMAX_ALLOCATE ) then
        status = -21
        char_length = '*************'
        iptrsize = util_get_ptrsize()
        call mm_error ('MMGETBLK', 'Invalid length request. Array: '
     1   // aname(1:icharlnf(aname)) // '    in partition: ' // 
     &      pname(1:icharlnf(pname)) // 
     2   ' with length: ' // char_length(1:icharlnf(char_length)) )
        print*,'MMGETBLK ERROR: value exceeds sizeof: ',iptrsize
        print*,'MAX ALLOC SIZE: ',RMAX_ALLOCATE
        print*,'ATTEMPTED SIZE: ',rbytes
        goto 999

      endif 
 
C  debug
c     print*,'mm2000 -- util_malloc 1 called with bytes: ',block_bytes
      pptr = util_malloc(block_bytes)
c     print*,'malloc pptr: ',pptr
      if ( pptr .eq. 0) then
         status = -1
       write(*,*)'MMGETBLK FAILED: Array '//aname(1:icharlnf(aname))// 
     &   ' with bytes: ',block_bytes
         goto 998
      endif
      aptr =  util_sum_addr(pptr, PROLOG_BYTES)
      eptr =  util_sum_addr(aptr, array_bytes)

      !! Get new array descriptor and initialize
      call mm_new_ad (index, status)
      ad_name(1,index) = aname
      ad_name(2,index) = pname
      ad_addr(1,index) = pptr
      ad_addr(2,index) = aptr
      ad_addr(3,index) = eptr
      ad_addr(4,index) = type
      ad_addr(5,index) = length
      ad_addr(6,index) = block_bytes

      !! Initialize the array
      if (type .eq. 1) then
         call mm_init_int(aptr, length)
      else if (type .eq. 2) then
         call mm_init_real(aptr, length)
      else if (type .eq. 3) then
        do j = 1, length
          char_array(j) = ' '
        end do
      end if
 
      !! Install the prolog and epilog; arbitrary, but specified, data.
      do j = 1, AD_SIZE
        prolog(j) = ad_addr(j,index)
        epilog(j) = ad_addr(j,index)
      end do
 
C     Check for errors
998   idone = 1
999   if (status .ne. 0 .and. idone.eq.0) then
        write(*,*)'MMGETBLK: return early with error flag: ',status 
      endif
      if (status .ne. 0 .and. idone.eq.1) then
        write(*,*)'MMGETBLK: return ending with error flag: ',status 
      endif
      if (status .eq. 0 .and. idone.eq.0) then
        write(*,*)'MMGETBLK: return ending early with unflagged errors.' 
      endif
 
      return
      end
C     end mmgetblk
 
 
      subroutine mmnewlen (aname, pname, aptr, length, status)
 
      implicit none
 
      character*(*) aname, pname
      integer length, status
 
      pointer (aptr,char_array)
      character*32 char_array(*)
      character*12 char_length
 
      pointer (pptr,prolog), (eptr,epilog)
      integer prolog(*), epilog(*)
 
      include 'mm2000.h'
 
      integer j, orig_length, type, index
      integer array_bytes, block_bytes, iptrsize

      integer icharlnf

      real*8 rbytes

      integer util_realloc, util_sum_addr
      external util_realloc, util_sum_addr
      integer util_get_ptrsize
      external util_get_ptrsize

CCCCCCCC
C begin mmnewlen
 
      status = 0
 
      call mmverify ()
 
      call mm_index_by_name (aname, pname, index)
      if (index .eq. 0) then  ! Array doesn't exist
        status = -16
        call mm_error ('MMNEWLEN',
     &    'Array ' // aname // ' in ' // pname // 'does not exist')
      end if
 
      if (length .lt. 1) then ! Cannot allocate memory with lenght less than 1.
        write(char_length,100)length
        status = -21
        call mm_error ('MMNEWLEN', 'Invalid length request. Array: '
     1   // aname // ' in partition: ' // pname // 
     2   ' with length: ' // char_length )
      end if
 100  format(i12)
 
      pptr = ad_addr(1,index)
      aptr = ad_addr(2,index)
      type = ad_addr(4,index)
      orig_length = ad_addr(5,index)
 
      if (length .eq. orig_length) return
 
      !! Allocate memory for the array
      if (type .eq. 1) then
        array_bytes = BYTES_PER_INT * length
      elseif (type .eq. 2) then
        array_bytes = BYTES_PER_REAL * length
      elseif (type .eq. 3) then
        array_bytes = BYTES_PER_CHAR * length
      endif
      block_bytes = PROLOG_BYTES + array_bytes + EPILOG_BYTES

C     Check that prolog + length*byte size will be a valid number
C     can not try to allocate number larger than max unsigned int
C     malloc will allocate the nonsense number and seg fault will
C     occur later when the unallocated part of memory is used
      rbytes = real(PROLOG_BYTES) + real(EPILOG_BYTES)
      rbytes = rbytes + (real(BYTES_PER_CHAR * length))

      if (rbytes .ge. RMAX_ALLOCATE ) then
        status = -21
        char_length = '*************'
        iptrsize = util_get_ptrsize()
        call mm_error ('MMNEWLEN', 'Invalid length request. Array: '
     1   // aname(1:icharlnf(aname)) // '    in partition: ' //
     &      pname(1:icharlnf(pname)) //
     2   ' with length: ' // char_length(1:icharlnf(char_length)) )
        print*,'MMNEWLEN ERROR: value exceeds sizeof: ',iptrsize
        print*,'MAX ALLOC SIZE: ',RMAX_ALLOCATE
        print*,'ATTEMPTED SIZE: ',rbytes
      endif

 
C  debug
c     print*,'mm2000 --- util_realloc: called with bytes: ',block_bytes
      pptr = util_realloc(pptr, block_bytes)
      aptr = util_sum_addr(pptr, PROLOG_BYTES)
      eptr = util_sum_addr(aptr, array_bytes)

      if ( pptr .eq. 0) then
         status = -1
          write(*,*)'MMNEWLEN FAILED: Array ' // aname // 
     *     ' with bytes: ',block_bytes
         return
      endif
 
      !! Reinitialize array descriptor
      ad_addr(1,index) = pptr
      ad_addr(2,index) = aptr
      ad_addr(3,index) = eptr
      ad_addr(4,index) = type
      ad_addr(5,index) = length
      ad_addr(6,index) = block_bytes
 
      !! Initialize new part of array
      if (length .gt. orig_length) then
         if (type .eq. 1) then
            call mm_init_int2(aptr, length, orig_length)
         else if (type .eq. 2) then
            call mm_init_real2(aptr, length, orig_length)
         else if (type .eq. 3) then
            do j = orig_length + 1, length
               char_array(j) = ' '
            end do
         end if
      end if
 
      !! Reinstall the prolog and epilog
      do j = 1, AD_SIZE
        prolog(j) = ad_addr(j,index)
        epilog(j) = ad_addr(j,index)
      end do
 
      status = 0
 
      return
      end
 
 
      subroutine mmrelblk (aname, pname, aptr, status)
 
      implicit none
 
      character*(*) aname, pname
      integer status
 
      pointer (aptr, array)
      integer array(*)
 
      include 'mm2000.h'
 
      pointer (pptr, block)
      integer block(*)
 
      integer prev, index, j, util_free ,i
 
      external util_free
 
      call mmverify ()
 
      prev = 0
      index = first
      do i=1,100000000
        if (index .lt. 1) then
          status = -16
 
!         call mm_warn ('MMRELBLK',
!    &      'Array ' // aname // ' in ' // pname // ' does not exist')
          return
        end if
        if (aname .eq. ad_name(1,index) .and.
     &      pname .eq. ad_name(2,index)) then
 
          !! DEALLOCATE THE MEMORY
          pptr = ad_addr(1,index)
          status = util_free(pptr)
 
          !! CLEAR THE ARRAY DESCRIPTOR
          ad_name(1,index) = ' '
          ad_name(2,index) = ' '
          do j = 1, AD_SIZE
            ad_addr(j,index) = 0
          end do
 
          !! MOVE THE ARRAY DESCRIPTOR TO THE FREE LIST
          if (prev .gt. 0) then
            link(prev) = link(index)
            link(index) = next
            next = index
          else
            first = link(index)
            link(index) = next
            next = index
          end if
          num_ad = num_ad - 1
          status = 0
          return
        end if
        prev = index
        index = link(index)
      end do
 
      return
      end
 
 
      subroutine mmrelprt (pname, status)
 
      implicit none
 
      character*(*) pname
      integer status
 
      include 'mm2000.h'
 
      pointer (pptr, block)
      integer block(*)
 
      integer prev, index, j, util_free ,i
 
      external util_free
 
      call mmverify ()
 
      prev = 0
      index = first
      status = -16
      do i=1,100000000
        if (index .lt. 1) then
          if (status .ne. 0) then
!            call mm_warn ('MMRELPRT',
!     &                    'Partition '// pname // ' does not exist')
          end if
          return
        end if
        if (pname .eq. ad_name(2,index)) then
 
          !! DEALLOCATE THE MEMORY
          pptr = ad_addr(1,index)
          status = util_free(pptr)
 
          !! CLEAR THE ARRAY DESCRIPTOR
          ad_name(1,index) = ' '
          ad_name(2,index) = ' '
          do j = 1, AD_SIZE
            ad_addr(j,index) = 0
          end do
 
          !! MOVE THE ARRAY DESCRIPTOR TO THE FREE LIST
          if (prev .gt. 0) then
            link(prev) = link(index)
            link(index) = next
            next = index
            index = link(prev)
          else
            first = link(index)
            link(index) = next
            next = index
            index = first
          end if
          num_ad = num_ad - 1
          status = 0
        else
          prev = index
          index = link(index)
        end if
      end do
 
      return
      end
 
 
      subroutine mmnamprt (newpname, oldpname, status)
 
      implicit none
 
      character*(*) newpname, oldpname
      integer status
 
      include 'mm2000.h'
 
      integer j ,i
 
      j = first
      status = -16
      do i=1,100000000
        if (j .lt. 1) return
        if (oldpname .eq. ad_name(2,j)) then
          ad_name(2,j) = newpname
          status = 0
        end if
        j = link(j)
      end do
 
      return
      end
 
 
      subroutine mmincblk (aname, pname, aptr, incr, status)
 
      implicit none
 
      character*(*) aname, pname
      integer incr, status
 
      pointer (aptr, array)
      integer array(*)
 
      integer length
 
      call mmfindbk (aname, pname, aptr, length, status)
      if (status .eq. 0) then
        length = length + incr
        call mmnewlen(aname, pname, aptr, length, status)
      end if
 
      return
      end
 
 
      subroutine mmggetbk (aname, pname, aptr, length, type, status)
 
      implicit none
 
      character*(*) aname, pname
      integer length, type, status
 
      pointer (aptr, array)
      integer array(*)
 
      integer len
 
      call mmblklen (aname, pname, aptr, len, status)
      if (status .ne. 0) then
        call mmgetblk (aname, pname, aptr, length, type, status)
      else if (length .gt. len) then
        call mmnewlen (aname, pname, aptr, length, status)
      else
        status = 0
      end if
 
      return
      end
 
 
      subroutine mm_ovall (aname, pname, aptr, need, extra, length,
     &                     type, status)
 
      implicit none
 
      character*(*) aname, pname
      integer need, extra, length, type, status
 
      pointer (aptr, array)
      integer array(*)
 
      if (need .gt. length) then
        if (length .eq. 0) then
          length = need + extra
          call mmgetblk (aname, pname, aptr, length, type, status)
        else
          length = need + extra
          call mmnewlen (aname, pname, aptr, length, status)
        end if
      end if
 
      return
      end
 
 
      subroutine mmcomprs (iholesize, status)
 
      implicit none
 
      integer iholesize, status
 
      status = 0
 
      return
      end
 
!!
!!  2. PUBLIC INQUIRY PROCEDURES
!!
 
      subroutine mmblklen (aname, pname, aptr, length, status)
 
      implicit none
 
      character*(*) aname, pname
      integer length, status
 
      pointer (aptr, array)
      integer array(*)
 
      call mmfindbk (aname, pname, aptr, length, status)
 
      return
      end
 
 
      subroutine mmfindbk (aname, pname, aptr, length, status)
 
      implicit none
 
      character*(*) aname, pname
      integer length, status
 
      pointer (aptr, array)
      integer array(*)
 
      include 'mm2000.h'
 
      integer index
 
      call mm_index_by_name (aname, pname, index)
      if (index .gt. 0) then
        aptr   = ad_addr(2,index)
        length = ad_addr(5,index)
        status = 0
      else
        aptr = 0
        length = 0
        status = -16
      end if
 
      return
      end
 
 
      subroutine mmgetindex (aname, pname, index, status)
 
      implicit none
 
      character*(*) aname, pname
      integer index, status
 
      call mm_index_by_name (aname, pname, index)
 
      if (index .gt. 0) then
        status = 0
      else
        status = -16
      end if
 
      return
      end
 
 
      subroutine mmgetlen (aptr, length, status)
 
      implicit none
 
      integer length, status
 
      pointer (aptr, array)
      integer array(*)
 
      include 'mm2000.h'
 
      integer index
 
      call mm_index_by_addr (aptr, index)
      if (index .gt. 0) then
        length = ad_addr(5,index)
        status = 0
      else
        status = -16
      end if
 
      return
      end
 
 
      subroutine mmgetnam (aptr, aname, pname, status)
 
      implicit none
 
      character*(*) aname, pname
      integer status
 
      pointer (aptr, array)
      integer array(*)
 
      include 'mm2000.h'
 
      integer index
 
      call mm_index_by_addr (aptr, index)
      if (index .gt. 0) then
        aname = ad_name(1,index)
        pname = ad_name(2,index)
        status = 0
      else
        aname = '$notset'
        pname = '$notset'
        status = -16
      end if
 
      return
      end
 
 
      subroutine mmgetpr (aname, pname, aptr, status)
 
      implicit none
 
      character*(*) aname, pname
      integer status
 
      pointer (aptr, array)
      integer array(*)
 
      include 'mm2000.h'
 
      integer index
 
      call mm_index_by_name (aname, pname, index)
      if (index .gt. 0) then
        aptr   = ad_addr(2,index)
        status = 0
      else
        aptr = 0
        status = -16
      end if
 
      return
      end
 
 
      subroutine mmgettyp (aptr, type, status)
 
      implicit none
 
      integer type, status
 
      pointer (aptr, array)
      integer array(*)
 
      include 'mm2000.h'
 
      integer index
 
      call mm_index_by_addr (aptr, index)
      if (index .gt. 0) then
        type   = ad_addr(4,index)
        status = 0
      else
        type = 0
        status = -16
      end if
 
      return
      end
 
!!
!!  3. PUBLIC DIAGNOSTIC PROCEDURES
!!
 
      subroutine mmverify ()
 
      implicit none
 
      include 'mm2000.h'
 
      pointer (pptr, prolog), (eptr, epilog)
      integer prolog(*), epilog(*)
 
      pointer (aptr, array)
      integer array(*)
 
      integer index, j, length, ics
      character*32 aname, pname
      logical corrupted, stomped_on

      integer util_get_ptrsize
      external util_get_ptrsize
      integer util_check_ptrsize
      external util_check_ptrsize
      integer util_print_sizes
      external util_print_sizes
      integer gmv_print_sizes
      external gmv_print_sizes

 
      index = first
      corrupted = .false.

      !! CHECK compiled pointer size
      ics = util_check_ptrsize()
      if (ics .ne. 0) then
       print *, 'MMVERIFY: Compile error: '
       print *, "sizeof ptr and int_ptrsize differ: ",ics
       ics=util_print_sizes()
       ics=gmv_print_sizes()
      endif
 
 
      do while (index .gt. 0)
 
        aname = ad_name(1,index)
        pname = ad_name(2,index)
        pptr  = ad_addr(1,index)
        aptr  = ad_addr(2,index)
        eptr  = ad_addr(3,index)
        length= ad_addr(5,index)
 
        !! CHECK THE PROLOG
        stomped_on = .false.
        do j = 1, AD_SIZE
          if (prolog(j) .ne. ad_addr(j,index)) stomped_on = .true.
        end do
        if (stomped_on) then
          corrupted = .true.
          print *, 'MMVERIFY: Array prolog overwritten'
          print *, '  Array ', aname, ' in ', pname
          print *, '  Length=', length, ', Address=', aptr
        end if
 
        !! CHECK THE EPILOG
        stomped_on = .false.
        do j = 1, AD_SIZE
          if (epilog(j) .ne. ad_addr(j,index)) stomped_on = .true.
        end do
        if (stomped_on) then
          corrupted = .true.
          print *, 'MMVERIFY: Array epilog overwritten'
          print *, '  Array ', aname, ' in ', pname
          print *, '  Length=', length, ', Address=', aptr
        end if
 
        index = link(index)
      end do
 
      if (corrupted) then
        call mmprint ()
        call mm_panic ('MMVERIFY', 'Array storage was corrupted')
      end if
 
      return
      end
 
CCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCC 
      subroutine mmprint ()
 
      implicit none
 
      include 'mm2000.h'
 
      integer j,icount,k
      pointer(tempadrptr,tempadr),(temppermptr,tempperm)
      integer tempadr(2,*),tempperm(*)
      integer util_free
      integer util_malloc
      integer util_get_ptrsize
      integer util_check_ptrsize
      integer util_print_sizes
      external util_malloc,util_free
      external util_check_ptrsize,util_print_sizes
      external util_get_ptrsize

      integer isum, itmp, ierr
      real*8 xsum

!!  check for correct compile
!!  ptr for malloc and pointer size must be equal
!!  32 bit is 4, 64 bit is 8

      write(*,'(a)')'--------'
      ierr = util_print_sizes()
      ierr = util_check_ptrsize()
      if (ierr .ne. 0) then
        write(*,*)
        write(*,*)'MMPRINT: COMPILE ERROR!'
        write(*,*)'pointer size and malloc pointer size differ.'
        write(*,*)
        return
      endif
!!
!!  get number of entries
!!
      j = first
      icount=0
      do while (j .gt. 0)
        icount=icount+1
        j = link(j)
      end do
!!
!! get space for sort
!!
      if (icount .le. 0) goto 999
      tempadrptr = util_malloc(BYTES_PER_INT*2*icount)
      temppermptr = util_malloc(BYTES_PER_INT*icount)

C ADDED
      if ( tempadrptr.eq.0 .or. temppermptr.eq.0) then
          write(*,*)'MMPRINT: sort temp Array NOT ENOUGH MEMORY!'
          write(*,*)'Could not allocate bytes: ',BYTES_PER_INT*icount
          return
      endif

!!
!! move addresses into temp array and get permutation thereof
!!
      j = first
      do k=1,icount
        tempadr(1,k)=ad_addr(2,j)
        tempadr(2,k)= j
        j = link(j)
      end do
 
      j=1
      k=2
      call hpsortim(icount,j,k,tempperm,tempadr)
      
C     Totals over sizeof int give garbage values
C     Use real to report large numbers
      isum = 0
      itmp = 0
      xsum = 0.
      do k=1,icount
      if    (ad_addr(4,k) .eq. 1) then
         itmp = (ad_addr(5,k)*BYTES_PER_INT)
         xsum = xsum + (ad_addr(5,k)*BYTES_PER_INT)
      elseif(ad_addr(4,k) .eq. 2) then
         itmp = (ad_addr(5,k)*BYTES_PER_REAL)
         xsum = xsum + (ad_addr(5,k)*BYTES_PER_REAL)
      elseif(ad_addr(4,k) .eq. 3) then
         itmp = (ad_addr(5,k)*BYTES_PER_CHAR)
         xsum = xsum + (ad_addr(5,k)*BYTES_PER_CHAR)
      endif

C     now add values and protect against int size restrictions 
C     WARNING range size of signed long integer is +- 2147483647
      if (BYTES_PER_INT .lt. 8 ) then
         if (isum.gt.0.and.((isum+itmp).gt.2147483000)) then
            isum=-99
         else
            isum=isum+itmp
         endif
      else
         isum=isum+itmp
      endif 

      enddo
      
      write(*,8010)
      do k=1,icount
        j=tempadr(2,k)
        write(*,8020) j, ad_addr(5,j), ad_addr(4,j), ad_addr(2,j),
     &                   ad_name(1,j), ad_name(2,j)
        j = link(j)
      end do
     
      if (BYTES_PER_INT .lt. 8) then
         if (isum .gt. 0) then
           write(*,8031) isum, xsum/1000000.0
         else
           write(*,8035) xsum, xsum/1000000.0
         endif
      else
         if (isum .gt. 0) then
           write(*,8041) isum, xsum/1000000.0
         else
           write(*,8035) xsum, xsum/1000000.0
         endif
      endif

      k = util_free(tempadrptr)
      k = util_free(temppermptr)
 
 8010 format(/,'INDEX',9x,'LENGTH',4x,'TYPE',5x,'ADDRESS',5x,
     &       'NAME',27x,'PARTITION')
 8020 format(i4,2x,i16,2x,i2,i16,1x,a,1x,a)

C avoid erroneous totals by using g or en format
c note - older compilers do not like en format
 8031 format(/,'Total BYTES = ',i12,'   Total MEGABYTES = ',en12.3)
 8035 format(/,'Total BYTES = ',en12.3,'   Total MEGABYTES = ',en12.3)
 8041 format(/,'Total BYTES = ',i20,'   Total MEGABYTES = ',en12.3)

999   if (icount .le. 0) then
       write(*,'(a)')' '
       write(*,'(a)')'PARTITION LIST IS EMPTY'
       write(*,'(a)')'Total BYTES =  0        Total MEGABYTES = 0.000'
       write(*,'(a)')' '
      endif 
      write(*,'(a)')'--------'
      write(*,'(a)')' '
 
      return
      end

!! end mmprint routine    
 
!!
!!  4. PRIVATE PROCEDURES -- DO NOT USE EXTERNALLY
!!
 
      subroutine mm_new_ad (index, status)
 
      implicit none
 
      integer index, status
 
      include 'mm2000.h'
 
      integer max_ad_orig, i, j, idone
      integer util_malloc, util_realloc
      integer NUM_AD_START, NUM_AD_INCR
      parameter (NUM_AD_START = 500, NUM_AD_INCR = 200)
      external util_malloc, util_realloc
 
      idone = 0
      if (next .eq. 0) then ! No free AD; allocate more.
 
        if (max_ad .gt. 0) then
 
          max_ad_orig = max_ad
          max_ad = max_ad + NUM_AD_INCR
          adnptr = util_realloc (adnptr, max_ad*2*BYTES_PER_CHAR)
          adaptr = util_realloc (adaptr, max_ad*AD_SIZE*BYTES_PER_PTR)
          lptr   = util_realloc (lptr,   max_ad*BYTES_PER_INT)
 
        else
 
          max_ad_orig = 0
          max_ad = NUM_AD_START
          adnptr = util_malloc (max_ad*2*BYTES_PER_CHAR)
          adaptr = util_malloc (max_ad*AD_SIZE*BYTES_PER_PTR)
          lptr   = util_malloc (max_ad*BYTES_PER_INT)

        end if
 
        if (adnptr.lt.1) then
          status = -6
          call mm_error('MM_NEW_AD',
     &                  'Unable to allocate array adnptr')
          goto 998
        endif
        if (adaptr.lt.1) then
          status = -6
          call mm_error('MM_NEW_AD',
     &                  'Unable to allocate array adaptr')
          goto 998
        endif
        if (lptr.lt.1) then
          status = -6
          call mm_error('MM_NEW_AD',
     &                  'Unable to allocate array lptr')
          goto 998
        endif

 
        !! Put new AD onto free list
        next = max_ad_orig + 1
        do j = max_ad_orig + 1, max_ad - 1
          link(j) = j + 1
        end do
        link(max_ad) = 0
 
        !! Set new AD to null values
        do j = max_ad_orig + 1, max_ad
          ad_name(1,j) = ' '
          ad_name(2,j) = ' '
          do i = 1, AD_SIZE
            ad_addr(i,j) = 0
          end do
        end do
 
      end if
 
      !! Move AD from free list onto active list
      index = next
      next = link(index)
      link(index) = first
      first = index
      num_ad = num_ad + 1
 
C     Check for errors
998   idone = 1
999   if (status .ne. 0 .and. idone.eq.0) then
        write(*,*)'MM_NEW_AD: return early with error flag: ',status
      endif
      if (status .ne. 0 .and. idone.eq.1) then
        write(*,*)'MM_NEW_AD: return ending with error flag: ',status
      endif
      if (status .eq. 0 .and. idone.eq.0) then
       write(*,*)'MM_NEW_AD: return ending early with unflagged errors.'
      endif
 
      return
      end
C     end mm_new_ad

 
      subroutine mm_index_by_name (aname, pname, index)
 
      implicit none
 
      character*(*) aname, pname
      integer index ,i
 
      include 'mm2000.h'
 
      index = first
      do i=1,100000000
        if (index .lt. 1) then
          index = 0
          return
        end if
        if (aname .eq. ad_name(1,index) .and.
     &      pname .eq. ad_name(2,index)) return
        index = link(index)
      end do
 
      return
      end
 
 
      subroutine mm_index_by_addr (aptr, index)
 
      implicit none
 
      integer index ,i
 
      pointer (aptr, array)
      integer array(*)
 
      include 'mm2000.h'
 
      index = first
      do i=1,100000000
        if (index .lt. 1) then
          index = 0
          return
        end if
        if (aptr .eq. ad_addr(2,index)) return
        index = link(index)
      end do
 
      return
      end
 
 
      subroutine mm_panic (proc, mesg)
      character*(*) proc, mesg
      write(*,'(3a)') proc, ': PANIC! FATAL ERROR  ', mesg
      call mmprint()
      write(*,'(a)') 'PROGRAM STOP'
      stop
      end

      subroutine mm_error (proc, mesg)
      character*(*) proc, mesg
      write(*,'(3a)') proc, ': ERROR! FATAL ', mesg
      call mmprint()
      write(*,'(a)') 'PROGRAM CONTINUE '
      return
      end
 
      subroutine mm_warn (proc, mesg)
      character*(*) proc, mesg
      write(*,'(3a)') proc, ': Warning! ', mesg
      return
      end

      subroutine mm_init_int(aptr, length)

      implicit none
      integer length, j
      pointer (aptr,int_array)
      integer int_array(*)
      do j = 1, length
         int_array(j) = 0
      end do
      return
      end
      
      subroutine mm_init_int2(aptr, length, orig_length)

      implicit none
      integer length, orig_length, j
      pointer (aptr,int_array)
      integer int_array(*)
      do j = orig_length + 1, length
         int_array(j) = 0
      end do
      return
      end
      
      subroutine mm_init_real(aptr, length)

      implicit none
      integer length, j
      pointer (aptr, real_array)
      real*8 real_array(*)
      do j = 1, length
         real_array(j) = 0.0
      end do
      return
      end

      subroutine mm_init_real2(aptr, length, orig_length)

      implicit none
      integer length, orig_length, j
      pointer (aptr, real_array)
      real*8 real_array(*)
      do j = orig_length + 1, length
         real_array(j) = 0.0
      end do
      return
      end
