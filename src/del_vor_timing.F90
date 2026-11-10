!-----------------------------------------------------------------------
! del_vor_timing: wall and CPU time of each step, and peak memory, for
! development (keyword timing on; off by default, so a production run
! prints nothing more and costs nothing more than two clock reads per
! step).
!
!   call timer_start('read')   ... work ...   call timer_stop()
!   call timer_report()        the block at the end of the run
!
! Wall time from system_clock (64-bit counts), CPU time from cpu_time
! (the whole process). Peak memory: getrusage's ru_maxrss, the
! largest resident set so far (kilobytes on Linux, bytes on macOS:
! CMakeLists.txt defines DEL_VOR_MAXRSS_BYTES on Apple).
! The report lines start with "timing" and keep one layout, so
! test_timing can read them:
!   timing  STEP                 wall   0.123456 s  cpu   0.123000 s
!   timing  peak memory       1234.5 MB
!-----------------------------------------------------------------------
module del_vor_timing
   use, intrinsic :: iso_fortran_env, only: int64, real64
   use, intrinsic :: iso_c_binding, only: c_int, c_long
   use del_vor_log, only: log_line
   implicit none
   private
   public :: timer_start, timer_stop, timer_report, timing_on

   logical, save :: timing_on = .false.
   integer, parameter :: maxstep = 32
   integer, save :: nstep = 0
   character(len=20), save :: sname(maxstep)
   real(real64), save :: swall(maxstep), scpu(maxstep)
   integer(int64), save :: c0 = 0, ctot0 = -1
   real(real64), save :: p0 = 0.0_real64, ptot0 = 0.0_real64
   integer, save :: icur = 0

   interface
      function c_getrusage(who, ru) bind(C, name='getrusage')
         import :: c_int, c_long
         integer(c_int), value :: who
         integer(c_long) :: ru(18)
         integer(c_int) :: c_getrusage
      end function c_getrusage
   end interface

contains

   subroutine timer_start(name)
      character(len=*), intent(in) :: name
      integer :: k
      if (.not. timing_on) return
      if (ctot0 < 0) then
         call system_clock(ctot0)
         call cpu_time(ptot0)
      end if
      icur = 0
      do k = 1, nstep
         if (sname(k) == name) icur = k
      end do
      if (icur == 0 .and. nstep < maxstep) then
         nstep = nstep + 1
         icur = nstep
         sname(icur) = name
         swall(icur) = 0.0_real64
         scpu(icur) = 0.0_real64
      end if
      call system_clock(c0)
      call cpu_time(p0)
   end subroutine timer_start

   subroutine timer_stop()
      integer(int64) :: c1, rate
      real(real64) :: p1
      if (.not. timing_on .or. icur == 0) return
      call system_clock(c1, rate)
      call cpu_time(p1)
      swall(icur) = swall(icur) + real(c1 - c0, real64)/real(rate, &
                                                            real64)
      scpu(icur) = scpu(icur) + (p1 - p0)
      icur = 0
   end subroutine timer_stop

   subroutine timer_report()
      character(len=80) :: msg
      character(len=20) :: lab
      integer(int64) :: c1, rate
      real(real64) :: p1, mb
      integer :: k
      if (.not. timing_on) return
      call system_clock(c1, rate)
      call cpu_time(p1)
      call log_line(' ')
      call log_line('del_vor timing (timing on)')
      do k = 1, nstep
         write (msg, '(a,a20,a,f11.6,a,f11.6,a)') 'timing  ', &
            sname(k), ' wall', swall(k), ' s  cpu', scpu(k), ' s'
         call log_line(msg)
      end do
      lab = 'total'
      write (msg, '(a,a20,a,f11.6,a,f11.6,a)') 'timing  ', lab, &
         ' wall', real(c1 - ctot0, real64)/real(rate, real64), &
         ' s  cpu', p1 - ptot0, ' s'
      call log_line(msg)
      mb = peak_mb()
      if (mb >= 0.0_real64) then
         lab = 'peak memory'
         write (msg, '(a,a20,f11.1,a)') 'timing  ', lab, mb, ' MB'
         call log_line(msg)
      end if
   end subroutine timer_report

   ! Largest resident set of this process so far, in MB (10^6 bytes);
   ! -1 if getrusage fails.
   real(real64) function peak_mb()
      integer(c_long) :: ru(18)
      ru = 0
      if (c_getrusage(0_c_int, ru) /= 0) then
         peak_mb = -1.0_real64
         return
      end if
      ! struct rusage: two struct timeval (16 bytes each), then
      ! ru_maxrss
#if defined(DEL_VOR_MAXRSS_BYTES) || defined(__APPLE__)
      peak_mb = real(ru(5), real64)/1.0e6_real64
#else
      peak_mb = real(ru(5), real64)*1024.0_real64/1.0e6_real64
#endif
   end function peak_mb

end module del_vor_timing
