!-----------------------------------------------------------------------
! del_vor_log: all screen and log output goes through log_line, so
! the LaGriT build can send it to LaGriT's own log (writloga) and the
! standalone build to standard output. Lines are kept to 80 columns.
! log_set_sink(p) sends every line to the subroutine p instead of the
! screen (LaGriT: a routine calling writloga); log_set_sink() with no
! argument goes back to the screen.
! log_copy_open also copies every later line to a file (the report
! keyword).
!-----------------------------------------------------------------------
module del_vor_log
   use, intrinsic :: iso_fortran_env, only: output_unit
   implicit none
   private
   public :: log_line, log_set_unit, errmsg, log_copy_open, &
             log_copy_close, log_set_sink, log_sink

   abstract interface
      subroutine log_sink(msg)
         character(len=*), intent(in) :: msg
      end subroutine log_sink
   end interface

   integer, save :: lu = output_unit
   integer, save :: lcopy = 0
   logical, save :: copying = .false.
   procedure(log_sink), pointer, save :: sink => null()

   ! Text of the last error, set by core routines that return ierr.
   character(len=256), save :: errmsg = ' '

contains

   subroutine log_set_unit(iunit)
      integer, intent(in) :: iunit
      lu = iunit
   end subroutine log_set_unit

   subroutine log_set_sink(p)
      procedure(log_sink), optional :: p
      if (present(p)) then
         sink => p
      else
         sink => null()
      end if
   end subroutine log_set_sink

   subroutine log_line(msg)
      character(len=*), intent(in) :: msg
      if (associated(sink)) then
         call sink(trim(msg))
      else
         write (lu, '(a)') trim(msg)
         flush (lu)
      end if
      if (copying) write (lcopy, '(a)') trim(msg)
   end subroutine log_line

   subroutine log_copy_open(fname, ierr)
      character(len=*), intent(in) :: fname
      integer, intent(out) :: ierr
      open (newunit=lcopy, file=fname, status='replace', &
            action='write', iostat=ierr)
      copying = ierr == 0
      if (ierr /= 0) errmsg = 'del_vor: cannot open report file '// &
                              trim(fname)
   end subroutine log_copy_open

   subroutine log_copy_close()
      if (copying) close (lcopy)
      copying = .false.
   end subroutine log_copy_close

end module del_vor_log
