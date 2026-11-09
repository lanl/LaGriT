!-----------------------------------------------------------------------
! del_vor_log: all screen and log output goes through log_line, so
! the LaGriT build can send it to LaGriT's own log (writloga) and the
! standalone build to standard output. Lines are kept to 80 columns.
! log_copy_open also copies every later line to a file (the report
! keyword).
!-----------------------------------------------------------------------
module del_vor_log
   use, intrinsic :: iso_fortran_env, only: output_unit
   implicit none
   private
   public :: log_line, log_set_unit, errmsg, log_copy_open, &
             log_copy_close

   integer, save :: lu = output_unit
   integer, save :: lcopy = 0
   logical, save :: copying = .false.

   ! Text of the last error, set by core routines that return ierr.
   character(len=256), save :: errmsg = ' '

contains

   subroutine log_set_unit(iunit)
      integer, intent(in) :: iunit
      lu = iunit
   end subroutine log_set_unit

   subroutine log_line(msg)
      character(len=*), intent(in) :: msg
      write (lu, '(a)') trim(msg)
      flush (lu)
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
