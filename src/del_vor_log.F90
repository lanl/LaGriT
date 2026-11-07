!-----------------------------------------------------------------------
! del_vor_log: all screen and log output goes through log_line, so
! the LaGriT build can send it to LaGriT's own log (writloga) and the
! standalone build to standard output. Lines are kept to 80 columns.
!-----------------------------------------------------------------------
module del_vor_log
   use, intrinsic :: iso_fortran_env, only: output_unit
   implicit none
   private
   public :: log_line, log_set_unit, errmsg

   integer, save :: lu = output_unit

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
   end subroutine log_line

end module del_vor_log
