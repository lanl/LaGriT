      module interface_lagrit
        interface

          subroutine initlagrit(mode, log_file, batch_file)
     &    bind(C, name="initlagrit")
            use, intrinsic :: iso_c_binding, only: c_char,
     &      c_null_char, c_size_t
            character(kind=c_char), dimension(*), intent(in) ::
     &      mode, log_file, batch_file
          end subroutine

          subroutine cmo_get_name(cmo_name, ierror)
     &    bind(C, name="cmo_get_name")
            use, intrinsic :: iso_c_binding, only: c_char,
     &      c_null_char, c_size_t, c_int
            character(kind=c_char), dimension(*), intent(out) ::
     &      cmo_name
            integer, intent(out) :: ierror
          end subroutine

          subroutine dotask(task_buff, ierror)
     &    bind(C, name="dotask")
            use, intrinsic :: iso_c_binding, only: c_char,
     &      c_null_char, c_size_t, c_int
            character(kind=c_char), dimension(*), intent(in) ::
     &      task_buff
            integer, intent(out) :: ierror
          end subroutine

!          subroutine cmo_get_info(ioption,cmo_name,ipout,
!     &    lout,itype,ierror_return)
!     &    bind(C, name="cmo_get_info")
!            use, intrinsic :: iso_c_binding, only: c_char,
!     &      c_null_char, c_size_t, c_int, c_ptr, C_DOUBLE
!            character(kind=c_char), dimension(*), intent(in) ::
!     &      ioption, cmo_name
!
!C            type(c_ptr), intent(inout) :: ipout
!            real(C_DOUBLE), pointer :: ipout
!            integer(c_int), intent(inout) :: lout, itype, ierror_return
!          end subroutine
        end interface
      end module
