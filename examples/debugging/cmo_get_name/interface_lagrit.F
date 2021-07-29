      module interface_lagrit
        interface

          subroutine cmo_get_name(cmo_name, ierror)
     &    bind(C, name="cmo_get_name")
            use, intrinsic :: iso_c_binding, only: c_char,
     &      c_null_char, c_size_t, c_int
            character(kind=c_char), dimension(*), intent(out) ::
     &      cmo_name
            integer, intent(out) :: ierror
          end subroutine

        end interface
      end module
