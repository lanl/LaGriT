module lg
    use iso_c_binding
    interface
        subroutine initlagrit(mode, log_file, batch_file) bind(C, name="initlagrit")
            use, intrinsic :: iso_c_binding, only: c_char, c_null_char
            character(kind=c_char), dimension(*), intent(in) :: mode, log_file, batch_file
          end subroutine
    end interface
end module