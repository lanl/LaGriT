module lg
    use iso_c_binding
    interface
        subroutine initlagrit(mode,log_file,batch_file) bind(C, name="initlagrit")
            character(len=*), intent(in) :: mode, log_file, batch_file
        end subroutine
    end interface
end module