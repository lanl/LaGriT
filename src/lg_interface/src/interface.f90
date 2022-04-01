module lg_interface
    use iso_c_binding

    interface
        !subroutine initlagrit(mode, log_file, batch_file) bind(C, name="initlagrit")
        !    use, intrinsic :: iso_c_binding, only: c_char, c_null_char
        !    character(kind=c_char), dimension(*), intent(in) :: mode, log_file, batch_file
        !end subroutine

        subroutine mytest() bind(C, name="mytest")
            use iso_c_binding
        end subroutine

        !subroutine inside_tet(x1,y1,z1,x2,y2,z2,x3,y3,z3,x4,y4,z4,xa,ya,za,iflag) bind(C, name="inside_tet")
        !    use iso_c_binding
        !    implicit none
        !    real(c_double), dimension(*), intent(in) :: x1, y1, z1, x2, y2, z2, x3, y3, z3, x4, y4, z4, xa, ya, za
        !    integer(c_int), intent(out) :: iflag
        !end subroutine
    end interface
end module