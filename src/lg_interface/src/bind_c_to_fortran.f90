module lg_interface
    use iso_c_binding
    implicit none

    interface
        subroutine computeentry(i, j, num_tets, tets, edges) bind(C)
            integer :: i, j, num_tets, tets, edges
        end subroutine

        !function entryprocessed(i, j) bind(C) result(r)
        !    integer :: i, j, r
        !end function

        subroutine initialize3ddiffusionmat(i1,i2,i3,d1,d2,d3,i4,i5,i6,i7,i8,d4,d5) bind(C)
            integer :: i1, i2, i3, i4, i5, i6, i7, i8
            real*8 :: d1, d2, d3, d4, d5
        end subroutine

        !subroutine getcomponentmatrixvalues(component, values) bind(C)
        !    integer :: component
        !    real*8 pointer(c_ptr), intent(out) :: values
        !end subroutine

        subroutine freematrixvalues() bind(C)
        end subroutine

        subroutine killsparsematrix() bind(C)
        end subroutine

        subroutine freematrixpointers() bind(C)
        end subroutine
    end interface
end module