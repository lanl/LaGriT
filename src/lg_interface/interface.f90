module lg_interface
    use iso_c_binding
    implicit none

    private
    public :: &
        entryprocessed, &
        cread4, &
        cread4_ptr, &
        cread4_int, &
        cread4_int_ptr, &
        cread8

interface

!contains

integer(kind=c_int) function entryprocessed(i, j) bind(c)! &
    !bind(C, name="entryprocessed_")
    use iso_c_binding, only: c_int
    implicit none
    integer :: i, j
end function

subroutine cread4(unit, array, ilen, ierr) bind(c, name="cread")
    use iso_c_binding
    implicit none

    integer(kind=4) :: unit
    integer :: ilen, ierr
    real(C_FLOAT) :: array
end subroutine

subroutine cread4_ptr(unit, array, ilen, ierr) bind(c, name="cread")
    use iso_c_binding
    implicit none

    integer(kind=4) :: unit
    integer :: ilen, ierr
    real(C_FLOAT) :: array(*)
end subroutine

subroutine cread4_int(unit, array, ilen, ierr) bind(c, name="cread")
    use iso_c_binding
    implicit none

    integer(kind=4) :: unit
    integer :: ilen, ierr
    integer(kind=4) :: array
end subroutine

subroutine cread4_int_ptr(unit, array, ilen, ierr) bind(c, name="cread")
    use iso_c_binding
    implicit none

    integer(kind=4) :: unit
    integer :: ilen, ierr
    integer(kind=4) :: array(*)
end subroutine

subroutine cread8(unit, array, ilen, ierr) bind(c, name="cread")
    use iso_c_binding
    implicit none

    integer(kind=4) :: unit
    integer :: ilen, ierr
    real(c_double) :: array
end subroutine

end interface
end module