module lg_interface
    !DEC$ ATTRIBUTES STDCALL, REFERENCE, ALIAS: "initialize3ddiffusionmat" :: initialize3ddiffusionmat
    !DEC$ ATTRIBUTES STDCALL, REFERENCE, ALIAS: "finalscalar3ddiffusionmat" :: finalscalar3ddiffusionmat
    !DEC$ ATTRIBUTES STDCALL, REFERENCE, ALIAS: "extractnegativecoefs" :: extractnegativecoefs
    !DEC$ ATTRIBUTES STDCALL, REFERENCE, ALIAS: "freenegcoefs" :: freenegcoefs
    !DEC$ ATTRIBUTES STDCALL, REFERENCE, ALIAS: "getmatrixsizes" :: getmatrixsizes
    !DEC$ ATTRIBUTES STDCALL, REFERENCE, ALIAS: "getvoronoivolumes" :: getvoronoivolumes
    !DEC$ ATTRIBUTES STDCALL, REFERENCE, ALIAS: "freevoronoivolumes" :: freevoronoivolumes
    !DEC$ ATTRIBUTES STDCALL, REFERENCE, ALIAS: "getentriesperrow" :: getentriesperrow
    !DEC$ ATTRIBUTES STDCALL, REFERENCE, ALIAS: "freeoccupiedcolumns" :: freeoccupiedcolumns
    !DEC$ ATTRIBUTES STDCALL, REFERENCE, ALIAS: "getoccupiedcolumns" :: getoccupiedcolumns
    !DEC$ ATTRIBUTES STDCALL, REFERENCE, ALIAS: "freeentriesperrow" :: freeentriesperrow
    !DEC$ ATTRIBUTES STDCALL, REFERENCE, ALIAS: "getmatrixpointers" :: getmatrixpointers
    
    !DEC$ ATTRIBUTES STDCALL, REFERENCE, ALIAS: "fgmvwriteopenfile" :: fgmvwriteopenfile
    !DEC$ ATTRIBUTES STDCALL, REFERENCE, ALIAS: "fgmvwritenodedata" :: fgmvwritenodedata
    !DEC$ ATTRIBUTES STDCALL, REFERENCE, ALIAS: "fgmvwritecellheader" :: fgmvwritecellheader
    !DEC$ ATTRIBUTES STDCALL, REFERENCE, ALIAS: "fgmvwritecelldata" :: fgmvwritecelldata
    !DEC$ ATTRIBUTES STDCALL, REFERENCE, ALIAS: "fgmvwritevelocitydata" :: fgmvwritevelocitydata
    !DEC$ ATTRIBUTES STDCALL, REFERENCE, ALIAS: "fgmvwritevariableheader" :: fgmvwritevariableheader

    use iso_c_binding
    implicit none

    interface
    end interface

contains

    !!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
    !!! Binding function for CMO_GET_INTINFO
    !!! Uses native ISO C interface to get around pointer weirdness
    subroutine cmo_get_intinfo_c(ioption, cmo_name, iout, ierr)
        use, intrinsic :: iso_c_binding
        implicit none

        ! C input args
        character*(*), intent(in) :: cmo_name, ioption
        integer(c_int), intent(out) :: iout, ierr

        ! Temporary Fortran params
        integer :: f_iout, f_lout, f_itype, f_ierr

        call cmo_get_intinfo(ioption, cmo_name, f_iout, f_lout, f_itype, f_ierr)

        iout = f_iout
        ierr = f_ierr
    end subroutine

    !!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
    !!! Binding function for CMO_GET_INTINFO
    !!! Uses native ISO C interface to get around pointer weirdness
    subroutine cmo_get_stdptrs_c(cmo, ipimt1, ipitp1, ipicr1, ipisn1, &
      ipxic, ipyic, ipzic, ipitetclr, ipitettyp, ipitetoff, &
      ipjtetoff, ipitet, ipjtet, ierror)
        use, intrinsic :: iso_c_binding
        !use, intrinsic :: iso_fortran_env, only: TRANSFER
        implicit none

        ! C input args
        character*(*), intent(in) :: cmo
        integer(c_int), intent(out) :: ierror
        type(c_ptr), intent(inout) :: ipimt1, ipitp1, ipicr1, ipisn1
        type(c_ptr), intent(inout) :: ipxic, ipyic, ipzic, ipitetclr, ipitettyp, ipitetoff
        type(c_ptr), intent(inout) :: ipjtetoff, ipitet, ipjtet

        ! Temporary Fortran params
        integer :: f_ierror
        character*32 :: f_cmo

        ! Cray pointers
        pointer(f_ipimt1,imt1)
        integer, target :: imt1(*)

        pointer (f_ipitp1,itp1)
        integer itp1(*)

        pointer (f_ipicr1,icr1)
        integer icr1(*)

        pointer (f_ipisn1,isn1)
        integer isn1(*)

        pointer (f_ipxic,xic)
        real*8 xic(*)

        pointer (f_ipyic,yic)
        real*8 yic(*)

        pointer (f_ipzic,zic)
        real*8 zic(*)

        pointer (f_ipitetclr,itetclr)
        integer itetclr(*)

        pointer (f_ipitettyp,itettyp)
        integer itettyp(*)

        pointer (f_ipitetoff,itetoff)
        integer itetoff(*)

        pointer (f_ipjtetoff,jtetoff)
        integer jtetoff(*)

        pointer (f_ipitet,itet)
        integer itet(*)

        pointer (f_ipjtet,jtet)
        integer jtet(*)

        integer j,tmp

        !!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!

        f_cmo = trim(cmo)
        print*,'cmo_get_stdptrs_c: cmo_name = ',cmo
        print*,'cmo_get_stdptrs_c: f_cmo    = ',f_cmo

        call cmo_get_stdptrs(f_cmo, f_ipimt1, f_ipitp1, &
            f_ipicr1, f_ipisn1, f_ipxic, f_ipyic, f_ipzic, &
            f_ipitetclr, f_ipitettyp, f_ipitetoff, &
            f_ipjtetoff, f_ipitet, f_ipjtet, f_ierror)

        print*,'f_ierr = ', f_ierror

        print*,'f_imt1 = ',( imt1(j), j=1,10 )
        print*,'f_ipimt1 = ',f_ipimt1

        tmp = f_ipimt1

        !ipimt1 = c_loc(imt1)!transfer(tmp, ipimt1)


        !ipimt1 = c_loc(imt1)
        !ipxic = c_loc(xic)
        !ipyic = c_loc(yic(1))
        !ipzic = c_loc(zic(1))

        print*,'returning...'

        ierror = f_ierror

    end subroutine

end module