subroutine RAISE_ERROR_LG(error_msg,error_code)
    ! Basic Fortran90 error handler for LaGriT.

    implicit none

    integer error_code,ierrw
    character(len=*),intent(in) :: error_msg
    character*132 logmess

    error_code = 1
    write(logmess,*) trim(error_msg)
    call writloga('default',0,logmess,0,ierrw)

end subroutine RAISE_ERROR_LG

subroutine dfntest_lg(imsgin,xmsgin,cmsgin,msgtype,nwds,ierror)
    implicit none

    ! Arguments
    integer      :: ierror,nwds
    integer      :: imsgin(nwds),msgtype(nwds)
    real*8       :: xmsgin(nwds)
    character*32 :: cmsgin(nwds),cmo,isubname

    ! Subroutine variables
    pointer (ipitetclr, itetclr)
    character*132        :: logmess
    character*100        :: data_filename
    real*8               :: tmp_real
    real*8, allocatable  :: scale_factors(:)
    integer              :: i,ierrw,mtri,ioerr,tmp_int,file_unit
    integer              :: itetclr(1000000)
    integer, allocatable :: indexes(:)
    logical              :: file_exists

    mtri = 6
    file_unit = 15
    isubname = 'dfntest'

    ! -------------------------------------------------------- !

    ! Verify command arguments
    if (nwds < 2) then
        call RAISE_ERROR_LG('ERROR: Malformed command',ierror)
        go to 9999
    else if (nwds > 2) then
        ! Parse CMO && verify that it exists
        cmo = cmsgin(3)
        call cmo_exist(cmo,ierror)
    else
        ! Capture CMO
        call cmo_get_name(cmo,ierror)
    endif

    ! Verify that captured CMO is valid
    if (ierror.ne.0) then
        call RAISE_ERROR_LG('ERROR: CMO does not exist: '//trim(cmo),ierror)
        go to 9999
    endif

    data_filename = cmsgin(2)

    call cmo_get_info('itetclr',cmo,ipitetclr,ilen,itype,icscode)
    call cmo_get_info('nelements',cmo,mtri,ilen,itype,icscode)

    allocate(scale_factors(mtri))
    scale_factors = 0

    ! -------------------------------------------------------- !
    ! Read file && map to mesh

    ! Check if the file exists...
    inquire(file=data_filename,exist=file_exists)
    if (file_exists .eqv. .false.) then
        call RAISE_ERROR_LG('ERROR: File "'//trim(data_filename)//'" does not exist!',ierror)
        go to 9999
    endif

    ! Read over open file until EOF
    ! Fill in scale_factors using the first column as
    ! an index
    i = 1
    open(unit=file_unit, file=data_filename)
    do
        read(file_unit, *, IOSTAT=ioerr) tmp_int, tmp_real
        if (ioerr > 0) then
            ! Malformed file format / broken stream / etc.
            call RAISE_ERROR_LG('ERROR: Something went wrong during file read',ierror)
            go to 9999
        else if (ioerr < 0) then
            ! EOF
            exit
        endif
        scale_factors(tmp_int) = tmp_real
        i = i + 1
    enddo
    close(file_unit)

    ! -------------------------------------------------------- !

    print*,scale_factors

9999 continue

    deallocate(scale_factors)

end subroutine dfntest_lg