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
    integer              :: ierror,nwds
    integer              :: imsgin(nwds),msgtype(nwds)
    real*8               :: xmsgin(nwds)
    character*32         :: cmsgin(nwds),cmo,isubname

    ! Subroutine variables
    pointer (ipitetclr, itetclr)
    character*132        :: logmess
    character*100        :: data_filename,tmp_string
    real*8               :: tmp_real
    real*8, allocatable  :: scale_factors(:)
    integer              :: i,j,ierrw,mtri,ioerr,tmp_int
    integer              :: ilen,itype,icscode,file_unit
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

    ! Get && verify number of elements
    call cmo_get_info('nelements',cmo,mtri,ilen,itype,icscode)
    if (mtri == 0) then
        call RAISE_ERROR_LG('ERROR: Mesh has no elements',ierror)
        go to 9999
    endif

    print*,'nelements = ',mtri

    ! Get itetclr
    call cmo_get_info('itetclr',cmo,ipitetclr,ilen,itype,icscode)

    print*,'itetclr = ',itetclr(1:mtri)

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
    ! TODO: Add comments
    i = 1
    open(unit=file_unit, file=data_filename)
    do
        !read(file_unit, *, IOSTAT=ioerr) tmp_int, tmp_real
        read(file_unit, '(A)', IOSTAT=ioerr) tmp_string
        if (ioerr > 0) then
            ! Malformed file format / broken stream / etc.
            call RAISE_ERROR_LG('ERROR: Something went wrong during file read',ierror)
            go to 9999
        else if (ioerr < 0) then
            ! EOF
            exit
        endif

        ! Ignore comments and empty lines
        if (tmp_string(1:1) == '#') cycle
        if (trim(tmp_string(1:1)) == '') cycle

        read(tmp_string,*) tmp_int, tmp_real

        do j = 1, mtri
            if (itetclr(j) == tmp_int) scale_factors(j) = tmp_real
        end do

        i = i + 1
    enddo
    close(file_unit)

    ! -------------------------------------------------------- !

    do i = 1, mtri
        print*,itetclr(i),': ',scale_factors(i)
    end do

    !call cmo_addatt(imsgin,xmsgin,cmsgin,msgtype,nwds,ierror_return)

9999 continue

    if (allocated(scale_factors)) deallocate(scale_factors)

end subroutine dfntest_lg