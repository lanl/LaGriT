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
    ! USAGE:
    ! dfntest / filename / [mo]
    implicit none

    ! Arguments
    integer              :: ierror,nwds
    integer              :: imsgin(nwds),msgtype(nwds)
    real*8               :: xmsgin(nwds)
    character*32         :: cmsgin(nwds),cmo,isubname

    ! Subroutine variables
    pointer (ipitetclr, itetclr)
    pointer (ipzdepths, zdepths)
    character*132        :: cmd,logmess,att_name
    character*100        :: data_filename,tmp_string
    real*8               :: tmp_real,zdepths(1000000)
    integer              :: i,j,ierrw,mtri,ioerr,tmp_int
    integer              :: ilen,itype,icscode,file_unit
    integer              :: itetclr(1000000)
    integer, allocatable :: indexes(:)
    logical              :: file_exists

    mtri = 6
    file_unit = 15
    isubname = 'dfntest'
    att_name = 'ZDEPTHVALUES'

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

    ! Create new attribute to store read values
    cmd = 'cmo / addatt / '// trim(cmo) //' / '// trim(att_name) //' / vdouble / scalar / nelements; finish'
    call dotask(cmd,ierror)

    ! Get itetclr && newly created attribute
    call cmo_get_info('itetclr',cmo,ipitetclr,ilen,itype,icscode)
    call cmo_get_info(trim(att_name),cmo,ipzdepths,ilen,itype,icscode)

    ! -------------------------------------------------------- !
    ! Read file && map to mesh

    ! Check if the file exists...
    inquire(file=data_filename,exist=file_exists)
    if (file_exists .eqv. .false.) then
        call RAISE_ERROR_LG('ERROR: File "'//trim(data_filename)//'" does not exist!',ierror)
        go to 9998
    endif

    ! Read over open file until EOF
    ! Fill in zdepths using the first column as
    ! an index
    open(unit=file_unit,file=data_filename)
    do
        read(file_unit,'(A)',IOSTAT=ioerr) tmp_string
        if (ioerr > 0) then
            ! Malformed file format / broken stream / etc.
            call RAISE_ERROR_LG('ERROR: Something went wrong during file read',ierror)
            close(file_unit)
            go to 9998
        else if (ioerr < 0) then
            ! EOF
            exit
        endif

        ! Ignore comments and empty lines
        if (tmp_string(1:1) == '#')      cycle
        if (trim(tmp_string(1:1)) == '') cycle

        read(tmp_string,*) tmp_int, tmp_real

        do i = 1, mtri
            if (itetclr(i) == tmp_int) zdepths(i) = tmp_real
        end do
    enddo
    close(file_unit)

    ! -------------------------------------------------------- !

    !do i = 1, mtri
    !    print*,itetclr(i),': ',zdepths(i)
    !end do

    !
    ! CALL VORONOI
    !

9998 continue

    ! Remove temp attribute
    cmd = 'cmo / delatt / '// trim(cmo) //' / '// trim(att_name) //'; finish'
    call dotask(cmd,ierror)

9999 continue

end subroutine dfntest_lg