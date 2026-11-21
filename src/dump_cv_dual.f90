!-----------------------------------------------------------------------
! dump_cv_dual: the keyword cv_dual | matbld on
!
!   dump / stor | fehm | pflotran | zone_outside / file_root / mo / ...
!
! (docs/pages/docs/commands/DUMP2.md). Called by writedump before
! dumpfehm (or dump_outside_list) with the options writedump found.
!
!   cv_dual (default) the .stor (stor, fehm) or .uge (pflotran) and the
!                     boundary areas (fehm, zone_outside) by cv_dual on
!                     the current mesh object, through cv_dual_mo_run
!                     (dump_control_volume.f90); the zone and geometry
!                     files as dumpfehm writes them. The control
!                     volumes: voronoi (default), median or hybrid
!                     (here cv_dual's hybrid). The .stor's second line
!                     ends with the command. ihandled 1.
!                     Zero-area connections (|A| <= zero_tol d^2,
!                     d in 2D) are left out unless the command says
!                     nofilter_zero (dump / pflotran: filter_zero is
!                     its default, as before).
!   matbld            ihandled 0: writedump goes on as before, the
!                     files unchanged. median here is an error.
!
! Without either word, a mesh whose cells are not all triangles or all
! tets goes to matbld, with one line saying so (cv_dual has no other
! cells). With either word, the code named is used.
!
! With cv_dual, an option cv_dual does not have (vector, both,
! area_*, binary, unformatted, pflotran's verbose) leaves the dump to
! matbld (ihandled 0) with one line saying so. A mesh cv_dual refuses
! gives its message and an ERROR line, and nothing is written (with
! dump / fehm not the geometry and zone files either); without either
! word the message also names / matbld (the file matbld wrote), and
! / median, / hybrid for a mesh that is not Delaunay.
!-----------------------------------------------------------------------
subroutine dump_cv_dual(imsgin, xmsgin, cmsgin, msgtype, nwds, ifirst, &
                        ioption, ifile, ifileini, iomode, coef_opt, &
                        write_opt, attrib_option, area_option, &
                        hybrid_option, ihandled, ierror_return)
   use dump_control_volume_lg, only: say, num_text, cv_barea, cv_tag, &
                                     cv_dual_mo_run
   implicit none
   integer :: nwds, ifirst, ihandled, ierror_return
   integer :: imsgin(nwds), msgtype(nwds)
   real*8 :: xmsgin(nwds)
   character(len=*) :: cmsgin(nwds)
   character(len=*) :: ioption, ifile, ifileini, iomode, coef_opt, &
                       write_opt, attrib_option, area_option, &
                       hybrid_option

   integer :: icharlnf
   external icharlnf

   character(len=32) :: cmd, mo, pol, why
   character(len=4096) :: options, command
   character(len=132) :: logmess
   integer :: k, ncv, nmat, npol, nnodes, ierr, want_area
   integer :: nen, ilen, ityp
   logical :: is_fehm, is_stor, is_pflo, is_zone, keepzero

   ihandled = 0
   ierror_return = 0
   is_fehm = ioption(1:4) == 'fehm'
   is_stor = ioption(1:4) == 'stor'
   is_pflo = ioption(1:4) == 'pflo'
   is_zone = ioption(1:12) == 'zone_outside'
   cmd = 'dump/'//ioption(1:icharlnf(ioption))
   if (is_pflo) cmd = 'dump/pflotran'

   ! the words: cv_dual, matbld, and the control volumes
   ncv = 0
   nmat = 0
   npol = 0
   pol = 'voronoi'
   keepzero = .false.
   do k = ifirst, nwds
      if (msgtype(k) /= 3) cycle
      select case (trim(cmsgin(k)))
      case ('cv_dual')
         ncv = ncv + 1
      case ('matbld')
         nmat = nmat + 1
      case ('voronoi', 'median')
         npol = npol + 1
         pol = cmsgin(k)
      case ('hybrid')
         npol = npol + 1
         pol = 'hybrid'
      case ('nofilter_zero')
         keepzero = .true.
      case ('filter_zero')
         keepzero = .false.
      end select
   end do
   if (ncv > 0 .and. nmat > 0) then
      call fail(trim(cmd)//': cv_dual or matbld, not both; nothing '// &
                'written')
      return
   end if

   ! matbld when the command says so (median is cv_dual's)
   if (nmat > 0) then
      if (trim(pol) == 'median' .and. npol == 1) then
         call fail(trim(cmd)//': median needs cv_dual; nothing written')
      end if
      return
   end if

   ! neither word: cv_dual, the default, unless the cells are not all
   ! triangles or all tets (matbld writes those, as before)
   if (ncv == 0) then
      call cmo_get_name(mo, ierr)
      call cmo_get_info('nodes_per_element', mo, nen, ilen, ityp, ierr)
      if (ierr == 0 .and. nen /= 3 .and. nen /= 4) then
         if (trim(pol) == 'median') then
            call fail(trim(cmd)//': cv_dual takes triangles or '// &
                      'tets only and matbld has no median; '// &
                      'nothing written')
            return
         end if
         call say(trim(cmd)//': cells are not all triangles or '// &
                  'tets; matbld writes this dump')
         return
      end if
   end if

   if (npol > 1) then
      call fail(trim(cmd)//': one of voronoi, median, hybrid; '// &
                'nothing written')
      return
   end if

   ! an option cv_dual does not have: matbld writes this dump
   why = ' '
   if (.not. is_zone) then
      if (iomode(1:5) /= 'ascii') why = iomode
      if (coef_opt(1:6) /= 'scalar') why = coef_opt
      if (is_pflo .and. write_opt(1:7) == 'verbose') why = 'verbose'
   end if
   if (len_trim(why) > 0) then
      if (trim(pol) == 'median') then
         call fail(trim(cmd)//': cv_dual does not have '//trim(why)// &
                   ' and matbld has no median; nothing written')
         return
      end if
      if (trim(pol) == 'hybrid') then
         call say(trim(cmd)//': cv_dual does not have '//trim(why)// &
                  '; matbld writes this dump (hybrid: matbld''s)')
      else
         call say(trim(cmd)//': cv_dual does not have '//trim(why)// &
                  '; matbld writes this dump')
      end if
      return
   end if
   ihandled = 1

   ! the boundary areas follow the control volumes
   if ((is_fehm .or. is_zone) .and. &
       ((area_option(1:14) == 'keepatt_median' .and. &
         trim(pol) /= 'median') .or. &
        (area_option(1:15) == 'keepatt_voronoi' .and. &
         trim(pol) /= 'voronoi'))) then
      call say(trim(cmd)//': with cv_dual the boundary areas follow '// &
               'the control volumes ('//trim(pol)//')')
   end if

   ! cv_dual's options and the command as typed (.stor line 2)
   options = 'cv, '//trim(pol)
   want_area = 0
   if (is_stor .or. is_fehm) then
      options = trim(options)//' / stor, '//ifile(1:icharlnf(ifile))// &
                '.stor'
      if (keepzero) then
         options = trim(options)//' / zero_area, keep'
      else
         options = trim(options)//' / zero_area, remove'
      end if
      if (write_opt(1:5) == 'graph' .or. write_opt(1:4) == 'none') then
         options = trim(options)//' / compress, off'
      else
         options = trim(options)//' / compress, on'
      end if
   else if (is_pflo) then
      options = trim(options)//' / uge, '//ifile(1:icharlnf(ifile))// &
                '.uge'
      if (write_opt(1:6) == 'filter') then
         options = trim(options)//' / zero_area, remove'
      else
         options = trim(options)//' / zero_area, keep'
      end if
   end if
   ! flagged facets (a mesh slightly not Delaunay) go beside the
   ! files, named for them, not to violations.vtu
   options = trim(options)//' / violations, '// &
             ifile(1:icharlnf(ifile))//'_violations.vtu'
   if (is_fehm .or. is_zone) want_area = 1
   command = 'cv_dual'
   do k = 1, nwds
      if (k == 1) then
         command = trim(command)//' '//trim(token(k))
      else
         command = trim(command)//' / '//trim(token(k))
      end if
   end do

   call cmo_get_name(mo, ierr)
   call say(trim(cmd)//': control volumes by cv_dual ('//trim(pol)//')')
   call cv_dual_mo_run(mo, trim(options), trim(command), want_area, &
                       nnodes, ierr)
   if (ierr /= 0) then
      if (nnodes == 0) then
         call fail(trim(cmd)//': mesh object '//mo(1:icharlnf(mo))// &
                   ' has no vertices or no cells; nothing written')
      else
         if (ncv == 0) call say(trim(cmd)//': the file matbld '// &
            'wrote before: add / matbld; a mesh that is not '// &
            'Delaunay: / median or / hybrid')
         call fail(trim(cmd)//': cv_dual stopped (see above); '// &
                   'nothing written')
      end if
      return
   end if

   ! dump / fehm: the geometry and zone files, as dumpfehm writes
   ! them; the boundary areas from cv_dual
   if (is_fehm) then
      write (logmess, '(a)') '*** Write FEHMN GEOM AND ZONE FILES ***'
      call writloga('default', 0, logmess, 0, ierr)
      call dump_fehm_geom(ifile, ifileini)
      call dump_material_list(ifile, 0)
      call dump_interface_list(ifile)
      call dump_multi_mat_con(ifile)
   end if
   if (is_fehm .or. is_zone) then
      cv_tag = pol(1:3)
      if (is_fehm) call dotaskx3d('log/tty/off; finish', ierr)
      call dump_outside_list(ifile, attrib_option, area_option)
      if (is_fehm) call dotaskx3d('log/tty/on; finish', ierr)
      cv_tag = '   '
      if (allocated(cv_barea)) deallocate (cv_barea)
   end if
   if (is_fehm) call dump_parent_list(ifile)

contains

   ! Token k as text: a word as it is, a number with every digit.
   function token(k) result(t)
      integer, intent(in) :: k
      character(len=64) :: t
      select case (msgtype(k))
      case (1)
         write (t, '(i0)') imsgin(k)
      case (2)
         t = num_text(xmsgin(k))
      case default
         t = cmsgin(k)
      end select
   end function token

   ! LaGriT's ERROR line, ierror_return 1, the dump handled.
   subroutine fail(why)
      character(len=*), intent(in) :: why
      call say('ERROR: '//why)
      ierror_return = 1
      ihandled = 1
   end subroutine fail

end subroutine dump_cv_dual

!-----------------------------------------------------------------------
! dump_fehm_word: is w one of the keywords of dump / stor, fehm,
! pflotran? writedump takes token 6 as an FEHM .ini file to read
! (dump_fehm_geom) unless it is -def-; a keyword there (dump / fehm /
! root / mo / ascii / keepatt) made LaGriT read a file of that name
! and stop. A keyword is now never taken as the .ini file.
!-----------------------------------------------------------------------
logical function dump_fehm_word(w)
   implicit none
   character(len=*), intent(in) :: w
   select case (trim(w))
   case ('ascii', 'binary', 'unformatted', 'asciic', 'binaryc', &
         'scalar', 'vector', 'both', 'area_scalar', 'area_vector', &
         'area_both', 'alternate_scalar', 'graph', 'coef', 'coefs', &
         'none', 'all', 'keepatt', 'delatt', 'keepatt_voronoi', &
         'keepatt_median', 'keepatt_area', 'hybrid', 'nohybrid', &
         'more', 'verbose', 'filter_zero', 'nofilter_zero', &
         'cv_dual', 'matbld', 'voronoi', 'median')
      dump_fehm_word = .true.
   case default
      dump_fehm_word = .false.
   end select
end function dump_fehm_word
