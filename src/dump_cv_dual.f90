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
! exclude, imt [imt ...] (dump / fehm and dump / stor, cv_dual only):
! the ghost generators of a conforming Voronoi mesh left out of every
! file. cv_dual computes on the whole mesh and writes the .stor for
! the kept vertices (numbered 1, 2, ... in their order); with fehm the
! geometry and zone files are written from a copy of the mesh object
! without the ghosts (the same numbers), and the outside zones and
! areas from the faces between kept vertices and ghosts (cv_dual_run's
! ghost_face_*): each face to the side its normal points to most, a
! vertex in each side it has a face on, with the sum of those faces.
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
! / median for a mesh that is not Delaunay (the only control volumes
! cv_dual makes on one; hybrid starts from Voronoi and is refused too).
!-----------------------------------------------------------------------
subroutine dump_cv_dual(imsgin, xmsgin, cmsgin, msgtype, nwds, ifirst, &
                        ioption, ifile, ifileini, iomode, coef_opt, &
                        write_opt, attrib_option, area_option, &
                        hybrid_option, ihandled, ierror_return)
   use dump_control_volume_lg, only: say, num_text, cv_barea, cv_tag, &
                                     cv_dual_mo_run
   use cv_dual_run, only: want_ghost_faces, nghost_face, &
                          ghost_face_v, ghost_face_vec
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
   ! exclude, imt [imt ...]: the ghost generators' materials
   integer :: nexcl, kx
   integer :: iexcl(64)
   character(len=256) :: ini

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
   ! exclude, imt [imt ...]: the numbers after the word
   nexcl = 0
   do k = ifirst, nwds
      if (msgtype(k) /= 3) cycle
      if (trim(cmsgin(k)) /= 'exclude') cycle
      do kx = k + 1, nwds
         if (msgtype(kx) == 1) then
            if (nexcl >= 64) exit
            nexcl = nexcl + 1
            iexcl(nexcl) = imsgin(kx)
         else if (msgtype(kx) == 2) then
            if (xmsgin(kx) /= anint(xmsgin(kx))) exit
            if (nexcl >= 64) exit
            nexcl = nexcl + 1
            iexcl(nexcl) = nint(xmsgin(kx))
         else
            exit
         end if
      end do
      if (nexcl == 0) then
         call fail(trim(cmd)//': exclude needs one or more material '// &
                   'numbers; nothing written')
         return
      end if
      exit
   end do
   ini = ifileini
   if (nexcl > 0) then
      if (.not. (is_fehm .or. is_stor)) then
         call fail(trim(cmd)//': exclude is for dump / fehm and '// &
                   'dump / stor; nothing written')
         return
      end if
      if (nmat > 0) then
         call fail(trim(cmd)//': exclude needs cv_dual, not matbld; '// &
                   'nothing written')
         return
      end if
      ! a number after exclude is not an FEHM .ini file
      if (len_trim(ini) > 0) then
         if (verify(trim(ini), '0123456789.+-eEdD') == 0) ini = ' '
      end if
   end if

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
         if (nexcl > 0) then
            call fail(trim(cmd)//': exclude needs triangles or tets '// &
                      '(cv_dual); nothing written')
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
      if (nexcl > 0) then
         call fail(trim(cmd)//': cv_dual does not have '//trim(why)// &
                   ' and exclude needs cv_dual; nothing written')
         return
      end if
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
   if (nexcl > 0) then
      options = trim(options)//' / exclude'
      do kx = 1, nexcl
         write (why, '(i0)') iexcl(kx)
         options = trim(options)//', '//trim(why)
      end do
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
   if (nexcl > 0 .and. is_fehm) then
      if (has_parents()) then
         call fail(trim(cmd)//': exclude with parent/child vertices '// &
                   'is not done; nothing written')
         return
      end if
   end if
   call say(trim(cmd)//': control volumes by cv_dual ('//trim(pol)//')')
   want_ghost_faces = nexcl > 0 .and. is_fehm
   nghost_face = 0
   call cv_dual_mo_run(mo, trim(options), trim(command), want_area, &
                       nnodes, ierr)
   want_ghost_faces = .false.
   if (ierr /= 0) then
      if (nnodes == 0) then
         call fail(trim(cmd)//': mesh object '//mo(1:icharlnf(mo))// &
                   ' has no vertices or no cells; nothing written')
      else
         if (ncv == 0) call say(trim(cmd)//': the file matbld '// &
            'wrote before: add / matbld; a mesh that is not '// &
            'Delaunay: / median')
         call fail(trim(cmd)//': cv_dual stopped (see above); '// &
                   'nothing written')
      end if
      return
   end if

   ! dump / fehm / ... / exclude: the files without the ghosts
   if (is_fehm .and. nexcl > 0) then
      call write_kept()
      if (allocated(cv_barea)) deallocate (cv_barea)
      if (allocated(ghost_face_v)) deallocate (ghost_face_v)
      if (allocated(ghost_face_vec)) deallocate (ghost_face_vec)
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

   ! exclude: the geometry and zone files written from a copy of the
   ! mesh object without the ghosts (their vertices and every cell
   ! that has one), so the vertices are numbered as in the .stor; the
   ! outside zones and areas from the faces the kept vertices share
   ! with the ghosts.
   subroutine write_kept()
      character(len=32), parameter :: tmp = 'cvx_kept_tmp'
      character(len=256) :: task
      integer :: nk, ierr2
      call dotaskx3d('log/tty/off; finish', ierr2)
      call dotaskx3d('cmo/copy/'//trim(tmp)//'/'//trim(mo)// &
                     '; finish', ierr2)
      call dotaskx3d('cmo/select/'//trim(tmp)//'; finish', ierr2)
      do kx = 1, nexcl
         write (task, '(a,i0,a)') 'pset/cvx_ghost/attribute/imt1/'// &
            '1,0,0/eq,', iexcl(kx), '; finish'
         call dotaskx3d(trim(task), ierr2)
         call dotaskx3d('rmpoint/pset,get,cvx_ghost/inclusive; '// &
                        'finish', ierr2)
         call dotaskx3d('pset/cvx_ghost/delete; finish', ierr2)
      end do
      call dotaskx3d('rmpoint/compress; finish', ierr2)
      call dotaskx3d('log/tty/on; finish', ierr2)
      call cmo_get_info('nnodes', tmp, nk, ilen, ityp, ierr2)
      write (logmess, '(a,i0,a)') '*** Write FEHMN GEOM AND ZONE '// &
         'FILES (exclude: ', nk, ' vertices) ***'
      call writloga('default', 0, logmess, 0, ierr2)
      call dump_fehm_geom(ifile, ini)
      call dump_material_list(ifile, 0)
      call dump_interface_list(ifile)
      call dump_multi_mat_con(ifile)
      call write_outside_ghost(nk)
      call dotaskx3d('log/tty/off; finish', ierr2)
      call dotaskx3d('cmo/delete/'//trim(tmp)//'; finish', ierr2)
      call dotaskx3d('cmo/select/'//trim(mo)//'; finish', ierr2)
      call dotaskx3d('log/tty/on; finish', ierr2)
   end subroutine write_kept

   ! ROOT_outside.zone and ROOT_outside_TAG.area in dump_outside_list's
   ! layout. Each face between a kept vertex and a ghost goes to the
   ! side its normal points to most (+z top, -z bottom, -x left_w,
   ! -y front_s, +x right_e, +y back_n); a vertex is listed in each
   ! side it has a face on, with the sum of its faces on that side.
   subroutine write_outside_ghost(nk)
      integer, intent(in) :: nk
      character(len=10), parameter :: side(6) = [character(len=10) :: &
         'top', 'bottom', 'left_w', 'front_s', 'right_e', 'back_n']
      integer, parameter :: order(6) = [1, 2, 3, 5, 6, 4]
      real*8, allocatable :: sv(:, :, :)
      logical, allocatable :: on(:, :)
      integer, allocatable :: list(:)
      real*8 :: v(3), a(3), total
      integer :: f, i, n, s, iz, iu, iu2, icount, ierr2
      character(len=24) :: tstring
      character(len=256) :: fname

      allocate (sv(3, nk, 6), on(nk, 6), list(nk))
      sv = 0.0d0
      on = .false.
      do f = 1, nghost_face
         i = ghost_face_v(f)
         if (i < 1 .or. i > nk) cycle
         v = ghost_face_vec(:, f)
         a = abs(v)
         if (a(1) >= a(2) .and. a(1) >= a(3)) then
            s = merge(5, 3, v(1) > 0.0d0)
         else if (a(2) >= a(3)) then
            s = merge(6, 4, v(2) > 0.0d0)
         else
            s = merge(1, 2, v(3) > 0.0d0)
         end if
         sv(:, i, s) = sv(:, i, s) + v
         on(i, s) = .true.
      end do
      select case (cv_tag_of(pol))
      case ('med')
         tstring = '   Sum MEDIAN vectors:  '
      case ('hyb')
         tstring = '   Sum HYBRID vectors:  '
      case default
         tstring = '   Sum VORONOI vectors: '
      end select
      iu = -1
      fname = ifile(1:icharlnf(ifile))//'_outside.zone'
      call hassign(iu, fname, ierr2)
      iu2 = -1
      fname = ifile(1:icharlnf(ifile))//'_outside_'// &
              cv_tag_of(pol)//'.area'
      call hassign(iu2, fname, ierr2)
      if (iu < 0 .or. iu2 < 0) then
         call say('ERROR: '//trim(cmd)//': cannot open the outside '// &
                  'files')
         return
      end if
      write (iu, '(a4)') 'zone'
      write (iu2, '(a4)') 'zone'
      total = 0.0d0
      do iz = 1, 6
         s = order(iz)
         icount = 0
         do n = 1, nk
            if (on(n, s)) then
               icount = icount + 1
               list(icount) = n
            end if
         end do
         if (icount == 0) cycle
         a = 0.0d0
         do n = 1, icount
            a = a + abs(sv(:, list(n), s))
         end do
         if (s <= 2) then
            total = total + a(3)
         else if (s == 3 .or. s == 5) then
            total = total + a(1)
         else
            total = total + a(2)
         end if
         write (iu, '(i5.5,2x,a)') s, trim(side(s))
         write (iu, '(a4)') 'nnum'
         write (iu, *) icount
         write (iu, '(10(i10,1x))') (list(n), n=1, icount)
         write (iu2, '(i5.5,2x,a,3e14.7)') s, trim(side(s))//tstring, &
            a(1), a(2), a(3)
         write (iu2, '(a4)') 'nnum'
         write (iu2, *) icount
         write (iu2, '(6(1x,1pe20.12))') ((sv(k, list(n), s), k=1, 3), &
                                          n=1, icount)
         write (logmess, '(a,a10,a,i9,a)') 'Face ', side(s), &
            ' has ', icount, ' nodes (faces shared with ghosts).'
         call writloga('default', 0, logmess, 0, ierr2)
      end do
      write (iu, '(a)') ' '
      write (iu2, '(a)') ' '
      write (iu, '(a4)') 'stop'
      write (iu2, '(a4)') 'stop'
      close (iu)
      close (iu2)
      write (logmess, '(a,1pe20.12)') 'Sum of outside areas: ', total
      call writloga('default', 0, logmess, 0, ierr2)
   end subroutine write_outside_ghost

   function cv_tag_of(p) result(t)
      character(len=*), intent(in) :: p
      character(len=3) :: t
      t = p(1:3)
   end function cv_tag_of

   ! any parent vertex (itp1 41: parent/child chains)?
   logical function has_parents()
      pointer (ipitp1, itp1)
      integer :: itp1(*)
      integer :: nn, ierr2, i
      has_parents = .false.
      call cmo_get_info('nnodes', mo, nn, ilen, ityp, ierr2)
      call cmo_get_info('itp1', mo, ipitp1, ilen, ityp, ierr2)
      if (ierr2 /= 0) return
      do i = 1, nn
         if (itp1(i) == 41) then
            has_parents = .true.
            return
         end if
      end do
   end function has_parents

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
         'cv_dual', 'matbld', 'voronoi', 'median', 'exclude')
      dump_fehm_word = .true.
   case default
      dump_fehm_word = .false.
   end select
end function dump_fehm_word
