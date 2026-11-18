!-----------------------------------------------------------------------
! dump_control_volume: the LaGriT command
!
!   dump / control_volume / mo_name / keyword, value / ...
!
! Control volumes and their geometric coefficients (Voronoi, median or
! hybrid) of a mesh object of triangles or tets, by cv_dual
! (src/cv_dual; docs/pages/docs/commands/dump/DUMP_CONTROL_VOLUME.md).
! Called by writedump. The keywords and values are cv_dual's (the
! program's names, defaults and messages), but not avs; one more,
! LaGriT's own:
!
!   attribute_cv_volume [, name]   the control volumes into the
!                                  vertex attribute name (default
!                                  cv_volume; a real per vertex; made
!                                  if missing)
!
! The mesh object's arrays are used in place (never copied, never
! changed). cv_dual's lines go to LaGriT's log (writloga). The .stor's
! second line ends with (cv_dual dump / control_volume / mo_name /
! ...), the command. An error gives cv_dual's message, an ERROR line
! and ierror_return 1; nothing is written and the session goes on.
!-----------------------------------------------------------------------
module dump_control_volume_lg
   implicit none
   private
   public :: to_writloga, num_text, say
   public :: cv_barea, cv_tag, cv_tag_word, cv_area_attribute, &
             cv_dual_mo_run

   ! cv_dual's boundary vector areas (3, nnodes) for dump_outside_list,
   ! with the control volumes' tag ('vor', 'med', 'hyb', 'bld'); a
   ! blank tag: dump_outside_list computes its own (get_xcontab)
   real*8, allocatable :: cv_barea(:, :)
   character(len=3) :: cv_tag = '   '

contains

   ! The control volumes' name for a tag, for messages and files.
   function cv_tag_word(tag) result(w)
      character(len=*), intent(in) :: tag
      character(len=:), allocatable :: w
      select case (tag)
      case ('vor')
         w = 'Voronoi'
      case ('med')
         w = 'Median'
      case ('hyb')
         w = 'Hybrid'
      case ('bld')
         w = 'Blend'
      case default
         w = trim(tag)
      end select
   end function cv_tag_word

   ! Find or make the real vertex attribute aname of the current mesh
   ! object; ip its pointer. ierr_att = 1 if it cannot be made.
   subroutine cv_area_attribute(aname, cmo, ip, ierr_att)
      character(len=*), intent(in) :: aname, cmo
      integer :: ip, ierr_att
      integer :: ilen, ityp, ierr, ics
      call mmfindbk(aname, cmo, ip, ilen, ierr)
      if (ierr /= 0) then
         call dotask('cmo/addatt/-def-/'//aname// &
                     '/vdouble/scalar/nnodes;finish', ics)
         call cmo_get_info(aname, cmo, ip, ilen, ityp, ierr)
         if (ierr /= 0) ierr_att = 1
      else
         call to_writloga('attribute already exists, will overwrite: '// &
                          aname)
      end if
   end subroutine cv_area_attribute

   ! cv_dual on the mesh object mo (its arrays in place): options and
   ! command as for cv_dual_lagrit_entry. With want_area 1 the
   ! boundary areas go into cv_barea. ierr 0, or 1 after an error (the
   ! message already in the log); nnodes 0 if mo has no vertices or
   ! cells.
   subroutine cv_dual_mo_run(mo, options, command, want_area, nnodes, &
                             ierr)
      use cv_dual_log, only: log_set_sink
      character(len=*), intent(in) :: mo, options, command
      integer, intent(in) :: want_area
      integer, intent(out) :: nnodes, ierr
      external cv_dual_lagrit_entry
      pointer (ipxic, xic)
      pointer (ipyic, yic)
      pointer (ipzic, zic)
      pointer (ipitettyp, itettyp)
      pointer (ipitetoff, itetoff)
      pointer (ipitet, itet)
      pointer (ipitetclr, itetclr)
      pointer (ipimt1, imt1)
      pointer (ipitp1, itp1)
      pointer (ipisn1, isn1)
      real*8 :: xic(*), yic(*), zic(*)
      integer :: itettyp(*), itetoff(*), itet(*), itetclr(*)
      integer :: imt1(*), itp1(*), isn1(*)
      real*8, allocatable :: vol(:)
      real*8 :: dummy(3)
      integer :: nelements, nen, ilen, ityp

      ierr = 0
      call cmo_get_info('nnodes', mo, nnodes, ilen, ityp, ierr)
      call cmo_get_info('nelements', mo, nelements, ilen, ityp, ierr)
      call cmo_get_info('nodes_per_element', mo, nen, ilen, ityp, ierr)
      if (nnodes < 1 .or. nelements < 1) then
         nnodes = 0
         ierr = 1
         return
      end if
      call cmo_get_info('xic', mo, ipxic, ilen, ityp, ierr)
      call cmo_get_info('yic', mo, ipyic, ilen, ityp, ierr)
      call cmo_get_info('zic', mo, ipzic, ilen, ityp, ierr)
      call cmo_get_info('itettyp', mo, ipitettyp, ilen, ityp, ierr)
      call cmo_get_info('itetoff', mo, ipitetoff, ilen, ityp, ierr)
      call cmo_get_info('itet', mo, ipitet, ilen, ityp, ierr)
      call cmo_get_info('itetclr', mo, ipitetclr, ilen, ityp, ierr)
      call cmo_get_info('imt1', mo, ipimt1, ilen, ityp, ierr)
      call cmo_get_info('itp1', mo, ipitp1, ilen, ityp, ierr)
      call cmo_get_info('isn1', mo, ipisn1, ilen, ityp, ierr)

      allocate (vol(nnodes))
      vol = 0.0d0
      if (allocated(cv_barea)) deallocate (cv_barea)
      call log_set_sink(to_writloga)
      if (want_area == 1) then
         allocate (cv_barea(3, nnodes))
         cv_barea = 0.0d0
         call cv_dual_lagrit_entry(nnodes, nelements, nen, xic, yic, &
                                   zic, itettyp, itetoff, itet, &
                                   itetclr, imt1, itp1, isn1, options, &
                                   command, vol, 1, cv_barea, ierr)
      else
         call cv_dual_lagrit_entry(nnodes, nelements, nen, xic, yic, &
                                   zic, itettyp, itetoff, itet, &
                                   itetclr, imt1, itp1, isn1, options, &
                                   command, vol, 0, dummy, ierr)
      end if
      call log_set_sink()
      if (ierr /= 0 .and. allocated(cv_barea)) deallocate (cv_barea)
   end subroutine cv_dual_mo_run

   ! cv_dual's log hook: every line to LaGriT's log and screen. The
   ! line goes in a buffer of at least 80 characters: writlogb writes
   ! with (a80), which would put a shorter message at the right.
   subroutine to_writloga(msg)
      character(len=*), intent(in) :: msg
      character(len=max(132, len(msg))) :: buf
      integer :: ierr
      buf = msg
      call writloga('default', 0, buf, 0, ierr)
   end subroutine to_writloga

   ! A message of any length, at most 80 columns a line (LaGriT's log
   ! cuts longer lines): broken at blanks, later lines indented 2.
   subroutine say(text)
      character(len=*), intent(in) :: text
      character(len=len(text) + 2) :: rest
      integer :: n, b
      rest = adjustl(text)
      do
         n = len_trim(rest)
         if (n <= 80) then
            call to_writloga(rest(1:n))
            exit
         end if
         b = index(rest(1:81), ' ', back=.true.)
         if (b <= 3) b = 80
         call to_writloga(rest(1:b - 1))
         rest = '  '//adjustl(rest(b:))
      end do
   end subroutine say

   ! A real token written back with every digit it has: the shortest
   ! E form that reads back to the same double.
   function num_text(x) result(t)
      real*8, intent(in) :: x
      character(len=40) :: t
      character(len=40) :: fm
      real*8 :: y
      integer :: d, ie
      do d = 1, 17
         write (fm, '(a,i0,a)') '(es40.', d, 'e3)'
         write (t, fm) x
         read (t, *) y
         if (y == x) exit
      end do
      t = adjustl(t)
      ! exponent as cv_dual writes it: E-013 -> E-13, E+000 -> E+00
      ie = index(t, 'E')
      if (ie > 0) then
         if (t(ie + 2:ie + 2) == '0') t = t(1:ie + 1)//t(ie + 3:)
      end if
   end function num_text

end module dump_control_volume_lg

subroutine dump_control_volume(imsgin, xmsgin, cmsgin, msgtype, nwds, &
                               ierror_return)
   use dump_control_volume_lg, only: to_writloga, num_text, say
   use cv_dual_log, only: log_set_sink
   use cv_dual_options, only: is_keyword
   implicit none
   integer :: nwds, ierror_return
   integer :: imsgin(nwds), msgtype(nwds)
   real*8 :: xmsgin(nwds)
   character(len=*) :: cmsgin(nwds)

   external cv_dual_lagrit_entry
   integer :: icharlnf
   external icharlnf

   ! the mesh object's arrays (LaGriT's memory manager)
   pointer (ipxic, xic)
   pointer (ipyic, yic)
   pointer (ipzic, zic)
   pointer (ipitettyp, itettyp)
   pointer (ipitetoff, itetoff)
   pointer (ipitet, itet)
   pointer (ipitetclr, itetclr)
   pointer (ipimt1, imt1)
   pointer (ipitp1, itp1)
   pointer (ipisn1, isn1)
   pointer (ipvatt, vatt)
   real*8 :: xic(*), yic(*), zic(*), vatt(*)
   integer :: itettyp(*), itetoff(*), itet(*), itetclr(*)
   integer :: imt1(*), itp1(*), isn1(*)

   character(len=32) :: mo, aname, ctype, crank, clen, cinter, cpers
   character(len=32) :: cio
   character(len=4096) :: options, command, word
   character(len=256) :: cbuf
   character(len=132) :: logmess
   real*8, allocatable :: vol(:)
   real*8 :: dummy(3)
   integer :: nnodes, nelements, nen, ilen, ityp, ierr, k, ind
   logical :: want_att, in_group, have_att
   real*8, parameter :: unset = -huge(1.0d0)

   ierror_return = 0
   want_att = .false.
   aname = 'cv_volume'

   ! the mesh object (required)
   if (nwds < 3) then
      call fail('dump/control_volume: a mesh object name is needed '// &
                '(dump / control_volume / mo_name / ...)')
      return
   end if
   if (msgtype(3) /= 3) then
      call fail('dump/control_volume: token 3 must be a mesh '// &
                'object name')
      return
   end if
   mo = cmsgin(3)
   call cmo_exist(mo, ierr)
   if (ierr /= 0) then
      call fail('dump/control_volume: no mesh object '// &
                mo(1:icharlnf(mo)))
      return
   end if

   ! the options (cv_dual's keywords, attribute_cv_volume taken out)
   ! and the command as typed, for the .stor's second line
   options = ' '
   command = 'cv_dual dump / control_volume / '//mo(1:icharlnf(mo))
   in_group = .false.
   k = 4
   do while (k <= nwds)
      word = token(k)
      if (msgtype(k) == 3 .and. trim(word) == 'attribute_cv_volume') &
         then
         want_att = .true.
         command = trim(command)//' / attribute_cv_volume'
         in_group = .true.
         if (k + 1 <= nwds) then
            if (msgtype(k + 1) == 3 .and. &
                .not. is_keyword(token(k + 1))) then
               aname = cmsgin(k + 1)
               command = trim(command)//', '//trim(aname)
               k = k + 1
            end if
         end if
         in_group = .false.
         k = k + 1
         cycle
      end if
      options = trim(options)//' '//trim(word)
      if (msgtype(k) == 3 .and. is_keyword(word)) then
         command = trim(command)//' / '//trim(word)
         in_group = .true.
      else if (in_group) then
         command = trim(command)//', '//trim(word)
         in_group = .false.
      else
         command = trim(command)//' '//trim(word)
      end if
      k = k + 1
   end do

   ! the mesh object's arrays, in place
   call cmo_get_info('nnodes', mo, nnodes, ilen, ityp, ierr)
   call cmo_get_info('nelements', mo, nelements, ilen, ityp, ierr)
   call cmo_get_info('nodes_per_element', mo, nen, ilen, ityp, ierr)
   if (nnodes < 1 .or. nelements < 1) then
      call fail('dump/control_volume: mesh object '// &
                mo(1:icharlnf(mo))//' has no vertices or no cells')
      return
   end if
   call cmo_get_info('xic', mo, ipxic, ilen, ityp, ierr)
   call cmo_get_info('yic', mo, ipyic, ilen, ityp, ierr)
   call cmo_get_info('zic', mo, ipzic, ilen, ityp, ierr)
   call cmo_get_info('itettyp', mo, ipitettyp, ilen, ityp, ierr)
   call cmo_get_info('itetoff', mo, ipitetoff, ilen, ityp, ierr)
   call cmo_get_info('itet', mo, ipitet, ilen, ityp, ierr)
   call cmo_get_info('itetclr', mo, ipitetclr, ilen, ityp, ierr)
   call cmo_get_info('imt1', mo, ipimt1, ilen, ityp, ierr)
   call cmo_get_info('itp1', mo, ipitp1, ilen, ityp, ierr)
   call cmo_get_info('isn1', mo, ipisn1, ilen, ityp, ierr)

   ! an attribute of that name, if there is one, must be a real per
   ! vertex: checked before the run, so nothing is written if not
   have_att = .false.
   if (want_att) then
      call cmo_get_attparam(aname, mo, ind, ctype, crank, clen, &
                            cinter, cpers, cio, ierr)
      if (ierr == 0) then
         if (ctype(1:7) /= 'VDOUBLE' .or. crank(1:6) /= 'scalar' &
             .or. clen(1:6) /= 'nnodes') then
            call fail('dump/control_volume: attribute '// &
                      aname(1:icharlnf(aname))//' exists and is not '// &
                      'a real vertex attribute (VDOUBLE, scalar, '// &
                      'nnodes); nothing written')
            return
         end if
         have_att = .true.
      end if
   end if

   ! the run, its lines to LaGriT's log
   allocate (vol(nnodes))
   vol = unset
   call log_set_sink(to_writloga)
   call cv_dual_lagrit_entry(nnodes, nelements, nen, xic, yic, zic, &
                             itettyp, itetoff, itet, itetclr, imt1, &
                             itp1, isn1, trim(adjustl(options)), &
                             trim(command), vol, 0, dummy, ierr)
   call log_set_sink()
   if (ierr /= 0) then
      call fail('dump/control_volume: cv_dual stopped (see above); '// &
                'nothing written')
      return
   end if

   ! the control volumes into a vertex attribute, if asked for
   if (want_att) then
      if (vol(1) == unset) then
         call say('dump/control_volume: check_only on: no control '// &
                  'volumes, attribute '//aname(1:icharlnf(aname))// &
                  ' not set')
         return
      end if
      if (.not. have_att) then
         cbuf = 'cmo/addatt/'//mo(1:icharlnf(mo))//'/'// &
                aname(1:icharlnf(aname))// &
                '/VDOUBLE/scalar/nnodes/linear/permanent/agfx/0.0'// &
                ' ; finish'
         call dotaskx3d(cbuf, ierr)
         if (ierr /= 0) then
            call fail('dump/control_volume: cannot make attribute '// &
                      aname(1:icharlnf(aname)))
            return
         end if
      end if
      call cmo_get_info(aname, mo, ipvatt, ilen, ityp, ierr)
      if (ierr /= 0 .or. ilen < nnodes) then
         call fail('dump/control_volume: cannot reach attribute '// &
                   aname(1:icharlnf(aname)))
         return
      end if
      vatt(1:nnodes) = vol
      write (logmess, '(a,a,a,i0,a)') 'dump/control_volume: control '// &
         'volumes in attribute ', aname(1:icharlnf(aname)), ' (', &
         nnodes, ' vertices)'
      call to_writloga(trim(logmess))
   end if

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

   ! LaGriT's ERROR line, ierror_return 1.
   subroutine fail(why)
      character(len=*), intent(in) :: why
      call say('ERROR: '//why)
      ierror_return = 1
   end subroutine fail

end subroutine dump_control_volume
