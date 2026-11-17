!-----------------------------------------------------------------------
! del_vor_entry: the LaGriT entry point, del_vor_lagrit_entry, an
! external subroutine (no module) so that LaGriT's fixed-form code
! calls it with no interface block:
!
!   call del_vor_lagrit_entry(nnodes, nelements, nen, xic, yic, zic,
!  *     itettyp, itetoff, itet, itetclr, imt1, itp1, isn1,
!  *     options, vol, ierr)
!
! The arguments are a LaGriT mesh object's own arrays, used in place
! (never copied, never changed). Integers are 8 bytes (LaGriT builds
! with -fdefault-integer-8), reals 8 bytes.
!
!   nnodes, nelements  vertices and cells
!   nen                vertices per cell: 3 (all triangles) or 4 (all
!                      tets)
!   xic, yic, zic      coordinates (nnodes)
!   itettyp            cell types (nelements): all 3 or all 5
!   itetoff, itet      itet(itetoff(it)+j): vertex j of cell it;
!                      itetoff(it) must be nen*(it-1)
!   itetclr            cell material (fracture) numbers
!   imt1, itp1, isn1   vertex material, type, parent/child link; a
!                      vertex with itp1 = 41 (a parent) is refused
!   options            the program's keywords and values in one
!                      string, separated by blanks, commas or '/', a
!                      keyword's dash optional: 'stor, a.stor /
!                      zero_area, keep'. Same parser, defaults and
!                      messages as the program; no avs (the mesh is
!                      the arguments). jtet is built inside.
!   vol                out: the control volumes (nnodes); unchanged
!                      with check_only on or after an error
!   ierr               out: 0, or 1 after an error, whose message is
!                      already in the log; del_vor never stops
!
! Every line goes through del_vor_log (log_set_sink sends it to
! LaGriT's log). The .stor's second line ends with (del_vor OPTIONS).
!-----------------------------------------------------------------------
subroutine del_vor_lagrit_entry(nnodes, nelements, nen, xic, yic, &
                                zic, itettyp, itetoff, itet, itetclr, &
                                imt1, itp1, isn1, options, vol, ierr)
   use del_vor_kinds, only: ik, rk
   use del_vor_log, only: log_line, errmsg
   use del_vor_options, only: options_t, parse_options, &
                              option_keywords, all_settings, is_keyword
   use del_vor_mesh, only: mesh_t, mesh_free, ifelmtri, ifelmtet
   use del_vor_run, only: run_start, run_mesh, run_finish, &
                          run_abandon, log_options
   implicit none
   integer(ik), intent(in) :: nnodes, nelements, nen
   real(rk), target :: xic(nnodes), yic(nnodes), zic(nnodes)
   integer(ik), target :: itettyp(nelements), itetoff(nelements)
   integer(ik), target :: itet(nen*nelements), itetclr(nelements)
   integer(ik), target :: imt1(nnodes), itp1(nnodes), isn1(nnodes)
   character(len=*), intent(in) :: options
   real(rk), intent(inout) :: vol(nnodes)
   integer(ik), intent(out) :: ierr

   type(options_t) :: opt
   type(mesh_t) :: m
   real(rk), allocatable :: v(:)
   character(len=1024), allocatable :: tok(:)
   integer :: ntok, ie
   integer(ik) :: it, ityp

   ierr = 0
   errmsg = ' '

   ! the options: words, the dash of a keyword dropped
   call split_words(options, tok, ntok)
   call parse_options(tok, ntok, opt, ie)
   if (ie == 0 .and. len_trim(opt%mesh) > 0) then
      ie = 1
      errmsg = 'del_vor: avs: not here (the mesh is the mesh object)'
   end if
   if (ie /= 0) then
      call log_line(errmsg)
      call log_line('del_vor: keywords are')
      call log_line('  '//option_keywords)
      ierr = 1
      return
   end if

   ! the mesh: all triangles or all tets, LaGriT's usual layout
   if (nnodes < 1 .or. nelements < 1) then
      call refuse('del_vor: the mesh has no vertices or no cells')
      return
   end if
   if (nen == 3) then
      ityp = ifelmtri
   else if (nen == 4) then
      ityp = ifelmtet
   else
      call refuse('del_vor: triangles or tets only (nen is not 3 '// &
                  'or 4)')
      return
   end if
   do it = 1, nelements
      if (itettyp(it) /= ityp) then
         call refuse('del_vor: mixed cell types; all triangles or '// &
                     'all tets')
         return
      end if
      if (itetoff(it) /= nen*(it - 1)) then
         call refuse('del_vor: itetoff is not nen*(cell-1); '// &
                     'compress the mesh first')
         return
      end if
   end do
   if (any(itet < 1) .or. any(itet > nnodes)) then
      call refuse('del_vor: a cell refers to a vertex that does '// &
                  'not exist')
      return
   end if

   call run_start(opt, ie)
   if (ie /= 0) then
      ierr = 1
      return
   end if
   call log_options(all_settings(opt))

   m%own_input = .false.
   m%nnodes = nnodes
   m%nelements = nelements
   m%nen = nen
   m%xic => xic
   m%yic => yic
   m%zic => zic
   m%itettyp => itettyp
   m%itetoff => itetoff
   m%itet => itet
   m%itetclr => itetclr
   m%imt1 => imt1
   m%itp1 => itp1
   m%isn1 => isn1

   call run_mesh(m, opt, 'del_vor '//trim(adjustl(options)), v, ie)
   if (ie /= 0) then
      ierr = 1
   else
      if (allocated(v)) vol = v
      call run_finish()
   end if
   call mesh_free(m)

contains

   ! An error before the run: its message, nothing written.
   subroutine refuse(why)
      character(len=*), intent(in) :: why
      errmsg = why
      call run_abandon()
      ierr = 1
   end subroutine refuse

   ! The words of s, separated by blanks, tabs, commas and '/'; a word
   ! -keyword loses its dash (a value keeps it: -1e-3, -file).
   subroutine split_words(s, w, n)
      character(len=*), intent(in) :: s
      character(len=1024), allocatable, intent(out) :: w(:)
      integer, intent(out) :: n
      integer :: i, i0, pass
      logical :: sep
      do pass = 1, 2
         n = 0
         i0 = 0
         do i = 1, len(s) + 1
            sep = .true.
            if (i <= len(s)) sep = index(' ,/'//achar(9), s(i:i)) > 0
            if (.not. sep .and. i0 == 0) i0 = i
            if (sep .and. i0 > 0) then
               n = n + 1
               if (pass == 2) then
                  w(n) = s(i0:i - 1)
                  if (w(n)(1:1) == '-') then
                     if (is_keyword(w(n)(2:))) w(n) = w(n)(2:)
                  end if
               end if
               i0 = 0
            end if
         end do
         if (pass == 1) allocate (w(max(n, 1)))
      end do
   end subroutine split_words

end subroutine del_vor_lagrit_entry
