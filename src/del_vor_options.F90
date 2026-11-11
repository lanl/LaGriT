!-----------------------------------------------------------------------
! del_vor_options: the run options, as keyword / value pairs.
!
! One keyword set for every front end. The standalone program passes
! its command line with the leading '-' removed; the LaGriT command
! will pass its tokens. Pairs may come in any order; the last one
! given wins (outputs add up). Written in LaGriT decks as
!
!   del_vor / stor, out.stor / zero_area, keep / compress, off
!
! keyword    value              default   meaning
! stor       file               (*)       FEHM .stor
! uge        file                         PFLOTRAN .uge
! tough2     file                         TOUGH2 MESH
! zero_area  remove | keep      remove    remove: zero-area connections
!                                         are left out of every file;
!                                         keep: the matrix has one
!                                         connection per mesh edge
! zero_tol   number >= 0        1e-12     a connection is zero-area if
!                                         |A| <= zero_tol d^2 (tets),
!                                         |A| <= zero_tol d (triangles)
! compress   on | off           on        .stor: each distinct A/d
!                                         value written once
! cv         voronoi | median | voronoi   where each cell's dual
!            blend | hybrid                points go (del_vor_policy):
!                                         circumcenters; centroids;
!                                         one alpha everywhere; per
!                                         cell, toward the centroid
!                                         only as far as needed
! alpha      number in [0, 1]             cv blend: 1 = Voronoi,
!                                         0 = median (required there,
!                                         refused with other cv)
! hybrid_tol number >= 0        0         how negative a coefficient
!                                         may stay, relative to the
!                                         sum of |its pieces|
! alpha_min  number in [0, 1]   0         the lowest alpha the hybrid
!                                         may give a cell
! hybrid_iter integer >= 1      100       limit on stage B passes
!            (hybrid_tol, alpha_min, hybrid_iter: cv hybrid only)
! alpha_file file                         ParaView file of the mesh
!                                         with alpha and stage per
!                                         cell (del_vor_write_vtu)
! delaunay_tol number >= 0      1e-10     a facet violates Delaunay
!                                         when delta/L < -delaunay_tol
!                                         (del_vor_delaunay)
! delaunay_fatal number >= 0    1e-2      slightly not Delaunay
!                                         (-delaunay_fatal <= delta/L
!                                         < -delaunay_tol) is a
!                                         warning; beyond it, fatal
!                                         (raised to delaunay_tol if
!                                         below it)
! violations file               violations.vtu  the flagged facets,
!                                         written when there are any
! check_only on | off           off       on: check the mesh and stop;
!                                         no coefficient files
! report     file                         a copy of the screen summary
! timing     on | off           off       development only: the time
!                                         of each step and peak memory
!                                         (del_vor_timing)
!
! (*) with no output keyword: stor voronoi.stor.
!
! lanl/voronoi's options are also accepted, as before:
!   type fehm|pflotran|tough2, o FILE (one output), compress with no
!   value (= on).
!-----------------------------------------------------------------------
module del_vor_options
   use del_vor_kinds, only: rk
   use del_vor_log, only: errmsg
   implicit none
   private
   public :: options_t, parse_options, option_keywords, lowercase

   integer, parameter :: lpath = 1024
   integer, parameter :: maxout = 16

   type :: options_t
      character(len=lpath) :: mesh = ' '
      integer :: nout = 0
      character(len=8) :: out_type(maxout) = ' '
      character(len=lpath) :: out_file(maxout) = ' '
      logical :: zero_keep = .false.
      real(rk) :: zero_tol = 1.0e-12_rk
      logical :: compress = .true.
      character(len=16) :: cv = 'voronoi'
      real(rk) :: alpha = -1.0_rk          ! < 0: not given
      real(rk) :: hybrid_tol = 0.0_rk
      real(rk) :: alpha_min = 0.0_rk
      integer :: hybrid_iter = 100
      character(len=lpath) :: alpha_file = ' '
      real(rk) :: delaunay_tol = 1.0e-10_rk
      real(rk) :: delaunay_fatal = 1.0e-2_rk
      character(len=lpath) :: violations = 'violations.vtu'
      logical :: check_only = .false.
      character(len=lpath) :: report = ' '
      logical :: timing = .false.
   end type options_t

   character(len=*), parameter :: option_keywords = &
      'avs stor uge tough2 zero_area zero_tol compress cv '// &
      'delaunay_tol delaunay_fatal violations check_only report '// &
      'timing alpha hybrid_tol alpha_min hybrid_iter alpha_file '// &
      'type o'

contains

   ! Parse tokens(1:n). ierr = 0 on success; otherwise errmsg names
   ! the token and nothing should be written.
   subroutine parse_options(tokens, n, opt, ierr)
      character(len=*), intent(in) :: tokens(:)
      integer, intent(in) :: n
      type(options_t), intent(inout) :: opt
      integer, intent(out) :: ierr

      character(len=lpath) :: key, val, legacy_type, legacy_file
      integer :: i, ios, j
      real(rk) :: x
      character(len=16) :: hyb_key

      ierr = 0
      legacy_type = ' '
      legacy_file = ' '
      hyb_key = ' '
      i = 1
      do while (i <= n)
         key = lowercase(adjustl(tokens(i)))
         select case (trim(key))
         case ('compress')
            ! value optional, for lanl/voronoi's bare -compress
            opt%compress = .true.
            if (i < n) then
               val = lowercase(adjustl(tokens(i + 1)))
               if (trim(val) == 'on') then
                  i = i + 1
               else if (trim(val) == 'off') then
                  opt%compress = .false.
                  i = i + 1
               end if
            end if
            i = i + 1
            cycle
         case ('avs', 'stor', 'uge', 'tough2', 'zero_area', &
               'zero_tol', 'cv', 'type', 'o', 'delaunay_tol', &
               'violations', 'check_only', 'report', 'timing', &
               'delaunay_fatal', 'alpha', 'hybrid_tol', 'alpha_min', &
               'hybrid_iter', 'alpha_file')
            if (i == n) then
               call bad('keyword '//trim(key)//' needs a value')
               return
            end if
            val = adjustl(tokens(i + 1))
         case default
            call bad('unknown keyword '//trim(tokens(i)))
            return
         end select

         select case (trim(key))
         case ('avs')
            opt%mesh = val
         case ('stor', 'uge', 'tough2')
            call add_output(opt, trim(key), val, ierr)
            if (ierr /= 0) return
         case ('zero_area')
            select case (trim(lowercase(val)))
            case ('remove')
               opt%zero_keep = .false.
            case ('keep')
               opt%zero_keep = .true.
            case default
               call bad('zero_area '//trim(val)// &
                        ': must be remove or keep')
               return
            end select
         case ('zero_tol')
            read (val, *, iostat=ios) x
            if (ios /= 0 .or. x < 0.0_rk) then
               call bad('zero_tol '//trim(val)// &
                        ': must be a number >= 0')
               return
            end if
            opt%zero_tol = x
         case ('delaunay_tol')
            read (val, *, iostat=ios) x
            if (ios /= 0 .or. x < 0.0_rk) then
               call bad('delaunay_tol '//trim(val)// &
                        ': must be a number >= 0')
               return
            end if
            opt%delaunay_tol = x
         case ('delaunay_fatal')
            read (val, *, iostat=ios) x
            if (ios /= 0 .or. x < 0.0_rk) then
               call bad('delaunay_fatal '//trim(val)// &
                        ': must be a number >= 0')
               return
            end if
            opt%delaunay_fatal = x
         case ('violations')
            opt%violations = val
         case ('report')
            opt%report = val
         case ('check_only')
            select case (trim(lowercase(val)))
            case ('on')
               opt%check_only = .true.
            case ('off')
               opt%check_only = .false.
            case default
               call bad('check_only '//trim(val)// &
                        ': must be on or off')
               return
            end select
         case ('timing')
            select case (trim(lowercase(val)))
            case ('on')
               opt%timing = .true.
            case ('off')
               opt%timing = .false.
            case default
               call bad('timing '//trim(val)//': must be on or off')
               return
            end select
         case ('cv')
            select case (trim(lowercase(val)))
            case ('voronoi', 'median', 'blend', 'hybrid')
               opt%cv = trim(lowercase(val))
            case default
               call bad('cv '//trim(val)// &
                        ': must be voronoi, median, blend or hybrid')
               return
            end select
         case ('alpha')
            read (val, *, iostat=ios) x
            if (ios /= 0 .or. x < 0.0_rk .or. x > 1.0_rk) then
               call bad('alpha '//trim(val)// &
                        ': must be a number from 0 to 1')
               return
            end if
            opt%alpha = x
         case ('hybrid_tol')
            hyb_key = 'hybrid_tol'
            read (val, *, iostat=ios) x
            if (ios /= 0 .or. x < 0.0_rk) then
               call bad('hybrid_tol '//trim(val)// &
                        ': must be a number >= 0')
               return
            end if
            opt%hybrid_tol = x
         case ('alpha_min')
            hyb_key = 'alpha_min'
            read (val, *, iostat=ios) x
            if (ios /= 0 .or. x < 0.0_rk .or. x > 1.0_rk) then
               call bad('alpha_min '//trim(val)// &
                        ': must be a number from 0 to 1')
               return
            end if
            opt%alpha_min = x
         case ('hybrid_iter')
            hyb_key = 'hybrid_iter'
            read (val, *, iostat=ios) j
            if (ios /= 0 .or. j < 1 .or. &
                verify(trim(val), '0123456789') /= 0) then
               call bad('hybrid_iter '//trim(val)// &
                        ': must be a whole number >= 1')
               return
            end if
            opt%hybrid_iter = j
         case ('alpha_file')
            opt%alpha_file = val
         case ('type')
            select case (lowercase(val(1:4)))
            case ('fehm')
               legacy_type = 'stor'
            case ('pflo')
               legacy_type = 'uge'
            case ('toug')
               legacy_type = 'tough2'
            case default
               call bad('type '//trim(val)// &
                        ': must be fehm, pflotran or tough2')
               return
            end select
         case ('o')
            legacy_file = val
         end select
         i = i + 2
      end do

      ! lanl/voronoi style: -type T -o FILE (either may be left out)
      if (len_trim(legacy_type) > 0 .or. len_trim(legacy_file) > 0) &
         then
         if (len_trim(legacy_type) == 0) legacy_type = 'stor'
         if (len_trim(legacy_file) == 0) then
            select case (trim(legacy_type))
            case ('stor')
               legacy_file = 'voronoi.stor'
            case ('uge')
               legacy_file = 'pflotran.uge'
            case ('tough2')
               legacy_file = 'MESH_VORONOI'
            end select
         end if
         call add_output(opt, trim(legacy_type), legacy_file, ierr)
         if (ierr /= 0) return
      end if

      ! alpha belongs to cv blend, and cv blend needs it
      if (trim(opt%cv) == 'blend' .and. opt%alpha < 0.0_rk) then
         call bad('cv blend needs alpha (a number from 0 to 1)')
         return
      end if
      if (trim(opt%cv) /= 'blend' .and. opt%alpha >= 0.0_rk) then
         call bad('alpha is for cv blend (here cv is '// &
                  trim(opt%cv)//')')
         return
      end if

      if (trim(opt%cv) /= 'hybrid' .and. len_trim(hyb_key) > 0) then
         call bad(trim(hyb_key)//' is for cv hybrid (here cv is '// &
                  trim(opt%cv)//')')
         return
      end if

      if (opt%nout == 0) call add_output(opt, 'stor', &
                                         'voronoi.stor', ierr)
      if (ierr /= 0) return
      ! a delaunay_fatal below delaunay_tol is raised to it
      ! (del_vor_delaunay), so raising delaunay_tol alone is enough
      call check_names(opt, ierr)

   contains

      subroutine bad(why)
         character(len=*), intent(in) :: why
         ierr = 1
         errmsg = 'del_vor: '//why
      end subroutine bad

   end subroutine parse_options

   subroutine add_output(opt, otype, ofile, ierr)
      type(options_t), intent(inout) :: opt
      character(len=*), intent(in) :: otype, ofile
      integer, intent(out) :: ierr
      integer :: k
      ierr = 0
      do k = 1, opt%nout
         if (trim(opt%out_file(k)) == trim(ofile)) then
            ierr = 1
            errmsg = 'del_vor: output file '//trim(ofile)// &
                     ' given twice'
            return
         end if
      end do
      if (opt%nout == maxout) then
         ierr = 1
         errmsg = 'del_vor: too many output files'
         return
      end if
      opt%nout = opt%nout + 1
      opt%out_type(opt%nout) = otype
      opt%out_file(opt%nout) = ofile
   end subroutine add_output

   subroutine check_names(opt, ierr)
!     Every file del_vor writes (outputs, report, violations file)
!     differs from the others and from the mesh.
      type(options_t), intent(in) :: opt
      integer, intent(out) :: ierr
      character(len=lpath) :: f(maxout+3)
      integer :: n, k, j
      ierr = 0
      n = opt%nout
      f(1:n) = opt%out_file(1:n)
      n = n + 1
      f(n) = opt%violations
      if (len_trim(opt%report) > 0) then
         n = n + 1
         f(n) = opt%report
      end if
      if (len_trim(opt%alpha_file) > 0) then
         n = n + 1
         f(n) = opt%alpha_file
      end if
      do k = 1, n
         if (trim(f(k)) == trim(opt%mesh)) then
            ierr = 1
            errmsg = 'del_vor: '//trim(f(k))// &
                     ' is the mesh; it would be overwritten'
            return
         end if
         do j = 1, k - 1
            if (trim(f(j)) == trim(f(k))) then
               ierr = 1
               errmsg = 'del_vor: output file '//trim(f(k))// &
                        ' given twice'
               return
            end if
         end do
      end do
   end subroutine check_names

   function lowercase(s) result(t)
      character(len=*), intent(in) :: s
      character(len=len(s)) :: t
      integer :: i, ic
      t = s
      do i = 1, len(s)
         ic = iachar(s(i:i))
         if (ic >= 65 .and. ic <= 90) t(i:i) = achar(ic + 32)
      end do
   end function lowercase

end module del_vor_options
