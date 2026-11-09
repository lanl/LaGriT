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
! cv         voronoi            voronoi   control volumes
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
   end type options_t

   character(len=*), parameter :: option_keywords = &
      'avs stor uge tough2 zero_area zero_tol compress cv type o'

contains

   ! Parse tokens(1:n). ierr = 0 on success; otherwise errmsg names
   ! the token and nothing should be written.
   subroutine parse_options(tokens, n, opt, ierr)
      character(len=*), intent(in) :: tokens(:)
      integer, intent(in) :: n
      type(options_t), intent(inout) :: opt
      integer, intent(out) :: ierr

      character(len=lpath) :: key, val, legacy_type, legacy_file
      integer :: i, ios
      real(rk) :: x

      ierr = 0
      legacy_type = ' '
      legacy_file = ' '
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
               'zero_tol', 'cv', 'type', 'o')
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
         case ('cv')
            if (trim(lowercase(val)) /= 'voronoi') then
               call bad('cv '//trim(val)// &
                        ': only voronoi for now')
               return
            end if
            opt%cv = 'voronoi'
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

      if (opt%nout == 0) call add_output(opt, 'stor', &
                                         'voronoi.stor', ierr)

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
