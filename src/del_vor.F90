!-----------------------------------------------------------------------
! del_vor: Voronoi control volumes and geometric coefficients from a
! Delaunay triangle or tet mesh. Standalone driver.
!
!   del_vor -avs MESH.inp [-stor FILE] [-uge FILE] [-tough2 FILE]
!           [-zero_area remove|keep] [-zero_tol X]
!           [-compress on|off] [-cv voronoi]
!
! Options are keyword / value pairs in any order (del_vor_options
! lists them, with their defaults); the LaGriT command will take the
! same keywords. lanl/voronoi's -type, -o and -compress also work.
! The run summary goes to the screen. Exit status is 0 on success,
! 1 on any error (then no output file is written).
!-----------------------------------------------------------------------
program del_vor
   use del_vor_kinds, only: ik, rk
   use del_vor_log, only: log_line, errmsg
   use del_vor_options, only: options_t, parse_options, &
                              option_keywords
   use del_vor_mesh, only: mesh_t
   use del_vor_read_avs, only: read_avs
   use del_vor_topology, only: build_edge_csr
   use del_vor_policy, only: policy_voronoi
   use del_vor_assemble, only: assemble, assembly_stats_t, &
                               zero_area_mask
   use del_vor_report, only: report_run
   use del_vor_write_stor, only: write_stor
   use del_vor_write_uge, only: write_uge
   use del_vor_write_tough, only: write_tough
   implicit none

   type(options_t) :: opt
   type(mesh_t) :: m
   type(assembly_stats_t) :: stats
   integer(ik), allocatable :: iedge_off(:), jedge(:)
   real(rk), allocatable :: area(:), vol(:)
   logical, allocatable :: zero(:), skip(:)
   integer(ik) :: nedges
   integer :: ierr, ndim, nargs, ia, k
   character(len=1024), allocatable :: tok(:)
   character(len=1024) :: arg

   nargs = command_argument_count()
   if (nargs == 0) call usage(0)
   allocate (tok(nargs))
   do ia = 1, nargs
      call get_command_argument(ia, arg)
      select case (trim(arg))
      case ('-help', '-h', '--help')
         call usage(0)
      end select
      ! keywords are written -keyword here; values are taken as they
      ! are (a negative number or a file name may start with '-')
      tok(ia) = arg
      if (arg(1:1) == '-' .and. is_keyword(arg(2:))) tok(ia) = arg(2:)
   end do
   call parse_options(tok, nargs, opt, ierr)
   if (ierr /= 0) then
      call log_line(errmsg)
      call log_line('del_vor: keywords are '//option_keywords)
      call log_line('del_vor: del_vor -help lists them')
      stop 1
   end if
   if (len_trim(opt%mesh) == 0) then
      call log_line('del_vor: no mesh given (-avs MESH.inp)')
      call log_line('del_vor: del_vor -help lists the options')
      stop 1
   end if

   call log_line('del_vor: reading '//trim(opt%mesh))
   call read_avs(opt%mesh, m, ierr)
   if (ierr /= 0) call fail_stop()
   ndim = 3
   if (m%nen == 3) ndim = 2

   call build_edge_csr(m%nnodes, m%nelements, m%nen, m%itetoff, &
                       m%itet, iedge_off, jedge, nedges, ierr)
   if (ierr /= 0) call fail_stop()

   allocate (area(nedges), vol(m%nnodes))
   call assemble(policy_voronoi, m%nnodes, m%nelements, m%nen, &
                 m%xic, m%yic, m%zic, m%itetoff, m%itet, iedge_off, &
                 jedge, area, vol, stats, ierr)
   if (ierr /= 0) call fail_stop()

   allocate (zero(nedges), skip(nedges))
   call zero_area_mask(m%nen, m%nnodes, m%xic, m%yic, m%zic, &
                       iedge_off, jedge, area, opt%zero_tol, zero)
   skip = zero .and. .not. opt%zero_keep

   call report_run(ndim, m%nnodes, m%nelements, m%xic, m%yic, m%zic, &
                   iedge_off, jedge, area, vol, stats, zero, &
                   opt%zero_keep, opt%zero_tol)

   do k = 1, opt%nout
      select case (trim(opt%out_type(k)))
      case ('stor')
         call write_stor(opt%out_file(k), ndim, m%nnodes, m%xic, &
                         m%yic, m%zic, iedge_off, jedge, area, vol, &
                         skip, opt%compress, ierr)
      case ('uge')
         call write_uge(opt%out_file(k), m%nnodes, m%xic, m%yic, &
                        m%zic, iedge_off, jedge, area, vol, skip, ierr)
      case ('tough2')
         call write_tough(opt%out_file(k), m%nnodes, m%xic, m%yic, &
                          m%zic, m%imt1, iedge_off, jedge, area, vol, &
                          skip, ierr)
      end select
      if (ierr /= 0) call fail_stop()
      call log_line('del_vor: wrote '//trim(opt%out_type(k))//' '// &
                    trim(opt%out_file(k)))
   end do

contains

   logical function is_keyword(s)
      character(len=*), intent(in) :: s
      select case (trim(s))
      case ('avs', 'stor', 'uge', 'tough2', 'zero_area', 'zero_tol', &
            'compress', 'cv', 'type', 'o')
         is_keyword = .true.
      case default
         is_keyword = .false.
      end select
   end function is_keyword

   ! Exit status 1 with the message; no output file has been opened
   ! before an input or option error.
   subroutine fail_stop()
      if (len_trim(errmsg) > 0) call log_line(errmsg)
      stop 1
   end subroutine fail_stop

   subroutine usage(istat)
      integer, intent(in) :: istat
      call log_line('usage: del_vor -avs MESH.inp [-stor FILE] '// &
                    '[-uge FILE] [-tough2 FILE]')
      call log_line('               [-zero_area remove|keep] '// &
                    '[-zero_tol X] [-compress on|off]')
      call log_line('               [-cv voronoi]')
      call log_line('  -avs MESH.inp     AVS UCD mesh, all '// &
                    'triangles or all tets')
      call log_line('  -stor FILE        FEHM .stor (default '// &
                    'voronoi.stor if no output given)')
      call log_line('  -uge FILE         PFLOTRAN .uge')
      call log_line('  -tough2 FILE      TOUGH2 MESH')
      call log_line('                    (several outputs in one '// &
                    'run are fine)')
      call log_line('  -zero_area MODE   remove (default): leave '// &
                    'zero-area connections out')
      call log_line('                    keep: one connection per '// &
                    'mesh edge')
      call log_line('  -zero_tol X       zero-area: |A| <= X d^2 '// &
                    '(tets), X d (triangles);')
      call log_line('                    default 1e-12')
      call log_line('  -compress on|off  .stor: each distinct '// &
                    'value written once (default on)')
      call log_line('  -cv voronoi       control volumes (median '// &
                    'and hybrid come later)')
      call log_line('  lanl/voronoi options -type fehm|pflotran|'// &
                    'tough2 and -o FILE also work.')
      if (istat == 0) stop
      stop 1
   end subroutine usage

end program del_vor
