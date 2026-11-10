!-----------------------------------------------------------------------
! del_vor: Voronoi control volumes and geometric coefficients from a
! Delaunay triangle or tet mesh. Standalone driver.
!
!   del_vor -avs MESH.inp [-stor FILE] [-uge FILE] [-tough2 FILE]
!           [-zero_area remove|keep] [-zero_tol X]
!           [-compress on|off] [-cv voronoi]
!           [-delaunay_tol X] [-delaunay_fatal X] [-violations FILE]
!           [-check_only on|off] [-report FILE] [-timing on|off]
!
! Options are keyword / value pairs in any order (del_vor_options
! lists them, with their defaults); the LaGriT command will take the
! same keywords. lanl/voronoi's -type, -o and -compress also work.
!
! Order of work: read the mesh; build jtet and check the mesh
! (del_vor_validity: fatal); check Delaunay (del_vor_delaunay: fatal
! for Voronoi control volumes); write the violations file if any facet
! was flagged; stop here with check_only on; else compute and write
! the coefficient files. The summary goes to the screen (and to the
! report file). Exit status 0 on success, 1 on any error; after an
! error no coefficient file is written.
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
   use del_vor_report, only: report_run, report_delaunay
   use del_vor_jtet, only: build_jtet, jtet_stats_t, default_mbndry
   use del_vor_validity, only: check_validity, validity_t
   use del_vor_delaunay, only: check_delaunay, delaunay_t
   use del_vor_write_vtu, only: write_violations_vtu
   use del_vor_log, only: log_copy_open, log_copy_close
   use del_vor_write_stor, only: write_stor
   use del_vor_write_uge, only: write_uge
   use del_vor_write_tough, only: write_tough
   use del_vor_timing, only: timer_start, timer_stop, timer_report, &
                             timing_on
   implicit none

   type(options_t) :: opt
   type(mesh_t) :: m
   type(assembly_stats_t) :: stats
   type(jtet_stats_t) :: jst
   type(validity_t) :: vst
   type(delaunay_t) :: dst
   logical :: not_delaunay
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
      if (arg(1:1) == '-' .and. is_keyword(arg(2:))) &
         tok(ia) = arg(2:)
   end do
   call parse_options(tok, nargs, opt, ierr)
   if (ierr /= 0) then
      call log_line(errmsg)
      call log_line('del_vor: keywords are')
      call log_line('  '//option_keywords)
      call log_line('del_vor: del_vor -help lists them')
      stop 1
   end if
   timing_on = opt%timing
   if (len_trim(opt%report) > 0) then
      call log_copy_open(opt%report, ierr)
      if (ierr /= 0) call fail_stop()
   end if
   if (len_trim(opt%mesh) == 0) then
      call log_line('del_vor: no mesh given (-avs MESH.inp)')
      call log_line('del_vor: del_vor -help lists the options')
      stop 1
   end if

   call log_line('del_vor: reading '//trim(opt%mesh))
   call timer_start('read')
   call read_avs(opt%mesh, m, ierr)
   call timer_stop()
   if (ierr /= 0) call fail_stop()
   ndim = 3
   if (m%nen == 3) ndim = 2

   ! neighbors and the mesh checks: fatal in every mode
   m%mbndry = default_mbndry(m%nen, m%nelements)
   call timer_start('jtet')
   allocate (m%jtetoff(m%nelements), m%jtet(m%nen*m%nelements))
   call build_jtet(m%nnodes, m%nelements, m%nen, m%itetoff, m%itet, &
                   m%itetclr, m%mbndry, m%jtetoff, m%jtet, jst)
   call timer_stop()
   call timer_start('mesh checks')
   call check_validity(m%nnodes, m%nelements, m%nen, m%xic, m%yic, &
                       m%zic, m%itetoff, m%itet, m%itetclr, jst, vst)
   call timer_stop()
   if (vst%nfatal > 0) then
      errmsg = 'del_vor: the mesh failed its checks (above)'
      call fail_stop()
   end if

   ! the Delaunay check: fatal for Voronoi control volumes
   call timer_start('Delaunay check')
   call check_delaunay(m%nnodes, m%nelements, m%nen, m%xic, m%yic, &
                       m%zic, m%itetoff, m%itet, m%itetclr, m%jtetoff, &
                       m%jtet, m%mbndry, opt%delaunay_tol, &
                       opt%delaunay_fatal, dst)
   call report_delaunay(m%nen, dst, m%itetclr, not_delaunay)
   call timer_stop()
   if (dst%rec%n > 0) then
      call timer_start('violations file')
      call write_violations_vtu(opt%violations, m%nen, m%nnodes, &
                                m%xic, m%yic, m%zic, dst%rec, ierr)
      call timer_stop()
      if (ierr /= 0) call fail_stop()
      call log_line('del_vor: wrote flagged facets to '// &
                    trim(opt%violations))
   end if
   if (not_delaunay) then
      errmsg = 'del_vor: not Delaunay; nothing else is written'
      call fail_stop()
   end if
   if (opt%check_only) then
      call log_line('del_vor: check_only on: no coefficient files')
      call timer_report()
      call log_copy_close()
      stop
   end if

   call timer_start('edge CSR')
   call build_edge_csr(m%nnodes, m%nelements, m%nen, m%itetoff, &
                       m%itet, iedge_off, jedge, nedges, ierr)
   call timer_stop()
   if (ierr /= 0) call fail_stop()

   call timer_start('assembly')
   allocate (area(nedges), vol(m%nnodes))
   call assemble(policy_voronoi, m%nnodes, m%nelements, m%nen, &
                 m%xic, m%yic, m%zic, m%itetoff, m%itet, iedge_off, &
                 jedge, area, vol, stats, ierr)
   call timer_stop()
   if (ierr /= 0) call fail_stop()

   call timer_start('zero-area mask')
   allocate (zero(nedges), skip(nedges))
   call zero_area_mask(m%nen, m%nnodes, m%xic, m%yic, m%zic, &
                       iedge_off, jedge, area, opt%zero_tol, zero)
   skip = zero .and. .not. opt%zero_keep
   call timer_stop()

   call timer_start('summary')
   call report_run(ndim, m%nnodes, m%nelements, m%xic, m%yic, m%zic, &
                   iedge_off, jedge, area, vol, stats, zero, &
                   opt%zero_keep, opt%zero_tol)
   call timer_stop()

   do k = 1, opt%nout
      call timer_start('write '//trim(opt%out_type(k)))
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
      call timer_stop()
      if (ierr /= 0) call fail_stop()
      call log_line('del_vor: wrote '//trim(opt%out_type(k))//' '// &
                    trim(opt%out_file(k)))
   end do
   call timer_report()
   call log_copy_close()

contains

   logical function is_keyword(s)
      character(len=*), intent(in) :: s
      select case (trim(s))
      case ('avs', 'stor', 'uge', 'tough2', 'zero_area', 'zero_tol', &
            'compress', 'cv', 'type', 'o', 'delaunay_tol', &
            'violations', 'check_only', 'report', 'timing', &
            'delaunay_fatal')
         is_keyword = .true.
      case default
         is_keyword = .false.
      end select
   end function is_keyword

   ! Exit status 1 with the message; no output file has been opened
   ! before an input or option error.
   subroutine fail_stop()
      if (len_trim(errmsg) > 0) call log_line(errmsg)
      call log_copy_close()
      stop 1
   end subroutine fail_stop

   subroutine usage(istat)
      integer, intent(in) :: istat
      call log_line('usage: del_vor -avs MESH.inp [-stor FILE] '// &
                    '[-uge FILE] [-tough2 FILE]')
      call log_line('               [-zero_area remove|keep] '// &
                    '[-zero_tol X] [-compress on|off]')
      call log_line('               [-cv voronoi] '// &
                    '[-delaunay_tol X] [-delaunay_fatal X]')
      call log_line('               [-violations FILE] '// &
                    '[-check_only on|off] [-report FILE]')
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
      call log_line('  -delaunay_tol X   a facet is not Delaunay '// &
                    'when delta/L < -X (default 1e-10)')
      call log_line('  -delaunay_fatal X down to delta/L = -X: a '// &
                    'warning; beyond: an error (1e-3)')
      call log_line('  -violations FILE  flagged facets for '// &
                    'ParaView (default violations.vtu)')
      call log_line('  -check_only on    check the mesh only; no '// &
                    'coefficient files')
      call log_line('  -report FILE      a copy of the screen '// &
                    'summary')
      call log_line('  lanl/voronoi options -type fehm|pflotran|'// &
                    'tough2 and -o FILE also work.')
      call log_line('  development: -timing on prints the time of '// &
                    'each step and peak memory.')
      if (istat == 0) stop
      stop 1
   end subroutine usage

end program del_vor
