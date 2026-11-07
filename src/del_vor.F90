!-----------------------------------------------------------------------
! del_vor: Voronoi control volumes and geometric coefficients from a
! Delaunay triangle or tet mesh. Standalone driver.
!
! Phase 1 command line (the lanl/voronoi options):
!
!   del_vor -avs MESH.inp [-type fehm|pflotran|tough2] [-o FILE]
!           [-compress] [-cv voronoi]
!
! Output file defaults, as voronoi: voronoi.stor, pflotran.uge,
! MESH_VORONOI. The run summary goes to the screen. Exit status is 0
! on success, 1 on any error.
!-----------------------------------------------------------------------
program del_vor
   use del_vor_kinds, only: ik, rk
   use del_vor_log, only: log_line, errmsg
   use del_vor_mesh, only: mesh_t
   use del_vor_read_avs, only: read_avs
   use del_vor_topology, only: build_edge_csr
   use del_vor_policy, only: policy_voronoi
   use del_vor_assemble, only: assemble, assembly_stats_t
   use del_vor_report, only: report_run
   use del_vor_write_stor, only: write_stor
   use del_vor_write_uge, only: write_uge
   use del_vor_write_tough, only: write_tough
   implicit none

   type(mesh_t) :: m
   type(assembly_stats_t) :: stats
   integer(ik), allocatable :: iedge_off(:), jedge(:)
   real(rk), allocatable :: area(:), vol(:)
   integer(ik) :: nedges
   integer :: ierr, ndim, nargs, ia
   character(len=1024) :: arg, fin, fout, otype, cv
   logical :: compress

   fin = ' '
   fout = ' '
   otype = 'fehm'
   cv = 'voronoi'
   compress = .false.

   nargs = command_argument_count()
   if (nargs == 0) call usage(0)
   ia = 1
   do while (ia <= nargs)
      call get_command_argument(ia, arg)
      select case (trim(arg))
      case ('-avs')
         call value_of(ia, fin)
      case ('-o')
         call value_of(ia, fout)
      case ('-type')
         call value_of(ia, otype)
      case ('-cv')
         call value_of(ia, cv)
      case ('-compress')
         compress = .true.
      case ('-help', '-h', '--help')
         call usage(0)
      case default
         call log_line('del_vor: unknown option '//trim(arg))
         call usage(1)
      end select
      ia = ia + 1
   end do

   if (len_trim(fin) == 0) then
      call log_line('del_vor: no mesh given (-avs MESH.inp)')
      call usage(1)
   end if
   if (trim(cv) /= 'voronoi') then
      call log_line('del_vor: -cv '//trim(cv)// &
                    ' not available yet; only voronoi')
      call fail_stop()
   end if
   select case (otype(1:4))
   case ('fehm')
      otype = 'fehm'
      if (len_trim(fout) == 0) fout = 'voronoi.stor'
   case ('pflo')
      otype = 'pflotran'
      if (len_trim(fout) == 0) fout = 'pflotran.uge'
   case ('toug')
      otype = 'tough2'
      if (len_trim(fout) == 0) fout = 'MESH_VORONOI'
   case default
      call log_line('del_vor: unknown -type '//trim(otype))
      call fail_stop()
   end select

   call log_line('del_vor: reading '//trim(fin))
   call read_avs(fin, m, ierr)
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

   call report_run(ndim, m%nnodes, m%nelements, m%xic, m%yic, m%zic, &
                   iedge_off, jedge, area, vol, stats)

   select case (trim(otype))
   case ('fehm')
      call write_stor(fout, ndim, m%nnodes, m%xic, m%yic, m%zic, &
                      iedge_off, jedge, area, vol, compress, ierr)
   case ('pflotran')
      call write_uge(fout, m%nnodes, m%xic, m%yic, m%zic, iedge_off, &
                     jedge, area, vol, ierr)
   case ('tough2')
      call write_tough(fout, m%nnodes, m%xic, m%yic, m%zic, m%imt1, &
                       iedge_off, jedge, area, vol, ierr)
   end select
   if (ierr /= 0) call fail_stop()
   call log_line('del_vor: wrote '//trim(fout))

contains

   subroutine value_of(ia, val)
      integer, intent(inout) :: ia
      character(len=*), intent(out) :: val
      character(len=1024) :: opt
      call get_command_argument(ia, opt)
      if (ia + 1 > nargs) then
         call log_line('del_vor: '//trim(opt)//' needs a value')
         call usage(1)
      end if
      ia = ia + 1
      call get_command_argument(ia, val)
   end subroutine value_of

   subroutine fail_stop()
      if (len_trim(errmsg) > 0) call log_line(errmsg)
      error stop 1
   end subroutine fail_stop

   subroutine usage(istat)
      integer, intent(in) :: istat
      call log_line('usage: del_vor -avs MESH.inp '// &
                    '[-type fehm|pflotran|tough2] [-o FILE]')
      call log_line('               [-compress] [-cv voronoi]')
      call log_line('  -avs MESH.inp  AVS UCD mesh, all triangles '// &
                    'or all tets')
      call log_line('  -type          output: fehm (.stor, '// &
                    'default), pflotran (.uge),')
      call log_line('                 tough2 (MESH)')
      call log_line('  -o FILE        output file (default '// &
                    'voronoi.stor, pflotran.uge,')
      call log_line('                 MESH_VORONOI)')
      call log_line('  -compress      .stor: write each distinct '// &
                    'coefficient once')
      call log_line('  -cv voronoi    control volumes (median and '// &
                    'hybrid come later)')
      if (istat == 0) stop
      error stop 1
   end subroutine usage

end program del_vor
