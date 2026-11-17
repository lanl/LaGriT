!-----------------------------------------------------------------------
! del_vor: Voronoi control volumes and geometric coefficients from a
! Delaunay triangle or tet mesh. Standalone driver.
!
!   del_vor -avs MESH.inp [-stor FILE] [-uge FILE] [-tough2 FILE]
!           [-zero_area remove|keep] [-zero_tol X]
!           [-compress on|off] [-cv voronoi|median|blend|hybrid]
!           [-alpha X] [-hybrid_tol X] [-alpha_min X] [-hybrid_iter N]
!           [-alpha_file FILE] [-paraview ROOT] [-paraview_faces on|off]
!           [-paraview_subset all|boundary|flagged|material N|box ...]
!           [-vtu_format binary|ascii] [-vtu_layout new|legacy]
!           [-matrix FILE] [-matrix_bins N]
!           [-delaunay_tol X] [-delaunay_fatal X] [-violations FILE]
!           [-check_only on|off] [-report FILE] [-timing on|off]
!
! Options are keyword / value pairs in any order (del_vor_options
! lists them, with their defaults); the LaGriT command will take the
! same keywords. lanl/voronoi's -type, -o and -compress also work.
!
! This program parses the options, reads the AVS file and calls
! del_vor_run (the library the LaGriT entry point calls too), which
! does the work; its header lists the order. The summary goes to the
! screen (and to the report file). Exit status 0 on success, 1 on any
! error; after an error no coefficient file is written.
!-----------------------------------------------------------------------
program del_vor
   use del_vor_kinds, only: rk
   use del_vor_log, only: log_line, errmsg
   use del_vor_options, only: options_t, parse_options, &
                              option_keywords, all_settings, is_keyword
   use del_vor_mesh, only: mesh_t, mesh_free
   use del_vor_read_avs, only: read_avs
   use del_vor_run, only: run_start, run_mesh, run_finish, &
                          run_abandon, log_options
   use del_vor_timing, only: timer_start, timer_stop
   implicit none

   type(options_t) :: opt
   type(mesh_t) :: m
   real(rk), allocatable :: vol(:)
   integer :: ierr, nargs, ia
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
   call run_start(opt, ierr)
   if (ierr /= 0) stop 1
   if (len_trim(opt%mesh) == 0) then
      call log_line('del_vor: no mesh given (-avs MESH.inp)')
      call log_line('del_vor: del_vor -help lists the options')
      stop 1
   end if

   call log_options(all_settings(opt))
   call log_line('del_vor: reading '//trim(opt%mesh))
   call timer_start('read')
   call read_avs(opt%mesh, m, ierr)
   call timer_stop()
   if (ierr /= 0) then
      call run_abandon()
      stop 1
   end if

   call run_mesh(m, opt, command_line(), vol, ierr)
   if (ierr /= 0) stop 1
   call run_finish()
   call mesh_free(m)

contains

   ! The command as typed, for the .stor's second line:
   ! "del_vor -avs mesh.inp -stor mesh.stor ...". An argument holding
   ! a blank, a quote or a parenthesis is put in single quotes (a
   ! quote in it as '\''), as a shell would need it.
   function command_line() result(t)
      character(len=:), allocatable :: t
      character(len=1024) :: a
      integer :: ia, k
      t = 'del_vor'
      do ia = 1, command_argument_count()
         call get_command_argument(ia, a)
         if (scan(trim(a), " '""()") == 0 .and. len_trim(a) > 0) then
            t = t//' '//trim(a)
         else
            t = t//" '"
            do k = 1, len_trim(a)
               if (a(k:k) == "'") then
                  t = t//"'\''"
               else
                  t = t//a(k:k)
               end if
            end do
            t = t//"'"
         end if
      end do
   end function command_line

   subroutine usage(istat)
      integer, intent(in) :: istat
      call log_line('usage: del_vor -avs MESH.inp [-stor FILE] '// &
                    '[-uge FILE] [-tough2 FILE]')
      call log_line('               [-zero_area remove|keep] '// &
                    '[-zero_tol X] [-compress on|off]')
      call log_line('               [-cv voronoi|median|blend|'// &
                    'hybrid] [-alpha X] [-alpha_file FILE]')
      call log_line('               [-hybrid_tol X] [-alpha_min X] '// &
                    '[-hybrid_iter N]')
      call log_line('               [-delaunay_tol X] '// &
                    '[-delaunay_fatal X]')
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
      call log_line('  -cv POLICY        where the dual points go:'// &
                    ' voronoi (default,')
      call log_line('                    circumcenters), median'// &
                    ' (centroids), blend (alpha')
      call log_line('                    everywhere), hybrid'// &
                    ' (moved toward the centroid only')
      call log_line('                    as far as needed for no'// &
                    ' negative coefficient)')
      call log_line('  -alpha X          cv blend: 1 = Voronoi, 0 '// &
                    '= median')
      call log_line('  -hybrid_tol X     cv hybrid: negative'// &
                    ' allowed, relative (default 0)')
      call log_line('  -alpha_min X      cv hybrid: lowest alpha '// &
                    '(default 0)')
      call log_line('  -hybrid_iter N    cv hybrid: stage B pass '// &
                    'limit (default 100)')
      call log_line('  -alpha_file FILE  ParaView file: alpha and '// &
                    'stage per cell')
      call log_line('  -paraview ROOT    ParaView files: ROOT.pvd, '// &
                    'the mesh, the control')
      call log_line('                    volumes, negative '// &
                    'coefficients')
      call log_line('  -paraview_faces on   also the dual faces '// &
                    '(tets)')
      call log_line('  -paraview_subset S   all, boundary, flagged,'// &
                    ' material N, box x1 y1 z1 x2 y2 z2')
      call log_line('  -matrix FILE      ParaView file of the '// &
                    'matrix: a point per .stor entry')
      call log_line('  -matrix_bins N    also an N x N image of '// &
                    'the matrix (FILE.vti)')
      call log_line('  -vtu_format F     binary (default) or ascii')
      call log_line('  -vtu_layout L     new (ParaView 6, default) '// &
                    'or legacy (ParaView 5)')
      call log_line('  -delaunay_tol X   a facet is not Delaunay '// &
                    'when delta/L < -X (default 1e-10)')
      call log_line('  -delaunay_fatal X down to delta/L = -X: a '// &
                    'warning; beyond: an error (1e-2)')
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
