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
!           [-delaunay_tol X] [-delaunay_fatal X] [-violations FILE]
!           [-check_only on|off] [-report FILE] [-timing on|off]
!
! Options are keyword / value pairs in any order (del_vor_options
! lists them, with their defaults); the LaGriT command will take the
! same keywords. lanl/voronoi's -type, -o and -compress also work.
!
! Order of work: read the mesh; build jtet and check the mesh
! (del_vor_validity: fatal); check Delaunay (del_vor_delaunay: fatal
! for every cv but median, where it is advisory); write the violations
! file if any facet was flagged; stop here with check_only on; else
! place the dual points (del_vor_policy; the hybrid's stages in
! del_vor_hybrid), compute and write the coefficient files. The
! summary goes to the screen (and to the report file). Exit status 0
! on success, 1 on any error; after an error no coefficient file is
! written.
!-----------------------------------------------------------------------
program del_vor
   use del_vor_kinds, only: ik, rk
   use del_vor_log, only: log_line, errmsg
   use del_vor_options, only: options_t, parse_options, &
                              option_keywords
   use del_vor_mesh, only: mesh_t
   use del_vor_read_avs, only: read_avs
   use del_vor_topology, only: build_edge_csr, edge_index
   use del_vor_policy, only: policy_voronoi, policy_median, &
                             policy_blend, policy_hybrid, policy_code
   use del_vor_assemble, only: assemble, assembly_stats_t, &
                               zero_area_mask, cell_policy_t
   use del_vor_hybrid, only: hybrid_t, stage_a, stage_b, &
                             boundary_edges, is_negative
   use del_vor_report, only: report_run, report_delaunay, &
                             report_policy
   use del_vor_jtet, only: build_jtet, jtet_stats_t, default_mbndry
   use del_vor_validity, only: check_validity, validity_t
   use del_vor_delaunay, only: check_delaunay, delaunay_t
   use del_vor_write_vtu, only: write_violations_vtu, write_alpha_vtu
   use del_vor_vtu, only: vtu_grid_t, write_vtu, write_pvd
   use del_vor_tess, only: build_cells, build_mesh_grid, &
                           build_negative_grid, interface_edges, &
                           boundary_vertices
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
   ! control-volume policy (cv other than voronoi)
   integer :: ipol, nstep
   type(cell_policy_t) :: pol
   type(hybrid_t) :: hs
   integer, allocatable :: stage(:)
   real(rk), allocatable :: asum(:), avec(:, :)
   logical, allocatable :: bedge(:)
   character(len=22) :: step(3)
   integer(ik) :: nb(3), ni(3)
   real(rk) :: rmin(3), angmax, angmean
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
   ipol = policy_code(trim(opt%cv))
   if (not_delaunay) then
      if (ipol /= policy_median) then
         errmsg = 'del_vor: not Delaunay; nothing else is written'
         call fail_stop()
      end if
      call log_line('del_vor: cv median does not need a Delaunay '// &
                    'mesh: going on')
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

   allocate (area(nedges), vol(m%nnodes))
   if (ipol == policy_voronoi) then
      call timer_start('assembly')
      call assemble(m%nnodes, m%nelements, m%nen, m%xic, m%yic, &
                    m%zic, m%itetoff, m%itet, iedge_off, jedge, area, &
                    vol, stats, ierr)
      call timer_stop()
      if (ierr /= 0) call fail_stop()
   else
      call run_policy()
   end if

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
   if (ipol /= policy_voronoi) then
      call angles(angmax, angmean)
      call report_policy(trim(opt%cv), nstep, step, nb, ni, rmin, &
                         pol%alpha, stage, opt%alpha_min, &
                         hs%nstage_a, hs%nstage_b, hs%npass, &
                         hs%stuck, hs%limit, angmax, angmean)
   end if
   call timer_stop()
   if (len_trim(opt%alpha_file) > 0) call alpha_file_out()
   if (len_trim(opt%paraview) > 0) then
      call timer_start('paraview files')
      call paraview_out()
      call timer_stop()
   end if

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

   ! Place the dual points for cv median, blend or hybrid, and
   ! assemble: area, vol, stats, plus asum and avec for the report.
   subroutine run_policy()
      allocate (pol%alpha(m%nelements), stage(m%nelements))
      allocate (asum(nedges), avec(3, nedges), bedge(nedges))
      pol%code = ipol
      stage = 0
      select case (ipol)
      case (policy_median)
         pol%alpha = 0.0_rk
      case (policy_blend)
         pol%alpha = opt%alpha
      case default
         pol%alpha = 1.0_rk
      end select
      call boundary_edges(m%nnodes, m%nelements, m%nen, m%itetoff, &
                          m%itet, m%jtetoff, m%jtet, m%mbndry, &
                          iedge_off, jedge, bedge)
      nstep = 0
      if (ipol == policy_hybrid) then
         ! where Voronoi stands
         call timer_start('assembly')
         call assemble(m%nnodes, m%nelements, m%nen, m%xic, m%yic, &
                       m%zic, m%itetoff, m%itet, iedge_off, jedge, &
                       area, vol, stats, ierr, asum=asum)
         call timer_stop()
         if (ierr /= 0) call fail_stop()
         call tally('Voronoi')
         call timer_start('stage A')
         pol%clamp = .true.
         pol%amin = opt%alpha_min
         call stage_a(m%nnodes, m%nelements, m%nen, m%xic, m%yic, &
                      m%zic, m%itetoff, m%itet, m%jtetoff, m%jtet, &
                      m%mbndry, opt%alpha_min, pol%alpha, stage, hs)
         call assemble(m%nnodes, m%nelements, m%nen, m%xic, m%yic, &
                       m%zic, m%itetoff, m%itet, iedge_off, jedge, &
                       area, vol, stats, ierr, pol=pol, &
                       jtetoff=m%jtetoff, jtet=m%jtet, &
                       mbndry=m%mbndry, asum=asum)
         call timer_stop()
         if (ierr /= 0) call fail_stop()
         call tally('after stage A')
         call timer_start('stage B')
         call stage_b(m%nnodes, m%nelements, m%nen, m%xic, m%yic, &
                      m%zic, m%itetoff, m%itet, m%jtetoff, m%jtet, &
                      m%mbndry, iedge_off, jedge, pol, area, vol, &
                      asum, opt%hybrid_tol, opt%zero_tol, &
                      opt%alpha_min, opt%hybrid_iter, stage, hs, ierr)
         call timer_stop()
         if (ierr /= 0) call fail_stop()
      end if
      ! the final numbers, from one clean assembly
      call timer_start('assembly')
      call assemble(m%nnodes, m%nelements, m%nen, m%xic, m%yic, &
                    m%zic, m%itetoff, m%itet, iedge_off, jedge, area, &
                    vol, stats, ierr, area_vec=avec, pol=pol, &
                    jtetoff=m%jtetoff, jtet=m%jtet, mbndry=m%mbndry, &
                    asum=asum)
      call timer_stop()
      if (ierr /= 0) call fail_stop()
      if (ipol == policy_hybrid) then
         call tally('after stage B')
      else
         call tally('cv '//trim(opt%cv))
      end if
   end subroutine run_policy

   ! One row of the report: negative edges on the boundary and
   ! inside, and the smallest area / sum of |pieces|.
   subroutine tally(label)
      character(len=*), intent(in) :: label
      integer(ik) :: i, kk
      nstep = nstep + 1
      step(nstep) = label
      nb(nstep) = 0
      ni(nstep) = 0
      rmin(nstep) = 0.0_rk
      do i = 1, m%nnodes
         do kk = iedge_off(i), iedge_off(i + 1) - 1
            if (.not. edge_negative(i, kk)) cycle
            if (bedge(kk)) then
               nb(nstep) = nb(nstep) + 1
            else
               ni(nstep) = ni(nstep) + 1
            end if
            rmin(nstep) = min(rmin(nstep), area(kk)/asum(kk))
         end do
      end do
   end subroutine tally

   ! del_vor_hybrid's test, on edge kk of row i
   logical function edge_negative(i, kk)
      integer(ik), intent(in) :: i, kk
      integer(ik) :: j
      j = jedge(kk)
      edge_negative = is_negative(area(kk), asum(kk), &
         [m%xic(i), m%yic(i), m%zic(i)], &
         [m%xic(j), m%yic(j), m%zic(j)], m%nen, opt%hybrid_tol, &
         opt%zero_tol)
   end function edge_negative

   ! The angle between each dual face's vector area and its edge,
   ! over the edges that are not zero-area: largest, and the mean
   ! weighted by |A| (edges of nearly zero area have an angle near
   ! 90 degrees that says little; weighting by area tells how much
   ! of the flux area is tilted). Degrees.
   subroutine angles(amax, amean)
      real(rk), intent(out) :: amax, amean
      integer(ik) :: i, kk, j
      real(rk) :: e(3), cs, an, ang, total, wsum
      real(rk), parameter :: deg = 180.0_rk/acos(-1.0_rk)
      amax = 0.0_rk
      total = 0.0_rk
      wsum = 0.0_rk
      do i = 1, m%nnodes
         do kk = iedge_off(i), iedge_off(i + 1) - 1
            if (zero(kk)) cycle
            an = norm2(avec(:, kk))
            if (an <= 0.0_rk) cycle
            j = jedge(kk)
            e = [m%xic(j) - m%xic(i), m%yic(j) - m%yic(i), &
                 m%zic(j) - m%zic(i)]
            cs = dot_product(avec(:, kk), e)/(an*norm2(e))
            cs = max(-1.0_rk, min(1.0_rk, cs))
            ang = acos(cs)*deg
            amax = max(amax, ang)
            total = total + an*ang
            wsum = wsum + an
         end do
      end do
      amean = 0.0_rk
      if (wsum > 0.0_rk) amean = total/wsum
   end subroutine angles

   ! alpha_file: the mesh with alpha, stage and the cells next to an
   ! edge still negative.
   subroutine alpha_file_out()
      logical, allocatable :: neg(:)
      real(rk), allocatable :: a1(:)
      integer, allocatable :: s0(:)
      integer(ik) :: it, ka, ga, gb
      integer :: a, b
      allocate (neg(m%nelements))
      neg = .false.
      if (ipol /= policy_voronoi) then
         do it = 1, m%nelements
            do a = 1, int(m%nen) - 1
               do b = a + 1, int(m%nen)
                  ga = m%itet(m%itetoff(it) + a)
                  gb = m%itet(m%itetoff(it) + b)
                  ka = edge_index(ga, gb, m%nnodes, iedge_off, jedge)
                  if (ka == 0) cycle
                  if (edge_negative(min(ga, gb), ka)) neg(it) = .true.
               end do
            end do
         end do
         call write_alpha_vtu(opt%alpha_file, m%nen, m%nnodes, &
                              m%nelements, m%xic, m%yic, m%zic, &
                              m%itetoff, m%itet, m%itetclr, pol%alpha, &
                              stage, neg, ierr)
      else
         allocate (a1(m%nelements), s0(m%nelements))
         a1 = 1.0_rk
         s0 = 0
         call write_alpha_vtu(opt%alpha_file, m%nen, m%nnodes, &
                              m%nelements, m%xic, m%yic, m%zic, &
                              m%itetoff, m%itet, m%itetclr, a1, s0, &
                              neg, ierr)
      end if
      if (ierr /= 0) call fail_stop()
      call log_line('del_vor: wrote alpha per cell to '// &
                    trim(opt%alpha_file))
   end subroutine alpha_file_out

   ! The ParaView files (del_vor_tess, del_vor_vtu): the mesh, the
   ! control volumes, the negative coefficients, the dual faces, and
   ! ROOT.pvd grouping them with the violations file.
   subroutine paraview_out()
      type(vtu_grid_t) :: gm, gc, gn, gf
      logical, allocatable :: sel(:), bedg(:), iedg(:)
      real(rk), allocatable :: a1(:)
      integer, allocatable :: s0(:)
      character(len=1024) :: files(5)
      integer :: nfile
      integer(ik) :: i, it, kk
      logical :: faces3, voro
      allocate (sel(m%nnodes), bedg(nedges), iedg(nedges))
      ! the vertices that get a cell
      select case (trim(opt%subset))
      case ('all')
         sel = .true.
      case ('boundary')
         call boundary_vertices(m, sel)
      case ('flagged')
         sel = .false.
         do i = 1, m%nnodes
            do kk = iedge_off(i), iedge_off(i + 1) - 1
               if (area(kk) < 0.0_rk .and. .not. zero(kk)) then
                  sel(i) = .true.
                  sel(jedge(kk)) = .true.
               end if
            end do
         end do
         if (allocated(pol%alpha)) then
            do it = 1, m%nelements
               if (pol%alpha(it) < 1.0_rk) sel(m%itet(m%itetoff(it) &
                  + 1:m%itetoff(it) + m%nen)) = .true.
            end do
         end if
      case ('material')
         sel = .false.
         do it = 1, m%nelements
            if (m%itetclr(it) == opt%subset_mat) sel(m%itet( &
               m%itetoff(it) + 1:m%itetoff(it) + m%nen)) = .true.
         end do
      case ('box')
         do i = 1, m%nnodes
            sel(i) = m%xic(i) >= opt%subset_box(1) .and. &
                     m%yic(i) >= opt%subset_box(2) .and. &
                     m%zic(i) >= opt%subset_box(3) .and. &
                     m%xic(i) <= opt%subset_box(4) .and. &
                     m%yic(i) <= opt%subset_box(5) .and. &
                     m%zic(i) <= opt%subset_box(6)
         end do
      end select
      write (arg, '(a,i0,a,i0,a)') 'del_vor: paraview_subset '// &
         trim(opt%subset)//': ', count(sel), ' of ', m%nnodes, &
         ' vertices get a cell'
      call log_line(trim(arg))

      faces3 = opt%paraview_faces .and. m%nen == 4
      if (opt%paraview_faces .and. m%nen /= 4) call log_line( &
         'del_vor: paraview_faces: tet meshes only; not written')
      voro = ipol == policy_voronoi
      if (faces3) then
         call build_cells(m, pol, voro, iedge_off, jedge, area, vol, &
                          zero, sel, gc, gf)
      else
         call build_cells(m, pol, voro, iedge_off, jedge, area, vol, &
                          zero, sel, gc)
      end if
      allocate (a1(m%nelements), s0(m%nelements))
      a1 = 1.0_rk
      s0 = 0
      if (allocated(pol%alpha)) a1 = pol%alpha
      if (allocated(stage)) s0 = stage
      call build_mesh_grid(m, vol, sel, a1, s0, gm)
      call boundary_edges(m%nnodes, m%nelements, m%nen, m%itetoff, &
                          m%itet, m%jtetoff, m%jtet, m%mbndry, &
                          iedge_off, jedge, bedg)
      call interface_edges(m, iedge_off, jedge, iedg)
      call build_negative_grid(m, iedge_off, jedge, area, zero, bedg, &
                               iedg, sel, gn)

      nfile = 0
      call vtu_one(gm, '_mesh.vtu', files, nfile)
      call vtu_one(gc, '_cells.vtu', files, nfile)
      if (gn%nc > 0) call vtu_one(gn, '_negative.vtu', files, nfile)
      if (faces3) call vtu_one(gf, '_faces.vtu', files, nfile)
      if (dst%rec%n > 0) then
         nfile = nfile + 1
         files(nfile) = opt%violations
      end if
      call write_pvd(trim(opt%paraview)//'.pvd', files, nfile, ierr)
      if (ierr /= 0) call fail_stop()
      call log_line('del_vor: wrote '//trim(opt%paraview)//'.pvd ('// &
                    'open it in ParaView)')
   end subroutine paraview_out

   ! Write one ParaView file ROOT//suffix and list it for the .pvd.
   subroutine vtu_one(g, suffix, files, nfile)
      type(vtu_grid_t), intent(in) :: g
      character(len=*), intent(in) :: suffix
      character(len=*), intent(inout) :: files(:)
      integer, intent(inout) :: nfile
      nfile = nfile + 1
      files(nfile) = trim(opt%paraview)//suffix
      call write_vtu(files(nfile), g, opt%vtu_binary, &
                     opt%vtu_legacy, ierr)
      if (ierr /= 0) call fail_stop()
      write (arg, '(a,i0,a)') 'del_vor: wrote '// &
         trim(files(nfile))//' (', g%nc, ' cells)'
      call log_line(trim(arg))
   end subroutine vtu_one

   logical function is_keyword(s)
      character(len=*), intent(in) :: s
      select case (trim(s))
      case ('avs', 'stor', 'uge', 'tough2', 'zero_area', 'zero_tol', &
            'compress', 'cv', 'type', 'o', 'delaunay_tol', &
            'violations', 'check_only', 'report', 'timing', &
            'delaunay_fatal', 'alpha', 'hybrid_tol', 'alpha_min', &
            'hybrid_iter', 'alpha_file', 'paraview', 'paraview_faces', &
            'paraview_subset', 'vtu_format', 'vtu_layout')
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
