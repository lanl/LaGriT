!-----------------------------------------------------------------------
! cv_dual_run: one cv_dual run on a mesh already in memory, for the
! standalone program (cv_dual.F90) and the LaGriT entry point
! (cv_dual_entry.F90). Nothing here stops: every error comes back in
! ierr, with its message already written to the log.
!
!   call run_start(opt, ierr)      timing, the report file
!   call log_options(all_settings(opt))
!   ... the caller gets the mesh (read_avs, or LaGriT's arrays) ...
!   call run_mesh(m, opt, cmd, vol, ierr)
!   call run_finish()              timing report, report file closed
!
! After an error, run_mesh has already called run_abandon (message,
! report file closed); the caller does not call run_finish.
!
! run_mesh's order of work: refuse parent/child chains; build jtet
! and check the mesh (cv_dual_validity: fatal); check Delaunay
! (cv_dual_delaunay: fatal for every cv but median, where it is
! advisory); write the violations file if any facet was flagged;
! stop here with check_only on (vol then stays unallocated); else
! place the dual points (cv_dual_policy; the hybrid's stages in
! cv_dual_hybrid), compute and write the coefficient files. cmd is
! the command for the .stor's second line. vol: the control volumes.
! m%jtetoff and m%jtet are left built (mesh_free frees them).
!-----------------------------------------------------------------------
module cv_dual_run
   use cv_dual_kinds, only: ik, rk
   use cv_dual_log, only: log_line, errmsg, log_copy_open, &
                          log_copy_close
   use cv_dual_options, only: options_t, bins_name, policy_word
   use cv_dual_mesh, only: mesh_t
   use cv_dual_topology, only: build_edge_csr, edge_index
   use cv_dual_policy, only: policy_voronoi, policy_median, &
                             policy_blend, policy_hybrid, policy_code
   use cv_dual_assemble, only: assemble, assembly_stats_t, &
                               zero_area_mask, cell_policy_t
   use cv_dual_hybrid, only: hybrid_t, stage_a, stage_b, &
                             boundary_edges, is_negative
   use cv_dual_report, only: report_run, report_delaunay, &
                             report_policy
   use cv_dual_jtet, only: build_jtet, jtet_stats_t, default_mbndry
   use cv_dual_validity, only: check_validity, validity_t
   use cv_dual_delaunay, only: check_delaunay, delaunay_t
   use cv_dual_write_vtu, only: write_violations_vtu, write_alpha_vtu
   use cv_dual_vtu, only: vtu_grid_t, vtu_data_t, write_vtu, &
                          write_pvd, write_vti
   use cv_dual_matrix, only: build_matrix_grid, matrix_shape, &
                             matrix_bins
   use cv_dual_tess, only: build_cells, build_mesh_grid, &
                           build_negative_grid, interface_edges, &
                           boundary_vertices
   use cv_dual_write_stor, only: write_stor
   use cv_dual_write_uge, only: write_uge
   use cv_dual_boundary, only: boundary_areas, write_outside_area
   use cv_dual_write_tough, only: write_tough
   use cv_dual_timing, only: timer_start, timer_stop, timer_report, &
                             timing_on, timer_reset
   implicit none
   private
   public :: run_start, run_mesh, run_finish, run_abandon, &
             log_options, want_vertex_stats, vertex_stats
   public :: want_ghost_faces, nghost_face, ghost_face_v, &
             ghost_face_vec

   ! For LaGriT's dump / fehm ... / exclude: with want_ghost_faces set
   ! and exclude given, exclude_reduce lists every face between a kept
   ! vertex and an excluded one (a ghost): ghost_face_v(k) the kept
   ! vertex in the kept numbering (1, 2, ... as in the .stor),
   ! ghost_face_vec(:, k) A(ij) times the unit vector from the vertex
   ! to the ghost. Zero-area connections are left out.
   logical, save :: want_ghost_faces = .false.
   integer(ik), save :: nghost_face = 0
   integer(ik), allocatable, save :: ghost_face_v(:)
   real(rk), allocatable, save :: ghost_face_vec(:, :)

   ! For LaGriT's dump / control_volume (its ccoef_* keywords): with
   ! want_vertex_stats set, run_mesh fills vertex_stats(10, nnodes),
   ! over each vertex's connections that are not zero-area (always
   ! left out here, whatever zero_area says):
   !    1, 2   smallest, largest A(ij)
   !    3, 4   the other vertex j of those two connections
   !    5, 6   smallest, largest A(ij)/d(ij)
   !    7, 8   the other vertex j of those two connections
   !    9, 10  shortest, longest d(ij)
   ! A vertex with no such connection: all 0. The vertex numbers are
   ! stored as reals (exact). Ties: the first connection found.
   logical, save :: want_vertex_stats = .false.
   real(rk), allocatable, save :: vertex_stats(:, :)

   ! LaGriT's vertex type of a parent (ifitpcup, blockcom.h)
   integer(ik), parameter :: itp_parent = 41

contains

   ! Before the mesh: forget earlier runs, timing, the report file.
   subroutine run_start(opt, ierr)
      type(options_t), intent(in) :: opt
      integer, intent(out) :: ierr
      ierr = 0
      call timer_reset()
      timing_on = opt%timing
      if (len_trim(opt%report) > 0) then
         call log_copy_open(opt%report, ierr)
         if (ierr /= 0) call run_abandon()
      end if
   end subroutine run_start

   ! After the run: the timing report, the report file closed.
   subroutine run_finish()
      call timer_report()
      call log_copy_close()
   end subroutine run_finish

   ! After an error: its message, the report file closed.
   subroutine run_abandon()
      if (len_trim(errmsg) > 0) call log_line(errmsg)
      call log_copy_close()
   end subroutine run_abandon

   ! The run's options, keyword=value, wrapped at 78 columns. A word
   ! longer than that (a long file name) gets a line of its own, so ln
   ! holds the longest word (it was 80: ln(1:n) read past its end).
   subroutine log_options(t)
      character(len=*), intent(in) :: t
      character(len=len(t) + 80) :: ln
      integer :: p, q, n
      ln = 'cv_dual: options'
      n = len_trim(ln)
      p = 1
      do while (p <= len(t))
         q = index(t(p:), ' ')
         if (q == 0) then
            q = len(t) + 1
         else
            q = p + q - 1
         end if
         if (q > p) then
            if (n + 1 + (q - p) > 78 .and. n > 1) then
               call log_line(ln(1:n))
               ln = ' '
               n = 1
            end if
            ln(n + 2:) = t(p:q - 1)
            n = n + 1 + (q - p)
         end if
         p = q + 1
      end do
      call log_line(ln(1:n))
   end subroutine log_options

   subroutine run_mesh(m, opt, cmd, vol, ierr, barea)
      type(mesh_t), intent(inout) :: m
      type(options_t), intent(in) :: opt
      character(len=*), intent(in) :: cmd
      real(rk), allocatable, intent(out) :: vol(:)
      integer, intent(out) :: ierr
      ! the boundary vector areas (cv_dual_boundary), when asked
      real(rk), allocatable, intent(out), optional :: barea(:, :)

      type(assembly_stats_t) :: stats
      type(jtet_stats_t) :: jst
      type(validity_t) :: vst
      type(delaunay_t) :: dst
      logical :: not_delaunay
      integer(ik), allocatable :: iedge_off(:), jedge(:)
      real(rk), allocatable :: area(:)
      logical, allocatable :: zero(:), skip(:)
      integer(ik) :: nedges, npar
      integer :: ndim, k
      ! control-volume policy (cv other than voronoi)
      integer :: ipol, nstep
      type(cell_policy_t) :: pol
      type(hybrid_t) :: hs
      integer, allocatable :: stage(:)
      real(rk), allocatable :: asum(:), avec(:, :), bar(:, :)
      logical, allocatable :: bedge(:)
      character(len=22) :: step(3)
      integer(ik) :: nb(3), ni(3)
      real(rk) :: rmin(3), angmax, angmean
      character(len=1024) :: arg
      ! exclude: the kept vertices' arrays (exclude_reduce)
      integer(ik) :: nx
      real(rk), allocatable :: xx(:), yx(:), zx(:), volx(:), areax(:)
      integer(ik), allocatable :: imtx(:), offx(:), jx(:)
      logical, allocatable :: skipx(:)

      ierr = 0
      errmsg = ' '
      ndim = 3
      if (m%nen == 3) ndim = 2

      ! parent/child chains (split control volumes): not yet
      npar = count(m%itp1(1:m%nnodes) == itp_parent)
      if (npar > 0) then
         write (arg, '(a,i0,a)') 'cv_dual: this mesh has parent/'// &
            'child chains (', npar, ' vertices with itp1 = 41)'
         call log_line(trim(arg))
         errmsg = 'cv_dual: run resetpts / parent, then rmpoint / '// &
                  'compress; nothing is written'
         go to 900
      end if

      ! neighbors and the mesh checks: fatal in every mode
      m%mbndry = default_mbndry(m%nen, m%nelements)
      call timer_start('jtet')
      if (associated(m%jtet)) deallocate (m%jtet)
      if (associated(m%jtetoff)) deallocate (m%jtetoff)
      allocate (m%jtetoff(m%nelements), m%jtet(m%nen*m%nelements))
      call build_jtet(m%nnodes, m%nelements, m%nen, m%itetoff, m%itet, &
                      m%itetclr, m%mbndry, m%jtetoff, m%jtet, jst)
      call timer_stop()
      call timer_start('mesh checks')
      call check_validity(m%nnodes, m%nelements, m%nen, m%xic, m%yic, &
                          m%zic, m%itetoff, m%itet, m%itetclr, jst, vst)
      call timer_stop()
      if (vst%nfatal > 0) then
         errmsg = 'cv_dual: the mesh failed its checks (above)'
         go to 900
      end if

      ! the Delaunay check: fatal for Voronoi control volumes
      call timer_start('Delaunay check')
      call check_delaunay(m%nnodes, m%nelements, m%nen, m%xic, m%yic, &
                          m%zic, m%itetoff, m%itet, m%itetclr, &
                          m%jtetoff, m%jtet, m%mbndry, &
                          opt%delaunay_tol, opt%delaunay_fatal, dst)
      call report_delaunay(m%nen, dst, m%itetclr, not_delaunay)
      call timer_stop()
      if (dst%rec%n > 0) then
         call timer_start('violations file')
         call write_violations_vtu(opt%violations, m%nen, m%nnodes, &
                                   m%xic, m%yic, m%zic, dst%rec, ierr)
         call timer_stop()
         if (ierr /= 0) go to 900
         call log_line('cv_dual: wrote flagged facets to '// &
                       trim(opt%violations))
      end if
      ipol = policy_code(trim(opt%cv))
      if (not_delaunay) then
         if (ipol /= policy_median) then
            errmsg = 'cv_dual: not Delaunay; nothing else is written'
            go to 900
         end if
         call log_line('cv_dual: cv median does not need a '// &
                       'Delaunay mesh: going on')
      end if
      if (opt%check_only) then
         call log_line('cv_dual: check_only on: no coefficient files')
         return
      end if

      call timer_start('edge CSR')
      call build_edge_csr(m%nnodes, m%nelements, m%nen, m%itetoff, &
                          m%itet, iedge_off, jedge, nedges, ierr)
      call timer_stop()
      if (ierr /= 0) go to 900

      allocate (area(nedges), vol(m%nnodes))
      if (ipol == policy_voronoi) then
         call timer_start('assembly')
         if (len_trim(opt%matrix) > 0) then
            ! the matrix file's angle needs each face's vector area
            allocate (avec(3, nedges))
            call assemble(m%nnodes, m%nelements, m%nen, m%xic, m%yic, &
                          m%zic, m%itetoff, m%itet, iedge_off, jedge, &
                          area, vol, stats, ierr, area_vec=avec)
         else
            call assemble(m%nnodes, m%nelements, m%nen, m%xic, m%yic, &
                          m%zic, m%itetoff, m%itet, iedge_off, jedge, &
                          area, vol, stats, ierr)
         end if
         call timer_stop()
         if (ierr /= 0) go to 900
      else
         call run_policy()
         if (ierr /= 0) go to 900
      end if

      call timer_start('zero-area mask')
      allocate (zero(nedges), skip(nedges))
      call zero_area_mask(m%nen, m%nnodes, m%xic, m%yic, m%zic, &
                          iedge_off, jedge, area, opt%zero_tol, zero)
      skip = zero .and. .not. opt%zero_keep
      call timer_stop()

      call timer_start('summary')
      call report_run(ndim, m%nnodes, m%nelements, m%xic, m%yic, &
                      m%zic, iedge_off, jedge, area, vol, stats, zero, &
                      opt%zero_keep, opt%zero_tol)
      if (ipol /= policy_voronoi) then
         call angles(angmax, angmean)
         call report_policy(trim(opt%cv), nstep, step, nb, ni, rmin, &
                            pol%alpha, stage, opt%alpha_min, &
                            hs%nstage_a, hs%nstage_b, hs%npass, &
                            hs%stuck, hs%limit, angmax, angmean)
      end if
      call timer_stop()
      if (len_trim(opt%alpha_file) > 0) then
         call alpha_file_out()
         if (ierr /= 0) go to 900
      end if
      if (len_trim(opt%matrix) > 0) then
         call timer_start('matrix file')
         call matrix_out()
         call timer_stop()
         if (ierr /= 0) go to 900
      end if
      if (len_trim(opt%paraview) > 0) then
         call timer_start('paraview files')
         call paraview_out()
         call timer_stop()
         if (ierr /= 0) go to 900
      end if

      if (opt%nexclude > 0 .and. opt%nout > 0) then
         call exclude_reduce()
         if (ierr /= 0) go to 900
      end if
      do k = 1, opt%nout
         call timer_start('write '//trim(opt%out_type(k)))
         if (opt%nexclude > 0) then
            ! only the vertices not excluded (exclude_reduce)
            select case (trim(opt%out_type(k)))
            case ('stor')
               call write_stor(opt%out_file(k), ndim, nx, xx, yx, zx, &
                               offx, jx, areax, volx, skipx, &
                               opt%compress, ierr, &
                               policy=trim(policy_word(opt%cv)), &
                               settings=cmd)
            case ('uge')
               call write_uge(opt%out_file(k), nx, xx, yx, zx, offx, &
                              jx, areax, volx, skipx, ierr)
            case ('tough')
               call write_tough(opt%out_file(k), nx, xx, yx, zx, imtx, &
                                offx, jx, areax, volx, skipx, ierr)
            end select
         else
         select case (trim(opt%out_type(k)))
         case ('stor')
            call write_stor(opt%out_file(k), ndim, m%nnodes, m%xic, &
                            m%yic, m%zic, iedge_off, jedge, area, vol, &
                            skip, opt%compress, ierr, &
                            policy=trim(policy_word(opt%cv)), &
                            settings=cmd)
         case ('uge')
            call write_uge(opt%out_file(k), m%nnodes, m%xic, m%yic, &
                           m%zic, iedge_off, jedge, area, vol, skip, &
                           ierr)
         case ('tough')
            call write_tough(opt%out_file(k), m%nnodes, m%xic, m%yic, &
                             m%zic, m%imt1, iedge_off, jedge, area, &
                             vol, skip, ierr)
         end select
         end if
         call timer_stop()
         if (ierr /= 0) go to 900
         call log_line('cv_dual: wrote '//trim(opt%out_type(k))// &
                       ' '//trim(opt%out_file(k)))
      end do

      ! per-vertex extremes of the connections, for dump /
      ! control_volume's ccoef_* attributes
      if (want_vertex_stats) call vertex_stats_fill()

      ! the boundary vector areas: the outside_area file, the caller
      if (len_trim(opt%outside_area) > 0 .or. present(barea)) then
         call timer_start('boundary areas')
         allocate (bar(3, m%nnodes))
         call boundary_areas(m, pol, bar)
         call timer_stop()
         if (len_trim(opt%outside_area) > 0) then
            call write_outside_area(opt%outside_area, m, bar, &
                                    trim(policy_word(opt%cv)), ierr)
            if (ierr /= 0) go to 900
            call log_line('cv_dual: wrote outside_area '// &
                          trim(opt%outside_area))
         end if
         if (present(barea)) call move_alloc(bar, barea)
      end if
      return

      ! every error: its message, the report file closed, no volumes
900   continue
      if (ierr == 0) ierr = 1
      if (allocated(vol)) deallocate (vol)
      if (present(barea)) then
         if (allocated(barea)) deallocate (barea)
      end if
      call run_abandon()
      return

   contains

      ! Place the dual points for cv median, blend or hybrid, and
      ! assemble: area, vol, stats, plus asum and avec for the report.
      ! ierr /= 0 on an error.
      ! vertex_stats (see the module's header): one pass over the
      ! edges, both ends of each connection that is not zero-area.
      ! exclude (the ghost generators of a conforming Voronoi mesh):
      ! the coefficients were computed on the whole mesh; keep the
      ! vertices whose imt1 is not excluded, in their order (numbered
      ! 1, 2, ...), and the connections between two of them.
      subroutine exclude_reduce()
         logical, allocatable :: keep(:)
         integer(ik), allocatable :: newid(:)
         integer(ik) :: i, e, j, k, ne, nlost, nf, ntiny, nkb, np, ri, rj
         integer(ik) :: ned, it, nk, e1, nf2
         integer :: a1, b1
         integer(ik), allocatable :: fpiece(:), seen(:)
         real(rk) :: d(3), dl, amax, atol
         logical, allocatable :: bvx(:), touch(:)
         integer(ik), allocatable :: root(:), piece(:), pn(:)
         real(rk), allocatable :: pvec(:, :), parea(:)
         real(rk) :: vkept
         character(len=256) :: ln
         allocate (keep(m%nnodes), newid(m%nnodes))
         do i = 1, m%nnodes
            keep(i) = .not. any(opt%exclude(1:opt%nexclude) == &
                                int(m%imt1(i)))
         end do
         nx = count(keep)
         if (nx == 0) then
            errmsg = 'cv_dual: exclude leaves no vertex; nothing '// &
                     'is written'
            ierr = 1
            return
         end if
         ! check 1: the excluded vertices (ghosts) must surround the
         ! model: no kept vertex on the outside of the whole mesh (the
         ! outer boundary or a gap in it)
         ! (nothing excluded: a material not in the mesh; no check)
         nkb = 0
         if (nx < m%nnodes) then
            allocate (bvx(m%nnodes))
            call boundary_vertices(m, bvx)
            nkb = count(keep .and. bvx)
            deallocate (bvx)
         end if
         if (nkb > 0) then
            write (ln, '(a,i0,a)') 'cv_dual: exclude: ', nkb, &
               ' kept vertices on the outside of the mesh'
            if (.not. opt%exclude_force) then
               call log_line(trim(ln)//':')
               call log_line('  the excluded materials do not '// &
                             'surround the model (not ghosts?)')
               errmsg = 'cv_dual: exclude: nothing is written '// &
                        '(exclude_force writes it anyway)'
               ierr = 1
               return
            end if
            call log_line('WARNING: '//trim(ln))
            call log_line('  exclude_force: written anyway')
         end if
         newid = 0
         j = 0
         do i = 1, m%nnodes
            if (keep(i)) then
               j = j + 1
               newid(i) = j
            end if
         end do
         ne = 0
         nlost = 0
         do i = 1, m%nnodes
            do e = iedge_off(i), iedge_off(i + 1) - 1
               if (keep(i) .and. keep(jedge(e))) then
                  ne = ne + 1
               else if (keep(i) .or. keep(jedge(e))) then
                  nlost = nlost + 1
               end if
            end do
         end do
         allocate (xx(nx), yx(nx), zx(nx), volx(nx), imtx(nx), &
                   offx(nx + 1), jx(max(ne, 1_ik)), &
                   areax(max(ne, 1_ik)), skipx(max(ne, 1_ik)))
         ne = 0
         do i = 1, m%nnodes
            if (.not. keep(i)) cycle
            j = newid(i)
            xx(j) = m%xic(i)
            yx(j) = m%yic(i)
            zx(j) = m%zic(i)
            volx(j) = vol(i)
            imtx(j) = m%imt1(i)
            offx(j) = ne + 1
            do e = iedge_off(i), iedge_off(i + 1) - 1
               if (.not. keep(jedge(e))) cycle
               ne = ne + 1
               jx(ne) = newid(jedge(e))
               areax(ne) = area(e)
               skipx(ne) = skip(e)
            end do
         end do
         offx(nx + 1) = ne + 1
         ! the faces between kept vertices and ghosts (see the
         ! module's header; kept for LaGriT when want_ghost_faces).
         ! Each edge is stored once, at its lower vertex: the kept
         ! vertex is either end. A face below 1e-10 of the largest is
         ! round-off (four nearly co-circular points), left out.
         amax = 0.0_rk
         do i = 1, m%nnodes
            do e = iedge_off(i), iedge_off(i + 1) - 1
               if (keep(i) .eqv. keep(jedge(e))) cycle
               if (skip(e)) cycle
               amax = max(amax, abs(area(e)))
            end do
         end do
         atol = 1.0e-10_rk*amax
         nf = 0
         ntiny = 0
         do i = 1, m%nnodes
            do e = iedge_off(i), iedge_off(i + 1) - 1
               if (keep(i) .eqv. keep(jedge(e))) cycle
               if (skip(e) .or. area(e) == 0.0_rk) cycle
               if (abs(area(e)) <= atol) then
                  ntiny = ntiny + 1
                  cycle
               end if
               nf = nf + 1
            end do
         end do
         if (allocated(ghost_face_v)) deallocate (ghost_face_v)
         if (allocated(ghost_face_vec)) deallocate (ghost_face_vec)
         allocate (ghost_face_v(max(nf, 1_ik)), &
                   ghost_face_vec(3, max(nf, 1_ik)))
         allocate (touch(m%nnodes))
         touch = .false.
         nf = 0
         do i = 1, m%nnodes
            do e = iedge_off(i), iedge_off(i + 1) - 1
               if (keep(i) .eqv. keep(jedge(e))) cycle
               if (skip(e) .or. abs(area(e)) <= atol) cycle
               if (area(e) == 0.0_rk) cycle
               ! k the kept vertex, j the ghost
               if (keep(i)) then
                  k = i
                  j = jedge(e)
               else
                  k = jedge(e)
                  j = i
               end if
               d = [m%xic(j) - m%xic(k), m%yic(j) - m%yic(k), &
                    m%zic(j) - m%zic(k)]
               dl = norm2(d)
               if (dl == 0.0_rk) cycle
               nf = nf + 1
               ghost_face_v(nf) = k
               ghost_face_vec(:, nf) = area(e)*d/dl
               touch(k) = .true.
            end do
         end do
         nghost_face = nf
         write (ln, '(a,i0,a)') 'cv_dual: exclude: ', nf, &
            ' faces between kept and excluded vertices'
         call log_line(trim(ln))
         if (ntiny > 0) then
            write (ln, '(a,i0,a)') 'cv_dual: exclude: ', ntiny, &
               ' of them below 1e-10 of the largest left out '// &
               '(round-off)'
            call log_line(trim(ln))
         end if

         ! check 2: the faces form closed surfaces. Faces dual to
         ! edges of one cell meet at that cell's circumcenter: the
         ! kept-excluded edges of each cell are joined (zero-area ones
         ! too, as links), and each piece's face vectors must add up to
         ! zero.
         ned = iedge_off(m%nnodes + 1) - 1
         allocate (root(ned))
         do e = 1, ned
            root(e) = e
         end do
         do it = 1, m%nelements
            nk = 0
            do a1 = 1, int(m%nen) - 1
               do b1 = a1 + 1, int(m%nen)
                  i = m%itet(m%itetoff(it) + a1)
                  j = m%itet(m%itetoff(it) + b1)
                  if (keep(i) .eqv. keep(j)) cycle
                  e = edge_index(i, j, m%nnodes, iedge_off, jedge)
                  if (e <= 0) cycle
                  nk = nk + 1
                  if (nk == 1) then
                     e1 = e
                  else
                     ri = e1
                     do while (root(ri) /= ri)
                        ri = root(ri)
                     end do
                     rj = e
                     do while (root(rj) /= rj)
                        rj = root(rj)
                     end do
                     if (ri /= rj) root(max(ri, rj)) = min(ri, rj)
                  end if
               end do
            end do
         end do
         allocate (piece(ned))
         piece = 0
         np = 0
         allocate (fpiece(max(nf, 1_ik)))
         nf2 = 0
         do i = 1, m%nnodes
            do e = iedge_off(i), iedge_off(i + 1) - 1
               if (keep(i) .eqv. keep(jedge(e))) cycle
               if (skip(e) .or. abs(area(e)) <= atol) cycle
               if (area(e) == 0.0_rk) cycle
               j = jedge(e)
               if (m%xic(i) == m%xic(j) .and. m%yic(i) == m%yic(j) &
                   .and. m%zic(i) == m%zic(j)) cycle
               k = e
               do while (root(k) /= k)
                  k = root(k)
               end do
               if (piece(k) == 0) then
                  np = np + 1
                  piece(k) = np
               end if
               nf2 = nf2 + 1
               if (nf2 <= nf) fpiece(nf2) = piece(k)
            end do
         end do
         allocate (pvec(3, max(np, 1_ik)), parea(max(np, 1_ik)), &
                   pn(max(np, 1_ik)), seen(m%nnodes))
         pvec = 0.0_rk
         parea = 0.0_rk
         pn = 0
         seen = 0
         do k = 1, nf
            j = fpiece(k)
            pvec(:, j) = pvec(:, j) + ghost_face_vec(:, k)
            parea(j) = parea(j) + norm2(ghost_face_vec(:, k))
            if (seen(ghost_face_v(k)) /= j) then
               seen(ghost_face_v(k)) = j
               pn(j) = pn(j) + 1
            end if
         end do
         vkept = sum(vol, mask=keep)
         write (ln, '(a,es13.6)') 'cv_dual: exclude: kept volume ', vkept
         call log_line(trim(ln))
         write (ln, '(a,i0,a)') 'cv_dual: exclude: faces shared with '// &
            'excluded vertices: ', np, ' piece(s)'
         call log_line(trim(ln))
         do k = 1, np
            dl = norm2(pvec(:, k))/max(parea(k), tiny(1.0_rk))
            if (k > 10 .and. dl <= 1.0e-8_rk) cycle
            write (ln, '(a,i0,a,i0,a,es13.6)') 'cv_dual: exclude:   '// &
               'piece ', k, ': ', pn(k), ' vertices, area ', parea(k)
            if (dl <= 1.0e-8_rk) then
               call log_line(trim(ln)//', closed')
            else
               call log_line(trim(ln))
               write (ln, '(a,i0,a,es8.1,a)') 'WARNING: cv_dual: '// &
                  'exclude: piece ', k, ' is NOT closed (net ', dl, &
                  ' of its area)'
               call log_line(trim(ln))
            end if
         end do
         if (np > 10) then
            write (ln, '(a,i0,a)') 'cv_dual: exclude:   and ', np - 10, &
               ' more pieces'
            call log_line(trim(ln))
         end if
         ! the module's list, numbered as written (kept numbering)
         do k = 1, nf
            ghost_face_v(k) = newid(ghost_face_v(k))
         end do
         if (.not. want_ghost_faces) then
            deallocate (ghost_face_v, ghost_face_vec)
            nghost_face = 0
         end if
         write (ln, '(a,i0,a,i0,a,i0,a)') 'cv_dual: exclude: ', nx, &
            ' of ', m%nnodes, ' vertices written (', m%nnodes - nx, &
            ' excluded)'
         call log_line(trim(ln))
         write (ln, '(a,i0,a)') 'cv_dual: exclude: ', nlost, &
            ' connections to excluded vertices left out'
         call log_line(trim(ln))
         if (len_trim(opt%outside_area) > 0) call log_line( &
            'cv_dual: exclude: outside_area is the whole mesh''s '// &
            '(its numbering)')


      end subroutine exclude_reduce

      subroutine vertex_stats_fill()
         integer(ik) :: i, j, kk, e, v, w
         real(rk) :: a, d, q, rw
         logical, allocatable :: seen(:)
         if (allocated(vertex_stats)) deallocate (vertex_stats)
         allocate (vertex_stats(10, m%nnodes), seen(m%nnodes))
         vertex_stats = 0.0_rk
         seen = .false.
         do i = 1, m%nnodes
            do kk = iedge_off(i), iedge_off(i + 1) - 1
               if (zero(kk)) cycle
               j = jedge(kk)
               a = area(kk)
               d = norm2([m%xic(j) - m%xic(i), m%yic(j) - m%yic(i), &
                          m%zic(j) - m%zic(i)])
               q = a/d
               do e = 1, 2
                  if (e == 1) then
                     v = i
                     w = j
                  else
                     v = j
                     w = i
                  end if
                  rw = real(w, rk)
                  if (.not. seen(v)) then
                     vertex_stats(:, v) = [a, a, rw, rw, q, q, rw, rw, &
                                           d, d]
                     seen(v) = .true.
                     cycle
                  end if
                  if (a < vertex_stats(1, v)) then
                     vertex_stats(1, v) = a
                     vertex_stats(3, v) = rw
                  end if
                  if (a > vertex_stats(2, v)) then
                     vertex_stats(2, v) = a
                     vertex_stats(4, v) = rw
                  end if
                  if (q < vertex_stats(5, v)) then
                     vertex_stats(5, v) = q
                     vertex_stats(7, v) = rw
                  end if
                  if (q > vertex_stats(6, v)) then
                     vertex_stats(6, v) = q
                     vertex_stats(8, v) = rw
                  end if
                  vertex_stats(9, v) = min(vertex_stats(9, v), d)
                  vertex_stats(10, v) = max(vertex_stats(10, v), d)
               end do
            end do
         end do
         deallocate (seen)
      end subroutine vertex_stats_fill

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
            if (ierr /= 0) return
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
            if (ierr /= 0) return
            call tally('after stage A')
            call timer_start('stage B')
            call stage_b(m%nnodes, m%nelements, m%nen, m%xic, m%yic, &
                         m%zic, m%itetoff, m%itet, m%jtetoff, m%jtet, &
                         m%mbndry, iedge_off, jedge, pol, area, vol, &
                         asum, opt%hybrid_tol, opt%zero_tol, &
                         opt%alpha_min, opt%hybrid_iter, stage, hs, &
                         ierr)
            call timer_stop()
            if (ierr /= 0) return
         end if
         ! the final numbers, from one clean assembly
         call timer_start('assembly')
         call assemble(m%nnodes, m%nelements, m%nen, m%xic, m%yic, &
                       m%zic, m%itetoff, m%itet, iedge_off, jedge, &
                       area, vol, stats, ierr, area_vec=avec, pol=pol, &
                       jtetoff=m%jtetoff, jtet=m%jtet, &
                       mbndry=m%mbndry, asum=asum)
         call timer_stop()
         if (ierr /= 0) return
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

      ! cv_dual_hybrid's test, on edge kk of row i
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

      ! alpha_file: the mesh with alpha, stage and the cells next to
      ! an edge still negative.
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
                     if (edge_negative(min(ga, gb), ka)) &
                        neg(it) = .true.
                  end do
               end do
            end do
            call write_alpha_vtu(opt%alpha_file, m%nen, m%nnodes, &
                                 m%nelements, m%xic, m%yic, m%zic, &
                                 m%itetoff, m%itet, m%itetclr, &
                                 pol%alpha, stage, neg, ierr)
         else
            allocate (a1(m%nelements), s0(m%nelements))
            a1 = 1.0_rk
            s0 = 0
            call write_alpha_vtu(opt%alpha_file, m%nen, m%nnodes, &
                                 m%nelements, m%xic, m%yic, m%zic, &
                                 m%itetoff, m%itet, m%itetclr, a1, s0, &
                                 neg, ierr)
         end if
         if (ierr /= 0) return
         call log_line('cv_dual: wrote alpha per cell to '// &
                       trim(opt%alpha_file))
      end subroutine alpha_file_out

      ! The vertices of paraview_subset (all of them by default).
      subroutine select_vertices(sel)
         logical, intent(out) :: sel(:)
         integer(ik) :: i, it, kk
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
      end subroutine select_vertices

      ! The matrix file (cv_dual_matrix): a point per .stor entry, the
      ! rows of paraview_subset; with matrix_bins, its image too.
      subroutine matrix_out()
         type(vtu_grid_t) :: gx
         type(vtu_data_t) :: d(3)
         logical, allocatable :: sel(:), bedg(:), iedg(:)
         real(rk), allocatable :: a1(:)
         real(rk) :: org(3), spc(3)
         integer(ik) :: nent, bw, prof, nbin
         allocate (sel(m%nnodes), bedg(nedges), iedg(nedges), &
                   a1(m%nelements))
         call select_vertices(sel)
         a1 = 1.0_rk
         if (allocated(pol%alpha)) a1 = pol%alpha
         call boundary_edges(m%nnodes, m%nelements, m%nen, m%itetoff, &
                             m%itet, m%jtetoff, m%jtet, m%mbndry, &
                             iedge_off, jedge, bedg)
         call interface_edges(m, iedge_off, jedge, iedg)
         call build_matrix_grid(m, iedge_off, jedge, area, vol, avec, &
                                zero, opt%zero_keep, bedg, iedg, a1, &
                                sel, gx)
         call write_vtu(opt%matrix, gx, opt%vtu_binary, .false., ierr)
         if (ierr /= 0) return
         call matrix_shape(m%nnodes, iedge_off, jedge, zero, &
                           opt%zero_keep, nent, bw, prof)
         write (arg, '(a,i0,a,i0,a)') 'cv_dual: wrote matrix '// &
            trim(opt%matrix)//' (', gx%np, ' entries in ', count(sel), &
            ' rows)'
         call log_line(trim(arg))
         write (arg, '(a,i0,a,i0,a,i0)') '  matrix entries ', nent, &
            ', bandwidth ', bw, ', profile ', prof
         call log_line(trim(arg))
         if (opt%matrix_bins > 0) then
            nbin = min(int(opt%matrix_bins, ik), m%nnodes)
            call matrix_bins(gx, m%nnodes, nbin, d, org, spc)
            call write_vti(bins_name(opt%matrix), nbin, nbin, org, &
                           spc, d, 3, opt%vtu_binary, ierr)
            if (ierr /= 0) return
            write (arg, '(a,i0,a,i0,a)') 'cv_dual: wrote matrix '// &
               'image '//trim(bins_name(opt%matrix))//' (', nbin, &
               ' x ', nbin, ' pixels)'
            call log_line(trim(arg))
         end if
      end subroutine matrix_out

      ! The ParaView files (cv_dual_tess, cv_dual_vtu): the mesh, the
      ! control volumes, the negative coefficients, the dual faces,
      ! and ROOT.pvd grouping them with the violations file.
      subroutine paraview_out()
         type(vtu_grid_t) :: gm, gc, gn, gf
         logical, allocatable :: sel(:), bedg(:), iedg(:)
         real(rk), allocatable :: a1(:)
         integer, allocatable :: s0(:)
         character(len=1024) :: files(5)
         integer :: nfile
         logical :: faces3
         allocate (sel(m%nnodes), bedg(nedges), iedg(nedges))
         call select_vertices(sel)
         write (arg, '(a,i0,a,i0,a)') 'cv_dual: paraview_subset '// &
            trim(opt%subset)//': ', count(sel), ' of ', m%nnodes, &
            ' vertices get a cell'
         call log_line(trim(arg))

         faces3 = opt%paraview_faces .and. m%nen == 4
         if (opt%paraview_faces .and. m%nen /= 4) call log_line( &
            'cv_dual: paraview_faces: tet meshes only; not written')
         if (faces3) then
            call build_cells(m, pol, iedge_off, jedge, area, vol, &
                             zero, sel, gc, gf)
         else
            call build_cells(m, pol, iedge_off, jedge, area, vol, &
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
         call build_negative_grid(m, iedge_off, jedge, area, zero, &
                                  bedg, iedg, sel, gn)

         nfile = 0
         call vtu_one(gm, '_mesh.vtu', files, nfile)
         if (ierr /= 0) return
         call vtu_one(gc, '_cells.vtu', files, nfile)
         if (ierr /= 0) return
         if (gn%nc > 0) then
            call vtu_one(gn, '_negative.vtu', files, nfile)
            if (ierr /= 0) return
         end if
         if (faces3) then
            call vtu_one(gf, '_faces.vtu', files, nfile)
            if (ierr /= 0) return
         end if
         if (dst%rec%n > 0) then
            nfile = nfile + 1
            files(nfile) = opt%violations
         end if
         call write_pvd(trim(opt%paraview)//'.pvd', files, nfile, ierr)
         if (ierr /= 0) return
         call log_line('cv_dual: wrote '//trim(opt%paraview)// &
                       '.pvd (open it in ParaView)')
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
         if (ierr /= 0) return
         write (arg, '(a,i0,a)') 'cv_dual: wrote '// &
            trim(files(nfile))//' (', g%nc, ' cells)'
         call log_line(trim(arg))
      end subroutine vtu_one

   end subroutine run_mesh

end module cv_dual_run
