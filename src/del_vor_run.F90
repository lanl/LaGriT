!-----------------------------------------------------------------------
! del_vor_run: one del_vor run on a mesh already in memory, for the
! standalone program (del_vor.F90) and the LaGriT entry point
! (del_vor_entry.F90). Nothing here stops: every error comes back in
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
! and check the mesh (del_vor_validity: fatal); check Delaunay
! (del_vor_delaunay: fatal for every cv but median, where it is
! advisory); write the violations file if any facet was flagged;
! stop here with check_only on (vol then stays unallocated); else
! place the dual points (del_vor_policy; the hybrid's stages in
! del_vor_hybrid), compute and write the coefficient files. cmd is
! the command for the .stor's second line. vol: the control volumes.
! m%jtetoff and m%jtet are left built (mesh_free frees them).
!-----------------------------------------------------------------------
module del_vor_run
   use del_vor_kinds, only: ik, rk
   use del_vor_log, only: log_line, errmsg, log_copy_open, &
                          log_copy_close
   use del_vor_options, only: options_t, bins_name, policy_word
   use del_vor_mesh, only: mesh_t
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
   use del_vor_vtu, only: vtu_grid_t, vtu_data_t, write_vtu, &
                          write_pvd, write_vti
   use del_vor_matrix, only: build_matrix_grid, matrix_shape, &
                             matrix_bins
   use del_vor_tess, only: build_cells, build_mesh_grid, &
                           build_negative_grid, interface_edges, &
                           boundary_vertices
   use del_vor_write_stor, only: write_stor
   use del_vor_write_uge, only: write_uge
   use del_vor_write_tough, only: write_tough
   use del_vor_timing, only: timer_start, timer_stop, timer_report, &
                             timing_on, timer_reset
   implicit none
   private
   public :: run_start, run_mesh, run_finish, run_abandon, &
             log_options

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

   ! The run's options, keyword=value, wrapped at 78 columns.
   subroutine log_options(t)
      character(len=*), intent(in) :: t
      character(len=80) :: ln
      integer :: p, q, n
      ln = 'del_vor: options'
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

   subroutine run_mesh(m, opt, cmd, vol, ierr)
      type(mesh_t), intent(inout) :: m
      type(options_t), intent(in) :: opt
      character(len=*), intent(in) :: cmd
      real(rk), allocatable, intent(out) :: vol(:)
      integer, intent(out) :: ierr

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
      real(rk), allocatable :: asum(:), avec(:, :)
      logical, allocatable :: bedge(:)
      character(len=22) :: step(3)
      integer(ik) :: nb(3), ni(3)
      real(rk) :: rmin(3), angmax, angmean
      character(len=1024) :: arg

      ierr = 0
      errmsg = ' '
      ndim = 3
      if (m%nen == 3) ndim = 2

      ! parent/child chains (split control volumes): not yet
      npar = count(m%itp1(1:m%nnodes) == itp_parent)
      if (npar > 0) then
         write (arg, '(a,i0,a)') 'del_vor: this mesh has parent/'// &
            'child chains (', npar, ' vertices with itp1 = 41)'
         call log_line(trim(arg))
         errmsg = 'del_vor: run resetpts / parent first; nothing '// &
                  'is written'
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
         errmsg = 'del_vor: the mesh failed its checks (above)'
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
         call log_line('del_vor: wrote flagged facets to '// &
                       trim(opt%violations))
      end if
      ipol = policy_code(trim(opt%cv))
      if (not_delaunay) then
         if (ipol /= policy_median) then
            errmsg = 'del_vor: not Delaunay; nothing else is written'
            go to 900
         end if
         call log_line('del_vor: cv median does not need a '// &
                       'Delaunay mesh: going on')
      end if
      if (opt%check_only) then
         call log_line('del_vor: check_only on: no coefficient files')
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

      do k = 1, opt%nout
         call timer_start('write '//trim(opt%out_type(k)))
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
         case ('tough2')
            call write_tough(opt%out_file(k), m%nnodes, m%xic, m%yic, &
                             m%zic, m%imt1, iedge_off, jedge, area, &
                             vol, skip, ierr)
         end select
         call timer_stop()
         if (ierr /= 0) go to 900
         call log_line('del_vor: wrote '//trim(opt%out_type(k))// &
                       ' '//trim(opt%out_file(k)))
      end do
      return

      ! every error: its message, the report file closed, no volumes
900   continue
      if (ierr == 0) ierr = 1
      if (allocated(vol)) deallocate (vol)
      call run_abandon()
      return

   contains

      ! Place the dual points for cv median, blend or hybrid, and
      ! assemble: area, vol, stats, plus asum and avec for the report.
      ! ierr /= 0 on an error.
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
         call log_line('del_vor: wrote alpha per cell to '// &
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

      ! The matrix file (del_vor_matrix): a point per .stor entry, the
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
         write (arg, '(a,i0,a,i0,a)') 'del_vor: wrote matrix '// &
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
            write (arg, '(a,i0,a,i0,a)') 'del_vor: wrote matrix '// &
               'image '//trim(bins_name(opt%matrix))//' (', nbin, &
               ' x ', nbin, ' pixels)'
            call log_line(trim(arg))
         end if
      end subroutine matrix_out

      ! The ParaView files (del_vor_tess, del_vor_vtu): the mesh, the
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
         write (arg, '(a,i0,a,i0,a)') 'del_vor: paraview_subset '// &
            trim(opt%subset)//': ', count(sel), ' of ', m%nnodes, &
            ' vertices get a cell'
         call log_line(trim(arg))

         faces3 = opt%paraview_faces .and. m%nen == 4
         if (opt%paraview_faces .and. m%nen /= 4) call log_line( &
            'del_vor: paraview_faces: tet meshes only; not written')
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
         call log_line('del_vor: wrote '//trim(opt%paraview)// &
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
         write (arg, '(a,i0,a)') 'del_vor: wrote '// &
            trim(files(nfile))//' (', g%nc, ' cells)'
         call log_line(trim(arg))
      end subroutine vtu_one

   end subroutine run_mesh

end module del_vor_run
