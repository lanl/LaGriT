!-----------------------------------------------------------------------
! del_vor_report: the run summary, printed through log_line.
!
! Volume balance: the control volumes must add up to the mesh volume
! (area in 2D) whatever the dual points, so the relative difference is
! a check on the whole calculation. Volumes are never changed by the
! zero-area filter.
!
! Zero-area connections are those marked by zero_area_mask
! (|A| <= zero_tol d^2 for tets, zero_tol d for triangles). Negative
! coefficients are the other connections with A(ij) < 0.
! (lanl/voronoi counted A/d > 0, the positive ones, by mistake.)
!-----------------------------------------------------------------------
module del_vor_report
   use del_vor_kinds, only: ik, rk
   use del_vor_assemble, only: assembly_stats_t
   use del_vor_log, only: log_line
   use del_vor_delaunay, only: delaunay_t, nhist, hist_edges, &
                               sliver_ratio
   implicit none
   private
   public :: report_run, report_delaunay

contains

   subroutine report_run(ndim, nnodes, nelements, xic, yic, zic, &
                         iedge_off, jedge, area, vol, stats, zero, &
                         zero_keep, zero_tol)
      integer, intent(in) :: ndim
      integer(ik), intent(in) :: nnodes, nelements
      real(rk), intent(in) :: xic(nnodes), yic(nnodes), zic(nnodes)
      integer(ik), intent(in) :: iedge_off(nnodes + 1), jedge(*)
      real(rk), intent(in) :: area(:), vol(nnodes)
      type(assembly_stats_t), intent(in) :: stats
      logical, intent(in) :: zero(:), zero_keep
      real(rk), intent(in) :: zero_tol

      character(len=80) :: s
      integer(ik) :: nedges, i, k, nneg, nzero, kmin, inegv
      real(rk) :: cmin, c, sumv, rel
      character(len=6) :: vname

      nedges = iedge_off(nnodes + 1) - 1
      vname = 'volume'
      if (ndim == 2) vname = 'area'

      call log_line(' ')
      call log_line('del_vor summary')
      write (s, '(a,i14)') '  vertices           ', nnodes
      call log_line(s)
      if (ndim == 2) then
         write (s, '(a,i14)') '  triangles          ', nelements
      else
         write (s, '(a,i14)') '  tets               ', nelements
      end if
      call log_line(s)
      write (s, '(a,i14)') '  edges              ', nedges
      call log_line(s)
      if (ndim == 3) then
         write (s, '(a,i14)') '  positive tets      ', &
            stats%npositive
         call log_line(s)
         write (s, '(a,i14)') '  negative tets      ', &
            stats%nnegative
         call log_line(s)
      end if
      if (stats%ndegenerate > 0) then
         write (s, '(a,i14)') '  degenerate cells   ', &
            stats%ndegenerate
         call log_line(s)
         call log_line('  (no circumcenter; left out of the sums)')
      end if

      sumv = sum(vol)
      rel = 0.0_rk
      if (stats%cell_measure > 0.0_rk) &
         rel = (sumv - stats%cell_measure)/stats%cell_measure
      write (s, '(a,a6,1x,1pe20.12)') '  mesh ', vname, &
         stats%cell_measure
      call log_line(s)
      write (s, '(a,a6,1x,1pe20.12)') '  sum of control ', vname, sumv
      call log_line(s)
      write (s, '(a,1pe10.2)') '  relative difference', rel
      call log_line(s)
      inegv = count(vol < 0.0_rk)
      write (s, '(a,i14)') '  negative volumes   ', inegv
      call log_line(s)

      nneg = 0
      nzero = count(zero(1:nedges))
      cmin = huge(1.0_rk)
      kmin = 0
      do i = 1, nnodes
         do k = iedge_off(i), iedge_off(i + 1) - 1
            if (zero(k)) cycle
            c = area(k)/norm2([xic(jedge(k)) - xic(i), &
                               yic(jedge(k)) - yic(i), &
                               zic(jedge(k)) - zic(i)])
            if (area(k) < 0.0_rk) nneg = nneg + 1
            if (c < cmin) then
               cmin = c
               kmin = k
            end if
         end do
      end do
      write (s, '(a,i14,a,1pe8.1,a)') '  zero-area edges    ', nzero, &
         '  (zero_tol', zero_tol, ')'
      call log_line(s)
      if (zero_keep) then
         call log_line('  zero_area keep: written, one connection '// &
                       'per edge')
      else
         call log_line('  zero_area remove: left out of the '// &
                       'output files')
      end if
      write (s, '(a,i14)') '  negative A(ij)     ', nneg
      call log_line(s)
      if (kmin > 0) then
         write (s, '(a,1pe20.12)') '  minimum A(ij)/d(ij)', cmin
         call log_line(s)
      end if
      call log_line(' ')
   end subroutine report_run

   ! The Delaunay check: counts, the worst facet, a histogram of
   ! delta/L, boundary circumcenters, slivers. Returns fatal = .true.
   ! when a facet violates Delaunay beyond the tolerance.
   subroutine report_delaunay(nen, d, itetclr, fatal)
      integer(ik), intent(in) :: nen
      type(delaunay_t), intent(in) :: d
      integer(ik), intent(in) :: itetclr(:)
      logical, intent(out) :: fatal
      character(len=80) :: s
      character(len=12) :: lo, hi
      character(len=8) :: fname
      integer :: b

      fname = 'faces'
      if (nen == 3) fname = 'edges'
      fatal = sum(d%nfatal) > 0
      call log_line(' ')
      write (s, '(a,1pe8.1,a,1pe8.1,a)') 'del_vor Delaunay check '// &
         '(delaunay_tol', d%tol, ', delaunay_fatal', d%fatal_tol, ')'
      call log_line(s)
      write (s, '(a,t33,i12)') '  interior '//trim(fname)//' checked', &
         d%nchecked(1)
      call log_line(s)
      if (d%nchecked(2) > 0) then
         write (s, '(a,t33,i12)') '  interface '//trim(fname)// &
            ' checked', d%nchecked(2)
         call log_line(s)
      end if
      write (s, '(a,t33,i12)') '  ties (|delta/L| <= tol)', d%nties
      call log_line(s)
      write (s, '(a,t33,i12)') '  violations                   ', &
         sum(d%nviol)
      call log_line(s)
      if (d%nviol(2) > 0) then
         write (s, '(a,t33,i12)') '    across material interfaces ', &
            d%nviol(2)
         call log_line(s)
      end if
      write (s, '(a,t33,i12)') '    beyond delaunay_fatal      ', &
         sum(d%nfatal)
      call log_line(s)
      if (d%worst_cell(1) > 0) then
         write (s, '(a,1pe12.4,a,i10,a,i10)') '  smallest delta/L', &
            d%worst, '  cells', d%worst_cell(1), ' and', &
            d%worst_cell(2)
         call log_line(s)
         write (s, '(a,3(1pe14.6),a,i6)') '    at', d%worst_x, &
            '  itetclr', itetclr(d%worst_cell(1))
         call log_line(s)
      end if
      call log_line('  delta/L histogram')
      do b = 1, nhist
         lo = edge_label(b - 1)
         hi = edge_label(b)
         write (s, '(a,a9,a,a9,t33,i12)') '    ', lo, ' to ', hi, &
            d%hist(b)
         call log_line(s)
      end do
      write (s, '(a,t33,i12)') '  boundary '//trim(fname)//' checked', &
         d%nbound
      call log_line(s)
      write (s, '(a,t33,i12)') '  boundary circumcenter outside', &
         d%nbound_out
      call log_line(s)
      if (d%nbound_out > 0) then
         write (s, '(a,1pe12.4,a,i10)') '    farthest outside, /L', &
            d%bound_worst, '  cell', d%bound_worst_cell
         call log_line(s)
         call log_line('    (negative boundary coefficients are '// &
                       'likely there; reported, not fatal)')
      end if
      write (s, '(a,i0,a,t33,i12)') '  slivers (R/shortest edge > ', &
         nint(sliver_ratio), ')', d%nsliver
      call log_line(s)
      write (s, '(a,t33,1pe12.4)') '  largest R / shortest edge    ', &
         d%max_ratio
      call log_line(s)
      if (fatal) then
         call log_line('  Delaunay check FAILED: the mesh is not '// &
                       'Delaunay beyond delaunay_fatal;')
         call log_line('  no Voronoi coefficients are written. See '// &
                       'the violations file.')
      else if (sum(d%nviol) > 0) then
         write (s, '(a,i0,a,a,1pe8.1,a)') '  WARNING: ', &
            sum(d%nviol), ' '//trim(fname)//' slightly not ', &
            'Delaunay (delta/L >= ', -d%fatal_tol, '):'
         call log_line(s)
         call log_line('    nearly co-circular / co-spherical; the '// &
                       'coefficients there are')
         call log_line('    accurate to about |delta/L| of a '// &
                       'typical coefficient (violations file)')
         call log_line('  Delaunay check passed (with the warning '// &
                       'above)')
      else
         call log_line('  Delaunay check passed')
      end if
   contains

      ! label of histogram edge k: -inf before the first, +inf after
      ! the last
      function edge_label(k) result(t)
         integer, intent(in) :: k
         character(len=12) :: t
         if (k < 1) then
            t = '     -inf'
         else if (k > nhist - 1) then
            t = '     +inf'
         else
            write (t, '(es9.1)') &
               hist_edges(min(max(k, 1), nhist - 1))
         end if
      end function edge_label

   end subroutine report_delaunay

end module del_vor_report
