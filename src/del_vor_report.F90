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
   implicit none
   private
   public :: report_run

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

end module del_vor_report
