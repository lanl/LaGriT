!-----------------------------------------------------------------------
! del_vor_assemble: accumulate the cells' contributions into the edge
! CSR, one cell at a time. Nothing is stored per cell.
!
!   area(k)   scalar area of the dual face of edge k (the projection of
!             its vector area on the edge direction); 2D: a length
!   vol(i)    control volume of vertex i; 2D: an area
!   area_vec  optional, (3,nedges): vector area of each dual face,
!             oriented from the lower to the higher vertex number
!
! Summation order is fixed, so results are the same run to run.
!-----------------------------------------------------------------------
module del_vor_assemble
   use del_vor_kinds, only: ik, rk
   use del_vor_geometry, only: kernel_tri, kernel_tet, tet_det, &
                               tri_area, iedge_tri, iedge_tet
   use del_vor_policy, only: dual_points_tri, dual_points_tet
   use del_vor_topology, only: edge_index
   use del_vor_log, only: errmsg
   implicit none
   private
   public :: assemble, assembly_stats_t, zero_area_mask

   type :: assembly_stats_t
      integer(ik) :: ndegenerate = 0  ! cells with no circumcenter
      ! tets of positive and negative volume (LaGriT's sign:
      ! det(x2-x1, x3-x1, x4-x1) > 0 is positive)
      integer(ik) :: npositive = 0
      integer(ik) :: nnegative = 0
      real(rk) :: cell_measure = 0.0_rk  ! sum of cell volumes (areas)
   end type assembly_stats_t

contains

   subroutine assemble(ipolicy, nnodes, nelements, nen, xic, yic, &
                       zic, itetoff, itet, iedge_off, jedge, area, &
                       vol, stats, ierr, area_vec)
      integer, intent(in) :: ipolicy
      integer(ik), intent(in) :: nnodes, nelements, nen
      real(rk), intent(in) :: xic(nnodes), yic(nnodes), zic(nnodes)
      integer(ik), intent(in) :: itetoff(nelements), itet(*)
      integer(ik), intent(in) :: iedge_off(nnodes + 1), jedge(*)
      real(rk), intent(out) :: area(:), vol(nnodes)
      type(assembly_stats_t), intent(out) :: stats
      integer, intent(out) :: ierr
      real(rk), intent(inout), optional :: area_vec(:, :)

      real(rk) :: x(3, 4), x0(3), pc(3), pf(3, 4)
      real(rk) :: avec(3, 6), wvol(6), e(3), det
      integer(ik) :: it, iv(4), ga, gb, k
      integer :: ie, ne, j, ia, ib
      logical :: ok, okp, want_vec

      ierr = 0
      area = 0.0_rk
      vol = 0.0_rk
      want_vec = present(area_vec)
      if (want_vec) area_vec = 0.0_rk
      ne = 3
      if (nen == 4) ne = 6

      do it = 1, nelements
         do j = 1, int(nen)
            iv(j) = itet(itetoff(it) + j)
         end do
         ! shift so vertex 1 is at the origin
         x0 = [xic(iv(1)), yic(iv(1)), zic(iv(1))]
         do j = 1, int(nen)
            x(:, j) = [xic(iv(j)), yic(iv(j)), zic(iv(j))] - x0
         end do

         if (nen == 3) then
            call dual_points_tri(ipolicy, x(:, 1:3), pc, okp)
            call kernel_tri(x(:, 1:3), pc, avec(:, 1:3), wvol(1:3), ok)
            stats%cell_measure = stats%cell_measure + &
                                 tri_area(x(:, 1:3))
         else
            call dual_points_tet(ipolicy, x, pc, pf, okp)
            call kernel_tet(x, pc, pf, avec, wvol, ok)
            det = tet_det(x)
            if (det > 0.0_rk) stats%npositive = stats%npositive + 1
            if (det < 0.0_rk) stats%nnegative = stats%nnegative + 1
            stats%cell_measure = stats%cell_measure + abs(det)/6.0_rk
         end if
         if (.not. (ok .and. okp)) then
            stats%ndegenerate = stats%ndegenerate + 1
            cycle
         end if

         do ie = 1, ne
            if (nen == 3) then
               ia = iedge_tri(1, ie)
               ib = iedge_tri(2, ie)
            else
               ia = iedge_tet(1, ie)
               ib = iedge_tet(2, ie)
            end if
            ga = iv(ia)
            gb = iv(ib)
            k = edge_index(ga, gb, nnodes, iedge_off, jedge)
            if (k == 0) then
               ierr = 1
               errmsg = 'assemble: edge missing from the CSR'
               return
            end if
            e = x(:, ib) - x(:, ia)
            area(k) = area(k) + dot_product(avec(:, ie), e)/norm2(e)
            vol(ga) = vol(ga) + wvol(ie)
            vol(gb) = vol(gb) + wvol(ie)
            if (want_vec) then
               if (ga < gb) then
                  area_vec(:, k) = area_vec(:, k) + avec(:, ie)
               else
                  area_vec(:, k) = area_vec(:, k) - avec(:, ie)
               end if
            end if
         end do
      end do
   end subroutine assemble

   ! zero(k) = .true. when edge k's dual face has zero area:
   ! |A| <= tol d^2 for tets (an area), |A| <= tol d for triangles
   ! (a length), d the edge length. Measured against each edge's own
   ! length, so the test means the same at every mesh size.
   subroutine zero_area_mask(nen, nnodes, xic, yic, zic, iedge_off, &
                             jedge, area, tol, zero)
      integer(ik), intent(in) :: nen, nnodes
      real(rk), intent(in) :: xic(nnodes), yic(nnodes), zic(nnodes)
      integer(ik), intent(in) :: iedge_off(nnodes + 1), jedge(*)
      real(rk), intent(in) :: area(:), tol
      logical, intent(out) :: zero(:)
      integer(ik) :: i, j, k
      real(rk) :: d
      do i = 1, nnodes
         do k = iedge_off(i), iedge_off(i + 1) - 1
            j = jedge(k)
            d = norm2([xic(j) - xic(i), yic(j) - yic(i), &
                       zic(j) - zic(i)])
            if (nen == 4) d = d*d
            zero(k) = abs(area(k)) <= tol*d
         end do
      end do
   end subroutine zero_area_mask

end module del_vor_assemble
