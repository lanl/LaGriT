!-----------------------------------------------------------------------
! del_vor_assemble: accumulate the cells' contributions into the edge
! CSR, one cell at a time. Nothing is stored per cell.
!
!   area(k)   scalar area of the dual face of edge k (the projection of
!             its vector area on the edge direction); 2D: a length
!   vol(i)    control volume of vertex i; 2D: an area
!   area_vec  optional, (3,nedges): vector area of each dual face,
!             oriented from the lower to the higher vertex number
!   asum      optional, (nedges): the sum of |each cell's piece| of
!             area(k), the scale against which the hybrid judges a
!             negative area(k) (del_vor_hybrid)
!
! The dual points come from pol (del_vor_policy): with no pol, or no
! alpha in it, they are the circumcenters (Voronoi). With alpha, a
! 3D run also needs jtet to give each face its weight.
!
! cell_pieces gives one cell's pieces; the hybrid uses it to take a
! cell's old pieces out and put its new ones in.
!
! Summation order is fixed, so results are the same run to run.
!-----------------------------------------------------------------------
module del_vor_assemble
   use del_vor_kinds, only: ik, rk
   use del_vor_geometry, only: kernel_tri, kernel_tet, tet_det, &
                               tri_area, iedge_tri, iedge_tet
   use del_vor_policy, only: dual_points_tri, dual_points_tet, &
                             face_alphas, policy_voronoi
   use del_vor_topology, only: edge_index
   use del_vor_log, only: errmsg
   implicit none
   private
   public :: assemble, assembly_stats_t, zero_area_mask
   public :: cell_policy_t, cell_pieces, pieces_t

   type :: assembly_stats_t
      integer(ik) :: ndegenerate = 0  ! cells with no circumcenter
      ! tets of positive and negative volume (LaGriT's sign:
      ! det(x2-x1, x3-x1, x4-x1) > 0 is positive)
      integer(ik) :: npositive = 0
      integer(ik) :: nnegative = 0
      real(rk) :: cell_measure = 0.0_rk  ! sum of cell volumes (areas)
   end type assembly_stats_t

   ! The dual points of every cell.
   type :: cell_policy_t
      integer :: code = policy_voronoi
      ! hybrid: also clamp exterior face points into their triangles
      logical :: clamp = .false.
      ! the lowest weight the clamp may give (alpha_min)
      real(rk) :: amin = 0.0_rk
      ! one weight per cell; not allocated = 1 everywhere (Voronoi)
      real(rk), allocatable :: alpha(:)
   end type cell_policy_t

   ! One cell's pieces, per local edge e = 1..ne (set by cell_pieces;
   ! no default values, so that a call does not clear them all first:
   ! this record is filled once per cell, a million times a second)
   type :: pieces_t
      integer :: ne
      logical :: ok                    ! false: degenerate, no pieces
      integer(ik) :: k(6)              ! the edge in the CSR
      integer(ik) :: ga(6), gb(6)      ! its two vertices
      real(rk) :: c(6)                 ! piece of area(k)
      real(rk) :: wv(6)                ! volume to each of ga, gb
      real(rk) :: av(3, 6)             ! vector area, from ga to gb
      real(rk) :: measure              ! |cell volume| (area)
      real(rk) :: det                  ! tets: tet_det
   end type pieces_t

contains

   subroutine assemble(nnodes, nelements, nen, xic, yic, zic, &
                       itetoff, itet, iedge_off, jedge, area, vol, &
                       stats, ierr, area_vec, pol, jtetoff, jtet, &
                       mbndry, asum)
      integer(ik), intent(in) :: nnodes, nelements, nen
      real(rk), intent(in) :: xic(nnodes), yic(nnodes), zic(nnodes)
      integer(ik), intent(in) :: itetoff(nelements), itet(*)
      integer(ik), intent(in) :: iedge_off(nnodes + 1), jedge(*)
      real(rk), intent(out) :: area(:), vol(nnodes)
      type(assembly_stats_t), intent(out) :: stats
      integer, intent(out) :: ierr
      real(rk), intent(inout), optional :: area_vec(:, :)
      type(cell_policy_t), intent(in), optional :: pol
      integer(ik), intent(in), optional :: jtetoff(:), jtet(:), mbndry
      real(rk), intent(inout), optional :: asum(:)

      type(pieces_t) :: p
      integer(ik) :: it
      type(cell_policy_t) :: vor

      ierr = 0
      area = 0.0_rk
      vol = 0.0_rk
      if (present(area_vec)) area_vec = 0.0_rk
      if (present(asum)) asum = 0.0_rk

      do it = 1, nelements
         if (present(pol)) then
            call cell_pieces(it, nnodes, nen, xic, yic, zic, itetoff, &
                             itet, iedge_off, jedge, pol, p, ierr, &
                             jtetoff, jtet, mbndry)
         else
            call cell_pieces(it, nnodes, nen, xic, yic, zic, itetoff, &
                             itet, iedge_off, jedge, vor, p, ierr)
         end if
         if (ierr /= 0) return
         stats%cell_measure = stats%cell_measure + p%measure
         if (nen == 4) then
            if (p%det > 0.0_rk) stats%npositive = stats%npositive + 1
            if (p%det < 0.0_rk) stats%nnegative = stats%nnegative + 1
         end if
         if (.not. p%ok) then
            stats%ndegenerate = stats%ndegenerate + 1
            cycle
         end if
         call add_pieces(p, 1.0_rk, area, vol, area_vec, asum)
      end do
   end subroutine assemble

   ! Add (sgn = 1) or take out (sgn = -1) one cell's pieces.
   subroutine add_pieces(p, sgn, area, vol, area_vec, asum)
      type(pieces_t), intent(in) :: p
      real(rk), intent(in) :: sgn
      real(rk), intent(inout) :: area(:), vol(:)
      real(rk), intent(inout), optional :: area_vec(:, :), asum(:)
      integer :: e
      integer(ik) :: k
      do e = 1, p%ne
         k = p%k(e)
         area(k) = area(k) + sgn*p%c(e)
         vol(p%ga(e)) = vol(p%ga(e)) + sgn*p%wv(e)
         vol(p%gb(e)) = vol(p%gb(e)) + sgn*p%wv(e)
         if (present(asum)) asum(k) = asum(k) + sgn*abs(p%c(e))
         if (present(area_vec)) then
            if (p%ga(e) < p%gb(e)) then
               area_vec(:, k) = area_vec(:, k) + sgn*p%av(:, e)
            else
               area_vec(:, k) = area_vec(:, k) - sgn*p%av(:, e)
            end if
         end if
      end do
   end subroutine add_pieces

   ! The pieces of cell it under policy pol. a_cell, if given,
   ! replaces the cell's own weight (the hybrid's trial values; the
   ! face weights still come from pol%alpha).
   subroutine cell_pieces(it, nnodes, nen, xic, yic, zic, itetoff, &
                          itet, iedge_off, jedge, pol, p, ierr, &
                          jtetoff, jtet, mbndry, a_cell)
      integer(ik), intent(in) :: it, nnodes, nen
      real(rk), intent(in) :: xic(nnodes), yic(nnodes), zic(nnodes)
      integer(ik), intent(in) :: itetoff(:), itet(*)
      integer(ik), intent(in) :: iedge_off(nnodes + 1), jedge(*)
      type(cell_policy_t), intent(in) :: pol
      type(pieces_t), intent(out) :: p
      integer, intent(out) :: ierr
      integer(ik), intent(in), optional :: jtetoff(:), jtet(:), mbndry
      real(rk), intent(in), optional :: a_cell

      real(rk) :: x(3, 4), x0(3), pc(3), pf(3, 4)
      real(rk) :: avec(3, 6), wvol(6), e(3), a, af(4)
      integer(ik) :: iv(4)
      integer :: j, ie, ia, ib
      logical :: ok, okp

      ierr = 0
      p%det = 0.0_rk
      do j = 1, int(nen)
         iv(j) = itet(itetoff(it) + j)
      end do
      ! shift so vertex 1 is at the origin
      x0 = [xic(iv(1)), yic(iv(1)), zic(iv(1))]
      do j = 1, int(nen)
         x(:, j) = [xic(iv(j)), yic(iv(j)), zic(iv(j))] - x0
      end do

      a = 1.0_rk
      if (allocated(pol%alpha)) a = pol%alpha(it)
      if (present(a_cell)) a = a_cell

      if (nen == 3) then
         p%ne = 3
         p%measure = tri_area(x(:, 1:3))
         call dual_points_tri(x(:, 1:3), a, pc, okp)
         call kernel_tri(x(:, 1:3), pc, avec(:, 1:3), wvol(1:3), ok)
      else
         p%ne = 6
         p%det = tet_det(x)
         p%measure = abs(p%det)/6.0_rk
         af = 1.0_rk
         if (allocated(pol%alpha)) then
            call face_alphas(it, x, pol%alpha, jtetoff, jtet, mbndry, &
                             pol%clamp, pol%amin, af)
            if (present(a_cell)) af = min(af, a_cell)
         end if
         call dual_points_tet(x, a, af, pc, pf, okp)
         call kernel_tet(x, pc, pf, avec, wvol, ok)
      end if
      p%ok = ok .and. okp
      if (.not. p%ok) return

      do ie = 1, p%ne
         if (nen == 3) then
            ia = iedge_tri(1, ie)
            ib = iedge_tri(2, ie)
         else
            ia = iedge_tet(1, ie)
            ib = iedge_tet(2, ie)
         end if
         p%ga(ie) = iv(ia)
         p%gb(ie) = iv(ib)
         p%k(ie) = edge_index(iv(ia), iv(ib), nnodes, iedge_off, jedge)
         if (p%k(ie) == 0) then
            ierr = 1
            errmsg = 'assemble: edge missing from the CSR'
            return
         end if
         e = x(:, ib) - x(:, ia)
         p%c(ie) = dot_product(avec(:, ie), e)/norm2(e)
         p%wv(ie) = wvol(ie)
         p%av(:, ie) = avec(:, ie)
      end do
   end subroutine cell_pieces

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
