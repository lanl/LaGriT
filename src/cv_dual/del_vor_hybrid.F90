!-----------------------------------------------------------------------
! del_vor_hybrid: the adaptive hybrid (design section 9): the largest
! alpha per cell that leaves no coefficient negative.
!
! Stage A (stage_a): closed form, one pass. A cell with an exterior
! boundary facet beyond which its circumcenter lies gets the alpha
! that puts its point on that facet (del_vor_policy), but not below
! alpha_min. Other cells keep alpha = 1.
!
! Stage B (stage_b): correction. An edge is negative (is_negative)
! when
!
!   area(k) < -hybrid_tol asum(k)   and   |area(k)| > zero_tol d^p
!
! with asum(k) the sum of |each cell's piece| of area(k), d the edge
! length, p = 2 for tets and 1 for triangles: the second condition is
! del_vor's zero-area test (zero_area_mask), so round-off on
! co-spherical ties, where the true area is 0, is not counted as
! negative. On each negative edge, the cell
! whose piece is most negative and whose alpha is above alpha_min has
! its alpha lowered to the largest value that makes the edge's total
! >= aim asum(k), aim = 1e-3, found by bisection on that cell's piece
! (its face weights follow, min with the neighbors). Aiming just above
! 0 keeps a neighbor's later move from pushing the edge back below:
! measured on 1 million random-point tets, 48 passes instead of more
! than 100, with alpha almost the same. The cells whose alpha
! changed, and in 3D their face neighbors (whose face points moved),
! then have their old pieces taken out and new ones put in. Passes
! repeat until no edge is negative, no alpha can move, or hybrid_iter
! passes.
! alpha only decreases and is bounded by alpha_min, so this ends.
! A last full assembly gives the final numbers.
!
! Every edge is checked: on a Delaunay mesh interior Voronoi
! coefficients are never negative, but moved points and nearly
! Delaunay meshes can make them so. Each negative edge is counted as
! boundary (on an exterior facet) or interior.
!
! Work per pass is one scan over the cells; only cells with a vertex
! on a negative edge compute anything. No vertex-to-cell lists.
!-----------------------------------------------------------------------
module del_vor_hybrid
   use del_vor_kinds, only: ik, rk
   use del_vor_assemble, only: cell_policy_t, cell_pieces, pieces_t
   use del_vor_policy, only: stage_a_alpha_tri, stage_a_alpha_tet
   use del_vor_jtet, only: iface_tri, iface_tet
   implicit none
   private
   public :: hybrid_t, stage_a, stage_b, boundary_edges, is_negative

   ! stage(it): 0 alpha = 1 untouched, 1 set by stage A, 2 by stage B
   real(rk), parameter :: aim = 1.0e-3_rk

   type :: hybrid_t
      integer(ik) :: nstage_a = 0      ! cells stage A lowered
      integer(ik) :: nstage_b = 0      ! cells stage B lowered
      integer :: npass = 0             ! stage B passes
      logical :: stuck = .false.       ! negative edges, no alpha left
      logical :: limit = .false.       ! stopped by hybrid_iter
   end type hybrid_t

contains

   ! Edge (i, j) with area a and sum of |pieces| asum is negative.
   pure logical function is_negative(a, asum, xi, xj, nen, tol, ztol)
      real(rk), intent(in) :: a, asum, xi(3), xj(3), tol, ztol
      integer(ik), intent(in) :: nen
      real(rk) :: d
      is_negative = .false.
      if (a >= -tol*asum) return
      d = norm2(xj - xi)
      if (nen == 4) d = d*d
      is_negative = abs(a) > ztol*d
   end function is_negative

   subroutine stage_a(nnodes, nelements, nen, xic, yic, zic, itetoff, &
                      itet, jtetoff, jtet, mbndry, alpha_min, alpha, &
                      stage, hs)
      integer(ik), intent(in) :: nnodes, nelements, nen
      real(rk), intent(in) :: xic(nnodes), yic(nnodes), zic(nnodes)
      integer(ik), intent(in) :: itetoff(:), itet(:), jtetoff(:), &
                                 jtet(:), mbndry
      real(rk), intent(in) :: alpha_min
      real(rk), intent(inout) :: alpha(:)
      integer, intent(inout) :: stage(:)
      type(hybrid_t), intent(inout) :: hs
      integer(ik) :: it, iv(4)
      integer :: j
      logical :: bnd(4)
      real(rk) :: x(3, 4), x0(3), a
      do it = 1, nelements
         do j = 1, int(nen)
            bnd(j) = jtet(jtetoff(it) + j) == mbndry
         end do
         if (.not. any(bnd(1:nen))) cycle
         do j = 1, int(nen)
            iv(j) = itet(itetoff(it) + j)
         end do
         x0 = [xic(iv(1)), yic(iv(1)), zic(iv(1))]
         do j = 1, int(nen)
            x(:, j) = [xic(iv(j)), yic(iv(j)), zic(iv(j))] - x0
         end do
         if (nen == 3) then
            a = stage_a_alpha_tri(x(:, 1:3), bnd(1:3))
         else
            a = stage_a_alpha_tet(x, bnd)
         end if
         a = max(a, alpha_min)
         if (a < alpha(it)) then
            alpha(it) = a
            stage(it) = 1
            hs%nstage_a = hs%nstage_a + 1
         end if
      end do
   end subroutine stage_a

   ! Stage B on the assembled area, vol, asum (updated in place; the
   ! caller assembles once more at the end for the final numbers).
   subroutine stage_b(nnodes, nelements, nen, xic, yic, zic, itetoff, &
                      itet, jtetoff, jtet, mbndry, iedge_off, jedge, &
                      pol, area, vol, asum, tol, ztol, alpha_min, &
                      maxiter, stage, hs, ierr)
      integer(ik), intent(in) :: nnodes, nelements, nen
      real(rk), intent(in) :: xic(nnodes), yic(nnodes), zic(nnodes)
      integer(ik), intent(in) :: itetoff(:), itet(:), jtetoff(:), &
                                 jtet(:), mbndry
      integer(ik), intent(in) :: iedge_off(nnodes + 1), jedge(:)
      type(cell_policy_t), intent(inout) :: pol
      real(rk), intent(inout) :: area(:), vol(:), asum(:)
      real(rk), intent(in) :: tol, ztol, alpha_min
      integer, intent(in) :: maxiter
      integer, intent(inout) :: stage(:)
      type(hybrid_t), intent(inout) :: hs
      integer, intent(out) :: ierr

      integer(ik), allocatable :: fl(:), best(:)
      real(rk), allocatable :: bestc(:)
      logical, allocatable :: vmark(:), affected(:)
      type(cell_policy_t) :: old
      type(pieces_t) :: p
      integer(ik) :: nf, k, it, jt, code, slot, nchanged, ir
      integer :: e, j, pass
      real(rk) :: anew

      ierr = 0
      allocate (vmark(nnodes), affected(nelements))
      old%code = pol%code
      old%clamp = pol%clamp
      old%amin = pol%amin

      do pass = 1, maxiter
         ! the negative edges, in increasing order
         nf = 0
         do ir = 1, size(iedge_off, kind=ik) - 1
            do k = iedge_off(ir), iedge_off(ir + 1) - 1
               if (neg(ir, k)) nf = nf + 1
            end do
         end do
         if (nf == 0) return
         hs%npass = pass
         if (allocated(fl)) deallocate (fl, best, bestc)
         allocate (fl(nf), best(nf), bestc(nf))
         nf = 0
         vmark = .false.
         do ir = 1, size(iedge_off, kind=ik) - 1
            do k = iedge_off(ir), iedge_off(ir + 1) - 1
               if (neg(ir, k)) then
                  nf = nf + 1
                  fl(nf) = k
                  vmark(ir) = .true.
                  vmark(jedge(k)) = .true.
               end if
            end do
         end do
         best = 0
         bestc = huge(1.0_rk)

         ! the most negative piece of each, among cells that can move
         do it = 1, nelements
            if (pol%alpha(it) <= alpha_min) cycle
            if (.not. any(vmark(itet(itetoff(it) + 1: &
                                     itetoff(it) + nen)))) cycle
            call cell_pieces(it, nnodes, nen, xic, yic, zic, itetoff, &
                             itet, iedge_off, jedge, pol, p, ierr, &
                             jtetoff, jtet, mbndry)
            if (ierr /= 0) return
            if (.not. p%ok) cycle
            do e = 1, p%ne
               slot = find(fl, p%k(e))
               if (slot == 0) cycle
               if (p%c(e) < bestc(slot)) then
                  bestc(slot) = p%c(e)
                  best(slot) = it
               end if
            end do
         end do

         ! lower those cells' alpha
         old%alpha = pol%alpha
         nchanged = 0
         do slot = 1, nf
            it = best(slot)
            if (it == 0) cycle
            if (bestc(slot) >= 0.0_rk) cycle
            k = fl(slot)
            anew = solve_alpha(it, k, area(k) - bestc(slot) - &
                               aim*asum(k))
            if (anew < pol%alpha(it)) then
               pol%alpha(it) = anew
               if (stage(it) /= 2) hs%nstage_b = hs%nstage_b + 1
               stage(it) = 2
               nchanged = nchanged + 1
            end if
         end do
         if (nchanged == 0) then
            hs%stuck = .true.
            return
         end if

         ! the cells whose pieces changed: those, and in 3D their
         ! face neighbors
         affected = pol%alpha /= old%alpha
         if (nen == 4) then
            do it = 1, nelements
               if (pol%alpha(it) == old%alpha(it)) cycle
               do j = 1, 4
                  code = jtet(jtetoff(it) + j)
                  if (code == mbndry) cycle
                  if (code > mbndry) code = code - mbndry
                  jt = (code - 1)/4 + 1
                  affected(jt) = .true.
               end do
            end do
         end if
         do it = 1, nelements
            if (.not. affected(it)) cycle
            call cell_pieces(it, nnodes, nen, xic, yic, zic, itetoff, &
                             itet, iedge_off, jedge, old, p, ierr, &
                             jtetoff, jtet, mbndry)
            if (ierr /= 0) return
            if (p%ok) call put(p, -1.0_rk)
            call cell_pieces(it, nnodes, nen, xic, yic, zic, itetoff, &
                             itet, iedge_off, jedge, pol, p, ierr, &
                             jtetoff, jtet, mbndry)
            if (ierr /= 0) return
            if (p%ok) call put(p, 1.0_rk)
         end do
      end do
      hs%limit = .true.

   contains

      logical function neg(i, kk)
         integer(ik), intent(in) :: i
         integer(ik), intent(in) :: kk
         integer(ik) :: jj
         jj = jedge(kk)
         neg = is_negative(area(kk), asum(kk), &
                           [xic(i), yic(i), zic(i)], &
                           [xic(jj), yic(jj), zic(jj)], nen, tol, ztol)
      end function neg

      subroutine put(q, sgn)
         type(pieces_t), intent(in) :: q
         real(rk), intent(in) :: sgn
         integer :: ee
         do ee = 1, q%ne
            area(q%k(ee)) = area(q%k(ee)) + sgn*q%c(ee)
            asum(q%k(ee)) = asum(q%k(ee)) + sgn*abs(q%c(ee))
            vol(q%ga(ee)) = vol(q%ga(ee)) + sgn*q%wv(ee)
            vol(q%gb(ee)) = vol(q%gb(ee)) + sgn*q%wv(ee)
         end do
      end subroutine put

      ! cell it's piece of edge k at weight a (faces follow)
      real(rk) function piece(it, k, a)
         integer(ik), intent(in) :: it, k
         real(rk), intent(in) :: a
         type(pieces_t) :: q
         integer :: ee, ie
         piece = 0.0_rk
         call cell_pieces(it, nnodes, nen, xic, yic, zic, itetoff, &
                          itet, iedge_off, jedge, pol, q, ie, &
                          jtetoff, jtet, mbndry, a)
         if (ie /= 0 .or. .not. q%ok) return
         do ee = 1, q%ne
            if (q%k(ee) == k) piece = q%c(ee)
         end do
      end function piece

      ! The largest a in [alpha_min, alpha(it)] with
      ! rest + piece(a) >= 0 (rest: the other cells' total), by
      ! bisection; alpha_min if none.
      real(rk) function solve_alpha(it, k, rest) result(a)
         integer(ik), intent(in) :: it, k
         real(rk), intent(in) :: rest
         real(rk) :: lo, hi, mid
         integer :: n
         lo = alpha_min
         hi = pol%alpha(it)
         if (rest + piece(it, k, lo) < 0.0_rk) then
            a = lo
            return
         end if
         do n = 1, 50
            mid = 0.5_rk*(lo + hi)
            if (rest + piece(it, k, mid) >= 0.0_rk) then
               lo = mid
            else
               hi = mid
            end if
            if (hi - lo <= 1.0e-12_rk) exit
         end do
         a = lo
      end function solve_alpha

   end subroutine stage_b

   ! bedge(k): edge k lies on an exterior facet (jtet = mbndry).
   subroutine boundary_edges(nnodes, nelements, nen, itetoff, itet, &
                             jtetoff, jtet, mbndry, iedge_off, jedge, &
                             bedge)
      use del_vor_topology, only: edge_index
      integer(ik), intent(in) :: nnodes, nelements, nen
      integer(ik), intent(in) :: itetoff(:), itet(:), jtetoff(:), &
                                 jtet(:), mbndry
      integer(ik), intent(in) :: iedge_off(nnodes + 1), jedge(:)
      logical, intent(out) :: bedge(:)
      integer(ik) :: it, iv(3), k
      integer :: j, a, b
      bedge = .false.
      do it = 1, nelements
         do j = 1, int(nen)
            if (jtet(jtetoff(it) + j) /= mbndry) cycle
            if (nen == 3) then
               iv(1:2) = itet(itetoff(it) + iface_tri(:, j))
               k = edge_index(iv(1), iv(2), nnodes, iedge_off, jedge)
               if (k > 0) bedge(k) = .true.
            else
               iv = itet(itetoff(it) + iface_tet(:, j))
               do a = 1, 2
                  do b = a + 1, 3
                     k = edge_index(iv(a), iv(b), nnodes, iedge_off, &
                                    jedge)
                     if (k > 0) bedge(k) = .true.
                  end do
               end do
            end if
         end do
      end do
   end subroutine boundary_edges

   ! Position of k in the sorted list fl (0 if absent).
   pure integer(ik) function find(fl, k)
      integer(ik), intent(in) :: fl(:), k
      integer(ik) :: lo, hi, mid
      find = 0
      lo = 1
      hi = size(fl, kind=ik)
      do while (lo <= hi)
         mid = (lo + hi)/2
         if (fl(mid) == k) then
            find = mid
            return
         else if (fl(mid) < k) then
            lo = mid + 1
         else
            hi = mid - 1
         end if
      end do
   end function find

end module del_vor_hybrid
