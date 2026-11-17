!-----------------------------------------------------------------------
! del_vor_policy: where each cell's dual points go (design section 9).
!
! A cell T gets a weight alpha(T) in [0, 1] and its point is
!
!   P(T) = G(T) + alpha(T) (C(T) - G(T))
!
! with C the circumcenter and G the centroid. alpha = 1 gives C and
! alpha = 0 gives G exactly (not through the formula), so cells left
! at alpha = 1 give bit for bit the Voronoi result. In 3D each face
! point is the same blend of the face's circumcenter and centroid with
! alpha(f) = min(alpha(T1), alpha(T2)) over the face's two tets, so
! the two halves of a dual face meet; edge midpoints never move.
!
!   policy     alpha
!   voronoi    1 in every cell (no alpha array)
!   median     0 in every cell
!   blend      the same value in every cell (keyword alpha)
!   hybrid     per cell, the largest that gives no negative
!              coefficient (del_vor_hybrid)
!
! Stage A of the hybrid (closed form): for a cell with an exterior
! boundary facet k beyond which its circumcenter lies (barycentric
! coordinate lam(k) < 0), alpha is set so that P(T) lands on that
! facet:  alpha = b / (b - lam(k)),  b = 1/(d+1) the centroid's
! coordinate (1/3 for triangles, 1/4 for tets); the smallest over
! such facets. With clamp on, a tet's boundary face also clamps its
! face point into the face triangle by the same formula with b = 1/3.
!
! All coordinates are relative to the cell's vertex 1 (the caller
! shifts them).
!-----------------------------------------------------------------------
module del_vor_policy
   use del_vor_kinds, only: ik, rk
   use del_vor_geometry, only: circumcenter_tri, circumcenter_tet, &
                               bary_tri, bary_tet
   use del_vor_jtet, only: iface_tet
   implicit none
   private
   public :: dual_points_tri, dual_points_tet, face_alphas
   public :: stage_a_alpha_tri, stage_a_alpha_tet, clamp_alpha
   public :: policy_voronoi, policy_median, policy_blend, &
             policy_hybrid, policy_code

   integer, parameter :: policy_voronoi = 1
   integer, parameter :: policy_median = 2
   integer, parameter :: policy_blend = 3
   integer, parameter :: policy_hybrid = 4

contains

   ! The code of a cv keyword (0 if unknown).
   pure integer function policy_code(cv)
      character(len=*), intent(in) :: cv
      select case (cv)
      case ('voronoi')
         policy_code = policy_voronoi
      case ('median')
         policy_code = policy_median
      case ('blend')
         policy_code = policy_blend
      case ('hybrid')
         policy_code = policy_hybrid
      case default
         policy_code = 0
      end select
   end function policy_code

   ! G + a (C - G), with the two ends exact.
   pure function blend(g, c, a) result(p)
      real(rk), intent(in) :: g(3), c(3), a
      real(rk) :: p(3)
      if (a >= 1.0_rk) then
         p = c
      else if (a <= 0.0_rk) then
         p = g
      else
         p = g + a*(c - g)
      end if
   end function blend

   ! The weight that puts the blend on the facet where the
   ! circumcenter's barycentric coordinate is lam < 0; b is the
   ! centroid's coordinate, 1/(d+1).
   pure real(rk) function clamp_alpha(b, lam)
      real(rk), intent(in) :: b, lam
      clamp_alpha = b/(b - lam)
   end function clamp_alpha

   ! Triangle x(:,1:3); cell weight a.
   pure subroutine dual_points_tri(x, a, pc, ok)
      real(rk), intent(in) :: x(3, 3), a
      real(rk), intent(out) :: pc(3)
      logical, intent(out) :: ok
      real(rk) :: c(3), g(3)
      g = (x(:, 1) + x(:, 2) + x(:, 3))/3.0_rk
      if (a <= 0.0_rk) then
         pc = g
         ok = .true.
         return
      end if
      call circumcenter_tri(x(:, 1), x(:, 2), x(:, 3), c, ok)
      pc = blend(g, c, a)
   end subroutine dual_points_tri

   ! Tet x(:,1:4); cell weight a, face weights af(k) for the face
   ! opposite vertex k. pf(:,k) is that face's point.
   pure subroutine dual_points_tet(x, a, af, pc, pf, ok)
      real(rk), intent(in) :: x(3, 4), a, af(4)
      real(rk), intent(out) :: pc(3), pf(3, 4)
      logical, intent(out) :: ok
      real(rk) :: c(3), g(3), y(3, 3)
      logical :: okf
      integer :: k
      ok = .true.
      g = 0.25_rk*(x(:, 1) + x(:, 2) + x(:, 3) + x(:, 4))
      if (a <= 0.0_rk) then
         pc = g
      else
         call circumcenter_tet(x(:, 1), x(:, 2), x(:, 3), x(:, 4), &
                               c, ok)
         pc = blend(g, c, a)
      end if
      do k = 1, 4
         y = x(:, iface_tet(:, k))
         g = (y(:, 1) + y(:, 2) + y(:, 3))/3.0_rk
         if (af(k) <= 0.0_rk) then
            pf(:, k) = g
         else
            call circumcenter_tri(y(:, 1), y(:, 2), y(:, 3), c, okf)
            ok = ok .and. okf
            pf(:, k) = blend(g, c, af(k))
         end if
      end do
   end subroutine dual_points_tet

   ! Face weights of tet it: af(k) = min(alpha(it), alpha(neighbor))
   ! for an interior or interface face; for an exterior face
   ! alpha(it), and with clamp also no more than the weight that keeps
   ! the face point inside the face triangle (but not below amin).
   pure subroutine face_alphas(it, x, alpha, jtetoff, jtet, mbndry, &
                               clamp, amin, af)
      integer(ik), intent(in) :: it, jtetoff(:), jtet(:), mbndry
      real(rk), intent(in) :: x(3, 4), alpha(:), amin
      logical, intent(in) :: clamp
      real(rk), intent(out) :: af(4)
      integer(ik) :: code, jt
      integer :: k
      real(rk) :: y(3, 3), c(3), lam(3)
      logical :: ok
      do k = 1, 4
         code = jtet(jtetoff(it) + k)
         if (code == mbndry) then
            af(k) = alpha(it)
            if (clamp .and. af(k) > 0.0_rk) then
               y = x(:, iface_tet(:, k))
               call circumcenter_tri(y(:, 1), y(:, 2), y(:, 3), c, ok)
               if (ok) call bary_tri(y, c, lam, ok)
               if (ok .and. minval(lam) < 0.0_rk) af(k) = min(af(k), &
                  max(amin, clamp_alpha(1.0_rk/3.0_rk, minval(lam))))
            end if
         else
            if (code > mbndry) code = code - mbndry
            jt = (code - 1)/4 + 1
            af(k) = min(alpha(it), alpha(jt))
         end if
      end do
   end subroutine face_alphas

   ! Stage A for a triangle: 1, or the weight that puts the point on
   ! the exterior edge(s) its circumcenter lies beyond. bnd(k) is true
   ! for an exterior edge opposite vertex k.
   pure real(rk) function stage_a_alpha_tri(x, bnd) result(a)
      real(rk), intent(in) :: x(3, 3)
      logical, intent(in) :: bnd(3)
      real(rk) :: c(3), lam(3)
      logical :: ok
      integer :: k
      a = 1.0_rk
      call circumcenter_tri(x(:, 1), x(:, 2), x(:, 3), c, ok)
      if (ok) call bary_tri(x, c, lam, ok)
      if (.not. ok) return
      do k = 1, 3
         if (bnd(k) .and. lam(k) < 0.0_rk) &
            a = min(a, clamp_alpha(1.0_rk/3.0_rk, lam(k)))
      end do
   end function stage_a_alpha_tri

   ! Stage A for a tet; bnd(k) true for an exterior face opposite k.
   pure real(rk) function stage_a_alpha_tet(x, bnd) result(a)
      real(rk), intent(in) :: x(3, 4)
      logical, intent(in) :: bnd(4)
      real(rk) :: c(3), lam(4)
      logical :: ok
      integer :: k
      a = 1.0_rk
      call circumcenter_tet(x(:, 1), x(:, 2), x(:, 3), x(:, 4), c, ok)
      if (ok) call bary_tet(x, c, lam, ok)
      if (.not. ok) return
      do k = 1, 4
         if (bnd(k) .and. lam(k) < 0.0_rk) &
            a = min(a, clamp_alpha(0.25_rk, lam(k)))
      end do
   end function stage_a_alpha_tet

end module del_vor_policy
