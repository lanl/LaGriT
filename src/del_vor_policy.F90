!-----------------------------------------------------------------------
! del_vor_policy: where each cell's dual points go.
!
! Phase 1 has the Voronoi policy only: the cell point is the cell's
! circumcenter and (tets) each face point is the face's circumcenter.
! Median and hybrid policies are added in phase 4 (design section 9);
! they change only these points, never the kernels.
!-----------------------------------------------------------------------
module del_vor_policy
   use del_vor_kinds, only: rk
   use del_vor_geometry, only: circumcenter_tri, circumcenter_tet
   implicit none
   private
   public :: dual_points_tri, dual_points_tet, policy_voronoi

   integer, parameter :: policy_voronoi = 1

contains

   ! x: the triangle's vertices, shifted so x(:,1) = 0.
   pure subroutine dual_points_tri(ipolicy, x, pc, ok)
      integer, intent(in) :: ipolicy
      real(rk), intent(in) :: x(3, 3)
      real(rk), intent(out) :: pc(3)
      logical, intent(out) :: ok
      select case (ipolicy)
      case default
         call circumcenter_tri(x(:, 1), x(:, 2), x(:, 3), pc, ok)
      end select
   end subroutine dual_points_tri

   ! x: the tet's vertices, shifted so x(:,1) = 0. pf(:,k) is the
   ! point of the face opposite vertex k.
   pure subroutine dual_points_tet(ipolicy, x, pc, pf, ok)
      integer, intent(in) :: ipolicy
      real(rk), intent(in) :: x(3, 4)
      real(rk), intent(out) :: pc(3), pf(3, 4)
      logical, intent(out) :: ok
      logical :: okf(4)
      select case (ipolicy)
      case default
         call circumcenter_tet(x(:, 1), x(:, 2), x(:, 3), x(:, 4), &
                               pc, ok)
         call circumcenter_tri(x(:, 2), x(:, 3), x(:, 4), pf(:, 1), &
                               okf(1))
         call circumcenter_tri(x(:, 1), x(:, 3), x(:, 4), pf(:, 2), &
                               okf(2))
         call circumcenter_tri(x(:, 1), x(:, 2), x(:, 4), pf(:, 3), &
                               okf(3))
         call circumcenter_tri(x(:, 1), x(:, 2), x(:, 3), pf(:, 4), &
                               okf(4))
         ok = ok .and. all(okf)
      end select
   end subroutine dual_points_tet

end module del_vor_policy
