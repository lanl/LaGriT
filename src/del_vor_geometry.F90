!-----------------------------------------------------------------------
! del_vor_geometry: circumcenters, centroids and the cell kernels.
!
! All routines expect coordinates already shifted so that vertex 1 of
! the cell is at the origin (the caller subtracts it). This keeps the
! circumcenter well conditioned for meshes in large coordinate frames
! (UTM, 1e5 to 1e6 m) with small cells; see design doc section 4.
!
! Kernels (from lanl/voronoi PhaseAreaVolumeLength and
! PhaseAreaVolumeLength3D, with the dual points passed in):
!
!   kernel_tri  triangle in any orientation in 3D. Cell point pc.
!   kernel_tet  tetrahedron. Cell point pc, face points pf(:,k) for
!               the face opposite vertex k.
!
! Output, per local edge e in LaGriT order (tri: 12 13 23;
! tet: 12 13 14 23 24 34), oriented from the edge's first vertex a to
! its second vertex b:
!
!   avec(:,e)  vector area of this cell's piece of the dual face of
!              edge e (2D: a length, as a vector normal to the piece
!              in the triangle's plane). For Voronoi points it is
!              parallel to the edge.
!   wvol(e)    the volume (2D: area) this edge's dual piece gives to
!              each of its two vertices: avec . (x_b - x_a) / 6 in 3D,
!              / 4 in 2D. A vertex's share of the cell is the sum of
!              wvol over its edges. This is the same decomposition as
!              voronoi's sub-tets (apex at the vertex, base the dual
!              piece); it holds for any dual points.
!
! Results do not depend on the cell's orientation.
!-----------------------------------------------------------------------
module del_vor_geometry
   use del_vor_kinds, only: rk
   implicit none
   private
   public :: cross, circumcenter_tri, circumcenter_tet
   public :: kernel_tri, kernel_tet, tet_det, tri_area
   public :: bary_tri, bary_tet
   public :: iedge_tri, iedge_tet

   ! LaGriT local edge tables (ielmedge1 in blockcom.h)
   integer, parameter :: iedge_tri(2, 3) = &
                         reshape([1, 2, 1, 3, 2, 3], [2, 3])
   integer, parameter :: iedge_tet(2, 6) = reshape( &
                         [1, 2, 1, 3, 1, 4, 2, 3, 2, 4, 3, 4], [2, 6])

   ! For tet edge (a,b): c, d such that (a,b,c,d) is an even
   ! permutation of (1,2,3,4).
   integer, parameter :: icd_tet(2, 6) = reshape( &
                         [3, 4, 4, 2, 2, 3, 1, 4, 3, 1, 1, 2], [2, 6])

   ! For triangle edge (a,b): +1 if (a,b,c) is a cyclic order of 123.
   real(rk), parameter :: sgn_tri(3) = [1.0_rk, -1.0_rk, 1.0_rk]

contains

   pure function cross(a, b) result(c)
      real(rk), intent(in) :: a(3), b(3)
      real(rk) :: c(3)
      c(1) = a(2)*b(3) - a(3)*b(2)
      c(2) = a(3)*b(1) - a(1)*b(3)
      c(3) = a(1)*b(2) - a(2)*b(1)
   end function cross

   ! det(x2-x1, x3-x1, x4-x1); six times the signed tet volume.
   pure function tet_det(x) result(d)
      real(rk), intent(in) :: x(3, 4)
      real(rk) :: d
      d = dot_product(x(:, 2) - x(:, 1), &
                      cross(x(:, 3) - x(:, 1), x(:, 4) - x(:, 1)))
   end function tet_det

   pure function tri_area(x) result(a)
      real(rk), intent(in) :: x(3, 3)
      real(rk) :: a
      a = 0.5_rk*norm2(cross(x(:, 2) - x(:, 1), x(:, 3) - x(:, 1)))
   end function tri_area

   ! Circumcenter of triangle p1 p2 p3 (any orientation in 3D),
   ! computed relative to p1. ok = .false. for a degenerate triangle.
   pure subroutine circumcenter_tri(p1, p2, p3, c, ok)
      real(rk), intent(in) :: p1(3), p2(3), p3(3)
      real(rk), intent(out) :: c(3)
      logical, intent(out) :: ok
      real(rk) :: a(3), b(3), axb(3), den
      a = p2 - p1
      b = p3 - p1
      axb = cross(a, b)
      den = 2.0_rk*dot_product(axb, axb)
      ok = den > 0.0_rk
      if (.not. ok) then
         c = p1
         return
      end if
      c = p1 + cross(dot_product(a, a)*b - dot_product(b, b)*a, axb) &
          /den
   end subroutine circumcenter_tri

   ! Barycentric coordinates lam(k) of point p in triangle x (any
   ! orientation in 3D; p taken in the triangle's plane): lam(k) is
   ! the signed area of the triangle with vertex k replaced by p, over
   ! the triangle's area. lam(k) < 0: p lies beyond the edge opposite
   ! vertex k. ok = .false. for a degenerate triangle.
   pure subroutine bary_tri(x, p, lam, ok)
      real(rk), intent(in) :: x(3, 3), p(3)
      real(rk), intent(out) :: lam(3)
      logical, intent(out) :: ok
      real(rk) :: n(3), nn
      n = cross(x(:, 2) - x(:, 1), x(:, 3) - x(:, 1))
      nn = dot_product(n, n)
      ok = nn > 0.0_rk
      if (.not. ok) then
         lam = 0.0_rk
         return
      end if
      lam(1) = dot_product(cross(x(:, 2) - p, x(:, 3) - p), n)/nn
      lam(2) = dot_product(cross(x(:, 3) - p, x(:, 1) - p), n)/nn
      lam(3) = 1.0_rk - lam(1) - lam(2)
   end subroutine bary_tri

   ! Barycentric coordinates lam(k) of point p in tet x: the signed
   ! volume with vertex k replaced by p, over the tet's volume.
   ! lam(k) < 0: p lies beyond the face opposite vertex k.
   pure subroutine bary_tet(x, p, lam, ok)
      real(rk), intent(in) :: x(3, 4), p(3)
      real(rk), intent(out) :: lam(4)
      logical, intent(out) :: ok
      real(rk) :: y(3, 4), det
      integer :: k
      det = tet_det(x)
      ok = det /= 0.0_rk
      if (.not. ok) then
         lam = 0.0_rk
         return
      end if
      do k = 2, 4
         y = x
         y(:, k) = p
         lam(k) = tet_det(y)/det
      end do
      lam(1) = 1.0_rk - lam(2) - lam(3) - lam(4)
   end subroutine bary_tet

   ! Circumcenter of tet p1..p4, relative to p1.
   pure subroutine circumcenter_tet(p1, p2, p3, p4, c, ok)
      real(rk), intent(in) :: p1(3), p2(3), p3(3), p4(3)
      real(rk), intent(out) :: c(3)
      logical, intent(out) :: ok
      real(rk) :: a(3), b(3), d(3), bxd(3), den
      a = p2 - p1
      b = p3 - p1
      d = p4 - p1
      bxd = cross(b, d)
      den = 2.0_rk*dot_product(a, bxd)
      ok = den /= 0.0_rk
      if (.not. ok) then
         c = p1
         return
      end if
      c = p1 + (dot_product(a, a)*bxd + dot_product(b, b)*cross(d, a) &
                + dot_product(d, d)*cross(a, b))/den
   end subroutine circumcenter_tet

   ! Triangle kernel. x(:,k) vertex k, pc the cell point.
   pure subroutine kernel_tri(x, pc, avec, wvol, ok)
      real(rk), intent(in) :: x(3, 3), pc(3)
      real(rk), intent(out) :: avec(3, 3), wvol(3)
      logical, intent(out) :: ok
      real(rk) :: nt(3), rn, xm(3), e(3)
      integer :: ie, ia, ib

      nt = cross(x(:, 2) - x(:, 1), x(:, 3) - x(:, 1))
      rn = norm2(nt)
      ok = rn > 0.0_rk
      if (.not. ok) then
         avec = 0.0_rk
         wvol = 0.0_rk
         return
      end if
      nt = nt/rn
      do ie = 1, 3
         ia = iedge_tri(1, ie)
         ib = iedge_tri(2, ie)
         xm = 0.5_rk*(x(:, ia) + x(:, ib))
         e = x(:, ib) - x(:, ia)
         avec(:, ie) = sgn_tri(ie)*cross(pc - xm, nt)
         wvol(ie) = 0.25_rk*dot_product(avec(:, ie), e)
      end do
   end subroutine kernel_tri

   ! Tet kernel. x(:,k) vertex k, pc the cell point, pf(:,k) the point
   ! of the face opposite vertex k.
   pure subroutine kernel_tet(x, pc, pf, avec, wvol, ok)
      real(rk), intent(in) :: x(3, 4), pc(3), pf(3, 4)
      real(rk), intent(out) :: avec(3, 6), wvol(6)
      logical, intent(out) :: ok
      real(rk) :: det, s, xm(3), e(3), fc(3), fd(3), rp(3)
      integer :: ie, ia, ib, ic, id

      det = tet_det(x)
      ok = det /= 0.0_rk
      if (.not. ok) then
         avec = 0.0_rk
         wvol = 0.0_rk
         return
      end if
      s = sign(1.0_rk, det)
      do ie = 1, 6
         ia = iedge_tet(1, ie)
         ib = iedge_tet(2, ie)
         ic = icd_tet(1, ie)
         id = icd_tet(2, ie)
         xm = 0.5_rk*(x(:, ia) + x(:, ib))
         e = x(:, ib) - x(:, ia)
         ! face (a,b,c) is the face opposite d, and the reverse
         fc = pf(:, id) - xm
         fd = pf(:, ic) - xm
         rp = pc - xm
         ! quadrilateral (m, F_abc, P, F_abd) as two triangles fanned
         ! at the edge midpoint m
         avec(:, ie) = 0.5_rk*s*(cross(fc, rp) + cross(rp, fd))
         wvol(ie) = dot_product(avec(:, ie), e)/6.0_rk
      end do
   end subroutine kernel_tet

end module del_vor_geometry
