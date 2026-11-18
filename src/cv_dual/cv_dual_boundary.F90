!-----------------------------------------------------------------------
! cv_dual_boundary: the boundary vector area of each vertex, the part
! of its control volume on the mesh boundary, from exactly the dual
! points the coefficients came from (cv_dual_policy: Voronoi, median,
! blend or hybrid; cv_dual_tess draws the same pieces).
!
! Tets: each exterior face (a b c), wound outward, with its face point
! F gives vertex a the quadrilateral (a, m(ab), F, m(ca)), vector area
! 0.5 (F - a) x (m(ca) - m(ab)); likewise b and c. The three add up to
! the face's area vector for any F in the face's plane. With cv
! voronoi F is the face's circumcenter, as LaGriT's get_xcontab
! ('area_voronoi'); a circumcenter outside the face makes a negative
! piece. With cv median F is the centroid: each vertex a third.
! Triangles (2D, fracture networks): each exterior edge (a b) gives a
! and b each half of it as a length vector, normal to the edge in the
! triangle's plane, outward. The cell point is not used (the boundary
! part of a 2D control volume runs from the vertex to the midpoint).
!
! Exterior: jtet = mbndry. Interfaces (material, fracture
! intersections) are not boundary.
!-----------------------------------------------------------------------
module cv_dual_boundary
   use cv_dual_kinds, only: ik, rk
   use cv_dual_log, only: errmsg
   use cv_dual_mesh, only: mesh_t
   use cv_dual_assemble, only: cell_policy_t
   use cv_dual_tess, only: cell_points, boundary_vertices
   use cv_dual_geometry, only: cross
   use cv_dual_jtet, only: iface_tri, iface_tet
   implicit none
   private
   public :: boundary_areas, write_outside_area

contains

   ! barea(:, i): the boundary vector area of vertex i (0 inside).
   subroutine boundary_areas(m, pol, barea)
      type(mesh_t), intent(in) :: m
      type(cell_policy_t), intent(in) :: pol
      real(rk), intent(out) :: barea(:, :)
      integer(ik) :: it, iv(3)
      integer :: k, j, jn, jp
      real(rk) :: x(3, 3), xo(3), pc(3), pf(3, 4), f(3), n(3), d(3)
      real(rk) :: mn(3), mp(3), e(3), half(3), dn

      barea = 0.0_rk
      do it = 1, m%nelements
         do k = 1, int(m%nen)
            if (m%jtet(m%jtetoff(it) + k) /= m%mbndry) cycle
            if (m%nen == 4) then
               do j = 1, 3
                  iv(j) = m%itet(m%itetoff(it) + iface_tet(j, k))
                  x(:, j) = [m%xic(iv(j)), m%yic(iv(j)), m%zic(iv(j))]
               end do
               xo = vtx(m%itet(m%itetoff(it) + k))
               ! wind the face outward: away from the opposite vertex
               n = cross(x(:, 2) - x(:, 1), x(:, 3) - x(:, 1))
               if (dot_product(n, x(:, 1) - xo) < 0.0_rk) then
                  call swap3(x, iv)
               end if
               call cell_points(m, pol, it, pc, pf)
               f = pf(:, k)
               do j = 1, 3
                  jn = mod(j, 3) + 1
                  jp = mod(j + 1, 3) + 1
                  mn = 0.5_rk*(x(:, j) + x(:, jn))
                  mp = 0.5_rk*(x(:, j) + x(:, jp))
                  barea(:, iv(j)) = barea(:, iv(j)) + &
                     0.5_rk*cross(f - x(:, j), mp - mn)
               end do
            else
               do j = 1, 2
                  iv(j) = m%itet(m%itetoff(it) + iface_tri(j, k))
                  x(:, j) = vtx(iv(j))
               end do
               xo = vtx(m%itet(m%itetoff(it) + k))
               e = x(:, 2) - x(:, 1)
               n = cross(e, xo - x(:, 1))       ! the triangle's normal
               d = cross(e, n)                  ! in its plane, normal to e
               dn = sqrt(dot_product(d, d))
               if (dn <= 0.0_rk) cycle
               if (dot_product(d, x(:, 1) - xo) < 0.0_rk) d = -d
               half = 0.5_rk*sqrt(dot_product(e, e))*d/dn
               barea(:, iv(1)) = barea(:, iv(1)) + half
               barea(:, iv(2)) = barea(:, iv(2)) + half
            end if
         end do
      end do

   contains

      function vtx(i) result(p)
         integer(ik), intent(in) :: i
         real(rk) :: p(3)
         p = [m%xic(i), m%yic(i), m%zic(i)]
      end function vtx

      subroutine swap3(x, iv)
         real(rk), intent(inout) :: x(3, 3)
         integer(ik), intent(inout) :: iv(3)
         real(rk) :: t(3)
         integer(ik) :: ti
         t = x(:, 2)
         x(:, 2) = x(:, 3)
         x(:, 3) = t
         ti = iv(2)
         iv(2) = iv(3)
         iv(3) = ti
      end subroutine swap3

   end subroutine boundary_areas

   ! The outside_area file: one line per boundary vertex (on an
   ! exterior facet): vertex, Ax, Ay, Az (17 digits); then the sums.
   subroutine write_outside_area(fname, m, barea, policy, ierr)
      character(len=*), intent(in) :: fname, policy
      type(mesh_t), intent(in) :: m
      real(rk), intent(in) :: barea(:, :)
      integer, intent(out) :: ierr
      logical, allocatable :: bv(:)
      integer(ik) :: i
      integer :: lu, ios
      real(rk) :: s(3)

      ierr = 0
      allocate (bv(m%nnodes))
      call boundary_vertices(m, bv)
      open (newunit=lu, file=fname, status='replace', action='write', &
            iostat=ios)
      if (ios /= 0) then
         ierr = 1
         errmsg = 'cv_dual: cannot open '//trim(fname)
         return
      end if
      write (lu, '(a,i0,a)') '# cv_dual outside_area: '//trim(policy)// &
         ' boundary vector areas, ', count(bv), ' boundary vertices'
      write (lu, '(a)') '#     vertex                      Ax'// &
         '                        Ay                        Az'
      s = 0.0_rk
      do i = 1, m%nnodes
         if (.not. bv(i)) cycle
         write (lu, '(i11,3(2x,1pe24.16))') i, barea(:, i)
         s = s + barea(:, i)
      end do
      write (lu, '(a,3(2x,1pe24.16))') '# sum      ', s
      close (lu)
   end subroutine write_outside_area

end module cv_dual_boundary
