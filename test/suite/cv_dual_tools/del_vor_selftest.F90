!-----------------------------------------------------------------------
! del_vor_selftest: checks of the geometry kernels against answers
! worked out independently. Prints one "key value" line per quantity
! and, with a mesh, the closure check. Exit status 0 if every HARD
! check passes, 1 otherwise.
!
!   del_vor_selftest                  kernel checks only
!   del_vor_selftest MESH.inp ...     plus closure on each mesh
!
! Also writes selftest_samples.txt (in the current folder): one obtuse
! sample triangle, one sample tet and the closure error at each inner
! vertex, for the pictures on the results page.
!
! Checks (random cells from a fixed seed, so every machine sees the
! same cells):
!
! cot2d    Voronoi triangle: A(ab)/d(ab) = cot(angle at c)/2 for each
!          edge (the P1 finite-element stiffness, cotangent formula).
!          Triangles in random orientations in 3D.
! circ3d   tet circumcenter: equally far from the 4 vertices.
! dual3d   Voronoi tet: the dual piece of edge ab is the quadrilateral
!          (m, F_abc, C, F_abd), two right triangles (right angle at
!          the face circumcenters F), so
!            A(ab)/d(ab) = (cot(t_c) h_abc + cot(t_d) h_abd) / 4
!          with t_c the angle at c in face abc and h_abc the signed
!          distance of C from face abc toward the tet (from vertex
!          angles and distances, not from the kernel's cross
!          products).
! perp3d   Voronoi tet: each dual piece is perpendicular to its edge.
! p1_3d    MEASURED: in 3D the Voronoi A/d is NOT the P1 stiffness
!          -V grad(phi_a).grad(phi_b) (they agree only in 2D);
!          the largest difference, relative.
! part2d   any cell point: the three vertex shares add up to the
! part3d   triangle area (tet volume); random cell and face points.
! shift    the same tet at the origin and moved by (7e5, 3.2e6, 1.5e3)
!          (UTM-like), cell size about 1: largest difference of
!          A/d, relative to the largest |A/d|. Also measured with
!          lanl/voronoi's 4x4-determinant circumcenter (MEASURED, to
!          show why it was replaced).
! closure  on a mesh: the sum of dual-face vector areas around each
!          vertex not on the bounding box, relative to the largest
!          |vector area|. Must be 0 (the dual cell is closed).
!-----------------------------------------------------------------------
program del_vor_selftest
   use del_vor_kinds, only: ik, rk
   use del_vor_geometry, only: cross, kernel_tri, kernel_tet, &
                               circumcenter_tri, circumcenter_tet, &
                               tet_det, tri_area, iedge_tri, iedge_tet
   use del_vor_policy, only: dual_points_tri, dual_points_tet
   use del_vor_mesh, only: mesh_t
   use del_vor_read_avs, only: read_avs
   use del_vor_topology, only: build_edge_csr
   use del_vor_assemble, only: assemble, assembly_stats_t
   use del_vor_log, only: errmsg
   implicit none

   integer, parameter :: ncells = 2000
   real(rk), parameter :: tol_id = 1.0e-10_rk
   real(rk), parameter :: tol_shift = 1.0e-8_rk
   real(rk), parameter :: tol_closure = 1.0e-10_rk
   integer :: nfail, ia, nargs, lus
   character(len=1024) :: fname

   nfail = 0
   open (newunit=lus, file='selftest_samples.txt', status='replace', &
         action='write')
   call seed_fixed()
   call check_cot2d()
   call check_cot3d()
   call check_part2d()
   call check_part3d()
   call check_shift()
   nargs = command_argument_count()
   do ia = 1, nargs
      call get_command_argument(ia, fname)
      call check_closure(fname)
   end do
   if (nfail > 0) then
      close (lus)
      write (*, '(a,i0)') 'selftest_failures ', nfail
      error stop 1
   end if
   write (*, '(a)') 'selftest_failures 0'
   close (lus)

contains

   subroutine judge(key, val, tol)
      character(len=*), intent(in) :: key
      real(rk), intent(in) :: val, tol
      character(len=4) :: res
      res = 'PASS'
      if (.not. (val <= tol)) then
         res = 'FAIL'
         nfail = nfail + 1
      end if
      write (*, '(a,1x,1pe12.4,1x,a,1x,1pe9.2,1x,a)') key, val, &
         'tol', tol, res
   end subroutine judge

   subroutine seed_fixed()
      integer :: n
      integer, allocatable :: s(:)
      call random_seed(size=n)
      allocate (s(n))
      s = 20261107
      call random_seed(put=s)
   end subroutine seed_fixed

   ! random point in the unit cube
   function rnd3() result(p)
      real(rk) :: p(3)
      call random_number(p)
   end function rnd3

   ! random rotation (from a random unit quaternion)
   function rnd_rot() result(r)
      real(rk) :: r(3, 3), q(4)
      call random_number(q)
      q = q - 0.5_rk
      q = q/norm2(q)
      r(1, :) = [1 - 2*(q(3)**2 + q(4)**2), 2*(q(2)*q(3) - q(1)*q(4)), &
                 2*(q(2)*q(4) + q(1)*q(3))]
      r(2, :) = [2*(q(2)*q(3) + q(1)*q(4)), 1 - 2*(q(2)**2 + q(4)**2), &
                 2*(q(3)*q(4) - q(1)*q(2))]
      r(3, :) = [2*(q(2)*q(4) - q(1)*q(3)), 2*(q(3)*q(4) + q(1)*q(2)), &
                 1 - 2*(q(2)**2 + q(3)**2)]
   end function rnd_rot

   ! a random triangle in a random plane, not too flat
   subroutine rnd_tri(x)
      real(rk), intent(out) :: x(3, 3)
      real(rk) :: r(3, 3)
      integer :: k
      do
         r = rnd_rot()
         do k = 1, 3
            x(:, k) = matmul(r, [rnd3_2d(), 0.0_rk])
         end do
         if (tri_area(x) > 0.02_rk) exit
      end do
      do k = 3, 1, -1
         x(:, k) = x(:, k) - x(:, 1)
      end do
   end subroutine rnd_tri

   function rnd3_2d() result(p)
      real(rk) :: p(2)
      call random_number(p)
   end function rnd3_2d

   subroutine rnd_tet(x)
      real(rk), intent(out) :: x(3, 4)
      integer :: k
      do
         do k = 1, 4
            x(:, k) = rnd3()
         end do
         if (abs(tet_det(x)) > 0.05_rk) exit
      end do
      do k = 4, 1, -1
         x(:, k) = x(:, k) - x(:, 1)
      end do
   end subroutine rnd_tet

   subroutine check_cot2d()
      real(rk) :: x(3, 3), pc(3), avec(3, 3), wv(3), e(3), u(3), v(3)
      real(rk) :: got, want, err, scale, gv(3), wt(3)
      integer :: n, ie, ia, ib, ic
      logical :: ok, okp, done
      err = 0.0_rk
      scale = 0.0_rk
      done = .false.
      do n = 1, ncells
         call rnd_tri(x)
         call dual_points_tri(x, 1.0_rk, pc, okp)
         call kernel_tri(x, pc, avec, wv, ok)
         do ie = 1, 3
            ia = iedge_tri(1, ie)
            ib = iedge_tri(2, ie)
            ic = 6 - ia - ib
            e = x(:, ib) - x(:, ia)
            got = dot_product(avec(:, ie), e)/dot_product(e, e)
            u = x(:, ia) - x(:, ic)
            v = x(:, ib) - x(:, ic)
            want = 0.5_rk*dot_product(u, v)/norm2(cross(u, v))
            err = max(err, abs(got - want))
            scale = max(scale, abs(want))
            gv(ie) = got
            wt(ie) = want
         end do
         ! the first obtuse triangle, for the picture
         if (.not. done .and. minval(wt) < -0.05_rk .and. &
             maxval(wt) < 1.5_rk) then
            write (lus, '(a,24(1x,es23.15))') 'tri', x, pc, gv, wt
            done = .true.
         end if
      end do
      call judge('cot2d_max_rel_diff', err/scale, tol_id)
   end subroutine check_cot2d

   subroutine check_cot3d()
      real(rk) :: x(3, 4), pc(3), pf(3, 4), avec(3, 6), wv(6)
      real(rk) :: jm(3, 3), ji(3, 3), g(3, 4), e(3), vtet, r
      real(rk) :: got, want, p1, err, errp1, errc, errp, scale
      real(rk) :: gv(6), wt(6), pv(6), lam(4)
      integer :: n, ie, ia, ib, ic, id, k
      logical :: ok, okp, done
      err = 0.0_rk
      errp1 = 0.0_rk
      errc = 0.0_rk
      errp = 0.0_rk
      scale = 0.0_rk
      done = .false.
      do n = 1, ncells
         call rnd_tet(x)
         call dual_points_tet(x, 1.0_rk, spread(1.0_rk, 1, 4), pc, &
                              pf, okp)
         call kernel_tet(x, pc, pf, avec, wv, ok)
         r = norm2(pc - x(:, 1))
         do k = 2, 4
            errc = max(errc, abs(norm2(pc - x(:, k)) - r)/r)
         end do
         jm(:, 1) = x(:, 2) - x(:, 1)
         jm(:, 2) = x(:, 3) - x(:, 1)
         jm(:, 3) = x(:, 4) - x(:, 1)
         ji = inv3(jm)
         ! grad phi_k for k = 2..4 are the rows of inv(J)
         g(:, 2) = ji(1, :)
         g(:, 3) = ji(2, :)
         g(:, 4) = ji(3, :)
         g(:, 1) = -(g(:, 2) + g(:, 3) + g(:, 4))
         vtet = abs(tet_det(x))/6.0_rk
         do ie = 1, 6
            ia = iedge_tet(1, ie)
            ib = iedge_tet(2, ie)
            ic = 0
            id = 0
            do k = 1, 4
               if (k /= ia .and. k /= ib) then
                  if (ic == 0) then
                     ic = k
                  else
                     id = k
                  end if
               end if
            end do
            e = x(:, ib) - x(:, ia)
            got = dot_product(avec(:, ie), e)/dot_product(e, e)
            want = 0.25_rk*(cotv(x, ia, ib, ic)*hdist(x, pc, ia, ib, &
                   ic, id) + cotv(x, ia, ib, id)*hdist(x, pc, ia, ib, &
                   id, ic))
            p1 = -vtet*dot_product(g(:, ia), g(:, ib))
            gv(ie) = got
            wt(ie) = want
            pv(ie) = p1
            err = max(err, abs(got - want))
            errp1 = max(errp1, abs(got - p1))
            errp = max(errp, norm2(cross(avec(:, ie), e))/norm2(e))
            scale = max(scale, abs(want))
         end do
         ! the first tet with its circumcenter well inside (every
         ! barycentric coordinate > 0.1), for the picture
         lam(2:4) = matmul(ji, pc - x(:, 1))
         lam(1) = 1.0_rk - sum(lam(2:4))
         if (.not. done .and. minval(lam) > 0.1_rk) then
            write (lus, '(a,45(1x,es23.15))') 'tet', x, pc, pf, gv, &
               wt, pv
            done = .true.
         end if
      end do
      call judge('circ3d_max_rel_diff', errc, tol_id)
      call judge('dual3d_max_rel_diff', err/scale, tol_id)
      call judge('perp3d_max_rel', errp/scale, tol_id)
      write (*, '(a,1x,1pe12.4,1x,a)') 'p1_3d_max_rel_diff', &
         errp1/scale, 'MEASURED'
   end subroutine check_cot3d

   ! cot of the angle at vertex k of triangle (a, b, k)
   real(rk) function cotv(x, ia, ib, k)
      real(rk), intent(in) :: x(3, 4)
      integer, intent(in) :: ia, ib, k
      real(rk) :: u(3), v(3)
      u = x(:, ia) - x(:, k)
      v = x(:, ib) - x(:, k)
      cotv = dot_product(u, v)/norm2(cross(u, v))
   end function cotv

   ! signed distance of point c from the plane of face (a, b, k),
   ! positive on the side of vertex l (inside the tet)
   real(rk) function hdist(x, c, ia, ib, k, l)
      real(rk), intent(in) :: x(3, 4), c(3)
      integer, intent(in) :: ia, ib, k, l
      real(rk) :: nn(3)
      nn = cross(x(:, ib) - x(:, ia), x(:, k) - x(:, ia))
      nn = nn/norm2(nn)
      if (dot_product(nn, x(:, l) - x(:, ia)) < 0.0_rk) nn = -nn
      hdist = dot_product(nn, c - x(:, ia))
   end function hdist

   subroutine check_part2d()
      real(rk) :: x(3, 3), pc(3), avec(3, 3), wv(3), w(3), s, err
      integer :: n
      logical :: ok
      err = 0.0_rk
      do n = 1, ncells
         call rnd_tri(x)
         ! any point in the triangle's plane, even outside it
         do
            call random_number(w)
            w = 2.0_rk*w - 0.5_rk
            if (sum(w) > 0.5_rk) exit
         end do
         w = w/sum(w)
         pc = matmul(x, w)
         call kernel_tri(x, pc, avec, wv, ok)
         ! each edge share goes to both of its vertices
         s = 2.0_rk*sum(wv)
         err = max(err, abs(s - tri_area(x))/tri_area(x))
      end do
      call judge('part2d_max_rel_diff', err, tol_id)
   end subroutine check_part2d

   subroutine check_part3d()
      real(rk) :: x(3, 4), pc(3), pf(3, 4), avec(3, 6), wv(6), w(4)
      real(rk) :: s, err, v
      integer :: n, k
      logical :: ok
      err = 0.0_rk
      do n = 1, ncells
         call rnd_tet(x)
         call random_number(w)
         w = w/sum(w)
         pc = matmul(x, w)
         do k = 1, 4
            ! a random point in the plane of the face opposite k
            do
               call random_number(w)
               w = 2.0_rk*w - 0.5_rk
               w(k) = 0.0_rk
               if (sum(w) > 0.5_rk) exit
            end do
            w = w/sum(w)
            pf(:, k) = matmul(x, w)
         end do
         call kernel_tet(x, pc, pf, avec, wv, ok)
         s = 2.0_rk*sum(wv)
         v = abs(tet_det(x))/6.0_rk
         err = max(err, abs(s - v)/v)
      end do
      call judge('part3d_max_rel_diff', err, tol_id)
   end subroutine check_part3d

   subroutine check_shift()
      real(rk), parameter :: off(3) = [7.0e5_rk, 3.2e6_rk, 1.5e3_rk]
      real(rk) :: x(3, 4), y(3, 4), c0(3), c1(3)
      real(rk) :: a0(6), a1(6), a2(6), err, err_old, scale
      integer :: n, k
      logical :: ok
      err = 0.0_rk
      err_old = 0.0_rk
      scale = 0.0_rk
      do n = 1, ncells
         call rnd_tet(x)
         call coefs(x, [0.0_rk, 0.0_rk, 0.0_rk], .false., a0)
         call coefs(x, off, .false., a1)
         call coefs(x, off, .true., a2)
         err = max(err, maxval(abs(a1 - a0)))
         err_old = max(err_old, maxval(abs(a2 - a0)))
         scale = max(scale, maxval(abs(a0)))
      end do
      call judge('shift_max_rel_diff', err/scale, tol_shift)
      write (*, '(a,1x,1pe12.4,1x,a)') &
         'shift_voronoi_det4x4_rel_diff', err_old/scale, 'MEASURED'
      ! the circumcenter itself, for the record
      call rnd_tet(x)
      do k = 1, 4
         y(:, k) = x(:, k) + off
      end do
      call circumcenter_tet(x(:, 1), x(:, 2), x(:, 3), x(:, 4), c0, ok)
      c1 = det_circumcenter(y) - off
      write (*, '(a,1x,1pe12.4,1x,a)') 'shift_det4x4_center_error', &
         norm2(c1 - c0), 'MEASURED'
   end subroutine check_shift

   ! A/d for the 6 edges of tet x moved by off. The kernel always
   ! works on coordinates relative to vertex 1, as assemble does. With
   ! old = .true. the cell circumcenter is lanl/voronoi's, from the
   ! absolute coordinates.
   subroutine coefs(x, off, old, c)
      real(rk), intent(in) :: x(3, 4), off(3)
      logical, intent(in) :: old
      real(rk), intent(out) :: c(6)
      real(rk) :: y(3, 4), z(3, 4), pc(3), pf(3, 4), avec(3, 6)
      real(rk) :: wv(6), e(3)
      integer :: k
      logical :: ok
      do k = 1, 4
         y(:, k) = x(:, k) + off
      end do
      do k = 1, 4
         z(:, k) = y(:, k) - y(:, 1)
      end do
      call dual_points_tet(z, 1.0_rk, spread(1.0_rk, 1, 4), pc, pf, &
                           ok)
      if (old) pc = det_circumcenter(y) - y(:, 1)
      call kernel_tet(z, pc, pf, avec, wv, ok)
      do k = 1, 6
         e = z(:, iedge_tet(2, k)) - z(:, iedge_tet(1, k))
         c(k) = dot_product(avec(:, k), e)/dot_product(e, e)
      end do
   end subroutine coefs

   ! lanl/voronoi TetrahedronCentroid: 4x4 determinants of absolute
   ! coordinates and their squared norms (MathWorld "Circumsphere").
   function det_circumcenter(v) result(c)
      real(rk), intent(in) :: v(3, 4)
      real(rk) :: c(3), ma(4, 4), mx(4, 4), my(4, 4), mz(4, 4), vs(4)
      real(rk) :: a
      integer :: k
      do k = 1, 4
         ma(1:3, k) = v(:, k)
         vs(k) = sum(v(:, k)**2)
      end do
      ma(4, :) = 1.0_rk
      mx(1, :) = vs
      mx(2, :) = ma(2, :)
      mx(3, :) = ma(3, :)
      mx(4, :) = 1.0_rk
      my(1, :) = vs
      my(2, :) = ma(1, :)
      my(3, :) = ma(3, :)
      my(4, :) = 1.0_rk
      mz(1, :) = vs
      mz(2, :) = ma(1, :)
      mz(3, :) = ma(2, :)
      mz(4, :) = 1.0_rk
      a = det4(ma)
      c(1) = det4(mx)/(2*a)
      c(2) = -det4(my)/(2*a)
      c(3) = det4(mz)/(2*a)
   end function det_circumcenter

   ! voronoi Det4x4 (cofactor expansion along the first column).
   real(rk) function det4(m)
      real(rk), intent(in) :: m(4, 4)
      det4 = m(1, 1)*det3(m(2:4, 2), m(2:4, 3), m(2:4, 4)) &
             - m(2, 1)*det3(m([1, 3, 4], 2), m([1, 3, 4], 3), &
                            m([1, 3, 4], 4)) &
             + m(3, 1)*det3(m([1, 2, 4], 2), m([1, 2, 4], 3), &
                            m([1, 2, 4], 4)) &
             - m(4, 1)*det3(m(1:3, 2), m(1:3, 3), m(1:3, 4))
   end function det4

   real(rk) function det3(a, b, c)
      real(rk), intent(in) :: a(3), b(3), c(3)
      det3 = dot_product(a, cross(b, c))
   end function det3

   function inv3(a) result(b)
      real(rk), intent(in) :: a(3, 3)
      real(rk) :: b(3, 3), d
      b(1, :) = cross(a(:, 2), a(:, 3))
      b(2, :) = cross(a(:, 3), a(:, 1))
      b(3, :) = cross(a(:, 1), a(:, 2))
      d = dot_product(a(:, 1), b(1, :))
      b = b/d
   end function inv3

   subroutine check_closure(fname)
      character(len=*), intent(in) :: fname
      type(mesh_t) :: m
      type(assembly_stats_t) :: st
      integer(ik), allocatable :: iedge_off(:), jedge(:)
      real(rk), allocatable :: area(:), vol(:), av(:, :), sv(:, :)
      real(rk) :: lo(3), hi(3), p(3), amax, err, tolb
      integer(ik) :: nedges, i, k, j, ninner
      integer :: ierr
      character(len=80) :: key

      call read_avs(fname, m, ierr)
      if (ierr /= 0) then
         write (*, '(a)') trim(errmsg)
         nfail = nfail + 1
         return
      end if
      call build_edge_csr(m%nnodes, m%nelements, m%nen, m%itetoff, &
                          m%itet, iedge_off, jedge, nedges, ierr)
      allocate (area(nedges), vol(m%nnodes), av(3, nedges))
      allocate (sv(3, m%nnodes))
      call assemble(m%nnodes, m%nelements, m%nen, &
                    m%xic, m%yic, m%zic, m%itetoff, m%itet, &
                    iedge_off, jedge, area, vol, st, ierr, av)
      ! sum of outward vector areas around each vertex
      sv = 0.0_rk
      do i = 1, m%nnodes
         do k = iedge_off(i), iedge_off(i + 1) - 1
            j = jedge(k)
            sv(:, i) = sv(:, i) + av(:, k)
            sv(:, j) = sv(:, j) - av(:, k)
         end do
      end do
      lo = [minval(m%xic), minval(m%yic), minval(m%zic)]
      hi = [maxval(m%xic), maxval(m%yic), maxval(m%zic)]
      tolb = 1.0e-9_rk*maxval(hi - lo)
      amax = maxval(norm2(av, dim=1))
      err = 0.0_rk
      ninner = 0
      do i = 1, m%nnodes
         p = [m%xic(i), m%yic(i), m%zic(i)]
         ! skip vertices on the bounding box; in 2D, the flat
         ! direction does not count
         if (m%nen == 4) then
            if (any(abs(p - lo) <= tolb) .or. &
                any(abs(p - hi) <= tolb)) cycle
         else
            if (any(abs(p - lo) <= tolb .and. hi - lo > tolb) .or. &
                any(abs(p - hi) <= tolb .and. hi - lo > tolb)) cycle
         end if
         ninner = ninner + 1
         err = max(err, norm2(sv(:, i)))
         write (lus, '(a,1x,a,1x,i0,4(1x,es23.15))') 'closure', &
            trim(basename(fname)), i, p, norm2(sv(:, i))/amax
      end do
      key = 'closure_'//trim(basename(fname))
      write (*, '(a,1x,i0)') trim(key)//'_inner_vertices', ninner
      call judge(trim(key)//'_max_rel', err/amax, tol_closure)
   end subroutine check_closure

   function basename(f) result(b)
      character(len=*), intent(in) :: f
      character(len=len(f)) :: b
      integer :: i
      i = index(f, '/', back=.true.)
      b = f(i + 1:)
   end function basename

end program del_vor_selftest
