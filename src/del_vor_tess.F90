!-----------------------------------------------------------------------
! del_vor_tess: the control volumes as cells for ParaView (design
! section 11), built from exactly the dual points the coefficients
! came from (del_vor_policy: Voronoi, median, blend or hybrid).
!
! Points: the mesh vertices, the edge midpoints, the cell points P(T)
! and (tets) the face points F(f), each once. A face point belongs to
! the face; its slot is that of the side with the smaller cell number.
!
! 3D, the control volume of vertex i (a VTK polyhedron):
!   - for each tet T at i and each edge ij of T, the kernel's two
!     triangles (m, F(ijk), P(T)) and (m, P(T), F(ijl)), wound with
!     the normal from i toward j (the orientation of the coefficient
!     A(ij)). With cv voronoi the triangles of one edge are joined into
!     one polygon (the Voronoi face, planar); otherwise each triangle
!     is a face (the dual faces of moved points are not planar);
!   - for each exterior face of T at i, the part of it nearer i:
!     (x(i), m(i, next), F, m(i, prev)), wound outward.
!   Every face has its normal outward from i (VTK requires it).
! 2D and fracture networks, one polygon per vertex per fracture: the
!   segments m(i, next) -> P(T) -> m(i, prev) of the triangles at i,
!   walked from triangle to neighbor through jtet (so a fracture's
!   polygon stays in its fracture), closed by x(i) at a boundary.
!   The polygon's area is computed from its points (data "volume").
!
! A selection sel(i) says which vertices get a cell.
!-----------------------------------------------------------------------
module del_vor_tess
   use, intrinsic :: iso_fortran_env, only: int8
   use del_vor_kinds, only: ik, rk
   use del_vor_mesh, only: mesh_t
   use del_vor_assemble, only: cell_policy_t
   use del_vor_policy, only: dual_points_tri, dual_points_tet, &
                             face_alphas
   use del_vor_geometry, only: tet_det, cross, iedge_tet, icd_tet
   use del_vor_jtet, only: iface_tet
   use del_vor_topology, only: edge_index, build_incidence
   use del_vor_vtu, only: vtu_grid_t, add_cdata, add_pdata
   implicit none
   private
   public :: build_cells, build_mesh_grid, build_negative_grid, &
             boundary_vertices, interface_edges

   integer, parameter :: maxf = 4096   ! faces of one polyhedron
   integer, parameter :: maxp = 64     ! points of one face

contains

   ! bv(i): vertex i is on an exterior facet.
   subroutine boundary_vertices(m, bv)
      type(mesh_t), intent(in) :: m
      logical, intent(out) :: bv(:)
      integer(ik) :: it
      integer :: k, q
      bv = .false.
      do it = 1, m%nelements
         do k = 1, int(m%nen)
            if (m%jtet(m%jtetoff(it) + k) /= m%mbndry) cycle
            do q = 1, int(m%nen)
               if (q /= k) bv(m%itet(m%itetoff(it) + q)) = .true.
            end do
         end do
      end do
   end subroutine boundary_vertices

   ! ie(k): edge k lies on a facet between two materials.
   subroutine interface_edges(m, iedge_off, jedge, ie)
      type(mesh_t), intent(in) :: m
      integer(ik), intent(in) :: iedge_off(:), jedge(:)
      logical, intent(out) :: ie(:)
      integer(ik) :: it, k, v(4)
      integer :: f, a, b, n
      ie = .false.
      n = int(m%nen)
      do it = 1, m%nelements
         do f = 1, n
            if (m%jtet(m%jtetoff(it) + f) <= m%mbndry) cycle
            v(1:n) = m%itet(m%itetoff(it) + 1:m%itetoff(it) + n)
            do a = 1, n - 1
               do b = a + 1, n
                  if (a == f .or. b == f) cycle
                  k = edge_index(v(a), v(b), m%nnodes, iedge_off, jedge)
                  if (k > 0) ie(k) = .true.
               end do
            end do
         end do
      end do
   end subroutine interface_edges

   ! The slot of tet it's face k (the side with the smaller cell).
   pure integer(ik) function face_key(m, it, k) result(key)
      type(mesh_t), intent(in) :: m
      integer(ik), intent(in) :: it
      integer, intent(in) :: k
      integer(ik) :: code, jt, jf
      key = 4*(it - 1) + k
      code = m%jtet(m%jtetoff(it) + k)
      if (code == m%mbndry) return
      if (code > m%mbndry) code = code - m%mbndry
      jt = (code - 1)/4 + 1
      jf = code - 4*(jt - 1)
      if (jt < it) key = 4*(jt - 1) + jf
   end function face_key

   ! The dual points of cell it, absolute coordinates.
   subroutine cell_points(m, pol, it, pc, pf)
      type(mesh_t), intent(in) :: m
      type(cell_policy_t), intent(in) :: pol
      integer(ik), intent(in) :: it
      real(rk), intent(out) :: pc(3), pf(3, 4)
      real(rk) :: x(3, 4), x0(3), a, af(4)
      integer(ik) :: iv
      integer :: j
      logical :: ok
      do j = 1, int(m%nen)
         iv = m%itet(m%itetoff(it) + j)
         x(:, j) = [m%xic(iv), m%yic(iv), m%zic(iv)]
      end do
      x0 = x(:, 1)
      do j = 1, int(m%nen)
         x(:, j) = x(:, j) - x0
      end do
      a = 1.0_rk
      if (allocated(pol%alpha)) a = pol%alpha(it)
      pf = 0.0_rk
      if (m%nen == 3) then
         call dual_points_tri(x(:, 1:3), a, pc, ok)
      else
         af = 1.0_rk
         if (allocated(pol%alpha)) call face_alphas(it, x, pol%alpha, &
            m%jtetoff, m%jtet, m%mbndry, pol%clamp, pol%amin, af)
         call dual_points_tet(x, a, af, pc, pf, ok)
         do j = 1, 4
            pf(:, j) = pf(:, j) + x0
         end do
      end if
      pc = pc + x0
   end subroutine cell_points

   ! The control volumes of the selected vertices, into g.
   ! voronoi: join each edge's triangles into one polygon (3D).
   subroutine build_cells(m, pol, voronoi, iedge_off, jedge, area, &
                          vol, zero, sel, g, faces)
      type(mesh_t), intent(in) :: m
      type(cell_policy_t), intent(in) :: pol
      logical, intent(in) :: voronoi
      integer(ik), intent(in) :: iedge_off(:), jedge(:)
      real(rk), intent(in) :: area(:), vol(:)
      logical, intent(in) :: zero(:), sel(:)
      type(vtu_grid_t), intent(out) :: g
      type(vtu_grid_t), intent(out), optional :: faces

      integer(ik), allocatable :: inc_off(:), inc(:), ren(:)
      integer(ik), allocatable :: cid(:), cface(:), cbnd(:), cmat(:)
      real(rk), allocatable :: cvol(:), cmin(:), minc(:)
      logical, allocatable :: touch(:), bv(:)
      integer(ik) :: nedges, nslot, s_e, s_c, s_f, i, it, k, j, nn, ip
      integer(ik) :: ncell, nconn, nfc, nfconn, np
      real(rk) :: pc(3), pf(3, 4), d
      integer :: q
      ! one cell's faces, as point slots: fp(:, f) its points, fn(f)
      ! how many, fe(f) its edge (0: a boundary piece), fj(f) the
      ! edge's other end
      integer(ik), allocatable :: fp(:, :), fn(:), fe(:), fj(:), ubuf(:)
      ! the triangles at a vertex (3D): points, edge, other end
      integer(ik), allocatable :: tri(:, :), tre(:), trj(:)
      ! the boundary pieces at a vertex (3D): the next and previous
      ! vertex, the face key, the outward normal; flat ends of edges
      integer(ik), allocatable :: bnx(:), bpv(:), bky(:), bflat(:)
      real(rk), allocatable :: bnr(:, :)
      integer :: nflat
      ! unique_points: the last call that took each slot
      integer, allocatable :: stamp(:)
      integer :: ncur
      integer :: nf
      ! faces.vtu
      integer(ik) :: nface_out, nfconn_out
      integer(ik), allocatable :: fo_i(:), fo_j(:)
      real(rk), allocatable :: fo_a(:), fo_c(:), fo_ang(:)

      nn = m%nnodes
      nedges = iedge_off(nn + 1) - 1
      s_e = nn
      s_c = nn + nedges
      s_f = s_c + m%nelements
      nslot = s_f
      if (m%nen == 4) nslot = s_f + 4*m%nelements
      call build_incidence(nn, m%nelements, m%nen, m%itetoff, m%itet, &
                           inc_off, inc)
      allocate (bv(nn))
      call boundary_vertices(m, bv)
      allocate (fp(maxp, maxf), fn(maxf), fe(maxf), fj(maxf), &
                ubuf(maxp*maxf), tri(3, maxf), tre(maxf), trj(maxf))
      allocate (bnx(maxf), bpv(maxf), bky(maxf), bflat(maxf), &
                bnr(3, maxf))

      ! the smallest coefficient of each vertex's edges
      allocate (minc(nn))
      minc = huge(1.0_rk)
      do i = 1, nn
         do k = iedge_off(i), iedge_off(i + 1) - 1
            if (zero(k)) cycle
            j = jedge(k)
            d = norm2([m%xic(j) - m%xic(i), m%yic(j) - m%yic(i), &
                       m%zic(j) - m%zic(i)])
            minc(i) = min(minc(i), area(k)/d)
            minc(j) = min(minc(j), area(k)/d)
         end do
      end do
      where (minc == huge(1.0_rk)) minc = 0.0_rk

      ! pass 1: the cells and faces, as slots; count
      allocate (ren(nslot), touch(m%nelements), stamp(nslot))
      stamp = 0
      ncur = 0
      ren = 0
      touch = .false.
      ncell = 0
      nconn = 0
      nfc = 0
      nfconn = 0
      nface_out = 0
      nfconn_out = 0
      do i = 1, nn
         if (.not. sel(i)) cycle
         do ip = inc_off(i), inc_off(i + 1) - 1
            touch(inc(ip)) = .true.
         end do
         if (m%nen == 4) then
            call faces3d(i, nf)
         else
            call faces2d(i, nf)
         end if
         call count_cell(nf)
      end do

      ! the points: coordinates of every slot used
      np = 0
      do k = 1, nslot
         if (ren(k) /= 0) then
            np = np + 1
            ren(k) = np
         end if
      end do
      g%np = np
      allocate (g%xyz(3, np))
      do i = 1, nn
         if (ren(i) > 0) g%xyz(:, ren(i)) = [m%xic(i), m%yic(i), &
                                              m%zic(i)]
      end do
      do i = 1, nn
         do k = iedge_off(i), iedge_off(i + 1) - 1
            if (ren(s_e + k) == 0) cycle
            j = jedge(k)
            g%xyz(:, ren(s_e + k)) = 0.5_rk*[m%xic(i) + m%xic(j), &
               m%yic(i) + m%yic(j), m%zic(i) + m%zic(j)]
         end do
      end do
      do it = 1, m%nelements
         if (.not. touch(it)) cycle
         call cell_points(m, pol, it, pc, pf)
         if (ren(s_c + it) > 0) g%xyz(:, ren(s_c + it)) = pc
         if (m%nen == 4) then
            do q = 1, 4
               k = s_f + face_key(m, it, q)
               if (ren(k) > 0) g%xyz(:, ren(k)) = pf(:, q)
            end do
         end if
      end do

      ! pass 2: the cells
      g%nc = ncell
      allocate (g%conn(nconn), g%off(ncell), g%types(ncell))
      allocate (cid(ncell), cface(ncell), cbnd(ncell), cmat(ncell), &
                cvol(ncell), cmin(ncell))
      if (m%nen == 4) then
         g%nf = nfc
         allocate (g%fconn(nfconn), g%foff(nfc), g%pfaces(nfc), &
                   g%poff(ncell))
      end if
      if (present(faces)) then
         faces%nc = nface_out
         allocate (faces%conn(nfconn_out), faces%off(nface_out), &
                   faces%types(nface_out))
         allocate (fo_i(nface_out), fo_j(nface_out), fo_a(nface_out), &
                   fo_c(nface_out), fo_ang(nface_out))
      end if
      ncell = 0
      nconn = 0
      nfc = 0
      nfconn = 0
      nface_out = 0
      nfconn_out = 0
      do i = 1, nn
         if (.not. sel(i)) cycle
         if (m%nen == 4) then
            call faces3d(i, nf)
            call put_polyhedron(i, nf)
         else
            call faces2d_put(i)
         end if
      end do

      call add_cdata(g, 'vertex', i=cid)
      call add_cdata(g, 'volume', r=cvol)
      call add_cdata(g, 'material', i=cmat)
      call add_cdata(g, 'boundary', i=cbnd)
      call add_cdata(g, 'nfaces', i=cface)
      call add_cdata(g, 'min_coefficient', r=cmin)
      if (present(faces)) then
         faces%np = g%np
         faces%xyz = g%xyz
         call add_cdata(faces, 'vertex_i', i=fo_i)
         call add_cdata(faces, 'vertex_j', i=fo_j)
         call add_cdata(faces, 'area', r=fo_a)
         call add_cdata(faces, 'coefficient', r=fo_c)
         call add_cdata(faces, 'angle', r=fo_ang)
      end if

   contains

      subroutine count_cell(nf)
         integer, intent(in) :: nf
         integer :: f
         integer(ik) :: nu
         if (m%nen == 4) then
            ncell = ncell + 1
            call unique_points(nf, ubuf, nu)
            nconn = nconn + nu
            nfc = nfc + nf
            do f = 1, nf
               nfconn = nfconn + fn(f)
               ren(fp(1:fn(f), f)) = 1
               if (fe(f) > 0) then
                  if (.not. sel(fj(f)) .or. fj(f) > i) &
                     call count_face(f)
               end if
            end do
         else
            ! 2D: faces2d gave the polygons in fp(:, f), one per cell
            do f = 1, nf
               ncell = ncell + 1
               nconn = nconn + fn(f)
               ren(fp(1:fn(f), f)) = 1
            end do
         end if
      end subroutine count_cell

      subroutine count_face(f)
         integer, intent(in) :: f
         nface_out = nface_out + 1
         nfconn_out = nfconn_out + fn(f)
      end subroutine count_face

      ! The distinct points of the faces fp(:, 1:nf), in the order
      ! met: stamp(slot) remembers the last cell that took a slot.
      subroutine unique_points(nf, u, nu)
         integer, intent(in) :: nf
         integer(ik), intent(out) :: u(:), nu
         integer :: f, a
         integer(ik) :: q
         ncur = ncur + 1
         nu = 0
         do f = 1, nf
            do a = 1, int(fn(f))
               q = fp(a, f)
               if (stamp(q) /= ncur) then
                  stamp(q) = ncur
                  nu = nu + 1
                  u(nu) = q
               end if
            end do
         end do
      end subroutine unique_points

      ! 3D: the faces of vertex i's control volume, as slots.
      subroutine faces3d(i, nf)
         integer(ik), intent(in) :: i
         integer, intent(out) :: nf
         integer(ik) :: v(4), kk, oj
         integer :: nt, e, li, a, b, c, dd, kf, pz, nx, pv, t, w, nb
         real(rk) :: x(3, 4)
         logical :: flip
         nf = 0
         nt = 0
         nb = 0
         do ip = inc_off(i), inc_off(i + 1) - 1
            it = inc(ip)
            v = m%itet(m%itetoff(it) + 1:m%itetoff(it) + 4)
            do w = 1, 4
               x(:, w) = [m%xic(v(w)), m%yic(v(w)), m%zic(v(w))] - &
                         [m%xic(v(1)), m%yic(v(1)), m%zic(v(1))]
            end do
            li = findloc(v, i, dim=1)
            do e = 1, 6
               a = iedge_tet(1, e)
               b = iedge_tet(2, e)
               if (a /= li .and. b /= li) cycle
               c = icd_tet(1, e)
               dd = icd_tet(2, e)
               kk = edge_index(v(a), v(b), nn, iedge_off, jedge)
               flip = (li == a) .neqv. (tet_det(x) > 0.0_rk)
               oj = v(b)
               if (li == b) oj = v(a)
               ! (m, F(abc), P) and (m, P, F(abd)), from a to b
               nt = nt + 1
               tri(:, nt) = [s_e + kk, s_f + face_key(m, it, dd), &
                             s_c + it]
               tre(nt) = kk
               trj(nt) = oj
               nt = nt + 1
               tri(:, nt) = [s_e + kk, s_c + it, &
                             s_f + face_key(m, it, c)]
               tre(nt) = kk
               trj(nt) = oj
               if (flip) then
                  tri(2:3, nt - 1) = tri([3, 2], nt - 1)
                  tri(2:3, nt) = tri([3, 2], nt)
               end if
            end do
            ! exterior faces at i: the part nearer i
            do kf = 1, 4
               if (kf == li) cycle
               if (m%jtet(m%jtetoff(it) + kf) /= m%mbndry) cycle
               ! iface_tet(:, kf) is outward for a positive tet
               pz = findloc(iface_tet(:, kf), li, dim=1)
               nx = iface_tet(mod(pz, 3) + 1, kf)
               pv = iface_tet(mod(pz + 1, 3) + 1, kf)
               if (tet_det(x) < 0.0_rk) then
                  t = nx
                  nx = pv
                  pv = t
               end if
               if (nb >= maxf) cycle
               nb = nb + 1
               bnx(nb) = v(nx)
               bpv(nb) = v(pv)
               bky(nb) = face_key(m, it, kf)
               bnr(:, nb) = cross(x(:, nx) - x(:, li), &
                                  x(:, pv) - x(:, li))
            end do
         end do
         call boundary3d(i, nb, nf)
         if (voronoi) then
            call join(tri, tre, trj, nt, nf)
         else
            do t = 1, nt
               nf = nf + 1
               fn(nf) = 3
               fe(nf) = tre(t)
               fj(nf) = trj(t)
               fp(1:3, nf) = tri(:, t)
            end do
         end if
      end subroutine faces3d

      ! Voronoi: the triangles (m, A, B) of each edge joined into one
      ! polygon: a ring A B ... (interior edge), or m A B ... (an
      ! open chain, at the boundary). The triangles are first grouped
      ! by edge (a vertex has few edges), then chained in each group.
      ! 3D boundary polygons of vertex i: the piece of each exterior
      ! face nearer i is (x_i, m(i,nx), F, m(i,pv)). With cv voronoi
      ! the pieces of coplanar faces next to each other are merged
      ! and the midpoint between them dropped (F, m and F' are on one
      ! line, m not always between), as in 2D; a vertex inside a flat
      ! patch is no corner either.
      subroutine boundary3d(i, nb, nf)
         integer(ik), intent(in) :: i
         integer, intent(in) :: nb
         integer, intent(inout) :: nf
         integer :: nxt(maxf), p, q, n, st
         logical :: hasp(maxf), done(maxf)
         nflat = 0
         if (.not. voronoi) then
            do p = 1, nb
               nf = nf + 1
               fn(nf) = 4
               fe(nf) = 0
               fj(nf) = 0
               fp(1:4, nf) = [i, mid(i, bnx(p)), s_f + bky(p), &
                              mid(i, bpv(p))]
            end do
            return
         end if
         nxt(1:nb) = 0
         hasp(1:nb) = .false.
         do p = 1, nb
            do q = 1, nb
               if (q == p .or. bnx(q) /= bpv(p)) cycle
               if (flat(bnr(:, p), bnr(:, q))) then
                  nxt(p) = q
                  hasp(q) = .true.
                  nflat = nflat + 1
                  bflat(nflat) = bpv(p)
               end if
               exit
            end do
         end do
         done(1:nb) = .false.
         ! open runs first, then rings
         do st = 1, 2
            do p = 1, nb
               if (done(p)) cycle
               if (st == 1 .and. hasp(p)) cycle
               nf = nf + 1
               fe(nf) = 0
               fj(nf) = 0
               n = 0
               if (st == 1) then
                  fp(1:2, nf) = [i, mid(i, bnx(p))]
                  n = 2
               end if
               q = p
               do
                  done(q) = .true.
                  if (n < maxp) n = n + 1
                  fp(n, nf) = s_f + bky(q)
                  if (nxt(q) == 0) exit
                  if (done(nxt(q))) exit
                  q = nxt(q)
               end do
               if (st == 1 .and. n < maxp) then
                  n = n + 1
                  fp(n, nf) = mid(i, bpv(q))
               end if
               fn(nf) = n
            end do
         end do
      end subroutine boundary3d

      integer(ik) function mid(i, j)
         integer(ik), intent(in) :: i, j
         mid = s_e + edge_index(i, j, nn, iedge_off, jedge)
      end function mid

      ! two outward normals of the same direction
      logical function flat(a, b)
         real(rk), intent(in) :: a(3), b(3)
         flat = dot_product(a, b) > 0.0_rk .and. &
                norm2(cross(a, b)) <= 1.0e-10_rk*norm2(a)*norm2(b)
      end function flat

      subroutine join(tri, tre, trj, nt, nf)
         integer(ik), intent(in) :: tri(:, :), tre(:), trj(:)
         integer, intent(in) :: nt
         integer, intent(inout) :: nf
         integer(ik) :: ek(maxf)
         integer :: grp(maxf), mem(maxf), gs(maxf + 1), ng, t, u, g0
         integer :: n, s, w, cnt
         logical :: used(maxf), hasb
         ! groups by edge
         ng = 0
         do t = 1, nt
            grp(t) = 0
            do u = 1, ng
               if (ek(u) == tre(t)) then
                  grp(t) = u
                  exit
               end if
            end do
            if (grp(t) == 0) then
               ng = ng + 1
               ek(ng) = tre(t)
               grp(t) = ng
            end if
         end do
         gs(1:ng + 1) = 0
         do t = 1, nt
            gs(grp(t) + 1) = gs(grp(t) + 1) + 1
         end do
         gs(1) = 1
         do u = 1, ng
            gs(u + 1) = gs(u + 1) + gs(u)
         end do
         cnt = 0
         do u = 1, ng
            do t = 1, nt
               if (grp(t) == u) then
                  cnt = cnt + 1
                  mem(cnt) = t
               end if
            end do
         end do
         used(1:nt) = .false.
         do g0 = 1, ng
            ! start at a triangle whose A is no triangle's B (open
            ! chain), else at the first (ring)
            s = mem(gs(g0))
            do w = gs(g0), gs(g0 + 1) - 1
               hasb = .false.
               do u = gs(g0), gs(g0 + 1) - 1
                  if (tri(3, mem(u)) == tri(2, mem(w))) hasb = .true.
               end do
               if (.not. hasb) then
                  s = mem(w)
                  exit
               end if
            end do
            hasb = .false.
            do u = gs(g0), gs(g0 + 1) - 1
               if (tri(3, mem(u)) == tri(2, s)) hasb = .true.
            end do
            nf = nf + 1
            fe(nf) = ek(g0)
            fj(nf) = trj(s)
            n = 0
            ! m starts an open chain, unless the edge's two boundary
            ! faces are coplanar: then m lies on the line through
            ! their circumcenters and is no corner
            if (.not. hasb) then
               if (all(bflat(1:nflat) /= trj(s))) then
                  n = 1
                  fp(1, nf) = tri(1, s)       ! m
               end if
            end if
            n = n + 1
            fp(n, nf) = tri(2, s)
            t = s
            do
               used(t) = .true.
               if (n >= maxp) exit
               n = n + 1
               fp(n, nf) = tri(3, t)
               s = 0
               do u = gs(g0), gs(g0 + 1) - 1
                  if (.not. used(mem(u)) .and. &
                      tri(2, mem(u)) == tri(3, t)) s = mem(u)
               end do
               if (s == 0) exit
               t = s
            end do
            ! a ring ends where it began: drop the repeated point
            if (fp(n, nf) == fp(1, nf)) n = n - 1
            ! a face point between two circumcenters lies on the line
            ! through them, but not always between them: not a
            ! corner, so it is left out (the boundary face points at
            ! the ends of an open chain stay)
            call drop_between(nf, n)
            fn(nf) = n
         end do
      end subroutine join

      subroutine drop_between(f, n)
         integer, intent(in) :: f
         integer, intent(inout) :: n
         integer :: a, k, prv, nxt
         logical :: keep(maxp)
         do a = 1, n
            prv = a - 1
            if (prv == 0) prv = n
            nxt = mod(a, n) + 1
            keep(a) = .not. (is_face(fp(a, f)) .and. &
                             is_cell(fp(prv, f)) .and. &
                             is_cell(fp(nxt, f)))
         end do
         k = 0
         do a = 1, n
            if (keep(a)) then
               k = k + 1
               fp(k, f) = fp(a, f)
            end if
         end do
         n = k
      end subroutine drop_between

      logical function is_face(q)
         integer(ik), intent(in) :: q
         is_face = q > s_f
      end function is_face

      logical function is_cell(q)
         integer(ik), intent(in) :: q
         is_cell = q > s_c .and. q <= s_f
      end function is_cell

      ! the cell's point is its circumcenter (alpha 1)
      logical function vor_cell(it)
         integer(ik), intent(in) :: it
         vor_cell = .true.
         if (allocated(pol%alpha)) vor_cell = pol%alpha(it) >= 1.0_rk
      end function vor_cell

      subroutine put_polyhedron(i, nf)
         integer(ik), intent(in) :: i
         integer, intent(in) :: nf
         integer(ik) :: nu
         integer :: f
         logical :: isb
         call unique_points(nf, ubuf, nu)
         ncell = ncell + 1
         g%conn(nconn + 1:nconn + nu) = ren(ubuf(1:nu))
         nconn = nconn + nu
         g%off(ncell) = nconn
         g%types(ncell) = 42_int8
         isb = .false.
         do f = 1, nf
            nfc = nfc + 1
            g%fconn(nfconn + 1:nfconn + fn(f)) = ren(fp(1:fn(f), f))
            nfconn = nfconn + fn(f)
            g%foff(nfc) = nfconn
            g%pfaces(nfc) = nfc
            if (fe(f) == 0) isb = .true.
            if (present(faces) .and. fe(f) > 0) then
               if (.not. sel(fj(f)) .or. fj(f) > i) call put_face(f, i)
            end if
         end do
         g%poff(ncell) = nfc
         cid(ncell) = i
         cvol(ncell) = vol(i)
         cmat(ncell) = 0
         if (allocated(m%imt1)) cmat(ncell) = m%imt1(i)
         cbnd(ncell) = merge(1_ik, 0_ik, bv(i))
         cface(ncell) = nf
         cmin(ncell) = minc(i)
      end subroutine put_polyhedron

      ! faces.vtu: one dual face (polygon) or piece (triangle)
      subroutine put_face(f, i)
         integer, intent(in) :: f
         integer(ik), intent(in) :: i
         integer(ik) :: jj, kk
         real(rk) :: av(3), e(3), p0(3)
         integer :: a
         jj = fj(f)
         kk = fe(f)
         nface_out = nface_out + 1
         faces%conn(nfconn_out + 1:nfconn_out + fn(f)) = &
            ren(fp(1:fn(f), f))
         nfconn_out = nfconn_out + fn(f)
         faces%off(nface_out) = nfconn_out
         faces%types(nface_out) = merge(7_int8, 5_int8, fn(f) > 3)
         fo_i(nface_out) = i
         fo_j(nface_out) = jj
         e = [m%xic(jj) - m%xic(i), m%yic(jj) - m%yic(i), &
              m%zic(jj) - m%zic(i)]
         ! the face's own vector area, about its first point
         av = 0.0_rk
         p0 = g%xyz(:, ren(fp(1, f)))
         do a = 2, int(fn(f)) - 1
            av = av + 0.5_rk*cross(g%xyz(:, ren(fp(a, f))) - p0, &
                                   g%xyz(:, ren(fp(a + 1, f))) - p0)
         end do
         fo_a(nface_out) = dot_product(av, e)/norm2(e)
         fo_c(nface_out) = area(kk)/norm2(e)
         fo_ang(nface_out) = 0.0_rk
         if (norm2(av) > 0.0_rk) fo_ang(nface_out) = &
            acos(max(-1.0_rk, min(1.0_rk, dot_product(av, e)/ &
            (norm2(av)*norm2(e)))))*180.0_rk/acos(-1.0_rk)
      end subroutine put_face

      ! 2D: the polygon(s) of vertex i, one per chain of triangles
      ! (one per fracture in a network), as slots in fp(:, f).
      subroutine faces2d(i, nf)
         integer(ik), intent(in) :: i
         integer, intent(out) :: nf
         integer(ik) :: tris(maxf), v(3), code, jt
         logical :: done(maxf), open
         integer :: nt, t, s, li, nx, pv, n, guard
         nf = 0
         nt = 0
         do ip = inc_off(i), inc_off(i + 1) - 1
            nt = nt + 1
            tris(nt) = inc(ip)
         end do
         done = .false.
         do guard = 1, 2
            ! first the open chains (start: edge (i,next) on a
            ! boundary), then the rings
            do t = 1, nt
               if (done(t)) cycle
               it = tris(t)
               v = m%itet(m%itetoff(it) + 1:m%itetoff(it) + 3)
               li = findloc(v, i, dim=1)
               pv = mod(li + 1, 3) + 1
               open = m%jtet(m%jtetoff(it) + pv) == m%mbndry
               if (guard == 1 .and. .not. open) cycle
               nf = nf + 1
               fe(nf) = it
               n = 0
               if (open) then
                  n = 1
                  fp(1, nf) = i
               end if
               s = t
               do
                  done(s) = .true.
                  it = tris(s)
                  v = m%itet(m%itetoff(it) + 1:m%itetoff(it) + 3)
                  li = findloc(v, i, dim=1)
                  nx = mod(li, 3) + 1
                  pv = mod(li + 1, 3) + 1
                  if (n + 3 > maxp) exit
                  if (n == 0 .or. open .and. n == 1) then
                     n = n + 1
                     fp(n, nf) = s_e + edge_index(i, v(nx), nn, &
                                                  iedge_off, jedge)
                  end if
                  n = n + 1
                  fp(n, nf) = s_c + it
                  n = n + 1
                  fp(n, nf) = s_e + edge_index(i, v(pv), nn, &
                                               iedge_off, jedge)
                  ! the neighbor across edge (i, prev): face nx
                  code = m%jtet(m%jtetoff(it) + nx)
                  if (code == m%mbndry) exit
                  if (code > m%mbndry) code = code - m%mbndry
                  jt = (code - 1)/3 + 1
                  s = findloc(tris(1:nt), jt, dim=1)
                  if (s == 0) exit
                  ! between two circumcenters the midpoint is on the
                  ! line through them, but not always between them
                  ! (an obtuse angle puts a circumcenter past the
                  ! edge): not a corner, so it is left out
                  if (vor_cell(it) .and. vor_cell(jt)) n = n - 1
                  if (done(s)) exit
               end do
               if (.not. open .and. fp(n, nf) == fp(1, nf)) then
                  n = n - 1
               else if (.not. open) then
                  ! the ring closed on the start triangle after
                  ! dropping its midpoint: drop the first one too
                  if (vor_cell(fe(nf)) .and. vor_cell(it)) then
                     fp(1:n - 1, nf) = fp(2:n, nf)
                     n = n - 1
                  end if
               end if
               fn(nf) = n
            end do
         end do
      end subroutine faces2d

      subroutine faces2d_put(i)
         integer(ik), intent(in) :: i
         integer :: f, a
         real(rk) :: nrm(3), p0(3), sa
         integer(ik) :: it0, v(3)
         call faces2d(i, nf)
         do f = 1, nf
            ncell = ncell + 1
            g%conn(nconn + 1:nconn + fn(f)) = ren(fp(1:fn(f), f))
            nconn = nconn + fn(f)
            g%off(ncell) = nconn
            g%types(ncell) = 7_int8
            it0 = fe(f)
            v = m%itet(m%itetoff(it0) + 1:m%itetoff(it0) + 3)
            nrm = cross([m%xic(v(2)) - m%xic(v(1)), &
                         m%yic(v(2)) - m%yic(v(1)), &
                         m%zic(v(2)) - m%zic(v(1))], &
                        [m%xic(v(3)) - m%xic(v(1)), &
                         m%yic(v(3)) - m%yic(v(1)), &
                         m%zic(v(3)) - m%zic(v(1))])
            nrm = nrm/norm2(nrm)
            p0 = [m%xic(i), m%yic(i), m%zic(i)]
            sa = 0.0_rk
            do a = 1, int(fn(f))
               sa = sa + 0.5_rk*dot_product(cross( &
                  g%xyz(:, ren(fp(a, f))) - p0, &
                  g%xyz(:, ren(fp(mod(a, int(fn(f))) + 1, f))) - p0), &
                  nrm)
            end do
            cid(ncell) = i
            cvol(ncell) = sa
            cmat(ncell) = m%itetclr(it0)
            cbnd(ncell) = merge(1_ik, 0_ik, fp(1, f) == i)
            cface(ncell) = fn(f)
            cmin(ncell) = minc(i)
         end do
      end subroutine faces2d_put

   end subroutine build_cells

   ! The input cells that have a selected vertex: itetclr, alpha,
   ! stage per cell; volume, imt1, boundary per vertex.
   subroutine build_mesh_grid(m, vol, sel, alpha, stage, g)
      type(mesh_t), intent(in) :: m
      real(rk), intent(in) :: vol(:)
      logical, intent(in) :: sel(:)
      real(rk), intent(in) :: alpha(:)
      integer, intent(in) :: stage(:)
      type(vtu_grid_t), intent(out) :: g
      integer(ik), allocatable :: ren(:), clr(:), st(:), pid(:), &
                                  pimt(:), pb(:)
      real(rk), allocatable :: al(:), pv(:)
      logical, allocatable :: keep(:), bv(:)
      integer(ik) :: it, i, n, nc
      integer :: q
      allocate (keep(m%nelements), ren(m%nnodes), bv(m%nnodes))
      call boundary_vertices(m, bv)
      ren = 0
      nc = 0
      do it = 1, m%nelements
         keep(it) = any(sel(m%itet(m%itetoff(it) + 1:m%itetoff(it) + &
                                   m%nen)))
         if (.not. keep(it)) cycle
         nc = nc + 1
         do q = 1, int(m%nen)
            ren(m%itet(m%itetoff(it) + q)) = 1
         end do
      end do
      n = 0
      do i = 1, m%nnodes
         if (ren(i) /= 0) then
            n = n + 1
            ren(i) = n
         end if
      end do
      g%np = n
      g%nc = nc
      allocate (g%xyz(3, n), pid(n), pimt(n), pb(n), pv(n))
      do i = 1, m%nnodes
         if (ren(i) == 0) cycle
         g%xyz(:, ren(i)) = [m%xic(i), m%yic(i), m%zic(i)]
         pid(ren(i)) = i
         pv(ren(i)) = vol(i)
         pimt(ren(i)) = 0
         if (allocated(m%imt1)) pimt(ren(i)) = m%imt1(i)
         pb(ren(i)) = merge(1_ik, 0_ik, bv(i))
      end do
      allocate (g%conn(nc*m%nen), g%off(nc), g%types(nc), clr(nc), &
                st(nc), al(nc))
      nc = 0
      do it = 1, m%nelements
         if (.not. keep(it)) cycle
         nc = nc + 1
         do q = 1, int(m%nen)
            g%conn((nc - 1)*m%nen + q) = ren(m%itet(m%itetoff(it) + q))
         end do
         g%off(nc) = nc*m%nen
         g%types(nc) = merge(10_int8, 5_int8, m%nen == 4)
         clr(nc) = m%itetclr(it)
         al(nc) = alpha(it)
         st(nc) = stage(it)
      end do
      call add_pdata(g, 'vertex', i=pid)
      call add_pdata(g, 'volume', r=pv)
      call add_pdata(g, 'imt1', i=pimt)
      call add_pdata(g, 'boundary', i=pb)
      call add_cdata(g, 'itetclr', i=clr)
      call add_cdata(g, 'alpha', r=al)
      call add_cdata(g, 'stage', i=st)
   end subroutine build_mesh_grid

   ! One segment per negative coefficient with a selected end:
   ! coefficient A/d, class 1 boundary, 2 interior, 3 interface.
   subroutine build_negative_grid(m, iedge_off, jedge, area, zero, &
                                  bedge, iedge, sel, g)
      type(mesh_t), intent(in) :: m
      integer(ik), intent(in) :: iedge_off(:), jedge(:)
      real(rk), intent(in) :: area(:)
      logical, intent(in) :: zero(:), bedge(:), iedge(:), sel(:)
      type(vtu_grid_t), intent(out) :: g
      integer(ik) :: i, j, k, n
      integer(ik), allocatable :: cls(:), vi(:), vj(:)
      real(rk), allocatable :: c(:)
      n = 0
      do i = 1, m%nnodes
         do k = iedge_off(i), iedge_off(i + 1) - 1
            if (neg(i, k)) n = n + 1
         end do
      end do
      g%np = 2*n
      g%nc = n
      allocate (g%xyz(3, 2*n), g%conn(2*n), g%off(n), g%types(n), &
                cls(n), c(n), vi(n), vj(n))
      n = 0
      do i = 1, m%nnodes
         do k = iedge_off(i), iedge_off(i + 1) - 1
            if (.not. neg(i, k)) cycle
            j = jedge(k)
            n = n + 1
            g%xyz(:, 2*n - 1) = [m%xic(i), m%yic(i), m%zic(i)]
            g%xyz(:, 2*n) = [m%xic(j), m%yic(j), m%zic(j)]
            g%conn(2*n - 1:2*n) = [2*n - 1, 2*n]
            g%off(n) = 2*n
            g%types(n) = 3_int8
            c(n) = area(k)/norm2([m%xic(j) - m%xic(i), &
                                  m%yic(j) - m%yic(i), &
                                  m%zic(j) - m%zic(i)])
            cls(n) = 2
            if (bedge(k)) cls(n) = 1
            if (iedge(k)) cls(n) = 3
            vi(n) = i
            vj(n) = j
         end do
      end do
      call add_cdata(g, 'coefficient', r=c)
      call add_cdata(g, 'class', i=cls)
      call add_cdata(g, 'vertex_i', i=vi)
      call add_cdata(g, 'vertex_j', i=vj)
   contains
      logical function neg(ii, kk)
         integer(ik), intent(in) :: ii, kk
         neg = area(kk) < 0.0_rk .and. .not. zero(kk) .and. &
               (sel(ii) .or. sel(jedge(kk)))
      end function neg
   end subroutine build_negative_grid

end module del_vor_tess
