!-----------------------------------------------------------------------
! del_vor_delaunay: the Delaunay check, facet by facet.
!
! Interior facet shared by cells T1 and T2 (one face of a tet, one
! edge of a triangle):
!
!   delta = (c2 - c1) . n,   n the unit normal of the facet from T1
!                            into T2 (in 2D, in the triangles' plane)
!
! c1, c2 the circumcenters, both computed relative to the same facet
! vertex. delta is the signed length of the Voronoi edge dual to the
! facet; it is >= 0 exactly when the facet is locally Delaunay. It is
! judged relative to L, the facet's mean edge length (2D: the edge):
!
!   |delta/L| <= tol             tie (co-circular, co-spherical):
!                                accepted
!   -fatal <= delta/L < -tol     slightly not Delaunay (nearly
!                                co-spherical, as LaGriT's connect
!                                leaves a few): a warning; the
!                                coefficients there are off by about
!                                |delta/L| of a typical coefficient
!   delta/L < -fatal             not Delaunay: fatal for Voronoi
!                                control volumes
! (tol = delaunay_tol, fatal = delaunay_fatal; both kinds are counted
! as violations and written to the violations file)
!
! Facets between two materials (jtet > mbndry) are tested the same
! way and counted as interface facets. In 2D only triangles of one
! itetclr are paired (del_vor_jtet), so a fracture network is tested
! fracture by fracture.
!
! Boundary facet (jtet = mbndry) of cell T: how far T's circumcenter
! lies beyond the facet, s = (c - x) . n_out / L. s > tol means the
! circumcenter is outside the domain there (2D: the angle opposite a
! boundary edge is obtuse), which makes a negative boundary
! coefficient likely. Reported, never fatal.
!
! Slivers: R / shortest edge, R the circumradius; cells above
! sliver_ratio are counted, and each recorded facet carries the
! larger ratio of its cells, so a large |delta| caused by round-off in
! a sliver can be told from a real violation.
!-----------------------------------------------------------------------
module del_vor_delaunay
   use del_vor_kinds, only: ik, rk
   use del_vor_geometry, only: cross, circumcenter_tri, &
                               circumcenter_tet
   use del_vor_jtet, only: iface_tri, iface_tet
   implicit none
   private
   public :: check_delaunay, delaunay_t, facet_rec_t, nhist, &
             hist_edges, sliver_ratio
   public :: class_interior, class_interface, class_boundary

   integer, parameter :: class_interior = 1   ! violation, same itetclr
   integer, parameter :: class_interface = 2  ! violation, 2 materials
   integer, parameter :: class_boundary = 3   ! circumcenter outside

   real(rk), parameter :: sliver_ratio = 5.0_rk

   ! histogram of delta/L: bins between these edges (and beyond)
   integer, parameter :: nhist = 8
   real(rk), parameter :: hist_edges(nhist - 1) = &
                          [-1.0e-2_rk, -1.0e-6_rk, -1.0e-10_rk, &
                           0.0_rk, 1.0e-10_rk, 1.0e-6_rk, 1.0e-2_rk]

   ! the facets written to violations.vtu
   type :: facet_rec_t
      integer :: n = 0
      integer(ik), allocatable :: vert(:, :)   ! (3, n); 0 if a segment
      integer(ik), allocatable :: cell(:, :)   ! (2, n); 0 if boundary
      integer(ik), allocatable :: clr(:, :)    ! itetclr of the cells
      real(rk), allocatable :: dl(:)           ! delta/L or s
      integer, allocatable :: kind(:)          ! class_*
      real(rk), allocatable :: ratio(:)        ! larger R/emin
   end type facet_rec_t

   type :: delaunay_t
      real(rk) :: tol = 0.0_rk
      real(rk) :: fatal_tol = 0.0_rk
      integer(ik) :: nchecked(2) = 0   ! interior, interface facets
      integer(ik) :: nviol(2) = 0      ! violations beyond tol
      integer(ik) :: nfatal(2) = 0     ! of them, beyond fatal_tol
      integer(ik) :: nties = 0         ! |delta/L| <= tol
      real(rk) :: worst = huge(1.0_rk) ! smallest delta/L
      integer(ik) :: worst_cell(2) = 0
      real(rk) :: worst_x(3) = 0.0_rk  ! facet center there
      integer(ik) :: hist(nhist) = 0
      integer(ik) :: nbound = 0        ! boundary facets
      integer(ik) :: nbound_out = 0    ! circumcenter beyond them
      real(rk) :: bound_worst = -huge(1.0_rk)
      integer(ik) :: bound_worst_cell = 0
      integer(ik) :: nsliver = 0       ! cells with R/emin > ratio
      real(rk) :: max_ratio = 0.0_rk
      integer(ik) :: ndegenerate = 0   ! no circumcenter
      type(facet_rec_t) :: rec
   end type delaunay_t

contains

   subroutine check_delaunay(nnodes, nelements, nen, xic, yic, zic, &
                             itetoff, itet, itetclr, jtetoff, jtet, &
                             mbndry, tol, fatal_tol, d)
      integer(ik), intent(in) :: nnodes, nelements, nen
      real(rk), intent(in) :: xic(nnodes), yic(nnodes), zic(nnodes)
      integer(ik), intent(in) :: itetoff(nelements), itet(*)
      integer(ik), intent(in) :: itetclr(nelements)
      integer(ik), intent(in) :: jtetoff(nelements), jtet(*), mbndry
      real(rk), intent(in) :: tol, fatal_tol
      type(delaunay_t), intent(out) :: d

      real(rk) :: ref(3), c1(3), c2(3), n(3), w(3), p(3, 3)
      real(rk) :: dl, el, r1, r2, s
      integer(ik) :: it, jt, code, fv(3), o1, o2, jf
      integer :: j, k, nf, cls
      logical :: ok1, ok2, iface

      d%tol = tol
      d%fatal_tol = max(fatal_tol, tol)
      nf = int(nen) - 1
      call rec_init(d%rec)

      ! slivers, over all cells
      do it = 1, nelements
         call cell_ratio(it, r1, ok1)
         if (.not. ok1) then
            d%ndegenerate = d%ndegenerate + 1
            cycle
         end if
         d%max_ratio = max(d%max_ratio, r1)
         if (r1 > sliver_ratio) d%nsliver = d%nsliver + 1
      end do

      do it = 1, nelements
         do j = 1, int(nen)
            code = jtet(jtetoff(it) + j)
            call facet(it, j, fv)
            ref = xyz(fv(1))
            do k = 1, nf
               p(:, k) = xyz(fv(k)) - ref
            end do
            o1 = itet(itetoff(it) + j)
            if (code == mbndry) then
               ! boundary facet: T's circumcenter beyond it?
               call circ(it, ref, c1, ok1)
               if (.not. ok1) cycle
               w = xyz(o1) - ref
               n = facet_normal(p, w)
               n = -n
               el = facet_length(p)
               s = dot_product(c1, n)/el
               d%nbound = d%nbound + 1
               if (s > d%bound_worst) then
                  d%bound_worst = s
                  d%bound_worst_cell = it
               end if
               if (s > tol) then
                  d%nbound_out = d%nbound_out + 1
                  call cell_ratio(it, r1, ok1)
                  call rec_add(d%rec, fv, [it, 0_ik], &
                               [itetclr(it), 0_ik], s, class_boundary, &
                               r1)
               end if
               cycle
            end if
            iface = code > mbndry
            if (iface) code = code - mbndry
            jt = (code - 1)/nen + 1
            jf = code - nen*(jt - 1)
            if (jt < it) cycle             ! each facet once
            o2 = itet(itetoff(jt) + jf)
            call circ(it, ref, c1, ok1)
            call circ(jt, ref, c2, ok2)
            if (.not. (ok1 .and. ok2)) cycle
            w = xyz(o2) - ref
            n = facet_normal(p, w)
            el = facet_length(p)
            dl = dot_product(c2 - c1, n)/el
            cls = class_interior
            if (iface) cls = class_interface
            d%nchecked(cls) = d%nchecked(cls) + 1
            call add_hist(dl)
            if (abs(dl) <= tol) d%nties = d%nties + 1
            if (dl < d%worst) then
               d%worst = dl
               d%worst_cell = [it, jt]
               d%worst_x = ref + sum(p(:, 1:nf), dim=2)/nf
            end if
            if (dl < -tol) then
               d%nviol(cls) = d%nviol(cls) + 1
               if (dl < -d%fatal_tol) &
                  d%nfatal(cls) = d%nfatal(cls) + 1
               call cell_ratio(it, r1, ok1)
               call cell_ratio(jt, r2, ok2)
               call rec_add(d%rec, fv, [it, jt], &
                            [itetclr(it), itetclr(jt)], dl, cls, &
                            max(r1, r2))
            end if
         end do
      end do

   contains

      function xyz(iv) result(x)
         integer(ik), intent(in) :: iv
         real(rk) :: x(3)
         x = [xic(iv), yic(iv), zic(iv)]
      end function xyz

      subroutine facet(ic, jj, f)
         integer(ik), intent(in) :: ic
         integer, intent(in) :: jj
         integer(ik), intent(out) :: f(3)
         integer :: q
         f = 0
         if (nen == 3) then
            do q = 1, 2
               f(q) = itet(itetoff(ic) + iface_tri(q, jj))
            end do
         else
            do q = 1, 3
               f(q) = itet(itetoff(ic) + iface_tet(q, jj))
            end do
         end if
      end subroutine facet

      ! circumcenter of cell ic, relative to the point r0
      subroutine circ(ic, r0, c, ok)
         integer(ik), intent(in) :: ic
         real(rk), intent(in) :: r0(3)
         real(rk), intent(out) :: c(3)
         logical, intent(out) :: ok
         real(rk) :: q(3, 4)
         integer :: m
         do m = 1, int(nen)
            q(:, m) = xyz(itet(itetoff(ic) + m)) - r0
         end do
         if (nen == 3) then
            call circumcenter_tri(q(:, 1), q(:, 2), q(:, 3), c, ok)
         else
            call circumcenter_tet(q(:, 1), q(:, 2), q(:, 3), q(:, 4), &
                                  c, ok)
         end if
      end subroutine circ

      ! circumradius / shortest edge of cell ic
      subroutine cell_ratio(ic, r, ok)
         integer(ik), intent(in) :: ic
         real(rk), intent(out) :: r
         logical, intent(out) :: ok
         real(rk) :: c(3), r0(3), emin
         integer :: a, b
         r0 = xyz(itet(itetoff(ic) + 1))
         call circ(ic, r0, c, ok)
         r = 0.0_rk
         if (.not. ok) return
         emin = huge(1.0_rk)
         do a = 1, int(nen) - 1
            do b = a + 1, int(nen)
               emin = min(emin, norm2(xyz(itet(itetoff(ic) + a)) - &
                                      xyz(itet(itetoff(ic) + b))))
            end do
         end do
         r = norm2(c)/emin
      end subroutine cell_ratio

      ! unit normal of the facet (p relative to its first vertex)
      ! pointing to the side of the point w
      function facet_normal(pp, ww) result(nn)
         real(rk), intent(in) :: pp(3, 3), ww(3)
         real(rk) :: nn(3), ee(3)
         if (nen == 4) then
            nn = cross(pp(:, 2), pp(:, 3))
         else
            ee = pp(:, 2)/norm2(pp(:, 2))
            nn = ww - dot_product(ww, ee)*ee
         end if
         nn = nn/norm2(nn)
         if (dot_product(nn, ww) < 0.0_rk) nn = -nn
      end function facet_normal

      real(rk) function facet_length(pp)
         real(rk), intent(in) :: pp(3, 3)
         if (nen == 4) then
            facet_length = (norm2(pp(:, 2)) + norm2(pp(:, 3)) + &
                            norm2(pp(:, 3) - pp(:, 2)))/3.0_rk
         else
            facet_length = norm2(pp(:, 2))
         end if
      end function facet_length

      subroutine add_hist(x)
         real(rk), intent(in) :: x
         integer :: b
         b = 1
         do while (b < nhist)
            if (x < hist_edges(b)) exit
            b = b + 1
         end do
         d%hist(b) = d%hist(b) + 1
      end subroutine add_hist

   end subroutine check_delaunay

   subroutine rec_init(r)
      type(facet_rec_t), intent(inout) :: r
      integer, parameter :: n0 = 64
      r%n = 0
      allocate (r%vert(3, n0), r%cell(2, n0), r%clr(2, n0), &
                r%dl(n0), r%kind(n0), r%ratio(n0))
   end subroutine rec_init

   subroutine rec_add(r, fv, cells, clrs, dl, kind, ratio)
      type(facet_rec_t), intent(inout) :: r
      integer(ik), intent(in) :: fv(3), cells(2), clrs(2)
      real(rk), intent(in) :: dl, ratio
      integer, intent(in) :: kind
      integer(ik), allocatable :: iv(:, :), ic(:, :), ik2(:, :)
      real(rk), allocatable :: rd(:), rr(:)
      integer, allocatable :: kk(:)
      integer :: m
      m = size(r%dl)
      if (r%n == m) then
         allocate (iv(3, 2*m), ic(2, 2*m), ik2(2, 2*m), rd(2*m), &
                   rr(2*m), kk(2*m))
         iv(:, 1:m) = r%vert
         ic(:, 1:m) = r%cell
         ik2(:, 1:m) = r%clr
         rd(1:m) = r%dl
         rr(1:m) = r%ratio
         kk(1:m) = r%kind
         call move_alloc(iv, r%vert)
         call move_alloc(ic, r%cell)
         call move_alloc(ik2, r%clr)
         call move_alloc(rd, r%dl)
         call move_alloc(rr, r%ratio)
         call move_alloc(kk, r%kind)
      end if
      r%n = r%n + 1
      r%vert(:, r%n) = fv
      r%cell(:, r%n) = cells
      r%clr(:, r%n) = clrs
      r%dl(r%n) = dl
      r%kind(r%n) = kind
      r%ratio(r%n) = ratio
   end subroutine rec_add

end module del_vor_delaunay
