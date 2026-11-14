!-----------------------------------------------------------------------
! del_vor_matrix: the coefficient matrix as a ParaView file, to see
! its shape (keyword matrix).
!
! One point per entry of the .stor matrix, at (x, y, z) = (j, -i, 0)
! (row 1 at the top, as a printed matrix): both halves, (i, j) and
! (j, i), and the diagonal (i, i); exactly the connections of the
! .stor (zero-area ones only with zero_area keep, flagged). Each point
! is a VTK_VERTEX cell, so ParaView's Threshold and Extract Selection
! work on single entries. Point data:
!   i, j          row and column (1-based, as in the .stor)
!   area          A(ij), signed; distance d(ij); coefficient A/d
!   class         0 diagonal, 1 boundary, 2 interior, 3 interface
!   negative      1: A/d < 0 (not a zero-area connection)
!   zero_area     1: a zero-area connection (zero_area keep)
!   alpha_min     the smallest alpha of the cells around the edge (of
!                 the vertex, on the diagonal); 1 with Voronoi
!   angle         degrees between the dual face's vector area and the
!                 edge (0 for a Voronoi face; 0 on the diagonal and
!                 for zero-area connections)
!   material_i, material_j   the vertices' imt1 (0 if none)
!   volume        the control volume (diagonal; 0 elsewhere)
! The diagonal's coefficient is 0, as the .stor writes it.
!
! With matrix_bins N, also an N x N image (.vti): the rows and the
! columns are cut into N groups, row i into group (i-1)*N/n (n the
! number of vertices), and each pixel holds the entries of its block:
! count, negative (how many negative) and min_coefficient (the
! smallest off-diagonal A/d; 0 if none). It lies over the points:
! pixel edges at half-integers.
!-----------------------------------------------------------------------
module del_vor_matrix
   use del_vor_kinds, only: ik, rk
   use del_vor_mesh, only: mesh_t
   use del_vor_topology, only: build_lower, edge_index
   use del_vor_geometry, only: cross
   use del_vor_vtu, only: vtu_grid_t, vtu_data_t, add_pdata
   implicit none
   private
   public :: build_matrix_grid, matrix_shape, matrix_bins

contains

   ! The point grid of the rows sel(i) = .true.; keep: zero-area
   ! connections are in the .stor (zero_area keep).
   subroutine build_matrix_grid(m, iedge_off, jedge, area, vol, avec, &
                                zero, keep, bedge, iedge, alpha, sel, g)
      type(mesh_t), intent(in) :: m
      integer(ik), intent(in) :: iedge_off(:), jedge(:)
      real(rk), intent(in) :: area(:), vol(:), avec(:, :), alpha(:)
      logical, intent(in) :: zero(:), keep, bedge(:), iedge(:), sel(:)
      type(vtu_grid_t), intent(out) :: g
      integer(ik), allocatable :: ilow_off(:), jlow(:), klow(:)
      integer(ik), allocatable :: pi(:), pj(:), pc(:), pn(:), pz(:)
      integer(ik), allocatable :: pmi(:), pmj(:)
      real(rk), allocatable :: pa(:), pd(:), pk(:), pal(:), pan(:)
      real(rk), allocatable :: pv(:), ealpha(:), valpha(:), eang(:)
      integer(ik) :: nn, i, k, j, n, p, it, v(4), kk
      integer :: a, b
      real(rk) :: e(3), an
      real(rk), parameter :: deg = 180.0_rk/acos(-1.0_rk)

      nn = m%nnodes
      call build_lower(nn, iedge_off, jedge, ilow_off, jlow, klow)
      ! alpha per edge and per vertex: the smallest of their cells
      allocate (ealpha(size(area)), valpha(nn), eang(size(area)))
      ealpha = huge(1.0_rk)
      valpha = huge(1.0_rk)
      do it = 1, m%nelements
         v(1:m%nen) = m%itet(m%itetoff(it) + 1:m%itetoff(it) + m%nen)
         do a = 1, int(m%nen)
            valpha(v(a)) = min(valpha(v(a)), alpha(it))
            do b = a + 1, int(m%nen)
               kk = edge_index(v(a), v(b), nn, iedge_off, jedge)
               if (kk > 0) ealpha(kk) = min(ealpha(kk), alpha(it))
            end do
         end do
      end do
      where (ealpha > 1.0_rk) ealpha = 1.0_rk
      where (valpha > 1.0_rk) valpha = 1.0_rk
      ! the angle of each edge's dual face (its vector area, i to j)
      eang = 0.0_rk
      do i = 1, nn
         do k = iedge_off(i), iedge_off(i + 1) - 1
            if (zero(k)) cycle
            an = norm2(avec(:, k))
            if (an <= 0.0_rk) cycle
            j = jedge(k)
            e = [m%xic(j) - m%xic(i), m%yic(j) - m%yic(i), &
                 m%zic(j) - m%zic(i)]
            ! atan2 keeps small angles accurate (acos near 1 does not)
            eang(k) = atan2(norm2(cross(avec(:, k), e)), &
                            dot_product(avec(:, k), e))*deg
         end do
      end do

      ! count the points
      n = 0
      do i = 1, nn
         if (.not. sel(i)) cycle
         n = n + 1
         do k = ilow_off(i), ilow_off(i + 1) - 1
            if (in_stor(klow(k))) n = n + 1
         end do
         do k = iedge_off(i), iedge_off(i + 1) - 1
            if (in_stor(k)) n = n + 1
         end do
      end do
      allocate (pi(n), pj(n), pc(n), pn(n), pz(n), pmi(n), pmj(n), &
                pa(n), pd(n), pk(n), pal(n), pan(n), pv(n))
      ! each row: lower neighbors, the diagonal, upper neighbors, in
      ! increasing column order (as the .stor)
      p = 0
      do i = 1, nn
         if (.not. sel(i)) cycle
         do k = ilow_off(i), ilow_off(i + 1) - 1
            if (in_stor(klow(k))) call put(i, jlow(k), klow(k))
         end do
         call put(i, i, 0_ik)
         do k = iedge_off(i), iedge_off(i + 1) - 1
            if (in_stor(k)) call put(i, jedge(k), k)
         end do
      end do

      g%np = n
      g%nc = n
      allocate (g%xyz(3, n), g%conn(n), g%off(n), g%types(n))
      do p = 1, n
         g%xyz(:, p) = [real(pj(p), rk), -real(pi(p), rk), 0.0_rk]
         g%conn(p) = p
         g%off(p) = p
      end do
      g%types = int(1, kind(g%types))
      call add_pdata(g, 'i', i=pi)
      call add_pdata(g, 'j', i=pj)
      call add_pdata(g, 'area', r=pa)
      call add_pdata(g, 'distance', r=pd)
      call add_pdata(g, 'coefficient', r=pk)
      call add_pdata(g, 'class', i=pc)
      call add_pdata(g, 'negative', i=pn)
      call add_pdata(g, 'zero_area', i=pz)
      call add_pdata(g, 'alpha_min', r=pal)
      call add_pdata(g, 'angle', r=pan)
      call add_pdata(g, 'material_i', i=pmi)
      call add_pdata(g, 'material_j', i=pmj)
      call add_pdata(g, 'volume', r=pv)

   contains

      logical function in_stor(kq)
         integer(ik), intent(in) :: kq
         in_stor = keep .or. .not. zero(kq)
      end function in_stor

      ! entry (r, c) of edge kq (0: the diagonal)
      subroutine put(r, c, kq)
         integer(ik), intent(in) :: r, c, kq
         p = p + 1
         pi(p) = r
         pj(p) = c
         pmi(p) = mat(r)
         pmj(p) = mat(c)
         if (kq == 0) then
            pa(p) = 0.0_rk
            pd(p) = 0.0_rk
            pk(p) = 0.0_rk
            pc(p) = 0
            pn(p) = 0
            pz(p) = 0
            pal(p) = valpha(r)
            pan(p) = 0.0_rk
            pv(p) = vol(r)
            return
         end if
         pa(p) = area(kq)
         pd(p) = norm2([m%xic(c) - m%xic(r), m%yic(c) - m%yic(r), &
                        m%zic(c) - m%zic(r)])
         pk(p) = area(kq)/pd(p)
         pc(p) = 2
         if (bedge(kq)) pc(p) = 1
         if (iedge(kq)) pc(p) = 3
         pz(p) = merge(1_ik, 0_ik, zero(kq))
         pn(p) = merge(1_ik, 0_ik, area(kq) < 0.0_rk .and. .not. &
                       zero(kq))
         pal(p) = ealpha(kq)
         pan(p) = eang(kq)
         pv(p) = 0.0_rk
      end subroutine put

      integer(ik) function mat(q)
         integer(ik), intent(in) :: q
         mat = 0
         if (allocated(m%imt1)) mat = m%imt1(q)
      end function mat

   end subroutine build_matrix_grid

   ! The bandwidth (largest |i - j|) and the profile (the sum over
   ! the rows of i minus the smallest column in row i) of the whole
   ! .stor matrix.
   subroutine matrix_shape(nn, iedge_off, jedge, zero, keep, nent, &
                           bw, prof)
      integer(ik), intent(in) :: nn, iedge_off(:), jedge(:)
      logical, intent(in) :: zero(:), keep
      integer(ik), intent(out) :: nent, bw, prof
      integer(ik), allocatable :: jmin(:)
      integer(ik) :: i, k, j
      allocate (jmin(nn))
      do i = 1, nn
         jmin(i) = i
      end do
      nent = nn
      bw = 0
      do i = 1, nn
         do k = iedge_off(i), iedge_off(i + 1) - 1
            if (zero(k) .and. .not. keep) cycle
            j = jedge(k)
            nent = nent + 2
            bw = max(bw, abs(j - i))
            jmin(max(i, j)) = min(jmin(max(i, j)), min(i, j))
         end do
      end do
      prof = 0
      do i = 1, nn
         prof = prof + (i - jmin(i))
      end do
   end subroutine matrix_shape

   ! The N x N image of the points of g (n vertices): its cell data in
   ! d(1:3), its origin and spacing.
   subroutine matrix_bins(g, nn, nbin, d, origin, spacing)
      type(vtu_grid_t), intent(in) :: g
      integer(ik), intent(in) :: nn, nbin
      type(vtu_data_t), intent(out) :: d(3)
      real(rk), intent(out) :: origin(3), spacing(3)
      integer(ik), allocatable :: cnt(:), neg(:)
      real(rk), allocatable :: cmin(:)
      logical, allocatable :: seen(:)
      integer(ik) :: p, bx, by, c, r, col
      real(rk) :: x
      allocate (cnt(nbin*nbin), neg(nbin*nbin), cmin(nbin*nbin), &
                seen(nbin*nbin))
      cnt = 0
      neg = 0
      cmin = 0.0_rk
      seen = .false.
      ! g's point data, in build_matrix_grid's order: 1 i, 2 j,
      ! 5 coefficient, 7 negative
      do p = 1, g%np
         r = g%pdata(1)%i(p)
         col = g%pdata(2)%i(p)
         by = nbin - 1 - (r - 1)*nbin/nn
         bx = (col - 1)*nbin/nn
         c = bx + nbin*by + 1
         cnt(c) = cnt(c) + 1
         neg(c) = neg(c) + g%pdata(7)%i(p)
         if (r == col) cycle
         x = g%pdata(5)%r(p)
         if (.not. seen(c) .or. x < cmin(c)) cmin(c) = x
         seen(c) = .true.
      end do
      d(1)%name = 'count'
      d(1)%is_real = .false.
      d(1)%i = cnt
      d(2)%name = 'negative'
      d(2)%is_real = .false.
      d(2)%i = neg
      d(3)%name = 'min_coefficient'
      d(3)%is_real = .true.
      d(3)%r = cmin
      spacing = [real(nn, rk)/real(nbin, rk), &
                 real(nn, rk)/real(nbin, rk), 1.0_rk]
      origin = [0.5_rk, -(real(nn, rk) + 0.5_rk), 0.0_rk]
   end subroutine matrix_bins

end module del_vor_matrix
