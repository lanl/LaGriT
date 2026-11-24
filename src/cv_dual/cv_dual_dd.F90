!-----------------------------------------------------------------------
! cv_dual_dd: double-double arithmetic (about 106 bits) for the
! circumcenter of a tet that is nearly flat.
!
! connect / exact makes the exact Delaunay mesh of the points as they
! are stored. Where points lie on common spheres to round-off (mirror
! images, for one) some of its tets are flat to round-off: their
! determinant is about 1e-17 of its size, and in double precision
! their circumcenter is round-off. They still have a true circumcenter,
! and since the mesh is Delaunay it lies between those of its
! neighbors. In double-double the differences of the coordinates are
! exact and the products and sums keep about 106 bits, so that
! circumcenter is found to about 1e-15 of the tet's size for a tet as
! flat as 1e-17.
!
! two_sum (Knuth) and two_prod (Dekker's split, no fused multiply-add)
! are error-free transformations; dd numbers are (hi, lo) with
! |lo| <= ulp(hi)/2 (Dekker 1971; Hida, Li and Bailey 2001).
!-----------------------------------------------------------------------
module cv_dual_dd
   use cv_dual_kinds, only: rk
   implicit none
   private
   public :: circumcenter_tet_dd

   type :: dd_t
      real(rk) :: hi = 0.0_rk
      real(rk) :: lo = 0.0_rk
   end type dd_t

contains

   pure subroutine two_sum(a, b, s, e)
      real(rk), intent(in) :: a, b
      real(rk), intent(out) :: s, e
      real(rk) :: bb
      s = a + b
      bb = s - a
      e = (a - (s - bb)) + (b - bb)
   end subroutine two_sum

   pure subroutine split(a, hi, lo)
      real(rk), intent(in) :: a
      real(rk), intent(out) :: hi, lo
      real(rk) :: c
      c = 134217729.0_rk*a             ! 2**27 + 1
      hi = c - (c - a)
      lo = a - hi
   end subroutine split

   pure subroutine two_prod(a, b, p, e)
      real(rk), intent(in) :: a, b
      real(rk), intent(out) :: p, e
      real(rk) :: ah, al, bh, bl
      p = a*b
      call split(a, ah, al)
      call split(b, bh, bl)
      e = ((ah*bh - p) + ah*bl + al*bh) + al*bl
   end subroutine two_prod

   pure function renorm(s, e) result(z)
      real(rk), intent(in) :: s, e
      type(dd_t) :: z
      z%hi = s + e
      z%lo = e - (z%hi - s)
   end function renorm

   pure function dd(a) result(z)
      real(rk), intent(in) :: a
      type(dd_t) :: z
      z%hi = a
      z%lo = 0.0_rk
   end function dd

   ! a - b of two doubles, exact
   pure function dsub(a, b) result(z)
      real(rk), intent(in) :: a, b
      type(dd_t) :: z
      real(rk) :: s, e
      call two_sum(a, -b, s, e)
      z = renorm(s, e)
   end function dsub

   pure function add(x, y) result(z)
      type(dd_t), intent(in) :: x, y
      type(dd_t) :: z
      real(rk) :: s, e
      call two_sum(x%hi, y%hi, s, e)
      e = e + (x%lo + y%lo)
      z = renorm(s, e)
   end function add

   pure function neg(x) result(z)
      type(dd_t), intent(in) :: x
      type(dd_t) :: z
      z%hi = -x%hi
      z%lo = -x%lo
   end function neg

   pure function mul(x, y) result(z)
      type(dd_t), intent(in) :: x, y
      type(dd_t) :: z
      real(rk) :: p, e
      call two_prod(x%hi, y%hi, p, e)
      e = e + (x%hi*y%lo + x%lo*y%hi)
      z = renorm(p, e)
   end function mul

   ! x / y to double precision (the last step)
   pure function quot(x, y) result(q)
      type(dd_t), intent(in) :: x, y
      real(rk) :: q
      type(dd_t) :: r
      real(rk) :: q1, q2
      q1 = x%hi/y%hi
      r = add(x, neg(mul(y, dd(q1))))
      q2 = r%hi/y%hi
      q = q1 + q2
   end function quot

   pure function cross_dd(a, b) result(c)
      type(dd_t), intent(in) :: a(3), b(3)
      type(dd_t) :: c(3)
      c(1) = add(mul(a(2), b(3)), neg(mul(a(3), b(2))))
      c(2) = add(mul(a(3), b(1)), neg(mul(a(1), b(3))))
      c(3) = add(mul(a(1), b(2)), neg(mul(a(2), b(1))))
   end function cross_dd

   pure function dot_dd(a, b) result(s)
      type(dd_t), intent(in) :: a(3), b(3)
      type(dd_t) :: s
      s = add(add(mul(a(1), b(1)), mul(a(2), b(2))), mul(a(3), b(3)))
   end function dot_dd

   ! Circumcenter of tet p1..p4 (the coordinates as stored) in
   ! double-double, returned relative to x0 (rounded once, at the end).
   ! ok false when the determinant is zero in double-double too (the
   ! four points coplanar as stored).
   pure subroutine circumcenter_tet_dd(p1, p2, p3, p4, x0, c, ok)
      real(rk), intent(in) :: p1(3), p2(3), p3(3), p4(3), x0(3)
      real(rk), intent(out) :: c(3)
      logical, intent(out) :: ok
      type(dd_t) :: a(3), b(3), d(3), bxd(3), dxa(3), axb(3), den
      type(dd_t) :: aa, bb, ddd, num
      integer :: k
      do k = 1, 3
         a(k) = dsub(p2(k), p1(k))
         b(k) = dsub(p3(k), p1(k))
         d(k) = dsub(p4(k), p1(k))
      end do
      bxd = cross_dd(b, d)
      dxa = cross_dd(d, a)
      axb = cross_dd(a, b)
      den = dot_dd(a, bxd)
      den = add(den, den)
      ok = den%hi /= 0.0_rk
      if (.not. ok) then
         c = p1 - x0
         return
      end if
      aa = dot_dd(a, a)
      bb = dot_dd(b, b)
      ddd = dot_dd(d, d)
      do k = 1, 3
         num = add(add(mul(aa, bxd(k)), mul(bb, dxa(k))), &
                   mul(ddd, axb(k)))
         num = add(dsub(p1(k), x0(k)), dd(quot(num, den)))
         c(k) = num%hi
      end do
   end subroutine circumcenter_tet_dd

end module cv_dual_dd
