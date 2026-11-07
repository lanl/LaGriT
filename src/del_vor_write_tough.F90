!-----------------------------------------------------------------------
! del_vor_write_tough: TOUGH2 MESH file (ELEME and CONNE blocks).
!
! Ported from lanl/voronoi GridWriteTOUGH2 and its helpers, so files
! match voronoi's column for column:
!
! ELEME  one record per vertex: name, material, volume, AHTX, x y z.
!        Material is the rank of the vertex's imt1 among the sorted
!        distinct imt1 values. AHTX is written equal to the volume, as
!        voronoi did (open question for TOUGH users).
! CONNE  one record per edge i < j: names, ISOT, d1 = d2 = d(ij)/2,
!        area, and BETAX = cos(angle to gravity), blank when ~0.
!
! Changes from voronoi:
! - Values keep their sign; a negative value is written with one digit
!   less so it fits its 10 columns (voronoi wrote absolute values).
! - Zero-area connections are left out when |A| <= 1e-12 times the
!   largest |A|. Voronoi left out every connection with A < 1e-4,
!   which drops negative areas and, on small meshes, real ones.
!-----------------------------------------------------------------------
module del_vor_write_tough
   use del_vor_kinds, only: ik, rk
   use del_vor_log, only: errmsg
   implicit none
   private
   public :: write_tough, elne_name

   integer, parameter :: elne_len = 5
   real(rk), parameter :: zero_area_rel = 1.0e-12_rk

contains

   subroutine write_tough(fname, nnodes, xic, yic, zic, imt1, &
                          iedge_off, jedge, area, vol, ierr)
      character(len=*), intent(in) :: fname
      integer(ik), intent(in) :: nnodes
      real(rk), intent(in) :: xic(nnodes), yic(nnodes), zic(nnodes)
      integer(ik), intent(in) :: imt1(nnodes)
      integer(ik), intent(in) :: iedge_off(nnodes + 1), jedge(*)
      real(rk), intent(in) :: area(:), vol(nnodes)
      integer, intent(out) :: ierr

      character(len=80) :: rec
      character(len=5) :: ma
      character(len=12) :: tmp
      integer(ik), allocatable :: ulist(:)
      integer(ik) :: i, j, k, nu, nedges, im
      real(rk) :: d, cosine, amax
      integer :: lu, ios, isot

      ierr = 0
      nedges = iedge_off(nnodes + 1) - 1
      open (newunit=lu, file=fname, status='replace', action='write', &
            iostat=ios)
      if (ios /= 0) then
         ierr = 1
         errmsg = 'write_tough: cannot open '//trim(fname)
         return
      end if

      ! sorted distinct imt1 values
      call distinct_sorted(imt1, ulist, nu)

      write (lu, '(a)') 'ELEME----1----*----2----*----3----*----4'// &
         '----*----5----*----6----*----7----*----8'
      do i = 1, nnodes
         im = rank_of(imt1(i), ulist, nu)
         write (tmp, '(i0)') im
         ma = adjustr(tmp(1:5))
         rec = repeat(' ', 80)
         rec(1:5) = elne_name(i)
         rec(16:20) = ma
         rec(21:30) = fe(vol(i))
         rec(31:40) = fe(vol(i))
         rec(51:60) = fe(xic(i))
         rec(61:70) = fe(yic(i))
         rec(71:80) = fe(zic(i))
         write (lu, '(a)') rec
      end do

      write (lu, '(a)') ''
      write (lu, '(a)') 'CONNE----1----*----2----*----3----*----4'// &
         '----*----5----*----6----*----7----*----8'
      amax = 0.0_rk
      if (nedges > 0) amax = maxval(abs(area(1:nedges)))
      do i = 1, nnodes
         do k = iedge_off(i), iedge_off(i + 1) - 1
            j = jedge(k)
            if (abs(area(k)) <= zero_area_rel*amax) cycle
            d = norm2([xic(j) - xic(i), yic(j) - yic(i), &
                       zic(j) - zic(i)])
            cosine = (zic(i) - zic(j))/d
            isot = get_isot([xic(i), yic(i), zic(i)], &
                            [xic(j), yic(j), zic(j)])
            rec = repeat(' ', 80)
            rec(1:5) = elne_name(i)
            rec(6:10) = elne_name(j)
            write (rec(30:30), '(i1)') isot
            rec(31:40) = fes(0.5_rk*d)
            rec(41:50) = fes(0.5_rk*d)
            rec(51:60) = fes(area(k))
            if (.not. eps_equal(cosine, 0.0_rk)) then
               write (rec(61:70), '(e10.3)') cosine
            end if
            write (lu, '(a)') rec
         end do
      end do
      write (lu, '(a)') ' '
      close (lu)
      deallocate (ulist)
   end subroutine write_tough

   ! E10.4 as voronoi's elemeTOUGH2 (values below 1e-15 written as
   ! 0); a negative value gets E10.3 to fit 10 columns.
   function fe(v) result(s)
      real(rk), intent(in) :: v
      character(len=10) :: s
      real(rk) :: w
      w = v
      if (abs(w) < 1.0e-15_rk) w = 0.0_rk
      if (w < 0.0_rk) then
         write (s, '(e10.3)') w
      else
         write (s, '(e10.4)') w
      end if
   end function fe

   ! ES10.4 as voronoi's conneTOUGH2; a negative value gets ES10.3.
   function fes(v) result(s)
      real(rk), intent(in) :: v
      character(len=10) :: s
      if (v < 0.0_rk) then
         write (s, '(es10.3)') v
      else
         write (s, '(es10.4)') v
      end if
   end function fes

   logical function eps_equal(v1, v2)
      real(rk), intent(in) :: v1, v2
      eps_equal = abs(abs(v2) - abs(v1)) <= 2.0e-3_rk
   end function eps_equal

   ! voronoi getISOT: 1, 2 or 3 for a connection along x, y or z;
   ! otherwise by the coordinate plane it lies in; 0 if none.
   integer function get_isot(v1, v2)
      real(rk), intent(in) :: v1(3), v2(3)
      real(rk) :: u(3)
      u = v2 - v1
      u = u/norm2(u)
      if (eps_equal(u(1), 1.0_rk)) then
         get_isot = 1
      else if (eps_equal(u(2), 1.0_rk)) then
         get_isot = 2
      else if (eps_equal(u(3), 1.0_rk)) then
         get_isot = 3
      else if (eps_equal(u(1), 0.0_rk)) then
         get_isot = 2
      else if (eps_equal(u(2), 0.0_rk)) then
         get_isot = 1
      else if (eps_equal(u(3), 0.0_rk)) then
         get_isot = 1
      else
         get_isot = 0
      end if
   end function get_isot

   ! voronoi idxToELNE with length 5: three characters from
   ! [A-Z a-z 0-9] and the last two digits of the vertex number.
   ! Ported as is, quirks included, so names match voronoi's files.
   function elne_name(idx) result(res)
      integer(ik), intent(in) :: idx
      character(len=elne_len) :: res
      integer(ik), parameter :: nperm = 62
      integer(ik) :: prefix, postfix, num, rem, ic
      integer :: i
      prefix = idx/100
      postfix = mod(idx, 100_ik)
      num = prefix + 1
      res = repeat('A', elne_len)
      if (prefix == 0) num = -1
      i = 0
      do while (num > 0)
         num = num - 1
         rem = mod(num, nperm)
         ic = rem
         if (ic < 26) then
            ic = ic + 65
            if (i > 0) ic = ic + 1
         else if (ic < 52) then
            ic = ic - 27 + 97 + 1
         else
            ic = ic - 52 + 48
         end if
         if (3 - i >= 1) res(3 - i:3 - i) = achar(int(ic))
         num = (num - rem)/nperm
         i = i + 1
      end do
      write (res(4:5), '(i2.2)') postfix
   end function elne_name

   subroutine distinct_sorted(a, u, nu)
      integer(ik), intent(in) :: a(:)
      integer(ik), allocatable, intent(out) :: u(:)
      integer(ik), intent(out) :: nu
      integer(ik) :: i, j, t
      allocate (u(size(a)))
      nu = 0
      do i = 1, size(a)
         if (nu > 0) then
            if (any(u(1:nu) == a(i))) cycle
         end if
         nu = nu + 1
         u(nu) = a(i)
         ! keep u(1:nu) sorted
         j = nu
         do while (j > 1)
            if (u(j - 1) <= u(j)) exit
            t = u(j)
            u(j) = u(j - 1)
            u(j - 1) = t
            j = j - 1
         end do
      end do
   end subroutine distinct_sorted

   integer(ik) function rank_of(v, u, nu)
      integer(ik), intent(in) :: v, nu
      integer(ik), intent(in) :: u(:)
      integer(ik) :: k
      rank_of = 0
      do k = 1, nu
         if (u(k) == v) then
            rank_of = k
            return
         end if
      end do
   end function rank_of

end module del_vor_write_tough
