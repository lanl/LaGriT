!-----------------------------------------------------------------------
! del_vor_write_stor: FEHM .stor file (ASCII), in the layout LaGriT's
! matbld2d_stor / matbld3d_stor write. The format is documented at the
! top of LaGriT's src/matbld3d_stor.f.
!
! Every row lists its lower neighbors, itself and its upper neighbors,
! in increasing order. Each entry's value is -A(ij)/d(ij); diagonal
! entries are 0. With compression, equal values are written once and
! shared through the pointer block; values are equal when they differ
! by at most 1e-14 times the largest |value| (LaGriT's
! compressmatrixvalues rule and epsilon).
!-----------------------------------------------------------------------
module del_vor_write_stor
   use del_vor_kinds, only: ik, rk
   use del_vor_topology, only: build_lower
   use del_vor_log, only: errmsg
   implicit none
   private
   public :: write_stor, stor_date

   real(rk), parameter :: compress_eps = 1.0e-14_rk

contains

   subroutine write_stor(fname, ndim, nnodes, xic, yic, zic, &
                         iedge_off, jedge, area, vol, compress, ierr)
      character(len=*), intent(in) :: fname
      integer, intent(in) :: ndim
      integer(ik), intent(in) :: nnodes
      real(rk), intent(in) :: xic(nnodes), yic(nnodes), zic(nnodes)
      integer(ik), intent(in) :: iedge_off(nnodes + 1), jedge(*)
      real(rk), intent(in) :: area(:), vol(nnodes)
      logical, intent(in) :: compress
      integer, intent(out) :: ierr

      integer(ik), allocatable :: ilow_off(:), jlow(:), klow(:)
      integer(ik), allocatable :: irow(:), icol(:), iptr(:), idiag(:)
      integer(ik), allocatable :: isrt(:)
      real(rk), allocatable :: coef(:), uval(:)
      integer(ik) :: neq, nedges, ncoef, nwritten, ncon_max
      integer(ik) :: i, k, p, n, ig
      real(rk) :: vmax
      integer :: lu, ios
      character(len=24) :: date
      character(len=4) :: model

      ierr = 0
      neq = nnodes
      nedges = iedge_off(nnodes + 1) - 1
      ncoef = 2*nedges + neq

      call build_lower(nnodes, iedge_off, jedge, ilow_off, jlow, klow)

      allocate (irow(neq + 1), icol(ncoef), coef(ncoef), idiag(neq))
      irow(1) = neq + 1
      p = 0
      ncon_max = 0
      do i = 1, neq
         do k = ilow_off(i), ilow_off(i + 1) - 1
            p = p + 1
            icol(p) = jlow(k)
            coef(p) = area(klow(k))/dist(i, jlow(k))
         end do
         p = p + 1
         icol(p) = i
         coef(p) = 0.0_rk
         idiag(i) = neq + 1 + p
         do k = iedge_off(i), iedge_off(i + 1) - 1
            p = p + 1
            icol(p) = jedge(k)
            coef(p) = area(k)/dist(i, jedge(k))
         end do
         n = (ilow_off(i + 1) - ilow_off(i)) + 1 + &
             (iedge_off(i + 1) - iedge_off(i))
         irow(i + 1) = irow(i) + n
         ncon_max = max(ncon_max, n)
      end do
      deallocate (ilow_off, jlow, klow)

      ! pointers into the value block
      allocate (iptr(ncoef))
      if (compress) then
         allocate (isrt(ncoef))
         call sort_index(coef, isrt)
         vmax = maxval(abs(coef))
         allocate (uval(ncoef))
         ig = 1
         uval(1) = coef(isrt(1))
         iptr(isrt(1)) = 1
         do k = 2, ncoef
            if (abs(coef(isrt(k)) - uval(ig)) > compress_eps*vmax) then
               ig = ig + 1
               uval(ig) = coef(isrt(k))
            end if
            iptr(isrt(k)) = ig
         end do
         nwritten = ig
         deallocate (isrt)
      else
         do k = 1, ncoef
            iptr(k) = k
         end do
         nwritten = ncoef
      end if

      open (newunit=lu, file=fname, status='replace', action='write', &
            iostat=ios)
      if (ios /= 0) then
         ierr = 1
         errmsg = 'write_stor: cannot open '//trim(fname)
         return
      end if

      model = '3-D '
      if (ndim == 2) model = '2-D '
      date = stor_date()
      write (lu, '(a)') &
         'fehmstor ascir8i4 LaGriT Sparse Matrix Voronoi Coefficients'
      write (lu, '(a)') ' '//date//' '//model// &
         'Linear Diffusion Model (del_vor)'
      write (lu, '(5i10)') nwritten, neq, ncoef + neq + 1, 1, ncon_max
      write (lu, '(5(1pe20.12))') vol
      write (lu, '(5i10)') irow, icol
      write (lu, '(5i10)') iptr, (0_ik, i=1, neq + 1)
      write (lu, '(5i10)') idiag
      if (compress) then
         write (lu, '(5(1pe20.12))') (-uval(k), k=1, nwritten)
      else
         write (lu, '(5(1pe20.12))') (-coef(k), k=1, ncoef)
      end if
      close (lu)

   contains

      real(rk) function dist(i1, i2)
         integer(ik), intent(in) :: i1, i2
         dist = norm2([xic(i2) - xic(i1), yic(i2) - yic(i1), &
                       zic(i2) - zic(i1)])
      end function dist

   end subroutine write_stor

   ! Date in the form of fdate(): 'Wed Aug 29 11:09:22 2018'.
   function stor_date() result(s)
      character(len=24) :: s
      character(len=3), parameter :: dn(0:6) = ['Sun', 'Mon', 'Tue', &
                                                'Wed', 'Thu', 'Fri', &
                                                'Sat']
      character(len=3), parameter :: mn(12) = ['Jan', 'Feb', 'Mar', &
                                               'Apr', 'May', 'Jun', &
                                               'Jul', 'Aug', 'Sep', &
                                               'Oct', 'Nov', 'Dec']
      integer, parameter :: mt(12) = [0, 3, 2, 5, 0, 3, 5, 1, 4, 6, &
                                       2, 4]
      integer :: v(8), y, m, iw
      call date_and_time(values=v)
      ! day of week, Sakamoto's method
      y = v(1)
      m = v(2)
      if (m < 3) y = y - 1
      iw = mod(y + y/4 - y/100 + y/400 + mt(m) + v(3), 7)
      write (s, '(a3,1x,a3,1x,i2,1x,i2.2,":",i2.2,":",i2.2,1x,i4)') &
         dn(iw), mn(v(2)), v(3), v(5), v(6), v(7), v(1)
   end function stor_date

   ! Stable merge sort of an index array by value.
   subroutine sort_index(a, idx)
      real(rk), intent(in) :: a(:)
      integer(ik), intent(out) :: idx(:)
      integer(ik), allocatable :: tmp(:)
      integer(ik) :: n, i, w, lo, mid, hi, p, q, r
      n = size(a)
      do i = 1, n
         idx(i) = i
      end do
      allocate (tmp(n))
      w = 1
      do while (w < n)
         lo = 1
         do while (lo <= n)
            mid = min(lo + w - 1, n)
            hi = min(lo + 2*w - 1, n)
            p = lo
            q = mid + 1
            r = lo
            do while (p <= mid .and. q <= hi)
               if (a(idx(q)) < a(idx(p))) then
                  tmp(r) = idx(q)
                  q = q + 1
               else
                  tmp(r) = idx(p)
                  p = p + 1
               end if
               r = r + 1
            end do
            do while (p <= mid)
               tmp(r) = idx(p)
               p = p + 1
               r = r + 1
            end do
            do while (q <= hi)
               tmp(r) = idx(q)
               q = q + 1
               r = r + 1
            end do
            lo = lo + 2*w
         end do
         idx = tmp
         w = 2*w
      end do
      deallocate (tmp)
   end subroutine sort_index

end module del_vor_write_stor
