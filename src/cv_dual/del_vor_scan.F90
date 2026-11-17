!-----------------------------------------------------------------------
! del_vor_scan: a fast reader of whitespace-separated text, for the
! AVS reader. The file is read in blocks of a fixed size (stream
! access), so memory does not grow with the file, and numbers are
! parsed in place:
!   integers by hand (exact; leading zeros and a sign allowed),
!   reals by C's strtod, which rounds correctly, so every number is
!   the same double as a Fortran read gives (a Fortran D exponent,
!   1.0D+00, is accepted too).
! Tokens are separated by blanks, tabs and line ends (LF, CR LF).
! A token must be shorter than maxtok characters.
!-----------------------------------------------------------------------
module del_vor_scan
   use, intrinsic :: iso_fortran_env, only: int64
   use, intrinsic :: iso_c_binding, only: c_char, c_double, c_ptr, &
                                          c_loc, c_null_char, &
                                          c_intptr_t
   use del_vor_kinds, only: ik, rk
   implicit none
   private
   public :: scan_t, scan_open, scan_close, scan_int, scan_real, &
             scan_word, scan_line, scan_skip_line

   integer, parameter :: blk = 1048576     ! bytes read at a time
   integer, parameter :: maxtok = 1024

   type :: scan_t
      integer :: lu = -1
      integer(int64) :: fsize = 0, fpos = 0   ! file size, bytes read
      integer :: n = 0, pos = 1               ! bytes in buf, cursor
      character(kind=c_char, len=1), allocatable :: buf(:)
   end type scan_t

   interface
      function c_strtod(str, endptr) bind(C, name='strtod')
         import :: c_ptr, c_double
         type(c_ptr), value :: str
         type(c_ptr) :: endptr
         real(c_double) :: c_strtod
      end function c_strtod
   end interface

contains

   subroutine scan_open(fname, sc, ierr)
      character(len=*), intent(in) :: fname
      type(scan_t), intent(inout) :: sc
      integer, intent(out) :: ierr
      open (newunit=sc%lu, file=fname, status='old', action='read', &
            access='stream', form='unformatted', iostat=ierr)
      if (ierr /= 0) return
      inquire (unit=sc%lu, size=sc%fsize)
      allocate (sc%buf(blk + maxtok + 1))
      sc%fpos = 0
      sc%n = 0
      sc%pos = 1
      call refill(sc)
   end subroutine scan_open

   subroutine scan_close(sc)
      type(scan_t), intent(inout) :: sc
      if (sc%lu >= 0 .or. sc%lu < -1) close (sc%lu)
      sc%lu = -1
      if (allocated(sc%buf)) deallocate (sc%buf)
   end subroutine scan_close

   ! Keep at least maxtok bytes after the cursor, unless the file ends
   ! first. The byte after the data is always a C null.
   subroutine refill(sc)
      type(scan_t), intent(inout) :: sc
      integer :: rem, k, ios
      rem = sc%n - sc%pos + 1
      if (rem >= maxtok .or. sc%fpos >= sc%fsize) return
      if (rem > 0) sc%buf(1:rem) = sc%buf(sc%pos:sc%n)
      if (rem < 0) rem = 0
      k = int(min(int(blk, int64), sc%fsize - sc%fpos))
      read (sc%lu, iostat=ios) sc%buf(rem + 1:rem + k)
      if (ios /= 0) k = 0
      sc%fpos = sc%fpos + k
      sc%n = rem + k
      sc%pos = 1
      sc%buf(sc%n + 1) = c_null_char
   end subroutine refill

   logical function is_blank(c)
      character(kind=c_char, len=1), intent(in) :: c
      is_blank = c == ' ' .or. c == achar(9) .or. c == achar(10) .or. &
                 c == achar(13)
   end function is_blank

   ! Move to the next token; false at the end of the file.
   logical function to_token(sc)
      type(scan_t), intent(inout) :: sc
      do
         call refill(sc)
         do while (sc%pos <= sc%n)
            if (.not. is_blank(sc%buf(sc%pos))) exit
            sc%pos = sc%pos + 1
         end do
         if (sc%pos <= sc%n) then
            call refill(sc)
            to_token = .true.
            return
         end if
         if (sc%fpos >= sc%fsize) then
            to_token = .false.
            return
         end if
      end do
   end function to_token

   subroutine scan_int(sc, iv, ok)
      type(scan_t), intent(inout) :: sc
      integer(ik), intent(out) :: iv
      logical, intent(out) :: ok
      integer :: p, d
      logical :: neg
      iv = 0
      ok = to_token(sc)
      if (.not. ok) return
      p = sc%pos
      neg = sc%buf(p) == '-'
      if (sc%buf(p) == '-' .or. sc%buf(p) == '+') p = p + 1
      ok = .false.
      do while (p <= sc%n)
         d = iachar(sc%buf(p)) - 48
         if (d < 0 .or. d > 9) exit
         iv = 10*iv + d
         ok = .true.
         p = p + 1
      end do
      ! the token must end here
      if (p <= sc%n) then
         if (.not. is_blank(sc%buf(p))) ok = .false.
      end if
      if (neg) iv = -iv
      if (ok) sc%pos = p
   end subroutine scan_int

   subroutine scan_real(sc, x, ok)
      type(scan_t), intent(inout), target :: sc
      real(rk), intent(out) :: x
      logical, intent(out) :: ok
      type(c_ptr) :: endp
      integer :: used, p, ios
      character(len=maxtok) :: tok
      x = 0.0_rk
      ok = to_token(sc)
      if (.not. ok) return
      x = real(c_strtod(c_loc(sc%buf(sc%pos)), endp), rk)
      used = int(transfer(endp, 0_c_intptr_t) - &
                 transfer(c_loc(sc%buf(sc%pos)), 0_c_intptr_t))
      p = sc%pos + used
      ok = used > 0
      if (ok .and. p <= sc%n) ok = is_blank(sc%buf(p))
      if (ok) then
         sc%pos = p
         return
      end if
      ! not a C number: a Fortran D exponent (1.0D+00), read the
      ! token with Fortran
      call scan_word(sc, tok)
      read (tok, *, iostat=ios) x
      ok = ios == 0
   end subroutine scan_real

   ! The next token as text (blank-padded).
   subroutine scan_word(sc, w)
      type(scan_t), intent(inout) :: sc
      character(len=*), intent(out) :: w
      integer :: p, k
      w = ' '
      if (.not. to_token(sc)) return
      p = sc%pos
      k = 0
      do while (p <= sc%n)
         if (is_blank(sc%buf(p))) exit
         k = k + 1
         if (k <= len(w)) w(k:k) = sc%buf(p)
         p = p + 1
      end do
      sc%pos = p
   end subroutine scan_word

   ! The rest of the current line, from the cursor (blank lines and
   ! leading blanks skipped first), and move past its end.
   subroutine scan_line(sc, s, ok)
      type(scan_t), intent(inout) :: sc
      character(len=*), intent(out) :: s
      logical, intent(out) :: ok
      integer :: k
      s = ' '
      ok = to_token(sc)
      if (.not. ok) return
      k = 0
      do
         if (sc%pos > sc%n) then
            call refill(sc)
            if (sc%pos > sc%n) return
         end if
         if (sc%buf(sc%pos) == achar(10)) exit
         if (sc%buf(sc%pos) /= achar(13)) then
            k = k + 1
            if (k <= len(s)) s(k:k) = sc%buf(sc%pos)
         end if
         sc%pos = sc%pos + 1
      end do
      sc%pos = sc%pos + 1
   end subroutine scan_line

   ! Move past the end of the current line.
   subroutine scan_skip_line(sc)
      type(scan_t), intent(inout) :: sc
      do
         call refill(sc)
         do while (sc%pos <= sc%n)
            if (sc%buf(sc%pos) == achar(10)) then
               sc%pos = sc%pos + 1
               return
            end if
            sc%pos = sc%pos + 1
         end do
         if (sc%fpos >= sc%fsize) return
      end do
   end subroutine scan_skip_line

end module del_vor_scan
