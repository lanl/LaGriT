!-----------------------------------------------------------------------
! del_vor_write_uge: PFLOTRAN explicit unstructured grid (.uge) file.
!
!   CELLS n
!   id  x  y  z  volume                  one line per vertex
!   CONNECTIONS m
!   i  j  x  y  z  area                  one line per edge, i < j
!
! As in lanl/voronoi, the connection point is the edge midpoint.
! Connections with skip(k) = .true. (zero-area ones, unless
! zero_area keep) are left out. Unlike voronoi, volumes and areas keep
! their sign (voronoi wrote absolute values, which hides negative
! coefficients).
!-----------------------------------------------------------------------
module del_vor_write_uge
   use del_vor_kinds, only: ik, rk
   use del_vor_log, only: errmsg
   implicit none
   private
   public :: write_uge

contains

   subroutine write_uge(fname, nnodes, xic, yic, zic, iedge_off, &
                        jedge, area, vol, skip, ierr)
      character(len=*), intent(in) :: fname
      integer(ik), intent(in) :: nnodes
      real(rk), intent(in) :: xic(nnodes), yic(nnodes), zic(nnodes)
      integer(ik), intent(in) :: iedge_off(nnodes + 1), jedge(*)
      real(rk), intent(in) :: area(:), vol(nnodes)
      logical, intent(in) :: skip(:)
      integer, intent(out) :: ierr
      integer(ik) :: i, j, k, nedges, nkeep
      integer :: lu, ios

      ierr = 0
      nedges = iedge_off(nnodes + 1) - 1
      open (newunit=lu, file=fname, status='replace', action='write', &
            iostat=ios)
      if (ios /= 0) then
         ierr = 1
         errmsg = 'write_uge: cannot open '//trim(fname)
         return
      end if

      write (lu, '(a,1x,i11)') 'CELLS', nnodes
      do i = 1, nnodes
         write (lu, '(i11,4(2x,1pe20.12))') i, xic(i), yic(i), zic(i), &
            vol(i)
      end do
      nkeep = count(.not. skip(1:nedges))
      write (lu, '(a,1x,i11)') 'CONNECTIONS', nkeep
      do i = 1, nnodes
         do k = iedge_off(i), iedge_off(i + 1) - 1
            if (skip(k)) cycle
            j = jedge(k)
            write (lu, '(i11,2x,i11,4(2x,1pe20.12))') i, j, &
               0.5_rk*(xic(i) + xic(j)), 0.5_rk*(yic(i) + yic(j)), &
               0.5_rk*(zic(i) + zic(j)), area(k)
         end do
      end do
      close (lu)
   end subroutine write_uge

end module del_vor_write_uge
