!-----------------------------------------------------------------------
! del_vor_topology: the edge CSR.
!
! The off-diagonal sparsity of the coefficient matrix is exactly the
! mesh's edge graph. It is stored once, upper triangle only:
!
!   row i holds the vertices j > i that share an edge with i,
!   sorted, in jedge(iedge_off(i) : iedge_off(i+1)-1).
!
! Built in two passes over a temporary vertex-to-cell incidence list,
! which is freed before return. edge_index finds edge (i,j) by binary
! search in row min(i,j).
!-----------------------------------------------------------------------
module del_vor_topology
   use del_vor_kinds, only: ik
   use del_vor_log, only: errmsg
   implicit none
   private
   public :: build_edge_csr, edge_index, build_lower

contains

   subroutine build_edge_csr(nnodes, nelements, nen, itetoff, itet, &
                             iedge_off, jedge, nedges, ierr)
      integer(ik), intent(in) :: nnodes, nelements, nen
      integer(ik), intent(in) :: itetoff(nelements)
      integer(ik), intent(in) :: itet(*)
      integer(ik), allocatable, intent(out) :: iedge_off(:), jedge(:)
      integer(ik), intent(out) :: nedges
      integer, intent(out) :: ierr

      integer(ik), allocatable :: inc_off(:), inc(:), mark(:)
      integer(ik) :: it, k, i, j, iv, n, pos, ic

      ierr = 0
      nedges = 0

      ! vertex-to-cell incidence: count, then fill
      allocate (inc_off(nnodes + 1))
      inc_off = 0
      do it = 1, nelements
         do k = 1, nen
            iv = itet(itetoff(it) + k)
            inc_off(iv + 1) = inc_off(iv + 1) + 1
         end do
      end do
      inc_off(1) = 1
      do i = 1, nnodes
         inc_off(i + 1) = inc_off(i + 1) + inc_off(i)
      end do
      allocate (inc(inc_off(nnodes + 1) - 1))
      allocate (mark(nnodes))
      mark = inc_off(1:nnodes)
      do it = 1, nelements
         do k = 1, nen
            iv = itet(itetoff(it) + k)
            inc(mark(iv)) = it
            mark(iv) = mark(iv) + 1
         end do
      end do

      ! row lengths: neighbors j > i, counted once each
      allocate (iedge_off(nnodes + 1))
      mark = 0
      iedge_off(1) = 1
      do i = 1, nnodes
         n = 0
         do ic = inc_off(i), inc_off(i + 1) - 1
            it = inc(ic)
            do k = 1, nen
               j = itet(itetoff(it) + k)
               if (j > i .and. mark(j) /= i) then
                  mark(j) = i
                  n = n + 1
               end if
            end do
         end do
         iedge_off(i + 1) = iedge_off(i) + n
      end do
      nedges = iedge_off(nnodes + 1) - 1

      ! fill and sort each row
      allocate (jedge(nedges))
      mark = 0
      do i = 1, nnodes
         pos = iedge_off(i)
         do ic = inc_off(i), inc_off(i + 1) - 1
            it = inc(ic)
            do k = 1, nen
               j = itet(itetoff(it) + k)
               if (j > i .and. mark(j) /= i) then
                  mark(j) = i
                  jedge(pos) = j
                  pos = pos + 1
               end if
            end do
         end do
         call isort(jedge(iedge_off(i):iedge_off(i + 1) - 1))
      end do

      deallocate (inc_off, inc, mark)

      if (nedges < 1) then
         ierr = 1
         errmsg = 'build_edge_csr: mesh has no edges'
      end if
   end subroutine build_edge_csr

   ! Position of edge (i,j) in jedge, or 0 if there is no such edge.
   pure function edge_index(i, j, nnodes, iedge_off, jedge) result(k)
      integer(ik), intent(in) :: i, j, nnodes
      integer(ik), intent(in) :: iedge_off(nnodes + 1), jedge(*)
      integer(ik) :: k
      integer(ik) :: a, b, lo, hi, mid
      a = min(i, j)
      b = max(i, j)
      k = 0
      lo = iedge_off(a)
      hi = iedge_off(a + 1) - 1
      do while (lo <= hi)
         mid = (lo + hi)/2
         if (jedge(mid) == b) then
            k = mid
            return
         else if (jedge(mid) < b) then
            lo = mid + 1
         else
            hi = mid - 1
         end if
      end do
   end function edge_index

   ! Lower triangle as a transpose of the upper one, for writers that
   ! need full rows: row i lists j < i in ilow_off(i):ilow_off(i+1)-1,
   ! ascending, with klow = the edge's position in jedge.
   subroutine build_lower(nnodes, iedge_off, jedge, ilow_off, jlow, &
                          klow)
      integer(ik), intent(in) :: nnodes
      integer(ik), intent(in) :: iedge_off(nnodes + 1), jedge(*)
      integer(ik), allocatable, intent(out) :: ilow_off(:), jlow(:)
      integer(ik), allocatable, intent(out) :: klow(:)
      integer(ik) :: nedges, i, k, j
      integer(ik), allocatable :: pos(:)

      nedges = iedge_off(nnodes + 1) - 1
      allocate (ilow_off(nnodes + 1), jlow(nedges), klow(nedges))
      ilow_off = 0
      do k = 1, nedges
         j = jedge(k)
         ilow_off(j + 1) = ilow_off(j + 1) + 1
      end do
      ilow_off(1) = 1
      do i = 1, nnodes
         ilow_off(i + 1) = ilow_off(i + 1) + ilow_off(i)
      end do
      allocate (pos(nnodes))
      pos = ilow_off(1:nnodes)
      ! rows i are visited in increasing order, so each lower row
      ! comes out sorted
      do i = 1, nnodes
         do k = iedge_off(i), iedge_off(i + 1) - 1
            j = jedge(k)
            jlow(pos(j)) = i
            klow(pos(j)) = k
            pos(j) = pos(j) + 1
         end do
      end do
      deallocate (pos)
   end subroutine build_lower

   ! Insertion sort; rows are short (about 7 to 20 entries).
   pure subroutine isort(a)
      integer(ik), intent(inout) :: a(:)
      integer(ik) :: i, j, t
      do i = 2, size(a)
         t = a(i)
         j = i - 1
         do while (j >= 1)
            if (a(j) <= t) exit
            a(j + 1) = a(j)
            j = j - 1
         end do
         a(j + 1) = t
      end do
   end subroutine isort

end module del_vor_topology
