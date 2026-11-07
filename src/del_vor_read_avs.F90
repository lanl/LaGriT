!-----------------------------------------------------------------------
! del_vor_read_avs: read an AVS UCD (.inp) mesh of triangles or tets.
!
! Reads vertex coordinates, cells with their material (itetclr) and,
! if present, the vertex attribute imt1. Other attributes are skipped.
! Vertex and cell numbers in the file must run 1, 2, 3, ... in order,
! as LaGriT writes them. A mesh must be all triangles or all tets.
! Tet vertices are put in LaGriT order as read / avs does
! (avs_cells.f): AVS vertex j is LaGriT vertex lperm_tet(j), so a tet
! LaGriT counts as positive has det(x2-x1, x3-x1, x4-x1) > 0.
!
! Phase 1 reader: the full checks (jtet, orientation, conformity) come
! with the phase 2 front end.
!-----------------------------------------------------------------------
module del_vor_read_avs
   use del_vor_kinds, only: ik, rk
   use del_vor_mesh, only: mesh_t, mesh_free, ifelmtri, ifelmtet
   use del_vor_log, only: errmsg
   implicit none
   private
   public :: read_avs

   integer, parameter :: maxline = 8192
   integer, parameter :: lperm_tet(4) = [1, 2, 4, 3]

contains

   subroutine read_avs(fname, m, ierr)
      character(len=*), intent(in) :: fname
      type(mesh_t), intent(inout) :: m
      integer, intent(out) :: ierr

      character(len=maxline) :: line
      character(len=16) :: ctype
      character(len=64) :: aname
      integer :: lu, ios, k, icomma
      integer(ik) :: nn, nc, nnd, ncd, nmd, i, id, imat, nen
      integer(ik) :: v(4), nattr, ncomp_tot, jimt, j
      integer(ik), allocatable :: ncomp(:)
      real(rk), allocatable :: vals(:)

      ierr = 0
      call mesh_free(m)

      open (newunit=lu, file=fname, status='old', action='read', &
            iostat=ios)
      if (ios /= 0) then
         ierr = 1
         errmsg = 'read_avs: cannot open '//trim(fname)
         return
      end if

      call next_line(lu, line, ios)
      if (ios == 0) read (line, *, iostat=ios) nn, nc, nnd, ncd, nmd
      if (ios /= 0) then
         call fail('bad header line')
         return
      end if
      if (nn < 1 .or. nc < 1) then
         call fail('no vertices or no cells')
         return
      end if

      m%nnodes = nn
      m%nelements = nc
      allocate (m%xic(nn), m%yic(nn), m%zic(nn))
      allocate (m%imt1(nn))
      m%imt1 = 1

      ! vertices
      do i = 1, nn
         call next_line(lu, line, ios)
         if (ios == 0) read (line, *, iostat=ios) id, m%xic(i), &
            m%yic(i), m%zic(i)
         if (ios /= 0) then
            call fail('bad vertex line')
            return
         end if
         if (id /= i) then
            call fail('vertex numbers must run 1, 2, 3, ...')
            return
         end if
      end do

      ! cells
      allocate (m%itettyp(nc), m%itetclr(nc), m%itetoff(nc))
      nen = 0
      do i = 1, nc
         call next_line(lu, line, ios)
         if (ios == 0) read (line, *, iostat=ios) id, imat, ctype
         if (ios /= 0) then
            call fail('bad cell line')
            return
         end if
         if (id /= i) then
            call fail('cell numbers must run 1, 2, 3, ...')
            return
         end if
         select case (trim(lower(ctype)))
         case ('tri')
            k = 3
            m%itettyp(i) = ifelmtri
         case ('tet')
            k = 4
            m%itettyp(i) = ifelmtet
         case default
            call fail('cell type '//trim(ctype)// &
                      ' not allowed; triangles or tets only')
            return
         end select
         if (nen == 0) then
            nen = k
            m%nen = nen
            allocate (m%itet(nen*nc))
         else if (k /= nen) then
            call fail('mixed cell types; all triangles or all tets')
            return
         end if
         read (line, *, iostat=ios) id, imat, ctype, v(1:nen)
         if (ios /= 0) then
            call fail('bad cell line')
            return
         end if
         if (any(v(1:nen) < 1) .or. any(v(1:nen) > nn)) then
            call fail('cell refers to a vertex that does not exist')
            return
         end if
         m%itetoff(i) = nen*(i - 1)
         if (nen == 4) then
            m%itet(m%itetoff(i) + lperm_tet) = v(1:4)
         else
            m%itet(m%itetoff(i) + 1:m%itetoff(i) + 3) = v(1:3)
         end if
         m%itetclr(i) = imat
      end do

      ! vertex attributes: keep imt1 if present
      if (nnd > 0) then
         call next_line(lu, line, ios)
         if (ios == 0) read (line, *, iostat=ios) nattr
         if (ios /= 0 .or. nattr < 1) then
            call fail('bad vertex attribute header')
            return
         end if
         allocate (ncomp(nattr))
         read (line, *, iostat=ios) nattr, ncomp
         if (ios /= 0) then
            call fail('bad vertex attribute header')
            return
         end if
         ncomp_tot = sum(ncomp)
         jimt = 0
         j = 0
         do k = 1, int(nattr)
            call next_line(lu, line, ios)
            if (ios /= 0) then
               call fail('missing vertex attribute name')
               return
            end if
            icomma = index(line, ',')
            if (icomma == 0) icomma = len_trim(line) + 1
            aname = lower(adjustl(line(1:icomma - 1)))
            if (trim(aname) == 'imt1' .or. trim(aname) == 'imt') then
               if (ncomp(k) == 1) jimt = j + 1
            end if
            j = j + ncomp(k)
         end do
         allocate (vals(ncomp_tot))
         do i = 1, nn
            call next_line(lu, line, ios)
            if (ios == 0) read (line, *, iostat=ios) id, vals
            if (ios /= 0) then
               call fail('bad vertex attribute line')
               return
            end if
            if (jimt > 0) m%imt1(i) = nint(vals(jimt), ik)
         end do
      end if

      close (lu)

   contains

      subroutine fail(why)
         character(len=*), intent(in) :: why
         ierr = 1
         write (errmsg, '(a,a,a,a)') 'read_avs: ', trim(fname), &
            ': ', why
         close (lu)
      end subroutine fail

   end subroutine read_avs

   ! Next non-blank line; ios /= 0 at end of file.
   subroutine next_line(lu, line, ios)
      integer, intent(in) :: lu
      character(len=*), intent(out) :: line
      integer, intent(out) :: ios
      do
         read (lu, '(a)', iostat=ios) line
         if (ios /= 0) return
         if (len_trim(line) > 0) return
      end do
   end subroutine next_line

   function lower(s) result(t)
      character(len=*), intent(in) :: s
      character(len=len(s)) :: t
      integer :: i, ic
      t = s
      do i = 1, len(s)
         ic = iachar(s(i:i))
         if (ic >= 65 .and. ic <= 90) t(i:i) = achar(ic + 32)
      end do
   end function lower

end module del_vor_read_avs
