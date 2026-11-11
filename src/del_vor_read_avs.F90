!-----------------------------------------------------------------------
! del_vor_read_avs: read an AVS UCD (.inp) mesh of triangles or tets.
!
! Reads vertex coordinates, cells with their material (itetclr) and,
! if present, the vertex attributes imt1, itp1 and isn1 (LaGriT's
! material, type and parent/child link). Other attributes, and all
! cell attributes, are skipped.
! Vertex and cell numbers in the file must run 1, 2, 3, ... in order,
! as LaGriT writes them. A mesh must be all triangles or all tets.
! Tet vertices are put in LaGriT order as read / avs does
! (avs_cells.f): AVS vertex j is LaGriT vertex lperm_tet(j), so a tet
! LaGriT counts as positive has det(x2-x1, x3-x1, x4-x1) > 0.
!
! The text is read by del_vor_scan (blocks of the file, numbers parsed
! in place, reals by strtod: the same doubles as a Fortran read, about
! 10 times faster).
!
! The checks of the mesh itself (jtet, conformity, orientation, size)
! are in del_vor_jtet and del_vor_validity.
!-----------------------------------------------------------------------
module del_vor_read_avs
   use del_vor_kinds, only: ik, rk
   use del_vor_mesh, only: mesh_t, mesh_free, ifelmtri, ifelmtet
   use del_vor_log, only: errmsg
   use del_vor_scan, only: scan_t, scan_open, scan_close, scan_int, &
                           scan_real, scan_word, scan_line, &
                           scan_skip_line
   implicit none
   private
   public :: read_avs

   integer, parameter :: maxline = 1024
   integer, parameter :: lperm_tet(4) = [1, 2, 4, 3]

contains

   subroutine read_avs(fname, m, ierr)
      character(len=*), intent(in) :: fname
      type(mesh_t), intent(inout) :: m
      integer, intent(out) :: ierr

      type(scan_t), target :: sc
      character(len=maxline) :: line
      character(len=16) :: ctype
      character(len=64) :: aname
      integer :: k, icomma, ios
      integer(ik) :: nn, nc, nnd, ncd, nmd, i, id, imat, nen
      integer(ik) :: v(4), nattr, ncomp_tot, jimt, jitp, jisn, j
      integer(ik), allocatable :: ncomp(:)
      real(rk), allocatable :: vals(:)
      logical :: ok

      ierr = 0
      call mesh_free(m)

      call scan_open(fname, sc, ios)
      if (ios /= 0) then
         ierr = 1
         errmsg = 'read_avs: cannot open '//trim(fname)
         return
      end if

      call scan_int(sc, nn, ok)
      if (ok) call scan_int(sc, nc, ok)
      if (ok) call scan_int(sc, nnd, ok)
      if (ok) call scan_int(sc, ncd, ok)
      if (ok) call scan_int(sc, nmd, ok)
      if (.not. ok) then
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
      allocate (m%imt1(nn), m%itp1(nn), m%isn1(nn))
      m%imt1 = 1
      m%itp1 = 0
      m%isn1 = 0

      ! vertices
      do i = 1, nn
         call scan_int(sc, id, ok)
         if (ok) call scan_real(sc, m%xic(i), ok)
         if (ok) call scan_real(sc, m%yic(i), ok)
         if (ok) call scan_real(sc, m%zic(i), ok)
         if (.not. ok) then
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
         call scan_int(sc, id, ok)
         if (ok) call scan_int(sc, imat, ok)
         if (ok) call scan_word(sc, ctype)
         if (.not. ok .or. len_trim(ctype) == 0) then
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
         do k = 1, int(nen)
            call scan_int(sc, v(k), ok)
            if (.not. ok) then
               call fail('bad cell line')
               return
            end if
         end do
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

      ! vertex attributes: keep imt1, itp1, isn1 if present
      if (nnd > 0) then
         call scan_int(sc, nattr, ok)
         if (.not. ok .or. nattr < 1) then
            call fail('bad vertex attribute header')
            return
         end if
         allocate (ncomp(nattr))
         do k = 1, int(nattr)
            call scan_int(sc, ncomp(k), ok)
            if (.not. ok) then
               call fail('bad vertex attribute header')
               return
            end if
         end do
         call scan_skip_line(sc)
         ncomp_tot = sum(ncomp)
         jimt = 0
         jitp = 0
         jisn = 0
         j = 0
         do k = 1, int(nattr)
            call scan_line(sc, line, ok)
            if (.not. ok) then
               call fail('missing vertex attribute name')
               return
            end if
            icomma = index(line, ',')
            if (icomma == 0) icomma = len_trim(line) + 1
            aname = lower(adjustl(line(1:icomma - 1)))
            if (ncomp(k) == 1) then
               select case (trim(aname))
               case ('imt1', 'imt')
                  jimt = j + 1
               case ('itp1', 'itp')
                  jitp = j + 1
               case ('isn1', 'isn')
                  jisn = j + 1
               end select
            end if
            j = j + ncomp(k)
         end do
         allocate (vals(ncomp_tot))
         ! token by token: LaGriT wraps a vertex's values onto more
         ! lines when there are many
         do i = 1, nn
            call scan_int(sc, id, ok)
            do k = 1, int(ncomp_tot)
               if (ok) call scan_real(sc, vals(k), ok)
            end do
            if (.not. ok) then
               call fail('bad vertex attribute line')
               return
            end if
            if (jimt > 0) m%imt1(i) = nint(vals(jimt), ik)
            if (jitp > 0) m%itp1(i) = nint(vals(jitp), ik)
            if (jisn > 0) m%isn1(i) = nint(vals(jisn), ik)
         end do
      end if

      call scan_close(sc)

   contains

      subroutine fail(why)
         character(len=*), intent(in) :: why
         ierr = 1
         write (errmsg, '(a,a,a,a)') 'read_avs: ', trim(fname), &
            ': ', why
         call scan_close(sc)
      end subroutine fail

   end subroutine read_avs

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
