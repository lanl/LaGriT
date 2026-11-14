!-----------------------------------------------------------------------
! del_vor_vtu: a VTK XML unstructured grid (.vtu) writer, by hand, no
! VTK library. Points, cells of any type, polyhedra, point and cell
! data; ASCII or appended raw binary (little endian, UInt64 headers).
!
! Polyhedra (cell type 42) in one of VTK's two layouts:
!   new (file version 2.3, VTK 9.4 and later; ParaView 6):
!     face_connectivity, face_offsets      the faces' point ids
!     polyhedron_to_faces, polyhedron_offsets   each cell's face ids
!   legacy (file version 2.2; also ParaView 5):
!     faces        per polyhedron: nfaces, then per face npts, ids
!     faceoffsets  end of each cell's entry in faces (-1: none)
! VTK's reader picks the layout by the file version (2.3 and later:
! new). Offsets are end offsets with no leading 0 and ids count from
! 0, as VTK writes them. Every polyhedron has its own faces, each
! wound with its normal outward (VTK requires outward faces; a face
! shared by two cells would point inward for one of them).
!
! The caller fills a vtu_grid_t with 1-based point ids; they are
! written 0-based.
!-----------------------------------------------------------------------
module del_vor_vtu
   use, intrinsic :: iso_fortran_env, only: int8, int64, real64
   use del_vor_kinds, only: ik, rk
   use del_vor_log, only: errmsg
   implicit none
   private
   public :: vtu_grid_t, vtu_data_t, write_vtu, add_cdata, add_pdata, &
             write_pvd

   type :: vtu_data_t
      character(len=32) :: name = ' '
      integer :: ncomp = 1
      logical :: is_real = .true.
      real(rk), allocatable :: r(:)
      integer(ik), allocatable :: i(:)
   end type vtu_data_t

   type :: vtu_grid_t
      integer(ik) :: np = 0, nc = 0
      real(rk), allocatable :: xyz(:, :)        ! (3, np)
      integer(ik), allocatable :: conn(:)       ! cell point ids
      integer(ik), allocatable :: off(:)        ! (nc) end offsets
      integer(int8), allocatable :: types(:)    ! (nc) VTK types
      ! polyhedra: every face, and the faces of each cell
      integer(ik) :: nf = 0
      integer(ik), allocatable :: fconn(:)      ! face point ids
      integer(ik), allocatable :: foff(:)       ! (nf) end offsets
      integer(ik), allocatable :: pfaces(:)     ! face ids per cell
      integer(ik), allocatable :: poff(:)       ! (nc) end offsets
      integer :: npd = 0, ncd = 0
      type(vtu_data_t) :: pdata(12), cdata(12)
   end type vtu_grid_t

contains

   subroutine add_cdata(g, name, r, i, ncomp)
      type(vtu_grid_t), intent(inout) :: g
      character(len=*), intent(in) :: name
      real(rk), intent(in), optional :: r(:)
      integer(ik), intent(in), optional :: i(:)
      integer, intent(in), optional :: ncomp
      g%ncd = g%ncd + 1
      call fill(g%cdata(g%ncd), name, r, i, ncomp)
   end subroutine add_cdata

   subroutine add_pdata(g, name, r, i, ncomp)
      type(vtu_grid_t), intent(inout) :: g
      character(len=*), intent(in) :: name
      real(rk), intent(in), optional :: r(:)
      integer(ik), intent(in), optional :: i(:)
      integer, intent(in), optional :: ncomp
      g%npd = g%npd + 1
      call fill(g%pdata(g%npd), name, r, i, ncomp)
   end subroutine add_pdata

   subroutine fill(d, name, r, i, ncomp)
      type(vtu_data_t), intent(out) :: d
      character(len=*), intent(in) :: name
      real(rk), intent(in), optional :: r(:)
      integer(ik), intent(in), optional :: i(:)
      integer, intent(in), optional :: ncomp
      d%name = name
      if (present(ncomp)) d%ncomp = ncomp
      d%is_real = present(r)
      if (present(r)) d%r = r
      if (present(i)) d%i = i
   end subroutine fill

   ! Write g to fname. binary: appended raw data; legacy: the legacy
   ! polyhedron layout.
   subroutine write_vtu(fname, g, binary, legacy, ierr)
      character(len=*), intent(in) :: fname
      type(vtu_grid_t), intent(in) :: g
      logical, intent(in) :: binary, legacy
      integer, intent(out) :: ierr

      integer :: lu, ios, k
      integer(int64) :: pos
      logical :: poly
      integer(ik), allocatable :: lfaces(:), lfoff(:)
      character(len=1), parameter :: nl = achar(10)

      ierr = 0
      poly = g%nf > 0
      if (poly .and. legacy) call legacy_arrays(g, lfaces, lfoff)
      open (newunit=lu, file=fname, status='replace', access='stream', &
            form='unformatted', action='write', iostat=ios)
      if (ios /= 0) then
         ierr = 1
         errmsg = 'write_vtu: cannot open '//trim(fname)
         return
      end if
      pos = 0
      call put('<?xml version="1.0"?>'//nl)
      if (legacy) then
         call put('<VTKFile type="UnstructuredGrid" version="2.2" '// &
                  'byte_order="LittleEndian" header_type="UInt64">'//nl)
      else
         call put('<VTKFile type="UnstructuredGrid" version="2.3" '// &
                  'byte_order="LittleEndian" header_type="UInt64">'//nl)
      end if
      call put('<UnstructuredGrid>'//nl)
      call put('<Piece NumberOfPoints="'//itoa(g%np)// &
               '" NumberOfCells="'//itoa(g%nc)//'">'//nl)
      ! the XML part: every array, in the order its data follows
      call put('<PointData>'//nl)
      do k = 1, g%npd
         call head_data(g%pdata(k), g%np)
      end do
      call put('</PointData>'//nl)
      call put('<CellData>'//nl)
      do k = 1, g%ncd
         call head_data(g%cdata(k), g%nc)
      end do
      call put('</CellData>'//nl)
      call put('<Points>'//nl)
      call head('Float64', 'Points', 3, 3*g%np, 8)
      if (.not. binary) call put_r(reshape(g%xyz(:, 1:g%np), &
                                           [3*g%np]), 3)
      call tail()
      call put('</Points>'//nl)
      call put('<Cells>'//nl)
      call head('Int64', 'connectivity', 1, size(g%conn, kind=ik), 8)
      if (.not. binary) call put_i(g%conn - 1)
      call tail()
      call head('Int64', 'offsets', 1, g%nc, 8)
      if (.not. binary) call put_i(g%off(1:g%nc))
      call tail()
      call head('UInt8', 'types', 1, g%nc, 1)
      if (.not. binary) call put_i(int(g%types(1:g%nc), ik))
      call tail()
      if (poly .and. legacy) then
         call head('Int64', 'faces', 1, size(lfaces, kind=ik), 8)
         if (.not. binary) call put_i(lfaces)
         call tail()
         call head('Int64', 'faceoffsets', 1, g%nc, 8)
         if (.not. binary) call put_i(lfoff)
         call tail()
      else if (poly) then
         call head('Int64', 'face_connectivity', 1, &
                   size(g%fconn, kind=ik), 8)
         if (.not. binary) call put_i(g%fconn - 1)
         call tail()
         call head('Int64', 'face_offsets', 1, g%nf, 8)
         if (.not. binary) call put_i(g%foff(1:g%nf))
         call tail()
         call head('Int64', 'polyhedron_to_faces', 1, &
                   size(g%pfaces, kind=ik), 8)
         if (.not. binary) call put_i(g%pfaces - 1)
         call tail()
         call head('Int64', 'polyhedron_offsets', 1, g%nc, 8)
         if (.not. binary) call put_i(g%poff(1:g%nc))
         call tail()
      end if
      call put('</Cells>'//nl)
      call put('</Piece>'//nl)
      call put('</UnstructuredGrid>'//nl)
      if (binary) then
         call put('<AppendedData encoding="raw">'//nl//'_')
         do k = 1, g%npd
            call raw_data(g%pdata(k))
         end do
         do k = 1, g%ncd
            call raw_data(g%cdata(k))
         end do
         call raw_r(reshape(g%xyz(:, 1:g%np), [3*g%np]))
         call raw_i(g%conn - 1)
         call raw_i(g%off(1:g%nc))
         write (lu) int(8*0 + g%nc, int64)
         write (lu) g%types(1:g%nc)
         if (poly .and. legacy) then
            call raw_i(lfaces)
            call raw_i(lfoff)
         else if (poly) then
            call raw_i(g%fconn - 1)
            call raw_i(g%foff(1:g%nf))
            call raw_i(g%pfaces - 1)
            call raw_i(g%poff(1:g%nc))
         end if
         call put(nl//'</AppendedData>'//nl)
      end if
      call put('</VTKFile>'//nl)
      close (lu)

   contains

      subroutine put(s)
         character(len=*), intent(in) :: s
         write (lu) s
      end subroutine put

      ! A DataArray's opening line; with binary, its offset into the
      ! appended data (pos counts the bytes of the arrays before it).
      subroutine head(typ, name, ncomp, n, nbytes)
         character(len=*), intent(in) :: typ, name
         integer, intent(in) :: ncomp, nbytes
         integer(ik), intent(in) :: n
         character(len=200) :: s
         if (binary) then
            write (s, '(5a,i0,a,i0,a)') '<DataArray type="', typ, &
               '" Name="', name, '" NumberOfComponents="', ncomp, &
               '" format="appended" offset="', pos, '"/>'
            call put(trim(s)//nl)
            pos = pos + 8 + int(nbytes, int64)*n
         else
            write (s, '(5a,i0,a)') '<DataArray type="', typ, &
               '" Name="', name, '" NumberOfComponents="', ncomp, &
               '" format="ascii">'
            call put(trim(s)//nl)
         end if
      end subroutine head

      subroutine tail()
         if (.not. binary) call put('</DataArray>'//nl)
      end subroutine tail

      subroutine head_data(d, n)
         type(vtu_data_t), intent(in) :: d
         integer(ik), intent(in) :: n
         if (d%is_real) then
            call head('Float64', trim(d%name), d%ncomp, d%ncomp*n, 8)
            if (.not. binary) call put_r(d%r, d%ncomp)
         else
            call head('Int64', trim(d%name), d%ncomp, d%ncomp*n, 8)
            if (.not. binary) call put_i(d%i)
         end if
         call tail()
      end subroutine head_data

      subroutine raw_data(d)
         type(vtu_data_t), intent(in) :: d
         if (d%is_real) then
            call raw_r(d%r)
         else
            call raw_i(d%i)
         end if
      end subroutine raw_data

      subroutine raw_r(a)
         real(rk), intent(in) :: a(:)
         write (lu) int(8, int64)*size(a, kind=int64)
         write (lu) real(a, real64)
      end subroutine raw_r

      subroutine raw_i(a)
         integer(ik), intent(in) :: a(:)
         write (lu) int(8, int64)*size(a, kind=int64)
         write (lu) int(a, int64)
      end subroutine raw_i

      ! ASCII: reals with 17 digits, ncomp per line (3 for points)
      subroutine put_r(a, ncomp)
         real(rk), intent(in) :: a(:)
         integer, intent(in) :: ncomp
         character(len=30*4) :: s
         integer(ik) :: j, n, per
         per = max(ncomp, 4)
         if (ncomp == 3) per = 3
         n = size(a, kind=ik)
         do j = 1, n, per
            write (s, '(*(1x,es24.16e3))') a(j:min(n, j + per - 1))
            call put(trim(s)//nl)
         end do
      end subroutine put_r

      subroutine put_i(a)
         integer(ik), intent(in) :: a(:)
         character(len=21*10) :: s
         integer(ik) :: j, n
         n = size(a, kind=ik)
         do j = 1, n, 10
            write (s, '(*(1x,i0))') a(j:min(n, j + 9))
            call put(trim(s)//nl)
         end do
      end subroutine put_i

   end subroutine write_vtu

   ! The legacy faces stream and its offsets, from the new arrays.
   subroutine legacy_arrays(g, lfaces, lfoff)
      type(vtu_grid_t), intent(in) :: g
      integer(ik), allocatable, intent(out) :: lfaces(:), lfoff(:)
      integer(ik) :: c, n, f, j, a, b, fa, fb, p0
      n = 0
      do c = 1, g%nc
         p0 = 0
         if (c > 1) p0 = g%poff(c - 1)
         if (g%poff(c) == p0) cycle
         n = n + 1
         do j = p0 + 1, g%poff(c)
            f = g%pfaces(j)
            fa = 0
            if (f > 1) fa = g%foff(f - 1)
            n = n + 1 + (g%foff(f) - fa)
         end do
      end do
      allocate (lfaces(n), lfoff(g%nc))
      n = 0
      do c = 1, g%nc
         p0 = 0
         if (c > 1) p0 = g%poff(c - 1)
         if (g%poff(c) == p0) then
            lfoff(c) = -1
            cycle
         end if
         n = n + 1
         lfaces(n) = g%poff(c) - p0
         do j = p0 + 1, g%poff(c)
            f = g%pfaces(j)
            fa = 0
            if (f > 1) fa = g%foff(f - 1)
            fb = g%foff(f)
            n = n + 1
            lfaces(n) = fb - fa
            do a = fa + 1, fb
               n = n + 1
               lfaces(n) = g%fconn(a) - 1
            end do
         end do
         lfoff(c) = n
         b = 0
      end do
   end subroutine legacy_arrays

   ! ROOT.pvd: the files, by name only (they sit beside it).
   subroutine write_pvd(fname, files, n, ierr)
      character(len=*), intent(in) :: fname
      character(len=*), intent(in) :: files(:)
      integer, intent(in) :: n
      integer, intent(out) :: ierr
      integer :: lu, ios, k
      ierr = 0
      open (newunit=lu, file=fname, status='replace', action='write', &
            iostat=ios)
      if (ios /= 0) then
         ierr = 1
         errmsg = 'write_pvd: cannot open '//trim(fname)
         return
      end if
      write (lu, '(a)') '<?xml version="1.0"?>'
      write (lu, '(a)') '<VTKFile type="Collection" version="0.1" '// &
         'byte_order="LittleEndian">'
      write (lu, '(a)') '<Collection>'
      do k = 1, n
         write (lu, '(a,i0,3a)') '<DataSet part="', k - 1, '" file="', &
            trim(basename(files(k))), '"/>'
      end do
      write (lu, '(a)') '</Collection>'
      write (lu, '(a)') '</VTKFile>'
      close (lu)
   end subroutine write_pvd

   function basename(p) result(b)
      character(len=*), intent(in) :: p
      character(len=len(p)) :: b
      integer :: k
      k = index(p, '/', back=.true.)
      b = p(k + 1:)
   end function basename

   function itoa(n) result(s)
      integer(ik), intent(in) :: n
      character(len=:), allocatable :: s
      character(len=24) :: t
      write (t, '(i0)') n
      s = trim(t)
   end function itoa

end module del_vor_vtu
