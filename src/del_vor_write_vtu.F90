!-----------------------------------------------------------------------
! del_vor_write_vtu: violations.vtu, the facets the Delaunay check
! flagged, as a VTK XML unstructured grid (ASCII, written by hand, no
! VTK library). ParaView opens it directly; open the mesh .inp with it
! (ParaView reads AVS UCD) to see where the facets are.
!
! Cells: one per flagged facet, a triangle (tets) or a line segment
! (triangles), with its own copies of the facet's vertices.
!
! Cell data:
!   kind           1 Delaunay violation, 2 violation across a material
!                  interface, 3 boundary facet with the cell's
!                  circumcenter outside
!   delta_over_L   kinds 1, 2: the signed Voronoi edge delta/L (< 0);
!                  kind 3: how far the circumcenter lies outside, /L
!   cell1, cell2   the cells on either side (cell2 = 0 for kind 3)
!   itetclr1, itetclr2
!   ratio          the larger circumradius / shortest edge of the
!                  cells (large = sliver; delta may be round-off)
! Point data:
!   vertex         the mesh vertex number
!-----------------------------------------------------------------------
module del_vor_write_vtu
   use del_vor_kinds, only: ik, rk
   use del_vor_delaunay, only: facet_rec_t
   use del_vor_log, only: errmsg
   implicit none
   private
   public :: write_violations_vtu

contains

   subroutine write_violations_vtu(fname, nen, nnodes, xic, yic, zic, &
                                   r, ierr)
      character(len=*), intent(in) :: fname
      integer(ik), intent(in) :: nen, nnodes
      real(rk), intent(in) :: xic(nnodes), yic(nnodes), zic(nnodes)
      type(facet_rec_t), intent(in) :: r
      integer, intent(out) :: ierr
      integer :: lu, ios, k, q, nv
      integer(ik) :: iv

      ierr = 0
      nv = int(nen) - 1
      open (newunit=lu, file=fname, status='replace', action='write', &
            iostat=ios)
      if (ios /= 0) then
         ierr = 1
         errmsg = 'write_vtu: cannot open '//trim(fname)
         return
      end if
      write (lu, '(a)') '<?xml version="1.0"?>'
      write (lu, '(a)') '<VTKFile type="UnstructuredGrid" '// &
         'version="0.1" byte_order="LittleEndian">'
      write (lu, '(a)') '<UnstructuredGrid>'
      write (lu, '(a,i0,a,i0,a)') '<Piece NumberOfPoints="', nv*r%n, &
         '" NumberOfCells="', r%n, '">'

      write (lu, '(a)') '<Points>'
      write (lu, '(a)') '<DataArray type="Float64" '// &
         'NumberOfComponents="3" format="ascii">'
      do k = 1, r%n
         do q = 1, nv
            iv = r%vert(q, k)
            write (lu, '(3(1x,es24.16e3))') xic(iv), yic(iv), zic(iv)
         end do
      end do
      write (lu, '(a)') '</DataArray>'
      write (lu, '(a)') '</Points>'

      write (lu, '(a)') '<Cells>'
      write (lu, '(a)') '<DataArray type="Int64" '// &
         'Name="connectivity" format="ascii">'
      do k = 1, r%n
         write (lu, '(3(1x,i0))') (nv*(k - 1) + q - 1, q=1, nv)
      end do
      write (lu, '(a)') '</DataArray>'
      write (lu, '(a)') '<DataArray type="Int64" Name="offsets" '// &
         'format="ascii">'
      write (lu, '(10(1x,i0))') (nv*k, k=1, r%n)
      write (lu, '(a)') '</DataArray>'
      write (lu, '(a)') '<DataArray type="UInt8" Name="types" '// &
         'format="ascii">'
      if (nv == 3) then
         write (lu, '(20(1x,i0))') (5, k=1, r%n)
      else
         write (lu, '(20(1x,i0))') (3, k=1, r%n)
      end if
      write (lu, '(a)') '</DataArray>'
      write (lu, '(a)') '</Cells>'

      write (lu, '(a)') '<PointData Scalars="vertex">'
      write (lu, '(a)') '<DataArray type="Int64" Name="vertex" '// &
         'format="ascii">'
      write (lu, '(10(1x,i0))') ((r%vert(q, k), q=1, nv), k=1, r%n)
      write (lu, '(a)') '</DataArray>'
      write (lu, '(a)') '</PointData>'

      write (lu, '(a)') '<CellData Scalars="delta_over_L">'
      call int_array('kind', [(int(r%kind(k), ik), k=1, r%n)])
      write (lu, '(a)') '<DataArray type="Float64" '// &
         'Name="delta_over_L" format="ascii">'
      write (lu, '(4(1x,es24.16e3))') r%dl(1:r%n)
      write (lu, '(a)') '</DataArray>'
      call int_array('cell1', r%cell(1, 1:r%n))
      call int_array('cell2', r%cell(2, 1:r%n))
      call int_array('itetclr1', r%clr(1, 1:r%n))
      call int_array('itetclr2', r%clr(2, 1:r%n))
      write (lu, '(a)') '<DataArray type="Float64" Name="ratio" '// &
         'format="ascii">'
      write (lu, '(4(1x,es24.16e3))') r%ratio(1:r%n)
      write (lu, '(a)') '</DataArray>'
      write (lu, '(a)') '</CellData>'

      write (lu, '(a)') '</Piece>'
      write (lu, '(a)') '</UnstructuredGrid>'
      write (lu, '(a)') '</VTKFile>'
      close (lu)

   contains

      subroutine int_array(name, a)
         character(len=*), intent(in) :: name
         integer(ik), intent(in) :: a(:)
         write (lu, '(a)') '<DataArray type="Int64" Name="'//name// &
            '" format="ascii">'
         write (lu, '(10(1x,i0))') a
         write (lu, '(a)') '</DataArray>'
      end subroutine int_array

   end subroutine write_violations_vtu

end module del_vor_write_vtu
