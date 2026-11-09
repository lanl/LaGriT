!-----------------------------------------------------------------------
! del_vor_validity: the checks every run makes before any geometry.
! A failure is fatal in every mode: the program reports and stops.
!
!   positive size   tets: det(x2-x1, x3-x1, x4-x1) > 0 in LaGriT's
!                   vertex order; triangles: area > 0
!   conformity      a tet face in at most 2 tets; a triangle edge in
!                   at most 2 triangles of one itetclr (del_vor_jtet)
!   orientation     neighboring triangles of one itetclr run along
!                   their shared edge in opposite directions
!                   (del_vor_jtet); each fracture may be either way
!
! Not fatal: vertices in no cell (for example LaGriT's dudded
! vertices) are counted and get no control volume.
!-----------------------------------------------------------------------
module del_vor_validity
   use del_vor_kinds, only: ik, rk
   use del_vor_geometry, only: tet_det, tri_area
   use del_vor_jtet, only: jtet_stats_t
   use del_vor_log, only: log_line
   implicit none
   private
   public :: check_validity, validity_t

   type :: validity_t
      integer(ik) :: nbad_size = 0     ! cells of zero or negative size
      integer(ik) :: bad_size_cell = 0 ! first one
      integer(ik) :: nunused = 0       ! vertices in no cell
      integer(ik) :: unused_vertex = 0 ! first one
      integer :: nfatal = 0            ! kinds of fatal problem found
   end type validity_t

contains

   subroutine check_validity(nnodes, nelements, nen, xic, yic, zic, &
                             itetoff, itet, itetclr, js, v)
      integer(ik), intent(in) :: nnodes, nelements, nen
      real(rk), intent(in) :: xic(nnodes), yic(nnodes), zic(nnodes)
      integer(ik), intent(in) :: itetoff(nelements), itet(*)
      integer(ik), intent(in) :: itetclr(nelements)
      type(jtet_stats_t), intent(in) :: js
      type(validity_t), intent(out) :: v

      real(rk) :: x(3, 4), x0(3), s
      integer(ik) :: it, iv(4), i
      logical, allocatable :: used(:)
      character(len=80) :: msg
      character(len=5) :: fw
      integer :: j

      allocate (used(nnodes))
      used = .false.
      do it = 1, nelements
         do j = 1, int(nen)
            iv(j) = itet(itetoff(it) + j)
            used(iv(j)) = .true.
         end do
         x0 = [xic(iv(1)), yic(iv(1)), zic(iv(1))]
         do j = 1, int(nen)
            x(:, j) = [xic(iv(j)), yic(iv(j)), zic(iv(j))] - x0
         end do
         if (nen == 4) then
            s = tet_det(x)
         else
            s = tri_area(x(:, 1:3))
         end if
         if (.not. (s > 0.0_rk)) then
            v%nbad_size = v%nbad_size + 1
            if (v%bad_size_cell == 0) v%bad_size_cell = it
         end if
      end do
      do i = 1, nnodes
         if (.not. used(i)) then
            v%nunused = v%nunused + 1
            if (v%unused_vertex == 0) v%unused_vertex = i
         end if
      end do
      deallocate (used)

      call log_line(' ')
      call log_line('del_vor mesh checks')
      if (v%nbad_size > 0) then
         v%nfatal = v%nfatal + 1
         if (nen == 4) then
            write (msg, '(a,i12,a)') '  ERROR:', v%nbad_size, &
               ' tets with zero or negative volume'
         else
            write (msg, '(a,i12,a)') '  ERROR:', v%nbad_size, &
               ' triangles with zero area'
         end if
         call log_line(msg)
         write (msg, '(a,i12,a,i8)') '         first: cell', &
            v%bad_size_cell, '  itetclr', itetclr(v%bad_size_cell)
         call log_line(msg)
      end if
      if (js%nnonconf > 0) then
         v%nfatal = v%nfatal + 1
         if (nen == 4) then
            write (msg, '(a,i12,a)') '  ERROR:', js%nnonconf, &
               ' tet faces shared by 3 or more tets'
         else
            write (msg, '(a,i12,a)') '  ERROR:', js%nnonconf, &
               ' edges in 3 or more triangles of one itetclr'
         end if
         call log_line(msg)
         write (msg, '(a,i12,a,i2)') '         first: cell', &
            js%nonconf_cell, '  local face', js%nonconf_face
         call log_line(msg)
      end if
      if (js%norient > 0) then
         v%nfatal = v%nfatal + 1
         write (msg, '(a,i12,a)') '  ERROR:', js%norient, &
            ' pairs of neighboring triangles oriented differently'
         call log_line(msg)
         write (msg, '(a,i12,a,i12)') '         first: cells', &
            js%orient_cell(1), ' and', js%orient_cell(2)
         call log_line(msg)
         call log_line('         (each itetclr must be oriented '// &
                       'one way; a Moebius strip cannot be)')
      end if
      if (v%nunused > 0) then
         write (msg, '(a,i12,a)') '  WARNING:', v%nunused, &
            ' vertices in no cell (no control volume)'
         call log_line(msg)
         write (msg, '(a,i12)') '         first: vertex', &
            v%unused_vertex
         call log_line(msg)
      end if
      fw = 'faces'
      if (nen == 3) fw = 'edges'
      write (msg, '(a,t33,i12)') '  '//fw//' paired, same itetclr', &
         js%ninterior
      call log_line(msg)
      write (msg, '(a,t33,i12)') '  '//fw//' paired, other itetclr', &
         js%ninterface
      call log_line(msg)
      write (msg, '(a,t33,i12)') '  boundary '//fw, &
         js%nboundary
      call log_line(msg)
      if (nen == 3) then
         write (msg, '(a,t33,i12)') '  fracture intersection edges', &
            js%nintersect
         call log_line(msg)
      end if
      if (v%nfatal == 0) then
         call log_line('  mesh checks passed')
      else
         call log_line('  mesh checks FAILED: nothing is computed')
      end if
   end subroutine check_validity

end module del_vor_validity
