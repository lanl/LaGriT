!-----------------------------------------------------------------------
! del_vor_mesh: the mesh arrays, named and laid out as in a LaGriT
! mesh object, so the LaGriT entry point can pass its own arrays.
!
!   nnodes, nelements   numbers of vertices and cells
!   nen                 vertices per cell: 3 (triangles) or 4 (tets)
!   xic, yic, zic       vertex coordinates
!   itet(itetoff(it)+j) vertex j of cell it, 1-based
!   itettyp             LaGriT cell type: 3 = triangle, 5 = tet
!   itetclr             cell material or fracture number
!   imt1, itp1, isn1    vertex material, type, parent/child link
!   jtetoff, jtet       neighbors: jtet(jtetoff(it)+j) for local face
!                       j (opposite vertex j) of cell it is
!                       nen*(jt-1)+jf when face jf of cell jt is the
!                       same face; mbndry on the exterior boundary;
!                       mbndry + nen*(jt-1)+jf across a material
!                       interface (LaGriT's convention; nef = nen
!                       for triangles and tets)
!   mbndry              boundary flag, larger than any face code
!
! The arrays are pointers: the standalone program allocates them
! (read_avs; own_input true), the LaGriT entry point points them at
! LaGriT's own arrays (own_input false), which are used in place and
! never freed here. jtetoff and jtet are always del_vor's own.
!-----------------------------------------------------------------------
module del_vor_mesh
   use del_vor_kinds, only: ik, rk
   implicit none
   private
   public :: mesh_t, mesh_free, ifelmtri, ifelmtet

   ! LaGriT cell type numbers (ifelmtri, ifelmtet in local_element.h)
   integer(ik), parameter :: ifelmtri = 3
   integer(ik), parameter :: ifelmtet = 5

   type :: mesh_t
      integer(ik) :: nnodes = 0
      integer(ik) :: nelements = 0
      integer(ik) :: nen = 0
      real(rk), pointer, contiguous :: xic(:) => null(), &
                                        yic(:) => null(), &
                                        zic(:) => null()
      integer(ik), pointer, contiguous :: itet(:) => null(), &
                                           itetoff(:) => null()
      integer(ik), pointer, contiguous :: itettyp(:) => null(), &
                                           itetclr(:) => null()
      integer(ik), pointer, contiguous :: imt1(:) => null(), &
                                           itp1(:) => null(), &
                                           isn1(:) => null()
      integer(ik), pointer, contiguous :: jtet(:) => null(), &
                                           jtetoff(:) => null()
      integer(ik) :: mbndry = 0
      logical :: own_input = .true.
   end type mesh_t

contains

   subroutine mesh_free(m)
      type(mesh_t), intent(inout) :: m
      if (m%own_input) then
         if (associated(m%xic)) deallocate (m%xic)
         if (associated(m%yic)) deallocate (m%yic)
         if (associated(m%zic)) deallocate (m%zic)
         if (associated(m%itet)) deallocate (m%itet)
         if (associated(m%itetoff)) deallocate (m%itetoff)
         if (associated(m%itettyp)) deallocate (m%itettyp)
         if (associated(m%itetclr)) deallocate (m%itetclr)
         if (associated(m%imt1)) deallocate (m%imt1)
         if (associated(m%itp1)) deallocate (m%itp1)
         if (associated(m%isn1)) deallocate (m%isn1)
      end if
      nullify (m%xic, m%yic, m%zic, m%itet, m%itetoff, m%itettyp, &
               m%itetclr, m%imt1, m%itp1, m%isn1)
      if (associated(m%jtet)) deallocate (m%jtet)
      if (associated(m%jtetoff)) deallocate (m%jtetoff)
      m%own_input = .true.
      m%mbndry = 0
      m%nnodes = 0
      m%nelements = 0
      m%nen = 0
   end subroutine mesh_free

end module del_vor_mesh
