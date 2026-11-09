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
! The core routines take these arrays as arguments; this module only
! holds the copy the standalone program reads from an AVS file.
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
      real(rk), allocatable :: xic(:), yic(:), zic(:)
      integer(ik), allocatable :: itet(:), itetoff(:)
      integer(ik), allocatable :: itettyp(:), itetclr(:)
      integer(ik), allocatable :: imt1(:), itp1(:), isn1(:)
      integer(ik), allocatable :: jtet(:), jtetoff(:)
      integer(ik) :: mbndry = 0
   end type mesh_t

contains

   subroutine mesh_free(m)
      type(mesh_t), intent(inout) :: m
      if (allocated(m%xic)) deallocate (m%xic, m%yic, m%zic)
      if (allocated(m%itet)) deallocate (m%itet, m%itetoff)
      if (allocated(m%itettyp)) deallocate (m%itettyp, m%itetclr)
      if (allocated(m%imt1)) deallocate (m%imt1)
      if (allocated(m%itp1)) deallocate (m%itp1)
      if (allocated(m%isn1)) deallocate (m%isn1)
      if (allocated(m%jtet)) deallocate (m%jtet)
      if (allocated(m%jtetoff)) deallocate (m%jtetoff)
      m%mbndry = 0
      m%nnodes = 0
      m%nelements = 0
      m%nen = 0
   end subroutine mesh_free

end module del_vor_mesh
