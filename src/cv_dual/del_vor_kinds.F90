!-----------------------------------------------------------------------
! del_vor_kinds: integer and real kinds used everywhere in del_vor.
!
! ik  vertex, cell and edge numbers, CSR offsets. 64-bit, to match
!     LaGriT builds with 8-byte integers and to allow meshes of
!     hundreds of millions of cells.
! rk  coordinates, areas, volumes.
!-----------------------------------------------------------------------
module del_vor_kinds
   use, intrinsic :: iso_fortran_env, only: int64, real64
   implicit none
   private
   public :: ik, rk

   integer, parameter :: ik = int64
   integer, parameter :: rk = real64

end module del_vor_kinds
