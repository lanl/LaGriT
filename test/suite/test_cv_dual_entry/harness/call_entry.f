c-----------------------------------------------------------------------
c call_entry: calls del_vor_lagrit_entry the way LaGriT will, from
c fixed-form Fortran built with LaGriT's gfortran flags
c (-fdefault-integer-8 -std=legacy ...; CMakeLists.txt) and with no
c interface block: plain integers (8 bytes) and real*8 arrays.
c-----------------------------------------------------------------------
      subroutine call_entry(nnodes, nelements, nen, xic, yic, zic,
     *     itettyp, itetoff, itet, itetclr, imt1, itp1, isn1,
     *     options, vol, ierr)
      implicit none
      integer nnodes, nelements, nen, ierr
      real*8 xic(nnodes), yic(nnodes), zic(nnodes), vol(nnodes)
      integer itettyp(nelements), itetoff(nelements)
      integer itet(nen*nelements), itetclr(nelements)
      integer imt1(nnodes), itp1(nnodes), isn1(nnodes)
      character*(*) options
      call del_vor_lagrit_entry(nnodes, nelements, nen, xic, yic, zic,
     *     itettyp, itetoff, itet, itetclr, imt1, itp1, isn1,
     *     options, vol, ierr)
      return
      end
