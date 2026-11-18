c-----------------------------------------------------------------------
c call_entry: calls cv_dual_lagrit_entry the way LaGriT will, from
c fixed-form Fortran built with LaGriT's gfortran flags
c (-fdefault-integer-8 -std=legacy ...; CMakeLists.txt) and with no
c interface block: plain integers (8 bytes) and real*8 arrays.
c-----------------------------------------------------------------------
      subroutine call_entry(nnodes, nelements, nen, xic, yic, zic,
     *     itettyp, itetoff, itet, itetclr, imt1, itp1, isn1,
     *     options, command, vol, ierr)
      implicit none
      integer nnodes, nelements, nen, ierr
      real*8 xic(nnodes), yic(nnodes), zic(nnodes), vol(nnodes)
      integer itettyp(nelements), itetoff(nelements)
      integer itet(nen*nelements), itetclr(nelements)
      integer imt1(nnodes), itp1(nnodes), isn1(nnodes)
      character*(*) options, command
      integer want_area
      real*8 barea(3)
c     the boundary areas are not asked for here (want_area 0: barea
c     is not touched); dump / fehm and the suite
c     test_cv_dual_dump_keyword check them
      want_area = 0
      call cv_dual_lagrit_entry(nnodes, nelements, nen, xic, yic, zic,
     *     itettyp, itetoff, itet, itetclr, imt1, itp1, isn1,
     *     options, command, vol, want_area, barea, ierr)
      return
      end
