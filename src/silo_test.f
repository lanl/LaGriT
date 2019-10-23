      subroutine silo_test()

      implicit none
      include "silo.inc"
      integer(kind=4) builducd
      integer(kind=4)  dbid, meshid, err, driver, nargs
      character*256 cloption

      driver = DB_HDF5

      err = dbcreate("ucdf77.silo", 11, 0, DB_LOCAL,
     .               "file info", 9, driver, dbid)

      meshid = builducd(dbid, "ucd", 3) 
      err = dbclose(dbid)

      end subroutine silo_test

ccccccccc

      integer(kind=4) function builducd (dbid, name, lname)
c----------------------------------------------------------------------
c  Routine                            builducd
c
c  Purpose
c
c   Build ucd-mesh, ucd-var, facelist and zonelist, and return
c   the mesh ID.
c
c  Modifications:
c    Kathleen Bonnell, Wed Sep 2 16:12:15 PDT 20099
c    Changed 'character*8 name' to 'character*(*) name' to remove 
c    'Character length argument mismatch' compiler error.
c
c-----------------------------------------------------------------------

      integer(kind=4)  dbid             ! Database identifier
      character*(*) name        ! Name of mesh to build
      integer(kind=4)     lname         ! Length of name

      include "silo.inc"       ! Don't forget to include this file

      integer(kind=4) :: NZONES = 7   ! Number of zones
      integer(kind=4) :: NNODES = 13  ! Number of nodes
      integer(kind=4) :: NZSHAPES = 1  ! Number of zone shapes
      integer(kind=4) :: LZNODELIST = 27 ! Length of zone nodelist
      integer(kind=4)      i, zlid, meshid, optlistid

      real*4, allocatable ::        x(:), y(:)
      integer(kind=4), allocatable ::  znodelist(:) !znodelist(LZNODELIST)
      integer(kind=4)      zshapesize(1), zshapecnt(1)

      integer lg_error, ilen, itype, n_nodes, nelems
      character(len=90) :: cmo

      pointer (ixic, xic)
      pointer (iyic, yic)
      pointer (izic, zic)
      pointer (ipitet, elem_connectivity)
      pointer (ipitettyp, elem_type)

      real*8 xic(*), yic(*), zic(*)
      integer*8 elem_connectivity(*), elem_type(*)

      call cmo_get_name(cmo,lg_error)
      call cmo_get_info('nnodes',cmo,n_nodes,ilen,itype,ierror)
      call cmo_get_info('nelements',cmo,nelems,ilen,itype,ierror)
      call cmo_get_info('xic',cmo,ixic,ilen,itype,lg_error)
      call cmo_get_info('yic',cmo,iyic,ilen,itype,lg_error)
      call cmo_get_info('zic',cmo,izic,ilen,itype,lg_error)
      call cmo_get_info('itet', cmo, ipitet, ilen, itype, ierror)
      call cmo_get_info('itettyp',cmo,ipitettyp,ilen,itype,ierror)

      NNODES = long(n_nodes)
      NZONES = nelems
      LZNODELIST = long(nelems * elem_type(1))

      print*,'LZNODELIST = ',LZNODELIST
      print*,'nelems = ',nelems,'; et = ',elem_type(1)

      allocate(x(NNODES))
      allocate(y(NNODES))
      allocate(znodelist(LZNODELIST))

      x(1:NNODES) = real(xic(1:n_nodes))
      y(1:NNODES) = real(yic(1:n_nodes))

      print*,'len = ',size(x)

      znodelist(1:LZNODELIST) = long(elem_connectivity(1:LZNODELIST))

      print*,'elem_connectivity = ',elem_connectivity(1:LZNODELIST)
      print*,x
      

      do i = 1, LZNODELIST
        znodelist(i) = znodelist(i) - 1
      enddo

      zshapesize(1) = long(elem_connectivity(1))
      zshapecnt(1) = long(nelems)

      ierr = dbmkoptlist(3, optlistid)

      err = dbputzl(dbid, 'Zonelist', 8, NZONES, 2, znodelist,
     .              LZNODELIST, 0,
     .              zshapesize, zshapecnt, NZSHAPES, zlid)

      err = dbputum(dbid, name, lname, 2, x, y, DB_F77NULL,
     .              "X", 1, "Y", 1, DB_F77NULLSTRING, 0, DB_FLOAT,
     .              NNODES, NZONES, 'Zonelist', 8, DB_F77NULLSTRING, 0,
     .              optlistid, meshid)


      deallocate(x)
      deallocate(y)
      deallocate(znodelist)

      builducd = meshid
      end
