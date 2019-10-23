      subroutine silo_test()

      implicit none
      include "silo.inc"
      integer(kind=4) builducd
      integer(kind=4)  dbid, meshid, err, driver, nargs
      character*256 cloption

      driver = DB_HDF5

      ! dump / silo / filename / meshobject / [with_voronoi] , [with_cell_atts] , [with_node_atts]

      err = dbcreate("ucdf77.silo", 11, 0, DB_LOCAL,
     .               "file info", 9, driver, dbid)

      meshid = builducd(dbid, "ucd", 3) 
      err = dbclose(dbid)

      end subroutine silo_test

ccccccccc

      integer(kind=4) function builducd (dbid, name, lname)

      implicit none

      integer(kind=4)  dbid          ! Database identifier
      character*(*)    name          ! Name of mesh to build
      integer(kind=4)  lname         ! Length of name

      include "silo.inc"             ! Don't forget to include this file

      character(len=90) :: cmo
      character*32 ctype

      logical :: DEBUG = .false.
      real*8, allocatable :: x(:), y(:), z(:)

      integer(kind=4) :: NZONES = 7   ! Number of zones
      integer(kind=4) :: NNODES = 13  ! Number of nodes
      integer(kind=4) :: NZSHAPES = 1  ! Number of zone shapes
      integer(kind=4) :: LZNODELIST = 27 ! Length of zone nodelist

      integer(kind=4) i, zlid, meshid, optlistid, NDIMS
      integer(kind=4), allocatable :: znodelist(:)
      integer(kind=4)  zshapesize(1), zshapecnt(1)
      integer(kind=4)  shape_type, ierr

      integer lg_error, ilen, itype, n_nodes, nelems, n_dims
      integer nsdtopo, nsdgeom

      pointer (ixic, xic)
      pointer (iyic, yic)
      pointer (izic, zic)
      pointer (ipitet, elem_connectivity)
      pointer (ipitettyp, elem_type)

      real*8 xic(*), yic(*), zic(*)
      integer*8 elem_connectivity(*), elem_type(*)

      ! ======================================== !

      ! Parse LaGriT libraries for mesh information
      call cmo_get_name(cmo,lg_error)
      call cmo_get_info('nnodes',cmo,n_nodes,ilen,itype,lg_error)
      call cmo_get_info('nelements',cmo,nelems,ilen,itype,lg_error)
      call cmo_get_info('xic',cmo,ixic,ilen,itype,lg_error)
      call cmo_get_info('yic',cmo,iyic,ilen,itype,lg_error)
      call cmo_get_info('zic',cmo,izic,ilen,itype,lg_error)
      call cmo_get_info('itet', cmo, ipitet, ilen, itype, lg_error)
      call cmo_get_info('itettyp',cmo,ipitettyp,ilen,itype,lg_error)
      call cmo_get_info('ndimensions_topo',cmo,nsdtopo,ilen,
     . itype,lg_error)
      call cmo_get_info('ndimensions_geom',cmo,nsdgeom,ilen,
     . itype,lg_error)
      call cmo_get_mesh_type(cmo,ctype,itype,lg_error)

      ! Cast int*8 and real*8 to int*4 and real*4
      NDIMS = int(nsdtopo,4)
      NNODES = int(n_nodes,4)
      NZONES = int(nelems,4)
      LZNODELIST = int(nelems * elem_type(1),4)

      allocate(x(NNODES))
      allocate(y(NNODES))
      allocate(z(NNODES))
      allocate(znodelist(LZNODELIST))

      x(1:NNODES) = (xic(1:n_nodes))
      y(1:NNODES) = (yic(1:n_nodes))
      z(1:NNODES) = (zic(1:n_nodes))

      znodelist(1:LZNODELIST) = int(elem_connectivity(1:LZNODELIST),4)
      znodelist = znodelist - 1  !  shift to 0-index

      !zshapesize(1) = int(elem_type(1),4)
      zshapecnt(1) = int(nelems,4)

      ! Set the SILO element type by parsing 'ctype'
      select case (trim(ctype))
      case ('quad')
        shape_type = DB_ZONETYPE_QUAD
        zshapesize(1) = 4
      case ('tet')
        shape_type = DB_ZONETYPE_TET
        zshapesize(1) = 4
      case ('triplane')
        shape_type = DB_ZONETYPE_TRIANGLE
        zshapesize(1) = 3
      case ('triangle')
        shape_type = DB_ZONETYPE_TRIANGLE
        zshapesize(1) = 3
      case ('hex')
        shape_type = DB_ZONETYPE_HEX
        zshapesize(1) = 6
      case ('line')
        shape_type = DB_ZONETYPE_BEAM
        zshapesize(1) = 2
      case default
         ! TODO: handle with LG error handler
         print*, "ERROR silo case select for this type not supported"
      end select

      if (DEBUG) then
        print*,'NNODES = ',NNODES
        print*,'NZONES = ',NZONES
        print*,'NZSHAPES = ',NZSHAPES
        print*,'NDIMS = ',NDIMS
        print*,'LZNODELIST = ',LZNODELIST
        print*,'zshapesize = ',zshapesize, ' elem_type = ', elem_type(1)
        print*,'zshapecnt = ',zshapecnt
        print*,'ctype = ',trim(ctype)
      endif

      ! Write data to SILO file
      ierr = dbmkoptlist(3, optlistid)

      ierr = dbputzl2(dbid, 'Zonelist', 8, NZONES, NDIMS,
     .               znodelist, LZNODELIST, 0, 0,
     .               0, shape_type, zshapesize, zshapecnt,
     .               NZSHAPES, optlistid, zlid)

      ierr = dbputum(dbid, name, lname, NDIMS, x, y, z,
     .              "X", 1, "Y", 1, "Z", 1, DB_DOUBLE,
     .              NNODES, NZONES, 'Zonelist', 8,
     .              DB_F77NULLSTRING, 0, optlistid, meshid)


      deallocate(x)
      deallocate(y)
      deallocate(z)
      deallocate(znodelist)

      builducd = meshid
      end
