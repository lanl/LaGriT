      subroutine silo_test()
      implicit none
      include "silo.inc"
c
    

        !integer, allocatable :: silo_err, silo_ierr, ndims, nnodes, nelems, optlistid
        !character*90 :: silo_dbfile

        print*,'THIS IS WORKING'

        !silo_err = db_create("test.silo", 9, DB_CLOBBER, DB_LOCAL, "test mesh", 9, DB_HDF5, silo_dbfile)

        !silo_err = dbputzl2(silo_dbfile, "zonelist", 8, nei_in, ndims, silo_cv, 
        !&    silo_nconnects, 1, 0, 0, shapetype, shapesize, shapecounts,
        !&    NSHAPETYPES, DB_F77NULL, silo_ierr)

        !silo_err = dbputum(silo_dbfile, "pointmesh", 9, ndims, xic, yic, zic, "X", 
        !&    1, "Y", 1, "Z", 1, DB_DOUBLE, nnodes, nelems, "zonelist", 8, 
        !&    DB_F77NULLSTRING, 0, optlistid, silo_ierr)

        !!silo_err = dbfreeoptlist(optlistid)
        !silo_err = dbclose(silo_dbfile)c

c
      end subroutine silo_test