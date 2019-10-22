      subroutine silo_test()
      implicit none
      include "silo.inc"
c
    
        integer, parameter :: NNODES = 4
        integer, parameter :: NZSHAPES = 1
        integer, parameter :: LZNODELIST = 6
        integer, parameter :: NZONES = 2
        integer, parameter :: NDIMS = 2

        integer silo_err, silo_ierr, optlistid
        integer meshid
        integer silo_dbfile

        real*8  x(NNODES), y(NNODES)
        integer zshapesize(NZSHAPES), zshapecnt(NZSHAPES) ! 1 == NZSHAPES = 1
        integer znodelist(LZNODELIST)

        data zshapesize /3/ ! triangle
        data zshapecnt  /2/ ! two triangles

        data x /0.0,1.0,2.0,1.0/
        data y /0.0,2.0,0.0,-2.0/
        data znodelist /1,2,3,3,1,4/

        silo_err = dbcreate("test.silo", 11, 0, DB_LOCAL,
     &               "file info", 9, DB_HDF5, silo_dbfile)

        print*,'silo_err = ',silo_err

        silo_err = dbmkoptlist(3, optlistid)

        silo_err = dbputzl(silo_dbfile, 'Zonelist', 8, NZONES, 2,
     &              znodelist, LZNODELIST, 0,
     &              zshapesize, zshapecnt, NZSHAPES, silo_ierr)

        print*,'silo_err = ',silo_err


        silo_err = dbputum(silo_dbfile, "ucd", 3, NDIMS,
     &              x, y, DB_F77NULL,
     &              "X", 1, "Y", 1, DB_F77NULLSTRING, 0, DB_DOUBLE,
     &              NNODES, NZONES, 'Zonelist', 8, 
     &              DB_F77NULLSTRING, 0,
     &              optlistid, meshid)   

        print*,'silo_err = ',silo_err     

        silo_err = dbclose(silo_dbfile)

c
      end subroutine silo_test