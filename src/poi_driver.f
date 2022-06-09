      subroutine poisson(imsgin,xmsgin,cmsgin,msgtyp,nwds,ierror)
C
C     poisson to test some code under development
C     syntax not yet defined, parameters hard-wired for now
C
C #####################################################################
C
C     PURPOSE -
C
C        Process user supplied commands
C        user_sub will be passed as a command through msgtty
C        to this routine along with user options
C
C     INPUT ARGUMENTS -
C
C        imsgin - integer array of tokens returned by parser
C        xmsgin - real array of tokens returned by parser
C        cmsgin - character array of tokens returned by parser
C        msgtyp - integer array of token types returned by parser
C
C     OUTPUT ARGUMENTS -
C
C        ierror - 0 for successful completion - -1 otherwise
C
C
C #####################################################################

C #####################################################################
C     driver for poisson routines
C     test c-fortran interface in poi_routine_2D.cpp
C     poisson_2d() will get current cmo which is the poly points
C
C     no inputs for this test, poly.inp is the files that will be read


C #####################################################################

      implicit none
C
C  Define user_sub arguments
      character*32 cmsgin(nwds)
      integer imsgin(nwds),msgtyp(nwds)
      real*8  xmsgin(nwds)
      integer nwds,ierror

C Define variables 
      integer i,ilen,ityp,ierr,ierrw
      integer h_fac, npx, npy, npz, nverts
      real*8  pxmin,pymin,pzmin,pxmax,pymax,pzmax

C     pointers
      pointer (ipxic,xic),(ipyic,yic),(ipzic,zic)
      pointer (ipout,out)
      real*8 xic(*),yic(*),zic(*),out(*)

C     temporary cray pointer with assigned variable
      pointer (ipxval, xval)
      real*8 xval

      character*32  cmop, cmol, cmodf, cout
      character*32  isubname, favs_poly
      character*512 logmess
      character*8092 cbuf

      integer icharlnf


C Begin
C     Do some work for the poisson routines 
C     in this fortran driver calling lagrit commands
C     once everything is setup, call poi to create points
C     hard-wired to read poly.inp from test

      isubname="poisson"
      ierror = 0

      write(logmess,'(a)') 'Begin Fortran driver for poisson.'
      call writloga('default',1,logmess,1,ierrw)

C ----------------------------------------------------------
C     Create background grid resolution for poi  routines 
C ----------------------------------------------------------
C     cmo / create / mo_dfield_pts / / / triplane
C     createpts / xyz / npx npy 1 / xmin ymin zmin / xmax ymax zmax / 1 1 1
C     cmo/printatt/mo_dfield_pts /-xyz-/minmax

C ----------------------------------------------------------
C     Create optional resolution for poi from distance field
C ----------------------------------------------------------

C ----------------------------------------------------------
C     Get polygon and grid data into poi data structure
C     Create cmo with poisson disc point distribution
C     For use in next lagrit commands such as connect
C ----------------------------------------------------------

C ----------------------------------------------------------
C     Read polygon cmo for poi_ routines
C ----------------------------------------------------------

      cmop = "poi_poly"
      favs_poly = "poly.inp"

      ilen = icharlnf(favs_poly)
      print*,"Read Poly File: ",favs_poly(1:ilen)

      cbuf = 'read/avs/' // favs_poly(1:ilen) //
     *       '/ poi_poly ' // ' ; finish '
      call dotaskx3d(cbuf,ierr)
      if(ierr .ne. 0) then
          call x3d_error(isubname, cmop )
          ierror = -1
          goto 9999
      endif
      cbuf='setsize; resetpts/itp ; finish'
      call dotaskx3d(cbuf,ierr)
      call cmo_select(cmop,ierr)

      call poisson_2d()

 9999 continue

      if (ierror .eq. 0) then
          write(logmess,"(a)")'driver_poisson done.'
      else
          write(logmess,"(a,i4)")'driver_poisson exit with error: ',ierror
      endif
      call writloga('default',0,logmess,1,ierrw)

      print*," "
      print*,"Exit driver_poisson command."
      print*,"-----------------------------------------------------"

      return
      end
