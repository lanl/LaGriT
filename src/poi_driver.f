subroutine poisson_disk(imsgin,xmsgin,cmsgin,msgtyp,nwds,ierror)

C
C     ??? Proposed syntax, could change as development continues
C     createpts / poisson_disk / 2d_polygon / mo_out / mo_polygon / [h_spacing_scalar|mo_h_spacing_field] / [connect|no_connect]
C
C     createpts / poisson_disk / 2d_polygon / mo_out / [mo_polygon|file_polygon] / h_spacing / [connect|no_connect]
C     createpts / poisson_disk / 2d_polygon / mo_out / [mo_polygon|file_polygon] / [mo_scalar_field_mesh|file_field_mesh] / attribute_name / [connect|no_connect]
C     createpts / poisson_disk / 3d_cube / mo_out / xmin ymin zmin / xmax ymax zmax / h_spacing / [connect|no_connect]
C     createpts / poisson_disk / 3d_cube / mo_out / xmin ymin zmin / xmax ymax zmax / [mo_scalar_field_mesh|file_field_mesh] / attribute_name / [connect|no_connect]

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
            integer i,ilen,ilen2,lenopt,ityp,ierr,ierrw
            integer h_fac, npx, npy, npz, nverts
            integer ndimension, if_connect
            integer if_h_provided, if_h_field_provided
            C
            C     Reproduce variables from J. Hyman Python driver
            C
            integer np_x, np_y
            real*8 h, h2, h_extrude, h_radius, h_trans, h_prime,
                 &       h_spacing,
                 &       delta, delta_x, delta_y, delta_z

                 C CWG change variable names CWG     real*8  pxmin,pymin,pzmin,pxmax,pymax,pzmax
                 real*8 xmin_poly,xmax_poly,
                 &       ymin_poly,ymax_poly,
                 &       zmin_poly,zmax_poly,
                 &       epsilona_poly,epsilonv_poly,
                 &       xmin_buff,xmax_buff,
                 &       ymin_buff,ymax_buff,
                 &       zmin_buff,zmax_buff,
                 &       z_constant,
                 &       buffer_factor

                 integer ijob_buffer

                 C     pointers
                 pointer (ipxic,xic),(ipyic,yic),(ipzic,zic)
                 pointer (ipout,out)
                 real*8 xic(*),yic(*),zic(*),out(*)

                 C     temporary cray pointer with assigned variable
                 pointer (ipxval, xval)
                 real*8 xval


                 character*32  mo_poi_poly, mo_h_field_pts, mo_poisson_pts_out
                 character*32  isubname, file_avs_poly
                 character*32  file_poisson_vertices, mo_h_field_user
                 character*512 logmess
                 character*8092 cbuf

                 integer icharlnf

                 C
                 C Begin
                 C     Do some work for the poisson routines
                 C     in this fortran driver calling lagrit commands
                 C     once everything is setup, call poi to create points
                 C     hard-wired to read poly.inp from test

                 isubname="poisson_disk"
                          ierror = 0

                                   write(logmess,'(a)') 'Begin Fortran driver for poisson.'
                                       call writloga('default',1,logmess,1,ierrw)
                                       C
                                       C     These parameters will be set based on user input.
                                       C
                                       mo_poi_poly      = "                                "
                                               file_avs_poly    = "                                "
                                                       ndimension = 0
                                                               if_connect = 0
                                                                       if_h_provided = 0
                                                                               if_h_field_provided = 0
                                                                                       C ----------------------------------------------------------
                                                                                       C     Command Argument 3 (2 since passed from createpts)
                                                                                       C     Determine if action is 2D polygon or 3D box
                                                                                       C ----------------------------------------------------------
                                                                                       if(msgtyp(2) .eq. 3) then
                                                                                               C
                                                                                               C     Argument is type character
                                                                                               C
                                                                                               lenopt=icharlnf(cmsgin(2))
                                                                                                       if(cmsgin(2)(1:lenopt) .eq. '2d_polygon')then
                                                                                                           ndimension = 2
                                                                                                                   elseif(cmsgin(2)(1:lenopt) .eq. '3d_box')then
                                                                                                                   ndimension = 3
                                                                                                                           else
                                                                                                                               call x3d_error(isubname,'invalid token 3')
                                                                                                                               call x3d_error(isubname,cmsgin(2)(1:lenopt))
                                                                                                                               call x3d_error(isubname,'valid tokens: 2d_polygon|3d_box')
                                                                                                                               endif
                                                                                                                               else
                                                                                                                                   call x3d_error(isubname,'invalid token 3')
                                                                                                                                   call x3d_error(isubname,'Must be type character')
                                                                                                                                   endif
                                                                                                                                   C ----------------------------------------------------------
                                                                                                                                   C     Command Argument 4 (3 since passed from createpts)
                                                                                                                                   C     Assign name of mesh object that will hold the
                                                                                                                                   C     Poisson Disk vertex distribution.
                                                                                                                                   C ----------------------------------------------------------
                                                                                                                                   if(msgtyp(3) .eq. 3) then
                                                                                                                                       lenopt=icharlnf(cmsgin(3))
                                                                                                                                               mo_poisson_pts_out = cmsgin(3)(1:lenopt)
                                                                                                                                                       endif
                                                                                                                                                       C ----------------------------------------------------------
                                                                                                                                                       C     Command Argument 5 (4 since passed from createpts)
                                                                                                                                                       C     Assign name of mesh object holding polygon data structure.
                                                                                                                                                       C ----------------------------------------------------------
                                                                                                                                                       if(msgtyp(4) .eq. 3) then
                                                                                                                                                           lenopt=icharlnf(cmsgin(3))
                                                                                                                                                                   mo_poi_poly = cmsgin(4)(1:lenopt)
                                                                                                                                                                           endif

                                                                                                                                                                           C ----------------------------------------------------------
                                                                                                                                                                           C     Command Argument 6 (5 since passed from createpts)
                                                                                                                                                                           C     If command argument is a float, assign h_spacing to this value.
                                                                                                                                                                           C     If command argument is character, assume it is the name of mesh
                                                                                                                                                                           C     object holding a quad point distribution that will be used as
                                                                                                                                                                           C     lookup table for variable h_spacing(x,y).
                                                                                                                                                                               C ----------------------------------------------------------
                                                                                                                                                                               if(msgtyp(5) .eq. 2) then
                                                                                                                                                                                   if_h_provided = 1
                                                                                                                                                                                           C
                                                                                                                                                                                           C        Use h as variable name
                                                                                                                                                                                           C
                                                                                                                                                                                           h_spacing = xmsgin(5)
                                                                                                                                                                                                   h = xmsgin(5)

                                                                                                                                                                                                           elseif(msgtyp(5) .eq. 3) then
                                                                                                                                                                                                           if_h_field_provided = 1
                                                                                                                                                                                                                   lenopt=icharlnf(cmsgin(5))
                                                                                                                                                                                                                           mo_h_field_pts = cmsgin(5)(1:lenopt)
                                                                                                                                                                                                                                   else
                                                                                                                                                                                                                                       C        ERROR
                                                                                                                                                                                                                                       endif
                                                                                                                                                                                                                                       C ----------------------------------------------------------
                                                                                                                                                                                                                                       C     Command Argument 7 (6 since passed from createpts)
                                                                                                                                                                                                                                       C     Decide of Poisson Disk point distribution is past through
                                                                                                                                                                                                                                       C     Delaunay triangulation/tetrahedralization module to create
                                                                                                                                                                                                                                   C     tri/tet connectivity.
                                                                                                                                                                                                                                   C ----------------------------------------------------------
                                                                                                                                                                                                                                   lenopt=icharlnf(cmsgin(6))
                                                                                                                                                                                                                                           if(cmsgin(6)(1:lenopt) .eq. 'connect')then
                                                                                                                                                                                                                                               if_connect = 1
                                                                                                                                                                                                                                                       elseif(cmsgin(6)(1:lenopt) .eq. 'no_connect')then
                                                                                                                                                                                                                                                       if_connect = 0
                                                                                                                                                                                                                                                               else
                                                                                                                                                                                                                                                                   call x3d_error(isubname,'invalid token 7')
                                                                                                                                                                                                                                                                   call x3d_error(isubname,cmsgin(6)(1:lenopt))
                                                                                                                                                                                                                                                                   call x3d_error(isubname,'valid tokens: connect|no_connect')
                                                                                                                                                                                                                                                                   ierror = -1
                                                                                                                                                                                                                                                                           goto 9999
                                                                                                                                                                                                                                                                           endif
                                                                                                                                                                                                                                                                           C ----------------------------------------------------------
                                                                                                                                                                                                                                                                           C     Done parsing command line tokens.
                                                                                                                                                                                                                                                                           C ----------------------------------------------------------
                                                                                                                                                                                                                                                                           C
                                                                                                                                                                                                                                                                           C
                                                                                                                                                                                                                                                                           C ----------------------------------------------------------
                                                                                                                                                                                                                                                                           C     Get polygon and grid data into poi data structure
                                                                                                                                                                                                                                                                           C     Create cmo with poisson disc point distribution
                                                                                                                                                                                                                                                                           C     For use in next lagrit commands such as connect
                                                                                                                                                                                                                                                                           C ----------------------------------------------------------
                                                                                                                                                                                                                                                                           C
                                                                                                                                                                                                                                                                           call cmo_select(mo_poi_poly,ierr)
                                                                                                                                                                                                                                                                           cbuf='setsize ; finish'
                                                                                                                                                                                                                                                                                   call dotaskx3d(cbuf,ierr)
                                                                                                                                                                                                                                                                                   write(logmess,'(a)') 'POISSON Checkpoint, setsize done'
                                                                                                                                                                                                                                                                                   call writloga('default',1,logmess,1,ierrw)

                                                                                                                                                                                                                                                                                   cbuf='resetpts/itp ; finish'
                                                                                                                                                                                                                                                                                           call dotaskx3d(cbuf,ierr)
                                                                                                                                                                                                                                                                                           write(logmess,'(a)') 'POISSON Checkpoint, resetpts done'
                                                                                                                                                                                                                                                                                           call writloga('default',1,logmess,1,ierrw)

                                                                                                                                                                                                                                                                                           call cmo_select(mo_poi_poly,ierr)
                                                                                                                                                                                                                                                                                           write(logmess,'(a)') 'POISSON Checkpoint, getsize'
                                                                                                                                                                                                                                                                                           call writloga('default',1,logmess,1,ierrw)
                                                                                                                                                                                                                                                                                           call getsize(xmin_poly,xmax_poly,
                                                                                                                                                                                                                                                                                                   &             ymin_poly,ymax_poly,
                                                                                                                                                                                                                                                                                                   &             zmin_poly,zmax_poly,
                                                                                                                                                                                                                                                                                                   &             epsilona_poly,epsilonv_poly)
                                                                                                                                                                                                                                                                                           C
                                                                                                                                                                                                                                                                                           C     Check if mesh object topology is 1D
                                                                                                                                                                                                                                                                                           C     TBD
                                                                                                                                                                                                                                                                                           C ----------------------------------------------------------
                                                                                                                                                                                                                                                                                           C     Check if the polygon is planar in XY plane
                                                                                                                                                                                                                                                                                               C
                                                                                                                                                                                                                                                                                               print *, 'xmin_poly,xmax_poly', xmin_poly,xmax_poly
                                                                                                                                                                                                                                                                                               print *, 'ymin_poly,ymax_poly', ymin_poly,ymax_poly
                                                                                                                                                                                                                                                                                               print *, 'zmin_poly,zmax_poly', zmin_poly,zmax_poly

                                                                                                                                                                                                                                                                                               if(((xmax_poly-xmin_poly)*1.0e-9 .gt. zmax_poly-zmin_poly).and.
                                                                                                                                                                                                                                                                                                           &   ((ymax_poly-ymin_poly)*1.0e-9 .gt. zmax_poly-zmin_poly)) then
                                                                                                                                                                                                                                                                                                       C         XY Planar Polygon test OK
                                                                                                                                                                                                                                                                                                       write(logmess,'(a)')
                                                                                                                                                                                                                                                                                                       &          'POISSON Checkpoint, XY Planar test passed'
                                                                                                                                                                                                                                                                                                       call writloga('default',1,logmess,1,ierrw)
                                                                                                                                                                                                                                                                                                       write(cbuf,'(a)')
                                                                                                                                                                                                                                                                                                       &          'cmo/printatt/-def-/-xyz-/minmax ; finish '
                                                                                                                                                                                                                                                                                                       call dotaskx3d(cbuf,ierr)
                                                                                                                                                                                                                                                                                                       else
                                                                                                                                                                                                                                                                                                           call x3d_error(isubname, mo_poi_poly)
                                                                                                                                                                                                                                                                                                           ierror = -1
                                                                                                                                                                                                                                                                                                                   goto 9999
                                                                                                                                                                                                                                                                                                                   endif
                                                                                                                                                                                                                                                                                                                   C ----------------------------------------------------------
                                                                                                                                                                                                                                                                                                                   C
                                                                                                                                                                                                                                                                                                                   C
                                                                                                                                                                                                                                                                                                                   C     Compute some lenght scale parameters based on h, minimum feature size
                                                                                                                                                                                                                                                                                                                   C
                                                                                                                                                                                                                                                                                                                   write(logmess,'(a)') 'POISSON Checkpoint, set h values'
                                                                                                                                                                                                                                                                                                                   C      h_spacing = 0.01d0
                                                                                                                                                                                                                                                                                                                           C      h = 0.01d0
                                                                                                                                                                                                                                                                                                                                   h2 = 2.0*h
                                                                                                                                                                                                                                                                                                                                           delta = 0.7d0
                                                                                                                                                                                                                                                                                                                                                   h_extrude = 0.8d0*h2

                                                                                                                                                                                                                                                                                                                                                           print *, 'h_spacing, h, h2, delta, h_extrude'
                                                                                                                                                                                                                                                                                                                                                           print *, h_spacing, h, h2, delta, h_extrude
                                                                                                                                                                                                                                                                                                                                                           C
                                                                                                                                                                                                                                                                                                                                                           C ??? Only keep one of these
                                                                                                                                                                                                                                                                                                                                                           C
                                                                                                                                                                                                                                                                                                                                                           h_radius = sqrt((0.5*h_extrude)**2 + (0.5*h_extrude)**2)
                                                                                                                                                                                                                                                                                                                                                                   h_radius = ((0.5*h_extrude)**2 + (0.5*h_extrude)**2)**0.5
                                                                                                                                                                                                                                                                                                                                                                           h_trans = -0.5*h_extrude + h_radius*cos(asin(delta))
                                                                                                                                                                                                                                                                                                                                                                                   h_prime = 0.4*h2

                                                                                                                                                                                                                                                                                                                                                                                           print *, 'h_radius, h_trans, h_prime'
                                                                                                                                                                                                                                                                                                                                                                                           print *, h_radius, h_trans, h_prime
                                                                                                                                                                                                                                                                                                                                                                                           C
                                                                                                                                                                                                                                                                                                                                                                                           write(logmess,'(a)') 'POISSON Checkpoint, compute buffer'
                                                                                                                                                                                                                                                                                                                                                                                           ijob_buffer = 1
                                                                                                                                                                                                                                                                                                                                                                                                   call buffer_xyz_minmax(
                                                                                                                                                                                                                                                                                                                                                                                                           &      ijob_buffer,h,
                                                                                                                                                                                                                                                                                                                                                                                                           &      xmin_buff,xmax_buff,ymin_buff,ymax_buff,zmin_buff,zmax_buff,
                                                                                                                                                                                                                                                                                                                                                                                                           &      xmin_poly,xmax_poly,ymin_poly,ymax_poly,zmin_poly,zmax_poly)

                                                                                                                                                                                                                                                                                                                                                                                                   delta_x = xmax_buff - xmin_buff
                                                                                                                                                                                                                                                                                                                                                                                                           delta_y = ymax_buff - ymin_buff
                                                                                                                                                                                                                                                                                                                                                                                                                   np_x = ceiling(delta_x/h)
                                                                                                                                                                                                                                                                                                                                                                                                                           np_y = ceiling(delta_y/h)

                                                                                                                                                                                                                                                                                                                                                                                                                                   print *, 'delta_x, delta_y, np_x, np_y'
                                                                                                                                                                                                                                                                                                                                                                                                                                   print *, delta_x, delta_y, np_x, np_y
                                                                                                                                                                                                                                                                                                                                                                                                                                   C
                                                                                                                                                                                                                                                                                                                                                                                                                                   call cmo_get_info('zic',mo_poi_poly,ipzic,ilen,ityp,ierr)
                                                                                                                                                                                                                                                                                                                                                                                                                                   z_constant = zic(1)
                                                                                                                                                                                                                                                                                                                                                                                                                                           C
                                                                                                                                                                                                                                                                                                                                                                                                                                           C     Error check that all zic are in the same plane, are equal +-epsilon
                                                                                                                                                                                                                                                                                                                                                                                                                                           C
                                                                                                                                                                                                                                                                                                                                                                                                                                           C     Only do this if h is passed in, don't create quad mesh object if length spacing is passing
                                                                                                                                                                                                                                                                                                                                                                                                                                               C     in with an existing MO.
                                                                                                                                                                                                                                                                                                                                                                                                                                               C
                                                                                                                                                                                                                                                                                                                                                                                                                                               print *, 'z_constant', z_constant
                                                                                                                                                                                                                                                                                                                                                                                                                                               C ----------------------------------------------------------
                                                                                                                                                                                                                                                                                                                                                                                                                                               C     Set some LaGriT string variables to values
                                                                                                                                                                                                                                                                                                                                                                                                                                               C ----------------------------------------------------------
                                                                                                                                                                                                                                                                                                                                                                                                                                               write(cbuf,'(a,1pe13.6,a)')
                                                                                                                                                                                                                                                                                                                                                                                                                                                   &     'define / H_FACTOR / ',h,' ; finish '
                                                                                                                                                                                                                                                                                                                                                                                                                                                   call dotaskx3d(cbuf,ierr)
                                                                                                                                                                                                                                                                                                                                                                                                                                                   write(cbuf,'(a,i10,a)')
                                                                                                                                                                                                                                                                                                                                                                                                                                                   &     'define / NPX / ',np_x,' ; finish '
                                                                                                                                                                                                                                                                                                                                                                                                                                                   call dotaskx3d(cbuf,ierr)
                                                                                                                                                                                                                                                                                                                                                                                                                                                   write(cbuf,'(a,i10,a)')
                                                                                                                                                                                                                                                                                                                                                                                                                                                   &     'define / NPY / ',np_y,' ; finish '
                                                                                                                                                                                                                                                                                                                                                                                                                                                   call dotaskx3d(cbuf,ierr)
                                                                                                                                                                                                                                                                                                                                                                                                                                                   write(cbuf,'(a,1pe13.6,a)')
                                                                                                                                                                                                                                                                                                                                                                                                                                                   &     'define / XMIN / ',xmin_buff,' ; finish '
                                                                                                                                                                                                                                                                                                                                                                                                                                                   call dotaskx3d(cbuf,ierr)
                                                                                                                                                                                                                                                                                                                                                                                                                                                   write(cbuf,'(a,1pe13.6,a)')
                                                                                                                                                                                                                                                                                                                                                                                                                                                   &     'define / XMAX / ',xmax_buff,' ; finish '
                                                                                                                                                                                                                                                                                                                                                                                                                                                   call dotaskx3d(cbuf,ierr)
                                                                                                                                                                                                                                                                                                                                                                                                                                                   write(cbuf,'(a,1pe13.6,a)')
                                                                                                                                                                                                                                                                                                                                                                                                                                                   &     'define / YMIN / ',ymin_buff,' ; finish '
                                                                                                                                                                                                                                                                                                                                                                                                                                                   call dotaskx3d(cbuf,ierr)
                                                                                                                                                                                                                                                                                                                                                                                                                                                   write(cbuf,'(a,1pe13.6,a)')
                                                                                                                                                                                                                                                                                                                                                                                                                                                   &     'define / YMAX / ',ymax_buff,' ; finish '
                                                                                                                                                                                                                                                                                                                                                                                                                                                   call dotaskx3d(cbuf,ierr)
                                                                                                                                                                                                                                                                                                                                                                                                                                                   write(cbuf,'(a,1pe13.6,a)')
                                                                                                                                                                                                                                                                                                                                                                                                                                                   &     'define / ZMIN / ',z_constant,' ; finish '
                                                                                                                                                                                                                                                                                                                                                                                                                                                   call dotaskx3d(cbuf,ierr)
                                                                                                                                                                                                                                                                                                                                                                                                                                                   write(cbuf,'(a,1pe13.6,a)')
                                                                                                                                                                                                                                                                                                                                                                                                                                                   &     'define / ZMAX / ',z_constant,' ; finish '
                                                                                                                                                                                                                                                                                                                                                                                                                                                   call dotaskx3d(cbuf,ierr)

                                                                                                                                                                                                                                                                                                                                                                                                                                                   C ----------------------------------------------------------
                                                                                                                                                                                                                                                                                                                                                                                                                                                   C     Create a quad mesh mo_h_field_pts
                                                                                                                                                                                                                                                                                                                                                                                                                                                   C ----------------------------------------------------------
                                                                                                                                                                                                                                                                                                                                                                                                                                                   if (if_h_provided .eq. 1) then
                                                                                                                                                                                                                                                                                                                                                                                                                                                       C ----------------------------------------------------------
                                                                                                                                                                                                                                                                                                                                                                                                                                                       C     Create lookup table quad mesh points for poi routines
                                                                                                                                                                                                                                                                                                                                                                                                                                                       C ----------------------------------------------------------
                                                                                                                                                                                                                                                                                                                                                                                                                                                       cbuf = 'cmo / create / mo_h_field_pts / / / triplane ; finish'
                                                                                                                                                                                                                                                                                                                                                                                                                                                               call dotaskx3d(cbuf,ierr)
                                                                                                                                                                                                                                                                                                                                                                                                                                                                   cbuf = 'createpts / xyz / NPX NPY 1 /
                                                                                                                                                                                                                                                                                                                                                                                                                                                                           &         XMIN YMIN ZMIN / XMAX YMAX ZMAX / 1 1 1 ;
finish'
call dotaskx3d(cbuf,ierr)
cbuf = 'cmo/printatt/mo_h_field_pts /-xyz-/minmax ; finish'
       call dotaskx3d(cbuf,ierr)
       cbuf = 'cmo / addatt / mo_h_field_pts / h_field_att /
              &        vdouble / scalar / nnodes ;
finish'
call dotaskx3d(cbuf,ierr)
write(cbuf,'(a,1pe13.6,a)')
&      'cmo / setatt / mo_h_field_pts / h_field_att / 1 0 0 / ',
&       h_spacing, ' ; finish'
call dotaskx3d(cbuf,ierr)
endif

if (if_h_field_provided .eq. 1) then
    C ----------------------------------------------------------
    C     Use optional user provided variable resolution field for poi routines
    C     Assume user provided MO will have an attribute called, h_field_att
    C ----------------------------------------------------------
    ilen = icharlnf(mo_h_field_user)
               cbuf = 'cmo / copy / mo_h_field_pts / '
                      &     // mo_h_field_user(1:ilen) // ' ; finish '
                      call dotaskx3d(cbuf,ierr)

                      endif
                      C
                      C     Debug/diagnostic output
                      C
                      C ----------------------------------------------------------
                      C     Create new mesh object into which the points are dumped
                      C     Carl, please check that this is okay.
                      C ----------------------------------------------------------
                      ilen = icharlnf(mo_poisson_pts_out)
                             cbuf = 'cmo / create / '
                                    &      // mo_poisson_pts_out(1:ilen) //
                                    &      ' / / / triplane ; finish'
                                    call dotaskx3d(cbuf,ierr)

                                    cbuf = 'cmo / status / brief ; finish'
                                           call dotaskx3d(cbuf,ierr)
                                           C
                                   C     Need to pass information to poisson_2d:
                                           C     mo_h_field_pts, NXP, NYP, xic, yic, zic, h_field_att
                                           C     mo_poi_poly, NP, xic, yic, zic in counter-clockwise order
                                           C
                                           call cmo_select(mo_poi_poly,ierr)

                                           call poisson_2d(mo_poi_poly, mo_poisson_pts_out,
                                                   & h_spacing, np_x, np_y)
                                           C      call poisson_2d(h_spacing, mo_poi_poly, mo_h_field_pts)

                                           C      call poisson_2d(mo_poi_poly, h_spacing, np_x, np_y)
                                           C
                                           C     ??? Clean up, remove temporary mesh objects.
                                           C
                                           C ----------------------------------------------------------
                                           C     Obtain distribution from poisson_2d and triangulate (connect)
                                           C ----------------------------------------------------------
                                           if(if_connect .eq. 1)then
                                               cbuf = 'connect ; finish '
                                                       call dotaskx3d(cbuf,ierr)
                                                       cbuf = 'quality ; finish '
                                                               call dotaskx3d(cbuf,ierr)
                                                               C ----------------------------------------------------------
                                                               C        Apply two iterations of Laplace smoothing and Lawson flipping to smooth the mesh
                                                               C        and recover the Delaunay triangulation.
                                                               C ----------------------------------------------------------
                                                               cbuf = 'rmpoint / compress ; finish '
                                                                       call dotaskx3d(cbuf,ierr)
                                                                       cbuf = 'resetpts / itp ; finish '
                                                                               call dotaskx3d(cbuf,ierr)
                                                                               cbuf = 'assign///maxiter_sm/ 1 ; finish '
                                                                                       call dotaskx3d(cbuf,ierr)
                                                                                       cbuf = 'smooth;recon 0 ; finish '
                                                                                               call dotaskx3d(cbuf,ierr)
                                                                                               cbuf = 'smooth;recon 1 ; finish '
                                                                                                       call dotaskx3d(cbuf,ierr)
                                                                                                       cbuf = 'quality ; finish '
                                                                                                               call dotaskx3d(cbuf,ierr)
                                                                                                               endif
                                                                                                               C
                                                                                                               9999 continue
                                                                                                               C
                                                                                                               if (ierror .eq. 0) then
                                                                                                                   write(logmess,"(a)")'driver_poisson exit.'
                                                                                                                   else
                                                                                                                       write(logmess,"(a,i4)")'driver_poisson exit with error: ',ierror
                                                                                                                       endif
                                                                                                                       call writloga('default',0,logmess,1,ierrw)

                                                                                                                       return
                                                                                                                               end
                                                                                                                               subroutine buffer_xyz_minmax(ijob,buffer_factor,
                                                                                                                                       &                       xmin_buff,xmax_buff,
                                                                                                                                       &                       ymin_buff,ymax_buff,
                                                                                                                                       &                       zmin_buff,zmax_buff,
                                                                                                                                       &                       xmin,xmax,
                                                                                                                                       &                       ymin,ymax,
                                                                                                                                       &                       zmin,zmax)
                                                                                                                               C #####################################################################
                                                                                                                               C
                                                                                                                               C     PURPOSE -
                                                                                                                               C
                                                                                                                               C        Take as input a bounding box XYZ min/max and create a new bounding
                                                                                                                               C        box that is either a constant amount larger (ijob=1) or a scale factor
                                                                                                                               C        of the original box size larger (ijob=2)
                                                                                                                               C
                                                                                                                               C     INPUT ARGUMENTS -
                                                                                                                               C
                                                                                                                               C        ijob = 1, then add/subtract buffer_factor to bounding box limits
                                                                                                                                       C             = 2, then add/subtract scale factor
                                                                                                                                               C                  e.g.  xmin_buff = xmin - ((xmax - xmin)*buffer_factor)
                                                                                                                                                       C        buffer_factor - value added/subtracted (ijob=1) or scale factor (ijob=2)
                                                                                                                                                       C        xmin,xmax,ymin,ymax,zmin,zmax - input bounding box
                                                                                                                                                       C
                                                                                                                                                       C     OUTPUT ARGUMENTS -
                                                                                                                                                       C
                                                                                                                                                       C        xmin_buff,xmax_buff,ymin_buff,ymax_buff,zmin_buff,zmax_buff, - output bounding box
                                                                                                                                                       C
                                                                                                                                                       C
                                                                                                                                                       C #####################################################################

                                                                                                                                                       implicit none

                                                                                                                                                       integer ijob
                                                                                                                                                       real*8 xmin,xmax,
                                                                                                                                                       &       ymin,ymax,
                                                                                                                                                       &       zmin,zmax,
                                                                                                                                                       &       xmin_buff,xmax_buff,
                                                                                                                                                       &       ymin_buff,ymax_buff,
                                                                                                                                                       &       zmin_buff,zmax_buff,
                                                                                                                                                       &       buffer_factor

                                                                                                                                                       if(ijob .eq. 1) then
                                                                                                                                                           C ----------------------------------------------------------
                                                                                                                                                           C  Set buffer based on a fixed value added or subtracted from the min/max values
                                                                                                                                                           C ----------------------------------------------------------
                                                                                                                                                           xmin_buff = xmin - buffer_factor
                                                                                                                                                                   xmax_buff = xmax + buffer_factor
                                                                                                                                                                           ymin_buff = ymin - buffer_factor
                                                                                                                                                                                   ymax_buff = ymax + buffer_factor
                                                                                                                                                                                           zmin_buff = zmin - buffer_factor
                                                                                                                                                                                                   zmax_buff = zmax + buffer_factor
                                                                                                                                                                                                           elseif(ijob .eq. 2) then
                                                                                                                                                                                                           C ----------------------------------------------------------
                                                                                                                                                                                                           C Set buffer based on a proportion of the bounding box dimension
                                                                                                                                                                                                           C ----------------------------------------------------------
                                                                                                                                                                                                           xmin_buff = xmin - ((xmax - xmin)*buffer_factor)
                                                                                                                                                                                                                   xmax_buff = xmax + ((xmax - xmin)*buffer_factor)
                                                                                                                                                                                                                           ymin_buff = ymin - ((ymax - ymin)*buffer_factor)
                                                                                                                                                                                                                                   ymax_buff = ymax + ((ymax - ymin)*buffer_factor)
                                                                                                                                                                                                                                           zmin_buff = zmin - ((zmax - zmin)*buffer_factor)
                                                                                                                                                                                                                                                   zmax_buff = zmax + ((zmax - zmin)*buffer_factor)
                                                                                                                                                                                                                                                           endif

                                                                                                                                                                                                                                                           return
                                                                                                                                                                                                                                                                   end
