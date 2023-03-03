      subroutine poisson_disk(imsgin,xmsgin,cmsgin,msgtyp,nwds,ierror)
C
C     createpts / poisson_disk / [2d_polygon|3d_box] / mo_out / mo_polygon / 
C                 h_spacing_scalar / [connect|no_connect] / [user_h_field_att.mlgi]
C
C #####################################################################
C
C     PURPOSE -
C
C     Parse inputs and setup necessary data structures for call to
C     Poisson disk point distribution algorithm.
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
C
C     driver for Poisson disk vertex distribution routine 
C     test c-fortran interface in poi_routine_2D.cpp
C      call poisson_2d
C     &     (mo_poi_poly, mo_poi_pts_out, h_spacing, np_x, np_y)
C
C #####################################################################
      implicit none
C
C  Define user_sub arguments
      character*32 cmsgin(nwds)
      integer imsgin(nwds),msgtyp(nwds)
      real*8  xmsgin(nwds)
      integer nwds,ierror,seed
C Define variables 
      integer i,ilen,ilen2,lenopt,ityp,ierr,ierrw,icharlnf
      integer h_fac, npx, npy, npz, nverts, nnodes_poly, if_rad_deg
      integer ndimension, if_connect
      integer if_h_provided, if_h_field_variable

      logical if_convex
C
      integer np_x, np_y
      real*8 h, h2, h02, h05, h08, h10, h_half,
     &       h_extrude, h_radius, h_trans, h_prime,
     &       h_spacing, delta, delta_x, delta_y, delta_z
      real*8 poi_poly_h_min, poi_poly_ang_min, poi_poly_ang_max

      real*8 
     &   xmin_poly,xmax_poly,ymin_poly,ymax_poly,zmin_poly,zmax_poly,
     &   epsilona_poly,epsilonv_poly,
     &   xmin_buff,xmax_buff,ymin_buff,ymax_buff,zmin_buff,zmax_buff,
     &   z_constant,buffer_factor
      real*8 angle_minimum
      integer ijob_buffer

C     pointers for x,y,z coordinates
      pointer (ipxic,xic),(ipyic,yic),(ipzic,zic)
      real*8 xic(*),yic(*),zic(*)
C     pointer for h_field_att
      pointer (iph_field, h_field)
      real*8 h_field(*)
      real*8 h_field_min, h_field_max
C
      character*32  mo_poi_poly, mo_poi_h_field, mo_poi_pts_out
      character*32  file_avs_poly
      character*32  file_poisson_vertices, mo_h_field_user
      character*32  file_user_h_field_att
      character*512 logmess
      character*8092 cbuf
      character*12  isubname
C
C ---------------------------------------------------------------------
C ---------------------------------------------------------------------
C
C     Do some work for the poisson routines 
C     in this fortran driver calling lagrit commands
C     once everything is setup, call poisson_2d to create points
C
      isubname="poisson_disk"
      ierror = 0
      mo_poi_h_field = 'mo_poi_h_field'
C
      write(logmess,'(a)') 
     &   'Begin driver for Poisson disk vertex distribution.'
      call writloga('default',1,logmess,1,ierrw)
C
C     These parameters will be set based on user input.
C
      mo_poi_poly      = "                                "
      file_avs_poly    = "                                "
      ndimension = 0
      if_connect = 0
      if_h_provided = 0
      if_h_field_variable = 0
      if_convex = .false.
C ---------------------------------------------------------------------
C     Command Argument 3 (2 since passed from createpts)
C     Determine if action is 2D polygon or 3D box
C ---------------------------------------------------------------------
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
         call writloga('default',1,'ERROR POISSON DISK:',0,ierrw)
         call writloga('default',0,'invalid token 3',0,ierrw)
         call writloga
     &        ('default',0,'valid token 3 2d_polygon|3d_box',0,ierrw)
         call writloga('default',1,'ERROR POISSON DISK',0,ierrw)
      endif
      else
         call writloga('default',1,'ERROR POISSON DISK:',0,ierrw)
         call writloga('default',0,'invalid token 3',0,ierrw)
         call writloga
     &        ('default',0,'valid token 3 2d_polygon|3d_box',0,ierrw)
         call writloga('default',1,'ERROR POISSON DISK',0,ierrw)
      endif
C ---------------------------------------------------------------------
C     Command Argument 4 (3 since passed from createpts)
C     Assign name of mesh object that will hold the
C     Poisson Disk vertex distribution.
C ---------------------------------------------------------------------
      if(msgtyp(3) .eq. 3) then
         lenopt=icharlnf(cmsgin(3))
         mo_poi_pts_out = cmsgin(3)(1:lenopt)
      endif
C ---------------------------------------------------------------------
C     Command Argument 5 (4 since passed from createpts)
C     Assign name of mesh object holding polygon data structure.
C ---------------------------------------------------------------------
      if(msgtyp(4) .eq. 3) then
         lenopt=icharlnf(cmsgin(3))
         mo_poi_poly = cmsgin(4)(1:lenopt)
      endif
C ---------------------------------------------------------------------
C     Command Argument 6 (5 since passed from createpts)
C     This is the target edge length for a uniform mesh or
C     this is the minmum edge length for a variable edge size mesh.
C
C     If command argument is a float or integer, assign h_spacing to this value.
C
C     If command argument is character, assume it is the name of mesh
C     object holding a quad point distribution that will be used as
C     lookup table for variable h_spacing(x,y).
C ---------------------------------------------------------------------
      if((msgtyp(5) .eq. 1) .or. (msgtyp(5) .eq. 2)) then
         if_h_provided = 1
C
C        Use h as variable name
C
         if(msgtyp(5) .eq. 2) then
         h_spacing = xmsgin(5)
         h = xmsgin(5)
         else
         h_spacing = float(imsgin(5))
         h = float(imsgin(5))
         endif  
      elseif(msgtyp(5) .eq. 3) then
         call writloga('default',1,'ERROR POISSON DISK:',0,ierrw)
         call writloga('default',0,'invalid token 6',0,ierrw)
         call writloga
     &        ('default',0,'valid token 6 real or integer',0,ierrw)
         call writloga('default',1,'ERROR POISSON DISK',0,ierrw)
      endif
C ---------------------------------------------------------------------
C     Command Argument 7 (6 since passed from createpts)
C     Decide if Poisson Disk point distribution is past through
C     Delaunay triangulation/tetrahedralization module to create
C     tri/tet connectivity.
C ---------------------------------------------------------------------
      lenopt=icharlnf(cmsgin(6))
      if(cmsgin(6)(1:lenopt) .eq. 'connect')then
         if_connect = 1
      elseif(cmsgin(6)(1:lenopt) .eq. 'no_connect')then
         if_connect = 0
      else
         call writloga('default',1,'ERROR POISSON DISK:',0,ierrw)
         call writloga('default',0,'invalid token 7',0,ierrw)
         call writloga
     &        ('default',0,'valid token 7 connect|no_connect',0,ierrw)
         call writloga('default',1,'ERROR POISSON DISK',0,ierrw)
         ierror = -1
         goto 9999
      endif
C ---------------------------------------------------------------------
C     Command Argument 8 (7 since passed from createpts)
C     If this argument exist, it should be type character, and it will
C     be used as the file name of the lookup table for h field. Once this
C     file is written to output, the code will exit this subroutine.
C
      if(msgtyp(7) .eq. 3) then
         if_h_field_variable = 1
         lenopt=icharlnf(cmsgin(7))
C         h_field_filename = cmsgin(7)(1:lenopt)
         file_user_h_field_att = cmsgin(7)(1:lenopt)
      endif
C ---------------------------------------------------------------------
C ---------------------------------------------------------------------
C     Finished parsing command line tokens.
C ---------------------------------------------------------------------
C ---------------------------------------------------------------------
C     Get polygon and grid data into poi data structure
C     Create cmo with poisson disc point distribution
C     For use in next lagrit commands such as connect
C ---------------------------------------------------------------------
C
      call cmo_select(mo_poi_poly,ierr)
      cbuf='setsize ; finish'
      call dotaskx3d(cbuf,ierr)
      cbuf='resetpts/itp ; finish'
      call dotaskx3d(cbuf,ierr)
      call cmo_select(mo_poi_poly,ierr)
      call getsize(xmin_poly,xmax_poly,
     &             ymin_poly,ymax_poly,
     &             zmin_poly,zmax_poly,
     &             epsilona_poly,epsilonv_poly)
C
C     Check if mesh object topology is 1D
C     TBD
C ---------------------------------------------------------------------
C     Check if the polygon is planar in XY plane
C
      if(((xmax_poly-xmin_poly)*1.0e-9 .lt. zmax_poly-zmin_poly) .or.
     &   ((ymax_poly-ymin_poly)*1.0e-9 .lt. zmax_poly-zmin_poly)) then
         call writloga('default',1,'ERROR POISSON DISK:',0,ierrw)
         call writloga('default',0,
     &          'ERROR: Polygon not planar in X,Y plane',0,ierrw)
         ilen = icharlnf(mo_poi_poly)
         cbuf = 'cmo/printatt/'//mo_poi_poly(1:ilen)//'/-xyz-/minmax' ; finish'
         call dotaskx3d(cbuf,ierr)
         ierror = -1
         goto 9999
      endif
C ---------------------------------------------------------------------
C
C     Compute some length scale parameters based on h, minimum feature size
C
      h2 = 2.0*h
      delta = 0.7d0
      h_extrude = 0.8d0*h2
C
      h_radius = ((0.5*h_extrude)**2 + (0.5*h_extrude)**2)**0.5
      h_trans = -0.5*h_extrude + h_radius*cos(asin(delta))
      h_prime = 0.4*h2
C
      ijob_buffer = 1
      call buffer_xyz_minmax(
     &      ijob_buffer,h,
     &      xmin_buff,xmax_buff,ymin_buff,ymax_buff,zmin_buff,zmax_buff,
     &      xmin_poly,xmax_poly,ymin_poly,ymax_poly,zmin_poly,zmax_poly)

      delta_x = xmax_buff - xmin_buff
      delta_y = ymax_buff - ymin_buff
      np_x = ceiling(delta_x/h)
      np_y = ceiling(delta_y/h)
      np_x = 1.5*np_x
      np_y = 1.5*np_y
C
      call cmo_get_info('nnodes',mo_poi_poly,nnodes_poly,ilen,ityp,ierr)
      call cmo_get_info('xic',   mo_poi_poly,ipxic,      ilen,ityp,ierr)
      call cmo_get_info('yic',   mo_poi_poly,ipyic,      ilen,ityp,ierr)
      call cmo_get_info('zic',   mo_poi_poly,ipzic,      ilen,ityp,ierr)
      z_constant = zic(1)
C
C ---------------------------------------------------------------------
C ---------------------------------------------------------------------
C
C     Error and quality checks on input polygon
C
C     Check that the polygon has at least 3 vertices
C
      if(nnodes_poly .lt. 3)then
         call writloga('default',1,'ERROR POISSON DISK:',0,ierrw)
         call writloga('default',0,
     &         'ERROR: Polygon has less than 3 vertices',0,ierrw)
         ierror = -1
         goto 9999
      endif
C
C ---------------------------------------------------------------------
C
C     Check if the polygon edge lengths are compatible with user defined h value
C     minimum polygon edge must be greater than h/2 otherwise ERROR
C
      call get_minimum_edge_length(nnodes_poly,xic,yic,poi_poly_h_min)
      if (poi_poly_h_min .lt. h/2.0)then
         call writloga('default',1,'ERROR POISSON DISK:',0,ierrw)
         write(cbuf,'(a)')
     &     'ERROR: Polygon edge smaller than target edge length'
         call writloga('default',0,cbuf,0,ierrw)
         write(cbuf,'(a,1pe13.6,a,1pe13.6)')
     &     'ERROR: h = ',h,' poi_poly_h_min = ',poi_poly_h_min
         call writloga('default',0,cbuf,0,ierrw)
         ierror = -1
         goto 9999
      else
         write(cbuf,'(a)')
     &     'Poisson disk driver:Polygon minimum edge length is'
         call writloga('default',0,cbuf,0,ierrw)
         write(cbuf,'(a)')
     &     'Poisson disk driver:compatible with user defined h'
         call writloga('default',0,cbuf,0,ierrw)
         write(cbuf,'(a,1pe13.6,a,1pe13.6)')
     &     'Poisson disk driver: h = ',h,
     &     ' poi_poly_h_min = ',poi_poly_h_min
         call writloga('default',0,cbuf,0,ierrw)
      endif
C
C ---------------------------------------------------------------------
C
C     Compute the min/max (degrees) interior angle of the input polygon
C     Hardwired to kick out if minimum angle is less than 9.9 degrees
C
      if_rad_deg = 2
      angle_minimum = 9.9

      call get_min_max_angle
     &       (nnodes_poly,xic,yic,if_rad_deg,
     &        poi_poly_ang_min,poi_poly_ang_max)

      if (poi_poly_ang_min .lt. angle_minimum)then
         call writloga('default',1,'ERROR POISSON DISK:',0,ierrw)
         write(cbuf,'(a)')
     &     'ERROR: Polygon minimum angle less than 9.9 degrees'
         call writloga('default',0,cbuf,0,ierrw)
         write(cbuf,'(a,1pe13.6,a,1pe13.6)')
     &     'ERROR: poi_poly_ang_min = ',poi_poly_ang_min,
     &           ' poi_poly_ang_max = ',poi_poly_ang_max
         call writloga('default',0,cbuf,0,ierrw)
         ierror = -1
         goto 9999
      endif
C
C
C ---------------------------------------------------------------------
C
C     Check if polygon is convex.
C        if_convex = logical .true. if convex
C                  = logical .false. if nonconvex
C
      call test_convex(nnodes_poly,xic,yic,if_convex)
C
C     Algorithm for recovering triangulation after connect
C     is different if the polygon is nonconvex
C
      if(if_convex)then
C
C       continue, no action
C
      else
         call writloga('default',1,'WARNING POISSON DISK:',0,ierrw)
         write(cbuf,'(a)')
     &     'WARNING: Polygon is nonconvex'
         call writloga('default',0,cbuf,0,ierrw)
         write(cbuf,'(a,1pe13.6,a,1pe13.6)')
     &     'WARNING: poi_poly_ang_min = ',poi_poly_ang_min,
     &           ' poi_poly_ang_max = ',poi_poly_ang_max
         call writloga('default',0,cbuf,0,ierrw)
         write(cbuf,'(a)')
     &     'WARNING: Vertices generated by Poisson disk method will'
         call writloga('default',0,cbuf,0,ierrw)
         write(cbuf,'(a)')
     &     'WARNING: be connected to form a convex hull Delaunay mesh'
         call writloga('default',0,cbuf,0,ierrw)
         write(cbuf,'(a)')
     &     'WARNING: and cells with centroid outside the bounding'
         call writloga('default',0,cbuf,0,ierrw)
         write(cbuf,'(a)')
     &     'WARNING: polygon will be removed. The exact outline of the'
         call writloga('default',0,cbuf,0,ierrw)
         write(cbuf,'(a)')
     &     'WARNING:  nonconvex polygon may not be recovered.'
         call writloga('default',0,cbuf,0,ierrw)
         write(cbuf,'(a)')
     &     'WARNING: Polygon is nonconvex'
         call writloga('default',0,cbuf,0,ierrw)
      endif
C
C ---------------------------------------------------------------------
C     Set some LaGriT string variables to values 
C ---------------------------------------------------------------------
      h_half = h*0.5d0
      h02 = h*2.0d0
      h05 = h*5.0d0
      h08 = h*8.0d0
      h10 = h*10.0d0
      write(cbuf,'(a,1pe13.6,a)')
     &     'define / POI_H_FACTOR / ',h,' ; finish '
      call dotaskx3d(cbuf,ierr)
      write(cbuf,'(a,1pe13.6,a)')
     &     'define / POI_H_FACTORX0.5 / ',h_half,' ; finish '
      call dotaskx3d(cbuf,ierr)
      write(cbuf,'(a,1pe13.6,a)')
     &     'define / POI_H_FACTORX2 / ',h02,' ; finish '
      call dotaskx3d(cbuf,ierr)
      write(cbuf,'(a,1pe13.6,a)')
     &     'define / POI_H_FACTORX5 / ',h05,' ; finish '
      call dotaskx3d(cbuf,ierr)
      write(cbuf,'(a,1pe13.6,a)')
     &     'define / POI_H_FACTORX8 / ',h08,' ; finish '
      call dotaskx3d(cbuf,ierr)
      write(cbuf,'(a,1pe13.6,a)')
     &     'define / POI_H_FACTORX10 / ',h10,' ; finish '
      call dotaskx3d(cbuf,ierr)

      write(cbuf,'(a,i10,a)')
     &     'define / POI_NPX / ',np_x,' ; finish '
      call dotaskx3d(cbuf,ierr)
      write(cbuf,'(a,i10,a)')
     &     'define / POI_NPY / ',np_y,' ; finish '
      call dotaskx3d(cbuf,ierr)
      write(cbuf,'(a,1pe13.6,a)')
     &     'define / POI_XMIN / ',xmin_buff,' ; finish '
      call dotaskx3d(cbuf,ierr)
      write(cbuf,'(a,1pe13.6,a)')
     &     'define / POI_XMAX / ',xmax_buff,' ; finish '
      call dotaskx3d(cbuf,ierr)
      write(cbuf,'(a,1pe13.6,a)')
     &     'define / POI_YMIN / ',ymin_buff,' ; finish '
      call dotaskx3d(cbuf,ierr)
      write(cbuf,'(a,1pe13.6,a)')
     &     'define / POI_YMAX / ',ymax_buff,' ; finish '
      call dotaskx3d(cbuf,ierr)
      write(cbuf,'(a,1pe13.6,a)')
     &     'define / POI_ZMIN / ',z_constant,' ; finish '
      call dotaskx3d(cbuf,ierr)
      write(cbuf,'(a,1pe13.6,a)')
     &     'define / POI_ZMAX / ',z_constant,' ; finish '
      call dotaskx3d(cbuf,ierr)

C ---------------------------------------------------------------------
C     Create a quad mesh mo_poi_h_field
C ---------------------------------------------------------------------
      if (if_h_provided .eq. 1) then
C ---------------------------------------------------------------------
C     Create lookup table quad mesh points for poi routines 
C     mesh object name:      mo_poi_h_field
C     mesh object attribute: h_field_att
C ---------------------------------------------------------------------
      cbuf = 'cmo/create/mo_poi_h_field/ / /triplane ; finish'
      call dotaskx3d(cbuf,ierr)
      cbuf = 'createpts / xyz / POI_NPX POI_NPY 1 / 
     &         POI_XMIN POI_YMIN POI_ZMIN / POI_XMAX POI_YMAX POI_ZMAX /
     &         1 1 1 ; finish'
      call dotaskx3d(cbuf,ierr)
      cbuf = 'cmo/printatt/mo_poi_h_field /-xyz-/minmax ; finish'
      call dotaskx3d(cbuf,ierr)
      cbuf = 'cmo / addatt / mo_poi_h_field / h_field_att / 
     &        vdouble / scalar / nnodes ; finish'
      call dotaskx3d(cbuf,ierr)
      write(cbuf,'(a,1pe13.6,a)')
     &      'cmo/setatt/mo_poi_h_field/h_field_att/ 1 0 0 / ', 
     &       h_spacing, ' ; finish'
      call dotaskx3d(cbuf,ierr)
      endif

      if (if_h_field_variable .eq. 1) then
C ---------------------------------------------------------------------
C     Assign mo_poi_h_field attribute h_field_att by calling user
C     supplied function user_h_field_att.mlgi
C     infile / user_h_field_att.mlgi
C     h_field_att(x,y) = ???
C
C     User defined field must have scalar values greater than or equal
C     to h_spacing_scalar.
C---------------------------------------------------------------------
      ilen = icharlnf(file_user_h_field_att)
      cbuf = 'infile/'//file_user_h_field_att(1:ilen)//' ;finish'
      call dotaskx3d(cbuf,ierr)
C
C     Test h_field_att after user sets values to be sure h_field_att
C     is greater than or equal to h_spacing_scalar.
C

      print *, mo_poi_h_field
      
      call cmo_get_info
     &     ('h_field_att',mo_poi_h_field,iph_field,ilen,ityp,ierr)
      h_field_min =  1.e20
      h_field_max = -1.e20
      do i = 1, np_x*np_y
         h_field_min = min(h_field_min,h_field(i))
         h_field_max = max(h_field_max,h_field(i))
      enddo
C
C     Error and quit if minimum in user supplied field is .lt. h*0.5
C
      if(h_field_min .lt. h*0.49)then
         call writloga('default',1,'ERROR POISSON DISK:',0,ierrw)
         write(cbuf,'(a)')
     &     'ERROR: h_field_att value is smaller than target edge length'
         call writloga('default',0,cbuf,0,ierrw)
         write(cbuf,'(a,1pe13.6,a,1pe13.6)')
     &     'ERROR: h = ',h,' h_field_min = ',h_field_min
         call writloga('default',0,cbuf,0,ierrw)
         write(cbuf,'(a)')
     &     'ERROR: User supplied function can be 0.5*h, but not smaller'
         call writloga('default',0,cbuf,0,ierrw)
         cbuf = 
     &     'cmo/printatt/mo_poi_h_field /h_field_att/minmax ; finish'
         call dotaskx3d(cbuf,ierr)
         ierror = -1
         goto 9999
      endif

C
C     WARNING if maximum in user supplied field is .gt. h*100
C
      if(h_field_max .gt. h*100.0)then
         call writloga('default',1,'WARNING POISSON DISK:',0,ierrw)
         write(cbuf,'(a)')
     &  'WARNING: h_field_att value is much larger than target h length'
         call writloga('default',0,cbuf,0,ierrw)
         write(cbuf,'(a,1pe13.6,a,1pe13.6)')
     &   'WARNING: h = ',h,' h_field_max = ',h_field_min
         call writloga('default',0,cbuf,0,ierrw)
         cbuf = 
     &     'cmo/printatt/mo_poi_h_field /h_field_att/minmax ; finish'
         call dotaskx3d(cbuf,ierr)
      endif
C
      endif
C
C ---------------------------------------------------------------------
C
C     Need to pass information to poisson_2d:
C     mo_poi_h_field, NXP, NYP, xic, yic, zic, h_field_att
C     mo_poi_poly, NP, xic, yic, zic in counter-clockwise order
C
      call cmo_select(mo_poi_poly,ierr)
C ---------------------------------------------------------------------
C ---------------------------------------------------------------------
C     Poisson Disk algorithm call
C ---------------------------------------------------------------------
      seed = 1
      call poisson_2d
     & (mo_poi_poly,mo_poi_pts_out,mo_poi_h_field,h_spacing,
     & np_x,np_y,seed)
C ---------------------------------------------------------------------
C     Poisson Disk algorithm call
C ---------------------------------------------------------------------
C ---------------------------------------------------------------------
C
C     ??? Clean up, remove temporary mesh objects.
C
C     Sort and reorder vertices based on x,y,z coordinates.
C
         cbuf = 'sort/-def-/index/ascending/ikeyv/xic yic zic ; finish'
         call dotaskx3d(cbuf,ierr)
         ilen = icharlnf(mo_poi_pts_out)
         cbuf = 'reorder/'// mo_poi_pts_out(1:ilen) //'/ikeyv ; finish'
         call dotaskx3d(cbuf,ierr)
         cbuf = 'cmo/DELATT/-def-/ikeyv ; finish '
         call dotaskx3d(cbuf,ierr)

C ---------------------------------------------------------------------
C     Obtain point distribution from poisson_2d and 
C     connect as a Delaunay triangulation
C ---------------------------------------------------------------------
      if(if_connect .eq. 1)then
         ilen = icharlnf(mo_poi_pts_out)
         cbuf = 'cmo / select / '// mo_poi_pts_out(1:ilen) //' ; finish'
         call dotaskx3d(cbuf,ierr)
         cbuf = 'cmo / setatt / -def- / imt / 1 0 0 / 1 ; finish '
         call dotaskx3d(cbuf,ierr)
         cbuf = 'connect ; finish '
         call dotaskx3d(cbuf,ierr)
         cbuf = 'quality ; finish '
         call dotaskx3d(cbuf,ierr)
C ---------------------------------------------------------------------
C        Apply two iterations of Laplace smoothing and 
C        Lawson flipping to smooth the mesh
C        and recover the Delaunay triangulation.
C        Also call 'recon 1' as final call to insure there are no
C        angles greater than 90 degrees facing an exterior boundary.
C
C        However, if the polygon is nonconvex, do not smooth/recon until
C        after clipping has been done.
C
C ---------------------------------------------------------------------
         cbuf = 'rmpoint / compress ; finish '
         call dotaskx3d(cbuf,ierr)
         cbuf = 'resetpts / itp ; finish '
         call dotaskx3d(cbuf,ierr)
         cbuf = 'assign///maxiter_sm/ 1 ; finish '
         call dotaskx3d(cbuf,ierr)

         if(if_convex)then
            cbuf = 'smooth;recon 0 ; finish '
            call dotaskx3d(cbuf,ierr)
            cbuf = 'smooth;recon 1 ; finish '
            call dotaskx3d(cbuf,ierr)
         else
C ---------------------------------------------------------------------
C Clipping nonconvex case
C ---------------------------------------------------------------------
         cbuf = 'cmo/create/mo_poi_poly_triangulate///tri ; finish'
         call dotaskx3d(cbuf,ierr)
         ilen = icharlnf(mo_poi_poly)
         cbuf = 
     &     'copypts/mo_poi_poly_triangulate/'//mo_poi_poly(1:ilen)//
     &     ';finish'
         call dotaskx3d(cbuf,ierr)
         cbuf = 'cmo/select/mo_poi_poly_triangulate;finish'
         call dotaskx3d(cbuf,ierr)
         cbuf = 'triangulate / counterclockwise;finish'
         call dotaskx3d(cbuf,ierr)
         cbuf = 
     &     'cmo/addatt/mo_poi_poly_triangulate/
     &    iatt_poi_tmp/vint/scalar/nelements; finish'
         call dotaskx3d(cbuf,ierr)
         ilen = icharlnf(mo_poi_pts_out)
         cbuf = 'cmo/addatt/'//mo_poi_pts_out(1:ilen)//
     &          '/iatt_poi_tmp/vint/scalar/nelements; finish'
         call dotaskx3d(cbuf,ierr)
         cbuf = 'cmo/setatt/mo_poi_poly_triangulate/iatt_poi_tmp/
     &           1 0 0 / 1;finish'
         call dotaskx3d(cbuf,ierr)
         cbuf = 'interpolate/map/'//mo_poi_pts_out(1:ilen)//
     &     '/iatt_poi_tmp/1,0,0/mo_poi_poly_triangulate/iatt_poi_tmp
     &     ; finish'
         call dotaskx3d(cbuf,ierr)
         ilen = icharlnf(mo_poi_pts_out)
         cbuf = 'cmo / select / '// mo_poi_pts_out(1:ilen) //';finish'
         call dotaskx3d(cbuf,ierr)
         cbuf = 'eltset/e_delete/iatt_poi_tmp/gt/ 1 ; finish'
         call dotaskx3d(cbuf,ierr)
         cbuf = 'rmpoint/element/eltset get e_delete ; finish'
         call dotaskx3d(cbuf,ierr)
         cbuf = 'resetpts/itp ; finish'
         call dotaskx3d(cbuf,ierr)
         cbuf = 'smooth;recon 0 ; finish '
         call dotaskx3d(cbuf,ierr)
         cbuf = 'smooth;recon 1 ; finish '
         call dotaskx3d(cbuf,ierr)
         cbuf = 'cmo/delete/mo_poi_poly_triangulate; finish'
         call dotaskx3d(cbuf,ierr)
         ilen = icharlnf(mo_poi_pts_out)
         cbuf = 
     &   'cmo/DELATT/'//mo_poi_pts_out(1:ilen)//'/iatt_poi_tmp; finish'
         call dotaskx3d(cbuf,ierr)
C ---------------------------------------------------------------------
         endif
C
C        Sort and reorder cells based on x,y,z coordinate of centroid
C
         cbuf = 'createpts / median ; finish '
         call dotaskx3d(cbuf,ierr)
         cbuf = 
     &       'sort/-def-/index/ascending/ikeyc/xmed ymed zmed ; finish '
         call dotaskx3d(cbuf,ierr)
         ilen = icharlnf(mo_poi_pts_out)
         cbuf = 'reorder / '
     &      // mo_poi_pts_out(1:ilen) //' / ikeyc; finish'
         call dotaskx3d(cbuf,ierr)
C
C        Clean up some attributes
C
         cbuf = 'cmo/DELATT/-def-/ikeyc ; finish '
         call dotaskx3d(cbuf,ierr)
         cbuf = 'cmo/DELATT/-def-/xmed ; finish '
         call dotaskx3d(cbuf,ierr)
         cbuf = 'cmo/DELATT/-def-/ymed ; finish '
         call dotaskx3d(cbuf,ierr)
         cbuf = 'cmo/DELATT/-def-/zmed ; finish '
         call dotaskx3d(cbuf,ierr)

      endif
C
C     Release the distance field lookup mesh object
C
      cbuf = 'cmo/release/mo_poi_h_field ; finish'
      call dotaskx3d(cbuf,ierr)      
C
C     Make the Poisson points/mesh the current mesh object.
C
      ilen = icharlnf(mo_poi_pts_out)
      cbuf = 'cmo / select / '// mo_poi_pts_out(1:ilen) //' ; finish'
      call dotaskx3d(cbuf,ierr)
      cbuf = 'quality ; finish '
      call dotaskx3d(cbuf,ierr)
C
 9999 continue
C 
      if (ierror .eq. 0) then
        write(logmess,"(a)")'poisson_disk exit'
        call writloga('default',0,logmess,0,ierrw)
      else
        write(logmess,"(a)")'ERROR: poisson_disk'
        call writloga('default',0,logmess,0,ierrw)
        write(logmess,"(a,i4)")'poisson_disk exit with error: ',ierror
        call writloga('default',0,logmess,0,ierrw)
        write(logmess,"(a)")'ERROR: poisson_disk'
        call writloga('default',0,logmess,0,ierrw)
      endif

      return
      end
C SUBROUTINE BEGIN ####################################################
      subroutine get_minimum_edge_length(nnode,x,y,h_min)
C #####################################################################
C
C     PURPOSE - Compute the minimum edge length of a closed polygon point set
C
C
C     INPUT ARGUMENTS - 
C        nnode = number of vertices in polygon point set. Implicit last vertex connects to first
C        x,y    = x,y coordinates of verrtices
C
C     OUTPUT ARGUMENTS -
C        h_min  = minimum edge lenth in polygon
C
C #####################################################################
      implicit none
      integer nnode, i
      real*8 x(*), y(*)
      real*8 h_min, dx

      h_min = 1.0e20
      do i = 1, nnode
         dx = ((x(i+1)-x(i))**2 + (y(i+1)-y(i))**2)**0.5
         h_min = min(h_min,dx)
      enddo
C
C Close the loop, last point to first point
C
      dx = ((x(1)-x(nnode))**2 + (y(1)-y(nnode))**2)**0.5
      h_min = min(h_min,dx)

      return
      end
C SUBROUTINE BEGIN ####################################################
      subroutine test_convex(nnode,x,y,if_convex)
C #####################################################################
C
C     PURPOSE - determine if a polygon is convex or nonconvex
C             - Compute cross products of neighboring segments
C               and if sign changes then polygon is nonconvex
C
C     INPUT ARGUMENTS - 
C        nnode = number of vertices in polygon point set
C.               Assume last vertex in list connects to the first
C        x,y    = x,y coordinates of vertices
C
C     OUTPUT ARGUMENTS -
C        if_convex = logical .true. if convex
C                  = logical .false. if nonconvex
C
C #####################################################################
      implicit none
      integer nnode, i, i_p1, i_p2, num_positive, num_negative
      real*8 x(*), y(*)
      real*8 dx1,dy1,dx2,dy2,zcrossproduct
      logical if_convex

      num_positive = 0
      num_negative = 0

      do i = 1, nnode-2
         i_p1 = i+1
         i_p2 = i+2
         if(i .eq. nnode-1)i_p2 = 1
         if(i .eq. nnode)then
            i_p1 = 1
            i_p2 = 2
         endif
         dx1 = x(i_p1)-x(i)
         dy1 = y(i_p1)-y(i)
         dx2 = x(i_p2)-x(i_p1)
         dy2 = y(i_p2)-y(i_p1)
         zcrossproduct = dx1*dy2 - dy1*dx2
         if(zcrossproduct .ge. 0.0d0)then
             num_positive = num_positive + 1
         else
             num_negative = num_negative + 1
         endif
      enddo
      if((num_positive .gt. 0).and.(num_negative .gt. 0))then
         if_convex = .false.
      else
         if_convex = .true.
      endif
      end
C SUBROUTINE BEGIN ####################################################
      subroutine get_min_max_angle(nnode,x,y,if_rad_deg,ang_min,ang_max)
C #####################################################################
C
C     PURPOSE - Compute the min and max interior angle of input polygon
C
C     Angle between two vectors in 2D
C     θ = acos[(a · b)/(|a|*|b|)]
C
C     INPUT ARGUMENTS - 
C        nnode = number of vertices in polygon point set
C.               Assume last vertex in list connects to the first
C        x,y    = x,y coordinates of vertices
C        if_rad_deg = 1 if output is radian
C                   = 2 if output is degree
C
C     OUTPUT ARGUMENTS -
C        ang_min = minimum interior angle in radian or degree
C        ang_max = maximum interior angle in radian or degree
C
C #####################################################################
      implicit none
      integer nnode, i, i_minus_1, i_plus_1, if_rad_deg
      real*8 x(*), y(*)
      real*8 pi
      real*8 ang_min, ang_max, ang_r, ang_d
      real*8 v1_x,v1_y,v2_x,v2_y,v1dotv2,v1mag,v2mag

      ang_min =  1.e20
      ang_max = -1.e20
      pi = 4.0d0*atan(1.d0)
C
C     i = 1 case
C
      i = 1
      i_minus_1 = nnode
      i_plus_1  = i+1
         v1_x = x(i_minus_1)-x(i)
         v1_y = y(i_minus_1)-y(i)
         v2_x = x(i_plus_1) -x(i)
         v2_y = y(i_plus_1) -y(i)
         v1dotv2= (v1_x * v2_x) + (v1_y * v2_y)
         v1mag = sqrt(v1_x**2 + v1_y**2)
         v2mag = sqrt(v2_x**2 + v2_y**2)
         ang_r = acos (v1dotv2/(v1mag*v2mag))
         ang_min = min(ang_min, ang_r)
         ang_max = max(ang_max, ang_r)

      do i = 2, nnode-1
         i_minus_1 = i-1
         i_plus_1 = i+1
         v1_x = x(i_minus_1)-x(i)
         v1_y = y(i_minus_1)-y(i)
         v2_x = x(i_plus_1) -x(i)
         v2_y = y(i_plus_1) -y(i)
         v1dotv2= (v1_x * v2_x) + (v1_y * v2_y)
         v1mag = sqrt(v1_x**2 + v1_y**2)
         v2mag = sqrt(v2_x**2 + v2_y**2)
         ang_r = acos (v1dotv2/(v1mag*v2mag))
         ang_min = min(ang_min, ang_r)
         ang_max = max(ang_max, ang_r)
      enddo
C
C     i = nnode case
C
      i = nnode
      i_minus_1 = nnode - 1
      i_plus_1  = 1
         v1_x = x(i_minus_1)-x(i)
         v1_y = y(i_minus_1)-y(i)
         v2_x = x(i_plus_1) -x(i)
         v2_y = y(i_plus_1) -y(i)
         v1dotv2= (v1_x * v2_x) + (v1_y * v2_y)
         v1mag = sqrt(v1_x**2 + v1_y**2)
         v2mag = sqrt(v2_x**2 + v2_y**2)
         ang_r = acos (v1dotv2/(v1mag*v2mag))
         ang_min = min(ang_min, ang_r)
         ang_max = max(ang_max, ang_r)
C
C     Convert to degrees if necessary
C
      if(if_rad_deg .eq. 2)then
         ang_min = (ang_min/pi)*180.0d0
         ang_max = (ang_max/pi)*180.0d0
      endif

      return
      end
C SUBROUTINE BEGIN ####################################################
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
