C #####################################################################
C
C     POISSON DISK VERTEX DISTRIBUTION DRIVERS
C
C     Public entry points (called from createpts):
C        poisson_disk     - dispatcher, routes on the dimension token
C        poisson_disk_2d  - vertices inside a planar polygon
C        poisson_disk_3d  - vertices inside an orthogonal box
C
C     The heavy lifting is done in C++:
C        poisson_2d  - poi_routine_2D.cpp
C        poisson_3d  - poi_routine_2D.cpp
C
C     2D command:
C        createpts / poisson_disk / 2d_polygon / mo_out / mo_polygon /
C                    h_spacing_scalar / [connect|no_connect] /
C                    [user_h_field_att.mlgi]
C                    [ poisson_seed      integer ]
C                    [ number_of_samples integer ]
C                    [ resample_sweeps   integer ]
C
C     3D command:
C        createpts / poisson_disk / 3d_box / mo_out /
C                    h_spacing_scalar / [connect|no_connect] /
C                    [user_h_field_att.mlgi]
C                    [ poisson_seed      integer ]
C                    [ number_of_samples integer ]
C                    [ resample_sweeps   integer ]
C
C     The 3D case takes its bounding box from user LaGriT variables:
C        define / POI_XMIN / real      define / POI_XMAX / real
C        define / POI_YMIN / real      define / POI_YMAX / real
C        define / POI_ZMIN / real      define / POI_ZMAX / real
C     and optionally
C        define / IF_POI_EDGE_BUFFER / integer   (default 1)
C        define / POI_EDGE_BUFFER    / real      (default 1.0)
C
C #####################################################################
C
C     HOW THIS FILE IS ORGANIZED
C
C     Roughly forty percent of the original code was input parsing,
C     input checking and error reporting, tangled together with the
C     work. The file is now split into sections that keep those three
C     concerns apart:
C
C       SECTION 1  Public entry points
C                  Each driver is a short, readable sequence of
C                  phases. No message text, no token indices buried in
C                  logic.
C
C       SECTION 2  Command line parsing
C                  Turns tokens into values. Reports faults by adding
C                  them to the error accumulator; never decides
C                  control flow, never writes to the log directly.
C
C       SECTION 3  Error accumulation and logging
C                  The ONLY place that writes error or warning text.
C                  One emitter owns the banner format, so every block
C                  is delimited identically by construction.
C
C       SECTION 4  Input validation
C                  Applies policy thresholds from poi_params.h to
C                  facts computed by SECTION 7, and records any
C                  violations. Contains no geometry math itself.
C
C       SECTION 5  LaGriT command helpers
C                  Thin wrappers that issue commands and define
C                  variables.
C
C       SECTION 6  Mesh construction and output
C                  The actual work: build lookup meshes, run the
C                  sampler, connect, sort, clean up.
C
C       SECTION 7  Pure geometry utilities
C                  No I/O, no globals, no error handling. Given
C                  coordinates, return numbers. Trivially testable in
C                  isolation.
C
C     THE ERROR HANDLING CONTRACT
C
C     Detection, reporting and control flow are deliberately separate:
C
C       - A parsing or validation routine that finds a fault calls
C         poi_err_add and keeps going. It does not log, and it does
C         not return a status.
C       - The driver ends each phase with poi_err_report, which emits
C         everything found in that phase as ONE error block and sets
C         ierror once.
C       - The driver branches on ierror at phase boundaries only.
C
C     The practical effect is that a command with several mistakes in
C     it reports all of them in one run. Previously only the 3D box
C     check did this; now the whole input phase does.
C
C     Warnings do not affect control flow, so they are logged
C     immediately through poi_log_warn rather than accumulated.
C
C #####################################################################
C
C
C =====================================================================
C =====================================================================
C     SECTION 1 - PUBLIC ENTRY POINTS
C =====================================================================
C =====================================================================
C
C SUBROUTINE BEGIN ####################################################
      subroutine poisson_disk(imsgin,xmsgin,cmsgin,msgtyp,nwds,ierror)
C #####################################################################
C
C     PURPOSE -
C
C     Dispatcher. Reads the dimension token and routes to the 2D or 3D
C     driver.
C
C     This exists so that the choice of dimension is made in exactly
C     one place, and so the dimension token is actually USED. Both
C     drivers previously parsed it and then ignored the result, each
C     one already knowing its own dimensionality.
C
C     OPTIONAL: createpts may continue to call poisson_disk_2d and
C     poisson_disk_3d directly if it already switches on the token
C     itself. Both remain public. Nothing in this file requires the
C     dispatcher to be used.
C
C     INPUT ARGUMENTS -
C        imsgin - integer array of tokens returned by parser
C        xmsgin - real array of tokens returned by parser
C        cmsgin - character array of tokens returned by parser
C        msgtyp - integer array of token types returned by parser
C        nwds   - number of tokens
C
C     OUTPUT ARGUMENTS -
C        ierror - 0 for successful completion, -1 otherwise
C
C #####################################################################
      implicit none
      include 'poi_params.h'
      include 'poi_errors.h'
C
      integer nwds,ierror
      character*32 cmsgin(nwds)
      integer imsgin(nwds),msgtyp(nwds)
      real*8  xmsgin(nwds)
C
      integer ndimension
      character*10 cdim(2)
      data cdim /'2d_polygon','3d_box'/
C
      ierror = 0
C
C     Token 2 selects the dimension in both command forms.
C
      call poi_err_reset()
      call poi_parse_enum(cmsgin,msgtyp,nwds,2,'dimension',
     &     cdim,2,ndimension)
      call poi_err_report(ierror)
      if(ierror .ne. 0) return
C
      if(ndimension .eq. 1)then
         call poisson_disk_2d(imsgin,xmsgin,cmsgin,msgtyp,nwds,ierror)
      else
         call poisson_disk_3d(imsgin,xmsgin,cmsgin,msgtyp,nwds,ierror)
      endif
C
      return
      end
C
C SUBROUTINE BEGIN ####################################################
      subroutine poisson_disk_2d
     &            (imsgin,xmsgin,cmsgin,msgtyp,nwds,ierror)
C #####################################################################
C
C     PURPOSE -
C
C     Distribute Poisson disk vertices inside a planar polygon.
C
C     INPUT ARGUMENTS -
C        imsgin - integer array of tokens returned by parser
C        xmsgin - real array of tokens returned by parser
C        cmsgin - character array of tokens returned by parser
C        msgtyp - integer array of token types returned by parser
C        nwds   - number of tokens
C
C     OUTPUT ARGUMENTS -
C        ierror - 0 for successful completion, -1 otherwise
C
C     Token layout (index into cmsgin/imsgin/xmsgin):
C        2 = 2d_polygon | 3d_box
C        3 = name of output mesh object
C        4 = name of input polygon mesh object
C        5 = h_spacing_scalar
C        6 = connect | no_connect
C        7 = start of optional arguments
C
C #####################################################################
      implicit none
      include 'poi_params.h'
      include 'poi_errors.h'
C
C  user_sub arguments
      integer nwds,ierror
      character*32 cmsgin(nwds)
      integer imsgin(nwds),msgtyp(nwds)
      real*8  xmsgin(nwds)
C
C  parsed command line values
      integer ierr
      integer ndimension,if_connect,if_h_field_variable
      integer seed,number_of_samples,resample_sweeps
      real*8  h,h_spacing
      character*32 mo_poi_poly,mo_poi_pts_out
      character*32 file_user_h_field_att
C
C  The h field mesh object name must be handed to poisson_2d in a
C  CHARACTER*32 variable, not as the POI_MO_HFIELD parameter.
C  poisson_2d is the C++ interface in poi_routine_2D.cpp and it reads
C  a fixed 32 character buffer. POI_MO_HFIELD is CHARACTER*(*) with
C  length 14, so passing it directly made the C++ side read 18 bytes
C  past the end of a read only constant, which segfaults.
C  The storage length is part of that interface, not just the text.
      character*32 mo_poi_h_field
C
C  polygon geometry
      integer nnodes_poly
      logical if_convex
      real*8  box_poly(NBOX)
      pointer (ipxic,xic),(ipyic,yic),(ipzic,zic)
      real*8  xic(*),yic(*),zic(*)
      real*8  z_constant
C
C  lookup lattice
      integer np(NDIR)
      real*8  box_look(NBOX)
C
      character*10 cdim(2),cconn(2)
      data cdim  /'2d_polygon','3d_box'/
      data cconn /'connect','no_connect'/
C
      character*32 isubname
C
C ---------------------------------------------------------------------
      isubname = 'poisson_disk_2d'
      ierror = 0
      mo_poi_h_field = POI_MO_HFIELD
      call poi_log_banner(
     & '=== Begin driver for Poisson disk 2D vertex distribution. ===')
C
C ---------------------------------------------------------------------
C     PHASE 1 - parse the command line.
C
C     Every token is parsed even if an earlier one was bad, so that a
C     command with several mistakes reports all of them at once. The
C     parsers are independent of each other, so this is safe: each one
C     only looks at its own token.
C ---------------------------------------------------------------------
      call poi_err_reset()
C
      call poi_parse_enum(cmsgin,msgtyp,nwds,2,'dimension',
     &     cdim,2,ndimension)
      call poi_parse_name(cmsgin,msgtyp,nwds,3,'output mesh object',
     &     mo_poi_pts_out)
      call poi_parse_name(cmsgin,msgtyp,nwds,4,'polygon mesh object',
     &     mo_poi_poly)
      call poi_parse_h(imsgin,xmsgin,msgtyp,nwds,5,'h_spacing_scalar',
     &     h,h_spacing)
      call poi_parse_enum(cmsgin,msgtyp,nwds,6,'connect flag',
     &     cconn,2,if_connect)
      call poi_parse_options(cmsgin,imsgin,msgtyp,nwds,7,
     &     if_h_field_variable,file_user_h_field_att,
     &     seed,number_of_samples,resample_sweeps)
C
C     The dimension token must agree with the driver that was called.
C     Catching a mismatch here means a 3d_box command routed to the 2D
C     driver is reported clearly rather than failing later on a
C     missing polygon argument.
C
      if(ndimension .ne. 1)then
         call poi_err_add(
     &      'token 3 says 3d_box but the 2D driver was called')
      endif
C
      call poi_err_report(ierror)
      if(ierror .ne. 0) goto 9999
C
C     poi_parse_enum returns the 1 based position in the keyword list,
C     so 1 is 'connect' and 2 is 'no_connect'. Convert to the 0/1
C     sense used from here on.
C
      if(if_connect .ne. 1) if_connect = 0
C
C ---------------------------------------------------------------------
C     PHASE 2 - fetch the polygon and check it is usable at this h.
C
C     poi_2d_get_polygon_geom performs the mesh object queries;
C     poi_2d_validate_polygon applies the quality thresholds. Both
C     record faults rather than logging them, so a polygon with, say,
C     a short edge AND a sharp corner reports both.
C ---------------------------------------------------------------------
      call poi_err_reset()
C
      call poi_2d_get_polygon_geom(mo_poi_poly,box_poly,nnodes_poly,
     &     ipxic,ipyic,ipzic,z_constant)
      call poi_2d_validate_polygon(nnodes_poly,xic,yic,h,if_convex)
C
      call poi_err_report(ierror)
      if(ierror .ne. 0) goto 9999
C
C ---------------------------------------------------------------------
C     PHASE 3 - build the background search lattice.
C
C     The lookup box is the polygon bounding box grown by h so the
C     sampler sees no edge effects at the boundary of the search grid.
C     The polygon is planar, so the lattice is one layer thick and
C     sits at the polygon's constant z.
C ---------------------------------------------------------------------
      call poi_buffer_box(1,h,box_look,box_poly)
      call poi_lattice_size(box_look,h,np)
      np(3) = 1
      box_look(IBX_ZMIN) = z_constant
      box_look(IBX_ZMAX) = z_constant
C
      call poi_define_h_factors(h)
      call poi_define_grid(2,'POI_',np,box_look)
C
C ---------------------------------------------------------------------
C     PHASE 4 - build and validate the h field lookup mesh.
C ---------------------------------------------------------------------
      call poi_create_h_field(2,'POI_',h_spacing)
C
      if(if_h_field_variable .eq. 1)then
         call poi_err_reset()
         call poi_apply_user_h_field(file_user_h_field_att)
         call poi_check_h_field(np(1)*np(2),h)
         call poi_err_report(ierror)
         if(ierror .ne. 0) goto 9999
      endif
C
C ---------------------------------------------------------------------
C     PHASE 5 - run the sampler.
C
C     poisson_2d needs the h field mesh (NPX,NPY,xic,yic,zic,
C     h_field_att) and the polygon (NP, xic,yic,zic counterclockwise).
C ---------------------------------------------------------------------
      call cmo_select(mo_poi_poly,ierr)
      call poisson_2d
     &     (mo_poi_poly,mo_poi_pts_out,mo_poi_h_field,h_spacing,
     &      np(1),np(2),seed,number_of_samples,resample_sweeps)
C
C ---------------------------------------------------------------------
C     PHASE 6 - connect, sort and clean up.
C ---------------------------------------------------------------------
      call poi_finish_output(2,mo_poi_poly,mo_poi_pts_out,
     &     if_connect,if_convex)
C
 9999 continue
      call poi_exit_message(isubname,ierror)
      return
      end
C
C SUBROUTINE BEGIN ####################################################
      subroutine poisson_disk_3d
     &            (imsgin,xmsgin,cmsgin,msgtyp,nwds,ierror)
C #####################################################################
C
C     PURPOSE -
C
C     Distribute Poisson disk vertices inside an orthogonal box.
C
C     INPUT ARGUMENTS -
C        imsgin - integer array of tokens returned by parser
C        xmsgin - real array of tokens returned by parser
C        cmsgin - character array of tokens returned by parser
C        msgtyp - integer array of token types returned by parser
C        nwds   - number of tokens
C
C     OUTPUT ARGUMENTS -
C        ierror - 0 for successful completion, -1 otherwise
C
C     Token layout. One less than the 2D case, because the 3D command
C     has no polygon mesh object argument:
C        2 = 2d_polygon | 3d_box
C        3 = name of output mesh object
C        4 = h_spacing_scalar
C        5 = connect | no_connect
C        6 = start of optional arguments
C
C #####################################################################
      implicit none
      include 'poi_params.h'
      include 'poi_errors.h'
C
C  user_sub arguments
      integer nwds,ierror
      character*32 cmsgin(nwds)
      integer imsgin(nwds),msgtyp(nwds)
      real*8  xmsgin(nwds)
C
C  parsed command line values
      integer ndimension,if_connect,if_h_field_variable
      integer seed,number_of_samples,resample_sweeps
      real*8  h,h_spacing
      character*32 mo_poi_pts_out
      character*32 file_user_h_field_att
C
C  See the note in poisson_disk_2d: poisson_3d reads a fixed 32
C  character buffer, so the name must travel in a CHARACTER*32
C  variable and not as the CHARACTER*(*) POI_MO_HFIELD parameter.
      character*32 mo_poi_h_field
C
C  bounding boxes. Three distinct boxes, deliberately named apart:
C     box_user  the box the user asked for
C     box_look  box_user grown by h, used for the search lattice
C     box_samp  the box actually handed to poisson_3d, which is
C               box_user unless the edge buffer shrinks it
C  The original reused one set of variables for all three, which made
C  it very hard to tell which box the sampler was really given.
      real*8  box_user(NBOX),box_look(NBOX),box_samp(NBOX)
C
      integer if_poi_edge_buffer
      real*8  poi_edge_buffer,buffer_dxyz
C
C  lookup lattice
      integer np(NDIR)
      real*8  dlook(NDIR)
C
      logical if_convex
      character*10 cdim(2),cconn(2)
      data cdim  /'2d_polygon','3d_box'/
      data cconn /'connect','no_connect'/
C
C  poi_finish_output takes a polygon mesh object name because the 2D
C  path may need it to recover a nonconvex outline. A box is always
C  convex, so the 3D path never uses it. Passing a declared blank
C  rather than a ' ' literal keeps the actual and dummy argument
C  lengths matched; a short literal here is undefined behavior even
C  though this particular argument is never read.
      character*32 mo_poly_unused
C
      character*32 isubname
C
C ---------------------------------------------------------------------
      isubname = 'poisson_disk_3d'
      ierror = 0
      mo_poi_h_field = POI_MO_HFIELD
      mo_poly_unused = ' '
C     A box is convex, so no outline recovery is ever needed.
      if_convex = .true.
      call poi_log_banner(
     & '=== Begin driver for Poisson disk 3D vertex distribution. ===')
C
C ---------------------------------------------------------------------
C     PHASE 1 - parse the command line.
C ---------------------------------------------------------------------
      call poi_err_reset()
C
      call poi_parse_enum(cmsgin,msgtyp,nwds,2,'dimension',
     &     cdim,2,ndimension)
      call poi_parse_name(cmsgin,msgtyp,nwds,3,'output mesh object',
     &     mo_poi_pts_out)
      call poi_parse_h(imsgin,xmsgin,msgtyp,nwds,4,'h_spacing_scalar',
     &     h,h_spacing)
      call poi_parse_enum(cmsgin,msgtyp,nwds,5,'connect flag',
     &     cconn,2,if_connect)
      call poi_parse_options(cmsgin,imsgin,msgtyp,nwds,6,
     &     if_h_field_variable,file_user_h_field_att,
     &     seed,number_of_samples,resample_sweeps)
C
C     The dimension token must agree with the driver that was called.
C
      if(ndimension .ne. 2)then
         call poi_err_add(
     &      'token 3 says 2d_polygon but the 3D driver was called')
      endif
C
      call poi_err_report(ierror)
      if(ierror .ne. 0) goto 9999
C
C     1 is 'connect', 2 is 'no_connect'.
C
      if(if_connect .ne. 1) if_connect = 0
C
C ---------------------------------------------------------------------
C     PHASE 2 - read the bounding box from the user's define
C     variables. All six are required; every missing one is reported.
C ---------------------------------------------------------------------
      call poi_err_reset()
      call poi_3d_get_box(box_user,if_poi_edge_buffer,poi_edge_buffer)
      call poi_err_report(ierror)
      if(ierror .ne. 0) goto 9999
C
C ---------------------------------------------------------------------
C     PHASE 3 - build the background search lattice.
C ---------------------------------------------------------------------
      call poi_buffer_box(1,h,box_look,box_user)
      call poi_lattice_size(box_look,h,np)
C
      dlook(1) = (box_look(IBX_XMAX)-box_look(IBX_XMIN))/np(1)
      dlook(2) = (box_look(IBX_YMAX)-box_look(IBX_YMIN))/np(2)
      dlook(3) = (box_look(IBX_ZMAX)-box_look(IBX_ZMIN))/np(3)
C
      call poi_define_h_factors(h)
      call poi_define_grid(3,'POI_3D_',np,box_look)
C
C ---------------------------------------------------------------------
C     PHASE 4 - build and validate the h field lookup mesh.
C
C     The lookup mesh is a hex mesh with np(1)*np(2)*np(3) nodes. The
C     original validated only np(1)*np(2) of them, so a bad
C     h_field_att value away from the first z layer went unnoticed.
C ---------------------------------------------------------------------
      call poi_3d_report_lookup(np,dlook)
      call poi_create_h_field(3,'POI_3D_',h_spacing)
C
      if(if_h_field_variable .eq. 1)then
         call poi_err_reset()
         call poi_apply_user_h_field(file_user_h_field_att)
         call poi_check_h_field(np(1)*np(2)*np(3),h)
         call poi_err_report(ierror)
         if(ierror .ne. 0) goto 9999
      endif
C
C ---------------------------------------------------------------------
C     PHASE 5 - choose the sample box and run the sampler.
C
C     if_poi_edge_buffer = 0
C        Sample the user box directly.
C     if_poi_edge_buffer = 1
C        Shrink the sample box by h*poi_edge_buffer, then add a
C        uniform vertex distribution on the six faces of the user box
C        afterwards. This produces clean, regularly spaced boundary
C        faces instead of whatever the sampler happens to place near
C        the walls.
C
C     Note that np is the LOOKUP lattice size, derived from box_look.
C     It stays tied to the lookup mesh even when the sample box is
C     smaller.
C ---------------------------------------------------------------------
      if(if_poi_edge_buffer .eq. 1)then
         buffer_dxyz = -1.0d0*h*poi_edge_buffer
         call poi_buffer_box(1,buffer_dxyz,box_samp,box_user)
      else
         call poi_copy_box(box_samp,box_user)
      endif
C
      call poisson_3d
     &     (mo_poi_pts_out,mo_poi_h_field,h_spacing,
     &      box_samp(IBX_XMIN),box_samp(IBX_XMAX),
     &      box_samp(IBX_YMIN),box_samp(IBX_YMAX),
     &      box_samp(IBX_ZMIN),box_samp(IBX_ZMAX),
     &      np(1),np(2),np(3),
     &      seed,number_of_samples,resample_sweeps)
C
      if(if_poi_edge_buffer .eq. 1)then
         call poi_3d_add_face_points(mo_poi_pts_out,h,box_user)
      endif
C
C ---------------------------------------------------------------------
C     PHASE 6 - connect, sort and clean up.
C ---------------------------------------------------------------------
      call poi_finish_output(3,mo_poly_unused,mo_poi_pts_out,
     &     if_connect,if_convex)
C
 9999 continue
      call poi_exit_message(isubname,ierror)
      return
      end
C
C
C =====================================================================
C =====================================================================
C     SECTION 2 - COMMAND LINE PARSING
C
C     Every routine here turns tokens into values. None of them log,
C     and none of them return a status. Faults are recorded with
C     poi_err_add and the driver decides what to do at the end of the
C     phase.
C =====================================================================
C =====================================================================
C
C SUBROUTINE BEGIN ####################################################
      subroutine poi_parse_enum(cmsgin,msgtyp,nwds,itok,clabel,
     &           clist,nlist,imatch)
C #####################################################################
C
C     PURPOSE -
C
C     Parse a token that must be one of a fixed set of keywords.
C
C     This one routine replaces the separate dimension and connect
C     parsers, which were the same code with different strings. It
C     also replaces their hand written error text: because the message
C     is built here from itok and clist, it automatically carries the
C     token POSITION and the full list of legal values, which the hand
C     written versions did inconsistently or not at all.
C
C     INPUT ARGUMENTS -
C        itok   - token position to read
C        clabel - human readable name of this argument, for messages
C        clist  - allowed keyword values
C        nlist  - number of allowed values
C
C     OUTPUT ARGUMENTS -
C        imatch - 1 based position of the matching keyword in clist,
C                 or 0 if the token was missing, the wrong type, or
C                 not in the list
C
C #####################################################################
      implicit none
      include 'poi_params.h'
      include 'poi_errors.h'
C
      integer nwds,itok,nlist,imatch,poi_lentrim
      character*32 cmsgin(nwds)
      integer msgtyp(nwds)
      character*(*) clabel
      character*(*) clist(nlist)
C
      integer i,lenopt,lv,llab,icharlnf
      logical poi_token_type
      character*(POI_MSG_LEN) cmsg,cvalid
C
      imatch = 0
      if(.not. poi_token_type(msgtyp,nwds,itok,POI_TYPE_CHAR,clabel))
     &   return
C
      lenopt = icharlnf(cmsgin(itok))
      do i = 1, nlist
         lv = icharlnf(clist(i))
         if(cmsgin(itok)(1:lenopt) .eq. clist(i)(1:lv))then
            imatch = i
            return
         endif
      enddo
C
C     Present but not a legal value. Report what was given and what
C     was expected.
C
      llab = poi_lentrim(clabel)
      write(cmsg,'(a,i3,a,a,a,a)')
     &   'token ',itok+POI_TOKEN_OFFSET,' (',clabel(1:llab),
     &   ') invalid value: ',
     &   cmsgin(itok)(1:lenopt)
      call poi_err_add(cmsg)
C
      call poi_str_list(cvalid,clist,nlist)
      lv = poi_lentrim(cvalid)
      write(cmsg,'(a,i3,a,a)')
     &   'token ',itok+POI_TOKEN_OFFSET,' valid values are: ',
     &   cvalid(1:lv)
      call poi_err_add(cmsg)
C
      return
      end
C
C SUBROUTINE BEGIN ####################################################
      subroutine poi_parse_name(cmsgin,msgtyp,nwds,itok,clabel,cname)
C #####################################################################
C
C     PURPOSE - Parse a mesh object name token.
C
C     FIX carried forward: the 2D driver originally extracted the
C     polygon name using the length of the PREVIOUS token,
C        lenopt = icharlnf(cmsgin(3))
C        mo_poi_poly = cmsgin(4)(1:lenopt)
C     which truncated or blank padded the name whenever the two
C     tokens had different lengths. Taking the length from the same
C     token that is being sliced makes that impossible.
C
C     OUTPUT ARGUMENTS -
C        cname - the name, or blank if the token was bad
C
C #####################################################################
      implicit none
      include 'poi_params.h'
      include 'poi_errors.h'
C
      integer nwds,itok
      character*32 cmsgin(nwds)
      integer msgtyp(nwds)
      character*(*) clabel
      character*32 cname
C
      integer lenopt,icharlnf
      logical poi_token_type
C
      cname = ' '
      if(.not. poi_token_type(msgtyp,nwds,itok,POI_TYPE_CHAR,clabel))
     &   return
C
      lenopt = icharlnf(cmsgin(itok))
      cname = cmsgin(itok)(1:lenopt)
C
      return
      end
C
C SUBROUTINE BEGIN ####################################################
      subroutine poi_parse_h(imsgin,xmsgin,msgtyp,nwds,itok,clabel,
     &           h,h_spacing)
C #####################################################################
C
C     PURPOSE -
C
C     Parse the h_spacing_scalar token, which may be given as either a
C     real or an integer constant. This is the target edge length for
C     a uniform distribution, or the minimum edge length for a
C     variable one.
C
C     NOTE on a flag that used to live here: the original set
C     if_h_provided = 1 on success and left it 0 on failure, then
C     later tested it before creating the h field mesh. Since a
C     failure now aborts the parsing phase, that flag could only ever
C     be 1 where it was tested. It has been removed rather than left
C     as a condition that is always true.
C
C #####################################################################
      implicit none
      include 'poi_params.h'
      include 'poi_errors.h'
C
      integer nwds,itok,poi_lentrim
      integer imsgin(nwds),msgtyp(nwds)
      real*8  xmsgin(nwds)
      character*(*) clabel
      real*8  h,h_spacing
C
      integer llab,icharlnf
      logical poi_token_present
      character*(POI_MSG_LEN) cmsg
C
      h = 0.0d0
      h_spacing = 0.0d0
      if(.not. poi_token_present(nwds,itok,clabel)) return
C
      if(msgtyp(itok) .eq. POI_TYPE_REAL)then
         h_spacing = xmsgin(itok)
         h = xmsgin(itok)
      elseif(msgtyp(itok) .eq. POI_TYPE_INT)then
         h_spacing = float(imsgin(itok))
         h = float(imsgin(itok))
      else
         llab = poi_lentrim(clabel)
         write(cmsg,'(a,i3,a,a,a)')
     &      'token ',itok+POI_TOKEN_OFFSET,' (',clabel(1:llab),
     &      ') must be real or integer'
         call poi_err_add(cmsg)
         return
      endif
C
C     A non positive spacing would divide by zero when the lattice is
C     sized, so it is rejected here rather than downstream.
C
      if(h .le. 0.0d0)then
         llab = poi_lentrim(clabel)
         write(cmsg,'(a,i3,a,a,a,1pe13.6)')
     &      'token ',itok+POI_TOKEN_OFFSET,' (',clabel(1:llab),
     &      ') must be greater than zero, given ',h
         call poi_err_add(cmsg)
      endif
C
      return
      end
C
C SUBROUTINE BEGIN ####################################################
      subroutine poi_parse_options(cmsgin,imsgin,msgtyp,nwds,
     &           ifirst,if_h_field_variable,file_user_h_field_att,
     &           seed,number_of_samples,resample_sweeps)
C #####################################################################
C
C     PURPOSE -
C
C     Parse the optional trailing arguments, which begin at token
C     index ifirst (7 for the 2D command, 6 for the 3D command):
C
C       - an optional variable resolution control file name (.mlgi)
C       - up to three optional keyword/value pairs
C
C     Both drivers previously encoded this as a six way table of nwds
C     values. Those tables said the same thing in both cases: the
C     control file is present exactly when nwds has the same parity as
C     ifirst, because the file occupies one token and each keyword
C     pair occupies two.
C
C        2D (ifirst=7): file present for nwds = 7, 9, 11, 13
C        3D (ifirst=6): file present for nwds = 6, 8, 10, 12
C
C     The three keyword branches were also three copies of one block
C     differing only in the keyword text and the destination variable.
C     They are now a table plus a loop.
C
C #####################################################################
      implicit none
      include 'poi_params.h'
      include 'poi_errors.h'
C
      integer nwds,ifirst,if_h_field_variable,poi_lentrim
      character*32 cmsgin(nwds)
      integer imsgin(nwds),msgtyp(nwds)
      character*32 file_user_h_field_att
      integer seed,number_of_samples,resample_sweeps
C
      integer NKEY
      parameter (NKEY = 3)
C
      integer i,k,kfound,lenopt,lk,istart,numpairs,ipair
      integer icharlnf
      integer ivals(NKEY)
      logical poi_token_type,has_ctrl_file
      character*(POI_MSG_LEN) cmsg,cvalid
      character*20 ckey(NKEY),ckeyu(NKEY)
C
C     Keyword table. Row k sets ivals(k). Lower and upper case spellings
C     are both accepted, as they were originally.
C
      data ckey  /'poisson_seed','number_of_samples',
     &            'resample_sweeps'/
      data ckeyu /'POISSON_SEED','NUMBER_OF_SAMPLES',
     &            'RESAMPLE_SWEEPS'/
C
C     Defaults, applied whether or not any options are present.
C
      ivals(1) = POI_DEF_SEED
      ivals(2) = POI_DEF_SAMPLES
      ivals(3) = POI_DEF_SWEEPS
      if_h_field_variable = 0
      file_user_h_field_att = ' '
C
      if(nwds .lt. ifirst) goto 100
C
C     Optional variable resolution control file.
C
      has_ctrl_file = (mod(nwds-ifirst,2) .eq. 0)
      if(has_ctrl_file)then
         if(poi_token_type(msgtyp,nwds,ifirst,POI_TYPE_CHAR,
     &      'h field control file'))then
            if_h_field_variable = 1
            lenopt = icharlnf(cmsgin(ifirst))
            file_user_h_field_att = cmsgin(ifirst)(1:lenopt)
         endif
         istart = ifirst + 1
      else
         istart = ifirst
      endif
C
C     Keyword/value pairs.
C
      numpairs = (nwds - istart + 1)/2
      do ipair = 1, numpairs
         i = istart + 2*(ipair-1)
C
         if(.not. poi_token_type(msgtyp,nwds,i,POI_TYPE_CHAR,
     &      'option keyword')) goto 50
C
         lenopt = icharlnf(cmsgin(i))
         kfound = 0
         do k = 1, NKEY
            lk = icharlnf(ckey(k))
            if(cmsgin(i)(1:lenopt) .eq. ckey(k)(1:lk)) kfound = k
            lk = icharlnf(ckeyu(k))
            if(cmsgin(i)(1:lenopt) .eq. ckeyu(k)(1:lk)) kfound = k
         enddo
C
         if(kfound .eq. 0)then
            write(cmsg,'(a,i3,a,a)')
     &         'token ',i+POI_TOKEN_OFFSET,' unknown option keyword: ',
     &         cmsgin(i)(1:lenopt)
            call poi_err_add(cmsg)
C           Split across two lines. Spliced on one line this came to
C           79 of the 78 usable columns, so a fourth keyword would
C           have been truncated without any sign that it had been.
            call poi_err_add('valid option keywords are:')
            call poi_str_list(cvalid,ckey,NKEY)
            lk = poi_lentrim(cvalid)
            write(cmsg,'(a,a)') '   ',cvalid(1:lk)
            call poi_err_add(cmsg)
            goto 50
         endif
C
C        The value that follows must be an integer.
C
         lk = icharlnf(ckey(kfound))
         if(poi_token_type(msgtyp,nwds,i+1,POI_TYPE_INT,
     &      ckey(kfound)(1:lk)))then
            ivals(kfound) = imsgin(i+1)
         endif
C
   50    continue
      enddo
C
  100 continue
      seed              = ivals(1)
      number_of_samples = ivals(2)
      resample_sweeps   = ivals(3)
C
      return
      end
C
C FUNCTION BEGIN ######################################################
      logical function poi_token_present(nwds,itok,clabel)
C #####################################################################
C
C     PURPOSE -
C
C     Report whether token itok exists at all, recording a fault if it
C     does not.
C
C     The original code indexed msgtyp(itok) without checking nwds
C     first, so a command that was simply too short read past the end
C     of the array before it ever reached a validity test.
C
C #####################################################################
      implicit none
      include 'poi_params.h'
      include 'poi_errors.h'
C
      integer nwds,itok,poi_lentrim
      character*(*) clabel
      integer llab,icharlnf
      character*(POI_MSG_LEN) cmsg
C
      poi_token_present = .true.
      if(itok .le. nwds) return
C
      poi_token_present = .false.
      llab = poi_lentrim(clabel)
      write(cmsg,'(a,i3,a,a,a)')
     &   'missing token ',itok+POI_TOKEN_OFFSET,' (',clabel(1:llab),')'
      call poi_err_add(cmsg)
C
      return
      end
C
C FUNCTION BEGIN ######################################################
      logical function poi_token_type(msgtyp,nwds,itok,itype,clabel)
C #####################################################################
C
C     PURPOSE -
C
C     Report whether token itok exists and has the required type,
C     recording a fault if not. Centralizing this means every wrong
C     type message in the file has the same shape and always names the
C     token position.
C
C     INPUT ARGUMENTS -
C        itype - one of POI_TYPE_INT, POI_TYPE_REAL, POI_TYPE_CHAR
C
C #####################################################################
      implicit none
      include 'poi_params.h'
      include 'poi_errors.h'
C
      integer nwds,itok,itype,poi_lentrim
      integer msgtyp(nwds)
      character*(*) clabel
C
      integer llab,lt,icharlnf
      logical poi_token_present
      character*(POI_MSG_LEN) cmsg
      character*16 ctype
C
      poi_token_type = .false.
      if(.not. poi_token_present(nwds,itok,clabel)) return
C
      if(msgtyp(itok) .eq. itype)then
         poi_token_type = .true.
         return
      endif
C
      call poi_type_name(itype,ctype)
      llab = poi_lentrim(clabel)
      lt   = poi_lentrim(ctype)
      write(cmsg,'(a,i3,a,a,a,a)')
     &   'token ',itok+POI_TOKEN_OFFSET,' (',clabel(1:llab),
     &   ') must be type ',
     &   ctype(1:lt)
      call poi_err_add(cmsg)
C
      return
      end
C
C SUBROUTINE BEGIN ####################################################
      subroutine poi_type_name(itype,ctype)
C #####################################################################
C
C     PURPOSE - Map a parser token type code to a word usable in a
C     message, so type names are spelled the same way everywhere.
C
C #####################################################################
      implicit none
      include 'poi_params.h'
      integer itype
      character*(*) ctype
C
      if(itype .eq. POI_TYPE_INT)then
         ctype = 'integer'
      elseif(itype .eq. POI_TYPE_REAL)then
         ctype = 'real'
      elseif(itype .eq. POI_TYPE_CHAR)then
         ctype = 'character'
      else
         ctype = 'unknown'
      endif
C
      return
      end
C
C SUBROUTINE BEGIN ####################################################
      subroutine poi_str_list(cout,clist,nlist)
C #####################################################################
C
C     PURPOSE - Render a keyword list as "a | b | c" for use in a
C     message, so the legal values never have to be spelled out a
C     second time in hand written error text.
C
C #####################################################################
      implicit none
      include 'poi_params.h'
      integer nlist
      character*(*) cout
      character*(*) clist(nlist)
C
      integer i,l,ipos,lout,icharlnf
C
      cout = ' '
      lout = len(cout)
      ipos = 1
      do i = 1, nlist
         l = icharlnf(clist(i))
         if(i .gt. 1)then
            if(ipos+3 .gt. lout) return
            cout(ipos:ipos+2) = ' | '
            ipos = ipos + 3
         endif
         if(ipos+l-1 .gt. lout) return
         cout(ipos:ipos+l-1) = clist(i)(1:l)
         ipos = ipos + l
      enddo
C
      return
      end
C
C
C =====================================================================
C =====================================================================
C     SECTION 3 - ERROR ACCUMULATION AND LOGGING
C
C     This is the only section that writes error or warning text. All
C     banner formatting lives in poi_log_block, so every block in the
C     file opens and closes identically. The original had seven error
C     blocks of which five never emitted a closing banner, and two
C     warning blocks neither of which did.
C =====================================================================
C =====================================================================
C
C SUBROUTINE BEGIN ####################################################
      subroutine poi_err_reset()
C #####################################################################
C
C     PURPOSE - Begin a new checking phase. Discards anything left
C     over from a previous one.
C
C #####################################################################
      implicit none
      include 'poi_params.h'
      include 'poi_errors.h'
C
      poi_nerr = 0
      poi_nerr_lost = 0
C
      return
      end
C
C SUBROUTINE BEGIN ####################################################
      subroutine poi_err_add(cmsg)
C #####################################################################
C
C     PURPOSE -
C
C     Record one fault. Does not log and does not abort, so the caller
C     can continue checking and find everything wrong in one pass.
C
C     Overflow past POI_ERR_MAX is counted rather than silently
C     dropped, and poi_err_report says how many were lost.
C
C #####################################################################
      implicit none
      include 'poi_params.h'
      include 'poi_errors.h'
C
      character*(*) cmsg
C
      if(poi_nerr .ge. POI_ERR_MAX)then
         poi_nerr_lost = poi_nerr_lost + 1
         return
      endif
C
      poi_nerr = poi_nerr + 1
      poi_errmsg(poi_nerr) = cmsg
C
      return
      end
C
C SUBROUTINE BEGIN ####################################################
      subroutine poi_err_report(ierror)
C #####################################################################
C
C     PURPOSE -
C
C     End a checking phase. Emits every accumulated fault as one error
C     block and sets the caller's status.
C
C     The accumulator is cleared afterwards, so the next phase starts
C     clean without an explicit reset.
C
C     OUTPUT ARGUMENTS -
C        ierror - -1 if anything was accumulated, 0 if nothing was
C
C #####################################################################
      implicit none
      include 'poi_params.h'
      include 'poi_errors.h'
C
      integer ierror
      integer n
      character*(POI_MSG_LEN) cmsg
C
      ierror = 0
      if(poi_nerr .le. 0) return
C
      ierror = -1
      n = poi_nerr
C
      if(poi_nerr_lost .gt. 0)then
         if(n .lt. POI_ERR_MAX)then
            n = n + 1
            write(cmsg,'(a,i5,a)')
     &         '... and ',poi_nerr_lost,' further problems not shown'
            poi_errmsg(n) = cmsg
         endif
      endif
C
      call poi_log_block(POI_SEV_ERROR,poi_errmsg,n)
C
      poi_nerr = 0
      poi_nerr_lost = 0
C
      return
      end
C
C SUBROUTINE BEGIN ####################################################
      subroutine poi_log_block(iseverity,clines,nlines)
C #####################################################################
C
C     PURPOSE -
C
C     Emit one delimited message block. This is the single point where
C     banner text and block structure are decided, which is what makes
C     every error and warning in this file look the same.
C
C     Layout:
C        <blank line>
C        ERROR POISSON DISK:        (or WARNING POISSON DISK:)
C        message line 1
C        ...
C        message line n
C        <blank line>
C        ERROR POISSON DISK         (or WARNING POISSON DISK)
C
C     INPUT ARGUMENTS -
C        iseverity - POI_SEV_ERROR or POI_SEV_WARN
C        clines    - message lines
C        nlines    - number of message lines
C
C #####################################################################
      implicit none
      include 'poi_params.h'
C
      integer iseverity,nlines
      character*(*) clines(nlines)
C
      integer i,ierrw
      character*(POI_MSG_LEN) cbuf
C
C     writloga writes with an A80 style edit descriptor. A string of
C     80 or more characters prints its leftmost 80 and reads normally.
C     Anything SHORTER is padded on the LEFT, so it appears jammed
C     against the right margin.
C
C     The original code passed bare literals here, which is why LaGriT
C     logs show those right shifted ERROR banners. Everything in this
C     routine goes through a POI_MSG_LEN buffer instead, so the whole
C     block is flush left and reads as one unit.
C
      if(iseverity .eq. POI_SEV_WARN)then
         cbuf = 'WARNING POISSON DISK:'
      else
         cbuf = 'ERROR POISSON DISK:'
      endif
      call writloga('default',1,cbuf,0,ierrw)
C
C     Message lines are passed at their FULL declared length. They must
C     not be trimmed with icharlnf - see the warning in poi_lentrim.
C
C     Message lines are indented two spaces so the block reads as a
C     unit and the banners stand out as its delimiters.
C
      do i = 1, nlines
         cbuf = '  '//clines(i)
         call writloga('default',0,cbuf,0,ierrw)
      enddo
C
      if(iseverity .eq. POI_SEV_WARN)then
         cbuf = 'WARNING POISSON DISK'
      else
         cbuf = 'ERROR POISSON DISK'
      endif
      call writloga('default',1,cbuf,0,ierrw)
C
      return
      end
C
C SUBROUTINE BEGIN ####################################################
      subroutine poi_log_error(msg1)
C #####################################################################
C     PURPOSE - Log a one line error block immediately.
C #####################################################################
      implicit none
      include 'poi_params.h'
      character*(*) msg1
      character*(POI_MSG_LEN) clines(1)
C
      clines(1) = msg1
      call poi_log_block(POI_SEV_ERROR,clines,1)
C
      return
      end
C
C SUBROUTINE BEGIN ####################################################
      subroutine poi_log_warn(msg1)
C #####################################################################
C     PURPOSE - Log a one line warning block.
C #####################################################################
      implicit none
      include 'poi_params.h'
      character*(*) msg1
      character*(POI_MSG_LEN) clines(1)
C
      clines(1) = msg1
      call poi_log_block(POI_SEV_WARN,clines,1)
C
      return
      end
C
C SUBROUTINE BEGIN ####################################################
      subroutine poi_log_warn_n(clines,nlines)
C #####################################################################
C     PURPOSE - Log a multi line warning block.
C #####################################################################
      implicit none
      include 'poi_params.h'
      integer nlines
      character*(*) clines(nlines)
C
      call poi_log_block(POI_SEV_WARN,clines,nlines)
C
      return
      end
C
C SUBROUTINE BEGIN ####################################################
      subroutine poi_log_banner(msg1)
C #####################################################################
C
C     PURPOSE - Log an informational banner, used to mark the start of
C     a driver. Not an error or a warning, so it carries no severity
C     banner of its own.
C
C #####################################################################
      implicit none
      include 'poi_params.h'
      character*(*) msg1
      integer ierrw
      character*(POI_MSG_LEN) logmess
C
      logmess = '---------------------------------------'
      call writloga('default',0,logmess,0,ierrw)
C     Copy into a full width buffer before writing. Passing msg1
C     directly would hand writloga a short string, which it right
C     justifies into an 80 column field.
      logmess = msg1
      call writloga('default',0,logmess,0,ierrw)
      logmess = '---------------------------------------'
      call writloga('default',0,logmess,0,ierrw)
C
      return
      end
C
C SUBROUTINE BEGIN ####################################################
      subroutine poi_log_info(msg1)
C #####################################################################
C     PURPOSE - Log one informational line, no banner.
C #####################################################################
      implicit none
      include 'poi_params.h'
      character*(*) msg1
      integer ierrw
      character*(POI_MSG_LEN) logmess
C
      logmess = msg1
      call writloga('default',0,logmess,0,ierrw)
C
      return
      end
C
C FUNCTION BEGIN ######################################################
      integer function poi_lentrim(cstr)
C #####################################################################
C
C     PURPOSE - Length of cstr ignoring TRAILING blanks.
C
C     *******************************************************
C     *** DO NOT USE icharlnf ON TEXT THAT CONTAINS BLANKS ***
C     *******************************************************
C
C     icharlnf returns the length up to the FIRST BLANK, not the last
C     non blank. Every use of it in the original driver was on a
C     single word - a mesh object name, a command token, a file name -
C     so the difference never showed. Applied to a message it silently
C     chops everything after the first space:
C
C        'ERROR POISSON DISK:'            came out as  'ERROR'
C        'token 6 invalid value: conect'  came out as  'token'
C        'poisson_disk_2d did not...'     came out as  'poisson_disk_2d'
C
C     That is not merely cosmetic. It made the entire error reporting
C     suite appear to produce no output at all, because the banner
C     being searched for had been reduced to a single word.
C
C     Use icharlnf for single tokens. Use poi_lentrim for anything
C     that might contain a blank.
C
C     NOTE on writloga: it writes with an A80 style edit descriptor,
C     so a string SHORTER than 80 characters is right justified into
C     an 80 column field, while one at least 80 long prints from the
C     left as expected. Message buffers are therefore declared
C     POI_MSG_LEN (132) and passed at full length rather than trimmed.
C
C #####################################################################
      implicit none
      character*(*) cstr
      integer i
C
      poi_lentrim = 0
      do i = len(cstr), 1, -1
         if(cstr(i:i) .ne. ' ')then
            poi_lentrim = i
            return
         endif
      enddo
C
      return
      end
C
C SUBROUTINE BEGIN ####################################################
      subroutine poi_str_r(cout,clab,v)
C #####################################################################
C
C     PURPOSE - Format "label = value" with the one floating point
C     edit descriptor used throughout this file, so numbers in
C     messages are never formatted three different ways.
C
C #####################################################################
      implicit none
      character*(*) cout,clab
      real*8 v
      integer l,poi_lentrim
C
C     poi_lentrim, NOT icharlnf: these labels contain blanks.
      l = poi_lentrim(clab)
      write(cout,'(a,a,1pe13.6)') clab(1:l),' = ',v
C
      return
      end
C
C SUBROUTINE BEGIN ####################################################
      subroutine poi_str_rr(cout,clab1,v1,clab2,v2)
C #####################################################################
C     PURPOSE - Format "label1 = value1   label2 = value2".
C #####################################################################
      implicit none
      character*(*) cout,clab1,clab2
      real*8 v1,v2
      integer l1,l2,poi_lentrim
C
C     poi_lentrim, NOT icharlnf: these labels contain blanks.
      l1 = poi_lentrim(clab1)
      l2 = poi_lentrim(clab2)
      write(cout,'(a,a,1pe13.6,a,a,a,1pe13.6)')
     &   clab1(1:l1),' = ',v1,'   ',clab2(1:l2),' = ',v2
C
      return
      end
C
C SUBROUTINE BEGIN ####################################################
      subroutine poi_exit_message(isubname,ierror)
C #####################################################################
C
C     PURPOSE - Report normal or error exit.
C
C     The original hardwired the string 'poisson_disk' in both
C     drivers, so a 3D failure reported the same name as a 2D one. The
C     caller's own name is used instead.
C
C #####################################################################
      implicit none
      include 'poi_params.h'
      character*(*) isubname
      integer ierror
      integer l,icharlnf
      character*(POI_MSG_LEN) cmsg
C
C     Plain lines, not a banner block. The fault itself was already
C     reported in its own block by poi_err_report, so wrapping the
C     exit status in a second ERROR block made one failure look like
C     two separate problems.
C
      l = icharlnf(isubname)
      if(ierror .eq. 0)then
         write(cmsg,'(a,a)') isubname(1:l),' exit'
      else
         write(cmsg,'(a,a,i4)')
     &      isubname(1:l),' exit with error status ',ierror
      endif
      call poi_log_info(cmsg)
C
      return
      end
C
C
C =====================================================================
C =====================================================================
C     SECTION 4 - INPUT VALIDATION
C
C     These routines apply the policy thresholds from poi_params.h to
C     facts computed by the pure geometry routines in SECTION 7. They
C     record violations with poi_err_add and do not decide control
C     flow. Warnings, which do not affect control flow, are logged
C     directly.
C =====================================================================
C =====================================================================
C
C SUBROUTINE BEGIN ####################################################
      subroutine poi_2d_validate_polygon(nnodes_poly,xic,yic,h,
     &           if_convex)
C #####################################################################
C
C     PURPOSE -
C
C     Check that the input polygon can be filled at spacing h:
C       - at least 3 vertices
C       - shortest edge  >= h * POI_EDGE_MIN_FACTOR
C       - smallest angle >= POI_ANGLE_MIN_DEG
C     and determine convexity.
C
C     A nonconvex polygon is a warning, not a fault: it simply needs
C     the extra clipping pass after connect.
C
C     The measured quantities are local. The original returned the
C     minimum edge length and the min/max angles to the driver, which
C     never looked at them.
C
C     OUTPUT ARGUMENTS -
C        if_convex - .true. if convex, .false. if nonconvex
C
C #####################################################################
      implicit none
      include 'poi_params.h'
      include 'poi_errors.h'
C
      integer nnodes_poly
      real*8  xic(*),yic(*),h
      logical if_convex
C
      integer if_rad_deg
      real*8  poly_h_min,ang_min,ang_max
      character*(POI_MSG_LEN) cmsg
      character*(POI_MSG_LEN) clines(6)
C
C     Convexity is meaningless for a degenerate point set, so assume
C     the harmless value until there are enough vertices to test.
C
      if_convex = .true.
C
      if(nnodes_poly .lt. 3)then
         write(cmsg,'(a,i8)')
     &      'polygon has fewer than 3 vertices, nnodes = ',nnodes_poly
         call poi_err_add(cmsg)
         return
      endif
C
C ---------------------------------------------------------------------
C     Shortest edge must be able to carry a sample point.
C ---------------------------------------------------------------------
      call get_minimum_edge_length(nnodes_poly,xic,yic,poly_h_min)
C
      if(poly_h_min .lt. h*POI_EDGE_MIN_FACTOR)then
         call poi_err_add(
     &      'polygon edge is smaller than the target edge length')
         call poi_str_rr(cmsg,'h',h,'polygon minimum edge',poly_h_min)
         call poi_err_add(cmsg)
      else
         call poi_str_rr(cmsg,'h',h,'polygon minimum edge',poly_h_min)
         call poi_log_info(
     &      'Poisson disk driver: polygon minimum edge length is '//
     &      'compatible with the requested h')
         call poi_log_info(cmsg)
      endif
C
C ---------------------------------------------------------------------
C     Smallest interior angle must not be too sharp to fill.
C ---------------------------------------------------------------------
      if_rad_deg = 2
      call get_min_max_angle(nnodes_poly,xic,yic,if_rad_deg,
     &     ang_min,ang_max)
C
      if(ang_min .lt. POI_ANGLE_MIN_DEG)then
         call poi_str_r(cmsg,'minimum polygon angle allowed (deg)',
     &        POI_ANGLE_MIN_DEG)
         call poi_err_add('polygon interior angle is too sharp')
         call poi_err_add(cmsg)
         call poi_str_rr(cmsg,'angle min',ang_min,'angle max',ang_max)
         call poi_err_add(cmsg)
      endif
C
C ---------------------------------------------------------------------
C     Convexity decides whether the connect step needs clipping.
C ---------------------------------------------------------------------
      call test_convex(nnodes_poly,xic,yic,if_convex)
C
      if(.not. if_convex)then
         clines(1) = 'Polygon is nonconvex.'
         call poi_str_rr(clines(2),'angle min',ang_min,
     &        'angle max',ang_max)
         clines(3) =
     &     'Vertices generated by the Poisson disk method will be'
         clines(4) =
     &     'connected to form a convex hull Delaunay mesh, and cells'
         clines(5) =
     &     'whose centroid lies outside the bounding polygon will be'
         clines(6) =
     &     'removed. The exact outline may not be recovered.'
         call poi_log_warn_n(clines,6)
      endif
C
      return
      end
C
C SUBROUTINE BEGIN ####################################################
      subroutine poi_check_h_field(nvalues,h)
C #####################################################################
C
C     PURPOSE -
C
C     Check the user supplied h_field_att values against the requested
C     minimum spacing h. The field may go down to h*0.5 but no lower;
C     a field very much larger than h earns a warning.
C
C     INPUT ARGUMENTS -
C        nvalues - number of nodes in the lookup mesh. This is
C                  np_x*np_y in 2D and np_x*np_y*np_z in 3D.
C
C     FIX: the 3D driver passed np_x*np_y here, so only the first z
C     layer of a hex lookup mesh was ever examined. A user field that
C     dipped below 0.5*h anywhere else in the box was accepted and
C     then produced a bad point distribution with no warning.
C
C #####################################################################
      implicit none
      include 'poi_params.h'
      include 'poi_errors.h'
C
      integer nvalues
      real*8  h
C
      pointer (iph_field,h_field)
      real*8  h_field(*)
      real*8  h_field_min,h_field_max
      integer i,ilen,ityp,ierr
      character*(POI_MSG_LEN) cmsg
      character*(POI_MSG_LEN) clines(3)
C
C     The mesh object name goes to cmo_get_info in a CHARACTER*32
C     variable, matching how the original called it. See the note in
C     poisson_disk_2d about why the POI_MO_HFIELD parameter must not
C     be passed directly into an argument position.
      character*32 cmo_name
C
      cmo_name = POI_MO_HFIELD
      call cmo_get_info(POI_ATT_HFIELD,cmo_name,
     &     iph_field,ilen,ityp,ierr)
C
      h_field_min =  1.d20
      h_field_max = -1.d20
      do i = 1, nvalues
         h_field_min = min(h_field_min,h_field(i))
         h_field_max = max(h_field_max,h_field(i))
      enddo
C
C ---------------------------------------------------------------------
C     Too small is fatal: the sampler cannot honor it.
C ---------------------------------------------------------------------
      if(h_field_min .lt. h*POI_HFIELD_MIN_FACTOR)then
         call poi_err_add(
     &      'h_field_att is smaller than the target edge length')
         call poi_str_rr(cmsg,'h',h,'h_field min',h_field_min)
         call poi_err_add(cmsg)
         call poi_err_add(
     &      'the user supplied field may be 0.5*h but no smaller')
         call poi_cmd('cmo/printatt/'//POI_MO_HFIELD//'/'//
     &        POI_ATT_HFIELD//'/minmax ; finish')
      endif
C
C ---------------------------------------------------------------------
C     Much too large is suspicious but not fatal.
C ---------------------------------------------------------------------
      if(h_field_max .gt. h*POI_HFIELD_WARN_FACTOR)then
         clines(1) =
     &      'h_field_att is much larger than the target edge length'
         call poi_str_rr(clines(2),'h',h,'h_field max',h_field_max)
         clines(3) = 'this is usually a units mistake'
         call poi_log_warn_n(clines,3)
         call poi_cmd('cmo/printatt/'//POI_MO_HFIELD//'/'//
     &        POI_ATT_HFIELD//'/minmax ; finish')
      endif
C
      return
      end
C
C SUBROUTINE BEGIN ####################################################
      subroutine poi_3d_get_box(box,if_poi_edge_buffer,
     &           poi_edge_buffer)
C #####################################################################
C
C     PURPOSE -
C
C     Read the 3D bounding box and edge buffer settings from the
C     user's LaGriT define variables:
C
C        define / POI_XMIN / real      define / POI_XMAX / real
C        define / POI_YMIN / real      define / POI_YMAX / real
C        define / POI_ZMIN / real      define / POI_ZMAX / real
C        define / IF_POI_EDGE_BUFFER / integer   (optional)
C        define / POI_EDGE_BUFFER    / real      (optional)
C
C     All six box limits are required. Every missing one is recorded,
C     so a user who forgot two of them is told about both at once.
C
C     FIX: the original collected a per variable status, printed a
C     message for each missing one, and then carried on anyway. ierror
C     was never set, and the box limit was assigned from rstring_out
C     whether or not the lookup had succeeded, so a missing POI_ZMAX
C     produced a silently garbage box.
C
C     FIX: the original passed the driver's own ierror as the status
C     argument of every get_define_variable_lg call, so the routine's
C     return code was whatever the last lookup happened to leave
C     behind. A local status is used here.
C
C     KNOWN LIMITATION: rstring_out is used without consulting
C     istring_type, so a limit entered as an integer constant, for
C     example define/POI_XMIN/0, may not be picked up. This matches
C     the original behavior and is left alone deliberately, because
C     changing it could alter results for existing input decks.
C
C     OUTPUT ARGUMENTS -
C        box                - bounding box, see poi_params.h layout
C        if_poi_edge_buffer - 0 or 1
C        poi_edge_buffer    - buffer size in units of h
C
C #####################################################################
      implicit none
      include 'poi_params.h'
      include 'poi_errors.h'
C
      real*8  box(NBOX)
      integer if_poi_edge_buffer
      real*8  poi_edge_buffer
C
      integer i,ierr_get
      integer istring_type,istring_out
      real*8  rstring_out
      character*32 cstring_in,cstring_out
      character*(POI_MSG_LEN) cmsg
C
C     Names of the six required variables, in poi_params.h box order.
C
      character*8 cbox(NBOX)
      data cbox /'POI_XMIN','POI_XMAX','POI_YMIN','POI_YMAX',
     &           'POI_ZMIN','POI_ZMAX'/
C
C ---------------------------------------------------------------------
C     Optional settings. A missing one is not a fault, it just means
C     the default applies.
C ---------------------------------------------------------------------
      cstring_in = 'IF_POI_EDGE_BUFFER'
      call get_define_variable_lg
     &  (cstring_in,
     &   istring_type,istring_out,rstring_out,cstring_out,ierr_get)
      if(ierr_get .eq. 0)then
         if_poi_edge_buffer = istring_out
      else
         if_poi_edge_buffer = POI_DEF_IFEDGEBUF
      endif
C
      cstring_in = 'POI_EDGE_BUFFER'
      call get_define_variable_lg
     &  (cstring_in,
     &   istring_type,istring_out,rstring_out,cstring_out,ierr_get)
      if(ierr_get .eq. 0)then
         poi_edge_buffer = rstring_out
      else
         poi_edge_buffer = POI_DEF_EDGEBUF
      endif
C
C ---------------------------------------------------------------------
C     Required box limits. Record every missing one.
C ---------------------------------------------------------------------
      do i = 1, NBOX
         box(i) = 0.0d0
         cstring_in = cbox(i)
         call get_define_variable_lg
     &     (cstring_in,
     &      istring_type,istring_out,rstring_out,cstring_out,ierr_get)
         if(ierr_get .eq. 0)then
            box(i) = rstring_out
         else
            write(cmsg,'(a,a,a)')
     &        'the 3d_box option requires define / ',cbox(i),
     &        ' / floating_point'
            call poi_err_add(cmsg)
         endif
      enddo
C
      return
      end
C
C
C =====================================================================
C =====================================================================
C     SECTION 5 - LAGRIT COMMAND HELPERS
C
C     Thin wrappers around dotaskx3d. Before these existed, thirteen
C     routines each declared a character*8092 scratch buffer and an
C     unused status variable purely to issue commands.
C =====================================================================
C =====================================================================
C
C SUBROUTINE BEGIN ####################################################
      subroutine poi_cmd(cmd)
C #####################################################################
C
C     PURPOSE - Issue one LaGriT command.
C
C     The command is copied into a character*8092 buffer before being
C     passed on, which is exactly how every call site used to do it.
C
C     The status returned by dotaskx3d is discarded here, as it was
C     at all 65 original call sites. That is a deliberate carry
C     forward, not an oversight: making these checked would change
C     control flow throughout the file and deserves its own pass.
C
C #####################################################################
      implicit none
      character*(*) cmd
      integer ierr
      character*8092 cbuf
C
      cbuf = cmd
      call dotaskx3d(cbuf,ierr)
C
      return
      end
C
C SUBROUTINE BEGIN ####################################################
      subroutine poi_define_real(cname,value)
C #####################################################################
C     PURPOSE - Issue "define / cname / value ; finish" for a real.
C #####################################################################
      implicit none
      character*(*) cname
      real*8 value
      integer ilen,icharlnf
      character*8092 cbuf
C
      ilen = icharlnf(cname)
      write(cbuf,'(a,a,a,1pe13.6,a)')
     &     'define / ',cname(1:ilen),' / ',value,' ; finish '
      call poi_cmd(cbuf)
C
      return
      end
C
C SUBROUTINE BEGIN ####################################################
      subroutine poi_define_int(cname,ivalue)
C #####################################################################
C     PURPOSE - Issue "define / cname / ivalue ; finish" for an int.
C #####################################################################
      implicit none
      character*(*) cname
      integer ivalue
      integer ilen,icharlnf
      character*8092 cbuf
C
      ilen = icharlnf(cname)
      write(cbuf,'(a,a,a,i10,a)')
     &     'define / ',cname(1:ilen),' / ',ivalue,' ; finish '
      call poi_cmd(cbuf)
C
      return
      end
C
C SUBROUTINE BEGIN ####################################################
      subroutine poi_define_h_factors(h)
C #####################################################################
C
C     PURPOSE - Publish h and its common multiples as LaGriT string
C     variables, for use by user h field control scripts. Identical in
C     both drivers.
C
C #####################################################################
      implicit none
      real*8 h
C
      call poi_define_real('POI_H_FACTOR',      h)
      call poi_define_real('POI_H_FACTORX0.5',  h*0.5d0)
      call poi_define_real('POI_H_FACTORX2',    h*2.0d0)
      call poi_define_real('POI_H_FACTORX5',    h*5.0d0)
      call poi_define_real('POI_H_FACTORX8',    h*8.0d0)
      call poi_define_real('POI_H_FACTORX10',   h*10.0d0)
C
      return
      end
C
C SUBROUTINE BEGIN ####################################################
      subroutine poi_define_grid(ndim,cprefix,np,box)
C #####################################################################
C
C     PURPOSE -
C
C     Publish the lookup grid dimensions and extents as LaGriT string
C     variables, so the createpts command that builds the h field mesh
C     can refer to them by name.
C
C     cprefix is 'POI_' for the 2D case and 'POI_3D_' for the 3D case,
C     giving POI_NPX / POI_XMIN ... and POI_3D_NPX / POI_3D_XMIN ...
C     respectively.
C
C     The 3D prefix matters. In 3D the unprefixed POI_XMIN .. POI_ZMAX
C     names belong to the USER and must not be overwritten with the
C     buffered lookup box, or the face vertex pass would land on the
C     wrong box.
C
C     NPZ is only defined for ndim=3; the 2D lookup mesh is one layer
C     thick and its createpts command hardwires a 1.
C
C #####################################################################
      implicit none
      include 'poi_params.h'
C
      integer ndim,np(NDIR)
      character*(*) cprefix
      real*8 box(NBOX)
C
      integer ilen,icharlnf
      character*32 cp
C
      ilen = icharlnf(cprefix)
      cp = cprefix(1:ilen)
C
      call poi_define_int(cp(1:ilen)//'NPX',np(1))
      call poi_define_int(cp(1:ilen)//'NPY',np(2))
      if(ndim .eq. 3)then
         call poi_define_int(cp(1:ilen)//'NPZ',np(3))
      endif
      call poi_define_real(cp(1:ilen)//'XMIN',box(IBX_XMIN))
      call poi_define_real(cp(1:ilen)//'XMAX',box(IBX_XMAX))
      call poi_define_real(cp(1:ilen)//'YMIN',box(IBX_YMIN))
      call poi_define_real(cp(1:ilen)//'YMAX',box(IBX_YMAX))
      call poi_define_real(cp(1:ilen)//'ZMIN',box(IBX_ZMIN))
      call poi_define_real(cp(1:ilen)//'ZMAX',box(IBX_ZMAX))
C
      return
      end
C
C
C =====================================================================
C =====================================================================
C     SECTION 6 - MESH CONSTRUCTION AND OUTPUT
C =====================================================================
C =====================================================================
C
C SUBROUTINE BEGIN ####################################################
      subroutine poi_2d_get_polygon_geom(mo_poi_poly,box,nnodes_poly,
     &           ipxic,ipyic,ipzic,z_constant)
C #####################################################################
C
C     PURPOSE -
C
C     Select the polygon mesh object, confirm it is planar in the X,Y
C     plane, and fetch its node count and coordinate pointers.
C
C     FIX: the "not planar" path originally built its command with
C     unbalanced quotes:
C        cbuf = 'cmo/printatt/'//mo_poi_poly(1:ilen)//
C     &         '/-xyz-/minmax' ; finish'
C     which is not valid Fortran. The "; finish" fell outside the
C     string, leaving a stray statement behind it.
C
C     The epsilon values returned by getsize are local. The original
C     threaded them all the way out to the driver, which never used
C     them; they exist only to satisfy the getsize argument list.
C
C     OUTPUT ARGUMENTS -
C        box         - polygon bounding box, poi_params.h layout
C        nnodes_poly - vertex count
C        ipxic/y/z   - Cray pointers to the coordinate arrays
C        z_constant  - the plane the polygon lies in
C
C #####################################################################
      implicit none
      include 'poi_params.h'
      include 'poi_errors.h'
C
      character*32 mo_poi_poly
      real*8  box(NBOX)
      integer nnodes_poly
      pointer (ipxic,xic),(ipyic,yic),(ipzic,zic)
      real*8  xic(*),yic(*),zic(*)
      real*8  z_constant
C
      integer ierr,ilen,ilen2,ityp,icharlnf
      real*8  epsilona_poly,epsilonv_poly
      real*8  dz
C
      nnodes_poly = 0
      z_constant  = 0.0d0
C
      call cmo_select(mo_poi_poly,ierr)
      call poi_cmd('setsize ; finish')
      call poi_cmd('resetpts/itp ; finish')
      call cmo_select(mo_poi_poly,ierr)
C
      call getsize(box(IBX_XMIN),box(IBX_XMAX),
     &             box(IBX_YMIN),box(IBX_YMAX),
     &             box(IBX_ZMIN),box(IBX_ZMAX),
     &             epsilona_poly,epsilonv_poly)
C
C ---------------------------------------------------------------------
C     The polygon must be planar in the X,Y plane. The test is
C     relative: the z extent must be negligible compared with the x
C     and y extents.
C ---------------------------------------------------------------------
      dz = box(IBX_ZMAX) - box(IBX_ZMIN)
      if(((box(IBX_XMAX)-box(IBX_XMIN))*POI_PLANAR_TOL .lt. dz) .or.
     &   ((box(IBX_YMAX)-box(IBX_YMIN))*POI_PLANAR_TOL .lt. dz))then
         call poi_err_add('polygon is not planar in the X,Y plane')
         ilen2 = icharlnf(mo_poi_poly)
         call poi_cmd('cmo/printatt/'//mo_poi_poly(1:ilen2)//
     &        '/-xyz-/minmax ; finish')
         return
      endif
C
      call cmo_get_info('nnodes',mo_poi_poly,nnodes_poly,ilen,ityp,
     &                   ierr)
      call cmo_get_info('xic',   mo_poi_poly,ipxic,      ilen,ityp,
     &                   ierr)
      call cmo_get_info('yic',   mo_poi_poly,ipyic,      ilen,ityp,
     &                   ierr)
      call cmo_get_info('zic',   mo_poi_poly,ipzic,      ilen,ityp,
     &                   ierr)
      z_constant = zic(1)
C
      return
      end
C
C SUBROUTINE BEGIN ####################################################
      subroutine poi_create_h_field(ndim,cprefix,h_spacing)
C #####################################################################
C
C     PURPOSE -
C
C     Create the lookup mesh that the Poisson disk sampler uses to
C     find the target edge length at a point, and initialize it to a
C     uniform h_spacing.
C
C        mesh object name:      POI_MO_HFIELD
C        mesh object attribute: POI_ATT_HFIELD
C
C     ndim=2 builds a one layer thick triplane mesh over the polygon;
C     ndim=3 builds a hex mesh filling the box. The grid dimensions
C     and extents come from the LaGriT variables written by
C     poi_define_grid, so cprefix must match what was used there.
C
C #####################################################################
      implicit none
      include 'poi_params.h'
C
      integer ndim
      character*(*) cprefix
      real*8 h_spacing
C
      integer ilen,icharlnf
      character*32 cp
      character*8092 cbuf
C
      ilen = icharlnf(cprefix)
      cp = cprefix(1:ilen)
C
      if(ndim .eq. 2)then
         call poi_log_banner(
     &   '=== Create quad mesh object for distance field lookup ===')
         call poi_cmd('cmo/create/'//POI_MO_HFIELD//
     &        '/ / /triplane ; finish')
         call poi_cmd('cmo / select / '//POI_MO_HFIELD//' ; finish')
         call poi_cmd('createpts / xyz / '//
     &        cp(1:ilen)//'NPX '//cp(1:ilen)//'NPY 1 / '//
     &        cp(1:ilen)//'XMIN '//cp(1:ilen)//'YMIN '//
     &        cp(1:ilen)//'ZMIN / '//
     &        cp(1:ilen)//'XMAX '//cp(1:ilen)//'YMAX '//
     &        cp(1:ilen)//'ZMAX / 1 1 1 ; finish')
      else
         call poi_log_banner(
     &   '=== Create 3d hex mesh object for distance field lookup ===')
         call poi_cmd('cmo/create/'//POI_MO_HFIELD//
     &        '/ / /hex ; finish')
         call poi_cmd('cmo / select / '//POI_MO_HFIELD//' ; finish')
         call poi_cmd('createpts / xyz / '//
     &        cp(1:ilen)//'NPX '//cp(1:ilen)//'NPY '//
     &        cp(1:ilen)//'NPZ / '//
     &        cp(1:ilen)//'XMIN '//cp(1:ilen)//'YMIN '//
     &        cp(1:ilen)//'ZMIN / '//
     &        cp(1:ilen)//'XMAX '//cp(1:ilen)//'YMAX '//
     &        cp(1:ilen)//'ZMAX / 1 1 1 ; finish')
      endif
C
      call poi_cmd('cmo/printatt/'//POI_MO_HFIELD//
     &     ' /-xyz-/minmax ; finish')
      call poi_cmd('cmo / addatt / '//POI_MO_HFIELD//' / '//
     &     POI_ATT_HFIELD//' / vdouble / scalar / nnodes ; finish')
C
      write(cbuf,'(a,1pe13.6,a)')
     &     'cmo/setatt/'//POI_MO_HFIELD//'/'//POI_ATT_HFIELD//
     &     '/ 1 0 0 / ',h_spacing,' ; finish'
      call poi_cmd(cbuf)
C
      return
      end
C
C SUBROUTINE BEGIN ####################################################
      subroutine poi_apply_user_h_field(file_user_h_field_att)
C #####################################################################
C
C     PURPOSE -
C
C     Run the user supplied control file that assigns h_field_att on
C     the lookup mesh, giving a spatially variable target edge length.
C
C        infile / user_h_field_att.mlgi
C
C     The resulting field is validated by poi_check_h_field.
C
C #####################################################################
      implicit none
      include 'poi_params.h'
      character*32 file_user_h_field_att
      integer ilen,icharlnf
C
C     Make the h field mesh current before running the user's script.
C     The supplied .mlgi files do name mo_poi_h_field explicitly, but
C     they are user written and need not, and anything in them that
C     relies on the current object should see the right one.
C
      call poi_cmd('cmo / select / '//POI_MO_HFIELD//' ; finish')
C
      ilen = icharlnf(file_user_h_field_att)
      call poi_cmd('infile/'//file_user_h_field_att(1:ilen)//' ;finish')
C
      return
      end
C
C SUBROUTINE BEGIN ####################################################
      subroutine poi_3d_report_lookup(np,dlook)
C #####################################################################
C     PURPOSE - Report the 3D lookup table size and spacing.
C #####################################################################
      implicit none
      include 'poi_params.h'
      integer np(NDIR)
      real*8  dlook(NDIR)
      character*(POI_MSG_LEN) cmsg
C
      call poi_log_info('=== Lookup Table nx ny nz ===')
      write(cmsg,'(i12,i12,i12)') np(1),np(2),np(3)
      call poi_log_info(cmsg)
      call poi_log_info('=== Lookup Table dx dy dz ===')
      write(cmsg,'(1pe13.6,1pe13.6,1pe13.6)')
     &     dlook(1),dlook(2),dlook(3)
      call poi_log_info(cmsg)
C
      return
      end
C
C SUBROUTINE BEGIN ####################################################
      subroutine poi_3d_add_face_points(mo_poi_pts_out,h,box_user)
C #####################################################################
C
C     PURPOSE -
C
C     Used when if_poi_edge_buffer = 1. The sampler was run on a box
C     shrunk by h*poi_edge_buffer, leaving a gap at the walls. Build a
C     regular brick mesh over the full user box, keep only the
C     vertices on its outside surface, and merge those into the
C     Poisson point set. The result has clean, uniformly spaced
C     boundary faces.
C
C     FIXES applied to the command strings here:
C       - 'cmo/delete'//name produced "cmo/deletename" with no
C         separator, so the mesh object was never deleted and the
C         intended name was mangled.
C       - cmo/move and copypts used a space where every other command
C         in this file uses a slash separator.
C
C     The brick is built from the user's own POI_XMIN..POI_ZMAX define
C     variables, not from the buffered POI_3D_* ones, so the face
C     vertices land exactly on the box the user asked for.
C
C #####################################################################
      implicit none
      include 'poi_params.h'
C
      character*32 mo_poi_pts_out
      real*8  h
      real*8  box_user(NBOX)
C
      integer ilen,icharlnf
      integer np(NDIR)
C
C     Vertex counts for a brick covering the full user box. No lattice
C     fudge factor here: these are the actual boundary vertices, not a
C     search lattice, so they should sit exactly h apart.
C
      np(1) = ceiling((box_user(IBX_XMAX)-box_user(IBX_XMIN))/h)
      np(2) = ceiling((box_user(IBX_YMAX)-box_user(IBX_YMIN))/h)
      np(3) = ceiling((box_user(IBX_ZMAX)-box_user(IBX_ZMIN))/h)
C
      call poi_define_int('POI_3D_NPX',np(1))
      call poi_define_int('POI_3D_NPY',np(2))
      call poi_define_int('POI_3D_NPZ',np(3))
C
      call poi_cmd('cmo / create / '//POI_MO_TMPHEX//
     &     ' / / / hex; finish')
      call poi_cmd('cmo / select / '//POI_MO_TMPHEX//' ; finish')
      call poi_cmd('createpts/brick/xyz/'//
     &     'POI_3D_NPX POI_3D_NPY POI_3D_NPZ/'//
     &     'POI_XMIN POI_YMIN POI_ZMIN/'//
     &     'POI_XMAX POI_YMAX POI_ZMAX/1 1 1; finish')
      call poi_cmd('resetpts / itp; finish')
C
C     itp >= 10 marks vertices lying on the outside surface.
C
      call poi_cmd('pset/p_out/attribute/itp/1 0 0/ge 10; finish')
      call poi_cmd('cmo/create/'//POI_MO_TMPHEXOUT//
     &     '/ / / tet; finish')
C     copypts names both objects, but cmo/create has just changed what
C     is current and the pset referenced below was built on the hex.
      call poi_cmd('copypts/'//POI_MO_TMPHEXOUT//'/'//
     &     POI_MO_TMPHEX//'/0 0 /pset get p_out; finish')
      call poi_cmd('cmo/delete/'//POI_MO_TMPHEX//'; finish')
C
      ilen = icharlnf(mo_poi_pts_out)
      call poi_cmd('addmesh/merge/'//mo_poi_pts_out(1:ilen)//'/'//
     &     mo_poi_pts_out(1:ilen)//'/'//POI_MO_TMPHEXOUT//' ; finish')
C
C     addmesh/merge does not leave a tet mesh object behind, so round
C     trip the points through a freshly created tet object and move it
C     back onto the output name.
C
      call poi_cmd('cmo/create/'//POI_MO_TMPOUT//'/ / /tet ; finish')
      call poi_cmd('copypts/'//POI_MO_TMPOUT//'/'//
     &     mo_poi_pts_out(1:ilen)//' ; finish')
      call poi_cmd('cmo/delete/'//mo_poi_pts_out(1:ilen)//' ; finish')
      call poi_cmd('cmo/move/'//mo_poi_pts_out(1:ilen)//'/'//
     &     POI_MO_TMPOUT//' ; finish')
      call poi_cmd('cmo/delete/'//POI_MO_TMPHEXOUT//'; finish')
      call poi_cmd('cmo / select /'//mo_poi_pts_out(1:ilen)//
     &     '; finish')
C
      return
      end
C
C SUBROUTINE BEGIN ####################################################
      subroutine poi_finish_output(ndim,mo_poi_poly,mo_poi_pts_out,
     &           if_connect,if_convex)
C #####################################################################
C
C     PURPOSE -
C
C     Common tail of both drivers: sort the vertices, optionally
C     connect them into a Delaunay mesh, sort the cells, and leave the
C     result as the current mesh object.
C
C     The ONLY difference between the 2D and 3D tails is that a 2D
C     polygon may need its outline recovered after connect. A 3D box
C     is convex, so it never does.
C
C     INPUT ARGUMENTS -
C        ndim         - 2 or 3
C        mo_poi_poly  - polygon mesh object, 2D only, may be blank
C        if_connect   - 1 to triangulate/tetrahedralize
C        if_convex    - 2D only; .true. skips the clipping pass
C
C #####################################################################
      implicit none
      include 'poi_params.h'
C
      integer ndim,if_connect
      character*32 mo_poi_poly,mo_poi_pts_out
      logical if_convex
C
      call poi_sort_reorder_nodes(mo_poi_pts_out)
C
      if(if_connect .eq. 1)then
         call poi_connect_delaunay(mo_poi_pts_out)
         if(ndim .eq. 2)then
            call poi_2d_recover_polygon(mo_poi_poly,mo_poi_pts_out,
     &           if_convex)
         endif
         call poi_sort_reorder_cells(mo_poi_pts_out)
      endif
C
      call poi_release_and_select(mo_poi_pts_out)
C
      return
      end
C
C SUBROUTINE BEGIN ####################################################
      subroutine poi_sort_reorder_nodes(mo_poi_pts_out)
C #####################################################################
C
C     PURPOSE - Sort and reorder vertices by x,y,z coordinate so the
C     output ordering is reproducible from run to run.
C
C #####################################################################
      implicit none
      character*32 mo_poi_pts_out
      integer ilen,icharlnf
C
C     Select explicitly. This routine runs immediately after
C     poisson_2d / poisson_3d, which are the C++ samplers, so "the
C     current mesh object" here depends on what code on the other side
C     of the language boundary happened to leave selected. The sort
C     below uses -def-, so without this it could sort the wrong object
C     and then reorder the output by a key that was never built on it.
C
      ilen = icharlnf(mo_poi_pts_out)
      call poi_cmd('cmo / select / '//mo_poi_pts_out(1:ilen)//
     &     ' ; finish')
C
      call poi_cmd('sort/-def-/index/ascending/ikeyv/xic yic zic'//
     &     ' ; finish')
      ilen = icharlnf(mo_poi_pts_out)
      call poi_cmd('reorder/'//mo_poi_pts_out(1:ilen)//
     &     '/ikeyv ; finish')
      call poi_cmd('cmo/DELATT/-def-/ikeyv ; finish ')
C
      return
      end
C
C SUBROUTINE BEGIN ####################################################
      subroutine poi_sort_reorder_cells(mo_poi_pts_out)
C #####################################################################
C
C     PURPOSE - Sort and reorder cells by the x,y,z coordinate of
C     their centroid, then remove the scratch attributes used to do
C     it.
C
C #####################################################################
      implicit none
      character*32 mo_poi_pts_out
      integer ilen,icharlnf
C
C     Select explicitly rather than relying on whatever is current.
C     createpts/median must run on the output mesh object; if it runs
C     on something with no elements then xmed/ymed/zmed are never
C     created and the sort below fails with
C         attribute type= RECON: Must be VINT or VDOUBLE
C
      ilen = icharlnf(mo_poi_pts_out)
      call poi_cmd('cmo / select / '//mo_poi_pts_out(1:ilen)//
     &     ' ; finish')
C
      call poi_cmd('createpts / median ; finish ')
      call poi_cmd('sort/-def-/index/ascending/ikeyc/xmed ymed zmed'//
     &     ' ; finish')
      ilen = icharlnf(mo_poi_pts_out)
      call poi_cmd('reorder / '//mo_poi_pts_out(1:ilen)//
     &     ' / ikeyc; finish')
C
      call poi_cmd('cmo/DELATT/-def-/ikeyc ; finish ')
      call poi_cmd('cmo/DELATT/-def-/xmed ; finish ')
      call poi_cmd('cmo/DELATT/-def-/ymed ; finish ')
      call poi_cmd('cmo/DELATT/-def-/zmed ; finish ')
C
      return
      end
C
C SUBROUTINE BEGIN ####################################################
      subroutine poi_connect_delaunay(mo_poi_pts_out)
C #####################################################################
C
C     PURPOSE - Connect the point distribution as a Delaunay mesh.
C     Produces triangles or tetrahedra according to the mesh object
C     type, so both drivers use this unchanged.
C
C #####################################################################
      implicit none
      character*32 mo_poi_pts_out
      integer ilen,icharlnf
C
      ilen = icharlnf(mo_poi_pts_out)
      call poi_cmd('cmo / select / '//mo_poi_pts_out(1:ilen)//
     &     ' ; finish')
      call poi_cmd('cmo / setatt / -def- / imt / 1 0 0 / 1 ; finish ')
      call poi_cmd('connect ; finish ')
      call poi_cmd('quality ; finish ')
C
      return
      end
C
C SUBROUTINE BEGIN ####################################################
      subroutine poi_2d_recover_polygon(mo_poi_poly,mo_poi_pts_out,
     &           if_convex)
C #####################################################################
C
C     PURPOSE -
C
C     2D only. After connect, apply two rounds of Laplace smoothing
C     and Lawson flipping to recover the Delaunay triangulation. The
C     final 'recon 1' ensures no angle greater than 90 degrees faces
C     an exterior boundary.
C
C     A nonconvex polygon must be clipped first: connect produces the
C     convex hull, so the triangles filling the concave bites have to
C     be removed before smoothing, or smoothing would pull vertices
C     into regions that are not part of the polygon.
C
C #####################################################################
      implicit none
      character*32 mo_poi_poly,mo_poi_pts_out
      logical if_convex
C
      integer ilen,icharlnf
C
C     Select on entry as well as before the smoothing below. The
C     commands that follow all act on the current mesh object.
C
      ilen = icharlnf(mo_poi_pts_out)
      call poi_cmd('cmo / select / '//mo_poi_pts_out(1:ilen)//
     &     ' ; finish')
C
      call poi_cmd('rmpoint / compress ; finish ')
      call poi_cmd('resetpts / itp ; finish ')
      call poi_cmd('assign///maxiter_sm/ 1 ; finish ')
C
      if(.not. if_convex)then
         call poi_2d_clip_nonconvex(mo_poi_poly,mo_poi_pts_out)
      endif
C
C     RE-SELECT THE OUTPUT MESH OBJECT BEFORE SMOOTHING.
C
C     poi_2d_clip_nonconvex finishes by deleting its temporary
C     triangulation mesh object, and cmo/delete leaves some OTHER mesh
C     object current - in practice the input polygon. Anything issued
C     after it without an explicit select operates on the wrong object.
C
C     The original code avoided this only by accident of ordering: it
C     ran smooth/recon BEFORE the delete. When the clip was factored
C     into its own routine and the common smooth/recon pair was hoisted
C     out of the convex and nonconvex branches, the pair moved after
C     the delete and silently began smoothing the 6 vertex polygon
C     instead of the 202 vertex Poisson mesh, producing
C         SMOOTH: nodes in point set = 6
C         Error from: mega3d  get_info: 0 elems
C     and a cascade of MMGETBLK failures. The mesh still came out
C     correct because the clip had already finished, but the smoothing
C     and Delaunay recovery were not happening at all.
C
C     Selecting explicitly is better than restoring the old ordering,
C     because it does not depend on which object cmo/delete happens to
C     leave current.
C
      ilen = icharlnf(mo_poi_pts_out)
      call poi_cmd('cmo / select / '//mo_poi_pts_out(1:ilen)//
     &     ' ; finish')
C
      call poi_cmd('smooth;recon 0 ; finish ')
      call poi_cmd('smooth;recon 1 ; finish ')
C
      return
      end
C
C SUBROUTINE BEGIN ####################################################
      subroutine poi_2d_clip_nonconvex(mo_poi_poly,mo_poi_pts_out)
C #####################################################################
C
C     PURPOSE -
C
C     2D only, nonconvex polygon. Build a counterclockwise
C     triangulation of the polygon outline, mark it with a scratch
C     attribute, interpolate that attribute onto the convex hull
C     Delaunay mesh built from the Poisson points, and remove every
C     element that falls outside the true outline.
C
C     Elements inside the polygon pick up the marked value; those
C     outside keep the interpolation default, which is larger, so the
C     eltset test is "greater than 1".
C
C #####################################################################
      implicit none
      include 'poi_params.h'
C
      character*32 mo_poi_poly,mo_poi_pts_out
      integer ilen,ilenp,icharlnf
C
      ilenp = icharlnf(mo_poi_poly)
      ilen  = icharlnf(mo_poi_pts_out)
C
C     Triangulate the polygon outline on its own.
C
      call poi_cmd('cmo/create/'//POI_MO_POLYTRI//'///tri ; finish')
      call poi_cmd('copypts/'//POI_MO_POLYTRI//'/'//
     &     mo_poi_poly(1:ilenp)//';finish')
      call poi_cmd('cmo/select/'//POI_MO_POLYTRI//';finish')
      call poi_cmd('triangulate / counterclockwise;finish')
      call poi_cmd('cmo/addatt/'//POI_MO_POLYTRI//'/'//
     &     POI_ATT_CLIP//'/vint/scalar/nelements; finish')
C
C     Mark it, and give the Poisson mesh somewhere to receive the mark.
C
      call poi_cmd('cmo/addatt/'//mo_poi_pts_out(1:ilen)//'/'//
     &     POI_ATT_CLIP//'/vint/scalar/nelements; finish')
      call poi_cmd('cmo/setatt/'//POI_MO_POLYTRI//'/'//
     &     POI_ATT_CLIP//'/ 1 0 0 / 1;finish')
      call poi_cmd('interpolate/map/'//mo_poi_pts_out(1:ilen)//'/'//
     &     POI_ATT_CLIP//'/1,0,0/'//POI_MO_POLYTRI//'/'//
     &     POI_ATT_CLIP//' ; finish')
C
C     Delete everything that did not land inside the outline.
C
      call poi_cmd('cmo / select / '//mo_poi_pts_out(1:ilen)//
     &     ';finish')
      call poi_cmd('eltset/e_delete/'//POI_ATT_CLIP//
     &     '/gt/ 1 ; finish')
      call poi_cmd('rmpoint/element/eltset get e_delete ; finish')
      call poi_cmd('resetpts/itp ; finish')
C
C     Clean up.
C
      call poi_cmd('cmo/delete/'//POI_MO_POLYTRI//'; finish')
      call poi_cmd('cmo/DELATT/'//mo_poi_pts_out(1:ilen)//'/'//
     &     POI_ATT_CLIP//'; finish')
C
      return
      end
C
C SUBROUTINE BEGIN ####################################################
      subroutine poi_release_and_select(mo_poi_pts_out)
C #####################################################################
C
C     PURPOSE - Release the lookup mesh object and make the Poisson
C     points/mesh the current mesh object. Shared tail of both
C     drivers.
C
C #####################################################################
      implicit none
      include 'poi_params.h'
      character*32 mo_poi_pts_out
      integer ilen,icharlnf
C
      call poi_cmd('cmo/release/'//POI_MO_HFIELD//' ; finish')
      ilen = icharlnf(mo_poi_pts_out)
      call poi_cmd('cmo / select / '//mo_poi_pts_out(1:ilen)//
     &     ' ; finish')
      call poi_cmd('quality ; finish ')
C
      return
      end
C
C
C =====================================================================
C =====================================================================
C     SECTION 7 - PURE GEOMETRY UTILITIES
C
C     No I/O, no COMMON, no error handling. Given numbers, return
C     numbers. These are the only routines in the file that can be
C     unit tested without a live LaGriT mesh object, and they should
C     stay that way: policy decisions about what is "too small" or
C     "too sharp" belong in SECTION 4, not here.
C =====================================================================
C =====================================================================
C
C SUBROUTINE BEGIN ####################################################
      subroutine poi_copy_box(box_out,box_in)
C #####################################################################
C     PURPOSE - Copy a bounding box.
C #####################################################################
      implicit none
      include 'poi_params.h'
      real*8 box_out(NBOX),box_in(NBOX)
      integer i
C
      do i = 1, NBOX
         box_out(i) = box_in(i)
      enddo
C
      return
      end
C
C SUBROUTINE BEGIN ####################################################
      subroutine poi_buffer_box(ijob,buffer_factor,box_out,box_in)
C #####################################################################
C
C     PURPOSE -
C
C     Grow or shrink a bounding box on all three axes at once.
C
C     A NEGATIVE buffer_factor with ijob=1 SHRINKS the box, which is
C     how the 3D edge buffer works. That is not obvious from the name,
C     so it is stated here.
C
C     INPUT ARGUMENTS -
C        ijob = 1  add/subtract buffer_factor from each limit
C             = 2  add/subtract a fraction of each extent
C
C #####################################################################
      implicit none
      include 'poi_params.h'
      integer ijob
      real*8  buffer_factor
      real*8  box_out(NBOX),box_in(NBOX)
C
      call buffer_minmax_range(ijob,buffer_factor,
     &     box_out(IBX_XMIN),box_out(IBX_XMAX),
     &     box_in(IBX_XMIN), box_in(IBX_XMAX))
      call buffer_minmax_range(ijob,buffer_factor,
     &     box_out(IBX_YMIN),box_out(IBX_YMAX),
     &     box_in(IBX_YMIN), box_in(IBX_YMAX))
      call buffer_minmax_range(ijob,buffer_factor,
     &     box_out(IBX_ZMIN),box_out(IBX_ZMAX),
     &     box_in(IBX_ZMIN), box_in(IBX_ZMAX))
C
      return
      end
C
C SUBROUTINE BEGIN ####################################################
      subroutine poi_lattice_size(box,h,np)
C #####################################################################
C
C     PURPOSE -
C
C     Size the background search lattice for a box at spacing h.
C
C     The count along each axis is ceiling(extent/h) refined by
C     POI_LATTICE_FUDGE. Oversampling the lookup grid relative to the
C     target spacing keeps the neighbor search cheap without missing
C     candidate points.
C
C     This calculation appeared six times across the two drivers, with
C     the 1.5 written as a bare literal each time.
C
C     NOTE: the multiply is a real expression assigned to an integer,
C     so it truncates. That is the original behavior and is kept.
C
C #####################################################################
      implicit none
      include 'poi_params.h'
      real*8  box(NBOX),h
      integer np(NDIR)
C
      np(1) = ceiling((box(IBX_XMAX)-box(IBX_XMIN))/h)
      np(2) = ceiling((box(IBX_YMAX)-box(IBX_YMIN))/h)
      np(3) = ceiling((box(IBX_ZMAX)-box(IBX_ZMIN))/h)
C
      np(1) = POI_LATTICE_FUDGE*np(1)
      np(2) = POI_LATTICE_FUDGE*np(2)
      np(3) = POI_LATTICE_FUDGE*np(3)
C
C     A degenerate axis would give zero and then divide by zero when
C     the lookup spacing is computed, so floor every count at 1.
C
      if(np(1) .lt. 1) np(1) = 1
      if(np(2) .lt. 1) np(2) = 1
      if(np(3) .lt. 1) np(3) = 1
C
      return
      end
C
C SUBROUTINE BEGIN ####################################################
      subroutine buffer_minmax_range(ijob,buffer_factor,
     &                       xmin_buff,xmax_buff,xmin,xmax)
C #####################################################################
C
C     PURPOSE -
C
C        Take as input a min/max range and create a new range that is
C        either a constant amount larger/smaller (ijob=1) or a scale
C        factor of the original range larger/smaller (ijob=2).
C
C     INPUT ARGUMENTS -
C
C        ijob = 1, add/subtract buffer_factor to the range limits
C             = 2, add/subtract a scale factor,
C                  e.g. xmin_buff = xmin - ((xmax-xmin)*buffer_factor)
C        buffer_factor - value added/subtracted (ijob=1) or scale
C                        factor (ijob=2)
C        xmin,xmax - input range
C
C     OUTPUT ARGUMENTS -
C
C        xmin_buff,xmax_buff - output modified min/max
C
C     (Behavior unchanged from the original.)
C
C #####################################################################
      implicit none
      integer ijob
      real*8 xmin,xmax,
     &       xmin_buff,xmax_buff,
     &       buffer_factor
C
      if(ijob .eq. 1)then
C ----------------------------------------------------------
C  Fixed value added to or subtracted from the min/max values
C ----------------------------------------------------------
         xmin_buff = xmin - buffer_factor
         xmax_buff = xmax + buffer_factor
      elseif(ijob .eq. 2)then
C ----------------------------------------------------------
C  Buffer based on a proportion of the range size
C ----------------------------------------------------------
         xmin_buff = xmin - ((xmax - xmin)*buffer_factor)
         xmax_buff = xmax + ((xmax - xmin)*buffer_factor)
      endif
C
      return
      end
C
C SUBROUTINE BEGIN ####################################################
      subroutine get_minimum_edge_length(nnode,x,y,h_min)
C #####################################################################
C
C     PURPOSE - Compute the minimum edge length of a closed polygon
C     point set.
C
C     FIX: the loop ran i = 1, nnode and evaluated x(i+1), so at
C     i = nnode it read x(nnode+1) and y(nnode+1), one element past
C     the end of the arrays. It also meant the closing edge was
C     counted twice, since the explicit wrap around calculation below
C     already handles it. The loop now stops at nnode-1.
C
C     INPUT ARGUMENTS -
C        nnode = number of vertices. The last vertex implicitly
C                connects back to the first.
C        x,y   = x,y coordinates of vertices
C
C     OUTPUT ARGUMENTS -
C        h_min = minimum edge length in the polygon
C
C #####################################################################
      implicit none
      integer nnode,i
      real*8 x(*),y(*)
      real*8 h_min,dx
C
      h_min = 1.0d20
      do i = 1, nnode-1
         dx = ((x(i+1)-x(i))**2 + (y(i+1)-y(i))**2)**0.5d0
         h_min = min(h_min,dx)
      enddo
C
C     Close the loop, last point to first point.
C
      dx = ((x(1)-x(nnode))**2 + (y(1)-y(nnode))**2)**0.5d0
      h_min = min(h_min,dx)
C
      return
      end
C
C SUBROUTINE BEGIN ####################################################
      subroutine test_convex(nnode,x,y,if_convex)
C #####################################################################
C
C     PURPOSE - Determine if a polygon is convex or nonconvex by
C     computing the cross product of each pair of neighboring edge
C     vectors. If the sign changes anywhere, the polygon is nonconvex.
C
C     FIX - INCOMPLETE CORNER COVERAGE.
C
C     The loop used to run
C        do i = 1, nnode-2
C     and each pass tests the turn AT vertex i+1, so only vertices 2
C     through nnode-1 were ever examined. The turns at vertex 1 and at
C     vertex nnode were never tested at all.
C
C     A polygon whose only reflex corner happens to sit at the first
C     or last vertex was therefore reported CONVEX. The driver then
C     skipped both the nonconvex warning and the clipping pass, so the
C     convex hull triangulation was kept and the mesh quietly filled
C     in the concave notch. Rotating the vertex list of such a polygon
C     changed the answer.
C
C     The original also carried
C        if(i .eq. nnode)then
C           i_p1 = 1
C           i_p2 = 2
C        endif
C     which looks like a half finished attempt at exactly these wrap
C     around cases; it could never execute under that loop bound.
C
C     The loop now runs over all nnode vertices and wraps with mod, so
C     every corner is tested once. A convex polygon is unaffected. A
C     polygon that was previously misclassified will now correctly be
C     reported nonconvex and will be clipped, which CHANGES RESULTS
C     for those cases - correctly, but visibly.
C
C     INPUT ARGUMENTS -
C        nnode = number of vertices. The last vertex implicitly
C                connects back to the first.
C        x,y   = x,y coordinates of vertices
C
C     OUTPUT ARGUMENTS -
C        if_convex = .true. if convex, .false. if nonconvex
C
C #####################################################################
      implicit none
      integer nnode,i,i_p1,i_p2,num_positive,num_negative
      real*8 x(*),y(*)
      real*8 dx1,dy1,dx2,dy2,zcrossproduct
      logical if_convex
C
      num_positive = 0
      num_negative = 0
C
C     Pass i tests the turn at vertex i_p1. Running i over 1..nnode
C     makes i_p1 take every vertex index exactly once.
C
      do i = 1, nnode
         i_p1 = mod(i,nnode) + 1
         i_p2 = mod(i+1,nnode) + 1
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
C
      if((num_positive .gt. 0) .and. (num_negative .gt. 0))then
         if_convex = .false.
      else
         if_convex = .true.
      endif
C
      return
      end
C
C SUBROUTINE BEGIN ####################################################
      subroutine get_min_max_angle
     &          (nnode,x,y,if_rad_deg,ang_min,ang_max)
C #####################################################################
C
C     PURPOSE - Compute the min and max interior angle of the input
C     polygon.
C
C     Angle between two vectors in 2D: theta = acos[(a.b)/(|a||b|)]
C
C     CLEANUP: the same seven line angle calculation appeared three
C     times, once for the i=1 wrap case, once in the interior loop and
C     once for the i=nnode wrap case. It is now the vertex_angle
C     function below, called three times.
C
C     INPUT ARGUMENTS -
C        nnode = number of vertices. The last vertex implicitly
C                connects back to the first.
C        x,y   = x,y coordinates of vertices
C        if_rad_deg = 1 for radians, 2 for degrees
C
C     OUTPUT ARGUMENTS -
C        ang_min,ang_max = minimum/maximum interior angle
C
C #####################################################################
      implicit none
      integer nnode,i,i_minus_1,i_plus_1,if_rad_deg
      real*8 x(*),y(*)
      real*8 pi,ang_min,ang_max,ang_r
      real*8 vertex_angle
C
      ang_min =  1.d20
      ang_max = -1.d20
      pi = 4.0d0*atan(1.d0)
C
C     i = 1 case: the previous vertex wraps to nnode.
C
      ang_r = vertex_angle(x,y,nnode,1,2)
      ang_min = min(ang_min,ang_r)
      ang_max = max(ang_max,ang_r)
C
C     Interior vertices.
C
      do i = 2, nnode-1
         i_minus_1 = i-1
         i_plus_1  = i+1
         ang_r = vertex_angle(x,y,i_minus_1,i,i_plus_1)
         ang_min = min(ang_min,ang_r)
         ang_max = max(ang_max,ang_r)
      enddo
C
C     i = nnode case: the next vertex wraps to 1.
C
      ang_r = vertex_angle(x,y,nnode-1,nnode,1)
      ang_min = min(ang_min,ang_r)
      ang_max = max(ang_max,ang_r)
C
      if(if_rad_deg .eq. 2)then
         ang_min = (ang_min/pi)*180.0d0
         ang_max = (ang_max/pi)*180.0d0
      endif
C
      return
      end
C
C FUNCTION BEGIN ######################################################
      real*8 function vertex_angle(x,y,i_prev,i_here,i_next)
C #####################################################################
C
C     PURPOSE - Interior angle in radians at vertex i_here, between
C     the vectors running to its neighbors i_prev and i_next.
C
C #####################################################################
      implicit none
      integer i_prev,i_here,i_next
      real*8 x(*),y(*)
      real*8 v1_x,v1_y,v2_x,v2_y,v1dotv2,v1mag,v2mag
C
      v1_x = x(i_prev)-x(i_here)
      v1_y = y(i_prev)-y(i_here)
      v2_x = x(i_next)-x(i_here)
      v2_y = y(i_next)-y(i_here)
      v1dotv2 = (v1_x*v2_x) + (v1_y*v2_y)
      v1mag = sqrt(v1_x**2 + v1_y**2)
      v2mag = sqrt(v2_x**2 + v2_y**2)
      vertex_angle = acos(v1dotv2/(v1mag*v2mag))
C
      return
      end
