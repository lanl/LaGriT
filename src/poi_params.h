C #####################################################################
C
C     poi_params.h
C
C     Shared parameters for the Poisson disk drivers in poi_driver.f.
C
C     USAGE - place immediately after the IMPLICIT NONE statement:
C
C        implicit none
C        include 'poi_params.h'
C
C     This is a Fortran INCLUDE (not a C preprocessor #include), so it
C     works in plain .f fixed form source with no -cpp flag. gfortran
C     searches the directory of the including source file, so this
C     file belongs next to poi_driver.f in src/. No CMakeLists change
C     is needed: include files are not compiled as sources.
C
C     Everything here is a POLICY value. The point of collecting them
C     is that a reviewer can see every tunable threshold, every
C     hardwired mesh object name and every magic factor on one screen,
C     instead of hunting for literals buried in twenty subroutines.
C
C #####################################################################
C
C ---------------------------------------------------------------------
C     Severity codes for poi_log_block.
C
C     Severity is an explicit argument rather than something implied
C     by the text of the message, so the banner wording is decided in
C     exactly one place and callers cannot drift apart.
C ---------------------------------------------------------------------
      integer POI_SEV_ERROR,POI_SEV_WARN
      parameter (POI_SEV_ERROR = 1)
      parameter (POI_SEV_WARN  = 2)
C
C ---------------------------------------------------------------------
C     Maximum length of one logged message line.
C
C     Messages longer than this are silently truncated on assignment,
C     which is standard Fortran behavior. 132 matches the width these
C     log files have traditionally been read at.
C ---------------------------------------------------------------------
      integer POI_MSG_LEN
      parameter (POI_MSG_LEN = 132)
C
C ---------------------------------------------------------------------
C     Display width of one logged line.
C
C     writloga passes the message straight through to writlogb, which
C     does the formatting with
C
C         lenmes = icharlnb(imessage)        ! TRAILING trim
C         length = (lenmes-1)/80 + 1
C         if(length .le. 1) write(unit,'(a80)') imessage
C         else  write each 80 column slice in turn
C
C     Two consequences follow, and the first one is the reason every
C     message in this file is written into a POI_MSG_LEN buffer and
C     passed at FULL LENGTH rather than trimmed to its text:
C
C     1) format(a80) with an actual argument SHORTER than 80 right
C        justifies it, padding on the left. Passing a trimmed five
C        character string puts it against the right margin. Passing a
C        132 character buffer prints its leftmost 80 and reads
C        normally from the left.
C
C     2) Text past column 80 is NOT lost. writlogb wraps it onto
C        further 80 column lines. Only the continuation slices are
C        short, so a wrapped tail is right justified and looks odd.
C
C     So POI_LINE_WIDTH is a STYLE limit, not a hard one. Messages
C     longer than this still appear in full, just awkwardly. Keeping
C     text within POI_LINE_WIDTH - 2 = 78 (poi_log_block indents
C     message lines by two spaces) avoids the ragged wrap.
C
C     Note also that icharlnb is a trailing trim, which is what makes
C     the padded buffer safe: a 132 character buffer holding 50
C     characters of text yields lenmes = 50 and takes the single line
C     branch, so no spurious wrapping occurs.
C
C     Do NOT confuse icharlnb with icharlnf. See poi_lentrim.
C ---------------------------------------------------------------------
      integer POI_LINE_WIDTH,POI_TEXT_WIDTH
      parameter (POI_LINE_WIDTH = 80)
      parameter (POI_TEXT_WIDTH = 78)
C
C ---------------------------------------------------------------------
C     Mesh object names.
C
C     These were previously BOTH a local character variable AND a
C     hardwired literal inside nine different LaGriT command strings.
C     Changing the variable left the command strings pointing at the
C     old name while the C++ sampler received the new one. Naming them
C     once here makes that class of mistake impossible.
C
C     CHARACTER*(*) named constants take their length from the
C     constant, so there is no trailing blank padding to trim.
C ---------------------------------------------------------------------
C
C     ***  DO NOT CHANGE THE VALUES OF THE NEXT TWO CONSTANTS.  ***
C
C     POI_MO_HFIELD and POI_ATT_HFIELD are PUBLIC API, not internal
C     names. A user supplied variable resolution control file assigns
C     the h field by referring to them literally. For example
C     test_user_resolution.mlgi contains
C
C        compute / distance_field / mo_poi_h_field / mo_dfield / dfield
C        math / floor / mo_poi_h_field / h_field_att / 1 0 0 / &
C                       mo_poi_h_field / h_field_att / H_FLOOR
C
C     Renaming either constant silently breaks every existing user
C     .mlgi deck: the user's commands would operate on a mesh object
C     that no longer exists, while the sampler read an h field nobody
C     had populated. The failure would look like a bad point
C     distribution, not like a name error.
C
C     They are collected here so the driver cannot drift out of step
C     with itself, NOT because they are free to change.
C ---------------------------------------------------------------------
      character*(*) POI_MO_HFIELD
      parameter (POI_MO_HFIELD = 'mo_poi_h_field')
C
      character*(*) POI_ATT_HFIELD
      parameter (POI_ATT_HFIELD = 'h_field_att')
C
C     Temporary mesh objects, created and deleted within one command.
C
      character*(*) POI_MO_POLYTRI
      parameter (POI_MO_POLYTRI = 'mo_poi_poly_triangulate')
C
      character*(*) POI_MO_TMPHEX
      parameter (POI_MO_TMPHEX = 'mo_tmp_wrk_poi_hex')
C
      character*(*) POI_MO_TMPHEXOUT
      parameter (POI_MO_TMPHEXOUT = 'mo_tmp_wrk_poi_hex_outside')
C
      character*(*) POI_MO_TMPOUT
      parameter (POI_MO_TMPOUT = 'mo_poi_tmp_poi_pts_out')
C
C     Scratch attribute used to mark elements for the nonconvex clip.
C
      character*(*) POI_ATT_CLIP
      parameter (POI_ATT_CLIP = 'iatt_poi_tmp')
C
C ---------------------------------------------------------------------
C     Bounding box array layout.
C
C     A box is a real*8 array of length NBOX. Passing one array
C     instead of six scalars turns poi_buffer_box from fourteen
C     arguments into four, and makes it impossible to transpose a
C     ymin and a zmin at a call site.
C
C     ALL box arrays in this file use this order. Do not introduce a
C     second convention.
C ---------------------------------------------------------------------
      integer IBX_XMIN,IBX_XMAX,IBX_YMIN,IBX_YMAX,IBX_ZMIN,IBX_ZMAX
      integer NBOX
      parameter (IBX_XMIN = 1)
      parameter (IBX_XMAX = 2)
      parameter (IBX_YMIN = 3)
      parameter (IBX_YMAX = 4)
      parameter (IBX_ZMIN = 5)
      parameter (IBX_ZMAX = 6)
      parameter (NBOX     = 6)
C
C ---------------------------------------------------------------------
C     Lattice array layout. np(1),np(2),np(3) = counts along x,y,z.
C ---------------------------------------------------------------------
      integer NDIR
      parameter (NDIR = 3)
C
C ---------------------------------------------------------------------
C     Quality and tolerance thresholds.
C
C     POI_ANGLE_MIN_DEG
C        Reject a polygon whose smallest interior angle is below this.
C        Very sharp corners cannot be filled cleanly by disk sampling.
C
C     POI_EDGE_MIN_FACTOR
C        Reject a polygon whose shortest edge is below h times this.
C        An edge shorter than half the target spacing cannot carry a
C        sample point.
C
C     POI_HFIELD_MIN_FACTOR
C        Reject a user h_field_att below h times this. Slightly under
C        0.5 so that a field deliberately set to exactly 0.5*h is
C        accepted despite floating point representation error.
C
C     POI_HFIELD_WARN_FACTOR
C        Warn (do not reject) when a user h_field_att exceeds h times
C        this. A field this much larger than the target spacing is
C        usually a units mistake.
C
C     POI_PLANAR_TOL
C        Relative tolerance for the 2D polygon planarity test.
C
C     POI_LATTICE_FUDGE
C        The background search lattice is made this much finer than
C        ceiling(extent/h) in each direction.
C
C        *** THIS VALUE WAS DETERMINED BY TESTING. IT IS NOT AN
C        *** ARBITRARY ROUND NUMBER, AND IT IS NOT A SAFETY MARGIN
C        *** THAT CAN SIMPLY BE TRIMMED.
C
C        It is tempting to lower it to 1.0 to save memory in the
C        lookup mesh, since the lattice is allocated as
C        np_x*np_y(*np_z) nodes and the cost grows as the square in 2D
C        and the cube in 3D. Do not do that without re-running the
C        Poisson disk test suite and checking point distribution
C        quality: the lattice size affects what the sampler finds, not
C        just how fast it runs.
C ---------------------------------------------------------------------
      real*8 POI_ANGLE_MIN_DEG
      parameter (POI_ANGLE_MIN_DEG = 9.0d0)
C
      real*8 POI_EDGE_MIN_FACTOR
      parameter (POI_EDGE_MIN_FACTOR = 0.5d0)
C
      real*8 POI_HFIELD_MIN_FACTOR
      parameter (POI_HFIELD_MIN_FACTOR = 0.49d0)
C
      real*8 POI_HFIELD_WARN_FACTOR
      parameter (POI_HFIELD_WARN_FACTOR = 100.0d0)
C
      real*8 POI_PLANAR_TOL
      parameter (POI_PLANAR_TOL = 1.0d-9)
C
      real*8 POI_LATTICE_FUDGE
      parameter (POI_LATTICE_FUDGE = 1.5d0)
C
C ---------------------------------------------------------------------
C     Defaults for the optional command line parameters.
C ---------------------------------------------------------------------
      integer POI_DEF_SEED,POI_DEF_SAMPLES,POI_DEF_SWEEPS
      parameter (POI_DEF_SEED    = 1)
      parameter (POI_DEF_SAMPLES = 10)
      parameter (POI_DEF_SWEEPS  = 1)
C
C     Default edge buffer behavior when the user does not define
C     IF_POI_EDGE_BUFFER / POI_EDGE_BUFFER.
C
      integer POI_DEF_IFEDGEBUF
      parameter (POI_DEF_IFEDGEBUF = 1)
      real*8 POI_DEF_EDGEBUF
      parameter (POI_DEF_EDGEBUF = 1.0d0)
C
C ---------------------------------------------------------------------
C     Parser token type codes as returned by the LaGriT parser in
C     msgtyp. Named so that call sites read as intent rather than as
C     bare integers.
C ---------------------------------------------------------------------
C ---------------------------------------------------------------------
C     Token numbering offset for user facing messages.
C
C     createpts_lg.f dispatches into these drivers with
C
C         call poisson_disk(imsgin(2),xmsgin(2),cmsgin(2),
C      *                    msgtype(2),nwds-1,ierr)
C
C     so the array the driver sees STARTS AT THE USER'S SECOND TOKEN.
C     What the driver indexes as token 2 is what the user typed as the
C     third slash separated field:
C
C         createpts / poisson_disk / 3d_box / mo_out / ...
C              1            2           3        4
C         driver:          1           2        3
C
C     The original code knew about this and said so in its comments
C     ("Command Argument 3 (2 since passed from createpts)") but the
C     messages themselves never corrected for it.
C
C     Reporting the internal index defeats the entire purpose of
C     naming a position, because the user counts slashes in what they
C     typed. Every message that prints a token number therefore adds
C     POI_TOKEN_OFFSET.
C
C     If a caller ever invokes these drivers with the FULL token array
C     rather than from createpts, this offset becomes wrong. There is
C     no way to detect that from inside the driver, so it is stated
C     here as an assumption of the calling convention.
C ---------------------------------------------------------------------
      integer POI_TOKEN_OFFSET
      parameter (POI_TOKEN_OFFSET = 1)
C
C ---------------------------------------------------------------------
C     Parser token type codes.
C ---------------------------------------------------------------------
      integer POI_TYPE_INT,POI_TYPE_REAL,POI_TYPE_CHAR
      parameter (POI_TYPE_INT  = 1)
      parameter (POI_TYPE_REAL = 2)
      parameter (POI_TYPE_CHAR = 3)
C
C     END poi_params.h
