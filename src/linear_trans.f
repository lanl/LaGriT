      subroutine linear_transform
     &   (imsgin,xmsgin,cmsgin,msgtype,nwds,ierror)
C
C######################################################################
C
C     PURPOSE -
C
C      Given a 3D mesh and a (geometrically) 2D surface, will
C      extrapolate some value from that surface onto every point
C      of the mesh. This can be used to (for example):
C      * Propogate head values down into a mesh.
C      * Expand a mesh to fit a surface, by propogating the appropriate
C        spatial coordinate.
C      * Whatever else you want.
C      
C     NOTES -
C
C      Syntax for this command:
C      compute / linear_transform / main_mesh / surface_mesh / [ direction / node attribute ]
C      old option was called linear_extrapolate
C      
C      "node attribute" must exist in both the surface and the main
C      meshes before beginning. If a node attribute is not
C      specified, the appropriate spatial attribute is selected
C      (depending on direction of propogation).
C
C      "direction" can be one of the following:
C          zpos (default), zneg, xpos, xneg, ypos, yneg.
C      
C      For example, if both "node attribute" and "direction" were
C      omitted, this routine would propogate zic (node z-coordinate)
C      from the surface into the mesh such that the bottom surface of
C      mesh remains unchanged, and the upper surface conforms to the 
C      shape of the surface. Selecting "zneg" would result in the bottom
C      of the mesh conforming to the surface instead. Similarly, "xneg"
C      would result in the "left-hand" side of the mesh conforming to
C      the surface. In the case of propogating a non-spatial attribute,
C      the attribute value at the "far end" of the mesh will remain
C      unchanged (0 if previously uninitialized) while the end of the
C      mesh specified by the direction attribute will match the surface
C      values exactly.
C        
C######################################################################
C     CHANGE HISTORY -
C
C       Developed by agable 6-7-2010
C
C
C######################################################################
C
      implicit none
C
C     Subroutine Input Variables
C
      integer       nwds, ierror
      character*(*) cmsgin(nwds)
      integer       imsgin(nwds), msgtype(nwds)
      real*8        xmsgin(nwds)
C
C     Name Variables and Message Variables
C
      character*32  isubname 
      character*32  main_mo
      character*32  surf_mo
      character*32  att
      character*4   direction
      character*132 logmess
      character*256 cmd
      pointer       (ipout, pout)
      integer       iout, pout, ilen, itype
      character*32  cout
      real*8        rout
      integer mlen ! length of name of mesh
      integer slen ! length of name of surface
      integer alen ! length of name of attribute
      integer       derive_flag
C
C     Variables for subroutine calls
C
      integer imtype
      character*4 mtype
C      
C     Function variable
C
      integer icharlnf
C
C######################################################################
C
      isubname = 'linear_transform'
C
C
C     Check input mesh objects
C
C     If we get this far, syntax of argument 1 and 2 is ok. Move on to
C     arguments 3, 4, 5, 6
C
      if(nwds < 4 .or. nwds > 6) then
         write(logmess,'(a)')
     &    'ERROR COMPUTE linear_transform: Wrong number of arguments.'
         call writloga('default',0,logmess,0,ierror)
         ierror = -2
         goto 9999
      endif
      if(msgtype(3) .ne. 3) then
         write(logmess,'(a,a)')
     &    'ERROR COMPUTE linear_transform: ',
     &    'Argument 3 must be of type character.'
         call writloga('default',0,logmess,0,ierror)
         ierror = -2
         goto 9999
      endif
      if(msgtype(4) .ne. 3) then
         write(logmess,'(a,a)')
     &    'ERROR COMPUTE linear_transform: ',
     &    ' Argument 4 must be of type character.'
         call writloga('default',0,logmess,0,ierror)
         ierror = -2
         goto 9999
      endif
      if (nwds .ge. 5) then
          if(msgtype(5) .ne. 3) then
             write(logmess,'(a,a)')
     &     'ERROR COMPUTE linear_transform: ',
     &      'Argument 5 must be of type character.'
             call writloga('default',0,logmess,0,ierror)
             ierror = -2
             goto 9999
          endif
      endif
      if (nwds .eq. 6) then
          if(msgtype(6) .ne. 3) then
             write(logmess,'(a,a)')
     &     'ERROR COMPUTE linear_transform: ',
     &     'Argument 6 must be of type character.'
             call writloga('default',0,logmess,0,ierror)
             ierror = -2
             goto 9999
          endif
      endif
          
C
      main_mo = cmsgin(3)
      mlen = icharlnf(main_mo)
      surf_mo = cmsgin(4)
      slen = icharlnf(surf_mo)
      if (nwds .ge. 5) then
          direction = cmsgin(5)(1:4)
          if (direction .ne. 'zpos' .and.
     &        direction .ne. 'zneg' .and.
     &        direction .ne. 'ypos' .and.
     &        direction .ne. 'yneg' .and.
     &        direction .ne. 'xpos' .and.
     &        direction .ne. 'xneg') then 
              write(logmess,'(a)') 'ERROR Invalid direction specified.'
              call writloga('default',0,logmess,0,ierror)
              ierror = -2
              goto 9999
          endif
      else
          direction = 'zpos'
      endif
      if (nwds .eq. 6) then
          att = cmsgin(6)
          alen = icharlnf(att)
      else
          att = direction(1:1) // 'ic'
          alen = 3
      endif
      print *, main_mo, surf_mo, att, direction
C
      call cmo_exist(main_mo,ierror)
      if(ierror.ne.0) then
         write(logmess,'(a)')
     &    'ERROR distance_field: main mesh object does not exist.'
         call writloga('default',0,logmess,0,ierror)
         ierror = -1
         go to 9999
      endif
      call cmo_exist(surf_mo,ierror)
      if(ierror.ne.0) then
         write(logmess,'(a)')
     &    'ERROR distance_field: surface mesh object does not exist.'
         call writloga('default',0,logmess,0,ierror)
         ierror = -1
         go to 9999
      endif

C
C     * Copy mesh and surface into manipulable copies
C     cmo/copy/main_mo_lin_ext_tmp/input_mesh
C     cmo/copy/surf_mo_lin_ext_tmp/input_surface
C
C     * Initialize node attributes 
C     cmo/addatt/main_mo_lin_ext_tmp/z_old
C     cmo/addatt/main_mo_lin_ext_tmp/z_new
C     cmo/addatt/main_mo_lin_ext_tmp/z_L0
C     cmo/addatt/main_mo_lin_ext_tmp/z_L1
C     cmo/addatt/main_mo_lin_ext_tmp/z_S
C     cmo/addatt/surf_mo_lin_ext_tmp/z_S
C
C     * Paint upper and lower surfaces of main mesh
C     extract/surfmesh/1,0,0/cmo_hull/main_mo_lin_ext_tmp/external
C     cmo/addatt/cmo_hull/area_normal/xyz/norm
C     cmo/addatt/cmo_hull/scalar/nx ny nz/norm
C     eltset/bot_els/nz/lt/0.0
C     eltset/top_els/nz/gt/0.0
C     pset/bot_pts/eltset/bot_els
C     pset/top_pts/eltset/top_els
C
C     * Save upper surface z-values and project it onto a 2-D plane
C     cmo/copy/cmo_top/cmo_hull
C     rmpoint/element/eltset,get,bot_els
C     cmo/addatt/cmo_top/z_L1
C     cmo/copyatt/cmo_top/cmo_top/z_L1/zic
C     cmo/setatt/cmo_top/zic/1,0,0/0
C
C     * Save lower surface z-values and project it onto a 2-D plane
C     cmo/copy/cmo_bot/cmo_hull
C     rmpoint/element/eltset,get,top_els
C     cmo/addatt/cmo_bot/z_L0
C     cmo/copyatt/cmo_bot/cmo_bot/z_L0/zic
C     cmo/setatt/cmo_bot/zic/1,0,0/0
C
C     * Save conforming surface z-values and project it onto 2-D plane
C     cmo/copyatt/surf_mo_lin_ext_tmp/surf_mo_lin_ext_tmp/z_S/zic
C     cmo/setatt/surf_mo_lin_ext_tmp/zic/1,0,0/0
C
C     * Finally, save old z-values of mesh and project it onto the 2-D plane
C     cmo/copyatt/main_mo_lin_ext_tmp/main_mo_lin_ext_tmp/z_old/zic
C     cmo/setatt/main_mo_lin_ext_tmp/zic/1,0,0/0
C
C     * Interpolate z_* values onto mesh
C     interpolate/continuous/main_mo_lin_ext_tmp,z_S/1,0,0/surf_mo_lin_ext_tmp,z_S
C     interpolate/continuous/main_mo_lin_ext_tmp,z_L1/1,0,0/cmo_top,z_L1
C     interpolate/continuous/main_mo_lin_ext_tmp,z_L0/1,0,0/cmo_bot,z_L0
C     cmo/copyatt/main_mo_lin_ext_tmp/main_mo_lin_ext_tmp/zic/z_old
C
C     * Calculate z_new from values painted onto mesh
C     math/subtract/main_mo_lin_ext_tmp/z_old/1,0,0/ & 
C           main_mo_lin_ext_tmp/z_old/main_mo_lin_ext_tmp/z_L0
C     math/subtract/main_mo_lin_ext_tmp/z_L1/1,0,0/ &
C           main_mo_lin_ext_tmp/z_L1/main_mo_lin_ext_tmp/z_L0
C     math/subtract/main_mo_lin_ext_tmp/z_S/1,0,0/ & 
C           main_mo_lin_ext_tmp/z_S/main_mo_lin_ext_tmp/z_L0
C     math/divide/main_mo_lin_ext_tmp/z_old/1,0,0/ &
C           main_mo_lin_ext_tmp/z_old/main_mo_lin_ext_tmp/z_L1
C     math/multiply/main_mo_lin_ext_tmp/z_old/1,0,0/ &
C           main_mo_lin_ext_tmp/z_S/main_mo_lin_ext_tmp/z_old
C     math/add/main_mo_lin_ext_tmp/z_new/1,0,0/ &
C           main_mo_lin_ext_tmp/z_old/main_mo_lin_ext_tmp/z_L0
C
C     * Finally, set z-value of orignal mesh to calculated z_new
C     cmo/copyatt/cmo/main_mo_lin_ext_tmp/zic/z_new
C
C     finish
C

      cmd = 'cmo/copy/main_mo_lin_ext_tmp/'
     &  // main_mo(1:mlen) // '; finish'
      call dotask(cmd,ierror)

      cmd = 'cmo/copy/surf_mo_lin_ext_tmp/'
     &  // surf_mo(1:slen) // '; finish'
      call dotask(cmd,ierror)

      cmd = 'cmo/addatt/main_mo_lin_ext_tmp/'
     &  // att(1:alen) // '_old; finish'
      call dotask(cmd,ierror)

      cmd = 'cmo/addatt/main_mo_lin_ext_tmp/'
     &  // att(1:alen) // '_new; finish'
      call dotask(cmd,ierror)

      cmd = 'cmo/addatt/main_mo_lin_ext_tmp/'
     &  // att(1:alen) // '_L0; finish'
      call dotask(cmd,ierror)

      cmd = 'cmo/addatt/main_mo_lin_ext_tmp/'
     &  // att(1:alen) // '_L1; finish'
      call dotask(cmd,ierror)

      cmd = 'cmo/addatt/main_mo_lin_ext_tmp/'
     &  // att(1:alen) // '_S; finish'
      call dotask(cmd,ierror)

      cmd = 'cmo/addatt/surf_mo_lin_ext_tmp/'
     &  // att(1:alen) // '_S; finish'
      call dotask(cmd,ierror)

      cmd = 'cmo/addatt/main_mo_lin_ext_tmp/' 
     &    // direction(1:1) // '_save; finish'
      call dotask(cmd,ierror)

      cmd = 'cmo/copyatt/main_mo_lin_ext_tmp/main_mo_lin_ext_tmp/'
     &    // direction(1:1) // '_save/' // direction(1:1) //'ic; finish'
      call dotask(cmd,ierror)

      call cmo_get_mesh_type('main_mo_lin_ext_tmp',mtype,imtype,ierror)

      if (mtype(1:4) .eq. 'quad' .or. mtype(1:3) .eq. 'tri') then 
          cmd = 'cmo/copy/hull_mo_lin_ext_tmp/main_mo_lin_ext_tmp;'
     &        // 'finish;'
          call dotask(cmd, ierror)
      else
          cmd = 'extract/surfmesh/1,0,0/hull_mo_lin_ext_tmp/'
     &      // 'main_mo_lin_ext_tmp/external; finish'
          call dotask(cmd, ierror)
      endif

C   Check for node attributes with the same names as those created by
C   dump / zone / outside (i.e. top, bottom, left_w, back_n, etc.)
      derive_flag = 0
      if (direction(1:1) .eq. 'z') then
          call cmo_get_attinfo('top','hull_mo_lin_ext_tmp',iout,rout,
     &                         cout,ipout,ilen,itype,ierror)
          if (ierror .ne. 0) then
              print *,"No user defined top found. Attempting to derive
     &                  top of mesh."
              derive_flag = 1
          endif
          call cmo_get_attinfo('bottom','hull_mo_lin_ext_tmp',iout,rout,
     &                         cout,ipout,ilen,itype,ierror)
          if (ierror .ne. 0) then
              print *,"No user defined base found. Attempting to derive
     &                  bottom of mesh."
              derive_flag = 1
          endif
      elseif (direction(1:1) .eq. 'y') then
          call cmo_get_attinfo('back_n','hull_mo_lin_ext_tmp',iout,rout,
     &                         cout,ipout,ilen,itype,ierror)
          if (ierror .ne. 0) then
              print *,"No user defined top found. Attempting to derive
     &                  top of mesh."
              derive_flag = 1
          endif
          call cmo_get_attinfo('front_s','hull_mo_lin_ext_tmp',iout,
     &                         rout,cout,ipout,ilen,itype,ierror)
          if (ierror .ne. 0) then
              print *,"No user defined base found. Attempting to derive
     &                  bottom of mesh."
              derive_flag = 1
          endif
      elseif (direction(1:1) .eq. 'x') then
          call cmo_get_attinfo('right_e','hull_mo_lin_ext_tmp',iout,
     &                         rout,cout,ipout,ilen,itype,ierror)
          if (ierror .ne. 0) then
              print *,"No user defined top found. Attempting to derive
     &                  top of mesh."
              derive_flag = 1
          endif
          call cmo_get_attinfo('left_w','hull_mo_lin_ext_tmp',iout,rout,
     &                         cout,ipout,ilen,itype,ierror)
          if (ierror .ne. 0) then
              print *,"No user defined base found. Attempting to derive
     &                  bottom of mesh."
              derive_flag = 1
          endif
      endif

      if (derive_flag .eq. 0) then
      
      if (direction(1:1) .eq. 'z') then
          if (direction(2:4) .eq. 'pos') then
              cmd = 'pset/top_pts/attribute/top/1,0,0/0.0/gt;finish'
              call dotask(cmd, ierror)

              cmd = 'pset/bot_pts/attribute/bottom/1,0,0/0.0/gt;finish'
              call dotask(cmd, ierror)
          elseif (direction(2:4) .eq. 'neg') then
              cmd = 'pset/top_pts/attribute/bottom/1,0,0/0.0/gt;finish'
              call dotask(cmd, ierror)

              cmd = 'pset/bot_pts/attribute/top/1,0,0/0.0/gt;finish'
              call dotask(cmd, ierror)
          endif
      elseif (direction(1:1) .eq. 'y') then
          if (direction(2:4) .eq. 'pos') then
              cmd = 'pset/top_pts/attribute/back_n/1,0,0/0.0/gt;finish'
              call dotask(cmd, ierror)

              cmd = 'pset/bot_pts/attribute/front_s/1,0,0/0.0/gt;finish'
              call dotask(cmd, ierror)
          elseif (direction(2:4) .eq. 'neg') then
              cmd = 'pset/top_pts/attribute/front_s/1,0,0/0.0/gt;finish'
              call dotask(cmd, ierror)

              cmd = 'pset/bot_pts/attribute/back_n/1,0,0/0.0/gt;finish'
              call dotask(cmd, ierror)
          endif
      elseif (direction(1:1) .eq. 'x') then
          if (direction(2:4) .eq. 'pos') then
              cmd = 'pset/top_pts/attribute/left_w/1,0,0/0.0/gt;finish'
              call dotask(cmd, ierror)

              cmd = 'pset/bot_pts/attribute/right_e/1,0,0/0.0/gt;finish'
              call dotask(cmd, ierror)
          elseif (direction(2:4) .eq. 'neg') then
              cmd = 'pset/top_pts/attribute/right_e/1,0,0/0.0/gt;finish'
              call dotask(cmd, ierror)

              cmd = 'pset/bot_pts/attribute/left_w/1,0,0/0.0/gt;finish'
              call dotask(cmd, ierror)
          endif
      endif

      cmd = 'cmo/copy/top_mo_lin_ext_tmp/hull_mo_lin_ext_tmp;finish'
      call dotask(cmd, ierror)

      cmd = 'pset/not_top/not/top_pts; finish'
      call dotask(cmd, ierror)

      cmd = 'cmo/setatt/top_mo_lin_ext_tmp/itp1/pset,get,not_top/21;'
     &        // 'finish'
      call dotask(cmd, ierror)

      cmd = 'rmpoint/compress; finish'
      call dotask(cmd, ierror)

      cmd = 'cmo/copy/bot_mo_lin_ext_tmp/hull_mo_lin_ext_tmp;finish'
      call dotask(cmd, ierror)

      cmd = 'pset/not_bot/not/bot_pts; finish'
      call dotask(cmd, ierror)

      cmd = 'cmo/setatt/bot_mo_lin_ext_tmp/itp1/pset,get,not_bot/21;'
     &        // 'finish'
      call dotask(cmd, ierror)

      cmd = 'rmpoint/compress; finish'
      call dotask(cmd, ierror)

      elseif (derive_flag .eq. 1) then
      
      call cmo_get_mesh_type('hull_mo_lin_ext_tmp',mtype,imtype,ierror)

      if (mtype(1:4) .eq. 'quad' .or. mtype(1:3) .eq. 'hex') then
          cmd = 'hextotet/4/hull_mo_lin_ext_tmp_tri/'
     &        // 'hull_mo_lin_ext_tmp; finish'
          call dotask(cmd, ierror)

          cmd = 'cmo/move/hull_mo_lin_ext_tmp/hull_mo_lin_ext_tmp_tri;'
     &        // ' finish'
          call dotask(cmd, ierror)
      elseif (mtype(1:4) .eq. 'line') then
          print *,"That mesh type (line) can't be used in this routine "
     &          //"without the user pre-defining a top and a bottom. "
     &          //"Please use dump/zone_outside or manually specify a "
     &          //"top and bottom using the same attribute names, so "
     &          //"this routine may function properly. See the online "
     &          //"documentation at lagrit.lanl.gov/docs/COMPUTE.html"
     &          //" for more details." 
          goto 9990
      endif

      cmd = 'cmo/addatt/hull_mo_lin_ext_tmp/area_normal/xyz/norm;'
     &  // 'finish' 
      call dotask(cmd,ierror)

      cmd = 'cmo/addatt/hull_mo_lin_ext_tmp/scalar/nx ny nz/norm;'
     &  // 'finish'
      call dotask(cmd,ierror)

      if (direction(2:4) .eq. 'pos') then
          cmd = 'eltset/bot_els/n' // direction(1:1) // '/le/0.0;finish'
          call dotask(cmd, ierror)

          cmd = 'eltset/top_els/n' // direction(1:1) // '/ge/0.0;finish'
          call dotask(cmd, ierror)
      elseif (direction(2:4) .eq. 'neg') then
          cmd = 'eltset/bot_els/n' // direction(1:1) // '/ge/0.0;finish'
          call dotask(cmd, ierror)

          cmd = 'eltset/top_els/n' // direction(1:1) // '/le/0.0;finish'
          call dotask(cmd, ierror)
      endif

      cmd = 'pset/bot_pts/eltset/bot_els; finish'
      call dotask(cmd, ierror)

      cmd = 'pset/top_pts/eltset/top_els; finish'
      call dotask(cmd, ierror)

C     cmd = 'dump/gmv/hull.gmv/hull_mo_lin_ext_tmp; finish'
C     call dotask(cmd, ierror)
      
      cmd = 'cmo/copy/top_mo_lin_ext_tmp/hull_mo_lin_ext_tmp; finish'
      call dotask(cmd, ierror)

      cmd = 'rmpoint/element/eltset,get,bot_els; finish'
      call dotask(cmd, ierror)

      cmd = 'cmo/copy/bot_mo_lin_ext_tmp/hull_mo_lin_ext_tmp; finish'
      call dotask(cmd, ierror)

      cmd = 'rmpoint/element/eltset,get,top_els; finish'
      call dotask(cmd, ierror)

      endif

      cmd = 'cmo/addatt/top_mo_lin_ext_tmp/'
     &  // att(1:alen) // '_L1; finish'
      call dotask(cmd, ierror)

      cmd = 'cmo/copyatt/top_mo_lin_ext_tmp/top_mo_lin_ext_tmp/'
     &  // att(1:alen) // '_L1/' // att(1:alen) // '; finish'
      call dotask(cmd, ierror)

      cmd = 'cmo/setatt/top_mo_lin_ext_tmp/'
     &  // direction(1:1) // 'ic/1,0,0/0.0; finish'
      call dotask(cmd, ierror)

      cmd = 'geniee; finish'
      call dotask(cmd, ierror)

C     cmd = 'dump/gmv/top.gmv/top_mo_lin_ext_tmp; finish'
C     call dotask(cmd, ierror)

      cmd = 'cmo/addatt/bot_mo_lin_ext_tmp/'
     &  // att(1:alen) // '_L0; finish'
      call dotask(cmd, ierror)

      cmd = 'cmo/copyatt/bot_mo_lin_ext_tmp/bot_mo_lin_ext_tmp/'
     &  // att(1:alen) // '_L0/' // att(1:alen) // '; finish'
      call dotask(cmd, ierror)

      cmd = 'cmo/setatt/bot_mo_lin_ext_tmp/'
     &  // direction(1:1) // 'ic/1,0,0/0.0; finish'
      call dotask(cmd, ierror)

      cmd = 'geniee; finish'
      call dotask(cmd, ierror)

C     cmd = 'dump/gmv/bot.gmv/bot_mo_lin_ext_tmp; finish'
C     call dotask(cmd, ierror)




      cmd = 'cmo/copyatt/surf_mo_lin_ext_tmp/surf_mo_lin_ext_tmp/'
     &  // att(1:alen) // '_S/' // att(1:alen) // '; finish'
      call dotask(cmd, ierror)

      call cmo_get_mesh_type('surf_mo_lin_ext_tmp',mtype,imtype,ierror)

      if (mtype(1:4) .eq. 'quad' .or. mtype(1:3) .eq. 'hex') then
          cmd = 'hextotet/4/surf_mo_lin_ext_tmp_tri/'
     &        // 'surf_mo_lin_ext_tmp; finish'
          call dotask(cmd, ierror)

          cmd = 'cmo/move/surf_mo_lin_ext_tmp/surf_mo_lin_ext_tmp_tri;'
     &        // ' finish'
          call dotask(cmd, ierror)
      endif

      cmd = 'cmo/setatt/surf_mo_lin_ext_tmp/'
     &  // direction(1:1) // 'ic/1,0,0/0.0; finish'
      call dotask(cmd, ierror)

      cmd = 'geniee; finish'
      call dotask(cmd, ierror)

C     cmd = 'dump/gmv/surf.gmv/surf_mo_lin_ext_tmp; finish'
C     call dotask(cmd, ierror)

      cmd = 'cmo/copyatt/main_mo_lin_ext_tmp/main_mo_lin_ext_tmp/'
     &  // att(1:alen) // '_old/' // att(1:alen) // '; finish'
      call dotask(cmd, ierror)

      cmd = 'cmo/setatt/main_mo_lin_ext_tmp/'
     &  // direction(1:1) // 'ic/1,0,0/0.0; finish'
      call dotask(cmd, ierror)

      cmd = 'geniee; finish'
      call dotask(cmd, ierror)

C     cmd = 'dump/gmv/main.gmv/main_mo_lin_ext_tmp; finish'
C     call dotask(cmd, ierror)

      cmd = 'interpolate/continuous/main_mo_lin_ext_tmp,'
     &  // att(1:alen) // '_S/1,0,0/surf_mo_lin_ext_tmp,'
     &  // att(1:alen) // '_S; finish'
      call dotask(cmd, ierror)

      cmd = 'interpolate/continuous/main_mo_lin_ext_tmp,'
     &  // att(1:alen) // '_L1/1,0,0/top_mo_lin_ext_tmp,'
     &  // att(1:alen) // '_L1; finish'
      call dotask(cmd, ierror)

      cmd = 'interpolate/continuous/main_mo_lin_ext_tmp,'
     &  // att(1:alen) // '_L0/1,0,0/bot_mo_lin_ext_tmp,'
     &  // att(1:alen) // '_L0; finish'
      call dotask(cmd, ierror)

C     cmd = 'dump/gmv/check.gmv/main_mo_lin_ext_tmp; finish'
C     call dotask(cmd, ierror)

      cmd = 'math/subtract/main_mo_lin_ext_tmp/'
     &  // att(1:alen) // '_old/1,0,0/main_mo_lin_ext_tmp/'
     &  // att(1:alen) // '_old/main_mo_lin_ext_tmp/'
     &  // att(1:alen) // '_L0; finish'
      call dotask(cmd, ierror)

      cmd = 'math/subtract/main_mo_lin_ext_tmp/'
     &  // att(1:alen) // '_L1/1,0,0/main_mo_lin_ext_tmp/'
     &  // att(1:alen) // '_L1/main_mo_lin_ext_tmp/'
     &  // att(1:alen) // '_L0; finish'
      call dotask(cmd, ierror)

      cmd = 'math/subtract/main_mo_lin_ext_tmp/'
     &  // att(1:alen) // '_S/1,0,0/main_mo_lin_ext_tmp/'
     &  // att(1:alen) // '_S/main_mo_lin_ext_tmp/'
     &  // att(1:alen) // '_L0; finish'
      call dotask(cmd, ierror)

      cmd = 'math/divide/main_mo_lin_ext_tmp/'
     &  // att(1:alen) // '_old/1,0,0/main_mo_lin_ext_tmp/'
     &  // att(1:alen) // '_old/main_mo_lin_ext_tmp/'
     &  // att(1:alen) // '_L1; finish'
      call dotask(cmd, ierror)

      cmd = 'math/multiply/main_mo_lin_ext_tmp/'
     &  // att(1:alen) // '_old/1,0,0/main_mo_lin_ext_tmp/'
     &  // att(1:alen) // '_old/main_mo_lin_ext_tmp/'
     &  // att(1:alen) // '_S; finish'
      call dotask(cmd, ierror)

      cmd = 'math/add/main_mo_lin_ext_tmp/'
     &  // att(1:alen) // '_new/1,0,0/main_mo_lin_ext_tmp/'
     &  // att(1:alen) // '_old/main_mo_lin_ext_tmp/'
     &  // att(1:alen) // '_L0; finish'
      call dotask(cmd, ierror)

      cmd = 'cmo/copyatt/' // main_mo(1:mlen) // '/main_mo_lin_ext_tmp/'
     &  // att(1:alen) // '/' // att(1:alen) // '_new; finish'
      call dotask(cmd, ierror)

      if (att(1:alen) .ne. 'zic' .and.
     &    att(1:alen) .ne. 'yic' .and.
     &    att(1:alen) .ne. 'xic') then
          cmd = 'cmo/copyatt/main_mo_lin_ext_tmp/main_mo_lin_ext_tmp/'
     &        // direction(1:1) // 'ic/' // direction(1:1) // '_save;'
     &        // 'finish'
          call dotask(cmd,ierror)
      endif

      cmd = 'cmo/release/main_mo_lin_ext_tmp ; finish'
      call dotask(cmd, ierror)
 
      cmd = 'cmo/release/surf_mo_lin_ext_tmp ; finish'
      call dotask(cmd, ierror)
 
      cmd = 'cmo/release/hull_mo_lin_ext_tmp ; finish'
      call dotask(cmd, ierror)
 
      cmd = 'cmo/release/top_mo_lin_ext_tmp ; finish'
      call dotask(cmd, ierror)
 
      cmd = 'cmo/release/bot_mo_lin_ext_tmp ; finish'
      call dotask(cmd, ierror)
 
C
C    All done
C

 9990 continue
      ierror = 1
      return

 9999 continue
      ierror = 0
      return
      end
C     
