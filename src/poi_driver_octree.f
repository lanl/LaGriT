C     File poi_driver_octree.f: the h_lookup options of createpts /
C     poisson_disk 3d_box (h_lookup octree). See poi_octree_opts.h and
C     poi_3D_octree.h.
C
C SUBROUTINE BEGIN ####################################################
      subroutine poi_octree_defaults()
C #####################################################################
C     PURPOSE - Set the h lookup options to their defaults.
C     if_poi_octree = -1 means h_lookup was not given: the 3D driver
C     then uses the octree when a control file sets h (variable h)
C     and the uniform hex grid for a uniform h (as before V3.340);
C     2d_polygon always uses its grid.
C #####################################################################
      implicit none
      include 'poi_octree_opts.h'
      if_poi_octree = -1
      if_poi_oct_key = 0
      poi_oct_k = POI_OCT_K_DEFAULT
      poi_oct_batch = POI_OCT_BATCH_DEFAULT
      if_poi_oct_dump = 0
      poi_oct_dump = ' '
      return
      end
C
C SUBROUTINE BEGIN ####################################################
      subroutine poi_parse_octree_key(cmsgin,imsgin,xmsgin,msgtyp,
     &           nwds,i,kfound)
C #####################################################################
C
C     PURPOSE -
C
C     Read one keyword / value pair of the h lookup options, at token
C     i. kfound is 0 if the keyword is not one of them (the caller
C     then reports it as unknown), 1 if it is. A wrong value is
C     recorded with poi_err_add.
C
C        h_lookup        hex_grid | octree
C        octree_k_factor real or integer > 0
C        octree_batch    integer > 0
C        octree_dump     file name
C
C #####################################################################
      implicit none
      include 'poi_params.h'
      include 'poi_octree_opts.h'
C
      integer nwds,i,kfound
      character*32 cmsgin(nwds)
      integer imsgin(nwds),msgtyp(nwds)
      real*8  xmsgin(nwds)
C
      integer lenopt,lenv,icharlnf,jtyp
      logical poi_token_type
      character*32 ckey,cval
      character*(POI_MSG_LEN) cmsg
C
      kfound = 0
      lenopt = icharlnf(cmsgin(i))
      ckey = cmsgin(i)(1:lenopt)
      call poi_oct_lower(ckey)
      if(ckey .ne. 'h_lookup' .and. ckey .ne. 'octree_k_factor' .and.
     &   ckey .ne. 'octree_batch' .and. ckey .ne. 'octree_dump')
     &   return
      kfound = 1
      if_poi_oct_key = 1
C
      if(ckey .eq. 'h_lookup')then
         if(poi_token_type(msgtyp,nwds,i+1,POI_TYPE_CHAR,
     &      'h_lookup'))then
            lenv = icharlnf(cmsgin(i+1))
            cval = cmsgin(i+1)(1:lenv)
            call poi_oct_lower(cval)
            if(cval .eq. 'octree')then
               if_poi_octree = 1
            elseif(cval .eq. 'hex_grid')then
               if_poi_octree = 0
            else
               write(cmsg,'(a,i3,a,a)') 'token ',i+1+POI_TOKEN_OFFSET,
     &            ' h_lookup must be hex_grid or octree, not ',
     &            cmsgin(i+1)(1:lenv)
               call poi_err_add(cmsg)
            endif
         endif
      elseif(ckey .eq. 'octree_k_factor')then
         jtyp = 0
         if(i+1 .le. nwds) jtyp = msgtyp(i+1)
         if(jtyp .eq. POI_TYPE_INT)then
            poi_oct_k = dble(imsgin(i+1))
         elseif(poi_token_type(msgtyp,nwds,i+1,POI_TYPE_REAL,
     &      'octree_k_factor'))then
            poi_oct_k = xmsgin(i+1)
         endif
         if(.not. (poi_oct_k .gt. 0.0d0))then
            write(cmsg,'(a,i3,a)') 'token ',i+1+POI_TOKEN_OFFSET,
     &         ' octree_k_factor must be more than 0'
            call poi_err_add(cmsg)
         endif
      elseif(ckey .eq. 'octree_batch')then
         if(poi_token_type(msgtyp,nwds,i+1,POI_TYPE_INT,
     &      'octree_batch'))then
            poi_oct_batch = imsgin(i+1)
            if(poi_oct_batch .lt. 1)then
               write(cmsg,'(a,i3,a)') 'token ',i+1+POI_TOKEN_OFFSET,
     &            ' octree_batch must be 1 or more'
               call poi_err_add(cmsg)
            endif
         endif
      else
         if(poi_token_type(msgtyp,nwds,i+1,POI_TYPE_CHAR,
     &      'octree_dump'))then
            lenv = icharlnf(cmsgin(i+1))
            poi_oct_dump = cmsgin(i+1)(1:lenv)
            if_poi_oct_dump = 1
         endif
      endif
      return
      end
C
C SUBROUTINE BEGIN ####################################################
      subroutine poi_oct_lower(cword)
C #####################################################################
C     PURPOSE - Change A-Z to a-z in cword, in place.
C #####################################################################
      implicit none
      character*(*) cword
      integer k,ic
      do k = 1, len(cword)
         ic = ichar(cword(k:k))
         if(ic .ge. ichar('A') .and. ic .le. ichar('Z'))
     &      cword(k:k) = char(ic + 32)
      enddo
      return
      end
C
C SUBROUTINE BEGIN ####################################################
      subroutine poi_oct_run(mo_out,file_mlgi,file_dump,
     &           if_variable,if_dump,h,box_samp,box_look,
     &           kfactor,nbatch,iseed,nsamples,nsweeps,ierror)
C #####################################################################
C
C     PURPOSE -
C
C     PHASE 5 of poisson_disk_3d with h_lookup octree: build the h
C     lookup octree (the user's h field file runs on batches of its
C     vertices), check h as the hex grid path checks its lattice,
C     dump the leaves if asked, then sample into mo_out. The octree
C     itself lives in poi_3D_octree_lg.cpp between these calls.
C
C     OUTPUT ARGUMENTS -
C        ierror - 0, or not 0 after an ERROR block
C
C #####################################################################
      implicit none
      include 'poi_params.h'
      include 'poi_errors.h'
C
      character*32 mo_out,file_mlgi,file_dump
      integer if_variable,if_dump,nbatch,iseed,nsamples,nsweeps
      integer ierror
      real*8  h,kfactor,box_samp(6),box_look(6)
C
      real*8  hmin,hmax
      integer ierr2
      character*80 cerr
      character*(POI_MSG_LEN) cmsg
      character*(POI_MSG_LEN) clines(3)
C
      ierror = 0
      call poi_err_reset()
      cerr = ' '
      call poisson_3d_octree_build(file_mlgi,if_variable,h,box_look,
     &     kfactor,nbatch,hmin,hmax,cerr,ierr2)
      if(ierr2 .ne. 0)then
         call poi_err_add('the h lookup octree could not be built:')
         call poi_err_add(cerr)
         call poi_err_report(ierror)
         call poisson_3d_octree_free()
         return
      endif
C
C     The same checks, limits and messages as poi_check_h_field, on
C     every octree vertex.
      if(hmin .lt. h*POI_HFIELD_MIN_FACTOR)then
         call poi_err_add(
     &      'h_field_att is smaller than the target edge length')
         call poi_str_rr(cmsg,'h',h,'h_field min',hmin)
         call poi_err_add(cmsg)
         call poi_err_add(
     &      'the user supplied field may be 0.5*h but no smaller')
      endif
      if(hmax .gt. h*POI_HFIELD_WARN_FACTOR)then
         clines(1) =
     &      'h_field_att is much larger than the target edge length'
         call poi_str_rr(clines(2),'h',h,'h_field max',hmax)
         clines(3) = 'this is usually a units mistake'
         call poi_log_warn_n(clines,3)
      endif
      call poi_err_report(ierror)
      if(ierror .ne. 0)then
         call poisson_3d_octree_free()
         return
      endif
C
      if(if_dump .eq. 1)then
         call poisson_3d_octree_dump(file_dump,ierr2)
         if(ierr2 .ne. 0)then
            cmsg = 'could not write the octree to '//file_dump
            call poi_log_warn(cmsg)
         endif
      endif
C
      call poisson_3d_octree_sample(mo_out,h,box_samp,iseed,
     &     nsamples,nsweeps)
      call poisson_3d_octree_free()
      return
      end
C
C SUBROUTINE BEGIN ####################################################
      subroutine poi_3d_choose_lookup(if_h_field_variable)
C #####################################################################
C
C     PURPOSE - 3d_box: when h_lookup was not given, choose it: octree
C     with a control file (a variable h; a hex grid fine enough for
C     the smallest h can need far more memory and time), hex_grid for
C     a uniform h (the octree packs a uniform h about 20% denser and
C     is slower there), but octree when an octree option is given
C     (octree_k_factor, octree_batch, octree_dump). Says which, and
C     why.
C
C     INPUT ARGUMENTS -
C        if_h_field_variable - 1 when a control file sets h
C
C #####################################################################
      implicit none
      include 'poi_octree_opts.h'
      integer if_h_field_variable
      character*80 cmsg
      integer ierrw
C
      if(if_poi_octree .ge. 0) return
      if(if_h_field_variable .eq. 1)then
         if_poi_octree = 1
         cmsg = 'poisson_disk 3d_box: h_lookup octree (the default '//
     &      'with a control file)'
      elseif(if_poi_oct_key .eq. 1)then
C        octree_k_factor, octree_batch or octree_dump given
         if_poi_octree = 1
         cmsg = 'poisson_disk 3d_box: h_lookup octree (octree '//
     &      'options given)'
      else
         if_poi_octree = 0
         cmsg = 'poisson_disk 3d_box: h_lookup hex_grid (the default '//
     &      'for a uniform h)'
      endif
      call writloga('default',0,cmsg,0,ierrw)
      return
      end
