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
C     and the uniform hex grid for a uniform h (as before V3.4.0);
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
      if_poi_face_lat = -1
      poi_face_fac = POI_FACE_FAC_DEFAULT
      if_poi_face_fac = 0
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
C        face_lattice    uniform | adaptive
C        face_lattice_factor  real > 0
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
      if(ckey .eq. 'face_lattice_factor')then
         kfound = 1
         if_poi_face_fac = 1
         jtyp = 0
         if(i+1 .le. nwds) jtyp = msgtyp(i+1)
         if(jtyp .eq. POI_TYPE_INT)then
            poi_face_fac = dble(imsgin(i+1))
         elseif(poi_token_type(msgtyp,nwds,i+1,POI_TYPE_REAL,
     &      'face_lattice_factor'))then
            poi_face_fac = xmsgin(i+1)
         endif
         if(.not. (poi_face_fac .gt. 0.0d0))then
            write(cmsg,'(a,i3,a)') 'token ',i+1+POI_TOKEN_OFFSET,
     &         ' face_lattice_factor must be more than 0'
            call poi_err_add(cmsg)
         endif
         return
      endif
      if(ckey .eq. 'face_lattice')then
         kfound = 1
C        not if_poi_oct_key: face_lattice does not choose the octree
         if_poi_face_lat = 2
         if(poi_token_type(msgtyp,nwds,i+1,POI_TYPE_CHAR,
     &      'face_lattice'))then
            lenv = icharlnf(cmsgin(i+1))
            cval = cmsgin(i+1)(1:lenv)
            call poi_oct_lower(cval)
            if(cval .eq. 'uniform')then
               if_poi_face_lat = 0
            elseif(cval .eq. 'adaptive')then
               if_poi_face_lat = 1
            else
               write(cmsg,'(a,i3,a,a)') 'token ',i+1+POI_TOKEN_OFFSET,
     &            ' face_lattice must be uniform or adaptive, not ',
     &            cmsgin(i+1)(1:lenv)
               call poi_err_add(cmsg)
            endif
         endif
         return
      endif
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
     &           kfactor,nbatch,iseed,nsamples,nsweeps,
     &           mo_face,nface,fscale,ierror)
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
      character*32 mo_out,file_mlgi,file_dump,mo_face
      integer if_variable,if_dump,nbatch,iseed,nsamples,nsweeps
      integer ierror,nface
      real*8  h,kfactor,box_samp(6),box_look(6),fscale
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
     &     nsamples,nsweeps,mo_face,nface,fscale)
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
C
C SUBROUTINE BEGIN ####################################################
      subroutine poi_3d_face_lattice_h(mo_face,file_mlgi,if_variable,
     &           h_spacing,box,nface,ierror)
C #####################################################################
C
C     PURPOSE -
C
C     The edge buffer's lattice on the six box faces, following the
C     h field (face_lattice adaptive, the default with h_lookup
C     octree and a control file). Each face starts as a one-layer
C     createpts / brick of nearly square quads, about as large as the
C     shortest side of the box, and is refined with refine (quadtree,
C     2:1 balanced by refine itself) in rounds: the control file runs
C     on the face vertices, and a leaf quad is refined when its longest
C     edge is longer than h at one of its corners, down to quads of
C     h_spacing_scalar. The faces are refined denser than the
C     interior (quads 0.5 h to h, tets inside about 1.4 h), so the
C     tets on the faces are a little long inward and their Voronoi
C     points (circumcenters) fall inside the box. The vertices of the
C     six faces are gathered in mo_face (vertices only, those on the
C     box edges once).
C
C     INPUT ARGUMENTS -
C        mo_face     - name of the mesh object to make
C        file_mlgi   - the user's control file (if_variable 1)
C        if_variable - 1: h from the control file; 0: h_spacing
C        h_spacing   - h_spacing_scalar
C        box         - the user box (IBX_* order)
C
C     OUTPUT ARGUMENTS -
C        nface       - vertices in mo_face
C        ierror      - 0 ok
C
C #####################################################################
      implicit none
      include 'poi_params.h'
      include 'poi_octree_opts.h'
      character*32 mo_face,file_mlgi
      integer if_variable,nface,ierror
      real*8  h_spacing,box(NBOX)
C
      character*32 mo_q
      character*4 cmn(3),cmx(3)
      character*512 cbuf
      real*8  side(3),smin
      integer m(3),cnt(3),iface,iax,iround,nref,ilen,icharlnf
      integer ityp,ierr
      data cmn /'XMIN','YMIN','ZMIN'/
      data cmx /'XMAX','YMAX','ZMAX'/
C
      ierror = 0
      nface = 0
      mo_q = 'mo_poi_face_quad'
      side(1) = box(IBX_XMAX)-box(IBX_XMIN)
      side(2) = box(IBX_YMAX)-box(IBX_YMIN)
      side(3) = box(IBX_ZMAX)-box(IBX_ZMIN)
      smin = min(side(1),side(2),side(3))
      do iax = 1, 3
         m(iax) = max(1,nint(side(iax)/smin))
      enddo
C
      call poi_cmd('cmo/create/'//mo_face(1:icharlnf(mo_face))//
     &     '/ / / tet ; finish')
      do iface = 1, 6
         iax = (iface+1)/2
         cnt(1) = m(1)+1
         cnt(2) = m(2)+1
         cnt(3) = m(3)+1
         cnt(iax) = 1
         call poi_define_int('POI_FQ_N1',cnt(1))
         call poi_define_int('POI_FQ_N2',cnt(2))
         call poi_define_int('POI_FQ_N3',cnt(3))
         call poi_cmd('cmo/create/'//mo_q//'/ / / quad ; finish')
         call poi_cmd('cmo/select/'//mo_q//' ; finish')
         if(mod(iface,2) .eq. 1)then
            call poi_face_box_cmd(cbuf,iax,cmn(iax),cmn(iax))
         else
            call poi_face_box_cmd(cbuf,iax,cmx(iax),cmx(iax))
         endif
         call poi_cmd(cbuf)
         call poi_cmd('cmo/addatt/'//mo_q//
     &        '/poi_ref/vint/scalar/nelements ; finish')
         do iround = 1, 40
            call poi_face_h(mo_q,file_mlgi,if_variable,h_spacing,
     &           ierror)
            if(ierror .ne. 0) return
            call poi_face_mark(mo_q,h_spacing,nref)
            if(nref .eq. 0) goto 100
            call poi_cmd('cmo/select/'//mo_q//' ; finish')
            call poi_cmd('eltset/e_poi_ref/poi_ref/eq/1 ; finish')
            call poi_cmd('refine/eltset/eltset,get,e_poi_ref ; finish')
            call poi_cmd('eltset/e_poi_ref/delete ; finish')
         enddo
 100     continue
         call poi_cmd('copypts/'//mo_face(1:icharlnf(mo_face))//'/'//
     &        mo_q//' ; finish')
         call poi_cmd('cmo/delete/'//mo_q//' ; finish')
      enddo
C
C     The vertices on the box edges and corners came from two or
C     three faces, with the same coordinates: keep one of each.
C
      call poi_cmd('cmo/select/'//mo_face(1:icharlnf(mo_face))//
     &     ' ; finish')
      call poi_cmd('filter/1 0 0 ; finish')
      call poi_cmd('rmpoint/compress ; finish')
      call cmo_get_info('nnodes',mo_face,nface,ilen,ityp,ierr)
      write(cbuf,'(a,i10,a,f6.2,a)') 'face_lattice adaptive:',nface,
     &     ' vertices on the box faces (quads up to',poi_face_fac,' h)'
      call poi_log_info(cbuf)
      return
      end
C
C SUBROUTINE BEGIN ####################################################
      subroutine poi_face_box_cmd(cbuf,iax,clo,chi)
C #####################################################################
C     The createpts / brick command of one face: POI_FQ_N1..N3 vertices
C     from the user box's min corner to its max corner, with the
C     face's axis iax at POI_<clo> (= POI_<chi>).
C #####################################################################
      implicit none
      character*(*) cbuf
      integer iax
      character*4 clo,chi
      character*9 lo(3),hi(3)
      lo(1) = 'POI_XMIN'
      lo(2) = 'POI_YMIN'
      lo(3) = 'POI_ZMIN'
      hi(1) = 'POI_XMAX'
      hi(2) = 'POI_YMAX'
      hi(3) = 'POI_ZMAX'
      lo(iax) = 'POI_'//clo
      hi(iax) = 'POI_'//chi
      cbuf = 'createpts/brick/xyz/POI_FQ_N1 POI_FQ_N2 POI_FQ_N3/'//
     &   lo(1)//' '//lo(2)//' '//lo(3)//'/'//
     &   hi(1)//' '//hi(2)//' '//hi(3)//'/1 1 1 ; finish'
      return
      end
C
C SUBROUTINE BEGIN ####################################################
      subroutine poi_face_h(mo_q,file_mlgi,if_variable,h_spacing,
     &           ierror)
C #####################################################################
C     h at the vertices of the face mesh mo_q, in its vertex attribute
C     poi_hf: the control file runs on a vertex-only copy named
C     mo_poi_h_field, as in the octree's h lookup.
C #####################################################################
      implicit none
      include 'poi_params.h'
      character*32 mo_q,file_mlgi
      integer if_variable,ierror
      real*8  h_spacing
      pointer (iphf,hf),(iph,hv)
      real*8  hf(*),hv(*)
      pointer (ipx,xq),(ipy,yq),(ipz,zq),(ipx2,x2),(ipy2,y2),(ipz2,z2)
      real*8  xq(*),yq(*),zq(*),x2(*),y2(*),z2(*)
      integer n,n2,i,ilen,ityp,ierr,icharlnf
      character*80 cbuf
C
      ierror = 0
      call cmo_get_info('nnodes',mo_q,n,ilen,ityp,ierr)
      call poi_cmd('cmo/addatt/'//mo_q//
     &     '/poi_hf/vdouble/scalar/nnodes ; finish')
      call cmo_get_info('poi_hf',mo_q,iphf,ilen,ityp,ierr)
      if(if_variable .ne. 1)then
         do i = 1, n
            hf(i) = h_spacing
         enddo
         return
      endif
C     every vertex of mo_q, as the octree's h lookup makes it (copypts
C     would leave out vertices refine has marked)
      write(cbuf,'(a,i10,a)') 'cmo/create/'//POI_MO_HFIELD//'/',n,
     &     '/0/hex ; finish'
      call poi_cmd(cbuf)
      call poi_cmd('cmo/select/'//POI_MO_HFIELD//' ; finish')
      call cmo_get_info('xic',mo_q,ipx,ilen,ityp,ierr)
      call cmo_get_info('yic',mo_q,ipy,ilen,ityp,ierr)
      call cmo_get_info('zic',mo_q,ipz,ilen,ityp,ierr)
      call cmo_get_info('xic',POI_MO_HFIELD,ipx2,ilen,ityp,ierr)
      call cmo_get_info('yic',POI_MO_HFIELD,ipy2,ilen,ityp,ierr)
      call cmo_get_info('zic',POI_MO_HFIELD,ipz2,ilen,ityp,ierr)
      do i = 1, n
         x2(i) = xq(i)
         y2(i) = yq(i)
         z2(i) = zq(i)
      enddo
      call poi_cmd('cmo/addatt/'//POI_MO_HFIELD//'/'//POI_ATT_HFIELD//
     &     '/vdouble/scalar/nnodes ; finish')
      write(cbuf,'(a,1pe24.16,a)') 'cmo/setatt/'//POI_MO_HFIELD//'/'//
     &     POI_ATT_HFIELD//'/1 0 0/',h_spacing,' ; finish'
      call poi_cmd(cbuf)
      ilen = icharlnf(file_mlgi)
      call poi_cmd('infile/'//file_mlgi(1:ilen)//' ; finish')
      call cmo_get_info('nnodes',POI_MO_HFIELD,n2,ilen,ityp,ierr)
      call cmo_get_info(POI_ATT_HFIELD,POI_MO_HFIELD,iph,ilen,ityp,
     &     ierr)
      if(ierr .ne. 0 .or. n2 .ne. n)then
         call poi_err_add('face_lattice adaptive: the control file '//
     &      'changed the vertices or removed h_field_att')
         call poi_err_report(ierror)
         ierror = 1
         return
      endif
C     the control file may have grown arrays: get poi_hf again
      call cmo_get_info('poi_hf',mo_q,iphf,ilen,ityp,ierr)
      do i = 1, n
         hf(i) = hv(i)
      enddo
      call poi_cmd('cmo/delete/'//POI_MO_HFIELD//' ; finish')
      call poi_cmd('cmo/select/'//mo_q//' ; finish')
      return
      end
C
C SUBROUTINE BEGIN ####################################################
      subroutine poi_face_mark(mo_q,h_spacing,nref)
C #####################################################################
C     Mark (poi_ref 1) the leaf quads of mo_q to refine: the longest
C     edge longer than face_lattice_factor x the smallest h (poi_hf) at
C     the corners, and at least h_spacing_scalar. nref: how many.
C #####################################################################
      implicit none
      include 'poi_octree_opts.h'
      character*32 mo_q
      real*8  h_spacing
      integer nref
      pointer (ipx,xic),(ipy,yic),(ipz,zic),(iphf,hf)
      real*8  xic(*),yic(*),zic(*),hf(*)
      pointer (ipitet,itet),(ipioff,itetoff),(ipref,iref),
     &        (ipkid,itetkid)
      integer itet(*),itetoff(*),iref(*),itetkid(*)
      integer ne,ie,k,i1,i2,ilen,ityp,ierr,ierrk
      real*8  s,s2,hmin
C
      nref = 0
      call cmo_get_info('nelements',mo_q,ne,ilen,ityp,ierr)
      call cmo_get_info('xic',mo_q,ipx,ilen,ityp,ierr)
      call cmo_get_info('yic',mo_q,ipy,ilen,ityp,ierr)
      call cmo_get_info('zic',mo_q,ipz,ilen,ityp,ierr)
      call cmo_get_info('poi_hf',mo_q,iphf,ilen,ityp,ierr)
      call cmo_get_info('itet',mo_q,ipitet,ilen,ityp,ierr)
      call cmo_get_info('itetoff',mo_q,ipioff,ilen,ityp,ierr)
      call cmo_get_info('poi_ref',mo_q,ipref,ilen,ityp,ierr)
      call cmo_get_info('itetkid',mo_q,ipkid,ilen,ityp,ierrk)
      do ie = 1, ne
         iref(ie) = 0
         if(ierrk .eq. 0)then
            if(itetkid(ie) .ne. 0) goto 10
         endif
         s = 0.0d0
         hmin = 1.0d300
         do k = 1, 4
            i1 = itet(itetoff(ie)+k)
            i2 = itet(itetoff(ie)+mod(k,4)+1)
            s2 = (xic(i2)-xic(i1))**2 + (yic(i2)-yic(i1))**2 +
     &           (zic(i2)-zic(i1))**2
            s = max(s,s2)
            hmin = min(hmin,hf(i1))
         enddo
         s = sqrt(s)
         if(s .gt. poi_face_fac*hmin .and. s .ge. h_spacing)then
            iref(ie) = 1
            nref = nref + 1
         endif
 10      continue
      enddo
      return
      end
C
C SUBROUTINE BEGIN ####################################################
      subroutine poi_3d_choose_face(if_buffer,if_variable,if_face_h,
     &           ierror)
C #####################################################################
C     if_face_h 1: the face lattice follows h (face_lattice adaptive);
C     0: the uniform lattice at h_spacing_scalar, or none (edge buffer
C     off). Says which, as poi_3d_choose_lookup does.
C #####################################################################
      implicit none
      include 'poi_params.h'
      include 'poi_octree_opts.h'
      integer if_buffer,if_variable,if_face_h,ierror
      character*80 cmsg
C
      ierror = 0
      if_face_h = 0
      if(if_buffer .ne. 1)then
         if(if_poi_face_lat .ge. 0 .or. if_poi_face_fac .eq. 1)then
            call poi_log_warn('face_lattice is ignored: '//
     &         'IF_POI_EDGE_BUFFER is 0 (no face lattice)')
         endif
         return
      endif
      if(if_poi_face_lat .eq. 1)then
         if(if_poi_octree .ne. 1)then
            call poi_err_add('face_lattice adaptive needs '//
     &         'h_lookup octree')
            call poi_err_report(ierror)
            return
         endif
         if_face_h = 1
         cmsg = 'poisson_disk 3d_box: face_lattice adaptive'
      elseif(if_poi_face_lat .eq. 0)then
         cmsg = 'poisson_disk 3d_box: face_lattice uniform'
      elseif(if_poi_octree .eq. 1 .and. if_variable .eq. 1)then
         if_face_h = 1
         cmsg = 'poisson_disk 3d_box: face_lattice adaptive (the '//
     &      'default with a control file)'
      else
         cmsg = 'poisson_disk 3d_box: face_lattice uniform (the '//
     &      'default for a uniform h)'
      endif
      call poi_log_info(cmsg)
      if(if_face_h .eq. 0 .and. if_poi_face_fac .eq. 1)then
         call poi_log_warn('face_lattice_factor is ignored: it is '//
     &      'for face_lattice adaptive')
      endif
      return
      end
C
C SUBROUTINE BEGIN ####################################################
      subroutine poi_set_itp_first(mo,n,ival)
C #####################################################################
C     itp1 = ival on the first n vertices of mo (the face vertices,
C     which the sampler writes first).
C #####################################################################
      implicit none
      character*32 mo
      integer n,ival
      pointer (ipitp,itp1)
      integer itp1(*)
      integer i,nn,ilen,ityp,ierr
      call cmo_get_info('nnodes',mo,nn,ilen,ityp,ierr)
      call cmo_get_info('itp1',mo,ipitp,ilen,ityp,ierr)
      if(ierr .ne. 0) return
      do i = 1, min(n,nn)
         itp1(i) = ival
      enddo
      return
      end
