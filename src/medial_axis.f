      subroutine medial_axis(imsgin,xmsgin,cmsgin,msgtype,nwds,
     &   ierror)
C
C #####################################################################
C
C     PURPOSE -
C
C     compute / medial_axis / mo_axis / mo_surface /
C             [ in | out | both ] [ / alpha_sharp ] [ / rmax ]
C
C     For every vertex P of MO_SURFACE (a line mesh in a plane, 2D,
C     or a triangle surface, 3D) find the largest ball that is
C     tangent to the surface at P and holds no part of the surface
C     (a shrinking ball).  Its center is a point of the medial axis,
C     twice its radius the thickness at P.
C
C     MO_AXIS (created, or replaced) gets one point per vertex and
C     side, at the ball's center, with the attributes
C        ma_r      radius (VDOUBLE)
C        ma_src    the surface vertex (VINT)
C        ma_side   1 in, 2 out (VINT); imt is the same
C        ma_angle  object angle, degrees (VDOUBLE): the widest
C                  angle at the center between P and a point Q
C                  where the ball touches the surface (the shrink's
C                  point, and the points across: see
C                  medial_axis_widen; a ball can touch along a ring)
C        ma_keep   1 kept, 0 flagged (VINT)
C     MO_SURFACE gets ma_thick_in and ma_keep_in (and _out with
C     side out or both): 2 x radius, and the same flag.
C
C     Normals: 3D, the angle-weighted average of the triangles'
C     normals at P; 2D, the average of the two segments' normals
C     D x A, D the segment's direction and A the axis of the plane
C     (+z for a mesh in the xy plane): for a loop traversed
C     counterclockwise they point out.  in grows the ball against
C     the normals, out along them, both does each.
C
C     The ball (Ma, Bae, Choi, Kim and Kim, 2012, here on the
C     cells): the balls tangent at P are nested, so the radius is
C     first doubled from half the shortest edge at P while the ball
C     is empty, then shrunk exactly to pass through the nearest
C     point Q of the surface inside it, r = |Q-P|**2/(2 (Q-P).n),
C     until nothing is inside.  A faceted surface always cuts a
C     tangent ball a little near P (the sag of its facets), so a
C     point seen from the center within 90 degrees of P is inside
C     only if it is closer than r (1 - TAU), TAU = 1 - cos(10 deg):
C     turns up to 20 degrees per vertex are taken as a sampled
C     smooth surface, sharper ones as corners.  Points further
C     round count when closer than r (to round-off).  The cells
C     nearest the center come from the k-D tree of kdtree.f
C     (nearestpoint.f); closest_pt_tri_lg.f measures them.
C
C     Flagged (ma_keep 0): object angle under 180 - alpha_sharp
C     (a branch of a corner wider than alpha_sharp, which says
C     nothing about thickness; a wedge of angle a gives 180 - a);
C     the ball not stopped at rmax (radius rmax); a junction or no
C     normal, with radius 0: in 2D a vertex not on exactly two
C     segments, in 3D a vertex on an edge of three or more
C     triangles (where three or more materials meet).
C
C     INPUT  - the command line
C     OUTPUT - ierror (0 = ok)
C
C #####################################################################
C
      implicit none
      include 'local_element.h'
C
      integer nwds, ierror
      character*(*) cmsgin(nwds)
      integer imsgin(nwds), msgtype(nwds)
      real*8 xmsgin(nwds)
C
      pointer (ipxs,xs),(ipys,ys),(ipzs,zs)
      pointer (ipitet,itet),(ipitetoff,itetoff),(ipitettyp,itettyp)
      pointer (ipitri,itri),(iplinkt,linkt),(ipsbox,sbox)
      pointer (ipicel,icel),(ipijun,ijun),(ipimrk,imrk)
      pointer (ipifound,ifound),(ipvn,vn),(ipiofs,iofs),
     &   (iphmin,hmin),(ipwk,wk)
      pointer (ipxa,xa),(ipya,ya),(ipza,za),(ipar,ar),(ipaa,aa),
     &   (ipisrc,isrc),(ipisid,isid),(ipikp,ikp),(ipimt,imta),
     &   (ipitp,itpa)
      pointer (ipth,th),(ipkp,kps)
      real*8 xs(*),ys(*),zs(*),sbox(*),vn(3,*),hmin(*),wk(*),xa(*),
     &   ya(*),za(*),ar(*),aa(*),th(*)
      integer itet(*),itetoff(*),itettyp(*),itri(3,*),linkt(*),
     &   ifound(*),iofs(*),icel(*),ijun(*),imrk(*),
     &   isrc(*),isid(*),ikp(*),
     &   imta(*),itpa(*),kps(*)
C
      integer nsrc,nelem,ntri,nlin,nother,ilen,ityp,ier,i,j,k,it,
     &   ioff,icscode,ldim,nside,iside,is,isd,nout,ia,ib,ic,io,
     &   istat,nkeep,nangle,nrmax,nnone,len1,len2,ntot
      real*8 alpha,rmax,eps,diag,ax,ay,az,ex,ey,ez,ux,uy,uz,
     &   d,ang,px,py,pz,nx,ny,nz,r,qx,qy,qz,cosa,tmin,tmax,
     &   v1x,v1y,v1z,v2x,v2y,v2z,b1x,b1y,b1z,b2x,b2y,b2z,
     &   xmn,xmx,ymn,ymx,zmn,zmx,sgn,deg
      character*32 isubname,cmoa,cmos,cside,cmocur
      character*3 csfx
      character*132 logmess
      character*256 cmdmess
      integer icharlnf
C
      isubname='medial_axis'
      ierror=0
      deg=45.0d0/atan(1.0d0)
      call cmo_get_name(cmocur,ier)
C
C.... The command line.
C
      if (nwds.lt.4 .or. msgtype(3).ne.3 .or. msgtype(4).ne.3) then
         write(logmess,'(a)') 'ERROR compute / medial_axis: usage'
         call writloga('default',0,logmess,0,ier)
         write(logmess,'(a)') '  compute / medial_axis / mo_axis /'//
     &      ' mo_surface / [in|out|both] / [alpha] / [rmax]'
         call writloga('default',0,logmess,0,ier)
         ierror=-1
         goto 9999
      endif
      cmoa=cmsgin(3)
      cmos=cmsgin(4)
      cside='in'
      if (nwds.ge.5) then
         if (msgtype(5).eq.3) then
            if (cmsgin(5)(1:5).ne.'-def-') cside=cmsgin(5)
         endif
      endif
      if (cside.ne.'in' .and. cside.ne.'out' .and.
     &    cside.ne.'both') then
         write(logmess,'(a)')
     &   'ERROR compute / medial_axis: side must be in, out or both'
         call writloga('default',0,logmess,0,ier)
         ierror=-1
         goto 9999
      endif
      alpha=35.0d0
      if (nwds.ge.6) then
         if (msgtype(6).eq.1) alpha=dble(imsgin(6))
         if (msgtype(6).eq.2) alpha=xmsgin(6)
      endif
      rmax=-1.0d0
      if (nwds.ge.7) then
         if (msgtype(7).eq.1) rmax=dble(imsgin(7))
         if (msgtype(7).eq.2) rmax=xmsgin(7)
      endif
      len1=icharlnf(cmoa)
      len2=icharlnf(cmos)
C
C.... The surface: lines (2D) or triangles (3D), not both.
C
      call cmo_exist(cmos,ier)
      if (ier.ne.0) then
         write(logmess,'(a,a)')
     &   'ERROR compute / medial_axis: no mesh object ',cmos(1:len2)
         call writloga('default',0,logmess,0,ier)
         ierror=-1
         goto 9999
      endif
      if (cmoa.eq.cmos) then
         write(logmess,'(a)') 'ERROR compute / medial_axis:'//
     &      ' mo_axis must differ from mo_surface'
         call writloga('default',0,logmess,0,ier)
         ierror=-1
         goto 9999
      endif
      call cmo_get_info('nnodes',cmos,nsrc,ilen,ityp,ier)
      call cmo_get_info('nelements',cmos,nelem,ilen,ityp,ier)
      if (nelem.le.0 .or. nsrc.le.0) then
         write(logmess,'(a)')
     &   'ERROR compute / medial_axis: the surface has no cells.'
         call writloga('default',0,logmess,0,ier)
         ierror=-1
         goto 9999
      endif
      call cmo_get_info('itettyp',cmos,ipitettyp,ilen,ityp,ier)
      call cmo_get_info('itet',cmos,ipitet,ilen,ityp,ier)
      call cmo_get_info('itetoff',cmos,ipitetoff,ilen,ityp,ier)
      call cmo_get_info('xic',cmos,ipxs,ilen,ityp,ier)
      call cmo_get_info('yic',cmos,ipys,ilen,ityp,ier)
      call cmo_get_info('zic',cmos,ipzs,ilen,ityp,ier)
      ntri=0
      nlin=0
      nother=0
      do it=1,nelem
         if (itettyp(it).eq.ifelmtri) then
            ntri=ntri+1
         elseif (itettyp(it).eq.ifelmlin) then
            nlin=nlin+1
         else
            nother=nother+1
         endif
      enddo
      if (nother.gt.0 .or. (ntri.gt.0 .and. nlin.gt.0)) then
         write(logmess,'(a)') 'ERROR compute / medial_axis: the'//
     &      ' surface must be all lines or all triangles.'
         call writloga('default',0,logmess,0,ier)
         ierror=-1
         goto 9999
      endif
      ldim=3
      if (nlin.gt.0) ldim=2
      nside=1
      if (cside.eq.'both') nside=2
C
C.... Cells as triangles (lines as (a,b,b)); cells at each vertex
C.... (iofs: the count is iofs(i+1)-iofs(i)).
C
      call mmgetblk('itri',isubname,ipitri,3*nelem,1,icscode)
      call mmgetblk('linkt',isubname,iplinkt,2*nelem,1,icscode)
      call mmgetblk('sbox',isubname,ipsbox,12*nelem,2,icscode)
      call mmgetblk('ifound',isubname,ipifound,nelem,1,icscode)
      call mmgetblk('wk',isubname,ipwk,nelem,2,icscode)
      call mmgetblk('vn',isubname,ipvn,3*nsrc,2,icscode)
      call mmgetblk('hmin',isubname,iphmin,nsrc,2,icscode)
      call mmgetblk('iofs',isubname,ipiofs,nsrc+1,1,icscode)
      do it=1,nelem
         ioff=itetoff(it)
         itri(1,it)=itet(ioff+1)
         itri(2,it)=itet(ioff+2)
         if (ldim.eq.2) then
            itri(3,it)=itet(ioff+2)
         else
            itri(3,it)=itet(ioff+3)
         endif
      enddo
      do i=1,nsrc+1
         iofs(i)=0
      enddo
      do it=1,nelem
         do k=1,ldim
            i=itri(k,it)
            iofs(i+1)=iofs(i+1)+1
         enddo
      enddo
      do i=1,nsrc
         iofs(i+1)=iofs(i+1)+iofs(i)
      enddo
      call mmgetblk('icel',isubname,ipicel,3*nelem,1,icscode)
      call mmgetblk('ijun',isubname,ipijun,nsrc,1,icscode)
      call mmgetblk('imrk',isubname,ipimrk,nsrc,1,icscode)
      do i=1,nsrc
         imrk(i)=0
         ijun(i)=0
      enddo
      do it=1,nelem
         do k=1,ldim
            i=itri(k,it)
            imrk(i)=imrk(i)+1
            icel(iofs(i)+imrk(i))=it
         enddo
      enddo
C
C.... Junctions: 2D, a vertex not on two segments; 3D, a vertex on
C.... an edge of three or more triangles (counted at each vertex:
C.... how many of its triangles hold each neighbor).
C
      do i=1,nsrc
         imrk(i)=0
      enddo
      do i=1,nsrc
         if (ldim.eq.2) then
            if (iofs(i+1)-iofs(i).ne.2) ijun(i)=1
         else
            do j=iofs(i)+1,iofs(i+1)
               it=icel(j)
               do k=1,3
                  io=itri(k,it)
                  if (io.ne.i) then
                     imrk(io)=imrk(io)+1
                     if (imrk(io).ge.3) ijun(i)=1
                  endif
               enddo
            enddo
            do j=iofs(i)+1,iofs(i+1)
               it=icel(j)
               do k=1,3
                  imrk(itri(k,it))=0
               enddo
            enddo
         endif
      enddo
C
C.... The box, and for 2D the axis of the plane.
C
      xmn=xs(1)
      xmx=xs(1)
      ymn=ys(1)
      ymx=ys(1)
      zmn=zs(1)
      zmx=zs(1)
      do i=2,nsrc
         xmn=min(xmn,xs(i))
         xmx=max(xmx,xs(i))
         ymn=min(ymn,ys(i))
         ymx=max(ymx,ys(i))
         zmn=min(zmn,zs(i))
         zmx=max(zmx,zs(i))
      enddo
      diag=sqrt((xmx-xmn)**2+(ymx-ymn)**2+(zmx-zmn)**2)
      eps=1.0d-8*max(diag,1.0d-30)
      if (rmax.le.0.0d0) rmax=diag
      ax=0.0d0
      ay=0.0d0
      az=0.0d0
      if (ldim.eq.2) then
         if (zmx-zmn.le.eps) then
            az=1.0d0
         elseif (ymx-ymn.le.eps) then
            ay=1.0d0
         elseif (xmx-xmn.le.eps) then
            ax=1.0d0
         else
C.... Newell's sum of the segments about the box center.
            v1x=0.5d0*(xmn+xmx)
            v1y=0.5d0*(ymn+ymx)
            v1z=0.5d0*(zmn+zmx)
            do it=1,nelem
               ia=itri(1,it)
               ib=itri(2,it)
               b1x=xs(ia)-v1x
               b1y=ys(ia)-v1y
               b1z=zs(ia)-v1z
               b2x=xs(ib)-v1x
               b2y=ys(ib)-v1y
               b2z=zs(ib)-v1z
               ax=ax+b1y*b2z-b1z*b2y
               ay=ay+b1z*b2x-b1x*b2z
               az=az+b1x*b2y-b1y*b2x
            enddo
            d=sqrt(ax*ax+ay*ay+az*az)
            if (d.le.eps*eps) then
               write(logmess,'(a)') 'ERROR compute / medial_axis:'//
     &            ' cannot find the plane of the line mesh.'
               call writloga('default',0,logmess,0,ier)
               ierror=-1
               goto 9999
            endif
            ax=ax/d
            ay=ay/d
            az=az/d
         endif
      endif
C
C.... Vertex normals and the shortest edge at each vertex.
C
      do i=1,nsrc
         vn(1,i)=0.0d0
         vn(2,i)=0.0d0
         vn(3,i)=0.0d0
         hmin(i)=1.0d60
      enddo
      do it=1,nelem
         ia=itri(1,it)
         ib=itri(2,it)
         ic=itri(3,it)
         if (ldim.eq.2) then
            ex=xs(ib)-xs(ia)
            ey=ys(ib)-ys(ia)
            ez=zs(ib)-zs(ia)
            d=sqrt(ex*ex+ey*ey+ez*ez)
            hmin(ia)=min(hmin(ia),d)
            hmin(ib)=min(hmin(ib),d)
            if (d.gt.eps) then
               ux=(ey*az-ez*ay)/d
               uy=(ez*ax-ex*az)/d
               uz=(ex*ay-ey*ax)/d
               vn(1,ia)=vn(1,ia)+ux
               vn(2,ia)=vn(2,ia)+uy
               vn(3,ia)=vn(3,ia)+uz
               vn(1,ib)=vn(1,ib)+ux
               vn(2,ib)=vn(2,ib)+uy
               vn(3,ib)=vn(3,ib)+uz
            endif
         else
            v1x=xs(ib)-xs(ia)
            v1y=ys(ib)-ys(ia)
            v1z=zs(ib)-zs(ia)
            v2x=xs(ic)-xs(ia)
            v2y=ys(ic)-ys(ia)
            v2z=zs(ic)-zs(ia)
            ux=v1y*v2z-v1z*v2y
            uy=v1z*v2x-v1x*v2z
            uz=v1x*v2y-v1y*v2x
            d=sqrt(ux*ux+uy*uy+uz*uz)
            if (d.gt.eps*eps) then
               ux=ux/d
               uy=uy/d
               uz=uz/d
               do k=1,3
                  if (k.eq.1) then
                     io=ia
                     b1x=xs(ib)-xs(ia)
                     b1y=ys(ib)-ys(ia)
                     b1z=zs(ib)-zs(ia)
                     b2x=xs(ic)-xs(ia)
                     b2y=ys(ic)-ys(ia)
                     b2z=zs(ic)-zs(ia)
                  elseif (k.eq.2) then
                     io=ib
                     b1x=xs(ic)-xs(ib)
                     b1y=ys(ic)-ys(ib)
                     b1z=zs(ic)-zs(ib)
                     b2x=xs(ia)-xs(ib)
                     b2y=ys(ia)-ys(ib)
                     b2z=zs(ia)-zs(ib)
                  else
                     io=ic
                     b1x=xs(ia)-xs(ic)
                     b1y=ys(ia)-ys(ic)
                     b1z=zs(ia)-zs(ic)
                     b2x=xs(ib)-xs(ic)
                     b2y=ys(ib)-ys(ic)
                     b2z=zs(ib)-zs(ic)
                  endif
                  d=sqrt(b1x*b1x+b1y*b1y+b1z*b1z)
                  ang=sqrt(b2x*b2x+b2y*b2y+b2z*b2z)
                  hmin(io)=min(hmin(io),d,ang)
                  cosa=(b1x*b2x+b1y*b2y+b1z*b2z)/max(d*ang,1.0d-300)
                  cosa=max(-1.0d0,min(1.0d0,cosa))
                  ang=acos(cosa)
                  vn(1,io)=vn(1,io)+ang*ux
                  vn(2,io)=vn(2,io)+ang*uy
                  vn(3,io)=vn(3,io)+ang*uz
               enddo
            endif
         endif
      enddo
      do i=1,nsrc
         d=sqrt(vn(1,i)**2+vn(2,i)**2+vn(3,i)**2)
         if (d.gt.1.0d-12) then
            vn(1,i)=vn(1,i)/d
            vn(2,i)=vn(2,i)/d
            vn(3,i)=vn(3,i)/d
         else
            vn(1,i)=0.0d0
            vn(2,i)=0.0d0
            vn(3,i)=0.0d0
         endif
         if (hmin(i).gt.1.0d59) hmin(i)=0.0d0
      enddo
C
      call kdtree(xs,ys,zs,itri,nelem,linkt,sbox,ier)
C
C.... The axis mesh object: points only.
C
      nout=nsrc*nside
      call cmo_exist(cmoa,ier)
      if (ier.eq.0) then
         cmdmess='cmo/delete/'//cmoa(1:len1)//' ; finish'
         call dotask(cmdmess,ier)
      endif
      cmdmess='cmo/create/'//cmoa(1:len1)//'///tet ; finish'
      call dotask(cmdmess,ier)
      call cmo_set_info('nnodes',cmoa,nout,1,1,ier)
      call cmo_set_info('nelements',cmoa,0,1,1,ier)
      call cmo_newlen(cmoa,ier)
      cmdmess='cmo/addatt/'//cmoa(1:len1)//
     &   '/ma_r/VDOUBLE/scalar/nnodes/linear/permanent/ /0.0'//
     &   ' ; cmo/addatt/'//cmoa(1:len1)//
     &   '/ma_angle/VDOUBLE/scalar/nnodes/linear/permanent/ /0.0'//
     &   ' ; finish'
      call dotask(cmdmess,ier)
      cmdmess='cmo/addatt/'//cmoa(1:len1)//
     &   '/ma_src/VINT/scalar/nnodes/linear/permanent/ /0'//
     &   ' ; cmo/addatt/'//cmoa(1:len1)//
     &   '/ma_side/VINT/scalar/nnodes/linear/permanent/ /0'//
     &   ' ; cmo/addatt/'//cmoa(1:len1)//
     &   '/ma_keep/VINT/scalar/nnodes/linear/permanent/ /0'//
     &   ' ; finish'
      call dotask(cmdmess,ier)
C
C.... The surface's attributes.
C
      do isd=1,2
         if ((isd.eq.1 .and. cside.ne.'out') .or.
     &       (isd.eq.2 .and. cside.ne.'in')) then
            csfx='in '
            if (isd.eq.2) csfx='out'
            cmdmess='cmo/addatt/'//cmos(1:len2)//'/ma_thick_'//
     &       csfx(1:icharlnf(csfx))//
     &       '/VDOUBLE/scalar/nnodes/linear/permanent/ /0.0'//
     &       ' ; cmo/addatt/'//cmos(1:len2)//'/ma_keep_'//
     &       csfx(1:icharlnf(csfx))//
     &       '/VINT/scalar/nnodes/linear/permanent/ /0 ; finish'
            call dotask(cmdmess,ier)
         endif
      enddo
C
C.... Pointers after every addatt (attributes may move memory).
C
      call cmo_get_info('xic',cmos,ipxs,ilen,ityp,ier)
      call cmo_get_info('yic',cmos,ipys,ilen,ityp,ier)
      call cmo_get_info('zic',cmos,ipzs,ilen,ityp,ier)
      call cmo_get_info('xic',cmoa,ipxa,ilen,ityp,ier)
      call cmo_get_info('yic',cmoa,ipya,ilen,ityp,ier)
      call cmo_get_info('zic',cmoa,ipza,ilen,ityp,ier)
      call cmo_get_info('imt1',cmoa,ipimt,ilen,ityp,ier)
      call cmo_get_info('itp1',cmoa,ipitp,ilen,ityp,ier)
      call cmo_get_info('ma_r',cmoa,ipar,ilen,ityp,ier)
      call cmo_get_info('ma_angle',cmoa,ipaa,ilen,ityp,ier)
      call cmo_get_info('ma_src',cmoa,ipisrc,ilen,ityp,ier)
      call cmo_get_info('ma_side',cmoa,ipisid,ilen,ityp,ier)
      call cmo_get_info('ma_keep',cmoa,ipikp,ilen,ityp,ier)
C
C.... The balls.
C
      nkeep=0
      nangle=0
      nrmax=0
      nnone=0
      tmin=1.0d60
      tmax=0.0d0
      io=0
      do isd=1,2
       if ((isd.eq.1 .and. cside.ne.'out') .or.
     &     (isd.eq.2 .and. cside.ne.'in')) then
         sgn=-1.0d0
         if (isd.eq.2) sgn=1.0d0
         do i=1,nsrc
            io=io+1
            px=xs(i)
            py=ys(i)
            pz=zs(i)
            nx=sgn*vn(1,i)
            ny=sgn*vn(2,i)
            nz=sgn*vn(3,i)
            is=iofs(i+1)-iofs(i)
            if ((nx.eq.0.0d0 .and. ny.eq.0.0d0 .and. nz.eq.0.0d0)
     &          .or. is.eq.0 .or. ijun(i).eq.1) then
               r=0.0d0
               istat=3
               qx=px
               qy=py
               qz=pz
            else
               call medial_axis_ball(i,px,py,pz,nx,ny,nz,hmin(i),
     &            rmax,eps,xs,ys,zs,itri,linkt,sbox,ifound,wk,
     &            r,qx,qy,qz,istat)
            endif
            xa(io)=px+r*nx
            ya(io)=py+r*ny
            za(io)=pz+r*nz
            ar(io)=r
            isrc(io)=i
            isid(io)=isd
            imta(io)=isd
            itpa(io)=0
            ang=0.0d0
            if (istat.eq.0 .and. r.gt.0.0d0) then
               v1x=px-xa(io)
               v1y=py-ya(io)
               v1z=pz-za(io)
               v2x=qx-xa(io)
               v2y=qy-ya(io)
               v2z=qz-za(io)
               d=sqrt((v1x*v1x+v1y*v1y+v1z*v1z)*
     &                (v2x*v2x+v2y*v2y+v2z*v2z))
               cosa=(v1x*v2x+v1y*v2y+v1z*v2z)/max(d,1.0d-300)
               cosa=max(-1.0d0,min(1.0d0,cosa))
               ang=acos(cosa)*deg
C.... The object angle is the widest over the points the ball
C.... touches; the shrink found one of them.  Also try the point
C.... of the surface nearest the far end of the diameter through P
C.... (on a sphere every point touches: that one gives 180).
               call medial_axis_near(2.0d0*xa(io)-px,2.0d0*ya(io)-py,
     &            2.0d0*za(io)-pz,eps,xs,ys,zs,itri,linkt,sbox,
     &            ifound,wk,b1x,b1y,b1z)
               v2x=b1x-xa(io)
               v2y=b1y-ya(io)
               v2z=b1z-za(io)
               d=sqrt(v2x*v2x+v2y*v2y+v2z*v2z)
               if (d.le.r*(1.0d0+1.519224698779198d-2)) then
                  cosa=(v1x*v2x+v1y*v2y+v1z*v2z)/max(d*r,1.0d-300)
                  cosa=max(-1.0d0,min(1.0d0,cosa))
                  if (acos(cosa)*deg.gt.ang) then
                     ang=acos(cosa)*deg
                     qx=b1x
                     qy=b1y
                     qz=b1z
                  endif
               endif
               call medial_axis_widen(px,py,pz,xa(io),ya(io),za(io),r,
     &            qx,qy,qz,eps,xs,ys,zs,itri,linkt,sbox,ifound,wk,ang)
            elseif (istat.eq.0) then
C.... The ball shrank to nothing: a corner (angle 180 - corner).
               v1x=qx-px
               v1y=qy-py
               v1z=qz-pz
               d=sqrt(v1x*v1x+v1y*v1y+v1z*v1z)
               if (d.gt.0.0d0) then
                  cosa=(v1x*nx+v1y*ny+v1z*nz)/d
                  cosa=max(-1.0d0,min(1.0d0,cosa))
                  ang=180.0d0-2.0d0*acos(cosa)*deg
                  ang=max(0.0d0,ang)
               endif
            endif
            aa(io)=ang
            ikp(io)=0
            if (istat.eq.0) then
               if (alpha.le.0.0d0 .or.
     &             ang.ge.180.0d0-alpha-1.0d-9) then
                  ikp(io)=1
                  nkeep=nkeep+1
                  tmin=min(tmin,2.0d0*r)
                  tmax=max(tmax,2.0d0*r)
               else
                  nangle=nangle+1
               endif
            elseif (istat.eq.1) then
               nrmax=nrmax+1
            else
               nnone=nnone+1
            endif
         enddo
       endif
      enddo
C
C.... Thickness and the flag on the surface.
C
      io=0
      do isd=1,2
       if ((isd.eq.1 .and. cside.ne.'out') .or.
     &     (isd.eq.2 .and. cside.ne.'in')) then
         if (isd.eq.1) then
            call cmo_get_info('ma_thick_in',cmos,ipth,ilen,ityp,ier)
            call cmo_get_info('ma_keep_in',cmos,ipkp,ilen,ityp,ier)
         else
            call cmo_get_info('ma_thick_out',cmos,ipth,ilen,ityp,
     &         ier)
            call cmo_get_info('ma_keep_out',cmos,ipkp,ilen,ityp,ier)
         endif
         do i=1,nsrc
            io=io+1
            th(i)=2.0d0*ar(io)
            kps(i)=ikp(io)
         enddo
       endif
      enddo
C
      ntot=nkeep+nangle+nrmax+nnone
      if (nkeep.eq.0) tmin=0.0d0
      write(logmess,'(a,i10,a,i2,a)') 'medial_axis:',nsrc,
     &   ' surface vertices, ',ldim,'D, side '//cside(1:4)
      call writloga('default',0,logmess,0,ier)
      write(logmess,'(a,i10,a,i10,a)') 'medial_axis:',ntot,
     &   ' balls,',nkeep,' kept'
      call writloga('default',0,logmess,0,ier)
      write(logmess,'(a,i9,a,i9,a,i9,a)') 'medial_axis: flagged',
     &   nangle,' angle,',nrmax,' rmax,',nnone,' junction'
      call writloga('default',0,logmess,0,ier)
      write(logmess,'(a,1pe13.5,a,1pe13.5)')
     &   'medial_axis: kept thickness',tmin,' to',tmax
      call writloga('default',0,logmess,0,ier)
C
 9999 continue
      call mmrelprt(isubname,icscode)
      if (cmocur(1:1).ne.' ') then
         call cmo_exist(cmocur,ier)
         if (ier.eq.0) then
            cmdmess='cmo/select/'//cmocur(1:icharlnf(cmocur))//
     &         ' ; finish'
            call dotask(cmdmess,ier)
         endif
      endif
      return
      end
C
C
      subroutine medial_axis_ball(ip,px,py,pz,nx,ny,nz,h,rmax,eps,
     &   xs,ys,zs,itri,linkt,sbox,ifound,wk,r,qx,qy,qz,istat)
C
C #####################################################################
C
C     PURPOSE -
C
C     The largest ball tangent at vertex IP (point P, unit normal
C     N into the side) that holds no point of the cells (to the
C     tolerance of medial_axis_hit).  OUT: radius R, the touching
C     point Q, ISTAT 0 (stopped by Q; R 0 at a corner), 1 (not
C     stopped: R = RMAX).
C
C #####################################################################
C
      implicit none
      integer ip,itri(3,*),linkt(*),ifound(*),istat
      real*8 px,py,pz,nx,ny,nz,h,rmax,eps,xs(*),ys(*),zs(*),
     &   sbox(*),wk(*),r,qx,qy,qz
C
      integer iter,lhit
      real*8 dq,tx,ty,tz,ex,ey,ez,den
C
      istat=0
      qx=px
      qy=py
      qz=pz
C
C.... Grow: double from half the shortest edge while empty.
C
      r=min(max(0.5d0*h,10.0d0*eps),rmax)
      do iter=1,200
         call medial_axis_hit(ip,px,py,pz,nx,ny,nz,r,eps,xs,ys,zs,
     &      itri,linkt,sbox,ifound,wk,lhit,dq,tx,ty,tz)
         if (lhit.eq.1) goto 100
         if (r.ge.rmax) then
            r=rmax
            istat=1
            return
         endif
         r=min(2.0d0*r,rmax)
      enddo
      istat=1
      return
C
C.... Shrink: the ball through P (tangent) and the point found.
C
 100  continue
      do iter=1,200
         ex=tx-px
         ey=ty-py
         ez=tz-pz
         den=2.0d0*(ex*nx+ey*ny+ez*nz)
         qx=tx
         qy=ty
         qz=tz
         if (den.le.0.0d0) then
            r=0.0d0
            return
         endif
         r=(ex*ex+ey*ey+ez*ez)/den
         if (r.le.10.0d0*eps) then
            r=0.0d0
            return
         endif
         call medial_axis_hit(ip,px,py,pz,nx,ny,nz,r,eps,xs,ys,zs,
     &      itri,linkt,sbox,ifound,wk,lhit,dq,tx,ty,tz)
         if (lhit.eq.0) return
      enddo
      return
      end
C
C
      subroutine medial_axis_hit(ip,px,py,pz,nx,ny,nz,r,eps,xs,ys,
     &   zs,itri,linkt,sbox,ifound,wk,lhit,dq,tx,ty,tz)
C
C #####################################################################
C
C     PURPOSE -
C
C     Is the nearest point T of the cells to the center C = P + R N
C     inside the ball of radius R?  Inside: closer than R (1 - TAU),
C     or, when the angle at C between P and T is 90 degrees or more,
C     closer than R (1 - 1e-9).  LHIT 1 and T (distance DQ from C),
C     or LHIT 0.
C
C #####################################################################
C
      implicit none
      integer ip,itri(3,*),linkt(*),ifound(*),lhit
      real*8 px,py,pz,nx,ny,nz,r,eps,xs(*),ys(*),zs(*),sbox(*),
     &   wk(*),dq,tx,ty,tz
C
      real*8 cx,cy,cz,dmin2,rlim,tau,cosb,rin,ca
      parameter (tau=1.519224698779198d-2, cosb=0.0d0)
C
      lhit=0
      cx=px+r*nx
      cy=py+r*ny
      cz=pz+r*nz
      rlim=(r*(1.0d0-tau))**2
      rin=(r*(1.0d0-1.0d-9))**2
      call medial_axis_near(cx,cy,cz,eps,xs,ys,zs,itri,linkt,sbox,
     &   ifound,wk,tx,ty,tz)
      dmin2=(tx-cx)**2+(ty-cy)**2+(tz-cz)**2
      if (dmin2.lt.rlim) then
         lhit=1
      elseif (dmin2.lt.rin) then
C.... the angle at C between P and T: -N . (T - C) / |T - C|
         ca=-(nx*(tx-cx)+ny*(ty-cy)+nz*(tz-cz))/
     &      max(sqrt(dmin2),1.0d-300)
         if (ca.le.cosb) lhit=1
      endif
      if (lhit.eq.1) dq=sqrt(dmin2)
      return
      end
C
C
      subroutine medial_axis_near(cx,cy,cz,eps,xs,ys,zs,itri,linkt,
     &   sbox,ifound,wk,tx,ty,tz)
C
C #####################################################################
C
C     PURPOSE -
C
C     The point T of the cells nearest the point C (the k-D tree's
C     candidates of nearestpoint.f, each measured exactly).
C
C #####################################################################
C
      implicit none
      integer itri(3,*),linkt(*),ifound(*)
      real*8 cx,cy,cz,eps,xs(*),ys(*),zs(*),sbox(*),wk(*),tx,ty,tz
C
      integer nf,k,j,ier
      real*8 rx,ry,rz,d2,dmin2,ox,oy,oz
C
      ox=1.0d30
      oy=1.0d30
      oz=1.0d30
      call nearestpoint(cx,cy,cz,xs,ys,zs,itri,ox,oy,oz,linkt,sbox,
     &   eps,wk,nf,ifound,ier)
      dmin2=1.0d300
      tx=cx
      ty=cy
      tz=cz
      do k=1,nf
         j=ifound(k)
         call closest_pt_tri_lg(cx,cy,cz,
     &      xs(itri(1,j)),ys(itri(1,j)),zs(itri(1,j)),
     &      xs(itri(2,j)),ys(itri(2,j)),zs(itri(2,j)),
     &      xs(itri(3,j)),ys(itri(3,j)),zs(itri(3,j)),
     &      rx,ry,rz,d2)
         if (d2.lt.dmin2) then
            dmin2=d2
            tx=rx
            ty=ry
            tz=rz
         endif
      enddo
      return
      end
C
C
      subroutine medial_axis_widen(px,py,pz,cx,cy,cz,r,qx,qy,qz,eps,
     &   xs,ys,zs,itri,linkt,sbox,ifound,wk,ang)
C
C #####################################################################
C
C     PURPOSE -
C
C     The object angle is the widest angle at the center C between
C     P and the points where the ball (radius R) touches the
C     surface.  A ball can touch along a curve (a ring, on a surface
C     of revolution) and the shrink finds only one of its points,
C     so look across: the points of the ball at 12, 24 and 36
C     degrees round the far end of the diameter through P (8 each),
C     and the surface point nearest each.  One that touches (within
C     R TOUCH of the ball) and is seen at a wider angle raises ANG
C     (degrees) and becomes Q.
C
C #####################################################################
C
      implicit none
      integer itri(3,*),linkt(*),ifound(*)
      real*8 px,py,pz,cx,cy,cz,r,qx,qy,qz,eps,xs(*),ys(*),zs(*),
     &   sbox(*),wk(*),ang
C
      integer iring,iaz
      real*8 ux,uy,uz,ax,ay,az,bx,by,bz,d,ca,deg,touch,th,ph,
     &   yx,yy,yz,zx,zy,zz,sx,sy,sz
      parameter (touch=5.0d-3)
C
      deg=45.0d0/atan(1.0d0)
      ux=(px-cx)/r
      uy=(py-cy)/r
      uz=(pz-cz)/r
C.... two unit vectors normal to U
      if (abs(ux).lt.0.9d0) then
         ax=0.0d0
         ay=-uz
         az=uy
      else
         ax=uz
         ay=0.0d0
         az=-ux
      endif
      d=sqrt(ax*ax+ay*ay+az*az)
      ax=ax/d
      ay=ay/d
      az=az/d
      bx=uy*az-uz*ay
      by=uz*ax-ux*az
      bz=ux*ay-uy*ax
      do iring=1,3
         th=dble(iring)*12.0d0/deg
         do iaz=0,7
            ph=dble(iaz)*45.0d0/deg
            sx=-ux*cos(th)+sin(th)*(cos(ph)*ax+sin(ph)*bx)
            sy=-uy*cos(th)+sin(th)*(cos(ph)*ay+sin(ph)*by)
            sz=-uz*cos(th)+sin(th)*(cos(ph)*az+sin(ph)*bz)
            yx=cx+r*sx
            yy=cy+r*sy
            yz=cz+r*sz
            call medial_axis_near(yx,yy,yz,eps,xs,ys,zs,itri,linkt,
     &         sbox,ifound,wk,zx,zy,zz)
            d=sqrt((zx-cx)**2+(zy-cy)**2+(zz-cz)**2)
            if (d.le.r*(1.0d0+touch) .and. d.gt.0.0d0) then
               ca=(ux*(zx-cx)+uy*(zy-cy)+uz*(zz-cz))/d
               ca=max(-1.0d0,min(1.0d0,ca))
               if (acos(ca)*deg.gt.ang) then
                  ang=acos(ca)*deg
                  qx=zx
                  qy=zy
                  qz=zz
               endif
            endif
         enddo
      enddo
      return
      end
