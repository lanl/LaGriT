      subroutine distance_field_facet(mo_sink,mo_src,cattribute,
     &   ierror)
C
C #####################################################################
C
C     PURPOSE -
C
C     compute / distance_field / mo_sink / mo_src / dfield / facet
C
C     For every vertex of MO_SINK, the exact distance to the nearest
C     point of the cells (facets) of MO_SRC: inside a triangle, on
C     an edge or at a vertex.  MO_SRC may hold triangles and line
C     segments, or quads only.  Quads are first cut into 4 triangles
C     at their centers with hextotet / 4 into a temporary mesh
C     object.  The result goes in the VDOUBLE attribute CATTRIBUTE.
C
C     The candidate triangles of each query come from the k-D tree
C     of kdtree.f / nearestpoint.f; closest_pt_tri_lg.f measures
C     each one exactly.
C
C     INPUT  - mo_sink, mo_src, cattribute (names)
C     OUTPUT - ierror (0 = ok)
C
C #####################################################################
C
      implicit none
      include 'local_element.h'
C
      character*(*) mo_sink, mo_src, cattribute
      integer ierror
C
      pointer (ipxs,xs),(ipys,ys),(ipzs,zs)
      pointer (ipitet,itet),(ipitetoff,itetoff),(ipitettyp,itettyp)
      pointer (ipxk,xk),(ipyk,yk),(ipzk,zk),(ipdf,df)
      pointer (ipitri,itri),(iplinkt,linkt),(ipsbox,sbox)
      pointer (ipitfound,itfound),(ipwork,work)
      real*8 xs(*),ys(*),zs(*),xk(*),yk(*),zk(*),df(*),sbox(*),
     &   work(*)
      integer itet(*),itetoff(*),itettyp(*),itri(3,*),linkt(*),
     &   itfound(*)
C
      integer nsrc,nelem,nsink,ntri,nlin,nqud,nother,ilen,ityp,
     &   ier,i,j,k,it,ioff,nfound,icscode,len1,len2,len3
      real*8 eps,diag,qx,qy,qz,dmin2,d2,rx,ry,rz,xp,yp,zp,dmax
      character*32 isubname,cmosrc,cmocur
      character*132 logmess
      character*256 cmdmess
      logical ltmp
      integer icharlnf
C
      isubname='distance_field_facet'
      ierror=0
      ltmp=.false.
      cmosrc=mo_src
      len1=icharlnf(mo_sink)
      len3=icharlnf(cattribute)
      call cmo_get_name(cmocur,ier)
C
C.... Count the cell types of the source.
C
      call cmo_get_info('nelements',cmosrc,nelem,ilen,ityp,ier)
      if (ier.ne.0 .or. nelem.le.0) then
         write(logmess,'(a)')
     &   'ERROR distance_field facet: source has no cells.'
         call writloga('default',0,logmess,0,ier)
         ierror=-1
         goto 9999
      endif
      call cmo_get_info('itettyp',cmosrc,ipitettyp,ilen,ityp,ier)
      ntri=0
      nlin=0
      nqud=0
      nother=0
      do it=1,nelem
         if (itettyp(it).eq.ifelmtri) then
            ntri=ntri+1
         elseif (itettyp(it).eq.ifelmlin) then
            nlin=nlin+1
         elseif (itettyp(it).eq.ifelmqud) then
            nqud=nqud+1
         else
            nother=nother+1
         endif
      enddo
      if (nother.gt.0 .or. (nqud.gt.0 .and. ntri+nlin.gt.0)) then
         write(logmess,'(a)')
     &   'ERROR distance_field facet: source cells must be'
         call writloga('default',0,logmess,0,ier)
         write(logmess,'(a)')
     &   '  triangles and lines, or quads only.'
         call writloga('default',0,logmess,0,ier)
         ierror=-1
         goto 9999
      endif
C
C.... Quads: cut into 4 triangles at the center (temporary mo).
C
      if (nqud.gt.0) then
         cmosrc='mo_dfacet_tmp_lg'
         ltmp=.true.
         call cmo_exist(cmosrc,ier)
         if (ier.eq.0) then
            cmdmess='cmo/delete/mo_dfacet_tmp_lg ; finish'
            call dotask(cmdmess,ier)
         endif
         len2=icharlnf(mo_src)
         cmdmess='hextotet/4/mo_dfacet_tmp_lg/'//mo_src(1:len2)//
     &      ' ; finish'
         call dotask(cmdmess,ier)
         call cmo_exist(cmosrc,ier)
         if (ier.ne.0) then
            write(logmess,'(a)')
     &      'ERROR distance_field facet: hextotet / 4 failed.'
            call writloga('default',0,logmess,0,ier)
            ierror=-1
            ltmp=.false.
            goto 9999
         endif
         call cmo_get_info('nelements',cmosrc,nelem,ilen,ityp,ier)
         call cmo_get_info('itettyp',cmosrc,ipitettyp,ilen,ityp,ier)
         ntri=nelem
         nlin=0
      endif
C
C.... The result attribute on the sink (same as the vertex option).
C
      cmdmess = 'cmo/addatt/'// mo_sink(1:len1) // '/'
     &     // cattribute(1:len3) //
     &    '/VDOUBLE/scalar/nnodes/linear/' //
     &    'permanent/   /-1.0;finish'
      call dotask(cmdmess,ier)
C
C.... Triangle list: lines are stored as (a,b,b).
C
      call cmo_get_info('nnodes',cmosrc,nsrc,ilen,ityp,ier)
      call cmo_get_info('xic',cmosrc,ipxs,ilen,ityp,ier)
      call cmo_get_info('yic',cmosrc,ipys,ilen,ityp,ier)
      call cmo_get_info('zic',cmosrc,ipzs,ilen,ityp,ier)
      call cmo_get_info('itet',cmosrc,ipitet,ilen,ityp,ier)
      call cmo_get_info('itetoff',cmosrc,ipitetoff,ilen,ityp,ier)
      ntri=ntri+nlin
      call mmgetblk('itri',isubname,ipitri,3*ntri,1,icscode)
      call mmgetblk('linkt',isubname,iplinkt,2*ntri,1,icscode)
      call mmgetblk('sbox',isubname,ipsbox,12*ntri,2,icscode)
      call mmgetblk('itfound',isubname,ipitfound,ntri,1,icscode)
      call mmgetblk('work',isubname,ipwork,ntri,2,icscode)
      do it=1,nelem
         ioff=itetoff(it)
         itri(1,it)=itet(ioff+1)
         itri(2,it)=itet(ioff+2)
         if (itettyp(it).eq.ifelmlin) then
            itri(3,it)=itet(ioff+2)
         else
            itri(3,it)=itet(ioff+3)
         endif
      enddo
      call kdtree(xs,ys,zs,itri,ntri,linkt,sbox,ier)
C
C.... Length tolerance for the tree: 1e-8 of the box diagonal.
C.... sbox(2,3,node): node 1 is the box of the whole source.
C
      diag=sqrt((sbox(2)-sbox(1))**2+(sbox(4)-sbox(3))**2+
     &   (sbox(6)-sbox(5))**2)
      eps=1.0d-8*max(diag,1.0d-30)
C
C.... Every sink vertex: the nearest point over the candidates.
C.... The previous answer starts the next search (faster when the
C.... sink vertices come in spatial order).
C
      call cmo_get_info('nnodes',mo_sink,nsink,ilen,ityp,ier)
      call cmo_get_info('xic',mo_sink,ipxk,ilen,ityp,ier)
      call cmo_get_info('yic',mo_sink,ipyk,ilen,ityp,ier)
      call cmo_get_info('zic',mo_sink,ipzk,ilen,ityp,ier)
      call cmo_get_info(cattribute,mo_sink,ipdf,ilen,ityp,ier)
      qx=1.0d30
      qy=1.0d30
      qz=1.0d30
      dmax=0.0d0
      do i=1,nsink
         xp=xk(i)
         yp=yk(i)
         zp=zk(i)
         call nearestpoint(xp,yp,zp,xs,ys,zs,itri,qx,qy,qz,
     &      linkt,sbox,eps,work,nfound,itfound,ier)
         dmin2=1.0d60
         do k=1,nfound
            j=itfound(k)
            call closest_pt_tri_lg(xp,yp,zp,
     &         xs(itri(1,j)),ys(itri(1,j)),zs(itri(1,j)),
     &         xs(itri(2,j)),ys(itri(2,j)),zs(itri(2,j)),
     &         xs(itri(3,j)),ys(itri(3,j)),zs(itri(3,j)),
     &         rx,ry,rz,d2)
            if (d2.lt.dmin2) then
               dmin2=d2
               qx=rx
               qy=ry
               qz=rz
            endif
         enddo
         df(i)=sqrt(dmin2)
         dmax=max(dmax,df(i))
      enddo
C
      write(logmess,'(a,i10,a,i10,a)') 'distance_field facet:',
     &   nsink,' sink vertices,',ntri,' facets'
      call writloga('default',0,logmess,0,ier)
      write(logmess,'(a,1pe14.6)')
     &   'distance_field facet: largest distance ',dmax
      call writloga('default',0,logmess,0,ier)
C
 9999 continue
      call mmrelprt(isubname,icscode)
      if (ltmp) then
         cmdmess='cmo/delete/mo_dfacet_tmp_lg ; finish'
         call dotask(cmdmess,ier)
      endif
      if (cmocur(1:1).ne.' ') then
         cmdmess='cmo/select/'//cmocur(1:icharlnf(cmocur))//
     &      ' ; finish'
         call dotask(cmdmess,ier)
      endif
      return
      end
