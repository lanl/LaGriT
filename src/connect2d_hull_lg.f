      subroutine connect2d_hull_lg(cmo,ierror)
C #####################################################################
C     PURPOSE -
C        Last step of connect (2D): fill the triangulation out to the
C        convex hull of its vertices and flip it to a Delaunay
C        triangulation (lg_connect2d_hull, in lg_connect2d_hull.c).
C        The enclosing triangle connect starts from is close enough to
C        the points that thin triangles along the hull are lost when
C        it is removed; this puts them back.
C     INPUT ARGUMENTS -
C        cmo    - the triangle mesh object connect just made
C     OUTPUT ARGUMENTS -
C        ierror - 0, or the code from lg_connect2d_hull (reported)
C #####################################################################
      implicit none
      include 'chydro.h'
      include 'local_element.h'
      character*(*) cmo
      integer ierror
C
      pointer (ipitp1,itp1)
      integer itp1(*)
      pointer (ipxic,xic),(ipyic,yic),(ipzic,zic)
      real*8 xic(*),yic(*),zic(*)
      pointer (ipitet,itet1),(ipjtet,jtet1)
      integer itet1(*),jtet1(*)
      pointer (ipitetclr,itetclr),(ipitettyp,itettyp)
      integer itetclr(*),itettyp(*)
      pointer (ipitetoff,itetoff),(ipjtetoff,jtetoff)
      integer itetoff(*),jtetoff(*)
      pointer (ipisint,isint)
      integer isint(*)
      integer npoints,ntets,ntri,maxtri,mbndry,leni,itype,ier,i,it,
     *  iflip,nadded,nflips,icscode
      logical itsttp
      character*32 isubname
      character*132 logmess
C
      isubname='connect2d_hull_lg'
      ierror=0
      call cmo_get_info('nnodes',cmo,npoints,leni,itype,ier)
      call cmo_get_info('nelements',cmo,ntets,leni,itype,ier)
      if (npoints.lt.3 .or. ntets.lt.1) return
      call cmo_get_info('mbndry',cmo,mbndry,leni,itype,ier)
      call cmo_get_info('itp1',cmo,ipitp1,leni,itype,ier)
      call mmgetblk('isint',isubname,ipisint,npoints,1,icscode)
      do i=1,npoints
         isint(i)=0
         if (itsttp('intrface',itp1(i))) isint(i)=1
      enddo
C     room for the added triangles: at most one per vertex. Only the
C     attributes whose length is nelements grow (hull_grow_elements, in
C     connect3d_hull_lg.f: never shrink, never a vertex array);
C     cmo_newlen is not used, because it resizes every array to the
C     mesh object's counts and can move arrays the caller still holds
C     (Nov 4 2026: macOS failures with the 3D fill).
      maxtri=ntets+npoints
      call hull_grow_elements(cmo,ntets,maxtri)
      call cmo_get_info('xic',cmo,ipxic,leni,itype,ier)
      call cmo_get_info('yic',cmo,ipyic,leni,itype,ier)
      call cmo_get_info('zic',cmo,ipzic,leni,itype,ier)
      call cmo_get_info('itet',cmo,ipitet,leni,itype,ier)
      call cmo_get_info('jtet',cmo,ipjtet,leni,itype,ier)
      call cmo_get_info('itetclr',cmo,ipitetclr,leni,itype,ier)
      call cmo_get_info('itettyp',cmo,ipitettyp,leni,itype,ier)
      call cmo_get_info('itetoff',cmo,ipitetoff,leni,itype,ier)
      call cmo_get_info('jtetoff',cmo,ipjtetoff,leni,itype,ier)
      ntri=ntets
      iflip=1
      call lg_connect2d_hull(npoints,ntri,maxtri,itet1,itetclr,jtet1,
     *   mbndry,isint,xic,yic,zic,iflip,nadded,nflips,ierror)
      do it=ntets+1,ntri
         itettyp(it)=ifelmtri
         itetoff(it)=3*(it-1)
         jtetoff(it)=3*(it-1)
      enddo
      call cmo_set_info('nelements',cmo,ntri,1,1,ier)
      call mmrelprt(isubname,icscode)
C
      write(logmess,'(a,i10,a)') ' connect: added',nadded,
     *   ' triangles to reach the convex hull'
      call writloga('default',0,logmess,0,ier)
      write(logmess,'(a,i10,a)') ' connect: flipped',nflips,
     *   ' edges to make the triangulation Delaunay'
      call writloga('default',0,logmess,0,ier)
      if (ierror.eq.1) then
         write(logmess,'(a)') ' WARNING: connect: the boundary is'//
     *      ' not one simple loop; hull not filled'
         call writloga('default',0,logmess,0,ier)
      elseif (ierror.eq.2) then
         write(logmess,'(a)') ' WARNING: connect: an edge is in 3 or'//
     *      ' more triangles; hull not filled'
         call writloga('default',0,logmess,0,ier)
      elseif (ierror.eq.3) then
         write(logmess,'(a)') ' WARNING: connect: a hull pocket'//
     *      ' could not be filled completely'
         call writloga('default',0,logmess,0,ier)
      elseif (ierror.eq.4) then
         write(logmess,'(a)') ' WARNING: connect: no memory to fill'//
     *      ' the hull'
         call writloga('default',0,logmess,0,ier)
      endif
      return
      end
