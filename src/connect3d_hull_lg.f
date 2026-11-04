      subroutine connect3d_hull_lg(cmo,ntets,ierror)
C #####################################################################
C     PURPOSE -
C        Last step of connect (3D): add the Delaunay tets between the
C        mesh and the convex hull of its vertices (lg_connect3d_hull,
C        in lg_connect3d_hull.c). The enclosing tet connect starts from
C        is close enough to the points that thin tets along the hull
C        are lost when it is removed; this puts them back.
C        Called by connect after the enclosing tet is removed and
C        before interface faces are marked in jtet.
C     INPUT ARGUMENTS -
C        cmo    - the tet mesh object connect just made
C        ntets  - its number of tets
C     OUTPUT ARGUMENTS -
C        ntets  - with the tets added
C        ierror - 0, or the code from lg_connect3d_hull (reported)
C #####################################################################
      implicit none
      include 'chydro.h'
      character*(*) cmo
      integer ntets,ierror
C
      pointer (ipitp1,itp1)
      integer itp1(*)
      pointer (ipimt1,imt1)
      integer imt1(*)
      pointer (ipxic,xic),(ipyic,yic),(ipzic,zic)
      real*8 xic(*),yic(*),zic(*)
      pointer (ipitet,itet1),(ipjtet,jtet1)
      integer itet1(*),jtet1(*)
      pointer (ipisint,isint)
      integer isint(*)
      integer npoints,maxtet,leni,itype,ier,i,nadded,nskip,icscode,
     *   mbndry,ntry,nadd1,nskip1,inohull
      logical itsttp
      character*32 isubname
      character*132 logmess
C
      isubname='connect3d_hull_lg'
      ierror=0
      call cmo_get_info('nnodes',cmo,npoints,leni,itype,ier)
      if (npoints.lt.4 .or. ntets.lt.1) return
C     connect / nohull: leave the mesh as the enclosing tet left it
      call lg_connect_nohull_get(inohull)
      if (inohull.ne.0) return
      call cmo_get_info('itp1',cmo,ipitp1,leni,itype,ier)
      call cmo_get_info('mbndry',cmo,mbndry,leni,itype,ier)
      call mmgetblk('isint',isubname,ipisint,npoints,1,icscode)
      do i=1,npoints
         isint(i)=0
         if (itsttp('intrface',itp1(i))) isint(i)=1
      enddo
C     room for the added tets; if it runs out (ierror 5, the mesh is
C     valid but not yet filled), more room and go on from there
      nadded=0
      nskip=0
      ntry=0
      ierror=5
C     First with no room for new tets: when there is nothing to add
C     (the usual case for a convex or exact-grid point set), no memory
C     is touched. When it runs out of room (ierror 5), only the arrays
C     whose length is nelements grow (hull_grow_elements: never shrink,
C     never a vertex array); cmo_newlen is NOT used here, because in
C     the middle of connect it would also resize the vertex arrays to a
C     count that is not final yet, and move arrays connect still holds
C     (Nov 4 2026: refine_recon_quality failed on macOS).
      maxtet=ntets
      do while (ierror.eq.5 .and. ntry.lt.7)
         if (ntry.gt.0) then
            maxtet=ntets+max(1000,npoints/2)*2**(ntry-1)
            call hull_grow_elements(cmo,ntets,maxtet)
         endif
         ntry=ntry+1
         call cmo_get_info('xic',cmo,ipxic,leni,itype,ier)
         call cmo_get_info('yic',cmo,ipyic,leni,itype,ier)
         call cmo_get_info('zic',cmo,ipzic,leni,itype,ier)
         call cmo_get_info('imt1',cmo,ipimt1,leni,itype,ier)
         call cmo_get_info('itet',cmo,ipitet,leni,itype,ier)
         call cmo_get_info('jtet',cmo,ipjtet,leni,itype,ier)
         call lg_connect3d_hull(npoints,ntets,maxtet,itet1,jtet1,
     *      xic,yic,zic,imt1,isint,mbndry,nadd1,nskip1,ierror)
         nadded=nadded+nadd1
         nskip=nskip1
      enddo
      if (ntry.gt.1) call cmo_set_info('nelements',cmo,ntets,1,1,ier)
      call mmrelprt(isubname,icscode)
C
      write(logmess,'(a,i10,a)') ' connect: added',nadded,
     *   ' tets to reach the convex hull'
      call writloga('default',0,logmess,0,ier)
      if (nskip.gt.0) then
         write(logmess,'(a,i10,a)') ' connect:',nskip,
     *      ' hull pockets left open (flat, tied or two materials)'
         call writloga('default',0,logmess,0,ier)
      endif
      if (ierror.eq.1) then
         write(logmess,'(a)') ' WARNING: connect: a face is in 3 or'//
     *      ' more tets; hull not filled'
         call writloga('default',0,logmess,0,ier)
      elseif (ierror.eq.4) then
         write(logmess,'(a)') ' WARNING: connect: no memory to fill'//
     *      ' the hull'
         call writloga('default',0,logmess,0,ier)
      elseif (ierror.eq.5) then
         write(logmess,'(a)') ' WARNING: connect: no room to fill'//
     *      ' the whole hull'
         call writloga('default',0,logmess,0,ier)
      endif
      return
      end
C
      subroutine hull_grow_elements(cmo,ntets,maxtet)
C     every memory-managed attribute of cmo whose length is nelements
C     grows to at least maxtet (times its rank); none shrinks, and the
C     vertex attributes are left alone. mmnewlen keeps the contents and
C     zeroes the new part. nelements is left at ntets.
      implicit none
      character*(*) cmo
      integer ntets,maxtet
      integer nmcmoatt,i,ilen,itype,ics,index,length,irank,len,need
      character*32 cname,ctype,crank,clen,cinter,cpers,cio
      pointer (ipa,ia)
      integer ia(*)
      call cmo_set_info('nelements',cmo,maxtet,1,1,ics)
      call cmo_get_info('number_of_attributes',cmo,nmcmoatt,
     *   ilen,itype,ics)
      do i=1,nmcmoatt
         call cmo_get_attribute_name(cmo,i,cname,ics)
         call cmo_get_attparam(cname,cmo,index,ctype,crank,
     *      clen,cinter,cpers,cio,ics)
         if (ctype(1:1).eq.'V' .and. clen.eq.'nelements') then
            call cmo_get_length(cname,cmo,length,irank,ics)
            need=max(1,irank*length)
            call mmfindbk(cname,cmo,ipa,len,ics)
            if (ics.eq.0 .and. len.lt.need)
     *         call mmnewlen(cname,cmo,ipa,need,ics)
         endif
      enddo
      call cmo_set_info('nelements',cmo,ntets,1,1,ics)
      return
      end
