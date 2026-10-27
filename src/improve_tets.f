      subroutine improve_tets(imsgin,xmsgin,cmsgin,msgtype,nwds,ierror)
C
C#######################################################################
C
C     PURPOSE -
C
C     IMPROVE_TETS raises the quality of the worst tets of a tet mesh
C     without adding or removing vertices, so the spatial variation of
C     the mesh resolution is kept. Quality is the smallest sine of the
C     six dihedral angles of a tet (quality / minsine), with the sine of
C     an obtuse angle weighted by OBTUSE. The method follows Klingner &
C     Shewchuk, "Aggressive Tetrahedral Mesh Improvement" (16th
C     International Meshing Roundtable, 2007), restricted to operations
C     that keep the vertex set:
C       - smoothing: each vertex of a poor tet moves to raise the worst
C         tet around it; boundary and material-interface vertices slide
C         within their plane or along their line, or stay fixed
C       - flips: edge removal (rings of 3 .. MAXRING tets, including the
C         3-2 and 4-4 flips) and multi-face removal (including the 2-3
C         flip), each made only if the worst tet it touches improves;
C         boundary and interface faces are never changed
C       - passes alternate smoothing and flips on the tets below a
C         target that is raised in stages up to TARGET.
C     The work is done in improve_tets_core.cpp; this routine reads the
C     command, passes the mesh arrays in and writes the result back.
C     No other LaGriT routine is changed by this command.
C
C     FORMAT -
C
C     improve_tets / mo / [pset,get,pname] / [keyword / value] ...
C
C        mo        the tet mesh object (-def- = the current one)
C        pset      only the vertices of the pset may move, and only tets
C                  whose vertices are all in the pset are reconnected
C        keywords  target    final target min sine (default 0.65)
C                  passes    maximum passes (default 40)
C                  maxring   largest ring for edge removal (default 7)
C                  maxmove   largest distance a vertex may move, in its
C                            local spacing (default 1.0)
C                  obtuse    weight on the sine of an obtuse angle
C                            (default 0.75; 1 = plain min sine)
C                  smooth / flip / multiface / slide / global   on|off
C                  progress  on|off: a line per pass while it runs, and
C                            one every 15 s inside a long pass (on)
C                  planetol  tolerance for coplanar boundary faces
C                            (default 1.e-6)
C
C     Vertices with a parent/child (doubled) interface chain (isn1 not
C     0) are held fixed. Cell attributes other than itetclr are not
C     carried over to the new tets (they keep the values of the old
C     cell numbers). jtet is rebuilt.
C
C     CHANGE HISTORY -
C
C        Oct 2026 new command
C        Oct 2026 progress lines (progress keyword); a value that is not
C                 a number, or not on/off, is refused with a message
C
C#######################################################################
C
      implicit none
      include 'local_element.h'
C
      integer nwds,ierror
      character*(*) cmsgin(nwds)
      integer imsgin(nwds),msgtype(nwds)
      real*8 xmsgin(nwds)
C
      pointer (ipimt1,imt1),(ipitp1,itp1),(ipicr1,icr1),(ipisn1,isn1)
      integer imt1(*),itp1(*),icr1(*),isn1(*)
      pointer (ipxic,xic),(ipyic,yic),(ipzic,zic)
      real*8 xic(*),yic(*),zic(*)
      pointer (ipitetclr,itetclr),(ipitettyp,itettyp),
     &   (ipitetoff,itetoff),(ipjtetoff,jtetoff),(ipitet,itet),
     &   (ipjtet,jtet)
      integer itetclr(*),itettyp(*),itetoff(*),jtetoff(*),itet(*),
     &   jtet(*)
      pointer (ipmpary,mpary)
      integer mpary(*)
      pointer (ipmov,mov)
      integer mov(*)
C
      character*32 cmo,isubname,psetname,key
      character*132 logmess
      integer nnodes,nelements,nnew,ilen,ityp,icscode,ierr,i,k,iw
      integer mpno,len_mpary
      integer iopt(8)
      real*8 ropt(4),stats(30)
      integer imsgp(3),msgtp(3)
      real*8 xmsgp(3)
      character*32 cmsgp(3)
      logical ifpset
      integer icharlnf
C
      isubname='improve_tets'
      ierror=0
C
C     defaults (iopt: passes, maxring, smooth, flip, slide, multiface,
C     global, progress; ropt: obtuse, target, maxmove, planetol)
      iopt(1)=40
      iopt(2)=7
      iopt(3)=1
      iopt(4)=1
      iopt(5)=1
      iopt(6)=1
      iopt(7)=1
      iopt(8)=1
      ropt(1)=0.75d0
      ropt(2)=0.65d0
      ropt(3)=1.0d0
      ropt(4)=1.0d-6
C
C.... the mesh object
      if (nwds.lt.2) then
         call cmo_get_name(cmo,ierr)
      else
         cmo=cmsgin(2)(1:icharlnf(cmsgin(2)))
         if (cmo(1:5).eq.'-def-' .or. cmo(1:5).eq.'-cmo-')
     &      call cmo_get_name(cmo,ierr)
      endif
      call cmo_exist(cmo,ierr)
      if (ierr.ne.0) then
         write(logmess,'(a,a)') 'improve_tets: no mesh object ',
     &      cmo(1:icharlnf(cmo))
         call writloga('default',0,logmess,0,ierr)
         ierror=1
         return
      endif
C
C.... pset and keywords; copy the pset tokens now, since any dotask
C     overwrites the command tokens
      ifpset=.false.
      iw=3
      if (nwds.ge.5 .and. msgtype(3).eq.3) then
         if (cmsgin(3)(1:4).eq.'pset') then
            ifpset=.true.
            do k=1,3
               imsgp(k)=imsgin(2+k)
               xmsgp(k)=xmsgin(2+k)
               cmsgp(k)=cmsgin(2+k)
               msgtp(k)=msgtype(2+k)
            enddo
            psetname=cmsgin(5)
            iw=6
         endif
      endif
      do while (iw.le.nwds)
         if (msgtype(iw).ne.3 .or. iw+1.gt.nwds) then
            write(logmess,'(a,i4,a)') 'improve_tets: token ',iw,
     &         ' should be a keyword followed by a value'
            call writloga('default',0,logmess,0,ierr)
            ierror=1
            return
         endif
         key=cmsgin(iw)
         if (key(1:icharlnf(key)).eq.'passes') then
            call improve_tets_ival(imsgin(iw+1),xmsgin(iw+1),
     &         msgtype(iw+1),iopt(1),ierror)
         elseif (key(1:icharlnf(key)).eq.'maxring') then
            call improve_tets_ival(imsgin(iw+1),xmsgin(iw+1),
     &         msgtype(iw+1),iopt(2),ierror)
         elseif (key(1:icharlnf(key)).eq.'smooth') then
            call improve_tets_onoff(cmsgin(iw+1),iopt(3),ierror)
         elseif (key(1:icharlnf(key)).eq.'flip') then
            call improve_tets_onoff(cmsgin(iw+1),iopt(4),ierror)
         elseif (key(1:icharlnf(key)).eq.'slide') then
            call improve_tets_onoff(cmsgin(iw+1),iopt(5),ierror)
         elseif (key(1:icharlnf(key)).eq.'multiface') then
            call improve_tets_onoff(cmsgin(iw+1),iopt(6),ierror)
         elseif (key(1:icharlnf(key)).eq.'global') then
            call improve_tets_onoff(cmsgin(iw+1),iopt(7),ierror)
         elseif (key(1:icharlnf(key)).eq.'progress') then
            call improve_tets_onoff(cmsgin(iw+1),iopt(8),ierror)
         elseif (key(1:icharlnf(key)).eq.'obtuse') then
            call improve_tets_rval(imsgin(iw+1),xmsgin(iw+1),
     &         msgtype(iw+1),ropt(1),ierror)
         elseif (key(1:icharlnf(key)).eq.'target') then
            call improve_tets_rval(imsgin(iw+1),xmsgin(iw+1),
     &         msgtype(iw+1),ropt(2),ierror)
         elseif (key(1:icharlnf(key)).eq.'maxmove') then
            call improve_tets_rval(imsgin(iw+1),xmsgin(iw+1),
     &         msgtype(iw+1),ropt(3),ierror)
         elseif (key(1:icharlnf(key)).eq.'planetol') then
            call improve_tets_rval(imsgin(iw+1),xmsgin(iw+1),
     &         msgtype(iw+1),ropt(4),ierror)
         else
            write(logmess,'(a,a)') 'improve_tets: unknown keyword ',
     &         key(1:icharlnf(key))
            call writloga('default',0,logmess,0,ierr)
            ierror=2
         endif
         if (ierror.eq.1) then
            write(logmess,'(a,a,a,a)') 'improve_tets: bad value ',
     &         cmsgin(iw+1)(1:icharlnf(cmsgin(iw+1))),' for ',
     &         key(1:icharlnf(key))
            call writloga('default',0,logmess,0,ierr)
         endif
         if (ierror.ne.0) then
            ierror=1
            return
         endif
         iw=iw+2
      enddo
      if (ropt(2).le.0.0d0 .or. ropt(2).gt.0.95d0 .or.
     &    ropt(1).le.0.0d0 .or. ropt(1).gt.1.0d0 .or.
     &    ropt(3).le.0.0d0 .or. iopt(1).lt.1 .or. iopt(2).lt.3) then
         write(logmess,'(a)') 'improve_tets: need 0 < target <= 0.95,'
     &      //' 0 < obtuse <= 1, maxmove > 0, passes >= 1, maxring >= 3'
         call writloga('default',0,logmess,0,ierr)
         ierror=1
         return
      endif
C
C.... the mesh
      call cmo_get_info('nnodes',cmo,nnodes,ilen,ityp,ierr)
      call cmo_get_info('nelements',cmo,nelements,ilen,ityp,ierr)
      call cmo_get_stdptrs(cmo,ipimt1,ipitp1,ipicr1,ipisn1,ipxic,
     &   ipyic,ipzic,ipitetclr,ipitettyp,ipitetoff,ipjtetoff,ipitet,
     &   ipjtet,ierr)
      if (nelements.lt.1) then
         write(logmess,'(a)') 'improve_tets: the mesh has no cells'
         call writloga('default',0,logmess,0,ierr)
         ierror=1
         return
      endif
      do i=1,nelements
         if (itettyp(i).ne.ifelmtet .or. itetoff(i).ne.4*(i-1)) then
            write(logmess,'(a,i10,a)') 'improve_tets: cell ',i,
     &         ' is not a tet (only tet meshes are supported)'
            call writloga('default',0,logmess,0,ierr)
            ierror=1
            return
         endif
      enddo
C
C.... which vertices may move
      call mmgetblk('mov',isubname,ipmov,max(nnodes,1),1,icscode)
      if (ifpset) then
         do i=1,nnodes
            mov(i)=0
         enddo
         len_mpary=max(nnodes,1)
         call mmgetblk('mpary',isubname,ipmpary,len_mpary,1,icscode)
         call cmo_select(cmo,ierr)
         call getmpary(imsgp,xmsgp,cmsgp,msgtp,ipmpary,mpno,
     &      psetname,ierr)
         do k=1,mpno
            mov(mpary(k))=1
         enddo
      else
         do i=1,nnodes
            mov(i)=1
         enddo
      endif
C     doubled (parent/child) interface vertices stay where they are
      do i=1,nnodes
         if (isn1(i).ne.0) mov(i)=0
      enddo
C
      call improve_tets_run(nnodes,nelements,xic,yic,zic,itet,
     &   itetclr,mov,iopt,ropt,nnew,stats,ierr)
      if (ierr.ne.0) then
         if (ierr.eq.2) then
            write(logmess,'(a)') 'improve_tets: the mesh has a tet '
     &         //'with zero or negative volume; nothing done'
         else
            write(logmess,'(a,i4)') 'improve_tets: error ',ierr
         endif
         call writloga('default',0,logmess,0,ierr)
         call mmrelprt(isubname,icscode)
         ierror=1
         return
      endif
C
C.... new number of tets: resize the mesh object, copy the tets back,
C     rebuild jtet
      if (nnew.ne.nelements) then
         call cmo_set_info('nelements',cmo,nnew,1,1,ierr)
         call cmo_newlen(cmo,ierr)
      endif
      call cmo_get_stdptrs(cmo,ipimt1,ipitp1,ipicr1,ipisn1,ipxic,
     &   ipyic,ipzic,ipitetclr,ipitettyp,ipitetoff,ipjtetoff,ipitet,
     &   ipjtet,ierr)
      call improve_tets_fetch(itet,itetclr,ierr)
      do i=1,nnew
         itettyp(i)=ifelmtet
         itetoff(i)=4*(i-1)
         jtetoff(i)=4*(i-1)
      enddo
      call geniee_cmo(cmo)
C
C.... report
      write(logmess,'(a)') 'improve_tets: tet quality (min sine of '
     &   //'the dihedral angles, obtuse weighted)'
      call writloga('default',1,logmess,0,ierr)
      write(logmess,'(a,f8.4,a,f8.4,a,f8.4,a,f8.4)')
     &   '  worst  ',stats(1),' ->',stats(6),'   mean ',stats(2),
     &   ' ->',stats(7)
      call writloga('default',0,logmess,0,ierr)
      write(logmess,'(a,f7.2,a,f7.2,a,f7.2,a,f7.2,a)')
     &   '  dihedral angles (degrees): min ',stats(4),' ->',stats(9),
     &   '   max ',stats(5),' ->',stats(10),''
      call writloga('default',0,logmess,0,ierr)
      write(logmess,'(a,f6.3,a,i10,a,i10)') '  tets below target ',
     &   ropt(2),': ',nint(stats(3)),' ->',nint(stats(8))
      call writloga('default',0,logmess,0,ierr)
      write(logmess,'(a,i10,a,i10,a,i6)') '  tets ',nint(stats(19)),
     &   ' ->',nint(stats(20)),'   passes',nint(stats(11))
      call writloga('default',0,logmess,0,ierr)
      write(logmess,'(a,i10,a,i8,a,i8)') '  vertex moves',
     &   nint(stats(12)),'   edge removals',nint(stats(13)),
     &   '   face removals',nint(stats(14))
      call writloga('default',0,logmess,0,ierr)
      write(logmess,'(a,4i9)') '  vertices free/plane/line/fixed:',
     &   (nint(stats(k)),k=15,18)
      call writloga('default',0,logmess,0,ierr)
C     resolution: h = mean length of the edges at a vertex in the input
      write(logmess,'(a)') '  resolution (h = mean length of the edges'
     &   //' at a vertex, before):'
      call writloga('default',0,logmess,0,ierr)
      write(logmess,'(a,f7.3,a,f7.3)') '    vertex move / h:  median',
     &   stats(26),'   largest',stats(27)
      call writloga('default',0,logmess,0,ierr)
      write(logmess,'(a,f7.3,a,f7.3,a,f7.3)')
     &   '    h after / h before:  1%',stats(28),'   median',stats(29),
     &   '   99%',stats(30)
      call writloga('default',0,logmess,1,ierr)
C
      call mmrelprt(isubname,icscode)
      return
      end
C
C     improve_tets_ival / _rval: the value of a token as an integer /
C     real, whichever way it was typed (3 or 3.0); ierror = 1 for a
C     token that is not a number
      subroutine improve_tets_ival(imsg,xmsg,mtype,ival,ierror)
      implicit none
      integer imsg,mtype,ival,ierror
      real*8 xmsg
      if (mtype.eq.1) then
         ival=imsg
      elseif (mtype.eq.2) then
         ival=nint(xmsg)
      else
         ierror=1
      endif
      return
      end
C
      subroutine improve_tets_rval(imsg,xmsg,mtype,rval,ierror)
      implicit none
      integer imsg,mtype,ierror
      real*8 xmsg,rval
      if (mtype.eq.2) then
         rval=xmsg
      elseif (mtype.eq.1) then
         rval=dble(imsg)
      else
         ierror=1
      endif
      return
      end
C
      subroutine improve_tets_onoff(cword,ival,ierror)
      implicit none
      character*(*) cword
      integer ival,ierror
      if (cword(1:2).eq.'on' .or. cword(1:3).eq.'yes') then
         ival=1
      elseif (cword(1:3).eq.'off' .or. cword(1:2).eq.'no') then
         ival=0
      else
         ierror=1
      endif
      return
      end
C
C     improve_tets_progress: one progress line, called from
C     improve_tets_core.cpp while it runs (see note() there).
C       kind 1 start       iv: vertices, tets, below; rv: worst, target
C            2 opening flip pass starts
C            3 opening flip pass done   iv: flips
C            4 a pass done  iv: pass, below, moves, flips;
C                           rv: target of the pass, worst
C            5 still working iv: pass (0 = opening), phase (1 smoothing,
C                           2 flips, 3 opening flips), percent
C            6 finished     iv: passes
C            7 the heading of the pass table
C       rv(3) is the time since the start, in seconds
      subroutine improve_tets_progress(kind,iv,rv)
      implicit none
      integer kind,iv(4)
      real*8 rv(3)
      character*132 logmess
      character*16 cphase
      integer ierr
      if (kind.eq.1) then
         write(logmess,'(a,i0,a,i0,a)') 'improve_tets: ',iv(1),
     &      ' vertices, ',iv(2),' tets'
         call writloga('default',0,logmess,0,ierr)
         write(logmess,'(a,f6.4,a,i0,a,f4.2)')
     &      '  worst quality ',rv(1),'; ',iv(3),' tets below target ',
     &      rv(2)
         call writloga('default',0,logmess,0,ierr)
      elseif (kind.eq.2) then
         write(logmess,'(a)') '  opening flip pass over all tets ...'
         call writloga('default',0,logmess,0,ierr)
      elseif (kind.eq.3) then
         write(logmess,'(a,i0,a,f0.1,a)') '  opening flip pass done: ',
     &      iv(1),' flips, ',rv(3),' s'
         call writloga('default',0,logmess,0,ierr)
      elseif (kind.eq.7) then
         write(logmess,'(a)') '  pass  target   worst     below'
     &      //'     moves     flips    time (s)'
         call writloga('default',0,logmess,0,ierr)
      elseif (kind.eq.4) then
         write(logmess,'(i6,f8.2,f8.4,3i10,f12.1)') iv(1),rv(1),rv(2),
     &      iv(2),iv(3),iv(4),rv(3)
         call writloga('default',0,logmess,0,ierr)
      elseif (kind.eq.5) then
         cphase='smoothing'
         if (iv(2).eq.2) cphase='flips'
         if (iv(2).eq.3) cphase='opening flips'
         if (iv(1).eq.0) then
            write(logmess,'(a,a,a,i4,a,f9.1,a)') '    ... ',
     &         cphase(1:len_trim(cphase)),',',iv(3),'% done',rv(3),' s'
         else
            write(logmess,'(a,i4,a,a,a,i4,a,f9.1,a)') '    ... pass',
     &         iv(1),', ',cphase(1:len_trim(cphase)),',',iv(3),
     &         '% done',rv(3),' s'
         endif
         call writloga('default',0,logmess,0,ierr)
      elseif (kind.eq.6) then
         write(logmess,'(a,i0,a,f0.1,a)') 'improve_tets: finished, ',
     &      iv(1),' passes, ',rv(3),' s'
         call writloga('default',0,logmess,0,ierr)
      endif
      return
      end
