      subroutine hfield_lg(imsgin,xmsgin,cmsgin,msgtype,nwds,ierror)
C
C #####################################################################
C
C     PURPOSE -
C
C     hfield: a named description of the spacing h, built from a few
C     sources, for createpts / poisson_disk (keyword hfield, NAME) or
C     for any mesh object (hfield / eval).
C
C     hfield / create / hf_name / h_floor, F / h_ceiling, C
C                                              [/ h_growth, G]
C     hfield / add / hf_name / near, mo_src [/ h_value, H |
C              h_attribute, ATT [/ h_factor, A]] [/ h_band, B]
C              [/ h_growth, G]
C     hfield / add / hf_name / inside, surface / mo_surf
C              [/ h_value, H] [/ h_band, B] [/ h_growth, G]
C     hfield / list / hf_name
C     hfield / eval / hf_name / mo / att
C     hfield / dump / hf_name / file.inp [/ n]
C     hfield / delete / hf_name
C
C     For each source i, with d_i the distance to it (0 inside the
C     surface for inside):
C
C        h_i = H_i + h_growth_i * max(0, d_i - h_band_i)
C        h   = min(h_ceiling, max(h_floor, smallest h_i))
C
C     Storage (mmgetblk, partition hfield_lg), per hfield NAME:
C
C        hfr_NAME  real*8:   1 number of sources, 2 h_floor,
C                            3 h_ceiling, 4 h_growth (the default);
C                            then 8 per source, from 4+8*(k-1):
C                            +1 kind (1 near, 2 inside)
C                            +2 h_value
C                            +3 how H is set (0 h_floor, 1 h_value,
C                               2 h_attribute)
C                            +4 h_factor  +5 h_band  +6 h_growth
C                            +7, +8 not used
C        hfc_NAME  char*32:  2 per source: mesh object, attribute
C
C     So an hfield name has at most 28 characters.
C
C     INPUT ARGUMENTS - the parsed command (imsgin, xmsgin, cmsgin,
C                       msgtype, nwds)
C     OUTPUT ARGUMENTS - ierror (0 = ok)
C
C #####################################################################
C
      implicit none
      integer nwds,imsgin(nwds),msgtype(nwds),ierror
      real*8 xmsgin(nwds)
      character*(*) cmsgin(nwds)
C
      integer MAXTOK
      parameter (MAXTOK=40)
      character*32 ctok(MAXTOK),cact,hname,cmo,catt
      real*8 rtok(MAXTOK)
      integer ityp(MAXTOK),ntok,i,n,lenh
      character*132 cmess
C
C     The tokens are copied first: every dotask below parses into the
C     same arrays that came in.
C
      ierror=0
      if(nwds.gt.MAXTOK)then
         write(cmess,'(a,i4)') 'too many words, at most',MAXTOK
         call hf_err(cmess)
         ierror=1
         return
      endif
      ntok=nwds
      do i=1,ntok
         ityp(i)=msgtype(i)
         ctok(i)=' '
         rtok(i)=0.0d0
         if(msgtype(i).eq.1) rtok(i)=dble(imsgin(i))
         if(msgtype(i).eq.2) rtok(i)=xmsgin(i)
         if(msgtype(i).eq.3) ctok(i)=cmsgin(i)
      enddo
C
      if(ntok.lt.3 .or. ityp(2).ne.3 .or. ityp(3).ne.3)then
         call hf_err('usage: hfield / create|add|list|eval|dump|'//
     &      'delete / hf_name / ...')
         ierror=1
         return
      endif
      cact=ctok(2)
      hname=ctok(3)
      lenh=len_trim(hname)
      if(lenh.gt.28)then
         call hf_err('an hfield name has at most 28 characters: '//
     &      hname(1:lenh))
         ierror=1
         return
      endif
C
      if(cact.eq.'create')then
         call hf_create(hname,ctok,rtok,ityp,ntok,ierror)
      elseif(cact.eq.'add')then
         call hf_add(hname,ctok,rtok,ityp,ntok,ierror)
      elseif(cact.eq.'list')then
         call hf_list(hname,ierror)
      elseif(cact.eq.'eval')then
         if(ntok.ne.5 .or. ityp(4).ne.3 .or. ityp(5).ne.3)then
            call hf_err('usage: hfield / eval / hf_name / mo / att')
            ierror=1
            return
         endif
         cmo=ctok(4)
         catt=ctok(5)
         call hfield_eval(hname,cmo,catt,0.0d0,1,ierror)
      elseif(cact.eq.'dump')then
         n=21
         if(ntok.lt.4 .or. ntok.gt.5 .or. ityp(4).ne.3)then
            call hf_err('usage: hfield / dump / hf_name / '//
     &         'file.inp [/ n]')
            ierror=1
            return
         endif
         if(ntok.eq.5)then
            if(ityp(5).ne.1)then
               call hf_err('dump: n must be an integer')
               ierror=1
               return
            endif
            n=nint(rtok(5))
         endif
         call hf_dump(hname,ctok(4),n,ierror)
      elseif(cact.eq.'delete')then
         call hf_delete(hname,ierror)
      else
         call hf_err('unknown action '//cact(1:len_trim(cact))//
     &      '; the actions are create, add, list, eval, dump, delete')
         ierror=1
      endif
      return
      end
C
C SUBROUTINE BEGIN ####################################################
      subroutine hf_err(ctext)
C     An ERROR, wrapped at spaces so no line is longer than 80.
      implicit none
      character*(*) ctext
      character*132 cmess
      integer ier,n,i,j,k,lmax
      n=len_trim(ctext)
      i=1
      lmax=66
      cmess='ERROR hfield:'
      do while(i.le.n)
         j=min(n,i+lmax-1)
         if(j.lt.n)then
            k=j+1
            do while(k.gt.i .and. ctext(k:k).ne.' ')
               k=k-1
            enddo
            if(k.gt.i) j=k-1
         endif
         if(i.eq.1)then
            cmess='ERROR hfield: '//ctext(i:j)
         else
            cmess='   '//ctext(i:j)
         endif
         call writloga('default',0,cmess,0,ier)
         i=j+1
         do while(i.le.n .and. ctext(i:i).eq.' ')
            i=i+1
         enddo
         lmax=77
      enddo
      return
      end
C
C SUBROUTINE BEGIN ####################################################
      subroutine hf_say(ctext)
C     One line of output.
      implicit none
      character*(*) ctext
      character*132 cmess
      integer ier
      cmess=ctext
      call writloga('default',0,cmess,0,ier)
      return
      end
C
C SUBROUTINE BEGIN ####################################################
      subroutine hf_get(hname,ipr,ipc,nsrc,ierror)
C     The blocks of hfield hname, or an ERROR if it was not created.
      implicit none
      character*(*) hname
      pointer (ipr,hr),(ipc,hc)
      real*8 hr(*)
      character*32 hc(*)
      integer nsrc,ierror,ilen,ier,lenh
      lenh=len_trim(hname)
      call mmfindbk('hfr_'//hname(1:lenh),'hfield_lg',ipr,ilen,ier)
      if(ier.ne.0)then
         call hf_err('no hfield named '//hname(1:lenh)//
     &      ' (hfield / create makes one)')
         ierror=1
         nsrc=0
         return
      endif
      call mmfindbk('hfc_'//hname(1:lenh),'hfield_lg',ipc,ilen,ier)
      nsrc=nint(hr(1))
      ierror=0
      return
      end
C
C SUBROUTINE BEGIN ####################################################
      subroutine hfield_get_floor(hname,hfloor,ierror)
C     h_floor of hfield hname (for createpts / poisson_disk).
      implicit none
      character*(*) hname
      real*8 hfloor
      integer ierror,ilen
      pointer (ipr,hr)
      real*8 hr(*)
C     Quiet: the caller reports a missing hfield.
      hfloor=0.0d0
      call mmfindbk('hfr_'//hname(1:len_trim(hname)),'hfield_lg',
     &   ipr,ilen,ierror)
      if(ierror.eq.0) hfloor=hr(2)
      return
      end
C
C SUBROUTINE BEGIN ####################################################
      subroutine hf_create(hname,ctok,rtok,ityp,ntok,ierror)
      implicit none
      character*(*) hname
      integer ntok,ityp(ntok),ierror
      character*32 ctok(ntok),key
      real*8 rtok(ntok)
      pointer (ipr,hr),(ipc,hc)
      real*8 hr(*)
      character*32 hc(*)
      real*8 hfl,hce,hgr
      logical lfl,lce
      integer i,ilen,ier,lenh
      character*132 cmess
C
      ierror=0
      lenh=len_trim(hname)
      call mmfindbk('hfr_'//hname(1:lenh),'hfield_lg',ipr,ilen,ier)
      if(ier.eq.0)then
         call hf_err('hfield '//hname(1:lenh)//' already exists '//
     &      '(hfield / delete / '//hname(1:lenh)//' first)')
         ierror=1
         return
      endif
      lfl=.false.
      lce=.false.
      hgr=0.2d0
      i=4
      do while(i.le.ntok)
         if(ityp(i).ne.3)then
            write(cmess,'(a,i3,a)') 'create: word',i,
     &         ' should be h_floor, h_ceiling or h_growth'
            call hf_err(cmess)
            ierror=1
            return
         endif
         key=ctok(i)
         if(i+1.gt.ntok .or. ityp(min(i+1,ntok)).eq.3)then
            call hf_err('create: a number must follow '//
     &         key(1:len_trim(key)))
            ierror=1
            return
         endif
         if(key.eq.'h_floor')then
            hfl=rtok(i+1)
            lfl=.true.
         elseif(key.eq.'h_ceiling')then
            hce=rtok(i+1)
            lce=.true.
         elseif(key.eq.'h_growth')then
            hgr=rtok(i+1)
         else
            call hf_err('create: unknown keyword '//
     &         key(1:len_trim(key))//
     &         '; the keywords are h_floor, h_ceiling, h_growth')
            ierror=1
            return
         endif
         i=i+2
      enddo
      if(.not.lfl .or. .not.lce)then
         call hf_err('create: h_floor and h_ceiling are required')
         ierror=1
         return
      endif
      if(hfl.le.0.0d0)then
         call hf_err('create: h_floor must be greater than 0')
         ierror=1
      endif
      if(hce.lt.hfl)then
         call hf_err('create: h_ceiling must not be less than h_floor')
         ierror=1
      endif
      if(hgr.le.0.0d0)then
         call hf_err('create: h_growth must be greater than 0')
         ierror=1
      endif
      if(ierror.ne.0) return
C
      call mmgetblk('hfr_'//hname(1:lenh),'hfield_lg',ipr,4,2,ier)
      call mmgetblk('hfc_'//hname(1:lenh),'hfield_lg',ipc,2,3,ier)
      if(ier.ne.0)then
         call hf_err('create: no memory')
         ierror=1
         return
      endif
      hr(1)=0.0d0
      hr(2)=hfl
      hr(3)=hce
      hr(4)=hgr
      write(cmess,'(a,a,a,1pe10.3,a,1pe10.3,a,1pe10.3)')
     &   'hfield ',hname(1:lenh),': h_floor',hfl,'  h_ceiling',hce,
     &   '  h_growth',hgr
      call hf_say(cmess)
      return
      end
C
C SUBROUTINE BEGIN ####################################################
      subroutine hf_add(hname,ctok,rtok,ityp,ntok,ierror)
      implicit none
      character*(*) hname
      integer ntok,ityp(ntok),ierror
      character*32 ctok(ntok),key,csrc,catt
      real*8 rtok(ntok)
      pointer (ipr,hr),(ipc,hc)
      real*8 hr(*)
      character*32 hc(*)
      integer nsrc,kind,ihow,i,ier,lenh,ibase
      logical lval,latt,lfac
      real*8 hval,hfac,hband,hgr
      character*132 cmess
C
      call hf_get(hname,ipr,ipc,nsrc,ierror)
      if(ierror.ne.0) return
      lenh=len_trim(hname)
C
      kind=0
      if(ntok.ge.5 .and. ctok(4).eq.'near' .and. ityp(5).eq.3)then
         kind=1
         csrc=ctok(5)
         i=6
      elseif(ntok.ge.6 .and. ctok(4).eq.'inside' .and.
     &       ctok(5).eq.'surface' .and. ityp(6).eq.3)then
         kind=2
         csrc=ctok(6)
         i=7
      endif
      if(kind.eq.0)then
         call hf_err('add: a source is near, mo_src  or  '//
     &      'inside, surface / mo_surf')
         ierror=1
         return
      endif
C
      lval=.false.
      latt=.false.
      lfac=.false.
      hval=hr(2)
      hfac=1.0d0
      hband=0.0d0
      hgr=hr(4)
      catt=' '
      do while(i.le.ntok)
         key=ctok(i)
         if(ityp(i).ne.3 .or. i+1.gt.ntok)then
            write(cmess,'(a,i3,a)') 'add: word',i,
     &         ' should be a keyword followed by its value'
            call hf_err(cmess)
            ierror=1
            return
         endif
         if(key.eq.'h_attribute')then
            if(ityp(i+1).ne.3)then
               call hf_err('add: an attribute name must follow '//
     &            'h_attribute')
               ierror=1
               return
            endif
            catt=ctok(i+1)
            latt=.true.
         else
            if(ityp(i+1).eq.3)then
               call hf_err('add: a number must follow '//
     &            key(1:len_trim(key)))
               ierror=1
               return
            endif
            if(key.eq.'h_value')then
               hval=rtok(i+1)
               lval=.true.
            elseif(key.eq.'h_factor')then
               hfac=rtok(i+1)
               lfac=.true.
            elseif(key.eq.'h_band')then
               hband=rtok(i+1)
            elseif(key.eq.'h_growth')then
               hgr=rtok(i+1)
            else
               call hf_err('add: unknown keyword '//
     &            key(1:len_trim(key))//'; the keywords are '//
     &            'h_value, h_attribute, h_factor, h_band, h_growth')
               ierror=1
               return
            endif
         endif
         i=i+2
      enddo
C
      if(lval .and. latt)then
         call hf_err('add: give h_value or h_attribute, not both')
         ierror=1
      endif
      if(latt .and. kind.eq.2)then
         call hf_err('add: h_attribute is for near sources only')
         ierror=1
      endif
      if(lfac .and. .not.latt)then
         call hf_err('add: h_factor goes with h_attribute')
         ierror=1
      endif
      if(lval .and. hval.le.0.0d0)then
         call hf_err('add: h_value must be greater than 0')
         ierror=1
      endif
      if(lfac .and. hfac.le.0.0d0)then
         call hf_err('add: h_factor must be greater than 0')
         ierror=1
      endif
      if(hband.lt.0.0d0)then
         call hf_err('add: h_band must not be negative')
         ierror=1
      endif
      if(hgr.le.0.0d0)then
         call hf_err('add: h_growth must be greater than 0')
         ierror=1
      endif
      if(ierror.ne.0) return
      call hf_checksrc(kind,csrc,catt,latt,ierror)
      if(ierror.ne.0) return
C
      ihow=0
      if(lval) ihow=1
      if(latt) ihow=2
      call mmnewlen('hfr_'//hname(1:lenh),'hfield_lg',ipr,
     &   4+8*(nsrc+1),ier)
      call mmnewlen('hfc_'//hname(1:lenh),'hfield_lg',ipc,
     &   2*(nsrc+1),ier)
      if(ier.ne.0)then
         call hf_err('add: no memory')
         ierror=1
         return
      endif
      nsrc=nsrc+1
      hr(1)=dble(nsrc)
      ibase=4+8*(nsrc-1)
      hr(ibase+1)=dble(kind)
      hr(ibase+2)=hval
      hr(ibase+3)=dble(ihow)
      hr(ibase+4)=hfac
      hr(ibase+5)=hband
      hr(ibase+6)=hgr
      hr(ibase+7)=0.0d0
      hr(ibase+8)=0.0d0
      hc(2*nsrc-1)=csrc
      hc(2*nsrc)=catt
      call hf_srcline(hr,hc,nsrc,cmess)
      call hf_say('hfield '//hname(1:lenh)//' add:')
      call hf_say(cmess)
      return
      end
C
C SUBROUTINE BEGIN ####################################################
      subroutine hf_checksrc(kind,csrc,catt,latt,ierror)
C
C     A source must exist and have vertices. near: no cells, or only
C     triangles and lines, or only quads (as compute / distance_field
C     / facet). inside: a closed surface (hf_surf). h_attribute: a
C     VDOUBLE vertex attribute of the source.
C
      implicit none
      include 'local_element.h'
      integer kind,ierror
      character*(*) csrc,catt
      logical latt
      pointer (ipitettyp,itettyp)
      integer itettyp(*)
      integer nn,ne,ilen,ity,ier,it,ntri,nlin,nqud,noth,nvf,nf,ns,
     &   index,ls
      character*32 ctype,crank,clen,cinter,cpers,cio
C
      ierror=0
      ls=len_trim(csrc)
      call cmo_exist(csrc,ier)
      if(ier.ne.0)then
         call hf_err('mesh object '//csrc(1:ls)//' does not exist')
         ierror=1
         return
      endif
      call cmo_get_info('nnodes',csrc,nn,ilen,ity,ier)
      call cmo_get_info('nelements',csrc,ne,ilen,ity,ier)
      if(nn.le.0)then
         call hf_err('mesh object '//csrc(1:ls)//' has no vertices')
         ierror=1
         return
      endif
      if(kind.eq.1 .and. ne.gt.0)then
         call cmo_get_info('itettyp',csrc,ipitettyp,ilen,ity,ier)
         ntri=0
         nlin=0
         nqud=0
         noth=0
         do it=1,ne
            if(itettyp(it).eq.ifelmtri)then
               ntri=ntri+1
            elseif(itettyp(it).eq.ifelmlin)then
               nlin=nlin+1
            elseif(itettyp(it).eq.ifelmqud)then
               nqud=nqud+1
            else
               noth=noth+1
            endif
         enddo
         if(noth.gt.0 .or. (nqud.gt.0 .and. ntri+nlin.gt.0))then
            call hf_err('near: '//csrc(1:ls)//' must have no cells, '//
     &         'or triangles and lines only, or quads only')
            ierror=1
            return
         endif
      endif
      if(kind.eq.2)then
         call hf_surf(csrc,0,nvf,nf,ns,ierror)
         if(ierror.ne.0) return
      endif
      if(latt)then
         call cmo_get_attparam(catt,csrc,index,ctype,crank,clen,
     &      cinter,cpers,cio,ier)
         if(ier.ne.0 .or. ctype(1:7).ne.'VDOUBLE' .or.
     &      clen(1:6).ne.'nnodes' .or. crank(1:6).ne.'scalar')then
            call hf_err('h_attribute '//catt(1:len_trim(catt))//
     &         ' is not a VDOUBLE scalar vertex attribute of '//
     &         csrc(1:ls))
            ierror=1
            return
         endif
      endif
      return
      end
C
C SUBROUTINE BEGIN ####################################################
      subroutine hf_srcline(hr,hc,k,cline)
C     One source in words.
      implicit none
      real*8 hr(*)
      character*32 hc(*)
      integer k
      character*(*) cline
      integer ibase,kind,ihow,ls,la
      character*40 chow
      character*16 ckind
      ibase=4+8*(k-1)
      kind=nint(hr(ibase+1))
      ihow=nint(hr(ibase+3))
      ls=len_trim(hc(2*k-1))
      ckind='near'
      if(kind.eq.2) ckind='inside surface'
      if(ihow.eq.0)then
         chow='h_floor'
      elseif(ihow.eq.1)then
         write(chow,'(1pe10.3)') hr(ibase+2)
      else
         la=len_trim(hc(2*k))
         write(chow,'(1pe10.3,a,a)') hr(ibase+4),' * ',hc(2*k)(1:la)
      endif
      write(cline,'(i3,1x,a,1x,a,a,a,a,1pe10.3,a,1pe10.3)') k,
     &   ckind(1:len_trim(ckind)),hc(2*k-1)(1:ls),': H ',
     &   chow(1:len_trim(chow)),'  band',hr(ibase+5),
     &   '  growth',hr(ibase+6)
      return
      end
C
C SUBROUTINE BEGIN ####################################################
      subroutine hf_list(hname,ierror)
      implicit none
      character*(*) hname
      integer ierror
      pointer (ipr,hr),(ipc,hc)
      real*8 hr(*)
      character*32 hc(*)
      integer nsrc,k,lenh
      character*132 cmess
      call hf_get(hname,ipr,ipc,nsrc,ierror)
      if(ierror.ne.0) return
      lenh=len_trim(hname)
      write(cmess,'(a,a,a,1pe10.3,a,1pe10.3,a,1pe10.3)')
     &   'hfield ',hname(1:lenh),': h_floor',hr(2),'  h_ceiling',hr(3),
     &   '  h_growth',hr(4)
      call hf_say(cmess)
      write(cmess,'(a,i3,a)') '   h = min(h_ceiling, max(h_floor, '//
     &   'smallest h_i)), over',nsrc,' sources:'
      call hf_say(cmess)
      call hf_say('   h_i = H + growth * max(0, distance - band)')
      do k=1,nsrc
         call hf_srcline(hr,hc,k,cmess)
         call hf_say(cmess)
      enddo
      if(nsrc.eq.0) call hf_say('   (no sources: h is h_ceiling '//
     &   'everywhere)')
      return
      end
C
C SUBROUTINE BEGIN ####################################################
      subroutine hf_delete(hname,ierror)
      implicit none
      character*(*) hname
      integer ierror,nsrc,ier,lenh
      pointer (ipr,hr),(ipc,hc)
      real*8 hr(*)
      character*32 hc(*)
      call hf_get(hname,ipr,ipc,nsrc,ierror)
      if(ierror.ne.0) return
      lenh=len_trim(hname)
      call mmrelblk('hfr_'//hname(1:lenh),'hfield_lg',ipr,ier)
      call mmrelblk('hfc_'//hname(1:lenh),'hfield_lg',ipc,ier)
      call hf_say('hfield '//hname(1:lenh)//' deleted')
      return
      end
C
C SUBROUTINE BEGIN ####################################################
      subroutine hf_delatt(cmo,catt)
C     Delete attribute catt of cmo if it is there.
      implicit none
      character*(*) cmo,catt
      integer index,ier
      character*32 ctype,crank,clen,cinter,cpers,cio
      character*256 cbuf
      call cmo_get_attparam(catt,cmo,index,ctype,crank,clen,
     &   cinter,cpers,cio,ier)
      if(ier.eq.0)then
         cbuf='cmo/DELATT/'//cmo(1:len_trim(cmo))//'/'//
     &      catt(1:len_trim(catt))//' ; finish'
         call dotask(cbuf,ier)
      endif
      return
      end
C
C SUBROUTINE BEGIN ####################################################
      subroutine hfield_eval(hname,cmo,catt,hlow,iverb,ierror)
C
C     h of hfield hname at the vertices of cmo, into the VDOUBLE
C     attribute catt (made if it is not there). hlow raises the floor
C     (createpts / poisson_disk passes its h_spacing_scalar; the eval
C     action passes 0). iverb 1: one line with the range of h.
C
      implicit none
      character*(*) hname,cmo,catt
      real*8 hlow
      integer iverb,ierror
C
      pointer (ipr,hr),(ipc,hc)
      real*8 hr(*)
      character*32 hc(*)
      pointer (iphm,hmin),(ipd,dist),(ipa,aval),(ipho,hout)
      pointer (ipx,xic),(ipy,yic),(ipz,zic)
      real*8 hmin(*),dist(*),aval(*),hout(*),xic(*),yic(*),zic(*)
      integer nsrc,nn,ne,ilen,ity,ier,k,i,ibase,kind,ihow,lc,ls,la,
     &   lenh,index
      real*8 hfl,hce,hh,hfac,hband,hgr,hi,dd,hlo,hhi
      character*32 csrc,csatt,ccur,ctype,crank,clen,cinter,cpers,cio
      character*256 cbuf
      character*132 cmess
C
      call hf_get(hname,ipr,ipc,nsrc,ierror)
      if(ierror.ne.0) return
      lenh=len_trim(hname)
      lc=len_trim(cmo)
      call cmo_exist(cmo,ier)
      if(ier.ne.0)then
         call hf_err('eval: mesh object '//cmo(1:lc)//
     &      ' does not exist')
         ierror=1
         return
      endif
      call cmo_get_info('nnodes',cmo,nn,ilen,ity,ier)
      if(nn.le.0)then
         call hf_err('eval: mesh object '//cmo(1:lc)//
     &      ' has no vertices')
         ierror=1
         return
      endif
      call cmo_get_attparam(catt,cmo,index,ctype,crank,clen,
     &   cinter,cpers,cio,ier)
      if(ier.eq.0)then
         if(ctype(1:7).ne.'VDOUBLE' .or. clen(1:6).ne.'nnodes' .or.
     &      crank(1:6).ne.'scalar')then
            call hf_err('eval: '//catt(1:len_trim(catt))//
     &         ' is not a VDOUBLE scalar vertex attribute of '//
     &         cmo(1:lc))
            ierror=1
            return
         endif
      endif
C
      hfl=max(hr(2),hlow)
      hce=max(hr(3),hfl)
      call cmo_get_name(ccur,ier)
      call mmgetblk('hf_hmin','hfield_lg_w',iphm,nn,2,ier)
      if(ier.ne.0)then
         call hf_err('eval: no memory')
         ierror=1
         return
      endif
      do i=1,nn
         hmin(i)=hce
      enddo
C
      do k=1,nsrc
         ibase=4+8*(k-1)
         kind=nint(hr(ibase+1))
         hh=hr(ibase+2)
         ihow=nint(hr(ibase+3))
         hfac=hr(ibase+4)
         hband=hr(ibase+5)
         hgr=hr(ibase+6)
         csrc=hc(2*k-1)
         csatt=hc(2*k)
         ls=len_trim(csrc)
         call cmo_exist(csrc,ier)
         if(ier.ne.0)then
            call hf_err('eval: source '//csrc(1:ls)//
     &         ' no longer exists')
            ierror=1
            goto 900
         endif
         call cmo_get_info('nnodes',csrc,i,ilen,ity,ier)
         if(i.le.0)then
            call hf_err('eval: source '//csrc(1:ls)//
     &         ' has no vertices')
            ierror=1
            goto 900
         endif
         call cmo_get_info('nelements',csrc,ne,ilen,ity,ier)
C
C        A polygon of vertices (inside, no cells): hf_inside measures
C        the distance as well.
C
         if(kind.eq.2 .and. ne.le.0)then
            call mmgetblk('hf_d','hfield_lg_w',ipd,nn,2,ier)
            call cmo_get_info('xic',cmo,ipx,ilen,ity,ier)
            call cmo_get_info('yic',cmo,ipy,ilen,ity,ier)
            call cmo_get_info('zic',cmo,ipz,ilen,ity,ier)
            call hf_inside(csrc,nn,xic,yic,zic,dist,1,ierror)
            if(ierror.ne.0) goto 900
            goto 60
         endif
C
C        The distance to the source: its cells when it has any.
C
         call hf_delatt(cmo,'hf_tmp_dist')
         cbuf='compute/distance_field/'//cmo(1:lc)//'/'//
     &      csrc(1:ls)//'/hf_tmp_dist'
         if(ne.gt.0) cbuf=cbuf(1:len_trim(cbuf))//'/facet'
         cbuf=cbuf(1:len_trim(cbuf))//' ; finish'
         call dotask(cbuf,ier)
         call cmo_get_info('hf_tmp_dist',cmo,ipd,ilen,ity,ier)
         if(ier.ne.0)then
            call hf_err('eval: the distance to '//csrc(1:ls)//
     &         ' could not be computed')
            ierror=1
            goto 900
         endif
C
         if(ihow.eq.2)then
            la=len_trim(csatt)
            call hf_delatt(cmo,'hf_tmp_hatt')
            cbuf='cmo/addatt/'//cmo(1:lc)//
     &         '/hf_tmp_hatt/VDOUBLE/scalar/nnodes ; finish'
            call dotask(cbuf,ier)
            cbuf='interpolate/voronoi/'//cmo(1:lc)//
     &         '/hf_tmp_hatt/1,0,0/'//csrc(1:ls)//'/'//
     &         csatt(1:la)//' ; finish'
            call dotask(cbuf,ier)
            call cmo_get_info('hf_tmp_hatt',cmo,ipa,ilen,ity,ier)
            if(ier.ne.0)then
               call hf_err('eval: '//csatt(1:la)//' of '//
     &            csrc(1:ls)//' could not be copied')
               ierror=1
               goto 900
            endif
         endif
         call cmo_get_info('hf_tmp_dist',cmo,ipd,ilen,ity,ier)
C
         if(kind.eq.2)then
            call cmo_get_info('xic',cmo,ipx,ilen,ity,ier)
            call cmo_get_info('yic',cmo,ipy,ilen,ity,ier)
            call cmo_get_info('zic',cmo,ipz,ilen,ity,ier)
            call hf_inside(csrc,nn,xic,yic,zic,dist,0,ierror)
            if(ierror.ne.0) goto 900
         endif
C
   60    continue
         call mmfindbk('hf_hmin','hfield_lg_w',iphm,ilen,ier)
         do i=1,nn
            if(ihow.eq.0)then
               hi=hr(2)
            elseif(ihow.eq.1)then
               hi=hh
            else
               hi=hfac*aval(i)
            endif
            dd=max(0.0d0,dist(i)-hband)
            hi=hi+hgr*dd
            if(hi.lt.hmin(i)) hmin(i)=hi
         enddo
         if(kind.eq.2 .and. ne.le.0)then
            call mmrelblk('hf_d','hfield_lg_w',ipd,ier)
         else
            call hf_delatt(cmo,'hf_tmp_dist')
         endif
         if(ihow.eq.2) call hf_delatt(cmo,'hf_tmp_hatt')
      enddo
C
      call cmo_get_attparam(catt,cmo,index,ctype,crank,clen,
     &   cinter,cpers,cio,ier)
      if(ier.ne.0)then
         cbuf='cmo/addatt/'//cmo(1:lc)//'/'//catt(1:len_trim(catt))//
     &      '/VDOUBLE/scalar/nnodes ; finish'
         call dotask(cbuf,ier)
      endif
      call cmo_get_info(catt,cmo,ipho,ilen,ity,ier)
      if(ier.ne.0)then
         call hf_err('eval: could not make '//catt(1:len_trim(catt)))
         ierror=1
         goto 900
      endif
      call mmfindbk('hf_hmin','hfield_lg_w',iphm,ilen,ier)
      hlo=hce
      hhi=hfl
      do i=1,nn
         hout(i)=min(hce,max(hfl,hmin(i)))
         hlo=min(hlo,hout(i))
         hhi=max(hhi,hout(i))
      enddo
      if(iverb.ne.0)then
         write(cmess,'(7a,i0,a,1pe10.3,a,1pe10.3)')
     &      'hfield ',hname(1:lenh),' eval ',cmo(1:lc),'/',
     &      catt(1:len_trim(catt)),': ',nn,' vertices, h',hlo,' to',
     &      hhi
         call hf_say(cmess)
      endif
C
  900 continue
      call mmrelprt('hfield_lg_w',ier)
      if(ccur.ne.' ')then
         cbuf='cmo/select/'//ccur(1:len_trim(ccur))//' ; finish'
         call dotask(cbuf,ier)
      endif
      return
      end
C
C SUBROUTINE BEGIN ####################################################
      subroutine hf_dump(hname,cfile,n,ierror)
C
C     h on an n x n x n lattice (n x n when the sources are flat in
C     z) over the bounding box of the sources' vertices, written with
C     dump / avs.
C
      implicit none
      character*(*) hname,cfile
      integer n,ierror
      pointer (ipr,hr),(ipc,hc)
      real*8 hr(*)
      character*32 hc(*)
      pointer (ipx,xic),(ipy,yic),(ipz,zic)
      real*8 xic(*),yic(*),zic(*)
      integer nsrc,k,i,nn,ilen,ity,ier,nx,ny,nz
      real*8 bmin(3),bmax(3),diag,tol
      character*32 ccur,ctmp,cs
      character*512 cbuf
C
      call hf_get(hname,ipr,ipc,nsrc,ierror)
      if(ierror.ne.0) return
      if(n.lt.2)then
         call hf_err('dump: n must be at least 2')
         ierror=1
         return
      endif
      if(nsrc.eq.0)then
         call hf_err('dump: the hfield has no sources, so no '//
     &      'bounding box')
         ierror=1
         return
      endif
      do i=1,3
         bmin(i)=1.0d99
         bmax(i)=-1.0d99
      enddo
      do k=1,nsrc
         cs=hc(2*k-1)
         call cmo_exist(cs,ier)
         if(ier.ne.0)then
            call hf_err('dump: source '//cs(1:len_trim(cs))//
     &         ' no longer exists')
            ierror=1
            return
         endif
         call cmo_get_info('nnodes',cs,nn,ilen,ity,ier)
         call cmo_get_info('xic',cs,ipx,ilen,ity,ier)
         call cmo_get_info('yic',cs,ipy,ilen,ity,ier)
         call cmo_get_info('zic',cs,ipz,ilen,ity,ier)
         do i=1,nn
            bmin(1)=min(bmin(1),xic(i))
            bmax(1)=max(bmax(1),xic(i))
            bmin(2)=min(bmin(2),yic(i))
            bmax(2)=max(bmax(2),yic(i))
            bmin(3)=min(bmin(3),zic(i))
            bmax(3)=max(bmax(3),zic(i))
         enddo
      enddo
      diag=sqrt((bmax(1)-bmin(1))**2+(bmax(2)-bmin(2))**2+
     &   (bmax(3)-bmin(3))**2)
      tol=1.0d-9*max(diag,1.0d0)
      nx=n
      ny=n
      nz=n
      if(bmax(1)-bmin(1).le.tol) nx=1
      if(bmax(2)-bmin(2).le.tol) ny=1
      if(bmax(3)-bmin(3).le.tol) nz=1
      if(nx+ny+nz.lt.2*n)then
         call hf_err('dump: the sources lie on a line or a point; '//
     &      'no lattice to make')
         ierror=1
         return
      endif
C
      call cmo_get_name(ccur,ier)
      ctmp='hfield_dump_tmp'
      call cmo_exist(ctmp,ier)
      if(ier.eq.0) call dotask('cmo/delete/hfield_dump_tmp ; finish',
     &   ier)
      call dotask('cmo/create/hfield_dump_tmp ; finish',ier)
      write(cbuf,'(a,3(i6,a),3(1pe22.14,a),3(1pe22.14,a),a)')
     &   'createpts/brick/xyz/',nx,',',ny,',',nz,'/',
     &   bmin(1),',',bmin(2),',',bmin(3),'/',
     &   bmax(1),',',bmax(2),',',bmax(3),'/',' 1,1,1 ; finish'
      call dotask(cbuf,ier)
      call hfield_eval(hname,ctmp,'h_field',0.0d0,1,ierror)
      if(ierror.eq.0)then
         cbuf='dump/avs/'//cfile(1:len_trim(cfile))//
     &      '/hfield_dump_tmp ; finish'
         call dotask(cbuf,ier)
      endif
      call dotask('cmo/delete/hfield_dump_tmp ; finish',ier)
      if(ccur.ne.' ' .and. ccur.ne.ctmp)then
         cbuf='cmo/select/'//ccur(1:len_trim(ccur))//' ; finish'
         call dotask(cbuf,ier)
      endif
      return
      end
C
C SUBROUTINE BEGIN ####################################################
      subroutine hf_inside(csrc,nq,xq,yq,zq,dist,idist,ierror)
C
C     dist(i) = 0 for the query vertices inside the closed surface
C     (3D, triangles or quads) or the closed loop (2D, lines) csrc:
C     the winding number of the consistently oriented cells (hf_surf),
C     with the even-odd rule, so a hollow shape (a shell inside a
C     shell) has its hole outside. Coordinates relative to the
C     source's first vertex. idist 1 (a polygon of vertices, which
C     has no cells for compute / distance_field): dist is first set
C     here, to the distance to the polygon's sides.
C
      implicit none
      character*(*) csrc
      integer nq,idist,ierror
      real*8 xq(*),yq(*),zq(*),dist(*)
      pointer (ipfv,ifv),(ipsx,sx),(ipsy,sy),(ipsz,sz)
      integer ifv(*)
      real*8 sx(*),sy(*),sz(*)
      integer nvf,nf,ns,ilen,ier,i,j,i1,i2,i3,iw
      real*8 x0,y0,z0,px,py,pz,ax,ay,az,bx,by,bz,cx,cy,cz,
     &   la,lb,lc,det,den,wsum,pi,tol,bmin(3),bmax(3),t,d2,dmin2
C
      call hf_surf(csrc,1,nvf,nf,ns,ierror)
      if(ierror.ne.0) return
      call mmfindbk('hf_fv','hfield_lg_s',ipfv,ilen,ier)
      call mmfindbk('hf_sx','hfield_lg_s',ipsx,ilen,ier)
      call mmfindbk('hf_sy','hfield_lg_s',ipsy,ilen,ier)
      call mmfindbk('hf_sz','hfield_lg_s',ipsz,ilen,ier)
      pi=4.0d0*atan(1.0d0)
      x0=sx(1)
      y0=sy(1)
      z0=sz(1)
      do j=1,3
         bmin(j)=1.0d99
         bmax(j)=-1.0d99
      enddo
      do i=1,ns
         sx(i)=sx(i)-x0
         sy(i)=sy(i)-y0
         sz(i)=sz(i)-z0
         bmin(1)=min(bmin(1),sx(i))
         bmax(1)=max(bmax(1),sx(i))
         bmin(2)=min(bmin(2),sy(i))
         bmax(2)=max(bmax(2),sy(i))
         bmin(3)=min(bmin(3),sz(i))
         bmax(3)=max(bmax(3),sz(i))
      enddo
      tol=1.0d-9*max(1.0d0,bmax(1)-bmin(1),bmax(2)-bmin(2),
     &   bmax(3)-bmin(3))
C
      if(idist.eq.1)then
         do i=1,nq
            px=xq(i)-x0
            py=yq(i)-y0
            pz=zq(i)-z0
            dmin2=1.0d99
            do j=1,nf
               i1=ifv(2*j-1)
               i2=ifv(2*j)
               bx=sx(i2)-sx(i1)
               by=sy(i2)-sy(i1)
               bz=sz(i2)-sz(i1)
               ax=px-sx(i1)
               ay=py-sy(i1)
               az=pz-sz(i1)
               t=bx*bx+by*by+bz*bz
               if(t.gt.0.0d0) t=max(0.0d0,min(1.0d0,
     &            (ax*bx+ay*by+az*bz)/t))
               d2=(ax-t*bx)**2+(ay-t*by)**2+(az-t*bz)**2
               dmin2=min(dmin2,d2)
            enddo
            dist(i)=sqrt(dmin2)
         enddo
      endif
C
      do i=1,nq
         if(dist(i).le.0.0d0) goto 50
         px=xq(i)-x0
         py=yq(i)-y0
         pz=zq(i)-z0
         if(px.lt.bmin(1)-tol .or. px.gt.bmax(1)+tol .or.
     &      py.lt.bmin(2)-tol .or. py.gt.bmax(2)+tol) goto 50
         wsum=0.0d0
         if(nvf.eq.3)then
            if(pz.lt.bmin(3)-tol .or. pz.gt.bmax(3)+tol) goto 50
            do j=1,nf
               i1=ifv(3*j-2)
               i2=ifv(3*j-1)
               i3=ifv(3*j)
               ax=sx(i1)-px
               ay=sy(i1)-py
               az=sz(i1)-pz
               bx=sx(i2)-px
               by=sy(i2)-py
               bz=sz(i2)-pz
               cx=sx(i3)-px
               cy=sy(i3)-py
               cz=sz(i3)-pz
               la=sqrt(ax*ax+ay*ay+az*az)
               lb=sqrt(bx*bx+by*by+bz*bz)
               lc=sqrt(cx*cx+cy*cy+cz*cz)
               det=ax*(by*cz-bz*cy)-ay*(bx*cz-bz*cx)+
     &             az*(bx*cy-by*cx)
               den=la*lb*lc+(ax*bx+ay*by+az*bz)*lc+
     &             (ax*cx+ay*cy+az*cz)*lb+(bx*cx+by*cy+bz*cz)*la
               wsum=wsum+2.0d0*atan2(det,den)
            enddo
            iw=nint(abs(wsum)/(4.0d0*pi))
         else
            do j=1,nf
               i1=ifv(2*j-1)
               i2=ifv(2*j)
               ax=sx(i1)-px
               ay=sy(i1)-py
               bx=sx(i2)-px
               by=sy(i2)-py
               wsum=wsum+atan2(ax*by-ay*bx,ax*bx+ay*by)
            enddo
            iw=nint(abs(wsum)/(2.0d0*pi))
         endif
         if(mod(iw,2).eq.1) dist(i)=0.0d0
   50    continue
      enddo
      call mmrelprt('hfield_lg_s',ier)
      return
      end
C
C SUBROUTINE BEGIN ####################################################
      subroutine hf_surf(csrc,ikeep,nvf,nf,ns,ierror)
C
C     Check that csrc is closed, and orient its cells consistently.
C
C     Cells: only triangles or only quads (a surface, 3D), or only
C     lines (a loop, 2D). A ridge is an edge of a triangle or quad,
C     or an end vertex of a line. Closed: every ridge is shared by
C     exactly two cells. Two cells agree in orientation when they go
C     along their shared edge in opposite directions (a line: one
C     starts where the other ends). The cells are flipped as needed,
C     one connected piece at a time.
C
C     ikeep 1: the oriented facets are left in partition hfield_lg_s:
C        hf_fv  (nvf*nf) vertex numbers; nvf 3 triangles, 2 lines;
C               each quad is 4 triangles to its center
C        hf_sx, hf_sy, hf_sz (ns) the coordinates: the vertices of
C               csrc, then the quad centers
C     The caller releases the partition. ikeep 0: check only.
C
      implicit none
      include 'local_element.h'
      character*(*) csrc
      integer ikeep,nvf,nf,ns,ierror
      pointer (ipitet,itet),(ipitetoff,itetoff),(ipitettyp,itettyp)
      integer itet(*),itetoff(*),itettyp(*)
      pointer (ipx,xic),(ipy,yic),(ipz,zic)
      real*8 xic(*),yic(*),zic(*)
      pointer (ipkey,ikey),(ipsg,isg),(ipperm,iperm),(ippart,ipart),
     &   (ipflip,iflip),(ipque,ique),(ipfv,ifv)
      integer ikey(*),isg(*),iperm(*),ipart(*),iflip(*),ique(*),
     &   ifv(*)
      pointer (ipsx,sx),(ipsy,sy),(ipsz,sz)
      real*8 sx(*),sy(*),sz(*)
      integer nn,ne,ilen,ity,ier,it,ntri,nlin,nqud,noth,nvc,nrc,nr,
     &   ic,ir,jr,ia,ib,i,j,nbad,ihead,itail,ic1,ic2,iwant,ioff,
     &   iq,ls
      character*132 cmess
C
      ierror=0
      ls=len_trim(csrc)
      call cmo_get_info('nnodes',csrc,nn,ilen,ity,ier)
      call cmo_get_info('nelements',csrc,ne,ilen,ity,ier)
      if(ne.le.0)then
C
C        No cells: the vertices in order are a closed polygon (as for
C        createpts / poisson_disk / 2d_polygon).
C
         if(nn.lt.3)then
            call hf_err('inside: '//csrc(1:ls)//' has no cells and '//
     &         'fewer than 3 vertices, so it is not a polygon')
            ierror=1
            return
         endif
         nvf=2
         nf=nn
         ns=nn
         if(ikeep.eq.0) return
         call cmo_get_info('xic',csrc,ipx,ilen,ity,ier)
         call cmo_get_info('yic',csrc,ipy,ilen,ity,ier)
         call cmo_get_info('zic',csrc,ipz,ilen,ity,ier)
         call mmgetblk('hf_fv','hfield_lg_s',ipfv,2*nf,1,ier)
         call mmgetblk('hf_sx','hfield_lg_s',ipsx,ns,2,ier)
         call mmgetblk('hf_sy','hfield_lg_s',ipsy,ns,2,ier)
         call mmgetblk('hf_sz','hfield_lg_s',ipsz,ns,2,ier)
         do i=1,nn
            sx(i)=xic(i)
            sy(i)=yic(i)
            sz(i)=zic(i)
            ifv(2*i-1)=i
            ifv(2*i)=mod(i,nn)+1
         enddo
         return
      endif
      call cmo_get_info('itet',csrc,ipitet,ilen,ity,ier)
      call cmo_get_info('itetoff',csrc,ipitetoff,ilen,ity,ier)
      call cmo_get_info('itettyp',csrc,ipitettyp,ilen,ity,ier)
      ntri=0
      nlin=0
      nqud=0
      noth=0
      do it=1,ne
         if(itettyp(it).eq.ifelmtri)then
            ntri=ntri+1
         elseif(itettyp(it).eq.ifelmlin)then
            nlin=nlin+1
         elseif(itettyp(it).eq.ifelmqud)then
            nqud=nqud+1
         else
            noth=noth+1
         endif
      enddo
      if(noth.gt.0 .or. ntri.ne.ne .and. nqud.ne.ne .and.
     &   nlin.ne.ne)then
         call hf_err('inside: '//csrc(1:ls)//' must have only '//
     &      'triangles, only quads, or only lines')
         ierror=1
         return
      endif
      if(ntri.eq.ne)then
         nvc=3
         nrc=3
      elseif(nqud.eq.ne)then
         nvc=4
         nrc=4
      else
         nvc=2
         nrc=2
      endif
      nr=ne*nrc
      call mmgetblk('hf_key','hfield_lg_s',ipkey,nr,1,ier)
      call mmgetblk('hf_sg','hfield_lg_s',ipsg,nr,1,ier)
      call mmgetblk('hf_perm','hfield_lg_s',ipperm,nr,1,ier)
      call mmgetblk('hf_part','hfield_lg_s',ippart,nr,1,ier)
      call mmgetblk('hf_flip','hfield_lg_s',ipflip,ne,1,ier)
      call mmgetblk('hf_que','hfield_lg_s',ipque,ne,1,ier)
      if(ier.ne.0)then
         call hf_err('inside: no memory')
         ierror=1
         goto 900
      endif
C
C     The ridges, with +1 or -1 for the direction the cell takes.
C
      do ic=1,ne
         ioff=itetoff(ic)
         do j=1,nrc
            ir=(ic-1)*nrc+j
            if(nvc.eq.2)then
               ikey(ir)=itet(ioff+j)
               isg(ir)=3-2*j
            else
               ia=itet(ioff+j)
               ib=itet(ioff+mod(j,nvc)+1)
               ikey(ir)=min(ia,ib)*(nn+1)+max(ia,ib)
               isg(ir)=1
               if(ia.gt.ib) isg(ir)=-1
            endif
            iperm(ir)=ir
            ipart(ir)=0
         enddo
      enddo
      call hf_hsort(nr,ikey,iperm)
      nbad=0
      i=1
      do while(i.le.nr)
         j=i
         do while(j.lt.nr)
            if(ikey(iperm(j+1)).ne.ikey(iperm(i))) goto 20
            j=j+1
         enddo
   20    continue
         if(j-i+1.eq.2)then
            ipart(iperm(i))=iperm(j)
            ipart(iperm(j))=iperm(i)
         else
            nbad=nbad+1
         endif
         i=j+1
      enddo
      if(nbad.gt.0)then
         if(nvc.eq.2)then
            write(cmess,'(a,a,a,i10,a)') 'inside: ',csrc(1:ls),
     &      ' is not a closed loop:',nbad,
     &      ' vertices are not on exactly two lines'
         else
            write(cmess,'(a,a,a,i10,a)') 'inside: ',csrc(1:ls),
     &      ' is not a closed surface:',nbad,
     &      ' edges are not shared by exactly two cells'
         endif
         call hf_err(cmess)
         ierror=1
         goto 900
      endif
C
C     Orient, one connected piece at a time.
C
      do ic=1,ne
         iflip(ic)=-1
      enddo
      do ic=1,ne
         if(iflip(ic).ge.0) goto 40
         iflip(ic)=0
         ihead=1
         itail=1
         ique(1)=ic
         do while(ihead.le.itail)
            ic1=ique(ihead)
            ihead=ihead+1
            do j=1,nrc
               ir=(ic1-1)*nrc+j
               jr=ipart(ir)
               ic2=(jr-1)/nrc+1
               iwant=iflip(ic1)
               if(isg(ir).eq.isg(jr)) iwant=1-iwant
               if(iflip(ic2).lt.0)then
                  iflip(ic2)=iwant
                  itail=itail+1
                  ique(itail)=ic2
               elseif(iflip(ic2).ne.iwant)then
                  call hf_err('inside: '//csrc(1:ls)//' cannot be '//
     &               'oriented (a one-sided surface)')
                  ierror=1
                  goto 900
               endif
            enddo
         enddo
   40    continue
      enddo
      if(ikeep.eq.0) goto 900
C
C     The oriented facets.
C
      call cmo_get_info('xic',csrc,ipx,ilen,ity,ier)
      call cmo_get_info('yic',csrc,ipy,ilen,ity,ier)
      call cmo_get_info('zic',csrc,ipz,ilen,ity,ier)
      ns=nn
      nf=ne
      nvf=3
      if(nvc.eq.2) nvf=2
      if(nvc.eq.4)then
         ns=nn+ne
         nf=4*ne
      endif
      call mmgetblk('hf_fv','hfield_lg_s',ipfv,nvf*nf,1,ier)
      call mmgetblk('hf_sx','hfield_lg_s',ipsx,ns,2,ier)
      call mmgetblk('hf_sy','hfield_lg_s',ipsy,ns,2,ier)
      call mmgetblk('hf_sz','hfield_lg_s',ipsz,ns,2,ier)
      if(ier.ne.0)then
         call hf_err('inside: no memory')
         ierror=1
         goto 900
      endif
      do i=1,nn
         sx(i)=xic(i)
         sy(i)=yic(i)
         sz(i)=zic(i)
      enddo
      do ic=1,ne
         ioff=itetoff(ic)
         if(nvc.eq.3)then
            ifv(3*ic-2)=itet(ioff+1)
            ifv(3*ic-1)=itet(ioff+2)
            ifv(3*ic)=itet(ioff+3)
            if(iflip(ic).eq.1)then
               ifv(3*ic-1)=itet(ioff+3)
               ifv(3*ic)=itet(ioff+2)
            endif
         elseif(nvc.eq.2)then
            ifv(2*ic-1)=itet(ioff+1)
            ifv(2*ic)=itet(ioff+2)
            if(iflip(ic).eq.1)then
               ifv(2*ic-1)=itet(ioff+2)
               ifv(2*ic)=itet(ioff+1)
            endif
         else
            iq=nn+ic
            sx(iq)=0.0d0
            sy(iq)=0.0d0
            sz(iq)=0.0d0
            do j=1,4
               ia=itet(ioff+j)
               sx(iq)=sx(iq)+0.25d0*xic(ia)
               sy(iq)=sy(iq)+0.25d0*yic(ia)
               sz(iq)=sz(iq)+0.25d0*zic(ia)
            enddo
            do j=1,4
               ia=itet(ioff+j)
               ib=itet(ioff+mod(j,4)+1)
               ir=4*(ic-1)+j
               ifv(3*ir-2)=iq
               ifv(3*ir-1)=ia
               ifv(3*ir)=ib
               if(iflip(ic).eq.1)then
                  ifv(3*ir-1)=ib
                  ifv(3*ir)=ia
               endif
            enddo
         endif
      enddo
      call mmrelblk('hf_key','hfield_lg_s',ipkey,ier)
      call mmrelblk('hf_sg','hfield_lg_s',ipsg,ier)
      call mmrelblk('hf_perm','hfield_lg_s',ipperm,ier)
      call mmrelblk('hf_part','hfield_lg_s',ippart,ier)
      call mmrelblk('hf_flip','hfield_lg_s',ipflip,ier)
      call mmrelblk('hf_que','hfield_lg_s',ipque,ier)
      return
C
  900 continue
      call mmrelprt('hfield_lg_s',ier)
      return
      end
C
C SUBROUTINE BEGIN ####################################################
      subroutine hf_hsort(n,ikey,iperm)
C     Heap sort of iperm(1..n) by ikey(iperm(i)).
      implicit none
      integer n,ikey(*),iperm(*)
      integer i,ir,j,l,itmp
      if(n.lt.2) return
      l=n/2+1
      ir=n
   10 continue
      if(l.gt.1)then
         l=l-1
         itmp=iperm(l)
      else
         itmp=iperm(ir)
         iperm(ir)=iperm(1)
         ir=ir-1
         if(ir.eq.1)then
            iperm(1)=itmp
            return
         endif
      endif
      i=l
      j=l+l
   20 if(j.le.ir)then
         if(j.lt.ir)then
            if(ikey(iperm(j)).lt.ikey(iperm(j+1))) j=j+1
         endif
         if(ikey(itmp).lt.ikey(iperm(j)))then
            iperm(i)=iperm(j)
            i=j
            j=j+j
         else
            j=ir+1
         endif
         goto 20
      endif
      iperm(i)=itmp
      goto 10
      end
