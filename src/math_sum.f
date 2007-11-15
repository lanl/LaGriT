dk,math_sum
      subroutine math_sum(imsgin,xmsgin,cmsgin,msgtype,nwds,
     &   ierror)
C
C #####################################################################
C
C     PURPOSE -
C
C        Sum the values in the source attribute.
C        The sink attribute is type scalar and has the summed value
C
C     SYNTAX -
C
C     The commands from math are passed directly in, so they will
C     be in the following form:
C
c      math/integrate/cmosnk/attsnk/RANGE/cmosrc/attsrc
c       1      2         3     4     5-7      8   9
C
C
C
C  EXAMPLES:
C
c#######################################################################
C
C     INPUT ARGUMENTS -
C
C         imsgin()  - Integer array of command input tokens
C         xmsgin()  - Real array of command input tokens
C         cmsgin()  - Character array of command input tokens
C         msgtype() - Integer array of command input token types
C         nwds      - Number of command input tokens
C
C     OUTPUT ARGUMENTS -
C
C         ierror - Error Return Code (==0 ==> OK, <>0 ==> Error)
C
C     CHANGE HISTORY -
C
C
C        $Log: math_sum.f,v $
C        Revision 2.00  2007/11/05 19:46:01  spchu
C        Import to CVS
C
CPVCS
CPVCS       Rev 1.21   02 Oct 2007 12:40:28   spchu
CPVCS    original version
C
c#######################################################################
C
      implicit none
C
      integer nplen
      parameter (nplen=1000000)
C
C#######################################################################
C
      integer nwds, imsgin(nwds), msgtype(nwds)
      real*8 xmsgin(nwds)
      character*(*) cmsgin(nwds)
      integer ierror
C
C#######################################################################
C
 
C-----Sink Mesh Object
c     scalar attribute is only valid attribute
      real*8  xvalue
      integer ivalue
 
C-----Source Mesh Object
      pointer (ipiattsrc, iattsrc)
      pointer (ipmpary, mpary)
      pointer (ipout, out)
      pointer (ipout, iout)
      real*8 out(*)
      integer iout(*)
      pointer (ipisetwd, isetwd)
      pointer (ipxtetwd, xtetwd)
      pointer (ipitp1, itp1)
      integer  iattsrc(nplen),mpary(nplen),
     *         isetwd(nplen),xtetwd(nplen),itp1(nplen)
      pointer (ipxattsrc, xattsrc)
      real*8 xattsrc(nplen)
 
      integer ierr,ierrw,ics,ics2,i,i1,j1,l,ipt,itin,it
      integer len,ilen,ityp,ipt1,ipt2,ipt3,mpno,mbndry
      integer nnodes,nelements,length,nset,irank,idebug,
     * num_src,npts_snk,npts_src,nelm_snk,nelm_src,
     * ipointi,ipointj,icreate,idx,idx_snk
 
      character*32 ich1,ich2,ich3,blkname,cvalue
      character*32 cmosnk, cmosrc, attsrc, attsnk
      character*32 ctype,crank,
     * clen,cinter,cpers,cio,
     * ctype_snk,clen_snk,crank_snk
      character*32  isubname
      character*132 logmess
      integer icharlnf
      logical isint, isreal
 
C
C ######################################################################
C
c     set defaults
      isubname='math_sum'
      ierror = -1
      isint=.false.
      isreal=.false.
 
c     Get the command parameters
c     math/integrate/cmosnk/attsnk/RANGE/cmosrc/attsrc
c     1      2         3     4     5-7      8       9
c
      if (nwds.ne.9) then
         write(logmess,"('SUM: Incorrect syntax.')")
         call writloga('default',1,logmess,1,ierrw)
         goto 9990
      endif
 
c     get the cmo name and the attributes for result and field
c     that leaves the RANGE parameters for the indexed set
      cmosnk = cmsgin(3)
      attsnk = cmsgin(4)
      cmosrc = cmsgin(8)
      attsrc = cmsgin(9)
 
C     Check the mesh object names
      call cmo_exist(cmosnk,ierr)
      if(ierr.ne.0) then
         write(logmess,'(a,a)')
     *     'SUM: Not a valid mesh object: ',cmosnk
         call writloga('default',1,logmess,1,ierrw)
 
         call cmo_exist(cmosrc,ics)
         if(ics.ne.0) then
            write(logmess,'(a,a)')
     *       'SUM: Not a valid mesh object: ',cmosrc
            call writloga('default',1,logmess,1,ierrw)
         endif
         if (ierr.ne.0 .or. ics.ne.0) goto 9990
      endif
 
C     Check the mesh object attributes
      call mmfindbk(attsrc,cmosrc,ipout,ilen,ierr)
      if(ierr.ne.0) then
         write(logmess,'(a,a,a,a,a)') 'attribute does not exist: ',
     *      '  cmo = ',cmosrc(1:icharlnf(cmosrc)),
     *      '  att = ',attsrc(1:icharlnf(attsrc))
         call writloga('default',0,logmess,0,ierrw)
         goto 9990
      endif
 
C     ******************************************************************
C     Get Sink Mesh Object
C
      call cmo_select(cmosnk,ierr)
      call cmo_get_intinfo('nnodes',cmosnk,nnodes,ilen,ityp,ierr)
      if(ierr.ne.0) call x3d_error(isubname,'intinfo nnodes cmosink')
      call cmo_get_intinfo('nelements',cmosnk,nelements,ilen,ityp,ierr)
      if(ierr.ne.0) call x3d_error(isubname,'intinfo nelements cmosink')
      npts_snk = nnodes
      nelm_snk = nelements
 
      call cmo_get_info('idebug',cmosnk,idebug,len,ityp,ierr)
      call cmo_get_intinfo('mbndry',cmosnk,mbndry,ilen,ityp,ierr)
      if(ierr.ne.0) call x3d_error(isubname,'intinfo mbndry cmosink')

c     look for sink attribute, must be scalar
c     create attribute if does not exist
      icreate = 0

      call cmo_get_attparam(attsnk,cmosnk,idx_snk,
     *    ctype_snk,crank_snk,clen_snk,cinter,cpers,cio,ics)
      if(ics.ne.0) then
         icreate = 1
      else
         if(ctype_snk(1:4).eq.'REAL'.or.ctype_snk(1:4).eq.'real')then
             isreal=.true.
         elseif(ctype_snk(1:3).eq.'INT'.or.ctype_snk(1:3).eq.'int')then
             isint=.true.
         else
             write(logmess,'(a)')
     *       'sink attribute must be INT or REAL '
             write(logmess,'(a,a,a,a,a,a)')
     *       '  cmo = ',cmosnk(1:icharlnf(cmosnk)),
     *       '  att = ',attsnk(1:icharlnf(attsnk)),
     *       '  typ = ',ctype_snk(1:icharlnf(ctype_snk))
             call writloga('default',0,logmess,0,ierrw)
             goto 9990
         endif
         if(clen_snk(1:6).ne.'scalar') then
             write(logmess,'(a)')
     *         'sink attribute must be scalar: '
             call writloga('default',0,logmess,0,ierrw)
             write(logmess,'(a,a,a,a,a,a)')
     *         '  cmo = ',cmosnk(1:icharlnf(cmosnk)),
     *         '  att = ',attsnk(1:icharlnf(attsnk)),
     *         '  len = ',clen_snk(1:icharlnf(clen_snk))
             call writloga('default',0,logmess,0,ierrw)
             goto 9990
         endif
      endif

 
C     ******************************************************************
C     Get Source Mesh Object
C
      call cmo_select(cmosrc,ierr)
      call cmo_get_intinfo('nnodes',cmosrc,npts_src,ilen,ityp,ierr)
      if(ierr.ne.0) call x3d_error(isubname,'intinfo nnodes ')
      call cmo_get_intinfo('nelements',cmosrc,nelm_src,ilen,ityp,ierr)
      if(ierr.ne.0) call x3d_error(isubname,'intinfo nelements')
 
      call cmo_get_info('isetwd',cmosrc,ipisetwd,ilen,ityp,ierr)
      if(ierr.ne.0) call x3d_error(isubname,'get_info isetwd cmosrc')
      call cmo_get_info('itp1',cmosrc,ipitp1,ilen,ityp,ierr)
      if(ierr.ne.0) call x3d_error(isubname,'get_info itp1 cmosrc')
 
c     get source attribute info and assign attribute length
      call cmo_get_attparam(attsrc,cmosrc,idx,ctype,crank,
     *                      clen,cinter,cpers,cio,ierr)
      if(ierr.ne.0) call x3d_error(isubname,'get_attparam source')
 
      call cmo_get_info(crank,cmosrc,irank,ilen,itin,ierr)
      if (ierr.ne.0) call x3d_error(' get rank in ',isubname)
      if (ierr.ne.0) goto 9999
      if (clen(1:5).eq.'nnode') then
         num_src = npts_src
      else
         num_src = nelm_src
      endif
 
 
C     ******************************************************************
C     set the point index boundaries
      ich1=' '
      ich2=' '
      ich3=' '
      mpno=0
      if(msgtype(5).eq.1) then
         ipt1=imsgin(5)
         ipt2=imsgin(6)
         ipt3=imsgin(7)
      else
         ich1=cmsgin(5)
         ich2=cmsgin(6)
         ich3=cmsgin(7)
      endif
      call cmo_select(cmosrc,ierr)
 
c     For node point set
      if(clen(1:6).eq.'nnodes') then
 
        ipointi=0
        ipointj=0
        call cmo_get_info('ipointi',cmosrc,ipointi,ilen,ityp,ics)
        if (ics .ne. 0) call x3d_error(isubname,'get_info_i')
        call cmo_get_info('ipointj',cmosrc,ipointj,ilen,ityp,ics)
        if (ics .ne. 0) call x3d_error(isubname,'get_info_i')
 
        if(ipt1.eq.0) ipt1=max(1,ipointi)
        if(ipt2.eq.0) then
          if(ipointj.le.0 .or. ipt1.eq.1) then
             ipt2=npts_src
          else
             ipt2=ipointj
          endif
        endif
        if(ipt3.eq.0) ipt3=1
        ipt1=max(1,min(ipt1,npts_src))
        ipt2=max(1,min(ipt2,npts_src))
        ipt3=max(1,min(ipt3,npts_src))
        if (clen(1:6).eq.'scalar') then
          ipt1=1
          ipt2=1
          ipt3=1
        endif
 
        length=npts_src
        if (length.le.0) then
          write(logmess,'(a,a)')' 0 length attribute: ',attsrc
          call writloga('default',0,logmess,0,ierrw)
          goto 9990
        endif
 
        call mmgetblk('mpary',isubname,ipmpary,length,1,ics)
        if(ics.ne.0) call x3d_error(isubname,'mmgetblk mpary')
 
        if(msgtype(5).eq.1) then
         call pntlimn(ipt1,ipt2,ipt3,ipmpary,mpno,npts_src,isetwd,itp1)
        else
         call pntlimc(ich1,ich2,ich3,ipmpary,mpno,npts_src,isetwd,itp1)
        endif
        if (mpno.gt.0) then
           write(logmess,'(a,i10)')
     *      'nodes in indexed point set  = ',mpno
           call writloga('default',0,logmess,0,ierrw)
        else
           write(logmess,'(a)') 'No points in indexed point set!'
           call writloga('default',1,logmess,1,ierrw)
           goto 9990
        endif
 
c     For element point set
      else
        length=nelm_src
        if (length.le.0) then
          write(logmess,'(a,a)')' 0 element attribute: ',attsrc
          call writloga('default',0,logmess,0,ierrw)
          goto 9990
        endif
 
        call mmgetblk('mpary',isubname,ipmpary,length,1,ics)
        if(ics.ne.0) call x3d_error(isubname,'mmgetblk mpary')
 
        if(msgtype(5).eq.1) then
          if (ipt2.le.0) ipt2=nelm_src
          ipt1=max(1,min(ipt1,nelm_src))
          ipt2=max(1,min(ipt2,nelm_src))
          ipt3=max(1,min(ipt3,nelm_src))
          mpno=0
          do it=ipt1,ipt2,ipt3
             mpno=mpno+1
             mpary(mpno)=it
          enddo
 
        else
          call cmo_get_info('xtetwd',cmosrc,ipxtetwd,ilen,ityp,ics)
          if (ics.ne.0) call x3d_error(isubname,'get xtetwd')
          call eltlimc(ich1,ich2,ich3,ipmpary,mpno,nelm_src,xtetwd)
        endif
        if (mpno.gt.0.and.idebug.gt.0) then
           write(logmess,'(a,i10)')
     *      'elements in indexed set  = ',mpno
           call writloga('default',0,logmess,0,ierrw)
        elseif(mpno.eq.0) then
           write(logmess,'(a)') 'No elements in indexed set!'
           call writloga('default',1,logmess,1,ierrw)
           goto 9990
        endif
      endif
C     End point set
 
 
C     Assign source attribute to xattsrc or iattsrc array
      length=num_src
      len=icharlnf(attsrc)
      blkname=' '
      blkname(1:len)=attsrc
      if(ctype(1:4).eq.'VINT') then
        call mmgetpr(blkname,cmosrc,ipiattsrc,ics)
        call mmgetlen(ipiattsrc,length,ics2)
        if(ics.ne.0) call x3d_error(isubname,'mmgetpr iattsrc')
      elseif(ctype(1:7).eq.'VDOUBLE') then
        call mmgetpr(blkname,cmosrc,ipxattsrc,ics)
        call mmgetlen(ipxattsrc,length,ics2)
        if(ics.ne.0) call x3d_error(isubname,'mmgetpr xattsrc')
      else
        write(logmess,'(a,a,a,a)')
     *  'Invalid attribute type for ',cmosrc(1:icharlnf(cmosrc)),
     *   ' ',blkname(1:icharlnf(blkname))
         call writloga('default',1,logmess,0,ierrw)
         goto 9990
      endif
 
c     get sink attribute info, must be scalar type
      if (icreate.eq.1) then
        write(logmess,'(a,a,a,a,a)') 'sink attribute being created: ',
     *     '  cmo = ',cmosnk(1:icharlnf(cmosnk)),
     *     '  att = ',attsnk(1:icharlnf(attsnk))
        call writloga('default',0,logmess,0,ierrw)
 
c       try to create the attribute, derive from source
        if (ctype(1:4).eq.'VINT') then
           ctype_snk = 'INT'
           isint=.true.
        else
           ctype_snk = 'REAL'
           isreal=.true.
        endif
        logmess='cmo/addatt/' //
     *      cmosnk(1:icharlnf(cmosnk)) // ' / ' //
     *      attsnk(1:icharlnf(attsnk)) // ' / ' //
     *      ctype_snk(1:icharlnf(ctype_snk)) //
     *      '/scalar/scalar/constant/permanent/agl ; finish'
        call dotaskx3d(logmess,ierror)
        if(ierror.eq.0) then
           call cmo_verify_cmo(cmosnk,ierror)
           if(ierror.ne.0) then
              write(logmess,'(a,a,a)')
     *        'CMO_ADDATT error: ',
     *        'Mesh Object is not consistent: ', cmosnk
              call writloga('default',0,logmess,0,ierr)
              goto 9999
           endif
        endif
      endif
      call cmo_get_attparam(attsnk,cmosnk,idx_snk,
     *    ctype_snk,crank_snk,clen_snk,cinter,cpers,cio,ics)

      if (idebug.ge.1) then
        write(logmess,'(a,a15,a10,a10,a10)')
     *  'sink attribute:   ',attsnk,ctype_snk,clen_snk,crank_snk
        call writloga('default',0,logmess,0,ierrw)
        write(logmess,'(a,a15,a10,a10,a10)')
     *  'source attribute: ',attsrc,ctype,clen,crank
        call writloga('default',0,logmess,0,ierrw)
      endif
 
C*****************************************************************
C     Sum the source attribute values
      xvalue = 0.0
      ivalue = 0
      nset = 0
      do j1=1,mpno
         i1=mpary(j1)
 
         do l = 1,irank
            ipt = (i1-1)*irank+l
 
            if(ctype(1:8).eq.'VDOUBLE' ) then
               xvalue = xattsrc(ipt) + xvalue
               nset=nset+1
            else
               ivalue = iattsrc(ipt) + ivalue
               nset=nset+1
            endif
         enddo
      enddo
C*****************************************************************
 
      write(logmess,'(i10,a,a)')
     *nset,' values summed from attribute ',attsrc(1:icharlnf(attsrc))
      call writloga('default',0,logmess,0,ierrw)
      if (ctype(1:7).eq.'VDOUBLE') then
         write(logmess,'(a,a,e20.12)')
     1         attsrc(1:icharlnf(attsrc)),' sum = ',xvalue
         call writloga('default',0,logmess,0,ierrw)
      elseif (ctype(1:7).eq.'VINT') then
         write(logmess,'(a,a,i10)')
     1         attsrc(1:icharlnf(attsrc)),' sum = ',ivalue
         call writloga('default',0,logmess,0,ierrw)
      endif
 
c     Set the source attribute to summed value
      if(isint) then
         if (ctype(1:7).eq.'VDOUBLE') ivalue = nint(xvalue)
         call cmo_set_info(attsnk,cmosnk,ivalue,1,1,ics)
         if(ics.ne.0) call x3d_error(isubname,'cmo_set_info attsnk')
      elseif(isreal) then
         if (ctype(1:4).eq.'VINT') xvalue = dble(ivalue)
         call cmo_set_attinfo(attsnk,cmosnk,ivalue,xvalue,cvalue,2,ics)
         if(ics.ne.0) call x3d_error(isubname,'cmo_set_info attsnk')
      elseif(ctype_snk.eq.'VDOUBLE') then
         if (ctype(1:4).eq.'VINT') out(1)=dble(ivalue)
         if (ctype(1:7).eq.'VDOUBLE') out(1)=xvalue
      elseif(ctype_snk.eq.'VINT') then
         if (ctype(1:4).eq.'VINT') iout(1)=ivalue
         if (ctype(1:7).eq.'VDOUBLE') iout(1)=nint(xvalue)
      else
         call x3d_error(isubname,'attsnk not INT or REAL')
         ics = 1
      endif
      if(ics.ne.0) goto 9999
 
c     finished with no errors
      ierror=0
 9999 continue
 
      len=icharlnf(cmosnk)
      logmess = 'cmo select '//cmosnk(1:len)//' ;  finish'
      call dotaskx3d(logmess,ierr)
 
 
c     Return here if error is found before setup is done
 9990 if(ierror.ne.0) then
         write(logmess,'(a)')'FATAL ERROR: SUM unable to begin.'
         call writloga('default',1,logmess,1,ierrw)
      endif
      call mmrelprt(isubname,ics)
 
      return
      end
C     END math_sum
 
C
 
