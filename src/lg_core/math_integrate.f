      subroutine math_integrate(imsgin,xmsgin,cmsgin,msgtype,nwds,
     &   ierror)
C
C #####################################################################
C
C     PURPOSE -
C
C        Integrate a field value over each triangle
C        The field values are the values at each vertex of the element. 
C        If the source attribute is element type, then the element
C        value is used.
C        The indexed set is a set of elements.
C
C        If sink attribute is type vector, fill with integrated value
C        for each element.
C
C        If sink attribute is type scalar, give result of summing
C        the integral values over all elements.
C
C     SYNTAX -
C
C     The commands from math are passed directly in, so they will
C     be in the following form:
C
c      math/integrate/cmosnk/attsnk/RANGE/cmosrc/attsrc
c       1      2        3      4     5-7      8       9   
c      math/integrate/cmosnk/attsnk/RANGE/attsrc
C
C     For integrate, the sink cmo and source cmo must be the same.
C     So allow the command line to leave cmosrc token out.
c
C     idebug levels change at 1 5 9
C
C
C  EXAMPLES:
C
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
C  $Log: math_integrate.f,v $
C  Revision 2.00  2007/11/05 19:46:01  spchu
C  Import to CVS
C
CPVCS    
CPVCS       Rev 1.4   09 May 2002 08:59:56   dcg
CPVCS    allow for 'real' scalar attributes
CPVCS    
CPVCS       Rev 1.3   14 Jan 2002 09:34:38   dcg
CPVCS    clean up unused code
C
c#######################################################################
C
      implicit none
C
      include 'local_element.h'
      include 'consts.h'
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
      pointer (ipitettyp, itettyp)
      pointer (ipitet, itet)
      pointer (ipitetoff, itetoff)
      integer itettyp(nplen),itet(nplen),itetoff(nplen)

      pointer (ipxic, xic)
      pointer (ipyic, yic)
      pointer (ipzic, zic)
      real*8  xic(nplen), yic(nplen), zic(nplen)

      pointer (ipiattsrc, iattsrc)
      pointer (ipmpary, mpary)
      pointer (ipout, out)
      pointer (ipxtetwd, xtetwd)
      integer iattsrc(nplen),mpary(nplen),out(nplen),xtetwd(nplen)

C-----Source Field attribute and Sink result attribute
      pointer (ipxattsnk, xattsnk)
      pointer (ipxattsrc, xattsrc)
      real*8 xattsnk(nplen), xattsrc(nplen)

      real*8 xicvol(maxnen), yicvol(maxnen), zicvol(maxnen)
      real*8 privol

      integer ierr,ierrw,ics,idx,i,j,idebug
      integer len,ilen,ityp,ipt1,ipt2,ipt3,mpno,mbndry,nen,nef
      integer nnodes,nelements,length,ivalue,
     * nsdgeom, nsdtopo, num_src,ifelement
      character*32 ich1,ich2,ich3,blkname,cvalue
      character*32 cmosnk, cmosrc, attsrc, attsnk
      character*32 ctype,clen,cintrp,cpers,cio,crank, ctype_snk,
     *             clen_snk,cinter_snk,cpers_snk,cio_snk,crank_snk
      real*8 sum,value
      character*32 isubname
      character*132 logmess
      integer icharlnf

C
C ######################################################################
C

c     set defaults
      isubname='math_integrate'
      ierror = -1

c     Get the command parameters
c     math/integrate/cmosnk/attsnk/RANGE/cmosrc/attsrc
c     1      2         3         4     5-7      8       9     
c     math/integrate/cmosnk/attsnk/RANGE/attsrc
c

c     get the cmo name and the attributes for result and field
c     that leaves the RANGE parameters for the indexed set
      cmosnk = cmsgin(3)
      attsnk = cmsgin(4)
      if (nwds.eq.9) then
        cmosrc = cmsgin(8)
        attsrc = cmsgin(9)
      else
        cmosrc = cmosnk
        attsrc = cmsgin(8)
      endif

C     ******************************************************************
C     Get Sink Mesh Object

C     Check the mesh object names
      call cmo_exist(cmosnk,ierr)
      if(ierr.ne.0) then
         write(logmess,'(a,a)')
     *   'INTEGRATE: Not a valid mesh object: ',cmosnk
         call writloga('default',1,logmess,1,ierrw)
         goto 9990
      endif
      if (cmosnk(1:icharlnf(cmosnk)).ne.cmosrc(1:icharlnf(cmosrc))) then
         write(logmess,'(a,a)')
     *   'INTEGRATE: Sink cmo and Source cmo must be the same.'
         call writloga('default',1,logmess,1,ierrw)
         goto 9990
      endif
      call cmo_select(cmosnk,ierr)
      call cmo_get_intinfo('nnodes',cmosnk,nnodes,ilen,ityp,ierr)
      if(ierr.ne.0) call x3d_error(isubname,'intinfo nnodes cmosink')
      call cmo_get_intinfo('nelements',cmosnk,nelements,ilen,ityp,ierr)
      if(ierr.ne.0) call x3d_error(isubname,'intinfo nelements cmosink')


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
c     Can only be an element set
         call cmo_get_info('xtetwd',cmosnk,ipxtetwd,length,ityp,ics)
         if (ics.ne.0) call x3d_error(isubname,'get xtetwd')
         ich1=cmsgin(5)
         ich2=cmsgin(6)
         ich3=cmsgin(7)
      endif

c     temporary storage for looping variable
      length=nelements
      call mmgetblk('mpary',isubname,ipmpary,length,1,ics)
      if(ics.ne.0) call x3d_error(isubname,'mmgetblk mpary')

      if(msgtype(5).eq.1) then
        mpno=0
        if(ipt2.le.0) ipt2=nelements
        if(ipt3.le.0) ipt3=1
        do i = ipt1,ipt2,ipt3
          mpno=mpno+1
          mpary(mpno) = i
        enddo
      else
        call eltlimc(ich1,ich2,ich3,ipmpary,mpno,nelements,xtetwd)
      endif
      call cmo_get_intinfo('idebug',cmosnk,idebug,len,ityp,ierr)
      if (mpno.gt.0.and.idebug.gt.0) then
         write(logmess,'(a,i10)')
     *    'elements in indexed set  = ',mpno
         call writloga('default',0,logmess,0,ierrw)
      elseif (mpno.le.0.) then
         write(logmess,'(a)') 'No elements in indexed set!'
         call writloga('default',1,logmess,1,ierrw)
         goto 9990
      endif

C
C     Done with command tokens
C
C     Get the mesh object info
c
      call cmo_get_intinfo('mbndry',cmosnk,mbndry,ilen,ityp,ierr)
      if(ierr.ne.0) call x3d_error(isubname,'intinfo mbndry cmosink')
      call cmo_get_intinfo('ndimensions_geom',cmosnk,
     *                      nsdgeom,ilen,ityp,ierr)
      call cmo_get_intinfo('ndimensions_topo',cmosnk,
     *                      nsdtopo,ilen,ityp,ierr)
      call cmo_get_intinfo('nodes_per_element',cmosnk,
     *                      nen,ilen,ityp,ierr)
      call cmo_get_intinfo('faces_per_element',cmosnk,
     *                      nef,ilen,ityp,ierr)

      call cmo_get_info('xic',cmosnk,ipxic,ilen,ityp,ierr)
      call cmo_get_info('yic',cmosnk,ipyic,ilen,ityp,ierr)
      call cmo_get_info('zic',cmosnk,ipzic,ilen,ityp,ierr)
      if(ierr.ne.0) call x3d_error(isubname,'get_info xyz cmosink')
      call cmo_get_info('itettyp',cmosnk,ipitettyp,ilen,ityp,ierr)
      call cmo_get_info('itetoff',cmosnk,ipitetoff,ilen,ityp,ierr)
      call cmo_get_info('itet',cmosnk,ipitet,ilen,ityp,ierr)
      if(ierr.ne.0) call x3d_error(isubname,'get_info itet cmosink')

c     Get source attribute info and assign attribute length
      call mmfindbk(attsrc,cmosnk,ipout,len,ierr)
      if(ierr.ne.0) then
         write(logmess,'(a,a,a,a,a)') 'attribute does not exist: ',
     *      '  cmo= ',cmosnk(1:icharlnf(cmosnk)),
     *      '  att= ',attsrc(1:icharlnf(attsrc))
         call writloga('default',0,logmess,0,ierrw)
         goto 9990
      endif
      call cmo_get_attparam(attsrc,cmosnk,idx,ctype,crank,
     *                      clen,cintrp,cpers,cio,ierr)
      if(ierr.ne.0) call x3d_error(isubname,'get_attparam source')
      if (clen(1:9).eq.'nelements') then
        ifelement = 1
        num_src = nelements
      elseif (clen(1:6).eq.'nnodes') then
        ifelement = 0
        num_src = nnodes
      else
         write(logmess,'(a,a,a,a,a)')'attribute is wrong length type: ',
     *      '  att = ',attsrc(1:icharlnf(attsrc)),
     *      '  length = ',clen(1:icharlnf(clen))
         call writloga('default',0,logmess,0,ierrw)
         goto 9990
      endif

C     Assign source attribute to xattsrc or iattsrc array
C     
      length=num_src
      len=icharlnf(attsrc)
      blkname=' '
      blkname(1:len)=attsrc
      if (ctype(1:7).eq.'VINT') then
         call mmgetpr(blkname,cmosnk,ipiattsrc,ics)
      elseif(ctype(1:7).eq.'VDOUBLE') then
         call mmgetpr(blkname,cmosnk,ipxattsrc,ics)
      else
         write(logmess,'(a,a,a,a,a10)')
     *   'Invalid attribute type for ',cmosnk(1:icharlnf(cmosnk)),
     *   ' ',blkname(1:len),ctype
         call writloga('default',1,logmess,0,ierrw)
         goto 9990
      endif

c     Get sink attribute info, create if does not exist
      call cmo_get_attparam(attsnk,cmosnk,i,
     *        ctype_snk,crank_snk,clen_snk,cinter_snk,
     *        cpers_snk,cio_snk,ics)
      if(ics.ne.0) then
         write(logmess,'(a,a,a,a,a)') 'attribute does not exist: ',
     *        '  cmo = ',cmosnk(1:icharlnf(cmosnk)),
     *        '  att = ',attsnk(1:icharlnf(attsnk))
         call writloga('default',0,logmess,0,ierrw)
c       try to create the attribute, derive from source
c       length must be nelement and type must be double
         ctype_snk = 'VDOUBLE'
         clen_snk = 'nelements'
         logmess='cmo/addatt/' //
     *      cmosnk(1:icharlnf(cmosnk)) // ' / ' //
     *      attsnk(1:icharlnf(attsnk)) // 
     *      '/VDOUBLE/scalar/nelements/' //
     *      cpers(1:icharlnf(cpers)) // ' / ' //
     *      cio(1:icharlnf(cio)) // ' ; finish'
         call dotaskx3d(logmess,ierror)
         if(ierror.eq.0) then
            call cmo_verify_cmo(cmosnk,ierror)
            if(ierror.ne.0) then
               write(logmess,'(a,a,a)')
     *         'CMO_ADDATT error: ',
     *         'Mesh Object is not consistent: ', cmosnk
               call writloga('default',0,logmess,0,ierr)
               goto 9990
            endif
         endif
         call mmfindbk(attsnk,cmosnk,ipxattsnk,ilen,ics)        
      else
         if(ctype_snk.ne.'REAL'.and.ctype_snk.ne.'INT') then
            call mmfindbk(attsnk,cmosnk,ipxattsnk,ilen,ics)
            if(ics.ne.0) then
               call x3d_error(isubname,'mmgetpr xatt sink')
               go to 9990
            endif
         endif
      endif

      call cmo_get_attparam(attsnk,cmosnk,idx,ctype_snk,crank_snk,
     *                 clen_snk,cinter_snk,cpers_snk,cio_snk,ierr)
      if(ierr.ne.0) call x3d_error(isubname,'get_attparam sink')

c     the sink attribute must be element or scalar length
      if (clen_snk(1:9).ne.'nelements'.and.
     *    clen_snk(1:6).ne.'scalar') then
         write(logmess,'(a,a,a,a,a)')'illegal attribute length: ',
     *      '  att = ',attsnk(1:icharlnf(attsnk)),
     *      '  length = ',clen_snk(1:icharlnf(clen_snk))
         call writloga('default',0,logmess,0,ierrw)
         goto 9990
      endif

      if(idebug.gt.0) then
        write(logmess,'(a,a15,a10,a10,a10)')'sink attribute:   ',
     *  attsnk,ctype_snk,clen_snk,cinter_snk
        call writloga('default',0,logmess,0,ierrw)
        write(logmess,'(a,a15,a10,a10,a10)')'source attribute: ',
     *  attsrc,ctype,clen,cintrp
        call writloga('default',0,logmess,0,ierrw)
      endif

C     Memory allocation and setup done  ********************************


C**********************************************************************
C     INTEGRATE each element *****************************************
      sum=zero
      do j = 1,mpno
        idx=mpary(j)
        nen=nelmnen(itettyp(idx))

        if (idebug.ge.1) then
          write(logmess,"(a)")'----------------------------------------'
          call writloga('default',0,logmess,0,ierrw)
          write(logmess,"(a,i14)")'element: ',idx
          call writloga('default',0,logmess,0,ierrw)
          if (idebug.ge.5) then
          write(logmess,"(a)")
     * '       new           old              d                norm'
          call writloga('default',0,logmess,0,ierrw)
          endif
        endif
c
c  do simple integration
c  if source is node based (ifelement=0) take value at midpoint
c  if source is element based take value of element
c  multiply value by element volume
c  if sink has length elements, save value*volume for each element
c  is sink is scalar, sum up and save sum of value*volume 
c
        if(ifelement.eq.0) then
           value=zero
           do i = 1,nen
              if(ctype.eq.'VDOUBLE') then
                 value=value+xattsrc(itet(itetoff(idx)+i))
              elseif(ctype.eq.'VINT') then
                 value=value+iattsrc(itet(itetoff(idx)+i))
              endif
           enddo
           value=value/dble(nen)
        else
           if(ctype.eq.'VDOUBLE') then
              value=xattsrc(idx)
           elseif(ctype.eq.'VINT') then
              value=iattsrc(idx)
           endif
        endif
        do i=1,nen
           xicvol(i)=xic(itet(itetoff(idx)+i))
           yicvol(i)=yic(itet(itetoff(idx)+i))
           zicvol(i)=zic(itet(itetoff(idx)+i))
        enddo
        call volume_element(itettyp(idx),xicvol,yicvol,zicvol,
     *     privol)
        value=value*privol
        if(ctype_snk.eq.'REAL') then
           ityp=2
           sum=sum+value
        elseif(ctype_snk.eq.'INT') then
           ityp=1
           sum=sum+value
        else
           xattsnk(idx)=value
        endif
      ivalue=nint(sum)
      enddo
      if(ctype_snk.eq.'REAL'.or.ctype_snk.eq.'INT') then
        call cmo_set_attinfo(attsnk,cmosnk,ivalue,sum,cvalue,ityp,
     *    ics)
      endif
      ierror = 0
      go to 9999

c     Return here if error is found before setup is done

 9990 if(ierror.ne.0) then
         write(logmess,'(a)')'FATAL ERROR: INTEGRATE unable to begin.'
         call writloga('default',1,logmess,1,ierrw)
      endif
 9999 continue
      call mmrelprt(isubname,ics)
      return
      end
C     END math_integrate

