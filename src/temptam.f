 
C#####################################################################
C
C      FILE -
C
C      Source code for geological applications using x3dgen
C
C      CHANGE HISTORY -
C
C      Original version - T.Cherry - 97
C
C        $Log: temptam.f,v $
C        Revision 2.00  2007/11/09 20:04:04  spchu
C        Import to CVS
C
C
CPVCS
CPVCS       Rev 1.21   02 Oct 2007 12:40:28   spchu
CPVCS    original version
C
C#####################################################################
 
      subroutine trilayer_derive(imsgin,xmsgin,cmsgin,
     >                      msgtyp,nwds,ierr_return)
C#####################################################################
C
C     PURPOSE -
C           derive a 3rd trilayer between 2 others
C
C     SYNTAX -
C           TRILAYER DERIVE / PROPORTIONAL | CONSTANT /
C                             CMOSINK / CMOLOWER / CMOUPPER /
C                             [ABOVE | BELOW] /  XVALUE
C
C           proportional | constant
C                      the method of detirmining distance from derived layer
C                      PROPORTIONAL(lt 1.) is a percentage of thickness
C                      CONSTANT is xvalue distance from derived layer
C           cmosink    the new layer between cmolower and cmoupper,
C                      copied from derived layer and elevations changed
C           above | below
C                      above (or pos xvalue) makes cmoupper the derived layer
C                      below (or neg xvalue) makes cmolower the derived layer
C           xvalue     is a real number value for proportional | constant
C
C           cmonew is a copy of the cmolower or cmoupper it is derived from
C           the new layer will stay between the cmolower and cmoupper layers
C           the elevations will be made equal to the layer they cross
C           if they go beyond either layer
C
C     EXAMPLES -
C
C     trilayer derive   constant   cmonew cmolow cmoup   70.
C           will copy cmoup to cmonew, with elevations 70 below cmoup
C     trilayer derive   proportional   cmonew cmolow cmoup    .5
C           will create a layer half way between cmonew and cmoup
C     trilayer derive   proportional   cmonew cmolow cmoup   -.5
C           same as above, except cmonew is derived from cmolow
C
C
C
C     INPUT ARGUMENTS -
C         imsgin()  - Integer array of command input tokens
C         xmsgin()  - Real array of command input tokens
C         cmsgin()  - Character array of command input tokens
C         msgtyp()  - Integer array of command input token types
C         nwds      - Number of command input tokens
C
C     OUTPUT ARGUMENTS -
C        ierr_return - Error Return Code (==0 ==> OK, <>0 ==> Error)
C
C      CHANGE HISTORY -
C
CPVCS  Original version - tcherry - 9/97
CPVCS
C
C
C#####################################################################
C
 
C
      implicit none
C
C ARGS
      integer nwds, imsgin(nwds), msgtyp(nwds)
      REAL*8 xmsgin(nwds)
      character*32 cmsgin(nwds)
      integer ierr_return
 
C CMO  Definitions
      integer nodesa
      pointer (ipzica, zica)
      real*8  zica(1000000)
 
      integer nodesb
      pointer (ipzicb, zicb)
      real*8  zicb(1000000)
 
      pointer (ipznew, znew)
      real*8  znew(1000000)
 
C LOCAL
      integer ierr, ierrw
      integer inxt, i, ilen, ityp, len, idone
      integer ilow, ihigh, isame
      integer icharlnf
      real*8  xdist,xthick,xvalue,zsurf
      real*8  xmin,xmax,xmin2,xmax2,xavg,xsum
C
      character*32 cmonew, cmoa, cmob
      character*32 isubname
      character*12 surfopt, distopt
      character*160 cbuf
      character*132 logmess
C
C
C.....BEGIN
C
      isubname='trilayer_derive'
      ierr_return = -1
      idone = 0
 
c Parse command line
      inxt=3
 
c     3 = proportional or constant
      if (msgtyp(inxt).eq.3) then
        distopt=cmsgin(inxt)
        inxt=inxt+1
      else
        goto 9995
      endif
 
c     4 = new cmo name
      if (msgtyp(inxt).eq.3) then
        cmonew=cmsgin(inxt)
        inxt=inxt+1
      else
        goto 9995
      endif
 
 
c     5 = lower trilayer cmoa
      if (msgtyp(inxt).eq.3) then
        cmoa=cmsgin(inxt)
        inxt=inxt+1
      else
        goto 9995
      endif
 
c     6 = upper trilayer cmob
      if (msgtyp(inxt).eq.3) then
        cmob=cmsgin(inxt)
        inxt=inxt+1
      else
        goto 9995
      endif
 
c     7 = above or below
      if (msgtyp(inxt).eq.3) then
        surfopt=cmsgin(inxt)
        inxt=inxt+1
      elseif (msgtyp(inxt).eq.2) then
        xdist=xmsgin(inxt)
        inxt=inxt+1
        if (xdist.lt. 0.) then
          surfopt = 'below'
        else
          surfopt = 'above'
        endif
      else
        goto 9995
      endif
 
      if(surfopt(1:5).ne.'below'.and.surfopt(1:5).ne.'above')goto 9995
      if(distopt(1:4).ne.'prop'.and.distopt(1:5).ne.'const') goto 9995
 
c     8 = proportional or constant value
      if (msgtyp(inxt).eq.2) then
        xdist=xmsgin(inxt)
        inxt=inxt+1
      elseif (msgtyp(inxt).eq.1) then
        xdist=dble(xmsgin(inxt))
        inxt=inxt+1
      else
        goto 9995
      endif
 
9995  if(inxt.lt.8) then
        call x3d_error('Syntax error: ','trilayer derive')
        goto 9999
      endif
 
      if (surfopt(1:5).eq.'below' .and. xdist.gt.0.0) xdist=-xdist
      xvalue=xdist
C
 
 
C   Check for character data names for cmoain, cmobin
C     ierr.eq.0 means that the cmo already exists.
      call cmo_exist(cmoa,ierr)
      if(ierr.ne.0) call x3d_error(isubname,cmoa)
      call cmo_exist(cmob,ierr)
      if(ierr.ne.0) call x3d_error(isubname,cmob)
      if(ierr.ne.0) goto 9999
 
C    Create new cmo between bottom a and top b
      len = icharlnf(cmonew)
      if (surfopt(1:5).eq.'below') then
        cbuf = 'cmo/copy/' // cmonew(1:len) // '/ '
     *       // cmoa(1:icharlnf(cmoa))
     *       // '/ ; finish'
      else
        cbuf = 'cmo/copy/' // cmonew(1:len) // '/ '
     *       // cmob(1:icharlnf(cmob))
     *       // '/ ; finish'
      endif
      call dotaskx3d(cbuf,ierr)
      if(ierr .ne. 0)call x3d_error(isubname, 'cmo copy')
      call cmo_get_info('zic',cmonew,ipznew,ilen,ityp,ierr)
      if(ierr.ne.0) call x3d_error(isubname,'get info zic')
 
C     cmoa - bottom surface
      call cmo_get_info('nnodes',cmoa,nodesa,ilen,ityp,ierr)
      if(ierr.ne.0) call x3d_error(isubname,cmoa)
      call cmo_get_info('zic',cmoa,ipzica,ilen,ityp,ierr)
      if(ierr.ne.0) call x3d_error(isubname,'get info zic')
 
 
C     cmob - top surface
      call cmo_get_info('nnodes',cmob,nodesb,ilen,ityp,ierr)
      if(ierr.ne.0) call x3d_error(isubname,cmob)
      call cmo_get_info('zic',cmob,ipzicb,ilen,ityp,ierr)
      if(ierr.ne.0) call x3d_error(isubname,'get info zic')
C
 
      if  (nodesa .ne. nodesb) then
         write(logmess,9010) nodesa, nodesb
 9010    format('ERROR: Total number nodes differ ',i10,' ne ',i10)
         call writloga('default',1,logmess,1,ierrw)
         ierr_return = 1
         return
      endif
 
 
C-----LOOP OVER Z VALUES----------------------------------------
C     Compute elevations
      ilow=0
      ihigh=0
      isame=0
      xsum=0.0
      do i = 1, nodesa
 
         if (surfopt(1:5).eq.'above') zsurf = zica(i)
         if (surfopt(1:5).eq.'below') zsurf = zicb(i)
 
         if ( zica(i) .ge. zicb(i) ) then
           xthick = 0.0
         else
           xthick = zicb(i) - zica(i)
         endif
 
         if (i.eq.1) then
           xmin=xthick
           xmax=xthick
         else
           if (xthick.lt.xmin) xmin=xthick
           if (xthick.gt.xmax) xmax=xthick
         endif
 
 
c........proportional
         if (distopt(1:4).eq.'prop') then
           if (xthick .le. 0.0) then
              znew(i) = zsurf
           else
              xdist= xvalue*xthick
              znew(i) = zsurf + xdist
           endif
 
c........constant
         else
            znew(i) = zsurf + xdist
            if (xthick .le. abs(xdist)) znew(i) = zica(i) + (.5*xthick)
         endif
 
 
c        error check
         if(xthick.gt. 0.0 .and. znew(i).lt.zica(i)) ilow=ilow+1
         if(xthick.gt. 0.0 .and. znew(i).gt.zicb(i)) ihigh=ihigh+1
         if(xthick.eq. 0.0) isame=isame+1
 
         xthick= abs(xdist)
         xsum= xsum+xthick
         if (i.eq.1) then
           xmin2=xthick
           xmax2=xthick
         else
           if (xthick.lt.xmin2) xmin2=xthick
           if (xthick.gt.xmax2) xmax2=xthick
         endif
 
      enddo
C-----END LOOP over z values--------------------------------------------
 
      xavg=xsum/nodesa
      idone = 1
 
      if (ihigh.ne.0) then
        write(logmess,'(i10,a,i10,a,a)')
     *  ihigh,' of ',nodesa,' ABOVE ',cmob(1:icharlnf(cmob))
        call writloga('default',1,logmess,1,ierrw)
      endif
      if (ilow.ne.0) then
        write(logmess,'(i10,a,i10,a,a)')
     *  ilow,' of ',nodesa,' BELOW ',cmoa(1:icharlnf(cmoa))
        call writloga('default',1,logmess,1,ierrw)
      endif
      if (isame.ne.0) then
        write(logmess,'(a,i10,a,i10)')
     *  'Warning: ',isame,' elevations SAME out of ',nodesa
        call writloga('default',1,logmess,1,ierrw)
      endif
      write(logmess,'(a,1pe15.7)')
     *'Created new trilayer with average distance of ',xavg
      call writloga('default',0,logmess,0,ierrw)
 
      write(logmess,'(a,2(2x,1pe15.7))')
     *'New height: min/max ',xmin2,xmax2
      call writloga('default',0,logmess,1,ierrw)
 
      write(logmess,'(a,2(2x,1pe15.7))')
     *'Old height: min/max ',xmin,xmax
      call writloga('default',0,logmess,0,ierrw)
 
 9999 if (idone.ne.1 .or. ilow.ne.0 .or. ihigh.ne.0 ) then
        write(logmess,'(a)')'ERRORS occurred in trilayer derive!'
        call writloga('default',1,logmess,1,ierrw)
      endif
 
      ierr_return = 0
      return
      end
c end trilayer derive
 
 
      subroutine attributes(imsgin,xmsgin,cmsgin,
     >                      msgtyp,nwds,ierr_return)
C#####################################################################
C
C      PURPOSE -
C           list attribute information to output
C      NOTE: this code was replaced by cmo/printatt/cmo1/list
C
C
C     INPUT ARGUMENTS -
C         imsgin()  - Integer array of command input tokens
C         xmsgin()  - Real array of command input tokens
C         cmsgin()  - Character array of command input tokens
C         msgtyp() - Integer array of command input token types
C         nwds      - Number of command input tokens
C
C     OUTPUT ARGUMENTS -
C        ierr_return - Error Return Code (==0 ==> OK, <>0 ==> Error)
C
C      CHANGE HISTORY -
C
CPVCS  Original version - tcherry - 9/97
CPVCS
C
C
C#####################################################################
C
c     This option was added to the report command as follows:
C
C     REPORT / ATTRIBUTES
C     REPORT / ATTRIBUTES / NODE
C     REPORT / ATTRIBUTES / ELEMENT
C     REPORT / attname
C     REPORT / ATTRIBUTES / MINMAX
C     REPORT / ATTRIBUTES / MINMAX / pset or start,end,stride
C
C     ATTRIBUTES        writes a list of all attributes in cmo.
C     attname           writes info only on selected attribute.
C              NODE     writes a list of all node attributes in cmo
C              ELEM     writes a list of all elem attributes in cmo
C              MINMAX   writes min/max values for all or selected attname
C       pset or 1,0,0   writes min and max values for all or selected
C                       attributes within selected point set
Ctodo   pset option not working correctly
C
C     see command REPORT for further details
C
C#######################################################################
C
C
      implicit none
C
C ARGS
      integer nwds, imsgin(nwds), msgtyp(nwds)
      REAL*8 xmsgin(nwds)
      character*32 cmsgin(nwds)
      integer ierr_return
C LOCAL
      integer ierror,ierr, ics, ierrw
      integer i, ilen, ityp, itet, num, len
      integer imin, imax
      integer iprint, ipntset, itotal
      integer icharlnf
      real*8  xmin,xmax
C
      pointer (ipmpary,mpary)
      integer mpary(10000000)
      pointer (ipisetwd,isetwd)
      pointer (ipitp1, itp1)
      integer isetwd(10000000)
      integer itp1(10000000)
 
      integer length,icmotype
      integer mpno,ipt1,ipt2,ipt3,nnodes
      integer ierror_return,index
      character*32 ich1,ich2,ich3
 
      character*8 sbname, defnam, parname
      character*32 clength, cname
      character*32 cmo, attnam, isubname
      character*32 clist, coption,ctype,crank,cinter,cpers,cio
      character*132 logmess
C
C
C.....BEGIN
C
      isubname='attributes'
 
C  Check that user has specified a valid mesh object.
      call cmo_get_name(cmo,ierr)
      if(ierr.ne.0) then
         write(logmess,'(a)')
     *   'ATTRIBUTES: ',cmo,' not a valid mesh object'
         call writloga('default',0,logmess,0,ierrw)
         ierr_return = -1
         goto 9999
      endif
      len=icharlnf(cmo)
 
C     Check to see if this Mesh Object exists.
      call cmo_exist(cmo,ierr)
      if(ierr.ne.0) then
         write(logmess,'(a,a)')
     *   'ATTRIBUTES: Mesh Object does not exist: ', cmo
         call writloga('default',0,logmess,0,ierrw)
         ierr_return = -1
         goto 9999
      endif
 
C     to set point index boundaries
      call cmo_get_info('nnodes',cmo,nnodes,length,ityp,ierr)
      if (ierr .ne. 0) call x3d_error(isubname,'cmo_get_info')
 
      call mmgetblk('mpary',isubname,ipmpary,nnodes,2,ics)
      if (ics .ne. 0) call x3d_error(isubname,'mmgetblk')
C
      call cmo_get_info('isetwd',cmo,ipisetwd,length,icmotype,ierr)
      if (ierr .ne. 0) call x3d_error(isubname,'cmo_get_info')
      call cmo_get_info('itp1',cmo,ipitp1,length,icmotype,ierr)
      if (ierr .ne. 0) call x3d_error(isubname,'cmo_get_info')
 
C     Parse commands, num to list, list options
C     2 = attributes or att_name
C     3 = coption    or node     or elem    or 1,0,0 or pset
C     4 = coption                           or 1,0,0 or pset
C     5 = 1,0,0      or pset
C
C.....set default args
      mpno=0
      coption = '-none-'
      clist=cmsgin(2)
      attnam=cmsgin(2)
      ipt1=1
      ipt2=0
      ipt3=0
      ich1=' '
      ich2=' '
      ich3=' '
 
C.....loop through arguments
C     set the point index boundaries,
C     mpary will hold index into valid nodes
      ipntset=0
      do i = 2, nwds
 
        if (msgtyp(i).eq.1 .and. ipntset.eq.0)then
          ipt1=imsgin(i)
          ipt2=imsgin(i+1)
          ipt3=imsgin(i+2)
          call pntlimn(ipt1,ipt2,ipt3,ipmpary,mpno,
     *      nnodes,isetwd,itp1)
          ipntset=1
 
        elseif (msgtyp(i).eq.3) then
 
          if (cmsgin(i)(1:9).eq.'attribute' .or.
     *        cmsgin(i)(1:3).eq.'all') then
              clist='all'
          elseif (cmsgin(i)(1:4).eq.'node' ) then
              clist='node'
          elseif (cmsgin(i)(1:4).eq.'elem' ) then
              clist='elem'
          elseif (cmsgin(i)(1:6).eq.'minmax') then
              coption='minmax'
          elseif (cmsgin(i)(1:3).eq.'pset' .and. ipntset.eq.0) then
              ich1=cmsgin(i)
              ich2=cmsgin(i+1)
              ich3=cmsgin(i+2)
              call pntlimc(ich1,ich2,ich3,ipmpary,mpno,
     *         nnodes,isetwd,itp1)
            ipntset=1
          endif
 
        endif
      enddo
      if (ipntset.eq.0) then
          call pntlimn(ipt1,ipt2,ipt3,ipmpary,mpno,
     *      nnodes,isetwd,itp1)
      endif
 
      call cmo_get_info('number_of_attributes',cmo,num,
     *                   ilen,itet,ierror)
 
C.....loop through attributes, write information
      cname='-undef- '
      sbname='sbcmoatt'
      defnam='default'
      parname='length'
      itotal=0
      do i = 1, num
 
C       decide if there is output for this attribute
        ics=0
        if(clist(1:3).eq.'all') then
          call cmo_get_attribute_name(cmo,i,cname,ics)
          iprint=1
        elseif(clist(1:4).eq.'node') then
          call cmo_get_attribute_name(cmo,i,cname,ics)
csb       call get_info_c(cname,cmo,sbname,parname,clength,ics)
          call cmo_get_attparam(cname,cmo,index,ctype,crank,
     *    clength,cinter,cpers,cio,ierror_return)
          if (clength(1:4).eq.'nnod') iprint=1
        elseif(clist(1:4).eq.'elem') then
          call cmo_get_attribute_name(cmo,i,cname,ics)
csb       call get_info_c(cname,cmo,sbname,parname,clength,ics)
          call cmo_get_attparam(cname,cmo,index,ctype,crank,
     *    clength,cinter,cpers,cio,ierror_return)
          if (clength(1:4).eq.'nele') iprint=1
        elseif(clist(1:3).eq.'xyz' .and. i.eq.1) then
          cname='xic'
          iprint=1
        elseif(clist(1:3).eq.'xyz' .and. i.eq.2) then
          cname='yic'
          iprint=1
        elseif(clist(1:3).eq.'xyz' .and. i.eq.3) then
          cname='zic'
          iprint=1
        elseif(i.eq.1) then
          cname=attnam
          iprint=1
        endif
        if (ics.ne.0) goto 9999
 
c       get length of attribute
csb     call get_info_c(cname,cmo,sbname,parname,clength,ierr)
                  call cmo_get_attparam(cname,cmo,index,ctype,crank,
     *    clength,cinter,cpers,cio,ierror_return)
        if (ierr .ne. 0 ) goto 9999
        call cmo_get_info(clength,cmo,len,ilen,ityp,ierr)
        if (ierr .ne. 0 ) goto 9999
 
 
        if (iprint.ne.0) then
 
C       ****************************************************************
C       LIST WITH OPTION MINMAX
        if (coption(1:6).eq.'minmax') then
 
          if (itotal.eq.0) then
            write(logmess,'(a)')
     >'INDEX   ATTRIBUTE NAME            MIN           MAX      LENGTH'
            call writloga('default',0,logmess,0,ierrw)
          endif
 
          call cmo_minmax(cmo,cname,
     >                    xmin,xmax,imin,imax,
     >                    mpno, mpary, ityp,ierr)
          if (ierr .ne. 0) then
             write(logmess,'(a,a)')
     >       'minmax failed for attribute ',cname
              call writloga('default',0,logmess,1,ierrw)
              goto 9999
          endif
 
          if (ityp.eq.1) then
            write(logmess,'(i5,a,a18,2(1x,i13),i10)') i, ') ',
     >      cname(1:17),imin,imax,len
            call writloga('default',0,logmess,0,ierrw)
            itotal=itotal+1
 
          else
            write(logmess,'(i5,a,a18,2(1x,1pe13.6),i10)') i, ') ',
     >      cname(1:17),xmin,xmax,len
            call writloga('default',0,logmess,0,ierrw)
            itotal=itotal+1
          endif
 
C       ****************************************************************
C       LIST ONLY
        else
          if (itotal.eq.0) then
            write(logmess,'(a)')
     >'INDEX   ATTRIBUTE NAME          LENGTH'
            call writloga('default',0,logmess,0,ierr)
          endif
          write(logmess,'(i5,a,a18,i10)') i, ') ', cname(1:17),len
          call writloga('default',0,logmess,0,ierrw)
          itotal=itotal+1
 
        endif
        endif
        iprint=0
 
      enddo
 
      ierr_return = 0
 9999 call mmrelprt(isubname,ics)
      if (ics.ne.0) call x3d_error(isubname,'mmrelprt')
 
      return
      end
 
      subroutine smooth_recon(imsgin,xmsgin,cmsgin,
     >                      msgtyp,nwds,ierr_return)
C#####################################################################
C
C      PURPOSE -
C          call smooth; recon; N number of times
C          need to implement pset option
C
C
C     INPUT ARGUMENTS -
C         imsgin()  - Integer array of command input tokens
C         xmsgin()  - Real array of command input tokens
C         cmsgin()  - Character array of command input tokens
C         msgtyp() - Integer array of command input token types
C         nwds      - Number of command input tokens
C
C     OUTPUT ARGUMENTS -
C        ierr_return - Error Return Code (==0 ==> OK, <>0 ==> Error)
C
C      CHANGE HISTORY -
C
CPVCS  Original version - Carl Gable - 1/97
CPVCS  Revised  version - tcherry    - 9/97
CPVCS
C
C
C#####################################################################
C
C      FORMAT:
C
C      SMOOTH_RECON / [ smooth_type ] / n_iteration / [pset,get,pname]
C
c      smooth type - Same choices as smooth command ie. mega or esug
C                    default is esug
C                    the default node alter type is position
C                    recon is used with option 0,no new nodes are added
C
C      EXAMPLE:
c      smooth_recon/ mega / 3
c           call the smooth recon calls 3 times with mega smooth type
C
C      smooth_recon 2
c           call the smooth recon calls 2 times with esug smooth type
C
C      smooth_recon/esug/2/pset,get,p1
c           call the smooth recon calls 2 times with esug smooth type
c           but smooth only moves points in pset p1
C
C      SAMPLE OUTPUT:
C       _______
C      |
C      |      4  smooth_recon 2
C      |
C      | SMOOTH: position option
C      | nodes in point set  =        384
C      | SMOOTH: Smooth 2D triangular mesh using ESUG
C      |      6   finish
C      |      7  recon   0
C      | RECON2D reconnection loop:   1
C      |      8   finish
C      |
C      |      9  smooth position  esug   1 0 0
C      | SMOOTH: position option
C      | nodes in point set  =        384
C      | SMOOTH: Smooth 2D triangular mesh using ESUG
C      |     10   finish
C      |     11  recon   0
C      |  RECON2D reconnection loop:   1
C      |     12   finish
C      | SMOOTH_RECON done for pass:     2
C      |______
C
C#######################################################################
C
C
      implicit none
C
C ARGS
      integer nwds, imsgin(nwds), msgtyp(nwds)
      REAL*8 xmsgin(nwds)
      character*32 cmsgin(nwds)
      integer ierr_return
C LOCAL
      integer i, nend, ilast
      integer ierr, ierrw
      integer icharlnf
      character*32 coption
      character*32 pname
      character*132 logmess
      character*132 cbuf, cbuf2
      integer ifset
C
C BEGIN
      ierr_return=0
      coption='esug'
      ifset = 0
      nend=1
c
c     Parse command string, allow:
c     smooth_recon
c     smooth_recon 3
c     smooth_recon 3 esug
c     smooth_recon esug
c     smooth_recon 3 esug pset,get,pname
 
      ilast = nwds
      do i = 1, nwds
        if (msgtyp(i).eq.3) then
           if (cmsgin(i)(1:4) .eq. 'pset') then
             pname = cmsgin(i+2)
             ilast = nwds-3
             ifset = 1
           endif
        endif
      enddo
 
      do i = 1, ilast
        if (msgtyp(i).eq.1) nend = imsgin(i)
        if (msgtyp(i).eq.3) coption = cmsgin(i)
      enddo
 
 
C.....loop through the smooth; recon; all points
      if (nwds.le.3 ) then
 
         cbuf = 'smooth/position/ '//
     *         coption(1:icharlnf(coption)) //
     *         ' / 1 0 0 / ; finish '
 
         do i = 1, nend
 
           call dotaskx3d(cbuf,ierr)
           if (ierr.ne.0) goto 9999
           call dotaskx3d('recon / 0 / ; finish ',ierr)
           if (ierr.ne.0) goto 9999
 
           write(logmess,'(a,i5)')'SMOOTH_RECON done for pass: ',i
           call writloga('default',0,logmess,0,ierrw)
         enddo
 
 
C......loop through the smooth; recon; on pset only
       elseif (ifset .eq. 1) then
 
         cbuf = 'smooth/position/ '//
     *         coption(1:icharlnf(coption)) //
     *         ' / pset,get,' // pname(1:icharlnf(pname)) //
     *         ' / ; finish '
         cbuf2 = 'recon/0/ '//
     *         ' / ' // pname(1:icharlnf(pname)) //
     *         ' / ; finish '
 
         do i = 1, nend
 
           call dotaskx3d(cbuf,ierr)
           if (ierr.ne.0) goto 9999
 
           call dotaskx3d('recon / 0 / ; finish ',ierr)
           if (ierr.ne.0) goto 9999
 
           write(logmess,'(a,i5)')'SMOOTH_RECON done for pass: ',i
           call writloga('default',0,logmess,0,ierrw)
         enddo
 
C......else syntax error
       else
         write(logmess,'(a)') 'SMOOTH_RECON: too many arguments.'
         call writloga('default',0,logmess,1,ierrw)
       endif
 
 9999  if (ierr.ne.0) then
         write(logmess,'(a,i3)')
     >   'SMOOTH_RECON: failed at iteration ',i
         call writloga('default',0,logmess,1,ierrw)
         ierr_return = 1
       endif
 
       return
       end
 
      subroutine mktrilayer(imsgin,xmsgin,cmsgin,msgtyp,nwds,ierr)
C
C#####################################################################
C
C      PURPOSE -
C       Given 2 trilayer cmo's, create 3rd trilayer
C
C
C     INPUT ARGUMENTS -
C         imsgin()  - Integer array of command input tokens
C         xmsgin()  - Real array of command input tokens
C         cmsgin()  - Character array of command input tokens
C         msgtyp() - Integer array of command input token types
C         nwds      - Number of command input tokens
C
C     OUTPUT ARGUMENTS -
C
C
C      CHANGE HISTORY -
C
C
C
C#####################################################################
C
c      implicit real*8 (a-h, o-z)
      implicit none
C
C
C####################################################################
C
C     DEFINE THE MESH_OBJECT POINTERS.
C
C
      integer nwds
      real*8 xmsgin(nwds)
      integer imsgin(nwds),msgtyp(nwds)
      character*32 cmsgin(nwds)
C
      integer i, ilen,ityp,ierr,ics
      integer iabove, iopt_con, iopt_trunc, ioptpri
      real*8 epsilonl, xvalue, xdist
      character*32 isubname, cmoin, cmotrunc, cmoout
      character*132 logmess
C
C CMO Definitions
      integer nodesa, nodesb, nnode, nelem
c     real*8  zlist(1000000)
      pointer (ipzic, zic)
      real*8  zic(1000000)
c
      pointer (ipzica, zica)
      real*8  zica(1000000)
      pointer (ipzicb, zicb)
      real*8  zicb(1000000)
C
C BEGIN
      ierr=0
      isubname='mktrilayer'
      iabove=1
      iopt_con=1
      ioptpri=0
      iopt_trunc=1
      epsilonl=0.
 
      call get_epsilon('epsilonl', epsilonl)
      write(logmess,'("epsilonl: ",1pe15.7)')
      call writloga('default',0,logmess,0,ics)
c
C READ PARSER VALUES, i is next value, nwds is last
      cmoin = cmsgin(2)
      cmotrunc = cmsgin(3)
      cmoout = cmsgin(4)
      xvalue = xmsgin(5)
      if (cmsgin(6)(1:4) .eq. 'cons') iopt_con = 1
      if (cmsgin(6)(1:4) .eq. 'prop') iopt_con = 0
C
C Setup input CMO's
C     cmoin - master surface
      call cmo_get_info('nnodes',cmoin,nodesa,ilen,ityp,ierr)
      call cmo_get_info('nelements',cmoin,nelem,ilen,ityp,ierr)
      call cmo_get_info('zic',cmoin,ipzica,ilen,ityp,ierr)
      if(ierr.ne.0) call x3d_error(isubname,'get info zica')
 
C     cmotrunc - truncation surface
      call cmo_get_info('nnodes',cmotrunc,nodesb,ilen,ityp,ierr)
      call cmo_get_info('zic',cmotrunc,ipzicb,ilen,ityp,ierr)
      if(ierr.ne.0) call x3d_error(isubname,'get info zicb')
C
      if (nodesa.ne.nodesb) then
         write(logmess,9010) nodesa, nodesb
 9010    format('ERROR: Total number nodes differ ',i8,' ne ',i8)
         call writloga('default',1,logmess,0,ierr)
         ierr = ierr+1
         return
      else
         nnode = nodesa
      endif
C
C Derive new output cmo
      call cmo_exist(cmoout,ierr)
      if(ierr.ne.0) then
         call cmo_copy(cmoout,cmoin,ierr)
         if(ierr.ne.0) call x3d_error(isubname,'copy cmo')
      endif
      call cmo_select(cmoout,ierr)
      call cmo_set_info('nnodes',cmoout,nnode,1,1,ierr)
      call cmo_set_info('nelements',cmoout,nelem,1,1,ierr)
      call cmo_newlen(cmoout,ierr)
      if(ierr.ne.0) call x3d_error(isubname,cmoout)
C
      call cmo_get_info('zic',cmoout,ipzic,ilen,ityp,ierr)
      if(ierr.ne.0) call x3d_error(isubname,'get info zic')
 
 
C-----LOOP OVER Z VALUES---------------------------------------
c assume cmoin is the source, cmo_trunc is used to truncate new layer
c for now assume cmoin is below cmo_trunc,
c     so layer will be created above cmoin
c
      do i = 1, nnode
 
c     constant value above input surface
      if (iopt_con.ne.0) then
        zic(i) = zica(i)+xvalue
        if(zic(i).gt.zicb(i)) zic(i)=zicb(i)
 
c     proportional value above input surface
      else
        xdist = zicb(i)-zica(i)
        if(xdist.le.epsilonl) then
           zic(i)=zica(i)
        else
           zic(i)= xvalue*xdist
        endif
        if(zic(i).gt.zicb(i)) zic(i)=zicb(i)
      endif
 
      enddo
C-----END LOOP over z values-----------------------------------
 
      call mmrelprt(isubname,ics)
 
 999  if (ierr.ne.0) call x3d_error(isubname,'error at end.')
 
      return
      end
C
c
       subroutine trilayertotet(imsgin,xmsgin,cmsgin,msgtyp,nwds,ierr)
C
C#####################################################################
C
C
C      PURPOSE -
C
C       Input is a trilayer cmo created with multiple tin
C       (triangulated surfaces)files and assumes that the surfaces
C       have identical connectivity. Use mread/trilayers command.
C       This routine calls pgg and hextotet with appropriate options.
C       By default hextotet is called with 3 for prism elements.
C
C      SYNTAX:
C
C      TRILAYERTOTET/
C      nlayers n | nnodes n / cmoout/cmoin/ ntet / usrclr | stratclr [prism]
C
C        nlayers   - character indicating number of TIN surfaces merged into
C                    the trilayer cmo followed by integer value
C        nnodes    - alternate to nlayers,  n is the number of nodes
C                    in each trilayer surface
C        cmoout    - the output tet cmo
C        cmoin     - the input trilayer cmo
C        ntet      - number of tets to break each prism into
C                    3 and 18 are reccommended, although the regular
C                    hex options of 6 and 24 can be called. Default
C                    is 3 tets from 1 prism.
C                    Prisms are formed into degenerate hexes which
C                    can cause degenerate tets for ntet = 6 or 24
C                    then volumes will not be
C                    removed during the hextotet phase as this will
C                    result in holes in the mesh
C           usrclr - keeps color of elements same
C         stratclr - colors by layer order
C        prism     - writes avs file "prism.inp"
C
C      EXAMPLES:
C
C      trilayertotet/ nodepersur 357 /cmotet/cmo1/usrclr/ 3
C      trilayertotet/ layers 4 / cmotet/cmotri/
C
C      EXAMPLE OUTPUT:
C
C     _____
C    |
C    |
C    |    47  * Create trilayer cmo from TINS
C    |    45  cmo create cmo1
C    | Enter a command
C    |    46  mread trilayers avs tri1.inp tri2.inp tri3.inp tri4.inp
C    |        TRI LAYERS:     4 files read out of     4
C    |
C    |    47  * TO TET
C    | Enter a command
C    |    48  trilayertotet 4 cmotet cmo1 3
C    |
C    |    49  pgg -defp- cmo1
C    |        Total Zero Prism Volumes:            1
C    |    55  hextotet      3 cmotet       -defp-
C    |        Epsilon-distance, distmax:   1.4142136E-06  2.0000000E+00
C    |        Epsilon-volume, volmax:   5.0000000E-07  5.0000000E-01
C    |        Deleting tets:          9         3
C    |    59  cmo delete -defp-
C    |    60  finish
C    |
C    |_____
C
C
C      INPUT ARGUMENTS -
C         imsgin()  - Integer array of command input tokens
C         xmsgin()  - Real array of command input tokens
C         cmsgin()  - Character array of command input tokens
C         msgtyp() - Integer array of command input token types
C         nwds      - Number of command input tokens
C
C      OUTPUT ARGUMENTS -
C
C
C      CHANGE HISTORY -
C
C      Original version - T.Cherry - 97
C
C#####################################################################
C
      implicit none
C
C
C####################################################################
C
C     DEFINE THE MESH_OBJECT POINTERS.
C
C
      integer nwds
      real*8 xmsgin(nwds)
      integer imsgin(nwds),msgtyp(nwds)
      character*32 cmsgin(nwds)
C
      integer nlayers, nodpersur, ntet, ioptdegen, stratclr, iwrtpri
      integer i,ii,ierr
      integer icharlnf
      character*32 isubname, cmoout, cmoin, cmoh
      character*1024  cbuf
C
C BEGIN
      isubname = 'trilayertotet'
      ierr = 0
      ntet = 3
      stratclr = 0
      nodpersur = 0
      nlayers = 0
      ioptdegen = 0
      iwrtpri = 0
      cmoh = '-defh-'
      cmoin = '-notset-'
      cmoout = '-notset-'
C
C     READ PARSER VALUES, i is next value, nwds is last
c     n(nod)s or (nod)persur = nodes per surface
c     n(lay)ers or (lay)ers  = layers for cmo
c     just an integer means nlayers for historical capatibility
      i = 2
      if (msgtyp(2).eq.3 ) then
        if (cmsgin(2)(1:3).eq.'nod' .or.
     >      cmsgin(2)(2:4).eq.'nod') then
          nodpersur = imsgin(3)
          i= i+2
        elseif (cmsgin(2)(1:3).eq.'lay' .or.
     >          cmsgin(2)(2:4).eq.'lay') then
          nlayers = imsgin(3)
          i= i+2
        else
          nlayers = imsgin(2)
          i= i+1
        endif
      endif
 
      cmoout = cmsgin(i)
      i= i+1
      cmoin = cmsgin(i)
      i= i+1
 
      do ii = i, nwds
      if(msgtyp(ii).eq.1) ntet = imsgin(ii)
      if(msgtyp(ii).eq.3 .and. cmsgin(ii)(1:5).eq.'degen') ioptdegen = 1
      if(msgtyp(ii).eq.3 .and. cmsgin(ii)(1:5).eq.'strat') stratclr = 1
      if(msgtyp(ii).eq.3 .and. cmsgin(ii)(1:6).eq.'prism') iwrtpri = 1
      if(msgtyp(ii).eq.3 .and. cmsgin(ii)(1:3).eq.'pri') iwrtpri = 1
      enddo
 
      call cmo_set_name(cmoin,ierr)
      if(ierr.ne.0) call x3d_error(isubname,'cmo_set_name')
      if(ierr.ne.0) goto 999
 
C-----convert trilayers cmoin to prismcmo named -defp-
      ierr=0
      if (stratclr.gt.0) then
        if (nodpersur .gt. 0) then
          write(cbuf,40)  cmoin, nodpersur
 40       format('pgg/-defp-/',a32,'/ nodes ',i10,'/stratclr/ ; finish')
        else
          write(cbuf,41)  cmoin, nlayers
 41       format('pgg/-defp-/',a32,'/layers ',i10,'/stratclr/ ; finish')
        endif
 
      else
        if (nodpersur .gt. 0) then
          write(cbuf,50)  cmoin, nodpersur
 50       format('pgg/-defp-/',a32,'/ nodes',i10,'/usrclr ; finish')
        else
          write(cbuf,51)  cmoin, nlayers
 51       format('pgg/-defp-/',a32,'/layers ',i10,'/usrclr ; finish')
        endif
      endif
 
      call dotaskx3d(cbuf,ierr)
      if(ierr.ne.0)call x3d_error(isubname, ' to prism')
 
C     write prism cmo
      if (iwrtpri.ne.0) then
        ierr=0
        cbuf='dump/avs/prism.inp/-defp-/ ; finish '
        call dotaskx3d(cbuf,ierr)
        if(ierr.ne.0)call x3d_error(isubname, 'dump prism.inp')
      endif
 
C If 6 or 24, then degenerate hex's formed, no remove of zero vols
C This option not available until new glolals done and _nosb versions done
c This option is not reccommended due to removal of 0 vol tets problems
      if (ntet.eq.6 .or. ntet.eq.24) then
        cbuf =' 6 and 24 options temporarily not available.'
        call writloga('default',1,cbuf,1,ierr)
        goto 999
cc
ccC     convert prismtohex
cc      ierr=0
cc      cbuf = 'prismtohex/-defh-/-defp-/ ; finish'
cc      call dotaskx3d(cbuf,ierr)
cc      if(ierr.ne.0)call x3d_error(isubname, 'hextotet')
cc
ccC     write hex cmo
cc      ierr=0
cc      cbuf='dump/avs/degenhex.inp/-defh-/ ; finish '
cc      call dotaskx3d(cbuf,ierr)
cc      if(ierr.ne.0)call x3d_error(isubname, 'dump prism.inp')
cc
ccC     set interpolation for imt
cc      cbuf='cmo/modatt/-defh-/imt1/interpolation/min/ ; finish'
cc      call dotaskx3d(cbuf,ierr)
cc     if(ierr.ne.0)call x3d_error(isubname, 'modchk hextotet')
cc
ccc     set model
cc       cbuf='modbld/-def-/hextotet/inital/rmpoint=no,rmvolume=no/'
cc    >       // ' ; modchk ; finish'
cc       call dotaskx3d(cbuf,ierr)
cc       if(ierr.ne.0)call x3d_error(isubname, 'modchk hextotet')
cc     else
cc       cmoh = -defp-
      endif
c-------------------------------------------------------------------
 
C-----convert prism -defp- to tet cmoout
      ierr=0
      write(cbuf,91) ntet, cmoout(1:icharlnf(cmoout))
 91   format('hextotet/',i10,'/',a32,'/-defp-/ ; finish')
      call dotaskx3d(cbuf,ierr)
      if(ierr.ne.0)call x3d_error(isubname, 'hextotet')
 
      call cmo_exist('-defp-',ierr)
      if (ierr.eq.0) call cmo_release('-defp-',ierr)
      call cmo_exist('-defh-',ierr)
      if (ierr.eq.0) call cmo_release('-defh-',ierr)
      call cmo_select(cmoout,ierr)
 
 
999   return
      end
 
       subroutine quadlayertotet(imsgin,xmsgin,cmsgin,msgtyp,nwds,ierr)
C
C#####################################################################
C
C
C      PURPOSE -
C
C       Input is a quadlayer cmo created with multiple quad
C       files and assumes that the surfaces
C       have identical connectivity. Use mread/trilayers command.
C       This routine calls pgg and hextotet with appropriate options.
C       By default hextotet is called with 6 for prism elements.
C
C      SYNTAX:
C
C      QUADLAYERTOTET/
C      layers n | nodes n / cmoout/cmoin/ ntet / usrclr | stratclr [hex]
C
C
C        layers    - are the number of TIN surfaces merged into
C                    the trilayer cmo
C        nodes     - alternate to nlayers,  n is the number of nodes
C                    in each trilayer surface
C        cmoout    - the output tet cmo
C        cmoin     - the input trilayer cmo
C        ntet      - number of tets to break each prism into
C                    3 and 18 are reccommended, although the regular
C                    hex options of 6 and 24 can be called. Default
C                    is 3 tets from 1 prism.
C                    Prisms are formed into degenerate hexes which
C                    can cause degenerate tets for ntet = 6 or 24
C                    then volumes will not be
C                    removed during the hextotet phase as this will
C                    result in holes in the mesh
C        usrclr    - keeps color of elements vertically same as input cmo
c                    (default)
C        stratclr  - colors by layer order
C        hex       - writes avs file "hex.inp"
C
C      EXAMPLES:
C
C      quadlayertotet/ nodes 357 /cmotet/cmo1/
C      quadlayertotet/ layers 10 /cmotet/cmo1/
C
C      INPUT ARGUMENTS -
C         imsgin()  - Integer array of command input tokens
C         xmsgin()  - Real array of command input tokens
C         cmsgin()  - Character array of command input tokens
C         msgtyp() - Integer array of command input token types
C         nwds      - Number of command input tokens
C
C      OUTPUT ARGUMENTS -
C
C
C      CHANGE HISTORY -
C
C      Original version - T.Cherry - 2000
C
C#####################################################################
C
      implicit none
C####################################################################
C
C     DEFINE THE MESH_OBJECT POINTERS.
C
C
      integer nwds
      real*8 xmsgin(nwds)
      integer imsgin(nwds),msgtyp(nwds)
      character*32 cmsgin(nwds)
C
      integer nlayers, nodpersur, ntet, ioptdegen, stratclr
      integer iwrthex
      integer i,ii,ierr
      integer icharlnf
      character*32 isubname, cmoout, cmoin, cmoh
      character*1024  cbuf
C
C BEGIN
      isubname='quadlayertotet'
      ierr = 0
      ntet = 6
      nodpersur = 0
      nlayers = 0
      ioptdegen = 0
      stratclr = 0
      iwrthex = 0
      cmoh = '-defh-'
      cmoin = '-notset-'
      cmoout = '-notset-'
C
c
C     READ PARSER VALUES, i is next value, nwds is last
      i = 2
      if (msgtyp(2).eq.3 ) then
        if (cmsgin(2)(1:3).eq.'nod' .or.
     >      cmsgin(2)(2:4).eq.'nod') then
           nodpersur = imsgin(3)
           i= i+2
        elseif (cmsgin(2)(1:3).eq.'lay' .or.
     >     cmsgin(2)(2:4).eq.'lay') then
           nlayers = imsgin(3)
           i= i+2
         else
           nlayers = imsgin(2)
           i= i+1
         endif
      endif
 
      cmoout = cmsgin(i)
      i= i+1
      cmoin = cmsgin(i)
      i= i+1
 
      do ii = i, nwds
      if(msgtyp(ii).eq.1) ntet = imsgin(ii)
      if(msgtyp(ii).eq.3 .and. cmsgin(ii)(1:5).eq.'strat') stratclr = 1
      if(msgtyp(ii).eq.3 .and. cmsgin(ii)(1:6).eq.'hex') iwrthex = 1
      enddo
 
      if (ntet.ne.6 .or. ntet.ne.24) then
        write(cbuf,'(a)')'hextotet 6 or 24 is needed for hex grid.'
        call writloga('default',1,cbuf,1,ierr)
      endif
      if (iwrthex.eq.1) then
        write(cbuf,'(a)')'hex.inp will be written before hextotet'
        call writloga('default',1,cbuf,1,ierr)
      endif
 
      call cmo_set_name(cmoin,ierr)
      if(ierr.ne.0) call x3d_error(isubname,'cmo_set_name')
      if(ierr.ne.0) goto 999
 
 
C     convert quadlayers cmoin to hexcmo -cmoh-
      ierr=0
      if (stratclr.gt.0) then
        if (nodpersur .gt. 0) then
         write(cbuf,40)  cmoin, nodpersur
 40      format('pgg/-defh-/',a32,'/ nodes ',i10,'/strat/hex ; finish')
        else
         write(cbuf,41)  cmoin, nlayers
 41      format('pgg/-defh-/',a32,'/layers',i10,'/strat/hex ; finish')
        endif
 
      else
        if (nodpersur .gt. 0) then
        write(cbuf,50)  cmoin, nodpersur
 50     format('pgg/-defh-/',a32,'/ nodes',i10,'/usrclr/hex ; finish')
        else
        write(cbuf,51)  cmoin, nlayers
 51     format('pgg/-defh-/',a32,'/layers',i10,'/usrclr/hex ; finish')
        endif
      endif
 
      call dotaskx3d(cbuf,ierr)
      if(ierr.ne.0)call x3d_error(isubname, 'to hex')
 
C     write hex cmo
      if (iwrthex.ne.0) then
        ierr=0
        cbuf='dump/avs/hex.inp/-defh-/ ; finish '
        call dotaskx3d(cbuf,ierr)
        if(ierr.ne.0)call x3d_error(isubname, 'dump hex.inp')
      endif
 
C     convert hex cmo -defh- to tet cmoout
      ierr=0
      write(cbuf,91) ntet, cmoout(1:icharlnf(cmoout))
 91   format('hextotet/',i10,'/',a32,'/-defh-/ ; finish')
      call dotaskx3d(cbuf,ierr)
      if(ierr.ne.0)call x3d_error(isubname, 'hextotet')
 
 
      call cmo_exist('-defh-',ierr)
      if (ierr.eq.0) call cmo_release('-defh-',ierr)
      call cmo_select(cmoout,ierr)
 
 
999   return
      end
 
C
      subroutine squeeze(imsgin,xmsgin,cmsgin,msgtyp,nwds,ierr)
C
C#####################################################################
C
C      PURPOSE -
C       Given a tritoprism cmo, squeeze elements to outside boundary
C       WARNING: can get elements turned inside out at bottom edges
C       seems to work best at sides and top
C
C
C     INPUT ARGUMENTS -
C       x y and/or z
C       xvalue for each  or cmoname of surface for each
C       lt or gt
C
C
C     OUTPUT ARGUMENTS -
C
C
C      CHANGE HISTORY -
C        initial version by terry cherry, minimal error checking
C
C
C
C#####################################################################
C
c      implicit real*8 (a-h, o-z)
      implicit none
C
C
C####################################################################
C
C     DEFINE THE MESH_OBJECT POINTERS.
C
      include "chydro.h"
      include "local_element.h"
C
      integer nwds
      real*8 xmsgin(nwds)
      integer imsgin(nwds),msgtyp(nwds)
      character*32 cmsgin(nwds)
C
      integer nnodes, numtet, mbndry
      integer ip, npoints
      integer i, inxt
      integer ilen,ityp,ierr,ics
      integer ioptz, ioptx, iopty
      integer ioptzsrf, ioptxsrf, ioptysrf
      integer ioptless, ioptmore
      integer xtotal, ytotal, ztotal, ntotal
      integer icharlnf
      real*8  xvalue, yvalue, zvalue
      character*32 isubname, cmo, cmonam
      character*32 cmoxsrf, cmoysrf, cmozsrf
      character*132 logmess
      character*4096 cbuf
C
      pointer (ipxic, xic)
      pointer (ipyic, yic)
      pointer (ipzic, zic)
      real*8 xic(10000000), yic(10000000), zic(10000000)
C
      pointer (ipxici, xici)
      pointer (ipyici, yici)
      pointer (ipzici, zici)
      real*8 xici(10000000), yici(10000000), zici(10000000)
C
      data ntotal /0/
C
C BEGIN
 
      isubname='squeeze'
      cmo='notset'
      cmoxsrf='notset'
      cmoysrf='notset'
      cmozsrf='notset'
      ioptx = 0
      iopty = 0
      ioptz = 0
      ioptxsrf = 0
      ioptysrf = 0
      ioptzsrf = 0
      ioptless = 0
      ioptmore = 0
      xtotal = 0
      ytotal = 0
      ztotal = 0
      call cmo_get_name(cmonam,ierr)
C
      call cmo_get_info('nnodes',cmonam,nnodes,ilen,ityp,ierr)
      call cmo_get_info('nelements',cmonam,numtet,ilen,ityp,ierr)
      call cmo_get_info('mbndry',cmonam,mbndry,ilen,ityp,ierr)
C
      call cmo_get_info('xic',cmonam,ipxic,ilen,ityp,ierr)
      call cmo_get_info('yic',cmonam,ipyic,ilen,ityp,ierr)
      call cmo_get_info('zic',cmonam,ipzic,ilen,ityp,ierr)
 
      if(ierr.ne.0) call x3d_error(isubname,'cmo_get_info')
      if(ierr.ne.0) goto 9999
 
C MESSEGE PROCESSING
      inxt=2
 
c     squeeze axis, x,y, and/or z
      do i = 2,4
        if(cmsgin(i)(1:1).eq.'x') then
          ioptx = 1
          inxt=inxt+1
        endif
        if(cmsgin(i)(1:1).eq.'y') then
          iopty = 1
          inxt=inxt+1
        endif
        if(cmsgin(i)(1:1).eq.'z') then
          ioptz = 1
          inxt=inxt+1
        endif
      enddo
 
c     squeeze source, one for each x,y, or/and z
c     can be a constant value or cmo surface
      if(ioptx.ne.0) then
        if(msgtyp(inxt).eq.2) then
          xvalue=xmsgin(inxt)
        elseif (msgtyp(inxt).eq.3) then
          cmoxsrf = cmsgin(inxt)
          ioptxsrf = 1
        else
          goto 9999
        endif
        inxt=inxt+1
      endif
      if(iopty.ne.0) then
        if(msgtyp(inxt).eq.2) then
          yvalue=xmsgin(inxt)
        elseif (msgtyp(inxt).eq.3) then
          cmoysrf = cmsgin(inxt)
          ioptysrf = 1
        else
          goto 9999
        endif
        inxt=inxt+1
      endif
      if(ioptz.ne.0) then
        if(msgtyp(inxt).eq.2) then
          zvalue=xmsgin(inxt)
        elseif (msgtyp(inxt).eq.3) then
          cmozsrf = cmsgin(inxt)
          ioptzsrf = 1
        else
          goto 9999
        endif
        inxt=inxt+1
      endif
 
c     squeeze values lt or gt constant or surface
      if(cmsgin(inxt)(1:2).eq.'lt') ioptless = 1
      if(cmsgin(inxt)(1:2).eq.'gt') ioptmore = 1
 
C Squeeze along appropriate axis
 
C Constant value--------------
C SQUEEZE Z
      if (ioptz.ne.0 .and. ioptzsrf.ne.1 )then
      if (ioptless.ne.0) then
      do i = 1,nnodes
         if (zic(i).lt.zvalue) then
           zic(i)=zvalue
           ztotal=ztotal+1
           ntotal=ntotal+1
         endif
      enddo
      endif
      if (ioptmore.ne.0) then
      do i = 1,nnodes
         if (zic(i).gt.zvalue) then
           zic(i)=zvalue
           ztotal=ztotal+1
           ntotal=ntotal+1
         endif
      enddo
      endif
      endif
 
C SQUEEZE X
      if (ioptx.ne.0 .and. ioptxsrf.ne.1 ) then
      if (ioptless.ne.0) then
      do i = 1,nnodes
         if (xic(i).lt.xvalue) then
           xic(i)=xvalue
           xtotal=xtotal+1
           ntotal=ntotal+1
         endif
      enddo
      endif
      if (ioptmore.ne.0) then
      do i = 1,nnodes
         if (xic(i).gt.xvalue) then
           xic(i)=xvalue
           xtotal=xtotal+1
           ntotal=ntotal+1
         endif
      enddo
      endif
      endif
 
C SQUEEZE Y
      if (iopty.ne.0 .and. ioptysrf.ne.1 ) then
      if (ioptless.ne.0) then
      do i = 1,nnodes
         if (yic(i).lt.yvalue) then
           yic(i)=yvalue
           ytotal=ytotal+1
           ntotal=ntotal+1
         endif
      enddo
      endif
      if (ioptmore.ne.0) then
      do i = 1,nnodes
         if (yic(i).gt.yvalue) then
           yic(i)=yvalue
           ytotal=ytotal+1
           ntotal=ntotal+1
         endif
      enddo
      endif
      endif
 
C Squeeze by surface-----------
C     Z Squeeze
      if (ioptz.ne.0 .and. ioptzsrf.ne.0) then
        cbuf=
     >  'intersect/cmopt/' //
     >   cmonam(1:icharlnf(cmonam)) //
     >   '/' // cmozsrf(1:icharlnf(cmozsrf)) //
     >   '/ ; finish'
        call dotaskx3d(cbuf,ierr)
        if(ierr.ne.0)call x3d_error(isubname, 'intersection')
 
        cmo='cmopt'
        call cmo_get_info('nnodes',cmo,npoints,ilen,ityp,ierr)
        if (npoints.le.0) goto 9999
        call cmo_get_info('xic',cmo,ipxici,ilen,ityp,ierr)
        call cmo_get_info('yic',cmo,ipyici,ilen,ityp,ierr)
        call cmo_get_info('zic',cmo,ipzici,ilen,ityp,ierr)
        write(logmess,9010) npoints
 9010   format('Total points of intersection: ',i10)
        call writloga('default',0,logmess,0,ics)
        if(npoints.le.0) goto 9999
 
C       Find and points and replace z component
        do ip = 1, npoints
 
          do i = 1, nnodes
            if (xici(ip).eq.xic(i)) then
              if (yici(ip).eq.yic(i)) then
                zic(i) = zici(ip)
                ztotal = ztotal+1
                ntotal=ntotal+1
              endif
            endif
          enddo
 
c       done sweeping through intersect pts
        enddo
c       Make cmonam current again
        cbuf='cmo/select/' // cmonam(1:icharlnf(cmonam)) // ' ; finish'
        call dotaskx3d(cbuf,ics)
 
C     Done squeeze by surface
      endif
 
9999  if (ioptx.ne.0) then
         write(logmess,'(i10,a,e14.6)')
     *   xtotal,' X Squeezed to ',xvalue
         call writloga('default',0,logmess,0,ierr)
      endif
      if (iopty.ne.0) then
         write(logmess,'(i10,a,e14.6)')
     *   ytotal,' Y Squeezed to ',yvalue
         call writloga('default',0,logmess,0,ierr)
      endif
      if (ioptz.ne.0) then
         write(logmess,'(i10,a,e14.6)')
     *   ztotal,' Z Squeezed to ',zvalue
         call writloga('default',0,logmess,0,ierr)
      endif
      if (ioptz.ne.0 .or. iopty.ne.0 .or. ioptz.ne.0) then
         write(logmess,'(i10,a)')
     *   ntotal,' Points Squeezed for cmo: ' // cmonam
         call writloga('default',0,logmess,0,ierr)
      endif
c
      return
      end
C
      subroutine fix_orientation(
     >           imsgin,xmsgin,cmsgin,msgtyp,nwds,ier)
C
C#####################################################################
C
C     PURPOSE -
C       Orient all elements so numbering starts at miny,minx = SW
C
C     INPUT ARGUMENTS -
C
C
C     OUTPUT ARGUMENTS -
C
C
C#####################################################################
C
C
      implicit NONE
C
C ARGS
      integer nwds
      real*8 xmsgin(nwds)
      integer imsgin(nwds),msgtyp(nwds)
      character*32 cmsgin(nwds)
C LOCAL
      integer i, it, in, ics
      integer i1, inxt, ifirst, imax
      integer itet_new(12)
      real*8 minx,miny,minz,mindist,dista,distb
      character*32 isubname, cmonam
      character*132 logmess
C CMO
      pointer (ipxic, xic)
      pointer (ipyic, yic)
      pointer (ipzic, zic)
      pointer (ipimt1, imt1)
c     pointer (ipitetclr, itetclr)
      pointer (ipitettyp, itettyp)
      pointer (ipitetoff, itetoff)
      pointer (ipitet1, itet1)
      integer imt1(1000000)
      integer itet1(10000000)
c     integer itetclr(1000000)
      integer itettyp(1000000), itetoff(1000000)
      integer ilen, itype, ier, lenimt1, lenitetoff
      integer lenitet1, lenitettyp
      integer istart, iend, iinc
      integer nnodes, numtet, mbndry
      real*8  xic(1000000), yic(1000000), zic(1000000)
C
      include "local_element.h"
C
C
      isubname='fix_orientation'
      call cmo_get_name(cmonam,ier)
C
C READ PARSER VALUES
      if (nwds .eq. 1)then
          iinc   = 1
          istart = 1
          iend   = 0
 
c      use usr input
       elseif (nwds .eq. 3)then
         istart = imsgin(2)
         iend   = imsgin(3)
 
       elseif (nwds .eq. 4)then
         istart = imsgin(2)
         iend   = imsgin(3)
         iinc   = imsgin(4)
       else
         goto 9999
      endif
 
 
C GET OBJECT MEMORY and INFO
C
      call cmo_get_info('nnodes',cmonam,nnodes,ilen,itype,ier)
      call cmo_get_info('nelements',cmonam,numtet,ilen,itype,ier)
      call cmo_get_info('mbndry',cmonam,mbndry,ilen,itype,ier)
C
      call cmo_get_info('xic',cmonam,ipxic,ilen,itype,ier)
      call cmo_get_info('yic',cmonam,ipyic,ilen,itype,ier)
      call cmo_get_info('zic',cmonam,ipzic,ilen,itype,ier)
      call cmo_get_info('imt1',cmonam,ipimt1,lenimt1,itype,ier)
      call cmo_get_info('itet',cmonam,ipitet1,lenitet1,itype,ier)
      call cmo_get_info('itettyp',cmonam,ipitettyp,lenitettyp,itype,ier)
      call cmo_get_info('itetoff',cmonam,ipitetoff,lenitetoff,itype,ier)
 
C
C FOR EACH ELEMENT, REORIENT
      do it = 1, numtet
 
c       find bottom front left corner, 1st in connectivity
        minx=xic(itet1(itetoff(it)+1))
        miny=yic(itet1(itetoff(it)+1))
        minz=zic(itet1(itetoff(it)+1))
 
c       lopp through nodes of bottom face
c       in prisms, this is the first 3 nodes in tet1
c       find min(x,y,z)
c       then find point closest to min(x,y,z)
        imax=3
        do i = 2, imax
           i1=itet1(itetoff(it)+i)
           if (xic(i1) .lt. minx ) minx=xic(i1)
           if (yic(i1) .lt. miny ) miny=yic(i1)
           if (zic(i1) .lt. minz ) minz=zic(i1)
        enddo
 
        do i = 1,imax
           i1=itet1(itetoff(it)+i)
           distb=(minx-xic(i1))*(minx-xic(i1))+
     *           (miny-yic(i1))*(miny-yic(i1))+
     *           (minz-zic(i1))*(minz-zic(i1))
           dista=sqrt(distb)
           if(i.eq.1) then
             mindist=dista
             ifirst=1
           elseif(dista.lt.mindist) then
             mindist=dista
             ifirst=i
           endif
        enddo
 
c       renumber into temp array
        imax=3
        in=ifirst
        do i = 1, imax
           inxt = mod(in,imax)
           if(inxt.eq.0) inxt=imax
           i1=itetoff(it)+inxt
           itet_new(i) = itet1(i1)
           itet_new(i+3) = itet1(i1+3)
           in = in+1
cdebug  print*,'itet_new(',i,')= ','itet(',i1,')= ',itet1(i1)
cdebug  print*,'itet_new(',i+3,')= ','itet(',i1+3,')= ',itet1(i1+3)
        enddo
 
c
c       copy new into old
        do i = 1, nelmnen(itettyp(it))
           i1=itetoff(it)+i
           itet1(i1) = itet_new(i)
cdebug  print*,'itet(',i1,')= ',itet_new(i)
        enddo
 
      enddo
C     END REORIENTATION
c
c
9999  write(logmess,'(i12,a)') numtet,' total elements reset.'
      call writloga('default',0,logmess,0,ics)
 
      return
      end
 
C
      subroutine assign_element_volume(
     >           imsgin,xmsgin,cmsgin,msgtyp,nwds,ier)
C
C#####################################################################
C
C     PURPOSE -
C          Assign vornoi volume to node attribute vor_volume
C
C     INPUT ARGUMENTS -
C
C
C     OUTPUT ARGUMENTS -
C
C
C#####################################################################
C
C
      implicit NONE
C
      include "local_element.h"
      include "chydro.h"
C
      integer nwds, ier
      integer imsgin(*), msgtyp(*)
      character*32 cmsgin(nwds)
      real*8 xmsgin(*)
 
c     pointer (ipicr1, icr1)
c     pointer (ipitp1, itp1)
c     integer icr1(10000000), itp1(1000000)
      pointer (ipxic, xic )
      real*8 xic(1000000)
      pointer (ipyic, yic )
      real*8 yic(1000000)
      pointer (ipzic, zic )
      real*8 zic(1000000)
C
c     pointer (ipitetclr, itetclr )
c     integer itetclr(1000000)
      pointer (ipitettyp, itettyp )
      integer itettyp(1000000)
      pointer (ipitetoff, itetoff )
      integer itetoff(1000000)
      pointer (ipjtetoff, jtetoff )
      integer jtetoff(1000000)
      pointer (ipitet, itet1 )
      integer itet1(1000000)
      pointer (ipjtet, jtet1 )
      integer jtet1(1000000)
C
      pointer (ipvor_volume, vor_volume )
      real*8 vor_volume(1000000)
C
c     integer i,i1,it,i2,i3,ioff,joff
      integer nnodes, nelements, mbndry
      integer ilen, itype
c     integer id
      integer ierror, ierr, ics
      integer icharlnf
 
      real*8 vol_test
c
      character*32  cmo
      character*72  isubname
      character*1024  cbuf
C
C
C BEGIN
      isubname='assign_color_volume'
 
      if(nwds .eq. 1)then
         vol_test = 0.0d0
      elseif(nwds .eq. 2)then
         vol_test = xmsgin(2)
      endif
      write(6,*)'Test volume of elements in ' , isubname
      write(6,*)'Report volume of elements .le. ' , vol_test
 
      call cmo_get_name(cmo,ierror)
 
      call cmo_get_info('nnodes',cmo,nnodes,ilen,itype,ics)
         if(ics.ne.0) call x3d_error(isubname,'cmo_get_info')
      call cmo_get_info('nelements',cmo,nelements,ilen,itype,ics)
         if(ics.ne.0) call x3d_error(isubname,'cmo_get_info')
      call cmo_get_info('mbndry',cmo,mbndry,ilen,itype,ics)
         if(ics.ne.0) call x3d_error(isubname,'cmo_get_info')
      call cmo_get_info('xic',cmo,ipxic,ilen,itype,ics)
         if(ics.ne.0) call x3d_error(isubname,'cmo_get_info')
      call cmo_get_info('yic',cmo,ipyic,ilen,itype,ics)
         if(ics.ne.0) call x3d_error(isubname,'cmo_get_info')
      call cmo_get_info('zic',cmo,ipzic,ilen,itype,ics)
         if(ics.ne.0) call x3d_error(isubname,'cmo_get_info')
      call cmo_get_info('itettyp',cmo,ipitettyp,ilen,itype,ics)
         if(ics.ne.0) call x3d_error(isubname,'cmo_get_info')
      call cmo_get_info('itetoff',cmo,ipitetoff,ilen,itype,ics)
         if(ics.ne.0) call x3d_error(isubname,'cmo_get_info')
      call cmo_get_info('jtetoff',cmo,ipjtetoff,ilen,itype,ics)
         if(ics.ne.0) call x3d_error(isubname,'cmo_get_info')
      call cmo_get_info('itet',cmo,ipitet,ilen,itype,ics)
         if(ics.ne.0) call x3d_error(isubname,'cmo_get_info')
      call cmo_get_info('jtet',cmo,ipjtet,ilen,itype,ics)
         if(ics.ne.0) call x3d_error(isubname,'cmo_get_info')
 
C    Add an attribute called vor_volume to cmo
C
      cbuf = 'cmo/addatt/' //
     >                     cmo(1:icharlnf(cmo)) //
     >                     '/' //
     >        'vor_volume' //
     >        '/vdouble/scalar/nnodes/linear/permanent/afgx/0.0/' //
     >        ' ; finish '
      call dotaskx3d(cbuf,ierr)
c
c     Get the pointer to the newly created array
c
      call cmo_get_info('vor_volume',cmo,ipvor_volume,ilen,itype,ics)
      if (ics.ne.0) call x3d_error(isubname,'cmo_get_info')
 
c
c
 
      return
      end
 
C
C####################################################################
      subroutine reverse(ierr)
C
c     reverse order of nodes (for testing)
 
      implicit none
C
C
C####################################################################
C
      include "chydro.h"
      include "local_element.h"
C
      integer nnodes
      integer i, j
      integer ilen,ityp,ierr,ics
c     integer icharlnf
      real*8  xvalue, yvalue, zvalue
      character*32 isubname, cmo
c     character*132 logmess
c     character*240 cbuf
C
      pointer (ipxic, xic)
      pointer (ipyic, yic)
      pointer (ipzic, zic)
      real*8 xic(10000000), yic(10000000), zic(10000000)
C
C
C BEGIN
 
      isubname='reverse'
      cmo='notset'
c
      call cmo_get_name(cmo,ics)
      call cmo_get_info('nnodes',cmo,nnodes,ilen,ityp,ierr)
      call cmo_get_info('xic',cmo,ipxic,ilen,ityp,ierr)
      call cmo_get_info('yic',cmo,ipyic,ilen,ityp,ierr)
      call cmo_get_info('zic',cmo,ipzic,ilen,ityp,ierr)
c
      xvalue = xic(1)
      yvalue = yic(1)
      zvalue = zic(1)
      j=nnodes
      do i=1,nnodes
        xic(i) = xic(j)
        yic(i) = yic(j)
        zic(i) = zic(j)
        j=j-1
      enddo
      xic(nnodes) = xvalue
      yic(nnodes) = yvalue
      zic(nnodes) = zvalue
 
cdebug print*,'Reversed nodes: ',nnodes
 
      return
      end
 
C=============================================================================
C file beads_ona_ring.f
C does not contain any lagrit calls but is used
C in geo routines such as read_trilayers
 
 
      subroutine beads_ona_ring_2d_sp (errmsg,
     >  z_out2d,z_in2d,d_pinch,d_min,id_move,npoint,nvec,iorder,ierr)
      implicit none
C     Passed in variables
      integer npoint, nvec, iorder, ierr
      character*72 errmsg
      real*4 z_in2d(npoint*nvec)
      real*4 z_out2d(npoint*nvec)
      real*4 d_pinch(npoint-1)
      real*4 d_min(npoint-1)
      integer id_move(npoint-1)
c
c     Declare malloc as integer function
      integer malloc
c
c     Local variables
      real*8 z_in, z_out
      pointer (ipz_in,  z_in(1000000))
      pointer (ipz_out, z_out(1000000))
      real*4 d_pinch_wk, d_min_wk
      pointer (ipd_pinch_wk, d_pinch_wk(1000000))
      pointer (ipd_min_wk,   d_min_wk(1000000))
 
      integer i, n, nbyte_r8, nbyte_r4
C
C     This version of the code assumes the reals that are passed in
C     z_out2d,z_in2d,d_pinch,d_min are single precision real*4
C
C     This is a driver routine for the beads_ona_ring program. It
C     is set up to take a 2d array as input and process the rows
C     through the beads_ona_ring subroutine. While
C     the calling program may treat the input as a 2d array a(i,j)
C     this driver program treats the input and output array as a
C     one dimensional array. To make this general the iorder parameter
C     is used to inform the driver if the input array to be processed
C     is row ordered a(i,j), a(i+1,j), a(i+2,j) ...   iorder = 1
C     column ordered a(i,j), a(i,j+1), a(i,j+2) ...   iorder = 2
C
C     For the above ordering to work correctly the input array must
C     have the same dimensions as npoint and nvec.
C
C     If the user wishes to overwrite the input array with the filtered
C     output array, the first two arguments of beads_ona_ring_2d can
C     be the same array (i.e. call beads_ona_ring_2d(a,a,...)).
C
C
c
c     Allocate Memory
c
      errmsg = '-undefined error-'
      nbyte_r8 = 8*npoint
      nbyte_r4 = 8*(npoint-1)
      ipz_in  = malloc(nbyte_r8)
      ipz_out = malloc(nbyte_r8)
      ipd_pinch_wk = malloc(nbyte_r4)
      ipd_min_wk = malloc(nbyte_r4)
c
      do i = 1, npoint-1
         d_pinch_wk(i) = 0.0d0
         d_pinch_wk(i) = d_pinch(i)
         d_min_wk(i)   = 0.0d0
         d_min_wk(i)   = d_min(i)
      enddo
 
      do n = 1,nvec
         if(iorder .eq. 1)then
            do i = 1, npoint
               z_in(i) = z_in2d(i+npoint*(n - 1))
            enddo
         elseif(iorder .eq. 2)then
            do i = 1, npoint
               z_in(i) = z_in2d(n+(i*nvec))
            enddo
         endif
c
         call beads_ona_ring
     1      (errmsg,z_out,z_in,d_pinch,d_min,id_move,npoint,ierr)
c
         if(iorder .eq. 1)then
            do i = 1, npoint
               z_out2d(i+npoint*(n - 1)) = z_out(i)
            enddo
         elseif(iorder .eq. 2)then
            do i = 1, npoint
               z_out2d(n+(i*nvec)) = z_out(i)
            enddo
         endif
      enddo
 
      call free(ipz_in)
      call free(ipz_out)
      return
      end
 
      subroutine beads_ona_ring_2d (errmsg,
     >  z_out2d,z_in2d,d_pinch,d_min,id_move,npoint,nvec,iorder,ierr)
      implicit none
C     Passed in variables
      character*72 errmsg
      integer npoint, nvec, iorder, ierr
      real*8 z_in2d(npoint*nvec)
      real*8 z_out2d(npoint*nvec)
      real*8 d_pinch(npoint-1)
      real*8 d_min(npoint-1)
      integer id_move(npoint-1)
c
c     Declare malloc as integer function
      integer malloc
c
c     Local variables
      real*8 z_in, z_out
      pointer (ipz_in,  z_in(1000000))
      pointer (ipz_out, z_out(1000000))
 
      integer i, n, nbyte_r
C
C
C     This version of the code assumes the reals that are passed in
C     z_out2d,z_in2d,d_pinch,d_min are single precision real*8
C
C     This is a driver routine for the beads_ona_ring program. It
C     is set up to take a 2d array as input and process the rows
C     through the beads_ona_ring subroutine. While
C     the calling program may treat the input as a 2d array a(i,j)
C     this driver program treats the input and output array as a
C     one dimensional array. To make this general the iorder parameter
C     is used to inform the driver if the input array to be processed
C     is row ordered a(i,j), a(i+1,j), a(i+2,j) ...   iorder = 1
C     column ordered a(i,j), a(i,j+1), a(i,j+2) ...   iorder = 2
C
C     For the above ordering to work correctly the input array must
C     have the same dimensions as npoint and nvec.
C
C     If the user wishes to overwrite the input array with the filtered
C     output array, the first two arguments of beads_ona_ring_2d can
C     be the same array (i.e. call beads_ona_ring_2d(a,a,...)).
C
C
c
c     Allocate Memory
c
      errmsg = '-undefined error-'
      nbyte_r = 8*npoint
      ipz_in  = malloc(nbyte_r)
      ipz_out = malloc(nbyte_r)
c
      do n = 1,nvec
         if(iorder .eq. 1)then
            do i = 1, npoint
               z_in(i) = z_in2d(i+npoint*(n - 1))
            enddo
         elseif(iorder .eq. 2)then
            do i = 1, npoint
               z_in(i) = z_in2d(n+(i*nvec))
            enddo
         endif
c
         call beads_ona_ring (errmsg,
     1       z_out,z_in,d_pinch,d_min,id_move,npoint,ierr)
c
         if(iorder .eq. 1)then
            do i = 1, npoint
               z_out2d(i+npoint*(n - 1)) = z_out(i)
            enddo
         elseif(iorder .eq. 2)then
            do i = 1, npoint
               z_out2d(n+(i*nvec)) = z_out(i)
            enddo
         endif
      enddo
 
      call free(ipz_in)
      call free(ipz_out)
      return
      end
 
 
      subroutine beads_ona_ring
     1   (errmsg,zout,z,d_pinch,d_min,id_move,npoint,ierr)
      implicit none
c     Passed in variables
      character*72 errmsg
      integer npoint, ierr
      real*8 z(npoint)
      real*8 zout(npoint)
      real*8 d_pinch(npoint-1)
      real*8 d_min(npoint-1)
      integer id_move(npoint-1)
c
c     Declare malloc as integer function
      integer malloc
c
c     Local variables
      real*8 d, d_change, d_up, d_dn
      integer id_status, ipt_up, ipt_dn
      pointer (ipd, d(1000000))
      pointer (ipd_change, d_change(1000000))
      pointer (ipd_up,     d_up(1000000))
      pointer (ipd_dn,     d_dn(1000000))
      pointer (ipid_status, id_status(1000000))
      pointer (ipipt_up,    ipt_up(1000000))
      pointer (ipipt_dn,    ipt_dn(1000000))
 
      integer i, itrue, ifalse, npoint_last_call, icall
      integer ndist, nbyte_r, nbyte_i
      real*8  d_total, z_start, z_end, d_min_sum
      data icall / 0 /
 
C     NAME: beads_ona_ring
C           (errmsg,zout,z,d_pinch,d_min,id_move,npoint,ierr)
C
C     errmsg         Is a character string that will contain any possible
C                    error messeges.
C     zout           output array with new elevation values
C     z              input array of zic values, ordered from bottom to top
C                    must be monotonic increasing
c     d_pinch        If interval length d <= d_pinch then set to zero
c     d_min          If interval length is d_pinch < d < d_min set to d_min
c     id_move   = 1  Get or put values equally up and down
c                 2  Get or put values up only
c                 3  Get or put values down only
c     id_status =  0  Interval has been set to zero or d_min, don't change.
c               =  1  Candidate for setting to zero
c               = -1  Interval has been set to zero but redistribution has
c                        has not been done.
c               =  2  Candidate for setting to d_min
c               = -2  Interval has been set to d_min but redistribution has
c                        has not been done.
c               =  3  Interval length is > d_min
C     npoint          number of elevations in arrays
C     ierr            returns 0 if no errors
c
c     begin main code for beads_ona_ring
      icall = icall + 1
      errmsg = '-undefined error-'
      ierr = 0
 
      npoint_last_call = npoint
      ndist = npoint - 1
c
c     Allocate Memory
c
      nbyte_r = 8*ndist
      nbyte_i = 4*ndist
      ipd =        malloc(nbyte_r)
      ipd_change = malloc(nbyte_r)
      ipd_up =     malloc(nbyte_r)
      ipd_dn =     malloc(nbyte_r)
      ipid_status= malloc(nbyte_i)
      ipipt_up =   malloc(nbyte_i)
      ipipt_dn =   malloc(nbyte_i)
c
c     Initialize some vectors
c
      z_start = z(1)
      call set_pointers(ipt_up,ipt_dn,ndist)
      do i = 1,ndist
         d_change(i) = 0.0d0
         id_status(i) = 3
      enddo
 
C     Do some error checking on the points.
c
c     Check that input vector is monotonic increasing.
c     It may happen that a layer dips below another and needs
c     to be truncated before points are sent to this routine.
      call monotonic(ifalse,z,npoint)
      if(ifalse .ne. 0) then
        write(errmsg,'(a)')'Error 10: Input not monotonic increasing.'
        call z_to_d(d,z,npoint)
        call d_to_z(zout,d,z_start,npoint)
        ierr = 10
        goto 9991
      endif
c
c     Check that d_pinch input is < d_min input for all intervals
      call check_input(itrue,d_pinch,d_min,ndist)
      if(itrue .ne. 0) then
        write(errmsg,'(a)')'Error 20: d_pinch >= d_min'
        ierr = 20
        call z_to_d(d,z,npoint)
        call d_to_z(zout,d,z_start,npoint)
        goto 9991
      endif
c
c     Check that sum(d_min) < z(npoint) - z(1)
      d_total = z(npoint) - z(1)
      z_start = z(1)
      z_end   = z(npoint)
      call check_d_min(itrue,d_min_sum,d_total,d_min,ndist)
      if(itrue .ne. 0) then
cdebug    write(6,*)
cdebug*   'Error 30: sum(d_min)>d_total ',itrue, d_min_sum, d_total
cdebug    write(6,*)
cdebug*   'Error: sum(d_min) should be a small percentage of d_total'
          write(errmsg,'(a)')'Error 30: sum(d_min) > z(npoint) - z(1)'
          ierr = 30
          call z_to_d(d,z,npoint)
          call d_to_z(zout,d,z_start,npoint)
          goto 9991
      endif
 
 9991 if (ierr .ne. 0 ) then
cdebug    write(6,*)'Error: No Action Possible - Vector not processed'
cdebug    write(6,*)'       Output = Input'
          goto 9999
      endif
 
 
C     if we get this far, the arrays are set up as expected
      ierr = 0
c
c     Calculate distance vector from coordinate vector.
c
      call z_to_d(d,z,npoint)
c
c     Flag intervals which will be pinched out.
c
      call set_id_status(id_status,d,d_pinch,d_min,ndist)
c
c     Pinch out flagged intervals
c
      call pinch_d(d,d_change,d_up,d_dn,id_move,id_status,ndist)
c
c     Redistribute pinched out length among candidate neighbors.
c
      call re_distribute_pinch
     1  (d,d_change,d_up,d_dn,id_status,id_move,ipt_up,ipt_dn,ndist)
c
c     Reset interval flags to identify intervals which will have
c     lenght added to them.
c
      call set_id_status(id_status,d,d_pinch,d_min,ndist)
c
c     Add lenght to flagged intervals.
c
      call pop_d(d,d_change,d_up,d_dn,d_min,id_move,id_status,ndist)
c
c     Redistribute added length by subtracting from candidate neighbors.
c
      call re_distribute_pop
     1  (d,d_change,d_up,d_dn,d_min,
     2   id_status,id_move,ipt_up,ipt_dn,ndist,ierr)
      if (ierr.ne.0) then
        write(errmsg,'(a)')'Error from routine re_distribute_pop '
        goto 9999
      endif
c
c      do i = 1,ndist
c       write(6,805)i,d(i),d_change(i),id_status(i)
c 805   format('slide pop',i5,2e15.5,i5 )
c      enddo
c
c     Convert interval lenght vector to coordinate vector.
c
      call d_to_z(zout,d,z_start,npoint)
c
c     Free memory
c
      call free(ipd)
      call free(ipd_change)
      call free(ipd_up)
      call free(ipd_dn)
      call free(ipid_status)
      call free(ipipt_up)
      call free(ipipt_dn)
 
 9999 return
      end
 
      subroutine re_distribute_pinch
     1  (d,d_change,d_up,d_dn,id_status,id_move,ipt_up,ipt_dn,ndist)
      implicit none
      integer ndist
      real*8 d(ndist)
      real*8 d_up(ndist)
      real*8 d_dn(ndist)
      real*8 d_change(ndist)
      integer id_status(ndist)
      integer id_move(ndist)
      integer ipt_up(ndist)
      integer ipt_dn(ndist)
      integer i, i_up, i_dn
 
      do i = 1,ndist
        if(id_status(i) .lt. 0)then
           if((id_move(i) .eq. 1) .or. (id_move(i) .eq. 2))then
              i_up = ipt_up(i)
              dowhile(id_status(i_up) .le. 1)
              i_up = ipt_up(i_up)
              enddo
              d(i_up) = d(i_up) + d_up(i)
              d_change(i_up) = d_change(i_up) + d_up(i)
           endif
           if((id_move(i) .eq. 1) .or. (id_move(i) .eq. 3))then
              i_dn = ipt_dn(i)
              dowhile(id_status(i_dn) .le. 1)
              i_dn = ipt_dn(i_dn)
              enddo
              d(i_dn) = d(i_dn) + d_dn(i)
              d_change(i_dn) = d_change(i_dn) + d_dn(i)
           endif
           id_status(i) = 0
        endif
      enddo
 
      return
      end
 
      subroutine re_distribute_pop
     1  (d,d_change,d_up,d_dn,d_min,
     2    id_status,id_move,ipt_up,ipt_dn,ndist,ierr)
      implicit none
      integer ndist,ierr
      real*8 d(ndist)
      real*8 d_up(ndist)
      real*8 d_dn(ndist)
      real*8 d_min(ndist)
      real*8 d_change(ndist)
      integer id_status(ndist)
      integer id_move(ndist)
      integer ipt_up(ndist)
      integer ipt_dn(ndist)
      integer i, i_up, i_dn
 
      ierr = 0
      do i = 1,ndist
        if(id_status(i) .lt. 0)then
c
c       Found an interval which was poped and needs go gain
c       some room from above or below.
c
           if((id_move(i) .eq. 1) .or. (id_move(i) .eq. 2))then
c
c             Look up for an interval that can be shrunk.
c
              i_up = ipt_up(i)
c              dowhile(id_status(i_up) .le. 0)
              dowhile((id_status(i_up)   .le. 0).or.
     1                (d(i_up)           .lt. 2*d_up(i)).or.
     2                (d(i_up) + d_up(i) .lt. d_min(i_up)))
              i_up = ipt_up(i_up)
              enddo
c
c             Interval d(i_up) is made smaller by d_up(i). d_up(i) is < 0.
c
              d(i_up) = d(i_up) + d_up(i)
              if(d(i_up) .lt. 0.0d0)then
cdebug           write(6,*)'Error:pop up',i, i_up, d(i_up), d_up(i)
                 ierr = 1
              endif
              if(d(i_up) .lt. d_min(i_up))then
cdebug           write(6,*)'Error:min up',i, i_up, d(i_up), d_min(i_up)
                 ierr = 1
              endif
              d_change(i_up) = d_change(i_up) + d_up(i)
           endif
           if((id_move(i) .eq. 1) .or. (id_move(i) .eq. 3))then
c
c             Look down for an interval that can be shrunk.
c
              i_dn = ipt_dn(i)
c              dowhile(id_status(i_dn) .le. 0)
              dowhile((id_status(i_dn)   .le.0).or.
     1                (d(i_dn)           .lt.2*d_dn(i)).or.
     2                (d(i_dn) + d_dn(i) .lt. d_min(i_dn)))
              i_dn = ipt_dn(i_dn)
              enddo
c
c             Interval d(i_dn) is made smaller by d_dn(i). d_dn(i) is < 0.
c
              d(i_dn) = d(i_dn) + d_dn(i)
              if(d(i_dn) .lt. 0.0d0)then
cdebug           write(6,*)'Error:pop dn',i, i_dn, d(i_dn), d_dn(i)
                 ierr = 1
              endif
              if(d(i_dn) .lt. d_min(i_dn))then
cdebug           write(6,*)'Error:min dn',i, i_dn, d(i_dn), d_min(i_dn)
                 ierr = 1
              endif
              d_change(i_dn) = d_change(i_dn) + d_dn(i)
           endif
           id_status(i) = 0
        endif
      enddo
 
      return
      end
 
      subroutine pop_d
     1   (d,d_change,d_up,d_dn,d_min,id_move,id_status,ndist)
      implicit none
      integer ndist
      real*8 d(ndist)
      real*8 d_change(ndist)
      real*8 d_up(ndist)
      real*8 d_dn(ndist)
      real*8 d_min(ndist)
      integer id_move(ndist)
      integer id_status(ndist)
      integer i
      real*8  d_get
 
      do i = 1, ndist
        if(id_status(i) .eq. 2)then
           d_get = d_min(i) - d(i)
           d_change(i) = d_change(i) +  d_get
           id_status(i) = - id_status(i)
           if(id_move(i) .eq. 1)then
              d_up(i) = - 0.5d0*d_get
              d_dn(i) = d_up(i)
           elseif(id_move(i) .eq. 2)then
              d_up(i) = -d_get
              d_dn(i) = 0.0d0
           elseif(id_move(i) .eq. 3)then
              d_up(i) = 0.0d0
              d_dn(i) = -d_get
           endif
           d(i) = d_min(i)
        endif
      enddo
 
      return
      end
 
      subroutine pinch_d(d,d_change,d_up,d_dn,id_move,id_status,ndist)
      implicit none
      integer ndist
      real*8 d(ndist)
      real*8 d_change(ndist)
      real*8 d_up(ndist)
      real*8 d_dn(ndist)
      integer id_move(ndist)
      integer id_status(ndist)
      integer i
 
 
      do i = 1, ndist
        if(id_status(i) .eq. 1)then
           d_change(i) = d_change(i) - d(i)
           id_status(i) = - id_status(i)
           if(id_move(i) .eq. 1)then
              d_up(i) = 0.5d0*d(i)
              d_dn(i) = d_up(i)
           elseif(id_move(i) .eq. 2)then
              d_up(i) = d(i)
              d_dn(i) = 0.0d0
           elseif(id_move(i) .eq. 3)then
              d_up(i) = 0.0d0
              d_dn(i) = d(i)
           endif
           d(i) = 0.0d0
        endif
      enddo
 
      return
      end
 
      subroutine set_pointers(ipt_up,ipt_dn,ndist)
      implicit none
      integer ndist
      integer ipt_up(ndist)
      integer ipt_dn(ndist)
      integer i
 
      do i = 1, ndist
        ipt_up(i) = i+1
      enddo
      ipt_up(ndist) = 1
 
      do i = 1, ndist
         ipt_dn(i) = i-1
      enddo
      ipt_dn(1) = ndist
 
      return
      end
      subroutine set_id_status(id_status,d,d_pinch,d_min,ndist)
      implicit none
      integer ndist
      integer id_status(ndist)
      real*8 d(ndist)
      real*8 d_pinch(ndist)
      real*8 d_min(ndist)
      integer i
 
      do i = 1, ndist
 
         if(id_status(i) .ne. 0)then
         if(d(i) .le. d_pinch(i))then
            id_status(i) = 1
         elseif((d(i) .gt. d_pinch(i)) .and. (d(i) .le. d_min(i)))then
            id_status(i) = 2
         else
            id_status(i) = 3
         endif
         endif
      enddo
      return
      end
 
c     INPUT Z(npoint) elevations
c     OUTPUT d(npoint) with distance between upper and lower elevation
c            for each layer
c
      subroutine z_to_d(d,z,npoint)
      implicit none
      integer npoint
      real*8 z(npoint)
      real*8 d(npoint-1)
      integer i
 
      do i = 1, npoint-1
         d(i) = z(i+1) - z(i)
      enddo
      return
      end
 
c     INPUT z_start         original start elevations
c           d(npoint)       distance between two points
c     OUTPUT z(npoint)      fill output elevations
 
      subroutine d_to_z(z,d,z_start,npoint)
      implicit none
      integer npoint
      real*8 z(npoint)
      real*8 d(npoint-1)
      real*8  z_start
      integer i
 
      z(1) = z_start
      do i = 1, npoint-1
         z(i+1) = z(i) + d(i)
      enddo
      return
      end
 
      subroutine check_input(itrue,d_pinch,d_min,ndist)
      implicit none
      integer itrue, ndist
      real*8 d_pinch(ndist)
      real*8 d_min(ndist)
      integer i
 
      itrue = 0
      do i = 1, ndist
         if( d_pinch(i) .ge. d_min(i))then
            itrue = itrue + 1
         endif
      enddo
      return
      end
 
      subroutine check_d_min(itrue,d_min_sum,d_total,d_min,ndist)
      implicit none
      integer itrue, ndist
      real*8 d_min(ndist)
      real*8 d_total
      real*8 d_min_sum
      integer i
 
      itrue = 0
      d_min_sum = 0.0d0
      do i = 1, ndist
         d_min_sum = d_min_sum + d_min(i)
      enddo
 
      if(d_min_sum .gt. d_total)itrue = -1
 
      return
      end
 
C     check that values are increasing
      subroutine monotonic(ifalse,z,npoint)
      implicit none
      integer ifalse, npoint
      real*8 z(npoint)
      integer i
 
 
      ifalse = 0
      do i = 2, npoint
c        if ifalse set, then not monotonic
         if(z(i) .lt. z(i-1))then
            ifalse = ifalse + 1
         endif
      enddo
      return
      end
