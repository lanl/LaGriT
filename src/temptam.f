 
C#####################################################################
C
C      FILE - temptam.f
C             misc code pieces to test or prototype
C
C      Source code for geological applications using x3dgen
C
C      CHANGE HISTORY -
C
C      Removed stack options and beads_ona_ring
C      remaining routines may not be used anymore
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
C      PURPOSE - wrapper
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

# end file temptam.f 
