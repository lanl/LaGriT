 
C**********************************************************************
C HDR
C   NAME:
         subroutine read_trilayers(
     >               imsgin,xmsgin,cmsgin,msgtype,nwds,ierr)
c        command stack/trilayers or stack/quadlayers
C
C   PURPOSE
C       read TIN (triangulated surface files) and merge into one cmo
C       Does some error checking and corrections with options
C       The lower elevation of each layer truncates the upper.
C       The trunc and buffer options allows a file to be chosen
C       so that when it is added to the cmo, it truncates all
C       the previous layers, commonly done with the topo surface.
C
C     FORMAT:
C
C     STACK/TRILAYERS/ 
C     STACK/QUADLAYERS/
C                      [AVS | gmv]/[minx,miny, maxx,maxy] &
C                      filename(1) [matno,  ] &
C                      filename(i) [matno, refine] &
C                      filename(n) [matno, refine] &
C                      [ FLIP ]
C                      [BUFFER [xdistance] ]   &
C                      [ PINCH [xthick]    ]   &
C                      [ TRUNC [ifile]     ]   &
C                      [ REFINE [xrefine or irefine]]  &
C                      [ NOINTERFACE ]
C    NEW OPTIONS->     [ d_pinch r d_min r id_move i ] &
C
C    old version: mread/trilayers
C    stack/trilayers/
C    stack/quadlayers/
C     filetype       - optional avs | gmv filetypes, default avs
C     minx, miny,
C     maxx, maxy     - optional to subset each layer
C     filename(1),
C     ...filename(n) - required list of surface files, starting
C                      with bottom layer first
C    flip            - optional (flip = 0 = don't flip normals)
C    pinch xthick    - default (pinch = 0.0)
C                      real value xthick is mininum thickness
C                      if layers cross, this must be set to at least 0.
C                      this allows upper elev to be equal to lower
C                      If upper dips below lower surface
C                      (dpinch uses bead algorithm to deterimine pinchouts)
C    layerclr        - optional (layerclr = 0 = color elem color)
C    trunc nth_file  - optional
C                      integer nth_file truncates all layers below
C    buffer          - optional
C                      xvalue forms layers above and below
C                      each layer except top and bottom.
C    refine          - irefine will proportionally divide thickness
C                      xrefine will divide into given thickness
C    nointerface     - does not add the interface to final cmo
C                      use with buffer option
C                      extremly experimental
C
C    New node attribute added to the stack cmo is VINT layertyp
C    -1   bottom surface
C    -2   top    surface
C     0   original input surfaces (usually interfaces)
C     1   derived surface to buffer interfaces
C     2   derived surface added as refinement layer
C
C     idebug can be set with amount changing > 3 and > 5 
C
C     Screen output will give this information:
C ................................................................                
C  
C          surface name  layer color type llcorner  zic                           
C           surf-12.inp     1    1   -1         1)  -1.200000E+01                 
C                refine     2    1    2        37)  -8.500000E+00                 
C                buffer     3    1    1        73)  -6.000000E+00                 
C            surf-5.inp     4    2    0       109)  -5.000000E+00                 
C                buffer     5    2    1       145)  -4.000000E+00                 
C                buffer     6    2    1       181)   4.000000E+00                 
C             surf5.inp     7    3    0       217)   5.000000E+00                 
C                buffer     8    3    1       253)   6.000000E+00                 
C                buffer     9    3    1       289)   1.700000E+01                 
C       surf2_slope.inp    10    4    0       325)   1.800000E+01                 
C                buffer    11    4    1       361)   1.900000E+01                 
C            surf25.inp    12    4   -2       397)   2.500000E+01                 
C  
C Elements per layer:         48 total:        576                                
C Nodes    per layer:         36 total:        432                                
C STACK DONE:         5 files read out of         5                             
C STACK DONE:         7 layers created for        12 total.                     
C Layers truncated by surf2_slope.inp layer        10                             
C ................................................................                
C  
C
C    NEW options for bead_ona_ring algorithm
C    These options are used along with buffers to help
C    elements to follow the boundary layer interfaces
C
C     dpinch      If interval length d <= d_pinch then set to zero
C                 (pinch uses layer truncation to cause pinchouts)
c     dmin        If interval length is d_pinch < d < d_min set to d_min
c     move      = 1  Get or put values equally up and down
c                 2  Get or put values up only
c                 3  Get or put values down only
c
c     bead algorithm returns the following status variable
c     id_status =  0  Interval has been set to zero or d_min, don't change.
c               =  1  Candidate for setting to zero
c               = -1  Interval has been set to zero but redistribution has
c                        has not been done.
c               =  2  Candidate for setting to d_min
c               = -2  Interval has been set to d_min but redistribution has
c                        has not been done.
c               =  3  Interval length is > d_min
C
C
C     EXAMPLES:
C
C      this command will read 6 triangulated surface files, flip the
C      normal from down to up, truncate srfs 1-4 with fsrfbe.inp, and
C      pinch the layers out at thickness le 1.0 meter
C      dpinch and dmin move points so that they are between
C      dpinch or dmin, nothing in between
C      Warning, if truncating then dpinch should be 0.
C
C      cmo create cmo1
C      stack trilayers avs &
C           fsrf575.inp  fsrf09.inp   fsrf31.inp &
C           fsrf44.inp   fsrfbe.inp   fsrf1900.buf.inp &
C           /flip/ truncate 5/ pinch 1.0 /
C  or
C
C      stack trilayers avs &
C           fsrf575.inp  fsrf09.inp   fsrf31.inp &
C           fsrf44.inp   fsrfbe.inp   fsrf1900.buf.inp &
C           /flip/ truncate 5/ &
C           / buffer 3.0 / dpinch 0.0 / dmin 3.0 / move 3
C
C      this command will take the above trilayer cmo and convert
C      it to a tet grid:
C         trilayertotet/6/cmotet/cmo1/3
C
C   INPUT/OUTPUT
C         ierr   - INTEGER ERROR FLAG (==0 ==> OK, <>0 ==> AN ERROR)
C
C   COMMENTS
C         The main part of this code reads each surface file, 2 at a time
C   and constructs intermediate surfaces if requested. So the points are
C   processed in a layer by layer fashion.
C         The postprocessing is done column by column.
C
C
C   CHANGE HISTORY
C         t.cherry 7/96  - initial version
C         $Log:   /pvcs.config/t3d/src/read_trilayers.f_a  
CPVCS    
CPVCS       Rev 1.10   Thu Apr 06 13:42:16 2000   dcg
CPVCS    replace get_info_i call
C
C**********************************************************************
CCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCC
C                         Copyright, 1996
C
C This program was prepared by the Regents of the University of
C California at Los Alamos National Laboratory (the University) under
C Contract No. W-7405-ENG-36 with the U.S. Department of Energy(DOE).
C The University has certain rights in the program pursuant to the
C contract and the program should not be copied or distributed outside
C your organization.All rights in the program are reserved by the DOE
C and the University. Neither the U.S. Government nor the University
C makes any warranty, express or implied, or assumes and liability
C or responsibility for the use of this software.
C
CCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCC
c
      implicit none
c
c ARGS
      integer nwds
      integer imsgin(nwds), msgtype(nwds)
      real*8 xmsgin(nwds)
      character*(*) cmsgin(nwds)
 
      character*72 ifile
      character*72 ifile2
      character*72 ifile_trunc
      integer      nfiletrunc, ninter_top
      integer      filetype
      integer      flip
      integer      pinchout, gobeads
      integer      layerclr
      integer      iopt_move
      integer      iopt_refine
      integer      iopt_trunc
      integer      iopt_buff
      integer      iopt_remove
      integer      idebug, ifound, ibuff2, ichange
      integer      maxclr
      integer      ierr,ierr1,ierrw,ics,iwarn1,iwarn2
 
C
c LOCAL VARS
      character*8092 cbuf
      character*160 logmess
      character*72 errmsg
      character*32  cmostak
      character*32  cmo
      character*32  isubname
      logical      usrclr, svd
 
      integer       icharlnf
      integer
     >        i,ii,idx,j,jcol,jclr,ncmo,icmo
     >,       nread, itrunc, ntrunc
     >,       isubset,itop,ibot, laytyp
     >,       nelm, ntri, npoints
     >,       nnode
     >,       icscode
     >,       nfile
     >,       nlayer, nlayer_tot, nrefine
     >,       ifile_single
     >,       ilen, ityp, ipointi, ipointj
     >,       idone, squished, ichg, icnt
 
      character*72 layerlist(100000)
      character*72 flist(nwds)
      pointer (ipflist, flist)
      pointer (iplayerlist, layerlist)
 
      integer ltyplist(100000)
      pointer (ipltyplist,ltyplist)
      integer iclr(100000)
      pointer (ipiclr, iclr)
      integer irefine(100000)
      pointer (ipirefine, irefine)
      integer layertyp(1000000)
      pointer (iplayertyp, layertyp)
 
      real*8 xmin,xmax,ymin,ymax
      real*8 xthick, xvalue, znext
      real*8 distbuf
 
      integer nbeads
      real*8  xprev, yprev
      real*8  dmin, dpinch
      real*8  zin(100000)
      real*8  zout(100000)
      real*8  d_pinch(100000)
      real*8  d_min(100000)
      integer id_move(100000)
      pointer (ipd_pinch, d_pinch)
      pointer (ipd_min, d_min)
      pointer (ipid_move, id_move)
      pointer (ipzin, zin)
      pointer (ipzout, zout)
 
 
 
c CMO
      integer ltype_sav(1000000)
      pointer (ipltype_sav, ltype_sav)
      integer itetclr_sav(1000000)
      pointer (ipitetclr_sav, itetclr_sav)
      integer itetclr(1000000)
      pointer (ipitetclr, itetclr)
      integer ibuff(1000000)
      pointer (ipibuff, ibuff)
 
      real*8 xic(1000000)
      pointer (ipxic, xic)
      real*8 yic(1000000)
      pointer (ipyic, yic)
 
      real*8 zic(1000000)
      pointer (ipzic, zic)
      real*8 zic2(1000000)
      pointer (ipzic2, zic2)
      real*8 z_trunc(1000000)
      pointer (ipz_trunc, z_trunc)
 
 
C
C#######################################################################
c BEGIN
 
C-----INIT LOCAL VARS
      isubname='read_trilayers'
      errmsg= '-undefined error-'
      idone = 0
      usrclr = .false.
      svd = .false.
      filetype = 1
      flip = 0
      pinchout = 1
      gobeads = 0
      iopt_trunc = 0
      iopt_buff = 0
      iopt_refine = 0
      iopt_remove = 0
      layerclr = 0
      isubset = 0
      ncmo = 1
      ifile_single = 0
      xthick = 0.
      dpinch = 0.
      dmin = 0.
      iopt_move = 3
 
C     get mesh object name
      call cmo_get_name(cmostak,ics)
      if (ics.ne.0) then
         write(logmess,"('No current mesh object')")
         call writloga('default',0,logmess,0,ierrw)
         ierr = -1
         return
      endif
      call cmo_get_intinfo('idebug',cmostak,idebug,ilen,ityp,ics)
      if (idebug.ne.0) then
        write(logmess,"('Running in DEBUG mode.',i5)") idebug
        call writloga('default',0,logmess,0,ierrw)
      endif
 
 
       write(errmsg,'(a)') 'command or syntax error'
C------READ PARSER VALUES, i is next value, nwds is last
       i = 3
       if (cmsgin(1)(1:icharlnf(cmsgin(1))).eq.'read_trilayers') i=2
       if (cmsgin(i)(1:icharlnf(cmsgin(i))).eq.'avs') then
         i = i+1
         filetype = 1
       endif
       if (cmsgin(i)(1:icharlnf(cmsgin(i))).eq.'gmv') then
         i = i+1
         filetype = 2
       endif
c-----READ subset if exists
       if (msgtype(i).eq.2 .or. msgtype(i).eq.1) then
         isubset = 1
         if (msgtype(i).eq.2) xmin = xmsgin(i)
         if (msgtype(i).eq.1) xmin = dble(imsgin(i))
         i = i+1
         if (msgtype(i).eq.2) ymin = xmsgin(i)
         if (msgtype(i).eq.1) ymin = dble(imsgin(i))
         i = i+1
         if (msgtype(i).eq.2) xmax = xmsgin(i)
         if (msgtype(i).eq.1) xmax = dble(imsgin(i))
         i = i+1
         if (msgtype(i).eq.2) ymax = xmsgin(i)
         if (msgtype(i).eq.1) ymax = dble(imsgin(i))
         i = i+1
       endif
       if (msgtype(i).eq.1) then
         isubset = 1
         xmin = dble(imsgin(i))
         i = i+1
         ymin = dble(imsgin(i))
         i = i+1
         xmax = dble(imsgin(i))
         i = i+1
         ymax = dble(imsgin(i))
         i = i+1
       endif

C------READ toggles from end of parser
c      possible options are
c         truncate [file_number]
c         flip
c         nointerface
c         buffer  buffer_distance
c         refine  n_times_to_refine
c         pinch   min_height
c         layerclr
       do ii = i,nwds
       if (msgtype(ii) .eq. 3) then
         ilen = icharlnf(cmsgin(ii))
         if (cmsgin(ii)(1:5).eq.'trunc') then
             iopt_trunc = 1
             nwds = nwds-1
             if(msgtype(ii+1) .eq. 1) then
               nfiletrunc=imsgin(ii+1)
               nwds = nwds-1
             endif
         endif
         if (cmsgin(ii)(1:5).eq.'noint') then
             iopt_remove = 1
             nwds = nwds-1
         endif
         if (cmsgin(ii)(1:ilen).eq.'flip') then
             flip = 1
             nwds = nwds-1
         endif
         if (cmsgin(ii)(1:3).eq.'buf') then
             iopt_buff = 1
             usrclr = .true.
             ncmo = 3
             nwds = nwds-1
             if(msgtype(ii+1) .eq. 2) then
               distbuf=xmsgin(ii+1)
               nwds = nwds-1
             endif
             if(msgtype(ii+1) .eq. 1) then
               distbuf= dble(imsgin(ii+1))
               nwds = nwds-1
             endif
         endif
         if (cmsgin(ii)(1:3).eq.'ref') then
             iopt_refine = 1
             usrclr = .true.
             nwds = nwds-1
             if(msgtype(ii+1) .eq. 1) then
               nrefine=imsgin(ii+1)
               ncmo = nrefine
               nwds = nwds-1
             endif
         endif
         if (cmsgin(ii)(1:ilen).eq.'pinch') then
            pinchout = 1
            nwds = nwds-1
            gobeads = 0
            if (msgtype(ii+1).eq.2) then
               xthick = xmsgin(ii+1)
               nwds = nwds-1
            endif
            if (msgtype(ii+1).eq.1) then
               xthick = dble(imsgin(ii+1))
               nwds = nwds-1
            endif
         endif
         if (cmsgin(ii)(1:ilen).eq.'layerclr') then
            layerclr = 1
            nwds = nwds-1
         endif
 
CCCCCCCCCCCCCfor new code
         if (cmsgin(ii)(1:ilen).eq.'dpinch') then
            nwds = nwds-1
            if (msgtype(ii+1).eq.2) then
              dpinch = xmsgin(ii+1)
            elseif (msgtype(ii+1).eq.1) then
              dpinch = dble(imsgin(ii+1))
            endif
            nwds = nwds-1
            gobeads = 1
         endif
         if (cmsgin(ii)(1:ilen).eq.'dmin') then
            nwds = nwds-1
            if (msgtype(ii+1).eq.2) then
              dmin = xmsgin(ii+1)
            elseif (msgtype(ii+1).eq.1) then
              dmin = dble(imsgin(ii+1))
            endif
            nwds = nwds-1
            gobeads = 1
         endif
         if (cmsgin(ii)(1:ilen).eq.'move') then
            nwds = nwds-1
            iopt_move = imsgin(ii+1)
            nwds = nwds-1
            gobeads = 1
         endif
 
       endif
       enddo

C      Do some error checking on the command syntax
       if (isubset.ne.0 .and. xmin.ge.xmax) then
         write(logmess,'(a,e14.6,e14.6)')
     >   'Syntax error: xmin greater than xmax: ',xmin,xmax
         call writloga('default',1,logmess,0,ierrw)
         ierr = -1
       endif
       if (isubset.ne.0 .and. ymin.ge.ymax) then
         write(logmess,'(a,e14.6,e14.6)')
     >   'Syntax error: ymin greater than ymax: ',ymin,ymax
         call writloga('default',1,logmess,0,ierrw)
         ierr = -1
       endif
       if (dpinch .gt. dmin) then
         write(logmess,'(a,f7.2,f7.2)')
     >   'Syntax error: dpinch greater than dmin',dpinch,dmin
         call writloga('default',1,logmess,0,ierrw)
         ierr = -1
       endif
       if (xthick .gt. distbuf) then
         write(logmess,'(a,f7.2,f7.2)')
     >   'Syntax error: pinch greater than buffer',xthick,distbuf
         call writloga('default',1,logmess,0,ierrw)
         ierr = -1
       endif

       if (ierr .lt. 0) then
          write(logmess,'(a)')
     >    'STACK SYNTAX:'
          call writloga('default',0,logmess,0,ierr1)
          write(logmess,'(a,a)')
     >    'stack/trilayers/avs|gmv/[minx,miny, maxx,maxy]',
     >    '/ filename_1, ... filename_n '
          call writloga('default',0,logmess,1,ierr1)
          goto 999
       endif
 
       write(errmsg,'(a)') 'cmo error'
C      Create the temporary cmos
C      ierr.eq.0 means that the cmo already exists.
       call cmo_exist('def1',ierr)
       if(ierr.eq.0) then
          call x3d_error(isubname, 'def1 exists')
          goto 999
       endif
 
       call cmo_exist('def2',ierr)
       if(ierr.eq.0) then
          call x3d_error(isubname, 'def2 exists')
          goto 999
       endif
 
       write(errmsg,'(a)') 'mmgetblk memory error'
C------ALLOCATE memory
       call mmgetblk("flist", isubname, ipflist, 72*nwds, 1, ierr)
       if(ierr.ne.0)call x3d_error(isubname, 'mmgetblk flist')
       call mmgetblk("iclr", isubname, ipiclr, nwds, 1, ierr)
       if(ierr.ne.0)call x3d_error(isubname, 'mmgetblk iclr')
       call mmgetblk("irefine", isubname, ipirefine, nwds, 1, ierr)
       if(ierr.ne.0)call x3d_error(isubname, 'mmgetblk irefine')
 
C------Finish command line processing of file names and their options
C      READ file names and options before read is called with parser
C todo syntax should allow refine numbers to start at first file
C      as well as current starting at second file
       write(errmsg,'(a)') 'syntax error in file list'
       nfile = 0
       do ii = i, nwds
         if (msgtype(ii).eq.2) then
           call x3d_error(isubname,'int or char expected')
           ierr = ierr+1
           goto 999
         elseif (msgtype(ii).eq.3 ) then
           nfile = nfile+1
           flist(nfile) = cmsgin(ii)
           if (msgtype(ii+1).eq.1) then
             iclr(nfile) = imsgin(ii+1)
             usrclr=.true.
           else
             iclr(nfile) = nfile
           endif
           if (msgtype(ii+2).eq.1) then
             if(nfile.eq.1) then
               write(logmess,'(a)') 
     > 'syntax warning: file1 integer / file2  integer integer '
               call writloga('default',0,logmess,0,ierrw)
               write(logmess,'(a)') 
     > 'refine starts at second surface, number ignored for file1.'
               call writloga('default',0,logmess,0,ierrw)
             else
               irefine(nfile) = imsgin(ii+2)
             endif
           else
             irefine(nfile) = 0
           endif
         endif
       enddo
       maxclr = iclr(nfile)
 
       write(errmsg,'(a)') 'error during setup'

       if (nfile .lt. 2 ) call x3d_error(isubname, '1 File, no merge')
       if (nfile .eq. 1 ) ifile_single = 1
       if (nfile .eq. 1 ) nfile = 2
       if (iopt_remove.ne.0 ) then
          write(logmess,'(a)')
     >    'ERROR: option for NO interfaces not available.'
          call writloga('default',1,logmess,1,ierr1)
          goto 999
       endif

c      INFORMATION computing
c      count number of layers there will be in the cmo
c      ntrunc are the layers to truncate by nfiletrunc
c      nfiletrunc is the truncating layer, or top layer
c      nfiletrunc can be buffered if it is not top layer
c      ninter_top is the top interface that can be buffered
       ninter_top = nfile - 1
       if (iopt_trunc.ne.0 .and. nfiletrunc.ne.nfile) 
     >     ninter_top = nfiletrunc
       nlayer_tot = 0
       do i = 1, nfile
         nlayer_tot = nlayer_tot+irefine(i)
         if (iopt_remove.eq.0) then
            nlayer_tot = nlayer_tot+1
         elseif (i.eq.1 .or. i.eq.nfile) then
            nlayer_tot = nlayer_tot+1
         endif
         if (iopt_buff.gt.0) then
           if (i.ne.1 .and. i.le.ninter_top) nlayer_tot=nlayer_tot+2
         endif
         if (i.le.nfiletrunc) ntrunc = nlayer_tot
       enddo
       if(iopt_buff.gt.0 .and. iopt_trunc.ne.0) then
         ntrunc = ntrunc - 1
       endif

       if (iopt_trunc.ne.0) then
          if (nfiletrunc.eq.0) nfiletrunc = nfile
          ifile_trunc = flist(nfiletrunc)
       endif

       write(logmess,'("Layers to create: ",i10)')nlayer_tot
       call writloga('default',1,logmess,0,ierrw)
       write(logmess,'("Max material number: ",i10)')maxclr
       call writloga('default',0,logmess,0,ierrw)
       if (iopt_trunc .ne. 0) then
         write(logmess,'("Layers under trunc file: ",i10)')ntrunc-1
         call writloga('default',0,logmess,0,ierrw)
          write(logmess,'(a)')
     >    'truncating file: '//ifile_trunc(1:icharlnf(ifile_trunc))
          call writloga('default',0,logmess,0,ierrw)
       endif
       write(logmess,'("Reading ",i5," surface files...")')nfile
       call writloga('default',1,logmess,0,ierrw)
 
c     add attribute to indicate layer type such as
c     bottom, top, interface, buffer or refinement 
      if(idebug.le.3) call writset('stat','tty','off',ierrw)
      call mmgetpr('layertyp',cmostak,iplayertyp,ifound)
      if (ifound.ne.0) then
        cbuf='cmo/addatt/' // cmostak(1:icharlnf(cmostak)) //
     >       '/layertyp/' //
     >       'VINT/scalar/nnodes//permanent/agfx ; finish'
        call dotaskx3d(cbuf,ierr)
        if(ierr.ne.0) write(errmsg,'(a)') 'make att layertyp'
        if(ierr.ne.0)  goto 999
      endif

c      FILL and SAVE TRUNCATE SURFACE ELEVATIONS
c      Save unaltered z values of the truncating surface
       if (iopt_trunc .ne. 0 ) then
         call file_exist(ifile_trunc(1:icharlnf(ifile_trunc)),ierr)
         if(ierr.ne.0) then 
           errmsg = 'Missing surface file.'
           goto 999
         endif

         write(logmess,'(a,a)')
     >  'Read truncate layer: ',ifile_trunc(1:icharlnf(ifile_trunc))
         call writloga('default',0,logmess,0,ierrw)
         cmo = 'def1'
         if (filetype.eq.1) then
           cbuf = 'read/avs/' // ifile_trunc(1:icharlnf(ifile_trunc))//
     >        '/ def2' // ' ; finish '
         else
           cbuf = 'read/gmv/' // ifile_trunc(1:icharlnf(ifile_trunc))//
     >        '/ def2' // ' ; finish '
         endif

         call dotaskx3d(cbuf,ierr)
         cmo="def2"
         if(ierr .ne. 0)call x3d_error(isubname, ifile_trunc)
         if(ierr.ne.0)  goto 999

c        subset the truncating surface, set pointers
         if (isubset .ne. 0) then
           write(errmsg,'(a)') 'error during subset'
           call trilayer_subset(cmo,xmin,ymin,xmax,ymax,ierr)
           if(ierr .ne. 0)call x3d_error(isubname, ifile_trunc)
           if(ierr.ne.0)  goto 999
           write(errmsg,'(a)') 'error during setup'
         endif

         call cmo_get_info('nnodes',cmo,nnode,ilen,ityp,ierr)
         call cmo_get_info('zic',cmo,ipzic2,ilen,ityp,ierr)
         write(errmsg,'("get_info saving zic for truncating layer")')
         if(ierr.ne.0) goto 999

c        check if z_trunc block exists
         call mmgetindex('z_trunc',isubname,ifound,ierr)
         if (ifound .le. 0) then
           call mmgetblk("z_trunc", isubname,ipz_trunc,nnode,2,ierr)
           if(ierr.ne.0) then
             call x3d_error(isubname, 'mmgetblk z_trunc')
             goto 999
           endif
         endif
         do i = 1, nnode
           z_trunc(i) = zic2(i)
         enddo

         svd = .true.
         write(logmess,'(a,i10,1x,a)')
     *      'Saved truncating layer: ',
     *      ntrunc,flist(ntrunc)(1:icharlnf(flist(ntrunc)))
         call writloga('default',1,logmess,0,ierr1)
         call writset('stat','tty','off',ierrw)
         call dotaskx3d('cmo/delete/def2/ ; finish',ierr)
       endif
c      end collection of truncating elevations

c      ALLOCATE MEMORY for local arrays
       write(errmsg,'(a)') 'mmgetblk memory error'
       call mmgetblk("layerlist",isubname,
     *               iplayerlist,72*nlayer_tot,1,ierr)
       if(ierr.ne.0)call x3d_error(isubname, 'mmgetblk layerlist')
       call mmgetblk("ltyplist",isubname,
     *               ipltyplist,nlayer_tot,1,ierr)
       if(ierr.ne.0)call x3d_error(isubname, 'mmgetblk ltyplist')
       do i = 1, nlayer_tot
         layerlist(i) = '-notset-'
         ltyplist(i)  = -1
       enddo


 
C...................................................................
C      FILL FIRST cmo def1
 
c      read the first file
       write(errmsg,'(a)') 'error setting up first surface'
       nread = 0
       nlayer = 0
       ifile = flist(1)
       call file_exist(ifile(1:icharlnf(ifile)),ierr)
       if(ierr.ne.0) goto 999
 
       write(logmess,'(a,a)')
     > 'Read first surface: ',ifile(1:icharlnf(ifile))
       call writloga('default',0,logmess,0,ierrw)
 
c      TURN OFF OUTPUT
       if(idebug.le.3) call writset('stat','tty','off',ierrw)
       cmo = 'def1'
       if (filetype.eq.1) then
         cbuf = 'read/avs/' //
     >        ifile(1:icharlnf(ifile)) //
     >        '/ def1' //
     >        ' ; finish '
       else
         cbuf = 'read/gmv/' //
     >        ifile(1:icharlnf(ifile)) //
     >        '/ def1' //
     >        ' ; finish '
 
       endif
       call dotaskx3d(cbuf,ierr)
       if(ierr .ne. 0)call x3d_error(isubname, ifile)
       if(ierr.ne.0)  goto 999
       nread = nread + 1
 
c      subset the first surface layer, set pointers
c      allow subset info to be written this once
       if (isubset .ne. 0) then
         call cmo_set_info('idebug',cmo,5,1,1,ierr)
         call trilayer_subset(cmo,xmin,ymin,xmax,ymax,ierr)
         if(ierr .ne. 0)call x3d_error(isubname, ifile)
         if(ierr.ne.0)  goto 999
         call cmo_set_info('idebug',cmo,idebug,1,1,ierr)
       endif

       call cmo_get_info('nelements',cmo,nelm,ilen,ityp,ierr)
       call cmo_get_info('nnodes',cmo,nnode,ilen,ityp,ierr)
       call cmo_get_info('itetclr',cmo,ipitetclr,ilen,ityp,ierr)
       call cmo_get_info('zic',cmo,ipzic,ilen,ityp,ierr)
       if(nelm .le. 0) call x3d_error(isubname, ' 0 elements found.')
       if( ierr.ne.0 .or. nelm.le.0 ) then
         goto 999
       endif
       ntri = nelm
       npoints = nnode

c-----truncate elevations
c      truncate first surface by saved elevations
       itrunc=0
       if (iopt_trunc.ne.0 .and. nlayer.lt.ntrunc) then
c        loop through points on this surface
         do i = 1, npoints
           xvalue= z_trunc(i)-zic(i)
           if (xvalue.lt.xthick) then
             zic(i)=z_trunc(i)
             itrunc=itrunc+1
           endif
         enddo
         if(itrunc.gt.0) then
           write(logmess,'(a,i10,a)')
     >     'Truncated ',itrunc,' in layer 1 '
     >     //flist(1)(1:icharlnf(flist(1)))
           call writloga('default',0,logmess,0,ierr)
         endif
       endif
c      end truncate
 
c      check normals of the triangles, flip if down
       call cktrilayer_norm('def1',idebug, flip, ierr)
       if(ierr .ne. 0)
     >   call x3d_error(isubname, ifile(1:icharlnf(ifile)))
 
       if (ifile_single .ne. 0 ) then
         nread=1
         nfile=1
         goto 500
       endif

c      create arrays to save the tet colors and layertyp
       call mmgetblk("itetclr_sav", isubname,
     >      ipitetclr_sav, ntri*nlayer_tot,1,ierr)
       if(ierr.ne.0) then
         call x3d_error(isubname, 'mmgetblk itetclr_sav') 
         goto 999
       endif
       call mmgetblk("ltype_sav", isubname,
     >      ipltype_sav, npoints*nlayer_tot,1,ierr)
       if(ierr.ne.0) then
         call x3d_error(isubname, 'mmgetblk ltype_sav') 
         goto 999
       endif
 
c      color by original tri colors, else 1 for first layer
c      if usrclr, use user chosen layer id, but allow
c         tetclr gt 1 to remain gt 1
c      layer type for first layer is -1
       do i = 1, npoints
         ltype_sav(i) = -1
       enddo
       if (layerclr .ne. 0) then
         do i = 1, ntri
            itetclr_sav(i) = 1
         enddo
       else
         do i = 1, ntri
            itetclr_sav(i) = itetclr(i)
            if (usrclr) then
              if (itetclr(i).eq.1 .or. itetclr(i).eq.2) then
                itetclr_sav(i) = iclr(1)
              else
                itetclr_sav(i) = itetclr(i) + maxclr
              endif
            endif
         enddo
       endif
       nlayer = nlayer+1
       layerlist(nlayer) = ifile
 
c      nfile  = number of files total to read into a cmo
c      nread  = number of files read  to create layers
c      nlayer = number of layers created from other layers
c      ncmo   = number of cmo layers created between layers read
c      nelm   = number of elements in each cmo
c      ntri   = number of elements expected based on first file read
C,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,
C------MAIN LOOP for read, merge files------------------------------
 
       do ii = 2, nfile
 
       write(errmsg,'(a,i10)') 'error while at surface ',ii
       if(idebug.gt.0) then
         write(logmess,'(a,i5,a)')
     >   '--- surface cmo ',ii,'-----------------------------'
         call writloga('default',0,logmess,0,ierr)
       endif
c      TURN OFF OUTPUT
       if(idebug.le.3) call writset('stat','tty','off',ierrw)
 
c      here def2 becomes def1 of the 2 surfaces
c      then we read in a new def2
       if (ii.gt.2 ) then
         call dotaskx3d('cmo/copy/def1/cmonxt/ ; finish',ierr)
         if(ierr .ne. 0)call x3d_error(isubname, 'cmo copy cmonxt')
         call dotaskx3d('cmo/delete/cmonxt/ ; finish',ierr)
       elseif (ii.eq.2) then
         call dotaskx3d('cmo/copy/cmoprev/def1/ ; finish',ierr)
         if(ierr .ne. 0)call x3d_error(isubname, 'cmo copy def1')
       endif
 
      ifile = flist(nread)
c     -------Read cmo def2
      ifile2 = flist(ii)
      call file_exist(ifile2(1:icharlnf(ifile2)),ierr)
      if(ierr.ne.0) goto 999
 
       write(logmess,'(a,a)')'Read surface: ',ifile2(1:icharlnf(ifile2))
       call writloga('default',0,logmess,0,ierrw)

c      TURN OFF OUTPUT
       if(idebug.le.3) call writset('stat','tty','off',ierrw)
       cmo = 'def2'
       if (filetype.eq.1) then
         cbuf = 'read/avs/' //
     >       ifile2(1:icharlnf(ifile2)) //
     >       '/ def2' //
     >       ' ; finish '
       else
         cbuf = 'read/gmv/' //
     >       ifile2(1:icharlnf(ifile2)) //
     >       '/ def2' //
     >       ' ; finish '
       endif
       call dotaskx3d(cbuf,ierr)
       if(ierr .ne. 0)call x3d_error(isubname, ifile2)
       if(ierr.ne.0)  goto 999
       nread = nread + 1
 
c      subset the next surface layer, set pointers
       if (isubset .ne. 0) then
         call trilayer_subset(cmo,xmin,ymin,xmax,ymax,ierr)
         if(ierr .ne. 0)call x3d_error(isubname, ifile2)
         if(ierr.ne.0)  goto 999
       endif

       call cmo_get_info('zic',cmo,ipzic2,ilen,ityp,ierr)
       if( ierr.ne.0 ) then
         if (ierr.ne.0) write(errmsg,'(a)') 'get_info zic2 '
         goto 999
       endif

c      truncate surface by saved elevations
       itrunc=0
       if (iopt_trunc.ne.0 .and. nlayer.lt.ntrunc) then
c        loop through points on this surface
         do i = 1, npoints
           xvalue= z_trunc(i)-zic2(i)
           if (xvalue.lt.xthick) then
             zic2(i)=z_trunc(i)
             itrunc=itrunc+1
           endif
         enddo
         if(itrunc.gt.0) then
           write(logmess,'(a,i10,a,i5,a)')
     >     'Truncated ',itrunc,' in layer',nlayer,
     >     ' '//flist(nlayer)(1:icharlnf(flist(nlayer)))
           call writloga('default',0,logmess,0,ierr)
         endif
       endif
c      end truncate

C     .........................................................
C     LOOP THROUGH LAYERS IN A UNIT (BETWEEN TWO LAYERS)
 
c     figure out how many layer cmo's to create for this unit
c     def1 is lower layer, def2 is top layer of unit
c     cmonxt will be the next layer after def1 and cmoprev
c     since only interfaces are buffered, top and bottom unit have 1 buffer
c     all other units will have 2 buffers
      ncmo=irefine(ii) + 1
      if (iopt_buff.ne.0) then
        ncmo=ncmo+1
        if (nread.ne.2 .and. nread.le.ninter_top) then
           ncmo=ncmo+1
        endif
      endif
 
      do icmo = 1, ncmo
c     figure out type of layer, buffer, refinement or top of unit
c     laytyp 1 = buffer, 2 = refinement, 0 = top layer (interface)
 
c     first unit
      if (nread.eq.2 .and. icmo.ne.ncmo) then
        laytyp = 2
        if (iopt_buff.ne.0 .and. icmo.eq.ncmo-1) laytyp = 1
 
c     last unit
      elseif (nread.eq.nfile .and. icmo.ne.ncmo) then
        laytyp = 2
        if (iopt_buff.ne.0 .and. icmo.eq.1) laytyp = 1
 
c     middle units
      elseif (iopt_buff.ne.0 .and. (icmo.eq.1)) then
        laytyp = 1
      elseif (iopt_buff.ne.0 .and. (icmo.eq.ncmo-1)) then
        laytyp = 1
      elseif (icmo.ne.ncmo .and. irefine(nread).ne.0) then
        laytyp = 2
c
c     done with this unit
      else
        laytyp = 0
      endif

c     DERIVE intermediate layers
      if(idebug.gt.0) then
        write(logmess,'(a,i5,a,i5,a,i5)')
     >  'Unit cmo ',icmo,' of ',ncmo,'  type: ',laytyp
        call writloga('default',0,logmess,0,ierr)
      endif
      if(idebug.le.3) call writset('stat','tty','off',ierrw)
 
c-----If this is a buffer layer
      if (laytyp .eq. 1) then
        cmo = 'cmonxt'
        xvalue=distbuf
        if (icmo.ne.1 .or. nread.eq.2) xvalue=-1.0*distbuf
        write(cbuf,'(a,1pe13.6,a)')
     >    'trilayer/derive/constant/cmonxt/def1/def2/ ',
     >     xvalue,' /  ; finish '
        call dotaskx3d(cbuf,ierr)
        if(ierr.ne.0)call x3d_error(isubname, 'derive tri layer')
        if(ierr.ne.0)  goto 999
        nlayer =  nlayer + 1
        layerlist(nlayer) = 'buffer'
 
 
c-----If this is a refinement layer
      elseif (laytyp .eq. 2) then
 
        cmo = 'cmonxt'
        if (iopt_buff.ne.0 ) then
           if (nread.eq.2 ) then
             xvalue= (1.0/(ncmo-1))*(icmo)
           elseif (nread.eq.nfile) then
             xvalue= (1.0/(ncmo-1))*(icmo-1)
           else
             xvalue= (1.0/(ncmo-2))*(icmo-1)
           endif
        else
           xvalue= (1.0/(ncmo))*icmo
        endif
        write(cbuf,'(a,1pe13.6,a)')
     >   'trilayer/derive/proportional/cmonxt/def1/def2/ ',
     >   xvalue,' /  ; finish '
        call dotaskx3d(cbuf,ierr)
        if(ierr.ne.0)call x3d_error(isubname, 'derive tri layer')
        if(ierr.ne.0)  goto 999
        nlayer =  nlayer + 1
        layerlist(nlayer) = 'refine'
 
c-----Else this is top layer of pair, make def2 cmonxt
      else
 
        call dotaskx3d('cmo/copy/cmonxt/def2/ ; finish',ierr)
        if(ierr .ne. 0)call x3d_error(isubname, 'cmo copy')
        call dotaskx3d('cmo/delete/def2/ ; finish',ierr)
        nlayer =  nlayer + 1
        layerlist(nlayer) = ifile2
 
      endif

c-----done filling cmonxt, nlayer is set

      ltyplist(nlayer) = laytyp
      if (nlayer .eq. nlayer_tot) ltyplist(nlayer) = -2
 
      cmo='cmonxt'
      call cmo_get_info('nelements',cmo,nelm,ilen,ityp,ierr)
      if(ierr.ne.0) goto 999
      if(nelm.ne.ntri) then
         write(logmess,'(a,i10,i10)')'Error - sets unequal: ',ntri,nelm
         call writloga('default',0,logmess,0,ierr)
         goto 999
      endif
      if(idebug.le.3) call writset('stat','tty','off',ierrw)
 
C**** IF NOT REMOVING THIS LAYER, DO SOME WORK
      if (iopt_remove.eq.0 .or. nlayer.eq.nlayer_tot .or.
     *    icmo.ne.ncmo ) then
 
c     If this is a derived layer (1 or 2), inherit color from layer below.
      if (ltyplist(nlayer) .gt. 0 ) then
          write(cbuf,'(a)')
     >   'cmo/copyatt/cmonxt / def1 /itetclr/itetclr/  ; finish '
        call dotaskx3d(cbuf,ierr)
        if(ierr.ne.0)call x3d_error('copy att itetclr',isubname)
        if(ierr.ne.0)  goto 999
      endif
 
c     save the tet colors and layer type, so not clobbered by merge
c     color by original tri colors, else layer count
c     if usrclr, use user chosen layer id, but allow
c       itetclr 1 normal points down, 2 points up
c       tetclr gt 2 to remain gt 2 as these are tipping elements
 
      do i = 1,npoints
         j=(nlayer-1)*npoints
         ltype_sav(j+i) = ltyplist(nlayer) 
      enddo
      call cmo_get_info('itetclr',cmo,ipitetclr,ilen,ityp,ierr)
      if (ierr.ne.0) write(errmsg,'(a)') 'get itetclr '
      if (ierr.ne.0) goto 999
      if (layerclr .ne. 0) then
        do i = 1, ntri
           itetclr_sav((nread*ntri)+i) = nread
        enddo
      else
        j = (nlayer-1)*ntri
        jclr = iclr(nread-1)
        if (icmo.eq.ncmo) jclr = iclr(nread)
 
        do i = 1, ntri
           itetclr_sav(j+i) = itetclr(i)
           if (usrclr) then
             if (itetclr(i).eq.1 .or. itetclr(i).eq.2) then
               itetclr_sav(j+i) = jclr
             else
               itetclr_sav(j+i) = itetclr(i) + maxclr
             endif
           endif
        enddo
      endif
 
C-----DONE filling cmos def1 and cmonxt, do error check
c     done reading or creating layers
 
C     Do some error checks and reporting
      cmo= 'cmonxt'
 
      call cktrilayer_norm('cmonxt',0, flip, ierr)
      if(ierr .ne. 0) then
        write(errmsg,'(a)') 'Normals in '//ifile2(1:icharlnf(ifile2))
        goto 999
      endif
 
C     pinchout layers at xthick
C     do not pinchoout truncating layer or its lower buffer
      j=nlayer
      ierr1 = idebug
      call cktrilayer_elev("cmoprev",cmo,pinchout,xthick,ierr1)
      write(logmess,'(a)') 'Layers compared: '//
     >layerlist(j-1)(1:icharlnf(layerlist(j-1)))//
     >' and '//layerlist(j)(1:icharlnf(layerlist(j)))
      call writloga('default',0,logmess,0,ierr1)
 
      endif
c     done checking finished stacked layers
 
 
C-----MERGE cmo cmonxt and def1 if first, else cmostak
c     TURN OFF OUTPUT
      if(idebug.le.3) call writset('stat','tty','off',ierrw)
 
c     copy current cmo to cmoprev
c     for ncmo = last - def2 becomes cmoprev
c     if intrfaces removed, do not change cmoprev
c     reminder - previous cmo not copied at interface
 
      if(idebug.le.3) call writset('stat','tty','off',ierrw)
      if (icmo.eq.ncmo .and. iopt_remove.ne.0) then
      else
        call writset('stat','tty','off',ierrw)
        call dotaskx3d('cmo/delete/cmoprev/ ; finish',ierr)
        call dotaskx3d('cmo/copy/cmoprev/cmonxt/ ; finish',ierr)
        if(ierr .ne. 0)call x3d_error(isubname, 'cmo copy')
      endif
 
c     this is the first layer
      if(idebug.le.3) call writset('stat','tty','off',ierrw)
      if (ii.eq.2 .and. icmo.eq.1 ) then
        call dotaskx3d('addmesh/merge/' //
     >       cmostak(1:icharlnf(cmostak)) //
     >       '/def1/cmonxt/ ; finish',ierr)
 
c     these are the interfaces do not add to mesh if iopt_remove
      elseif ( icmo.eq.ncmo  ) then
        if (iopt_remove .eq. 0 .or. nlayer.eq.nlayer_tot ) then
          call dotaskx3d('addmesh/merge/' //
     >         cmostak(1:icharlnf(cmostak)) // ' / ' //
     >         cmostak(1:icharlnf(cmostak)) //
     >         '/cmonxt/ ; finish',ierr)
        else
          nlayer = nlayer-1
        endif
 
c     these are derived buffer and refine layers between interfaces
      else
        call dotaskx3d('addmesh/merge/' //
     >       cmostak(1:icharlnf(cmostak)) // ' / ' //
     >       cmostak(1:icharlnf(cmostak)) //
     >       '/cmonxt/ ; finish',ierr)
      endif
 
c     TURN ON OUTPUT
      call writset('stat','tty','on',ierrw)
      if(ierr .ne. 0) write(errmsg,'(a)') 'addmesh merge'
      if(ierr.ne.0)  goto 999
      call cmo_newlen(cmostak,ierr)
      call cmo_get_info('nelements',cmostak,nelm,ilen,ityp,ierr)
      call cmo_get_info('nnodes',cmostak,nnode,ilen,ityp,ierr)
 
      enddo
C     .........................................................
C     END layers per file
 
      enddo
C,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,
C     END MAIN LOOP - layers are now merged into cmostak
C     Done building trilayer cmo, now do some post-processing
C     Make the stack cmo current

      cmo = cmostak
      write(errmsg,'(a)') 'error during update of new stack cmo'
 
c COPY SAVED VALUES
      if(idebug.le.3) call writset('stat','tty','off',ierrw)
      call cmo_get_info('nelements',cmo,nelm,ilen,ityp,ierr1)
      call cmo_get_info('itetclr',cmo,ipitetclr,ilen,ityp,ierr)
      call cmo_get_info('layertyp',cmo,iplayertyp,ilen,ityp,ics)
      if (ierr.ne.0 .or. ierr1.ne.0 .or. ics.ne.0)
     *     write(errmsg,'("get_info copy itetclr and layertyp")')
      if (ierr.ne.0 .or. ierr1.ne.0) goto 999
      if (iopt_buff.gt.0)
     >    call cmo_get_info('layertyp',cmo,iplayertyp,ilen,ityp,ierr1)
      if (ierr1.ne.0) write(errmsg,'("get_info copy layertyp ")')
      if (ierr1.ne.0) goto 999
 
      if (nelm.ne. nlayer*ntri) then
         write(logmess,'(a,i10,a,i10)')
     >   'Error: expected ',nlayer*ntri,' elem, got ',nelm
         call writloga('default',0,logmess,0,ierrw)
         goto 999
      else
        do i = 1, nelm
           itetclr(i) = itetclr_sav(i)
        enddo
        do i = 1, npoints*nlayer
           layertyp(i)=ltype_sav(i)
        enddo
      endif

      if (idebug.gt.5) then
         call dotaskx3d('dump gmv STACK_pre_process.gmv/ '//
     >   cmostak(1:icharlnf(cmostak)) //
     >   '/ ; finish',ierr)
      endif

      write(errmsg,'(a)') 'error during post-process setup'

C POST PROCESS - BUFFER and BEAD ALGORITHM
C This needs some clean up

      call cmo_get_info('nnodes',cmo,nnode,ilen,ityp,ierr)
      call cmo_get_info('zic',cmo,ipzic,ilen,ityp,ierr)
      call cmo_get_info('xic',cmo,ipxic,ilen,ityp,ierr)
      call cmo_get_info('yic',cmo,ipyic,ilen,ityp,ierr)
      if(ierr.ne.0) goto 9999
 
      ichange = 0
      iwarn1 = 0
      iwarn2 = 0
 
c     first fill Z array by column
c     npoints are the number of points in a layer
c     ntrunc is the truncating layer
c     nnode are the number of points total in cmo
c     number of vertical points are the number of layers = nbeads
c     nbeads = number of points in columns = nlayer_tot = nlayer final
 
      nbeads = nlayer
c     allocate bead arrays and error check arrays
      call mmgetblk("zin", isubname, ipzin, nbeads, 2, ierr)
      if(ierr.ne.0)call x3d_error(isubname, 'mmgetblk zin')
      call mmgetblk("zout", isubname, ipzout, nbeads, 2, ierr)
      if(ierr.ne.0)call x3d_error(isubname, 'mmgetblk zout')
      call mmgetblk("ibuff", isubname, ipibuff, nbeads, 1, ierr)
      if(ierr.ne.0)call x3d_error(isubname, 'mmgetblk ibuff')
 
      if (gobeads .gt. 0) then
        write(errmsg,'(a)') 'memory error for beads setup'
        call mmgetblk("d_pinch", isubname, ipd_pinch, nbeads, 2, ierr)
        if(ierr.ne.0)call x3d_error(isubname, 'mmgetblk d_pinch')
        call mmgetblk("d_min", isubname, ipd_min, nbeads, 2, ierr)
        if(ierr.ne.0)call x3d_error(isubname, 'mmgetblk d_min')
        call mmgetblk("id_move", isubname, ipid_move, nbeads, 1, ierr)
        if(ierr.ne.0)call x3d_error(isubname, 'mmgetblk id_move')
 
C       fill control arrays for beads_ona_ring() and error checking
        do i = 1, nbeads
           d_pinch(i) = dpinch
           d_min(i) =   dmin
           id_move(i) = iopt_move
        enddo
      endif
 
C   ..............................................................
C   LOOP through each of the bead columns
C   jcol is each bead column, i is the row of beads
 
      write(errmsg,'(a)') 'error during post-process of node columns'
      ichg=0
      ichange = 0
      do jcol = 1, npoints
 
c       fill zin for x,y column point, this will be the bead string
c       xprev and yprev are for error checking on the loop
        icnt=0
        do i = 1, nbeads
          idx = jcol + ((i-1)*npoints)
          if (i.eq.1) then
            xprev = xic(idx)
            yprev = yic(idx)
          endif
          if (xprev.ne.xic(idx)) then
            iwarn2 = iwarn2 + 1
            if (iwarn2.lt.20) then
              write(logmess,'(a,i10,i10,i10)')
     >        'Not valid column(i,jcol) node: ',i,jcol,idx
              call writloga('default',0,logmess,0,ierrw)
            endif
          endif
          if (yprev.ne.yic(idx)) then
            iwarn2 = iwarn2+1
            if (iwarn2.lt.20) then
              write(logmess,'(a,i10,i10,i10)')
     >        'Not valid column(i,jcol) node: ',i,jcol,idx
              call writloga('default',0,logmess,0,ierrw)
            endif
          endif
          zin(i) = zic(idx)
          xprev = xic(idx)
          yprev = yic(idx)
 
c         fill buffer array
          if (iopt_buff.ne.0) then
            idx = jcol + ((i-1)*npoints)
            if (layertyp(idx) .eq.1) icnt = icnt+1
            ibuff(i) = icnt
          endif
        enddo
c       CHECK ALL ELEVATIONS
        if (zin(1).eq.zin(nbeads)) then
          iwarn1 = iwarn1 + 1
          if (iwarn1.lt.20) then
            write(logmess,'(a,i10,i10)')
     >      'warning: Column has 0 height. column, node: ',jcol,idx
            call writloga('default',0,logmess,0,ierrw)
          endif
        endif
 
c   move points on the column for new zic values
c   - allow only one lower buffer point, rest get put on interface
c     this allows the colors between buffer and interface to be correct
c   - use beads_ona_ring to redistribute distances
 
C       REDISTRIBUTE BEADS
C       Fix distances according to bead input options
C       zout is filled with new values from zin
c         d_pinch If interval length d <= d_pinch then set to zero
c         d_min   If interval length is d_pinch < d < d_min set to d_min
c         id_move = 1  Get or put values equally up and down
c                 2  Get or put values up only
c                 3  Get or put values down only
        if (gobeads .gt. 0) then
          write(errmsg,'(a)') 'error during beads_ona_ring post-process'
          call beads_ona_ring(errmsg,
     >       zout,zin,d_pinch,d_min,id_move,nbeads,ierr)
          if (ierr.ne.0) then
            call writloga('default',1,errmsg,0,ierrw)
cdebug      print*,'ZIN: ',(zin(i),i=1,nbeads)
            write(errmsg,'("Fatal Error during post proccessing.")')
            goto 999
          endif
        else
          do i = 1,nbeads
            zout(i)=zin(i)
          enddo
        endif
c       Now zout contains the new zic values
 
c       CHECK BUFFERS
c       may not be needed anymore, use idebug setting to check
c       check that extra buffer points are pushed to intrface
c       and not sitting at the lower buffer (gives wrong color)
c       and check that only buffer points are at buffers
c       because layers are pinched down, only move lower buffer up

        if (iopt_buff.ne.0 .and. idebug.gt.1) then

          write(errmsg,'(a)') 'error during buffer post-process'
          i = 1
          dowhile (i.lt.nbeads)
            ifound = 0
            squished = 0
            idx = jcol + ((i-1)*npoints)
 
cdebug    if (i.eq.1) then
c         print*,jcol,'>',(j,zout(j),ibuff(j),j=1,nbeads)
c         endif
 
c           check to see if this a pile of duplicate points
            if (zout(i).eq.zout(i+1)) squished = 1
            if (squished.gt.0 ) then
              ii = i+1
              znext = zout(ii)
              dowhile (zout(ii).le.zout(i) .and. ii.le.nbeads)
                ii=ii+1
                squished = squished+1
              enddo
              if (zout(ii).gt.zout(i)) then
                znext = zout(ii)
              else
                write(logmess,'(a,i10,i10,e14.6)')
     >          'WARNING: znext for buffer not found ',jcol,i,zout(i)
                call writloga('default',0,logmess,0,ierrw)
              endif
 
c             find a buffer point, move all points after it
c             this should happen only on lower buffers
              j=0
              dowhile (ifound.eq.0 .and. j.lt.squished)
                ii=i+j
                idx = jcol + ((ii-1)*npoints)
                if (layertyp(idx).eq.1) ifound = ii
                j=j+1
              enddo
              if (mod(ibuff(ii),2).eq.0) ifound = 0
            endif
 
c           now move all beads except buffer, up to the interface
            ibot = ifound+1
            itop = i+squished-1
            if (squished.gt.0 .and. ifound.gt.0 .and.
     >           ibot.gt.0  .and. itop.gt.0 ) then
 
              do ii = ibot,itop
                if(idebug.gt.5) then
                  write(logmess,'(i9,i9,a,e14.6,a,e14.6)')
     >            jcol,ii,' change from ',zout(ii),' to ',znext
                  call writloga('default',0,logmess,0,ierr)
                endif
                zout(ii) = znext
                ichg = ichg+1
              enddo
            else
 
            if (squished.gt.1) then
               if(idebug.gt.9) then
                 write(logmess,'(a,i9,i9,i10)')
     >           'Skipped pile of points. ',jcol,i,squished
                 call writloga('default',0,logmess,0,ierr)
               endif
               do j = 0,squished
                 ii=i+j
                 idx = jcol + ((ii-1)*npoints)
               enddo
            endif
            endif
 
            if (squished.le.1) then
              i=i+1
            else
              i=i+squished
            endif
          enddo
c         end dowhile from bottom to top of column
        endif
c       end adjustments of buffers
        if(ichg.gt.0) then
          write(logmess,'(a,i10)')'Buffer node changes = ',ichg
          call writloga('default',0,logmess,0,ierr)
        endif

        write(errmsg,'(a)') 'error during post-process cleanup'
 
c       COPY NEW Z VALUES over old cmo zic
c       CHECK ELEVATIONS - should be monotonic for each column
        if (zout(1).eq.zout(nbeads)) then
          iwarn1 = iwarn1 +1
          if (iwarn1 .lt. 20) then
          write(logmess,'(a,i10)')'Warning: Column has 0 height: ',jcol
          call writloga('default',0,logmess,0,ierrw)
          endif
        endif
 
        do i = 1,nbeads
          idx = jcol + ((i-1)*npoints)
          if(i.gt.1) then
             if (zout(i).lt.zout(i-1)) then
               write(logmess,'(a,i10,1pe13.6,1pe13.6)')
     >         'Node pop: ',idx,zout(i),zout(i-1)
               call writloga('default',0,logmess,0,ierrw)
             endif
          endif
 
          if ( zic(idx) .ne. zout(i)) then
            ichange = ichange + 1
          endif
          zic(idx) = zout(i)
        enddo
 
      enddo
C     END loop through columns
C     ..........................................................
      if(ichange.gt.0) then
        write(logmess,'(a,i10)')'Post process node changes = ',ichange
        call writloga('default',0,logmess,0,ierr)
      endif
      if (iwarn2.gt.0) then
        write(logmess,'(a,i10)')
     > 'Total warnings for invalid columns: ',iwarn2
        call writloga('default',0,logmess,0,ierr)
      endif
      if (iwarn1.gt.0) then
        write(logmess,'(a,i10)')
     > 'Total warnings for flat columns: ',iwarn1
        call writloga('default',0,logmess,0,ierr)
      endif

 
c     END BUFFER and BEAD ALGORITHM
C,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,

      write(errmsg,'(a)') 'error during stack cleanup ' 
 
c     if idone = 1 then layers were successfully merged to cmo
 500  if(ierr.ne.0) write(errmsg,'(a)') 'addmesh merge'
      if(ierr.ne.0) goto 999
      idone = 1
 
 999  call writset('stat','tty','on',ierrw)
      if (idone .eq. 0) then
         write(logmess,9995) nread, nfile
         call writloga('default',1,logmess,0,ierrw)
 9995    format('STACK ERROR: ',i9,' files read out of ',i9)
         ierr = -1
         goto 9999
      endif
 
C,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,
C     Done creating cmo, now do screen summary
 
      call cmo_get_info('nnodes',cmo,nnode,ilen,ityp,ierr)
      call cmo_get_info('nelements',cmo,nelm,ilen,ityp,ierr)
      call cmo_get_info('zic',cmo,ipzic,ilen,ityp,ierr1)
      if (ierr.ne.0 .or. ierr1.ne.0)
     *    write(errmsg,'(a)')'get_info for summary list'
 
      write(logmess,"(a)")
     >'................................................................'
      call writloga('default',1,logmess,0,ierrw)
      write(logmess,"(a)")
     >'         surface name  layer color type llcorner  zic'
      call writloga('default',1,logmess,0,ierrw)
      ibuff2=0
      j=0
      do i = 1, nlayer_tot
       ifile = layerlist(i)
       if (ltyplist(i).ne.2 .and. ltyplist(i).ne.1 ) then
         j=j+1
       endif
 
       idx = 1 + ((i-1)*npoints)
       ii = iclr(j)
       write(logmess,"(a21,1x,i5,i5,i5,1x,i9,a3,1pe13.6)")
     *     ifile(1:icharlnf(ifile)),
     *     i,ii,ltyplist(i), idx ,')  ',zic(idx)
       call writloga('default',0,logmess,0,ierrw)
       if(iopt_remove.ne.0 .and. ibuff2.eq.0 .and.
     *    ltyplist(i).eq.1) j=j+1
 
      enddo
 
      write(logmess,'(a,i10,a,i14)')
     >'Elements per layer: ',ntri,'  stacked total: ',nelm
      call writloga('default',1,logmess,0,ierr1)
      write(logmess,'(a,i10,a,i14)')
     >'Nodes    per layer: ',npoints,'  stacked total: ',nnode
      call writloga('default',0,logmess,0,ierr1)
      if (ntrunc.ne.0) then
         write(logmess,'(a,a,i9)')'Layers truncated by ',
     >   ifile_trunc(1:icharlnf(ifile_trunc)) //' layer ', ntrunc
         call writloga('default',0,logmess,0,ierrw)
      endif

      write(logmess,'(a,i9,a,i9)') 
     >'files read: ',nread,'  from total: ', nfile
      call writloga('default',1,logmess,0,ierrw)
      if (iopt_buff.gt.0 .or. iopt_refine.gt.0) then
         write(logmess,'(a,i9,a,i9,a)')
     >'layers created: ',nlayer-nread,'  from total: ',nlayer
         call writloga('default',0,logmess,0,ierrw)
      endif
      write(logmess,"(a)")
     >'................................................................'
      call writloga('default',1,logmess,1,ierrw)

      call cmo_set_info('nnodes',cmo,nnode,1,1,ierr)
      call cmo_set_info('nelements',cmo,nelm,1,1,ierr)
      ipointi = 1
      ipointj = nnode
      call cmo_set_info('ipointi',cmo,ipointi,1,1,ierr)
               if (ierr.ne.0) call x3d_error(isubname,'set_ipointi')
      call cmo_set_info('ipointj',cmo,ipointj,1,1,ierr)
               if (ierr.ne.0) call x3d_error(isubname,'set_ipointj')
      call cmo_select(cmo,icscode)

      if (ierr.eq.0 .and. ierr1.eq.0 ) write(errmsg,'(a)') ' '

 9999 call mmrelprt(isubname,icscode)
      if(idebug.le.3) call writset('stat','tty','off',ierrw)
      call cmo_exist('def1',ierr)
      if (ierr.eq.0) call cmo_release('def1',icscode)
      call cmo_exist('def2',ierr)
      if (ierr.eq.0) call cmo_release('def2',icscode)
      call cmo_exist('cmonxt',ierr)
      if (ierr.eq.0) call cmo_release('cmonxt',icscode)
      call cmo_exist('cmoprev',ierr)
      if (ierr.eq.0) call cmo_release('cmoprev',icscode)


      call writset('stat','tty','on',ierrw)
      write(logmess,"(a,a)")'stack done. ',errmsg
      call writloga('default',0,logmess,1,ierrw) 

      return
      end
c     END read_trilayers()
 
 
 
C ######################################################################
C     NAME
       subroutine file_exist(ifile, ierr)
C
C     PURPOSE -
C        Check to see if file exist, give warning messege if not.
C
C     INPUT ARGUMENTS -
C
C        ifile  - char string for filename
C        ierr   - int error return
C
C     OUTPUT ARGUMENTS -
C
C        ierr   - error returned (zero if no errors)
C
C     CHANGE HISTORY -
C
C
C ######################################################################
C
       implicit none
 
       character*(*) ifile
       integer ierr, ierr1
       integer icharlnf
       integer ilen
       character*160 logmess
 
C-------- CHECK TO SEE IF THE FILE EXISTS, IF NOT THEN BAIL OUT.
 
          ilen=icharlnf(ifile)
          call fexist(ifile(1:ilen),ierr)
          if(ierr.eq.0) then
             write(logmess,2000) ifile(1:ilen)
 2000        format('ERROR: The file ',a,'  does not exist.')
             call writloga('default',1,logmess,0,ierr1)
             ierr = 1
          else
             ierr = 0
          endif
 
        return
        end
 
C #####################################################################
C     NAME
      subroutine cktrilayer_elev( cmoain, cmobin, pinchout, zmin, ierr)
C
C     PURPOSE -
C         Does error checking on two cmo's
C         Assuming they are tri surfaces
C
C         WARNINGS given if:
C             Elevations of top file fall below bottom file
C             Normals are not pointing up
C
C
C     INPUT ARGUMENTS -
C
C
C     OUTPUT ARGUMENTS -
C
C        ierr   - error returned (zero if no errors)
C                 may be used to set idebug at start
C
C     CHANGE HISTORY -
C
C
C ######################################################################
C
       implicit none
 
C ARGS
       character*(*) cmoain, cmobin
       integer pinchout
       integer ierr
C LOCAL
       integer       i, len,ity,nlower,idebug,
     *               ierr_z,ierr_eq,ierr1,icscode
       integer       icmotyp,lenzic
       real*8        zmin,ztotal
       character*32  isubname
       character*160 logmess
 
      pointer (ipzlist, zlist)
      real*8  zlist(1000000)
 
C CMO 1 Definitions
      integer nodesa
      pointer (ipzica, zica)
      real*8  zica(1000000)
 
C CMO 2 Definitions
      integer nodesb
      pointer (ipzicb, zicb)
      real*8  zicb(1000000)
 
 
C BEGIN
      idebug = ierr
      ierr = 0
      ierr_z = 0
      ierr_eq = 0
      nlower = 0
      isubname = 'cktrilayer_elev'
 
C   Check for character data names for cmoain, cmobin
C     ierr.eq.0 means that the cmo already exists.
      call cmo_exist(cmoain,ierr)
      if(ierr.ne.0) call x3d_error(isubname,cmoain)
      call cmo_exist(cmobin,ierr)
      if(ierr.ne.0) call x3d_error(isubname,cmobin)
      if(ierr.ne.0) goto 999
 
C     cmoain - bottom surface
      call cmo_get_info('nnodes',cmoain,nodesa,len,ity,ierr)
      if(ierr.ne.0) call x3d_error(isubname,cmoain)
      call cmo_get_info('zic',cmoain,ipzica,lenzic,icmotyp,ierr)
      if(ierr.ne.0) call x3d_error(isubname,'get info zic')
 
 
C     cmobin - top surface
      call cmo_get_info('nnodes',cmobin,nodesb,len,ity,ierr)
      if(ierr.ne.0) call x3d_error(isubname,cmobin)
      call cmo_get_info('zic',cmobin,ipzicb,lenzic,icmotyp,ierr)
      if(ierr.ne.0) call x3d_error(isubname,'get info zic')
C
 
      if  (nodesa .ne. nodesb) then
         write(logmess,9010) nodesa, nodesb
 9010    format('ERROR: Total number nodes differ ',i10,' ne ',i10)
         call writloga('default',1,logmess,0,ierr1)
         ierr = ierr + 1
         return
      endif
 
C  get working space
      call mmgetblk('zlist',isubname,ipzlist,nodesa,2,icscode)
      if (icscode.ne.0) then
        call x3d_error('mmgetlbk zlist',isubname)
        ierr = ierr+1
        return
      endif
 
C-----LOOP OVER Z VALUES----------------------------------------
C     CHECK elevations
      do i = 1, nodesa
         ztotal = zica(i) + zmin
 
c------- Elevation of upper layer is equal to lower layer+zmin
         if (ztotal .eq. zicb(i)) then
         ierr_eq = ierr_eq + 1
c         if (idebug.gt.3 .and. ierr_eq .lt.10) then
c            write(logmess,9051) i
c 9051       format('Elevations Equal at node ',i10)
c            call writloga('default',0,logmess,0,ierr1)
c         endif
 
 
c------- Elevation of upper layer falls below lower layer+zmin
         elseif (ztotal .gt. zicb(i)) then
         ierr_z = ierr_z + 1
         if (idebug.gt.0 .and. ierr_z .lt. 10) then
            write(logmess,9050) i, zica(i),zicb(i)
 9050       format('Elevation truncated at node ',i10,
     *       1pe13.6,' above ',1pe13.6)
            call writloga('default',0,logmess,0,ierr1)
         endif
         if (pinchout.ne.0) then
             zicb(i) = zica(i)
             nlower = nlower+1
         endif
 
         endif
 
      enddo
C-----END LOOP over z values----------------------------------------
 
      call mmrelprt(isubname,icscode)
 
 999  if (ierr_eq .ne. 0) then
         write(logmess,9992) ierr_eq, nodesa
 9992    format('Total equal elevations: ',i10,' of ',i10)
         call writloga('default',0,logmess,0,ierr1)
      endif
      if (ierr_z .ne. 0 .and. idebug.gt.0) then
         write(logmess,9991) ierr_z, nodesa
 9991    format('Total elevations truncated: ',i10,' of ',i10)
         call writloga('default',0,logmess,0,ierr1)
      endif
      if (pinchout.ne.0 .and. ierr_z.ne.0 .and. idebug.gt.0) then
         write(logmess,9993) nlower, nodesa
 9993    format('Total elevations made equal: ',i10,' of ',i10)
         call writloga('default',0,logmess,1,ierr1)
         write(logmess,9994) zmin
 9994    format('Layers pinched at layer plus: ',1pe15.7)
         call writloga('default',0,logmess,0,ierr1)
      elseif (pinchout.eq.0 .and. idebug.gt.0 ) then
         write(logmess,'(a)') 'Elevations unchanged.'
         call writloga('default',0,logmess,0,ierr1)
      endif
 
      ierr = ierr + ierr_eq + ierr_z
 
      return
      end
 
C #####################################################################
C     NAME
      subroutine cktrilayer_norm( cmoin, debug, flip, ierr)
C
C     PURPOSE -
C         Does error checking on cmo
C         Assuming they are tri surfaces
C
C         WARNINGS given if:
C             Normals are not pointing up
C
C
C     INPUT ARGUMENTS -
C
C
C     OUTPUT ARGUMENTS -
C
C        ierr   - error returned (zero if no errors)
C
C     CHANGE HISTORY -
C
C
C ######################################################################
C
       implicit none
 
C ARGS
       character*(*) cmoin
       integer debug, flip, ierr
C LOCAL
       integer       i, i1,i2,i3, len,ity,
     *               ierr1,ierr2,ics
       integer       icmotyp,lenzic
       real*8        znormal
       character*32  isubname
       character*160 logmess
 
C CMO 1 Definitions
      integer nodesa
      integer nelm
      pointer (ipiteta, iteta)
      pointer (ipitetoffa, itetoffa)
      pointer (ipxica, xica)
      pointer (ipyica, yica)
      pointer (ipzica, zica)
      integer  iteta(1000000)
      integer  itetoffa(1000000)
      real*8  xica(1000000)
      real*8  yica(1000000)
      real*8  zica(1000000)
 
 
C BEGIN
      ierr = 0
      ierr1 = 0
      ierr2 = 0
      isubname = 'cktrilayer_norm'
 
C   Check for character data names for cmoin, cmobin
C     ierr.eq.0 means that the cmo already exists.
      call cmo_exist(cmoin,ierr)
      if(ierr.ne.0) call x3d_error(isubname,cmoin)
      if(ierr.ne.0) goto 999
 
C     cmoin - bottom surface
      call cmo_get_info('nnodes',cmoin,nodesa,len,ity,ierr)
      if(ierr.ne.0) call x3d_error(isubname,cmoin)
      call cmo_get_info('nelements',cmoin,nelm,len,ity,ierr)
      if(ierr.ne.0) call x3d_error(isubname,cmoin)
      call cmo_get_info('xic',cmoin,ipxica,lenzic,icmotyp,ierr)
      if(ierr.ne.0) call x3d_error(isubname,'get info xic')
      call cmo_get_info('yic',cmoin,ipyica,lenzic,icmotyp,ierr)
      if(ierr.ne.0) call x3d_error(isubname,'get info yic')
      call cmo_get_info('zic',cmoin,ipzica,lenzic,icmotyp,ierr)
      if(ierr.ne.0) call x3d_error(isubname,'get info zic')
 
      call cmo_get_info('itet',cmoin,ipiteta,lenzic,icmotyp,ierr)
      if(ierr.ne.0) call x3d_error(isubname,'get info itet')
      call cmo_get_info('itetoff',cmoin,ipitetoffa,lenzic,icmotyp,ierr)
      if(ierr.ne.0) call x3d_error(isubname,'get info itetoff')
 
C-----LOOP OVER TRI(or QUAD) elements
C     CHECK normals, 0 vertical, >0 up, <0 down
c     N dot Zhat = (x1-x2)(y3-y2) - (x3-x2)(y1-y2)
      do i = 1, nelm
         i1 = iteta(itetoffa(i)+1)
         i2 = iteta(itetoffa(i)+2)
         i3 = iteta(itetoffa(i)+3)
 
         znormal = -( (xica(i1) -xica(i2)) *
     >                (yica(i3) -yica(i2)) -
     >                (xica(i3) -xica(i2)) *
     >                (yica(i1) -yica(i2)) )
 
         if(znormal .eq. 0.0) then
           ierr1 = ierr1 + 1
           if (debug.ne.0) then
            write(logmess,1000) i
1000        format('Element: ',i10,' has vertical normal')
            call writloga('default',0,logmess,0,ics)
           endif
 
         elseif(znormal .lt. 0.0) then
           ierr2 = ierr2 + 1
           if (debug.ne.0) then
            write(logmess,1002) i
1002        format('Element: ',i10,' has downward normal')
            call writloga('default',0,logmess,0,ics)
           endif
 
           if (flip .ne. 0) then
              iteta(itetoffa(i)+2) = i3
              iteta(itetoffa(i)+3) = i2
           endif
         endif
 
      enddo
C-----END LOOP over elements
 
 
 999    if (ierr2 .ne. 0) then
 
        if (flip .ne. 0 ) then
          write(logmess,9990) ierr2, nelm
9990    format(i10,' of ',i10,' downward elements flipped up')
          call writloga('default',0,logmess,0,ics)
 
        else
          write(logmess,9992) ierr2,nelm
9992      format(i10,' of ',i10,' down elements in layer')
          call writloga('default',0,logmess,0,ics)
        endif
       endif
      if (ierr2.ne.0 .and. ierr2.ne.nelm) then
      write(logmess,9993) ierr2,nelm
9993  format(i10,' of ',i10,' elem differ in norm direction.')
      call writloga('default',0,logmess,0,ics)
      endif
 
      if (ierr1.ne.0) then
         write(logmess,9995) ierr1,nelm
9995     format(i10,' of ',i10,' vertical elements in layer')
         call writloga('default',0,logmess,0,ics)
      endif
 
      return
      end
 
c
C#####################################################################
c
      subroutine trilayer_subset(cmonam,xmin,ymin,xmax,ymax,ierr)
C
C     PURPOSE -
C
C     INPUT ARGUMENTS -
C
C
C     OUTPUT ARGUMENTS -
C
C        ierr   - error returned (zero if no errors)
C
C     CHANGE HISTORY -
C
C
C#####################################################################
C
      implicit none
c
C ARGS
      character*(*) cmonam
      real*8  xmin,ymin,xmax,ymax
      integer ierr, ierrw, idebug
C LOCAL
      character*32  isubname
      character*160 logmess
      character*360 cbuf
      integer       ilen,ityp,ics
      real*8        xmin2,ymin2,xmax2,ymax2
      real*8        zmin,zmax
 
C CMO Definitions
      integer nnodes,nelem, npoints, numtet
 
 
C BEGIN
      ierr = 0
      idebug = 0
      isubname = 'trilayer_subset'
 
C   Check for exist cmonam
C     ierr.eq.0 means that the cmo already exists.
      call cmo_exist(cmonam,ierr)
      if(ierr.ne.0) call x3d_error(isubname,cmonam)
      if(ierr.ne.0) goto 999
 
      call cmo_get_info('nnodes',cmonam,nnodes,ilen,ityp,ierr)
      if(ierr.ne.0) call x3d_error(isubname,cmonam)
      call cmo_get_info('nelements',cmonam,nelem,ilen,ityp,ierr)
      if(ierr.ne.0) call x3d_error(isubname,cmonam)
      call cmo_get_intinfo('idebug',cmonam,idebug,ilen,ityp,ierr)
 
c     get the z extents, extend beyond min max
      call cmo_get_minmax(cmonam,xmin2,ymin2,zmin,
     >                     xmax2,ymax2,zmax,ierr)
      zmin = zmin - 100.
      zmax = zmax + 100.
 
c     TURN OFF OUTPUT
      if (idebug.le.3) call writset('stat','tty','off',ierrw)
 
      write(cbuf,50) xmin,ymin,zmin,xmax,ymax,zmax
 50   format('pset/sub/geom/xyz/1 0 0/',3(1x,1pe13.6),
     >        3(1x,1pe13.6),' ; finish')
      call dotaskx3d(cbuf,ierr)
      if(ierr.ne.0)  goto 999
 
      cbuf = 'pset/duds/not/sub/ ; finish'
      call dotaskx3d(cbuf,ierr)
      if(ierr.ne.0)  goto 999
 
      cbuf = 'rmpoint/ pset,get,duds/inclusive ; finish'
      call dotaskx3d(cbuf,ierr)
      if(ierr.ne.0)call x3d_error(isubname,'subset rmpoint')
      cbuf = 'rmpoint/compress/ ; finish'
      call dotaskx3d(cbuf,ierr)
      if(ierr.ne.0)call x3d_error(isubname,'subset compress')
      if(ierr.ne.0)  goto 999
 
 
      call cmo_newlen(cmonam,ierr)
      call cmo_get_info('nnodes',cmonam,npoints,ilen,ityp,ierr)
      if(ierr.ne.0) call x3d_error(isubname,cmonam)
      call cmo_get_info('nelements',cmonam,numtet,ilen,ityp,ierr)
      if(ierr.ne.0) call x3d_error(isubname,cmonam)
      if (npoints .le. 0) then
         call x3d_error(isubname,'No points in set.')
         goto 999
      endif
      if (numtet .le. 0) then
         call x3d_error(isubname,'No elements in set.')
         goto 999
      endif

      cbuf = 'pset/duds/delete/ ; finish '
      call dotaskx3d(cbuf,ierr)
      if(ierr.ne.0)  goto 999
      cbuf = 'pset/sub/delete/ ; finish '

 
 
c     TURN ON OUTPUT
      if(idebug.gt.3) call writset('stat','tty','on',ierrw)
 
      call cmo_set_info('nnodes',cmonam,npoints,1,1,ierr)
      call cmo_set_info('nelements',cmonam,numtet,1,1,ierr)
      call cmo_newlen(cmonam,ierr)
 
      if (npoints.le.1 .or. numtet.le.0) then
        write(logmess,'(a,i5)') 'Subset too small: ',npoints
        call writloga('default',0,logmess,0,ics)
        ierr=1
      endif
 
 
 999  if (ierr .ne. 0) then
        write(logmess,9992) ierr
9992    format('ERROR: ',i10,' during trilayer subset')
        call writloga('default',0,logmess,0,ics)
      elseif (idebug.gt.0) then
        write(logmess,9993) npoints, numtet
9993    format('Subset Created: ',i10,' nodes ',i10,' elements')
        call writloga('default',0,logmess,0,ics)
        write(logmess,9995) nnodes, nelem
9995    format('From Original:  ',i10,' nodes ',i10,' elements')
        call writloga('default',0,logmess,0,ics)
      endif
 
      return
      end
