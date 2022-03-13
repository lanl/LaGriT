
C**********************************************************************
C HDR
C   NAME:
       subroutine read_sheetij(imsgin,xmsgin,cmsgin,msgtype,nwds,ierr)
C
C   PURPOSE
c       Create a quad mesh cmo from an elevation file using syntax similar
C       to avs type .fld header info.
C       Binary files read sequentially into elevation array
C         of nx x ny values long.
C
C
C   SYNTAX:
C      read/sheetij/ file /nx,ny/minx,miny/dx,dy [toggle options]
C      read/sheetij/ file /nx,ny/minx,miny/dx,dy/ skip n / [toggle options]
C      read/sheetij/ file /nx,ny/minx,miny/dx,dy/istart,jstart,istop,jstop/[ops]
C      read/sheetij/ file /-header-/ [options]
C
C      file                 contains elevations at x,y
C      nx                   columns in x direction
C      ny                   rows in y direction
C      minx,  miny          min x,y values-location of lower left corner
C      dx,  dy              xinc and yinc are cell size in x and y direction 
C
C      -header-             after filename replaces nx,ny,minx,miny,dx,dy
c                           these values are at top of data file (ascii only)
C      skip n               skips n number of lines at top of file 
C      istart, istop,       start and stop gives a subset by
C      jstart, jstop        counting number of i,j's
C
C    OPTIONS:
C        ascii or binary      file type
C                             (def=ascii)
C        center               assume elevation located at cell center
C                             (def=lower left corner)
C        connect or points    connect points into quad grid, or keep as points
C                             (def=connect)
C        float or double      size of number
C                             (def=float)
C        xflip, yflip         reflect along x or y axis
C                             (def=xflip=0, yflip=0)
C        examples:
C
C        cmo create cmo1
C        read sheetij pre-qbog100 /90,60 /0.0,0.0 / 100.0,100.0 / binary
C
C        read sheetij topo50_bin /247,192/0.0,0.0/50.0,50.0/ yflip, binary
C
C        read sheetij 2a_sheet2_hdr  / -header- / ascii
c        2a_sheet2_hdr description =  /90,60 / 1630000, 1772000 / 100 100 
C                                  = /nx,ny  /minx,miny         /dx,dy
c       _________________________
c      |#   90
c      |#   60
c      |#   1630000
c      |#   1772000
c      |#   100
c      |#   100
C      |#
C
C   INPUT/OUTPUT
C         ifile  - file of Z values, (binary, formatted or unformatted)
C         ierr   - INTEGER ERROR FLAG (==0 ==> OK, <>0 ==> AN ERROR)
C
C   COMMENTS
C         ? marks code not yet implemented
C
C
C   CHANGE HISTORY
C
C     Initial version  07/31/96       tcherry
C
C
C        $Log: read_sheetij.f,v $
C        Revision 2.00  2007/11/09 20:03:59  spchu
C        Import to CVS
C
CPVCS    
CPVCS       Rev 1.7   18 Oct 2005 16:06:52   gable
CPVCS    Extend input file name length to 132 characters.
CPVCS    
CPVCS       Rev 1.6   30 Nov 2004 09:58:38   tam
CPVCS    added option to skip n header lines of input file
CPVCS    fixed some error checking and code comments
CPVCS    
CPVCS       Rev 1.5   01 Dec 2003 14:16:40   gable
CPVCS    Variable hdrlen was not initialized. Caused problems on SGI.
CPVCS    Initialize to zero so files with no header are read correctly.
CPVCS    
CPVCS       Rev 1.4   26 Nov 2003 10:54:30   gable
CPVCS    Adding header info.
C**********************************************************************
CCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCC
c
      implicit none
c
c
c ARGS
      integer nwds, ierr, ierrw
      integer imsgin(nwds), msgtype(nwds)
      real*8 xmsgin(nwds)
      character*(*) cmsgin(nwds)
C
      integer imsgout(40), msgout(40)
      real*8 xmsgout(40)
      character*(32) cmsgout(40)

      integer      filetype
      integer      vartype
      integer      center
      integer      connect
      integer      idebug
      integer      nx
      integer      ny
      integer      istart, istop
      integer      jstart, jstop
      integer      xflip, yflip
      real*8       minx, miny, zmin
      real*8       maxx, maxy, zmax
      real*8       xinc, yinc
      real*8       x1,y1,z1
      real*8       x2,y2,z2
      real*8       x3,y3,z3
      real*8       x4,y4,z4
      character*132 ifile
      character*32 cmotype

C MESH_OBJECT POINTERS.
C
      integer nsd, nsdgeom, nsdtopo, nen, nef
      real*8  zic(1000000)
      pointer (ipzic, zic)
      real*8  elev(1000000)
      pointer (ipelev, elev)

c LOCAL VARS
      character*132 cbuf
      character*132 logmess
      character*32  cmonam
      character*32  isubname
      character*32  ctmp
      character*8   cglobal, cdefault

      integer       icharlnf
      integer
     >        i, j
     >,       nxquad, nyquad
     >,       nzic, nelev
     >,       ilen
     >,       hdrlen
     >,       nnodes
     >,       iout
     >,       itype
     >,       idone


C
c BEGIN

      idone = 0
      isubname='read_sheetij'
      cglobal='global'
      cdefault='default'
      call cmo_get_name(cmonam,ierr)
      if(ierr .ne. 0) call x3d_error('Bad cmo ',cmonam)

C  ASSIGN PARAM VALUES
C     ftype
C      ascii   = 1(def),   binary  = 0, ?fortran binary = 2
C     vartype
C      float   = 0(def),   double  = 1, ?integer = 2, long = 3
C   ARGUMENTS:
c       1) "read" 2) "sheetij"
C       3) filename                 ==> elevations at x,y
C       [-header-] or [nx,ny,minx,miny,dx,dy]
C       [binary or ascii, double or float, connect or points]

       filetype = 1
       vartype  = 0
       center   = 0
       connect  = 1
       xflip    = 0
       yflip    = 0
       idebug    = 0
       hdrlen = 0

       i = 3
       if (cmsgin(1)(1:11) .eq. 'readsheetij') i = 2

       ifile = cmsgin(i)
       i = i+1

c      arguments /nx,ny/minx,miny/
       if (msgtype(i) .eq. 1) then
         nx = imsgin(i)
         i = i+1
         ny = imsgin(i)
         i = i+1
         minx = xmsgin(i)
         i = i+1
         miny = xmsgin(i)
         i = i+1
         xinc = xmsgin(i)
         i = i+1
         yinc = xmsgin(i)
         i = i+1
       endif

c      check for header 
       if (msgtype(i).eq.3) then

c        read header for arguments
         if ( cmsgin(i)(1:2).eq.'-h' ) then
           call sheetij_hdr(
     >          ifile,nx,ny,minx,miny,xinc,yinc,hdrlen,ierr)
           i = i+1
         elseif ( cmsgin(i)(1:4).eq.'skip' ) then
           i = i + 1
           hdrlen = imsgin(i)
           write(logmess,'(a,i10)')'Skipping Lines: ',hdrlen
           call writloga('default',0,logmess,0,ierrw)
           i = i+1
         endif

       endif

c      check for subset /istart,jstart,istop,jstop/
       if (msgtype(i) .eq. 1) then
         istart = imsgin(i)
         i = i+1
         istop = imsgin(i)
         i = i+1
         jstart = imsgin(i)
         i = i+1
         jstop = imsgin(i)
         i = i+1
         nxquad = istop - istart + 1
         nyquad = jstop - jstart + 1
         minx = minx + ((istart-1)*xinc)
         miny = miny + ((jstart-1)*yinc)
       else
         istart = 1
         jstart = 1
         istop = nx
         jstop = ny
         nxquad   = nx
         nyquad   = ny
       endif

c toggles come after main setup arguments in any order
       do i = i, nwds
         ctmp = cmsgin(i)
         if (ctmp(1:3) .eq. 'con') connect = 1
         if (ctmp(1:3) .eq. 'poi') connect = 0
         if (ctmp(1:3) .eq. 'bin') filetype = 0
         if (ctmp(1:3) .eq. 'asc') filetype = 1
         if (ctmp(1:3) .eq. 'for') filetype = 1
         if (ctmp(1:3) .eq. 'unf') filetype = 2
         if (ctmp(1:3) .eq. 'flo') vartype = 0
         if (ctmp(1:3) .eq. 'dou') vartype = 1
         if (ctmp(1:3) .eq. 'xfl') xflip = 1
         if (ctmp(1:3) .eq. 'yfl') yflip = 1
         if (ctmp(1:3) .eq. 'deb') idebug = 1
       enddo

C done reading parsed input arguments

       maxx = minx + ((nxquad-1)*xinc)
       maxy = miny + ((nyquad-1)*yinc)
       x1 = minx
       y1 = miny
       z1 = 0.0
       x2 = maxx
       y2 = miny
       z2 = 0.0
       x3 = maxx
       y3 = maxy
       z3 = 0.0
       x4 = minx
       y4 = maxy
       z4 = 0.0
       cmotype='quad'
       nsd=3
       nsdgeom=3
       nsdtopo=2
       nen=4
       nef=4


c CREATE QUADS  using quadxy command

       i=1
       cmsgout(i) = 'quadxy'
       i=i+1
       imsgout(i) = nxquad
       i=i+1
       imsgout(i) = nyquad
       i=i+1
       xmsgout(i) = x1
       i=i+1
       xmsgout(i) = y1
       i=i+1
       xmsgout(i) = z1
       i=i+1
       xmsgout(i) = x2
       i=i+1
       xmsgout(i) = y2
       i=i+1
       xmsgout(i) = z2
       i=i+1
       xmsgout(i) = x3
       i=i+1
       xmsgout(i) = y3
       i=i+1
       xmsgout(i) = z3
       i=i+1
       xmsgout(i) = x4
       i=i+1
       xmsgout(i) = y4
       i=i+1
       xmsgout(i) = z4

      call quadxy(imsgout,xmsgout,cmsgout,msgout,i,ierr)
      if(ierr .ne. 0)call x3d_error(isubname,'quadxy')
      if(ierr.ne.0) goto 9999

c READ ascii or binary Z values
      nnodes = nxquad*nyquad
      call cmo_get_info('nnodes',cmonam, iout,ilen,itype,ierr)
      if (iout.ne.nnodes) then
         write(logmess,8880) iout, nnodes
 8880    format('<WARNING> quadxy pts = ',i8,' Expected: ',i8)
         call writloga('default',1,logmess,0,ierrw)
      endif
      call read_zfile(ifile, nx,ny, filetype, vartype,
     >     istart, istop, jstart, jstop, hdrlen, idebug, ierr)
      if(ierr .ne. 0)call x3d_error(isubname,'read_zfile')
      if(ierr.ne.0) goto 9999
     
C CREATE attribute elev to save Z values read from file
c        Get the pointer to the attribute
      cbuf = 'cmo/addatt/' //
     >        cmonam(1:icharlnf(cmonam)) //
     >       '/elev/vdouble/scalar/nnodes/linear/permanent/' //
     >       'ag/0.0/' //
     >       ' ; finish '
      call dotaskx3d(cbuf,ierr)
      if(ierr .ne. 0)call x3d_error(isubname,'addatt elev')
      if(ierr.ne.0) goto 9999
      call cmo_newlen(cmonam, ierr)

      call mmfindbk('elev', cmonam, ipelev, ilen, ierr)
      if(ierr .ne. 0)call x3d_error(isubname,'mmfindbk elev')
      if(ierr.ne.0) goto 9999
      call cmo_get_info('zic',cmonam, ipzic,ilen,itype,ierr)
      if (ierr .ne. 0)write(6,*)cmonam, ierr,' ierr value  get zic'

      do i = 1, nnodes
        elev(i) = zic(i)
      enddo

      if (connect .ne. 0) then
        call connectij_surf(istop-istart+1,jstop-jstart+1, ierr)
        if(ierr .ne. 0) call x3d_error(isubname,'connectij surface')
        if (ierr .ne. 0) goto 9999
      endif
      call cmo_get_info('nnodes',cmonam, iout,ilen,itype,ierr)
      if (iout.ne.nnodes) then
         write(logmess,9990) iout, nnodes
 9990    format('<WARNING> rzbrick pts = ',i8,' Expected: ',i8)
         call writloga('default',1,logmess,0,ierrw)
      else
         write(logmess,1015) nnodes
 1015    format('  CONNECTED QUAD POINTS  1 TO ',i8)
         call writloga('default',1,logmess,0,ierrw)
      endif

C FLIP IMAGE and FIND Z MAX,MIN
      call cmo_get_info('elev',cmonam, ipelev,ilen,itype,ierr)
      if (ierr .ne. 0)write(6,*)cmonam, ierr,' ierr value  get elev'
      call cmo_get_info('zic',cmonam, ipzic,ilen,itype,ierr)
      if (ierr .ne. 0)write(6,*)cmonam, ierr,' ierr value  get zic'

      zmin = 0.10000000e+31
      zmax = 0.10000000e-31
      nzic = 0

C FLIP X
      if (xflip.eq.1 .and. yflip.eq.0) then
         do j = 1, nyquad
         do i = nxquad, 1, -1
           nelev = ((j-1)* nxquad) + i
           nzic = nzic + 1
           zic(nzic) = elev(nelev)
cebug      print*,i,j,' zic: ',nzic,' nelev: ',nelev,' = ',elev(nelev)
         enddo
         enddo
         do i = 1, nnodes
         elev(i) = zic(i)
         if ((zic(i).gt.zmax).and.(zic(i).lt. 0.1e+31)) zmax = zic(i)
         if ((zic(i).lt.zmin).and.(zic(i).gt. 0.1e-31)) zmin = zic(i)
         enddo

C FLIP Y
      elseif (yflip.eq.1 .and. xflip.eq.0) then
         do j = nyquad, 1, -1
         do i = 1, nxquad
           nelev = ((j-1)* nxquad) + i
           nzic = nzic + 1
           zic(nzic) = elev(nelev)
cebug      print*,i,j,' zic: ',nzic,' nelev: ',nelev,' = ',elev(nelev)
         enddo
         enddo
         do i = 1, nnodes
         elev(i) = zic(i)
         if ((zic(i).gt.zmax).and.(zic(i).lt. 0.1e+31)) zmax = zic(i)
         if ((zic(i).lt.zmin).and.(zic(i).gt. 0.1e-31)) zmin = zic(i)
         enddo
C FLIP Y
      elseif (yflip.eq.1 .and. xflip.eq.0) then
         do j = nyquad, 1, -1
         do i = 1, nxquad
           nelev = ((j-1)* nxquad) + i
           nzic = nzic + 1
           zic(nzic) = elev(nelev)
cebug      print*,i,j,' zic: ',nzic,' nelev: ',nelev,' = ',elev(nelev)
         enddo
         enddo
         do i = 1, nnodes
         elev(i) = zic(i)
         if ((zic(i).gt.zmax).and.(zic(i).lt. 0.1e+31)) zmax = zic(i)
         if ((zic(i).lt.zmin).and.(zic(i).gt. 0.1e-31)) zmin = zic(i)
         enddo

C FLIP X,Y
      elseif (xflip.eq.1 .and. yflip.eq.1) then
         do j = nyquad, 1, -1
         do i = nxquad, 1, -1
           nelev = ((j-1)* nxquad) + i
           nzic = nzic + 1
           zic(nzic) = elev(nelev)
cebug       print*,i,j,' zic: ',nzic,' nelev: ',nelev,' = ',elev(nelev)
         enddo
         enddo
         do i = 1, nnodes
         elev(i) = zic(i)
         if ((zic(i).gt.zmax).and.(zic(i).lt. 0.1e+31)) zmax = zic(i)
         if ((zic(i).lt.zmin).and.(zic(i).gt. 0.1e-31)) zmin = zic(i)
         enddo

      else
         do i = 1, nnodes
         if ((zic(i).gt.zmax).and.(zic(i).lt. 0.1e+31)) zmax = zic(i)
         if ((zic(i).lt.zmin).and.(zic(i).gt. 0.1e-31)) zmin = zic(i)
         enddo

      endif



 


      idone = 1
 9999 if (idone .eq. 0) then
         write(logmess,9991) ifile(1:icharlnf(ifile))
 9991    format('<<ERROR>> creating sheet ij from file ',a,'.')
         call writloga('default',1,logmess,0,ierrw)
      else
         write(logmess,9995)
     >   ifile(1:icharlnf(ifile)),nnodes,nxquad,nyquad
 9995    format('SHEETIJ: ',a,', quad points: ',i9,'(',i9,',',i9,')')
         call writloga('default',1,logmess,0,ierrw)

         write(logmess,9996) minx,miny,zmin
 9996    format('SHEETIJ: MIN X) ',e12.6,' Y) ',e12.6,' Z) ',e12.6 )
         call writloga('default',1,logmess,0,ierrw)

         write(logmess,9997) maxx,maxy,zmax
 9997    format('         MAX X) ',e12.6,' Y) ',e12.6,' Z) ',e12.6 )
         call writloga('default',1,logmess,0,ierrw)
         ierr = 0
      endif


      return
      end
c     END read_sheetij


CCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCC
C NAME:
       subroutine sheetij_hdr(ifile,
     >            nx, ny, minx, miny, dx, dy, hdrlen,  ierr)
c
c purpose
c      read the header at top of the file for sheetij 
c      currently only the first 6 lines are read
c      containing nx, ny, minx, miny, dx, dy
c
CCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCC

       implicit none

c ARGS
       character*(*) ifile
       integer       nx,ny
       real*8        minx,miny
       real*8        dx,dy
       integer       hdrlen
       integer       ierr, ierrw
C LOCAL
       character*1   cmarker
       character*32  cdummy,isubname
       character*132 logmess
       integer       lenfile
       integer       icharlnf
       integer       idone
       integer       icount
       integer       iunit
    
       integer*4 iunit4

C BEGIN
       idone = 0
       hdrlen = 0
       icount = 1
       isubname = "sheetij_hdr" 

       lenfile=icharlnf(ifile)
       call fexist(ifile(1:lenfile),ierr)
       if(ierr.eq.0) then
         write(logmess,3000) ifile(1:lenfile)
 3000    format('The Z file, ',a,', does not exist.')
         call writloga('default',1,logmess,0,ierrw)
         goto 999
       endif

       iunit=-1
       call hassign(iunit,ifile,ierr)
       if (iunit.lt.0 .or. ierr.lt.0) then
         call x3d_error(isubname,'hassign bad file unit')
         goto 999
       endif
       iunit4 = iunit

100    continue
       read (iunit,'(a1)',err=999) cmarker
       if (cmarker .eq. '#') then
         icount = icount +1
         go to 100
       endif
       hdrlen = icount-1
       if (hdrlen .gt. 0) then
         icount = 1
         rewind(unit=iunit,err=999)
         read(iunit,3001,err=999) cmarker, nx
         icount = icount + 1
         read(iunit,3001,err=999) cmarker, ny
         icount = icount + 1
         read(iunit,3002,err=999) cmarker, minx
         icount = icount + 1
         read(iunit,3002,err=999) cmarker, miny 
         icount = icount + 1
         read(iunit,3002,err=999) cmarker, dx
         icount = icount + 1
         read(iunit,3002,err=999) cmarker, dy
         icount = icount + 1
ccg 3001    format(a1, i8)
 3001    format(a1, i5)
 3002    format(a1, f15.5)
       endif

       idone = 1
       ierr = 0
       close(iunit)

 999   return
       end

c      END subroutine sheetij_hdr

CCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCC
C NAME:
      subroutine read_zfile(ifile, nx,ny, ftype, vartype,
     >   istart, istop, jstart, jstop,  num_skip, idebug, ierr)
C
C     ftype
C      ascii   = 0(def),   binary  = 1, ?fortran binary = 2
C     vartype
C      float   = 0(def),   double  = 1, ?integer = 2, ?long = 3
C
CCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCC
c
      implicit none
c
c
c ARGS
      character*(*) ifile
      integer
     >   nx,ny
     >,  ftype
     >,  vartype
     >,  istart, istop
     >,  jstart, jstop
     >,  num_skip
     >,  idebug
     >,  ierr,ierrw
c
c CMO
      real*8  zic(1000000)
      pointer (ipzic, zic)

c
c LOCAL
      character*132 logmess
      character*32  isubname
      character*32  cmonam

      integer       icharlnf
      integer
     >  i, ii
     >, irow, icol
     >, itotal
     >, icount
     >, ilen
     >, zlen
     >, iunit
     >, lenfile
     >, itype,ibytes
     >, idone

c     real*8    ztmp
      real*8    work2d
      real*8    work1d
      pointer (ipwork2d, work2d)
      pointer (ipwork1d, work1d)
      dimension work2d(nx,ny)
      dimension work1d(nx*ny)
C BEGIN

      idone = 0
      i = 0
      itotal = 0
      isubname='read_zfile'
      call cmo_get_name(cmonam,ierr)
      if(ierr .ne. 0)write(6,*)ierr,' ierr value cmonam'
      zlen = nx*ny

      if (idebug.ne.0) then
      print*,'nx, ny, num_skip: ', nx,ny, num_skip
      print*,'istart,istop, jstart,jstop: ',istart,istop, jstart,jstop
      endif

C
C CHECK TO SEE IF THE Z FILE EXISTS, IF NOT THEN BAIL OUT.
      lenfile=icharlnf(ifile)
      call fexist(ifile(1:lenfile),ierr)
      if(ierr.eq.0) then
         write(logmess,2000) ifile(1:lenfile)
 2000    format('The Z file, ',a,', does not exist.')
         call writloga('default',1,logmess,0,i,ierrw)
         goto 999
      endif

      call cmo_get_info('zic',cmonam, ipzic,ilen,itype,ierr)
      if (ierr.ne.0) call x3d_error(isubname,'get_info zic')
C READ BINARY file ----------------------------------------------------
      if (ftype .eq. 0) then

        if (vartype .eq. 0 .or. vartype .eq. 1) then
c       0=float=default 1=double
            ibytes = zlen*4
            if (vartype.eq.1) ibytes = zlen*8
            call read_binaryij(ifile, vartype, zic, zlen,
     >           istart, istop, jstart, jstop, nx, itotal)
         write(logmess,'(i10,a,a)') itotal,' bytes read from ',
     >           ifile(1:icharlnf(ifile))
         call writloga('default',0,logmess,0,ierrw)

        else
         write(logmess,2001) ifile(1:lenfile)
 2001    format('Var type for file, ',a,', must be float or double.')
         call writloga('default',1,logmess,0,ierrw)
         goto 999
        endif
        if(itotal.ne.ibytes) then 
          write(logmess,'(i10,a,i10)')itotal,' read out of ',ibytes
          call writloga('default',0,logmess,0,ierrw)
        endif
        if (itotal.le.0) goto 999
        i = zlen

C READ ASCII file-------------------------------------------------------
      elseif (ftype .eq. 1) then
        iunit=-1
        call hassign(iunit,ifile,ierr)
        if (iunit.lt.0 .or. ierr.lt.0) then
          call x3d_error(isubname,'hassign bad file unit')
          goto 999
        endif

c       ALLOCATE array
        call mmgetblk("work2d",isubname,ipwork2d,zlen,2,ierr)
        if (ierr.ne.0) call x3d_error(isubname,'mmgetblk work2d')
        call mmgetblk("work1d",isubname,ipwork1d,zlen,2,ierr)
        if (ierr.ne.0) call x3d_error(isubname,'mmgetblk work1d')

c       SKIP  HEADER
        if (num_skip .gt. 0)then
        do i = 1, num_skip
c cg          read(iunit,'(10e18.8)',err=999)
          read(iunit,*,err=999)
        enddo
        endif

        read(iunit,*,err=999)
     >              ((work2d(icol,irow),
     >                icol = 1, nx),
     >                irow = 1, ny)
c cg 20              format (10e18.8)

C       FILL Z COORDINATE
        itotal = 0
        if (istart.eq.1 .and. istop.eq.nx .and.
     >      jstart.eq.1 .and. jstop.eq.ny ) then
          do irow = 1, ny
          do icol = 1, nx
             itotal = itotal + 1
             zic(itotal) = work2d(icol,irow)
             if (idebug.ne.0) print*,
     >       'work2d[',icol,',',irow,'] = ',work2d(icol,irow)
          enddo
          enddo
          i = itotal

        else

          i = 0
          do irow = 1, ny
          do icol = 1, nx
             i = i + 1
             work1d(i) = work2d(icol,irow)
          enddo
          enddo
          do i = 1, zlen
            icount = ((i-1)/nx)+1
            if ( icount.ge.jstart .and. icount.le.jstop ) then
               ii = mod(i,nx)
               if (ii .eq. 0) ii = nx
               if ( ii.ge.istart .and. ii.le.istop) then
               itotal = itotal + 1
               zic(itotal) = work1d(i)
               if (idebug.ne.0) print*,'zic[',itotal,'] = ',work1d(i)
               endif
            endif
          enddo
          i = i-1
        endif

        call mmrelprt(isubname,ierr)
        close(iunit)

C UNKNOWN FORMAT --------------------------------------------------
      else
         write(logmess,2005) ifile(1:lenfile)
 2005    format('The format for file, ',a,', is not implemented.')
         call writloga('default',1,logmess,0,ierrw)
         goto 999

      endif
C DONE  READING --------------------------------------------------

      idone = 1
 999  if (idone .eq. 0) then
        write(logmess,9991) ifile(1:lenfile), itotal
 9991   format('<WARNING> Problem reading Z values from ',a,i9,' read.')
        call writloga('default',1,logmess,0,ierrw)
    

      else
        write(logmess,9992) i, ifile(1:lenfile),itotal
 9992   format('READ: ',i9,' Z values from ',a,' Saved: ',i9)
        call writloga('default',1,logmess,0,ierrw)
        ierr = 0
      endif




      return
      end
C     END read_zfile

C**********************************************************************
C   NAME:
         subroutine connectij_surf(nx, ny, ierr)
C
C   PURPOSE
c        connect a quad mesh from an avs type .fld header info
c        and Z values from file.
C
C   ARGUMENTS:
C        nx, ny
C        ierr
C
C
C   USAGE
C        assumes cmo has been set up with point distribution
C        for regularly spaced quads with read_sheetij()
C        Z values have been read into both zic and attribute elev
C        as quadxy and rzbrick overwrite zic values
C
C
C   INPUT/OUTPUT
C         ierr   - INTEGER ERROR FLAG (==0 ==> OK, <>0 ==> AN ERROR)
C
C   COMMENTS
C
C
C   CHANGE HISTORY
C
C
C**********************************************************************
c
      implicit none
c
c
c ARGS
      integer      nx, ny
      integer      ierr, ierrw

C MESH_OBJECT POINTERS.
C
      real*8  zic(1000000)
      pointer (ipzic, zic)
      real*8  elev(1000000)
      pointer (ipelev, elev)
C
C
c LOCAL VARS
      character*132 logmess
      character*132 cbuf
      character*32  cmonam
      character*32  isubname
      integer       icharlnf
      integer
     >        i
     >,       itp
     >,       nz
     >,       iout
     >,       nnodes
     >,       ilen
     >,       itype
     >,       idone
C
c BEGIN

      idone = 0
      ierr  = 0
      if(nx.eq.0 .or. ny.eq.0) then
         write(logmess,1000) nx, ny
 1000    format('ERROR: nx = ',i5,'ny = ',i5)
         call writloga('default',1,logmess,0,ierrw)
         goto 999
      endif
      isubname='connectij_surf'
      call cmo_get_name(cmonam,ierr)
      if(ierr .ne. 0)write(6,*)ierr,' ierr value cmonam'

 
      call cmo_get_info('nnodes',cmonam,nnodes,ilen,itype,ierr)
      if (nnodes .ne. nx*ny) then
        write(logmess,1005) cmonam(1:icharlnf(cmonam)),nnodes,nx*ny
 1005   format('WARNING: ',a,' has ',i10,' points. Expected: ',i10)
        call writloga('default',1,logmess,0,ierrw)
      endif
      nz  = nx*ny

      write(cbuf, 2000) nx, ny,1
 2000 format('rzbrick/ xyz /'
     >       ,i8,',',i8,',',i8,'/ 1,0,0/ connect/ ; finish')
      call dotaskx3d(cbuf, ierr)
      if(ierr .ne. 0)call x3d_error(isubname,'rzbrick')
      if(ierr.ne.0) goto 999
      call cmo_get_info('nnodes',cmonam,nnodes,ilen,itype,ierr)

c REPLACE dummy Z values with values read from file
      call cmo_get_info('zic',cmonam, ipzic, iout, itp, ierr)
      if(ierr .ne. 0)write(6,*)isubname,ierr,' ierr value zic'
      call cmo_get_info('elev',cmonam, ipelev, iout, itp, ierr)
      if(ierr .ne. 0)write(6,*)isubname,ierr,' ierr value elev'

      do i = 1, nnodes
        zic(i) = elev(i)
      enddo

      idone = 1
  999 if (idone .eq. 0) then
         write(logmess,9000) nnodes
 9000    format('ERROR: Connecting quads. Total points: ',i10)
         call writloga('default',1,logmess,0,ierrw)
      else
         ierr = 0
      endif

      return
      end
C     END connectij_surf

CCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCC
c  name read_binaryij
c
c  purpose
c      read a binary file containing a list of float or double values.
c      used for reading elevations given for each ij of a surface
c
c  input arguments
c      ifile       character name of binary file
c      vartype     float or double type
c                  vartype = 0 = flat = default
c                  vartype = 1 = double
c      xval        array to fill with values of flen long
c      istart, istop, jstart, jstop   used to subset along i,j's
c      nx          length in x direction of the grid
c      ierr_return is number of bytes read, <= 0 if error
c
CCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCC

      subroutine read_binaryij(ifile, vartype, xval, flen,
     >                  istart, istop, jstart, jstop, nx, ierr_return)

      implicit none
c
c args
      character*(*) ifile
      integer vartype
      integer istart, istop, jstart, jstop
      integer flen
      integer nx
      integer ierr_return
      real*8 xval(flen)
c
c local
      character*132 logmess
      character*32 isubname
      character*132 file
      character*132 Fifile

      integer icharlnf
      integer ierror, ics, ierrw
      integer iunit, len1, iadr, ibytes
      integer i, ii, icount, itotal

      integer*4 iunit4

      real*8 fdata8
      real*4 fdata4

c
      ierr_return = 0
      itotal = 0
      isubname="read_binaryij"
C
      len1=icharlnf(ifile)
      Fifile=ifile(1:len1) // 'F'
      file=ifile(1:len1) // char(0)
      iunit=-1

      call hassign(iunit,Fifile,ics)
      if (iunit.lt.0 .or. ierror.lt.0) then
        call x3d_error(isubname,'hassign bad file unit')
      endif
      iunit4 = iunit
      call cassignr(iunit4,file,ics)
      if (ics.ne.0) then
         write(logmess,'(a,a132)') ' error opening file ',file
         call writloga('default',0,logmess,0,ierrw)
         ierr_return = -1
         goto 9999
      endif
C
      ibytes = 4
      if (vartype .eq. 1 ) ibytes = 8
      iadr=0

C     loop through file values
      do i = 1,flen

         if (ibytes .eq. 4) then
            call cread(iunit4,fdata4,ibytes,ierror)
         else
            call cread(iunit4,fdata8,ibytes,ierror)
         endif
         if(ierror.ne.0) then
           write(logmess,'(a,a132)') ' error reading file ',file
           call writloga('default',0,logmess,0,ierrw)
           ierr_return = -1 * iadr 
           go to 9999
         endif
         iadr=iadr+ibytes
c
c        figure out subset, assign values to xval
         icount = ((i-1)/nx)+1
         if ((icount .ge. jstart) .and. (icount .le. jstop)) then
           ii = mod(i,nx)
           if(ii.eq.0) ii = nx
           if( (ii .ge. istart) .and. (ii .le. istop)) then
             itotal = itotal+1
             if (ibytes .eq. 4) then
               xval(itotal) = fdata4
             else
               xval(itotal) = fdata8
             endif
           endif
         endif

      enddo

      ierr_return = itotal * ibytes
 9999 continue
      if (iunit4.gt.0) call cclose(iunit4)
      return
      end
C     END read_binaryij
