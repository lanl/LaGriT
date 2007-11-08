c
          subroutine geohelp(imsgin,
     >                       xmsgin,
     >                       cmsgin,
     >                       msgtyp,
     >                       nwds,
     >                       ierr1)
 
c  PURPOSE:
c         This routine prints syntax help on various ees-5
c         routines for geologic applications. As commands are added
c         into the LAGrit set of commands, they should be removed
c         from this temporary help set.
c
c  Input Arguments:
c         cmsgin - character array of tokens returned by parser
c         imsgin - integer   array of tokens returned by parser
c         msgtyp - integer   array of tokens returned by parser
c         xmsgin - real      array of tokens returned by parser
c         nwds   - number          of tokens returned by parser
c
c  Output Arguments:
c         ierr1 - (=0 for successful call to routine), (= -1 otherwise)
c         ierr  - (returned from routine)
c
C  CHANGE HISTORY:
C
C        $Log: geo_help.f,v $
C        Revision 2.00  2007/11/05 19:45:56  spchu
C        Import to CVS
C
CPVCS
CPVCS       Rev 1.21   02 Oct 2007 12:40:28   spchu
CPVCS    original version
C
c-----------------------------------------------------------------------
c
c
        implicit none
c
c ARGS
        integer nwds,ierr1
        character*32 cmsgin(nwds)
        integer imsgin(nwds), msgtyp(nwds)
        real*8 xmsgin(nwds)
 
c LOCAL
        integer ics, lenc, leno, imore
        integer icharlnf
        character*132 logmess
 
c BEGIN
c      Get command length
c
       lenc = icharlnf( cmsgin(1) )
       leno = icharlnf( cmsgin(2) )
c
c      Set default error return to fail
c
      ierr1 = -1
      imore=0
c
      if (nwds .gt. 1) imore = 1
      if (nwds.eq.1 .or. cmsgin(2)(1:3).eq.'all') then
        write(logmess,'(a)')
     >  'GEO COMMANDS --------------------------------------------'
        call writloga('default',1,logmess,1,ics)
        if(cmsgin(2)(1:3).eq.'all') nwds=1
      endif
 
c ass
      if (nwds.eq.1 .or. cmsgin(2)(1:3).eq.'ass' ) then
       write(logmess,'(a)')
     >'ASSIGN_COLOR_NORMAL REPLACED by settets/normal'
       call writloga('default',1,logmess,0,ics)
         if (imore.gt.0 ) then
           write(logmess,'(a)')
     >'                   - colors tets by direction of normal'
           call writloga('default',0,logmess,0,ics)
         endif
      endif
 
c bou
      if (nwds.eq.1 .or. cmsgin(2)(1:3).eq.'bou' ) then
         write(logmess,'(a)')
     >'BOUNDARY_COMPONENTS'
       call writloga('default',1,logmess,0,ics)
         if (imore.gt.0 ) then
           write(logmess,'(a)')
     >'                   -reports number of boundaries, holes if > 1'
           call writloga('default',0,logmess,0,ics)
           write(logmess,'(a)')
     >'                   -must have current itp array'
           call writloga('default',0,logmess,0,ics)
         endif
      endif
 
c dbl
      if (nwds.eq.1 .or. cmsgin(2)(1:3).eq.'dbl' ) then
       write(logmess,'(a)')
     >'DBL_TO_SNGL'
       call writloga('default',1,logmess,0,ics)
         if (imore.gt.0 ) then
           write(logmess,'(a)')
     >'                   - removes parents for single defined nodes'
           call writloga('default',0,logmess,0,ics)
         endif
      endif
 
c cmo
      if (nwds.eq.1 .or. cmsgin(2)(1:3).eq.'cmo' ) then
       write(logmess,'(a)')
     >'CMO PRINTATT cmoname attname [minmax|list] point_or_elem_set'
       call writloga('default',1,logmess,0,ics)
         if (imore.gt.0 ) then
           write(logmess,'(a)')
     >'                   attnam = nnodes   applies node attributes'
           call writloga('default',0,logmess,0,ics)
           write(logmess,'(a)')
     >'                   attnam = nelement applies elem attributes'
           call writloga('default',0,logmess,0,ics)
           write(logmess,'(a)')
     >'                   attnam = -all-    applies all attributes'
           call writloga('default',0,logmess,0,ics)
           write(logmess,'(a)')
     >'                   attnam = -xyz-     applies x,y,z attributes'
           call writloga('default',0,logmess,0,ics)
           write(logmess,'(a)')
     >'                   list attributes or minmax attributes'
           call writloga('default',0,logmess,0,ics)
           write(logmess,'(a)')
     >'                   otherwise print field of attributes'
           call writloga('default',0,logmess,0,ics)
         endif
      endif
 
 
c fil
      if (nwds.eq.1 .or. cmsgin(2)(1:3).eq.'fil' ) then
       write(logmess,'(a)')
     >'FILTER2D /xy | xz | yz/epsilon_value/'
       call writloga('default',1,logmess,0,ics)
         if (imore.gt.0 ) then
           write(logmess,'(a)')
     >'                   - undefined'
           call writloga('default',0,logmess,0,ics)
         endif
      endif
 
c rep
      if (nwds.eq.1 .or. cmsgin(2)(1:6).eq.'report' ) then
       write(logmess,'(a)')
     > 'REPORT  REPLACED with quality and cmo/printatt commands'
       call writloga('default',1,logmess,0,ics)
      endif
 
c rfile
      if (nwds.eq.1 .or. cmsgin(2)(1:5).eq.'rfile' ) then
       write(logmess,'(a)')
     >'RFILE  / filename | fileroot / [cmoname] /'
       call writloga('default',1,logmess,0,ics)
         if (imore.gt.0 ) then
           write(logmess,'(a)')
     >'                   - finds and reads gmv and avs files'
           call writloga('default',0,logmess,0,ics)
           write(logmess,'(a)')
     >'                   - uses 3dmesh as default name'
           call writloga('default',0,logmess,0,ics)
         endif
      endif
 
c she
      if (nwds.eq.1 .or. cmsgin(2)(1:3).eq.'she' ) then
       write(logmess,'(a)')
     >'READ/SHEETIJ/ file /nx,ny/minx,miny/ [options]'
       call writloga('default',1,logmess,0,ics)
         if (imore.gt.0 ) then
           write(logmess,'(a)')
     >'                   - Create a quad cmo from elevation file'
           call writloga('default',0,logmess,0,ics)
           write(logmess,'(a)')
     >'          Options:  binary | ascii, center, xflip, yflip'
           call writloga('default',0,logmess,0,ics)
         endif
      endif
 
 
c smo
      if (nwds.eq.1 .or. cmsgin(2)(1:3).eq.'smo' ) then
       write(logmess,'(a)')
     >'SMOOTH_RECON  / [smooth_type] / niterations / [pset,get,pname]'
       call writloga('default',1,logmess,0,ics)
         if (imore.gt.0 ) then
           write(logmess,'(a)')
     >'                   - call smooth;recon 0; with n iterations'
           call writloga('default',0,logmess,0,ics)
         endif
      endif
 
c smo
      if (nwds.eq.1 .or. cmsgin(2)(1:3).eq.'tri' ) then
       write(logmess,'(a)')
     >'TRILAYER DERIVE / proportional or constant /'
       call writloga('default',1,logmess,0,ics)
       write(logmess,'(a)')
     >'                / cmosink cmolow cmohigh / xvalue'
       call writloga('default',0,logmess,0,ics)
         if (imore.gt.0 ) then
           write(logmess,'(a)')
     >'                   - derive cmosink from low or high layer'
           call writloga('default',0,logmess,0,ics)
           write(logmess,'(a)')
     >'                   - xvalue is the distance from derived layer'
           call writloga('default',0,logmess,0,ics)
           write(logmess,'(a)')
     >'                   - negative xvalue derives from cmolow'
           call writloga('default',0,logmess,0,ics)
         endif
      endif
 
 
 
c tri or pri
      if (nwds.eq.1 .or. cmsgin(2)(1:3).eq.'tri' .or.
     >                   cmsgin(2)(1:3).eq.'pri' ) then
       write(logmess,'(a)')
     >'MREAD TRILAYER /[minx,miny,maxx,maxy]/file(1),..f(n)/ &'
       call writloga('default',1,logmess,0,ics)
       write(logmess,'(a)')
     >'  /[LAYERCLR, FLIP, PINCH p_value/TOPO | BUFFER | TRUNC fileno/]'
       call writloga('default',0,logmess,0,ics)
 
         if(imore.gt.0) then
         write(logmess,'(a)')
     >'                   -reads tri-surface files into single cmo '
         call writloga('default',0,logmess,0,ics)
         write(logmess,'(a)')
     >'                   -option minxy,maxy subsets by pset '
         call writloga('default',0,logmess,0,ics)
         write(logmess,'(a)')
     >'                   -layerclr replaces tet clr according to layer'
         call writloga('default',0,logmess,0,ics)
         write(logmess,'(a)')
     >'                   -flip normals opposite direction '
         call writloga('default',0,logmess,0,ics)
         write(logmess,'(a)')
     >'                   -pinch to lower layer | to p_value thickness'
         call writloga('default',0,logmess,0,ics)
         write(logmess,'(a)')
     >'                   -topo truncate all lower layers by top layer'
         call writloga('default',0,logmess,0,ics)
         write(logmess,'(a)')
     >'                   -buffer truncate all lower layers by top - 2'
         call writloga('default',0,logmess,0,ics)
         write(logmess,'(a)')
     >'                   -truncate all lower layers by nth file'
         call writloga('default',0,logmess,0,ics)
         write(logmess,'(72a)')
 
c    >'mktrilayer        /cmoin/cmotrunc/cmoout/xvalue/const | prop /'
c        call writloga('default',0,logmess,0,ics)
c        write(logmess,'(72a)')
c    >'check trilayer    / brief | [v]erbose | flip /'
c        call writloga('default',0,logmess,0,ics)
c        write(logmess,'(a)')
c    >'                   -checks Z normal for tri-surface cmo'
c        call writloga('default',0,logmess,0,ics)
c        write(logmess,'(72a)')
c    >'flip trilayer    '
c        call writloga('default',0,logmess,0,ics)
c        write(logmess,'(a)')
c    >'                   -flips Z normal for tri-surface cmo'
c        call writloga('default',0,logmess,0,ics)
 
        endif
      endif
 
 
c pri or pgg or squ
       if (nwds.eq.1 .or. cmsgin(2)(1:3).eq.'pri'   .or.
     >                    cmsgin(2)(1:3).eq.'pgg'   .or.
     >                    cmsgin(2)(1:3).eq.'squ' ) then
 
         write(logmess,'(a)')
     >'TRILAYERTOTET  /nlayers/cmoout/cmoin/3, 6 or 24/[degen]/'
         call writloga('default',1,logmess,0,ics)
 
         write(logmess,'(a)')
     >'PGG  / cmoout / cmoin / numlayers / [layerclr]'
         call writloga('default',0,logmess,0,ics)
         if(imore.gt.0)then
         write(logmess,'(a)')
     >'                   -converts trilayers cmo to prisms cmo'
         call writloga('default',0,logmess,0,ics)
         endif
 
         write(logmess,'(a)')
     >'PRISMTOHEX  / cmohex / cmoprism /'
         call writloga('default',1,logmess,0,ics)
         if (imore.gt.0 ) then
         write(logmess,'(a)')
     >'                   -converts prism cmo to degenerate hex cmo'
         call writloga('default',0,logmess,0,ics)
         endif
 
c        write(logmess,'(a)')
c    >'squeeze           /x,y,z/ xcons | cmosrf / lt | gt /'
c        call writloga('default',1,logmess,0,ics)
 
       endif
 
c work (current work on new codes)
c      if (nwds.eq.1 .or. cmsgin(2)(1:4).eq.'work' ) then
 
c        write(logmess,'(a)')
c work
c    >'Temporary work routines:'
c        call writloga('default',1,logmess,1,ics)
 
c work rmpoint
c        write(logmess,'(a)')
c    >'  rmpoint         /merge / -threshold_value/'
c        call writloga('default',0,logmess,0,ics)
c        write(logmess,'(a)')
c    >'                  /merge / prism/'
c        call writloga('default',0,logmess,0,ics)
 
c        call writloga('default',0,logmess,0,ics)
c work rmpoint_new
c        write(logmess,'(a)')
c    >'  rmpoint_new     /merge_volume | merge_volume/threshold_value/'
c        call writloga('default',0,logmess,0,ics)
c        write(logmess,'(a)')
c    >'                  /edge_length | edge_ratio/threshold_value/'
c        call writloga('default',0,logmess,0,ics)
c        write(logmess,'(a)')
c    >'                  /face_area | face_ratio/threshold_value/'
c        call writloga('default',0,logmess,0,ics)
c        write(logmess,'(a)')
c    >'                  /prism6 | prism24/'
c        call writloga('default',0,logmess,0,ics)
c      endif
 
       write(logmess,'(a)')
     > '--------------------------------------------------------'
       call writloga('default',1,logmess,1,ics)
       ierr1 = 0
 
 
 
       return
       end
 
