
C#####################################################################
C
C      FILE - stack_options.f
C             calls used by the stack routine
C             subroutines once located in temptam.f and read_trilayers.f
C
C      CHANGE HISTORY -
C
C      Updates - T.A.Miller - May 2011
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
         if(ierr.ne.0) then
           call x3d_error(isubname,'copy cmoin to cmoout')
           ierr = ierr+1
           return
         endif
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
C#####################################################################
C
C      This routine has been replaced with calls to stack/layers and stack/fill
C
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
 
C#####################################################################
C      this call has been replaced with stack/layers and stack/fill

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
