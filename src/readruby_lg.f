      subroutine readruby_lg(ifile,cmo3dname,nbinx,nbiny,nbinz,ierror)
C
C ######################################################################
C
C        $Log: readruby_lg.f,v $
C        Revision 2.00  2007/11/09 20:04:00  spchu
C        Import to CVS
C
C
CPVCS
CPVCS       Rev 1.21   02 Oct 2007 12:40:28   spchu
CPVCS    original version
C
C#######################################################################
C
c
c  read ruby input file off iunit
c  identify vias - make surface, mregion, region command for each via
c  for other polygons, make an avs file, surface,region, mregion command
c
      implicit none
      include 'commands_lg.h'
      include 'chydro.h'
      pointer (ipimsgout,imsgout)
      pointer (ipxmsgout,xmsgout)
      pointer (ipcmsgout,cmsgout)
      pointer (ipmsgtype,msgtype)
      integer imsgout(*),msgtype(*)
      real*8 xmsgout(*)
      character*32 cmsgout(*),isubname
      integer icscode,numregs,maxregs,nwds,numvias,
     * ncoord,i, npoly,iavsunit,ibuffer,j,nvia,nplanes,
     * iunit,ierror,lenargs,jcmdchar,nbinx,nbiny,nbinz
      character*1 colon, lparen, rparen
      character*32 name, avsname ,cmoname, zmaxname,zminname,
     *  regname,megname,surfname,cmo3dname,sname,ifile
      pointer (ipxcoord,xcoord)
      pointer (ipycoord,ycoord)
      real*8 xcoord(*),ycoord(*)
      pointer (ipviainfo,viainfo)
      pointer (ipviaidx,viaidx)
      pointer (ipzplane,zplane)
      integer viaidx(*)
      real*8 viainfo(5,*),zplane(*)
      real*8 zmincur, zmaxcur, xcenter, ycenter, radius, ascend,
     *   zminprev,xm,ym,zm,xx,yx,zx
      character*4 nametail
      character*264 logmess
      character*4096 longcmd
      logical clockwise
c 
      colon =':'
      lparen='('
      rparen=')'
      isubname='readruby'
      ierror=0
      numvias=0
      npoly=0
      nplanes=0
      longcmd=' '
      jcmdchar=30
c
c create 3dmesh
c
      write(logmess,2)cmo3dname
 2    format('cmo/create/',a32,';finish')
      call dotask(logmess,ierror)
c
c  assign a unit number to the ruby file
c
      iunit=-1
      call hassign(iunit,ifile,ierror)
      if (iunit.lt.0 .or. ierror.lt.0) then
         call x3d_error(isubname,'hassign bad file unit')
         goto 9999
      endif

c
c     get temporary storage
c
      lenargs=10000
      call mmgetblk('imsgout',isubname,ipimsgout,lenargs,1,icscode)
      call mmgetblk('xmsgout',isubname,ipxmsgout,lenargs,2,icscode)
      call mmgetblk('cmsgout',isubname,ipcmsgout,lenargs,2,icscode)
      call mmgetblk('msgtype',isubname,ipmsgtype,lenargs,1,icscode)
      call mmgetblk('xcoord',isubname,ipxcoord,lenargs,2,icscode)
      call mmgetblk('ycoord',isubname,ipycoord,lenargs,2,icscode)
      numregs=0
      maxregs=1000
      call mmgetblk('viainfo',isubname,ipviainfo,5*maxregs,2,icscode)
      call mmgetblk('viaidx',isubname,ipviaidx,maxregs,2,icscode)
      call mmgetblk('zplane',isubname,ipzplane,2*maxregs,2,icscode)
 10   command=' '
c
c  read the next line
c
      read(iunit,'(a)',end=100) command
      if(command.eq.' ') go to 10
c
c  get rid of parentheses and parse line
c
      do i=1,maxlen_buff
        if(command(i:i).eq.lparen.or.command(i:i).eq.rparen)
     *     command(i:i)=' '
      enddo
      call parse_string(imsgout,msgtype,
     *      xmsgout,cmsgout,nwds)
      if(nwds.gt.lenargs) then
         write(logmess,"(' too many tokens in line')")
         call writloga('default',0,logmess,0,icscode)
         ierror=1
         go to 9999
      endif
      if(nwds.eq.0.or.cmsgout(1).ne.'poly') go to 10
c
c   find name zmin, zmax,
c
      name = cmsgout(2)
      zmincur=xmsgout(5)
      zmaxcur=xmsgout(6)
      if(npoly.eq.0.and.numvias.eq.0) then
         xm=xmsgout(9)
         xx=xmsgout(9)
         ym=xmsgout(10)
         yx=xmsgout(10)
         zm=zmincur
         zx=zmaxcur
      else
         if(zmincur.lt.zm) zm=zmincur
         if(zmaxcur.gt.zx) zx=zmaxcur
      endif
c
c   get x, y pairs
c
      ncoord=0
      do i=9,nwds,2
        ncoord=ncoord+1
        if(ncoord.gt.lenargs) then
           lenargs=lenargs+1000
           call mmnewlen('xcoord',isubname,ipxcoord,lenargs,icscode)
           call mmnewlen('ycoord',isubname,ipycoord,lenargs,icscode)
        endif
        xcoord(ncoord)=xmsgout(i)
        ycoord(ncoord)=xmsgout(i+1)
        if(xcoord(ncoord).lt.xm) xm=(xcoord(ncoord))
        if(xcoord(ncoord).gt.xx) xx=(xcoord(ncoord))
        if(ycoord(ncoord).lt.ym) ym=(ycoord(ncoord))
        if(ycoord(ncoord).gt.yx) yx=(ycoord(ncoord))
      enddo
c
c  see if via (xcoord and ycoords should lie on a circle)
c  via candidates must have more than 20 nodes
c  radius will be positive if yes
c
      if(ncoord.le.20) then
         radius=-100.
      else
         call checkvia(ncoord,xcoord,ycoord,xcenter,ycenter,radius)
      endif
      if(radius.gt.0.0) then
c
c  this is a via, store center, radius, z min and z max
c  round all these values to integer for later comparisons
c
         numvias=numvias+1
         if(numvias.gt.maxregs) then
           maxregs=maxregs+1000
           call mmnewlen('viainfo',isubname,ipviainfo,4*maxregs,icscode)
           call mmnewlen('viaidx',isubname,ipviaidx,maxregs,icscode)
           call mmnewlen('zplane',isubname,ipzplane,2*maxregs,icscode)
         endif
         viainfo(1,numvias)=nint(xcenter)
         viainfo(2,numvias)=nint(ycenter)
         viainfo(3,numvias)=nint(radius)
         viainfo(4,numvias)=zmincur
         viainfo(5,numvias)=zmaxcur
         viaidx(numvias)=numvias
      else
c
c  not a via - treat as sheet surface
c
c  see if the polygon is clockwise or counterclockwise
c
         clockwise=.true.
         call checkorientation(ncoord,xcoord,ycoord,clockwise)
c
c  make avs file and cmo/create, read/avsm, surface commands
c
         npoly=npoly+1
         if(npoly.gt.maxregs) then
           maxregs=maxregs+1000
           call mmnewlen('viainfo',isubname,ipviainfo,4*maxregs,icscode)
           call mmnewlen('viaidx',isubname,ipviaidx,maxregs,icscode)
           call mmnewlen('zplane',isubname,ipzplane,2*maxregs,icscode)
         endif
         iavsunit=-1
         ibuffer=0
         write(nametail,'(i4.4)') npoly
         avsname='avsruby'//nametail
         cmoname='cmoruby'//nametail
         sname='sruby'//nametail
         regname='r'//nametail
         megname='m'//nametail
         call hassign(iavsunit,avsname,ibuffer)
         write(iavsunit,*) 2*ncoord,2*ncoord,ibuffer,ibuffer,ibuffer
         do i=1,ncoord
            write(iavsunit,'(i6,3f14.5)')
     *          (i-1)*2+1,xcoord(i),ycoord(i),zmincur
            write(iavsunit,'(i6,3f14.5)')
     *          (i-1)*2+2,xcoord(i),ycoord(i),zmaxcur
         enddo
         do i=1,ncoord-1
            if(clockwise) then
               write(iavsunit,*) (i-1)*2+1, '  1    tri ',
     *         (i-1)*2+1,(i-1)*2+2,(i-1)*2+4
               write(iavsunit,*) (i-1)*2+2, '  1    tri ',
     *         (i-1)*2+4,(i-1)*2+3,(i-1)*2+1
            else
               write(iavsunit,*) (i-1)*2+1, '  1    tri ',
     *         (i-1)*2+1,(i-1)*2+4,(i-1)*2+2
               write(iavsunit,*) (i-1)*2+2, '  1    tri ',
     *         (i-1)*2+4,(i-1)*2+1,(i-1)*2+3
            endif
         enddo
         if(clockwise) then
            write(iavsunit,*) ncoord*2-1,   '  1    tri ',
     *        ncoord*2-1,ncoord*2,' 2'
            write(iavsunit,*) ncoord*2,'  1    tri ',' 2',' 1',
     *        ncoord*2-1
         else
            write(iavsunit,*) ncoord*2-1,   '  1    tri ',
     *        ncoord*2-1,' 2 ',ncoord*2
            write(iavsunit,*) ncoord*2,'  1    tri ',' 2',
     *        ncoord*2-1,' 1'
         endif
         close (iavsunit)
         write(logmess,12) cmoname
 12      format('cmo/create/',a32,'///tri;finish')
         call dotask(logmess,ierror)
         write(logmess,15)avsname
 15      format('read/avs/',a32,';cmo/setatt//imt1/1,0,0/1;',
     *     'cmo/setatt//itetclr/1,0,0/1;finish')
         call dotask(logmess,ierror)
c
c  select 3d mesh and write surface, region, mregion commands for 3d mesh
c
         write(logmess,18)cmo3dname
 18      format('cmo/select/',a32,';finish')
         call dotask(logmess,ierror)
         write(logmess,20)sname,cmoname
 20      format('surface/',a32,'/intrface/sheet/',a32,';finish')
         call dotask(logmess,ierror)
c
c  see if planes are already in list
c  if not generate surface commands
c
         do j=1,nplanes
                if(zmincur.eq.zplane(j)) then
                     write(nametail,'(i4.4)') j
                     zminname='plane'//nametail
                     go to 21
                 endif
         enddo
         nplanes=nplanes+1
         zplane(nplanes)=zmincur
         write(nametail,'(i4.4)')nplanes
         zminname='plane'//nametail
         write(logmess,23)zminname,zmincur,zmincur,zmincur
         call dotask(logmess,ierror)
 21      do j=1,nplanes
                if(zmaxcur.eq.zplane(j))then
                    write(nametail,'(i4.4)') j
                    zmaxname='plane'//nametail
                    go to 22
                endif
         enddo
         nplanes=nplanes+1
         zplane(nplanes)=zmaxcur
         write(nametail,'(i4.4)')nplanes
         zmaxname='plane'//nametail
         write(logmess,23)zmaxname,zmaxcur,zmaxcur,zmaxcur
         call dotask(logmess,ierror)
 23      format('surface/',a32,'/intrface/plane/0,0,',e20.12,
     *       '/1,0,',e20.12,'/1,1,',e20.12,'/;finish')
c
c  region, mregion commands
c
 22      write(logmess,24) regname,sname, zmaxname,zminname
 24      format('region/',a16,'/ le ',a16,'and le ',a16,' and ge ',
     *       a16,'/;finish')
         call dotask(logmess,ierror)
         write(logmess,26) megname,sname, zmaxname,zminname
 26      format('mregion/',a16,'/ lt ',a16,'and lt ',a16,' and gt ',
     *       a16,'/;finish')
         call dotask(logmess,ierror)
c
c  build region, mregion command strings for 'all else' region
c
         write(logmess,27) sname,zmaxname,zminname
 27      format (' and ( gt ',a16,' or gt ',a16,' or lt ',a16,' ) ')
         longcmd(jcmdchar+1:jcmdchar+76) = logmess(1:75)
         jcmdchar=jcmdchar+75
      endif
      go to 10
c
c   now find out which vias stack on top of each other
c   pack via info down to viainfo(numvias,4)
c   sort by center, then radius, then zmin
c
 100  ascend=1.0
      call hpsortrmp(numvias,4,5,viainfo,ascend,viaidx)
c
c  now look to see which can be combined
c  if zmin of current = zmax of previous - these can be combined
c
      nvia=0
      xcenter=viainfo(1,viaidx(1))
      ycenter=viainfo(2,viaidx(1))
      radius=viainfo(3,viaidx(1))
      zmincur=viainfo(4,viaidx(1))
      zmaxcur=viainfo(5,viaidx(1))
      zminprev=zmincur
      do i=2,numvias
c
c  test if identical
c
        if(viainfo(1,viaidx(i)).eq.xcenter.and.
     *     viainfo(2,viaidx(i)).eq.ycenter.and.
     *     viainfo(3,viaidx(i)).eq.radius .and.
     *     viainfo(4,viaidx(i)).eq.zminprev.and.
     *     viainfo(5,viaidx(i)).eq.zmaxcur.and.
     *     i.ne.numvias ) then
c
c  test if stackable
c
        elseif(viainfo(1,viaidx(i)).eq.xcenter.and.
     *     viainfo(2,viaidx(i)).eq.ycenter.and.
     *     viainfo(3,viaidx(i)).eq.radius .and.
     *     viainfo(4,viaidx(i)).eq.zmaxcur.and.
     *     i.ne.numvias) then
           zmaxcur=viainfo(5,viaidx(i))
           zminprev=viainfo(4,viaidx(i))
c
c  test if different
c
        elseif(viainfo(1,viaidx(i)).ne.xcenter.or.
     *     viainfo(2,viaidx(i)).ne.ycenter.or.
     *     viainfo(3,viaidx(i)).ne.radius .or.
     *     viainfo(4,viaidx(i)).ne.zmaxcur.or.
     *     i.eq.numvias) then
c
c  different via - output this one as a cylinder
c
           nvia=nvia+1
           if(i.eq.numvias) zmaxcur=viainfo(5,viaidx(i))
           write(nametail,'(i4.4)') nvia
           regname='rvia'//nametail
           megname='mvia'//nametail
           surfname='via'//nametail
           write(logmess,48)surfname,xcenter,ycenter,zmincur,
     *        xcenter,ycenter, zmaxcur,radius
 48        format('surface/',a16,'/intrface/cylinder/',
     *       7(e20.12,','),'/;finish')
           call dotask(logmess,ierror)
c
c  see if planes are already in list
c  if not generate surface commands
c
           do j=1,nplanes
                if(zmincur.eq.zplane(j)) then
                     write(nametail,'(i4.4)') j
                     zminname='plane'//nametail
                     go to 50
                 endif
           enddo
           nplanes=nplanes+1
           zplane(nplanes)=zmincur
           write(nametail,'(i4.4)')nplanes
           zminname='plane'//nametail
           write(logmess,23)zminname,zmincur,zmincur,zmincur
           call dotask(logmess,ierror)
 50        do j=1,nplanes
                if(zmaxcur.eq.zplane(j))then
                    write(nametail,'(i4.4)') j
                    zmaxname='plane'//nametail
                    go to 60
                endif
           enddo
           nplanes=nplanes+1
           zplane(nplanes)=zmaxcur
           write(nametail,'(i4.4)')nplanes
           zmaxname='plane'//nametail
           write(logmess,23)zmaxname,zmaxcur,zmaxcur,zmaxcur
           call dotask(logmess,ierror)
c
c  make region, mregion commands - be sure to pick up correct planes
c
 60        write(logmess,24) regname,surfname, zmaxname,zminname
           call dotask(logmess,ierror)
           write(logmess,26) megname,surfname, zmaxname,zminname
           call dotask(logmess,ierror)
c
c  build region, mregion command strings for 'all else' region
c
           write(logmess,27) surfname,zmaxname,zminname
           longcmd(jcmdchar+1:jcmdchar+76) = logmess(1:75)
           jcmdchar=jcmdchar+75
           xcenter=viainfo(1,viaidx(i))
           ycenter=viainfo(2,viaidx(i))
           radius=viainfo(3,viaidx(i))
           zmincur=viainfo(4,viaidx(i))
           zminprev=viainfo(4,viaidx(i))
           zmaxcur=viainfo(5,viaidx(i))
         else
         endif
      enddo
c
c  output reflective surface command
c  output region command for 'all else'
c
      write(logmess,70) xm,ym,zm,xx,yx,zx
 70   format('surface/outer/reflect/box/',6(e20.12,'/'),';finish')
      call dotask(logmess,ierror)
      longcmd(jcmdchar+1:jcmdchar+7)=';finish'
      longcmd(1:25)='region/allelse/ le outer '
      call dotask (longcmd,ierror)
      longcmd(1:27)='mregion/mallelse/ le outer '
      call dotask (longcmd,ierror)
c
c  output rz command using bins supplied on input
c
      write(logmess,80) nbinx,nbiny,nbinz,xm,ym,zm,xx,yx,zx
 80   format('createpts/xyz/',2(i10,','),i10,'/',6(e20.12,'/'),
     *  '0,0,0/;finish')
      call dotask(logmess,ierror)
 9999 continue
c
c  release temporary memory
c
      call mmrelprt(isubname,icscode)
      return
      end
c
c
      subroutine checkorientation(ncoord,xcoord,ycoord,clockwise)
c
c  determine if polygon is specified clockwise or counterclockwise
c  get area of polygon - negative means clockwise - positive means
c  counterclockwise
c
      implicit none
      integer ncoord,i,imin,ip1,im1
      real*8 xcoord(*),ycoord(*),area,ymin,xmin
      logical clockwise
c
      clockwise=.true.
      area=0.0
      xmin=xcoord(1)
      ymin=ycoord(1)
      imin=1
c 
c  find min y coord - only look at nodes in one plane
c
      do i=2,ncoord
         if(ycoord(i).lt.ymin) then
            ymin=ycoord(i)
            xmin=xcoord(i)
            imin=i
         elseif(ycoord(i).eq.ymin) then
            if(xcoord(i).gt.xmin) then
               ymin=ycoord(i)
               xmin=xcoord(i)
               imin=i
            endif
         endif
      enddo
      im1=imin-1
      ip1=imin+1
      if(imin.eq.1) then
         im1=ncoord
      elseif(imin.eq.ncoord) then
         ip1=1
      endif
      area=xcoord(im1)*ycoord(imin)-ycoord(im1)*xcoord(imin)+
     *   xcoord(ip1)*ycoord(im1)-xcoord(im1)*ycoord(ip1)+
     *   xcoord(imin)*ycoord(ip1)-xcoord(ip1)*ycoord(imin)
      if(area.gt.0.0) clockwise=.false.
      return
      end
c
c
      subroutine checkvia(ncoord,xcoord,ycoord,xcenter,ycenter,radius)
c
c  determine if all nodes with coordinates in xcoord and ycoord
C  fall on the same circle
c  if so return center and radius of the circle
c  if not return radius = -100
c
      implicit none
      integer ncoord,i
      real*8 xcoord(*),ycoord(*),xcenter,ycenter,radius
      real*8 xa,ya,xb,yb,xc,yc,dotb3,dot3,rb3,ql,xl,yl
      radius=-100.0
c
c  pick three nodes and get circle that these nodes determine
c
      xa=xcoord(1)
      ya=ycoord(1)
      xb=xcoord((ncoord+1)/2)-xa
      yb=ycoord((ncoord+1)/2)-ya
      xc=xcoord(ncoord)-xa
      yc=ycoord(ncoord)-ya
      dotb3=xb*xc+yb*yc
      dot3=dotb3/(xc*xc+yc*yc)
      rb3=1.0/(xb*xb+yb*yb)
      ql=(1.0-dot3)/(1.0-dot3*dotb3*rb3+1.0d-30)
      xl=0.5*(ql*(xc-dotb3*rb3*xb)+xb)
      yl=0.5*(ql*(yc-dotb3*rb3*yb)+yb)
      xcenter=xl+xa
      ycenter=yl+ya
      radius=sqrt(xl**2+yl**2)
c
C  Check  if other nodes fall on the circle determined by the
c  selected nodes
c
      do i=2,ncoord-1
         if (((xcoord(i)-xcenter)**2 + (ycoord(i)-ycenter)**2)-radius**2
     *          .gt. .01*radius**2) then
            radius = -100.
            go to 9999
          endif
      enddo
 9999 continue
      return
      end
 
 
