*dk,interp_lg
      subroutine interp_lg(imsgin,xmsgin,cmsgin,msgtype,nwds,ierr2)
C
C#######################################################################
C
C $Log: interp_lg.f,v $
C Revision 2.00  2007/11/05 19:45:59  spchu
C Import to CVS
C
CPVCS    
CPVCS       Rev 1.1   30 Sep 2004 09:15:52   dcg
CPVCS    replace calls to real( with calls to dble(
CPVCS    
CPVCS       Rev 1.0   20 Jul 2000 14:03:26   bap
CPVCS    Initial revision.
CPVCS    
CPVCS       Rev 1.1   29 Jun 2000 10:53:54   bap
CPVCS    Put in PVCS logging info
C
      implicit none
C
      include 'local_element.h'
C
      integer nwds
      integer imsgin(nwds), msgtype(nwds)
      real*8 xmsgin(nwds)
      character*(*) cmsgin(nwds)
      integer i1,i2,i3,i4,i5,i6,i7,i8,ipointi,ipointj,i1temp,
     *  it,iflag,itetcnt,itoff,jtoff,l,nef,nen,nsdtopo,
     *  nsdgeom,leni,i,nptsl,icmotp,nef1,nen1,nsdtopo1,
     *  nsdgeom1,ier,icscode,icmotype,nelements2,npoints_new,
     *  nelements_new,mpno2,mpno1,ics,length,itype,ierr,ilen,
     *  nnodes2,ilayers,nelements,nnodes,ierror,ierr2,mbndry
      real*8 xtetvol,dy1,dz1,dsfac,dx1
C
C     __________________________________________________________________
C     Master Mesh Object.
C
      pointer (ipimt1, imt1)
      pointer (ipitp1, itp1)
      integer itp1(1000000), imt1(1000000)
      pointer (ipxic1, xic1)
      pointer (ipyic1, yic1)
      pointer (ipzic1, zic1)
      real*8 xic1(10000000), yic1(10000000), zic1(10000000)
      pointer (ipitetclr1, itetclr1(10000000))
      pointer (ipitettyp1, itettyp1(10000000))
      pointer (ipitetoff1, itetoff1(10000000))
      pointer (ipjtetoff1, jtetoff1(10000000))
      pointer (ipitet1, itet1(10000000))
      pointer (ipjtet1, jtet1(10000000))
      integer itet1,jtet1,itetclr1,itettyp1,itetoff1,jtetoff1
C
C     __________________________________________________________________
C     Slave Mesh Object.
C
      pointer (ipimt2, imt2)
      pointer (ipitp2, itp2)
      integer itp2(1000000), imt2(1000000)
      pointer (ipxic2, xic2)
      pointer (ipyic2, yic2)
      pointer (ipzic2, zic2)
      real*8 xic2(10000000), yic2(10000000), zic2(10000000)
      pointer (ipitetclr2, itetclr2(10000000))
      pointer (ipitettyp2, itettyp2(10000000))
      pointer (ipitetoff2, itetoff2(10000000))
      pointer (ipjtetoff2, jtetoff2(10000000))
      pointer (ipitet2, itet2(10000000))
      pointer (ipjtet2, jtet2(10000000))
      integer itet2,jtet2,itetclr2,itettyp2,itetoff2,jtetoff2
C
      pointer (ipisetwd, isetwd)
      integer isetwd(1000000)
C
      pointer (ipmpary1, mpary1)
      pointer (ipmpary2, mpary2)
      integer mpary1(1000000), mpary2(1000000)
C
      pointer (ipipflag1, ipflag1)
      integer ipflag1(1000000)
      pointer (ipitflag1, itflag1)
      integer itflag1(1000000)
C
      character*132 logmess
      character*32 isubname, cmo, cmonew
      character*8 cglobal, cdefault
      integer ip1,ip2,ip3,ip4,ip5,ip6
C
      real*8 xicvol(100), yicvol(100), zicvol(100)
C
C#######################################################################
C
      isubname='interp'
      cglobal='global'
      cdefault='default'
C
      if(cmsgin(3).eq.'-cmo-') then
         call cmo_get_name(cmo,ierror)
      else
         cmo = cmsgin(3)
      endif
      call cmo_exist(cmo,ierr)
      if(ierror.ne.0) then
         write(logmess,'(a)')
     &        'Error in subroutine extrude: input MO does not exist'
         call writloga('default',0,logmess,0,ierror)
         ierror = 1
         go to 9999
      endif
C
      call cmo_get_info('nnodes',cmo,nnodes,ilen,itype,ierr)
      call cmo_get_info('nelements',cmo,nelements,ilen,itype,ierr)
C
      ilayers=imsgin(5)
      length=nnodes
      call mmgetblk('mpary1',isubname,ipmpary1,length,1,ics)
      call mmgetblk('mpary2',isubname,ipmpary2,length,1,ics)
      if(msgtype(6).eq.1) then
         ip1=imsgin(6)
         ip2=imsgin(7)
         ip3=imsgin(8)
         mpno1=0
         do i1=ip1,ip2,ip3
            mpno1=mpno1+1
            mpary1(mpno1)=i1
         enddo
      else
         call cmo_get_info('isetwd',cmo,
     *                     ipisetwd,leni,icmotype,ierror)
         call cmo_get_info('itp1',cmo,ipitp1,leni,icmotype,ics)
         call pntlimc(cmsgin(6),cmsgin(7),cmsgin(8),
     *                ipmpary1,mpno1,nnodes,isetwd,itp1)
      endif
      if(msgtype(9).eq.1) then
         ip4=imsgin(9)
         ip5=imsgin(10)
         ip6=imsgin(11)
         mpno2=0
         do i1=ip4,ip5,ip6
            mpno2=mpno2+1
            mpary1(mpno2)=i1
         enddo
      else
         call cmo_get_info('isetwd',cmo,
     *                     ipisetwd,leni,icmotype,ierror)
         call cmo_get_info('itp1',cmo,ipitp1,leni,icmotype,ics)
         call pntlimc(cmsgin(9),cmsgin(10),cmsgin(11),
     *                ipmpary2,mpno2,nnodes,isetwd,itp1)
      endif
C
      cmonew=cmsgin(2)
C
      call cmo_exist(cmonew,icscode)
      if(icscode.eq.0) then
         call cmo_get_info('nnodes',cmonew,nnodes2,ilen,itype,ierr)
         call cmo_get_info('nelements',cmonew,
     *                     nelements2,ilen,itype,ierr)
         call cmo_select(cmonew,icscode)
         npoints_new=nnodes2+(2+ilayers)*mpno1
         nelements_new=nelements2+(2+ilayers-1)*nelements
      else
         nnodes2=0
         nelements2=0
         call cmo_derive(cmonew,cmo,icscode)
         npoints_new=(2+ilayers)*mpno1
         nelements_new=(2+ilayers-1)*nelements
      endif
C
      call cmo_get_info('ndimensions_geom',cmo,nsdgeom1,ilen,itype,ier)
      call cmo_get_info('ndimensions_topo',cmo,nsdtopo1,ilen,itype,ier)
      call cmo_get_info('nodes_per_element',cmo,nen1,ilen,itype,ierr)
      call cmo_get_info('faces_per_element',cmo,nef1,ilen,itype,ierr)
      if(nsdtopo1.eq.3) then
         print *,"Grid topology too high: ",nsdtopo1
         goto 9999
      endif
      if(nen1.eq.nelmnen(ifelmhyb) .and.
     *   nef1.eq.nelmnef(ifelmhyb)) then
         nsdgeom=3
         if(nsdtopo1.eq.1) then
            nsdtopo=2
         else
            nsdtopo=3
         endif
         nen=nelmnen(ifelmhyb)
         nef=nelmnef(ifelmhyb)
      else
         if(nsdtopo1.eq.2) then
            if(nen1.eq.nelmnen(ifelmtri) .and.
     *         nef1.eq.nelmnef(ifelmtri)) then
               nen=nelmnen(ifelmpri)
               nef=nelmnef(ifelmpri)
            elseif(nen1.eq.nelmnen(ifelmqud) .and.
     *             nef1.eq.nelmnef(ifelmqud)) then
               nen=nelmnen(ifelmhex)
               nef=nelmnef(ifelmhex)
            endif
         elseif(nsdtopo1.eq.1) then
            nsdgeom=3
            nsdtopo=2
            if(nen1.eq.nelmnen(ifelmlin) .and.
     *         nef1.eq.nelmnef(ifelmlin)) then
               nen=nelmnen(ifelmqud)
               nef=nelmnef(ifelmqud)
            endif
         endif
      endif
C
      call cmo_set_info('nnodes',cmonew,npoints_new,1,1,ierr)
      call cmo_set_info('nelements',cmonew,nelements_new,1,1,ierr)
      call cmo_set_info('ndimensions_geom',cmonew,nsdgeom,1,1,ier)
      call cmo_set_info('ndimensions_topo',cmonew,nsdtopo,1,1,ier)
      call cmo_set_info('nodes_per_element',cmonew,nen,1,1,ierr)
      call cmo_set_info('faces_per_element',cmonew,nef,1,1,ierr)
C
      call cmo_newlen(cmonew,ierr)
C
      call cmo_get_intinfo('mbndry',cmonew,mbndry,
     *                  leni,icmotp,ierror)
      call cmo_get_info('imt1',cmonew,ipimt2,leni,icmotp,ierr)
      call cmo_get_info('itp1',cmonew,ipitp2,leni,icmotp,ierr)
      call cmo_get_info('xic',cmonew,ipxic2,leni,icmotp,ierr)
      call cmo_get_info('yic',cmonew,ipyic2,leni,icmotp,ierr)
      call cmo_get_info('zic',cmonew,ipzic2,leni,icmotp,ierr)
      call cmo_get_info('itetclr',cmonew,
     *                  ipitetclr2,leni,icmotp,ier)
      call cmo_get_info('itettyp',cmonew,
     *                  ipitettyp2,leni,icmotp,ier)
      call cmo_get_info('itetoff',cmonew,
     *                  ipitetoff2,leni,icmotp,ier)
      call cmo_get_info('jtetoff',cmonew,
     *                  ipjtetoff2,leni,icmotp,ier)
      call cmo_get_info('itet',cmonew,ipitet2,leni,icmotp,ierr)
      call cmo_get_info('jtet',cmonew,ipjtet2,leni,icmotp,ierr)
C
      call cmo_get_info('imt1',cmo,ipimt1,leni,icmotp,ierr)
      call cmo_get_info('itp1',cmo,ipitp1,leni,icmotp,ierr)
      call cmo_get_info('xic',cmo,ipxic1,leni,icmotp,ierr)
      call cmo_get_info('yic',cmo,ipyic1,leni,icmotp,ierr)
      call cmo_get_info('zic',cmo,ipzic1,leni,icmotp,ierr)
      call cmo_get_info('itetclr',cmo,
     *                  ipitetclr1,leni,icmotp,ier)
      call cmo_get_info('itettyp',cmo,
     *                  ipitettyp1,leni,icmotp,ier)
      call cmo_get_info('itetoff',cmo,
     *                  ipitetoff1,leni,icmotp,ier)
      call cmo_get_info('jtetoff',cmo,
     *                  ipjtetoff1,leni,icmotp,ier)
      call cmo_get_info('itet',cmo,ipitet1,leni,icmotp,ierr)
      call cmo_get_info('jtet',cmo,ipjtet1,leni,icmotp,ierr)
C
      nptsl=mpno1
      do i=1,mpno1
         i1=mpary1(i)
         imt2(nnodes2+i)=imt1(i1)
         itp2(nnodes2+i)=itp1(i1)
         xic2(nnodes2+i)=xic1(i1)
         yic2(nnodes2+i)=yic1(i1)
         zic2(nnodes2+i)=zic1(i1)
      enddo
      do i=1,mpno2
         i1=mpary2(i)
         imt2(nnodes2+nptsl*(2+ilayers-1)+i)=imt1(i1)
         itp2(nnodes2+nptsl*(2+ilayers-1)+i)=itp1(i1)
         xic2(nnodes2+nptsl*(2+ilayers-1)+i)=xic1(i1)
         yic2(nnodes2+nptsl*(2+ilayers-1)+i)=yic1(i1)
         zic2(nnodes2+nptsl*(2+ilayers-1)+i)=zic1(i1)
      enddo
      if(ilayers.gt.0) then
         do l=2,(2+ilayers)-1
            dsfac=dble(l-1)/dble(ilayers+1)
            do i=1,mpno1
               i1=mpary1(i)
               dx1=xic2(nnodes2+nptsl*(2+ilayers-1)+i1)-xic2(nnodes2+i1)
               dy1=yic2(nnodes2+nptsl*(2+ilayers-1)+i1)-yic2(nnodes2+i1)
               dz1=zic2(nnodes2+nptsl*(2+ilayers-1)+i1)-zic2(nnodes2+i1)
               imt2(nnodes2+nptsl*(l-1)+i1)=imt2(nnodes2+i1)
               itp2(nnodes2+nptsl*(l-1)+i1)=itp2(nnodes2+i1)
               xic2(nnodes2+nptsl*(l-1)+i1)=xic2(nnodes2+i1)+dx1*dsfac
               yic2(nnodes2+nptsl*(l-1)+i1)=yic2(nnodes2+i1)+dy1*dsfac
               zic2(nnodes2+nptsl*(l-1)+i1)=zic2(nnodes2+i1)+dz1*dsfac
            enddo
         enddo
      endif
C
      length=nnodes
      call mmgetblk('ipflag1',isubname,ipipflag1,length,1,icscode)
      do i=1,nnodes
         ipflag1(i)=0
      enddo
      length=nelements
      call mmgetblk('itflag1',isubname,ipitflag1,length,1,icscode)
      do it=1,nelements
         itflag1(it)=0
      enddo
      do i=1,mpno1
         i1=mpary1(i)
         ipflag1(i1)=i
      enddo
      do it=1,nelements
         iflag=0
         do i=1,nelmnen(itettyp1(it))
            i1=itet1(itetoff1(it)+i)
            if(ipflag1(i1).gt.0) then
               iflag=iflag+1
            endif
         enddo
         if(iflag.eq.nelmnen(itettyp1(it))) then
            itflag1(it)=1
         endif
      enddo
C
      if(nelements2.le.0) then
         itetcnt=0
         itoff=0
         jtoff=0
      else
         itetcnt=nelements2
         itoff=itetoff2(nelements2)+nelmnen(itettyp2(nelements2))
         jtoff=jtetoff2(nelements2)+nelmnef(itettyp2(nelements2))
      endif
      do l=1,2+ilayers-1
         do it=1,nelements
            if(itflag1(it).eq.1) then
               if(itettyp1(it).eq.ifelmlin) then
                  i1=nnodes2+nptsl*(l-1)+itet1(itetoff1(it)+1)
                  i2=nnodes2+nptsl*(l-1)+itet1(itetoff1(it)+2)
                  i4=nptsl+i1
                  i3=nptsl+i2
                  itetcnt=itetcnt+1
                  itetclr2(itetcnt)=itetclr1(it)
                  itettyp2(itetcnt)=ifelmqud
                  itetoff2(itetcnt)=itoff
                  jtetoff2(itetcnt)=jtoff
                  itoff=itoff+nelmnen(itettyp2(itetcnt))
                  jtoff=jtoff+nelmnef(itettyp2(itetcnt))
                  itet2(itetoff2(itetcnt)+1)=i1
                  itet2(itetoff2(itetcnt)+2)=i2
                  itet2(itetoff2(itetcnt)+3)=i3
                  itet2(itetoff2(itetcnt)+4)=i4
                  do i=1,nelmnef(itettyp2(it))
                     jtet2(jtetoff2(itetcnt)+i)=-1
                  enddo
                  do i=1,nelmnen(itettyp2(itetcnt))
                     i1temp=itet2(itetoff2(itetcnt)+i)
                     xicvol(i)=xic2(i1temp)
                     yicvol(i)=yic2(i1temp)
                     zicvol(i)=zic2(i1temp)
                  enddo
                  call volume_element(itettyp2(itetcnt),
     *                                xicvol,yicvol,zicvol,
     *                                xtetvol)
                  if(xtetvol.le.0.0d+00) then
                     itet2(itetoff2(itetcnt)+1)=i1
                     itet2(itetoff2(itetcnt)+2)=i4
                     itet2(itetoff2(itetcnt)+3)=i3
                     itet2(itetoff2(itetcnt)+4)=i2
                  endif
               elseif(itettyp1(it).eq.ifelmtri) then
                  i1=nnodes2+nptsl*(l-1)+itet1(itetoff1(it)+1)
                  i2=nnodes2+nptsl*(l-1)+itet1(itetoff1(it)+2)
                  i3=nnodes2+nptsl*(l-1)+itet1(itetoff1(it)+3)
                  i4=nptsl+i1
                  i5=nptsl+i2
                  i6=nptsl+i3
                  itetcnt=itetcnt+1
                  itetclr2(itetcnt)=itetclr1(it)
                  itettyp2(itetcnt)=ifelmpri
                  itetoff2(itetcnt)=itoff
                  jtetoff2(itetcnt)=jtoff
                  itoff=itoff+nelmnen(itettyp2(itetcnt))
                  jtoff=jtoff+nelmnef(itettyp2(itetcnt))
                  itet2(itetoff2(itetcnt)+1)=i1
                  itet2(itetoff2(itetcnt)+2)=i2
                  itet2(itetoff2(itetcnt)+3)=i3
                  itet2(itetoff2(itetcnt)+4)=i4
                  itet2(itetoff2(itetcnt)+5)=i5
                  itet2(itetoff2(itetcnt)+6)=i6
                  do i=1,nelmnef(itettyp2(it))
                     jtet2(jtetoff2(itetcnt)+i)=-1
                  enddo
                  do i=1,nelmnen(itettyp2(itetcnt))
                     i1temp=itet2(itetoff2(itetcnt)+i)
                     xicvol(i)=xic2(i1temp)
                     yicvol(i)=yic2(i1temp)
                     zicvol(i)=zic2(i1temp)
                  enddo
                  call volume_element(itettyp2(itetcnt),
     *                                xicvol,yicvol,zicvol,
     *                                xtetvol)
                  if(xtetvol.le.0.0d+00) then
C*****                     itet2(itetoff2(itetcnt)+1)=i1
C*****                     itet2(itetoff2(itetcnt)+2)=i3
C*****                     itet2(itetoff2(itetcnt)+3)=i2
C*****                     itet2(itetoff2(itetcnt)+4)=i4
C*****                     itet2(itetoff2(itetcnt)+5)=i6
C*****                     itet2(itetoff2(itetcnt)+6)=i5
                  endif
               elseif(itettyp1(it).eq.ifelmqud) then
                  i1=nnodes2+nptsl*(l-1)+itet1(itetoff1(it)+1)
                  i2=nnodes2+nptsl*(l-1)+itet1(itetoff1(it)+2)
                  i3=nnodes2+nptsl*(l-1)+itet1(itetoff1(it)+3)
                  i4=nnodes2+nptsl*(l-1)+itet1(itetoff1(it)+4)
                  i5=nptsl+i1
                  i6=nptsl+i2
                  i7=nptsl+i3
                  i8=nptsl+i4
                  itetcnt=itetcnt+1
                  itetclr2(itetcnt)=itetclr1(it)
                  itettyp2(itetcnt)=ifelmhex
                  itetoff2(itetcnt)=itoff
                  jtetoff2(itetcnt)=jtoff
                  itoff=itoff+nelmnen(itettyp2(itetcnt))
                  jtoff=jtoff+nelmnef(itettyp2(itetcnt))
                  itet2(itetoff2(itetcnt)+1)=i1
                  itet2(itetoff2(itetcnt)+2)=i2
                  itet2(itetoff2(itetcnt)+3)=i3
                  itet2(itetoff2(itetcnt)+4)=i4
                  itet2(itetoff2(itetcnt)+5)=i5
                  itet2(itetoff2(itetcnt)+6)=i6
                  itet2(itetoff2(itetcnt)+7)=i7
                  itet2(itetoff2(itetcnt)+8)=i8
                  do i=1,nelmnef(itettyp2(it))
                     jtet2(jtetoff2(itetcnt)+i)=-1
                  enddo
                  do i=1,nelmnen(itettyp2(itetcnt))
                     i1temp=itet2(itetoff2(itetcnt)+i)
                     xicvol(i)=xic2(i1temp)
                     yicvol(i)=yic2(i1temp)
                     zicvol(i)=zic2(i1temp)
                  enddo
                  call volume_element(itettyp2(itetcnt),
     *                                xicvol,yicvol,zicvol,
     *                                xtetvol)
                  if(xtetvol.le.0.0d+00) then
                     itet2(itetoff2(itetcnt)+1)=i1
                     itet2(itetoff2(itetcnt)+2)=i4
                     itet2(itetoff2(itetcnt)+3)=i3
                     itet2(itetoff2(itetcnt)+4)=i2
                     itet2(itetoff2(itetcnt)+5)=i5
                     itet2(itetoff2(itetcnt)+6)=i8
                     itet2(itetoff2(itetcnt)+7)=i7
                     itet2(itetoff2(itetcnt)+8)=i6
                     do i=1,nelmnen(itettyp2(itetcnt))
                        i1temp=itet2(itetoff2(itetcnt)+i)
                        xicvol(i)=xic2(i1temp)
                        yicvol(i)=yic2(i1temp)
                        zicvol(i)=zic2(i1temp)
                     enddo
                     call volume_element(itettyp2(itetcnt),
     *                                   xicvol,yicvol,zicvol,
     *                                   xtetvol)
                  endif
               else
                  print *,"Illegal element to extrude (interp): ",it
               endif
            endif
         enddo
      enddo
C
      call cmo_set_info('nelements',cmonew,itetcnt,1,1,ierr)
C
      ipointi=nnodes2+1
      ipointj=npoints_new
      call cmo_set_info('ipointi',cmonew,
     *                ipointi,1,1,icscode)
      if (icscode .ne. 0) call x3d_error(isubname,'cmo_get_info')
      call cmo_set_info('ipointj',cmonew,
     *                ipointj,1,1,icscode)
      if (icscode .ne. 0) call x3d_error(isubname,'cmo_get_info')
C
      call geniee_cmo(cmonew)
C
      goto 9999
 9999 continue
C
      call mmrelprt(isubname,icscode)
C
      return
      end
