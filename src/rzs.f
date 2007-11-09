      subroutine rzs_lg(imsgin,xmsgin,cmsgin,msgtype,nwds,ierr)
C#######################################################################
C
C     PURPOSE -
C
C
C     Wrapper ROUTINE to rzs
C
C        $Log: rzs.f,v $
C        Revision 2.00  2007/11/09 20:04:02  spchu
C        Import to CVS
C
C
CPVCS
CPVCS       Rev 1.21   02 Oct 2007 12:40:28   spchu
CPVCS    original version
C
C#######################################################################
      implicit none
      include 'chydro.h'
      integer ierr, nwds, imsgin(*),msgtype(*)
      real*8 xmsgin(*)
      character*(*) cmsgin(nwds)
      character*32 cmo,isubname
      integer ierror,ipointi,icscode,ipointj,npoints_save,icount,
     * itype,nrt,nptot,irz,irratio,npoints,ilen,ityp
      real*8 ri,rf,xcn,ycn,zcn,rrz
C#######################################################################
      isubname='rzs'
      ierr=0
C
      call cmo_get_name(cmo,ierror)
      call cmo_get_info('nnodes',cmo,npoints,ilen,ityp,ierr)
C
      call cmo_get_info('ipointi',cmo,
     *                      ipointi,ilen,ityp,icscode)
      if (icscode .ne. 0) call x3d_error(isubname,'cmo_get_info')
      call cmo_get_info('ipointj',cmo,
     *                      ipointj,ilen,ityp,icscode)
      if (icscode .ne. 0) call x3d_error(isubname,'cmo_get_info')
C
      npoints_save=ipointj
C
C           ------------------------------------------------------------
C           TRANSFORM POINTS AND VELOCITIES TO LOCAL COORD. SYSTEM
C
      if (normflgc .gt. 0) call chglocl(1,npoints,1)
C
      icount=ipointj
      itype=imsgin(2)
      nrt=imsgin(3)
      nptot=imsgin(4)
      ri=xmsgin(5)
      rf=xmsgin(6)
      xcn=xmsgin(7)
      ycn=xmsgin(8)
      zcn=xmsgin(9)
      irz=imsgin(10)
      irratio=imsgin(11)
      rrz=xmsgin(12)
C
      npoints=icount+nptot
      call cmo_set_info('nnodes',cmo,npoints,1,1,ierror)
      call cmo_newlen(cmo,ierror)
C
      if(msgtype(2).eq.3. and. cmsgin(2)(1:7).eq.'diamond') then
         call diamond(itype,icount,nrt,nptot,ri,rf,xcn,ycn,zcn,
     $          irz,irratio,rrz)
      else
         call rzs(itype,icount,nrt,nptot,ri,rf,xcn,ycn,zcn,
     $          irz,irratio,rrz)
      endif
C
      npoints=icount
      call cmo_set_info('nnodes',cmo,npoints,1,1,ierror)
      ipointi=npoints_save+1
      ipointj=icount
      call cmo_get_name(cmo,ierror)
C
      call cmo_set_info('ipointi',cmo,
     *                      ipointi,1,1,icscode)
      if (icscode .ne. 0) call x3d_error(isubname,'cmo_get_info')
      call cmo_set_info('ipointj',cmo,
     *                      ipointj,1,1,icscode)
      if (icscode .ne. 0) call x3d_error(isubname,'cmo_get_info')
C
C
C           ------------------------------------------------------------
C           TRANSFORM POINTS AND VELOCITIES TO NORMAL COORD. SYSTEM
C
      if (normflgc .gt. 0) call chgnorm(1,npoints,1)
      return
      end
C
c
      subroutine rzs(itype,icount,nr,npt,ri,rf,xcn,ycn,zcn,
     *  irz,irratio,rrz)
C
C#######################################################################
C
C     PURPOSE -
C
C
C     THIS ROUTINE IS USED TO RATIO ZONE A REGION OF SPACE USING SPHERES
C
C
C     FORMAT: RZS/ITYPE/NR,NPT,RI,RF/XCN,YCN,ZCN/IRZ,IRRATIO,RRZ
C
C
C
C     THE REFLECTIVE OR ABSOLUTE ZONING SWITCHES
C
C         IRZ = 0 ==> MIN AND MAX X-VALUES ARE USED AS REFLECTIVE POINTS
C         IRZ = 1 ==> MIN AND MAX X-VALUES ARE USED AS ABSOLUTE POINTS
C
C
C
C     THE RATIO ZONING SWITCHES
C
C         IRRATIO = 0  ==> NO RATIO ZONING IN THE X-DIRECTION
C         IRRATIO = 1  ==> RATIO ZONING IN THE X-DIRECTION
C
C
C     THE RATIO VALUE VALUES
C
C        RRZ = THE RATIO ZONING VALUE FOR THE X-DIRECTION
C
C
C
C     INPUT ARGUMENTS -
C
C
C
C     OUTPUT ARGUMENTS -
C
C
C
C     CHANGE HISTORY -
C
C        HT0823AA-87, FO0828AA-89
C
C
C#######################################################################
C
C
      implicit real*8 (a-h, o-z)
      include 'consts.h'
      character*132 logmess
C
C#######################################################################
C
      icntin=icount
      if(irz.ne.0.and.irz.ne.1) irz=0
      if(rrz.le.0.0) rrz=1.0
      if(irz.eq.0) then
         nr1=nr
         rsum=0.0
         if(nr1.eq.1) goto 31
         do 30 i1=1,nr1-1
            rsum=rsum+rrz**float(i1)
 30      continue
 31      continue
         rsumr=0.5+rsum+0.5*rrz**float(nr1+1)
         drrel1=(rf-ri)/rsumr
         dr1=drrel1
         r11=ri-0.5*dr1
      else
         rsum=0.0
         nr1=nr
         if(nr1.lt.2) nr1=2
         if(nr1.eq.2) goto 41
         do 40 i1=1,nr1-2
            rsum=rsum+rrz**float(i1)
 40      continue
 41      continue
         rsumr=1.0+rsum
         drabs1=(rf-ri)/rsumr
         dr1=drabs1/rrz
         r11=ri-dr1
      endif
C
C
C     define a radial setup
C
      if(itype.eq.8) then
         ibrick=1
         call rzsbrick(itype,icount,nr,npt,ri,rf,xcn,ycn,zcn,
     *                 irz,irratio,rrz,
     *                 ibrick)
      else
C
         r1=r11
         dr=dr1
         do 110 ir=1,nr1
            r1=r1+dr
            if(iabs(itype).eq.1) then
               call sphere1(itype,icount,npt,r1,xcn,ycn,zcn,zero)
            elseif(iabs(itype).eq.2) then
               call sphere2(itype,icount,npt,r1,xcn,ycn,zcn,zero)
            endif
 120        continue
            dr=cvmgtr(dr*rrz,dr,irratio.eq.1)
  110    continue
      endif
C
C     ******************************************************************
C     PRINT OUT THE POINT NUMBERS GENERATED
C
      write(logmess,6000) icntin+1,icount
 6000 format('  RZS GENERATED POINTS ',i6,' TO ',i6)
      call writloga('default',0,logmess,0,ierrw)
C
      goto 9999
 9999 continue
      return
      end
c
      subroutine rzsbrick(itype,icount,nr,npt,ri,rf,xcn,ycn,zcn,
     *                    irz,irratio,rrz,
     *                    ibrick)
C
C#######################################################################
C
C     PURPOSE -
C
C
C     THIS ROUTINE IS USED TO RATIO ZONE A REGION OF SPACE USING SPHERES
C
C
C     FORMAT: RZS/ITYPE/NR,NPT,RI,RF/XCN,YCN,ZCN/IRZ,IRRATIO,RRZ
C
C
C
C     THE REFLECTIVE OR ABSOLUTE ZONING SWITCHES
C
C         IRZ = 0 ==> MIN AND MAX X-VALUES ARE USED AS REFLECTIVE POINTS
C         IRZ = 1 ==> MIN AND MAX X-VALUES ARE USED AS ABSOLUTE POINTS
C
C
C
C     THE RATIO ZONING SWITCHES
C
C         IRRATIO = 0  ==> NO RATIO ZONING IN THE X-DIRECTION
C         IRRATIO = 1  ==> RATIO ZONING IN THE X-DIRECTION
C
C
C     THE RATIO VALUE VALUES
C
C        RRZ = THE RATIO ZONING VALUE FOR THE X-DIRECTION
C
C
C
C     INPUT ARGUMENTS -
C
C
C
C     OUTPUT ARGUMENTS -
C
C
C
C     CHANGE HISTORY -
C
C        HT0823AA-87, FO0828AA-89
C
C
C#######################################################################
C
      implicit real*8 (a-h, o-z)
      character*132 logmess
C
C#######################################################################
C
C
      include 'chydro.h'
      include 'local_element.h'
C
      pointer (ipxic, xic)
      pointer (ipyic, yic)
      pointer (ipzic, zic)
      REAL*8 xic(1000000), yic(1000000), zic(1000000)
C
      pointer (ipitetclr, itetclr )
      integer itetclr(1000000)
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
C#######################################################################
C
      character*32 iblkname, isubname
C
C#######################################################################
C
      character*32 cmohex, cmo
C
      real*8 xicvol(100), yicvol(100), zicvol(100)
C
C#######################################################################
C
C
      iblkname='NOTSET'
      isubname='global'
CC
      icntin=icount
      if(irz.ne.0.and.irz.ne.1) irz=0
      if(rrz.le.0.0) rrz=1.0
      if(irz.eq.0) then
         nr1=nr
         rsum=0.0
         if(nr1.eq.1) goto 31
         do 30 i1=1,nr1-1
            rsum=rsum+rrz**float(i1)
 30      continue
 31      continue
         rsumr=0.5+rsum+0.5*rrz**float(nr1+1)
         drrel1=(rf-ri)/rsumr
         dr1=drrel1
         r11=ri-0.5*dr1
      else
         rsum=0.0
         nr1=nr
         if(nr1.lt.2) nr1=2
         if(nr1.eq.2) goto 41
         do 40 i1=1,nr1-2
            rsum=rsum+rrz**float(i1)
 40      continue
 41      continue
         rsumr=1.0+rsum
         drabs1=(rf-ri)/rsumr
         dr1=drabs1/rrz
         r11=ri-dr1
      endif
C
C
C     define a radial setup
C
C
C     The number of points per sphere surface is the
C        total divided by the number of surfaces.
C
      nrt1=int(sqrt(float(npt+1)/10.0))
C
C     SINCE THE LOGICAL SIDE LENGTH OF A PATCH MUST BE
C       A POWER OF 2 ENFORCE THIS RESTRICTION.
C
      do 10 i1=0,100
         if(nrt1.ge.2**i1.and.nrt1.lt.2**(i1+1)) then
            if(nrt1.eq.2**i1) goto 11
            nrt1=2**i1
            write(logmess,9000) nrt1
            call writloga('default',0,logmess,0,ierr)
 9000       format('Sphere2: each patch will be ',i5,'**2 points')
            goto 11
         endif
 10      continue
 11      continue
C
C
C       CHECK TO SEE IF THERE IS ENOUGH WORK SPACE.
C
      if(itype.gt.0) then
         npts=(nrt1+1)*(nrt1+1)
      else
         npts=(4*nrt1)+(nrt1+1)+2*nrt1*nrt1
      endif
C
C
      call cmo_get_name(cmohex,ierror)
      call cmo_get_info('nnodes',cmohex,npoints,
     *                  icmolen,icmotyp,ierror)
      call cmo_get_info('nelements',cmohex,nelm,
     *                  icmolen,icmotyp,ierror)
      nnodes=npoints+nr1*npts*10+1000
      if(ibrick.eq.1) then
         if(nr.eq.1) then
            numhex=nelm+nr1*nrt1*nrt1*10
            nsd=2
            nen=4
            nef=4
         else
            numhex=nelm+nr1*nrt1*nrt1*10
            nsd=3
            nen=8
            nef=6
         endif
      else
         numhex=0
         nsd=3
         nen=4
         nef=4
      endif
C
      call cmo_set_info('nnodes',cmohex,nnodes,1,1,ierror)
      call cmo_set_info('nelements',cmohex,numhex,1,1,ierror)
      call cmo_set_info('ndimensions_geom',cmohex,nsd,1,1,ierror)
      call cmo_set_info('ndimensions_topo',cmohex,nsd,1,1,ierror)
      call cmo_set_info('nodes_per_element',cmohex,nen,1,1,ierror)
      call cmo_set_info('faces_per_element',cmohex,nef,1,1,ierror)
C
      call cmo_newlen(cmohex,ierror)
      call cmo_get_intinfo('mbndry',cmohex,mbndry,
     *                  lenxic,icmotype,ierror)
C
      ielement=nelm
C
      dr=dr1
      r1=r11
      do 110 ir=1,nr1
         r1=r1+dr
         if(iabs(itype).eq.8) then
            uradius=0.0
            icounts=icount
            call sphere2_brick(itype,icount,npt,r1,xcn,ycn,zcn,uradius,
     *                   nr1,ir,
     *                   ielement,ibrick)
         endif
 120     continue
         if(irratio.eq.1) then
            dr=dr*rrz
         else
            dr=dr
         endif
  110 continue
C
      cmo=cmohex
      call cmo_get_info('xic',cmo,ipxic,lenxic,icmotype,ierror)
      call cmo_get_info('yic',cmo,ipyic,lenyic,icmotype,ierror)
      call cmo_get_info('zic',cmo,ipzic,lenzic,icmotype,ierror)
      call cmo_get_info('itetclr',cmo,ipitetclr,lenitetclr,icmotype,ier)
      call cmo_get_info('itettyp',cmo,ipitettyp,lenitettyp,icmotype,ier)
      call cmo_get_info('itetoff',cmo,ipitetoff,lenitetoff,icmotype,ier)
      call cmo_get_info('jtetoff',cmo,ipjtetoff,lenjtetoff,icmotype,ier)
      call cmo_get_info('itet',cmo,ipitet,lenitet,icmotype,ier)
      call cmo_get_info('jtet',cmo,ipjtet,lenjtet,icmotype,ier)
C
      negvol=0
      do it=1,ielement
         do i=1,nelmnen(itettyp(it))
            i1=itet1(itetoff(it)+i)
            xicvol(i)=xic(i1)
            yicvol(i)=yic(i1)
            zicvol(i)=zic(i1)
         enddo
         call volume_element(itettyp(it),
     *                       xicvol,yicvol,zicvol,
     *                       xtetvol)
         if(itettyp(it).eq.ifelmhex) then
            if(xtetvol.le.0.0d+00) then
               negvol=negvol+1
               i1=itet1(itetoff(it)+1)
               i2=itet1(itetoff(it)+2)
               i3=itet1(itetoff(it)+3)
               i4=itet1(itetoff(it)+4)
               i5=itet1(itetoff(it)+5)
               i6=itet1(itetoff(it)+6)
               i7=itet1(itetoff(it)+7)
               i8=itet1(itetoff(it)+8)
               itet1(itetoff(it)+1)=i1
               itet1(itetoff(it)+2)=i4
               itet1(itetoff(it)+3)=i3
               itet1(itetoff(it)+4)=i2
               itet1(itetoff(it)+5)=i5
               itet1(itetoff(it)+6)=i8
               itet1(itetoff(it)+7)=i7
               itet1(itetoff(it)+8)=i6
            endif
         elseif(itettyp(it).eq.ifelmqud) then
            if(xtetvol.le.0.0d+00) then
               negvol=negvol+1
               i1=itet1(itetoff(it)+1)
               i2=itet1(itetoff(it)+2)
               i3=itet1(itetoff(it)+3)
               i4=itet1(itetoff(it)+4)
               itet1(itetoff(it)+1)=i1
               itet1(itetoff(it)+2)=i3
               itet1(itetoff(it)+3)=i2
               itet1(itetoff(it)+4)=i4
            endif
         endif
      enddo
C
C     ******************************************************************
C     PRINT OUT THE POINT NUMBERS GENERATED
C
      iein=nelm+1
      npoints=icount
      nelm=ielement
      call cmo_set_info('nnodes',cmohex,npoints,1,1,ierror)
      call cmo_set_info('nelements',cmohex,nelm,1,1,ierror)
C
      write(logmess,6000) icntin+1,icount,iein,nelm
 6000 format('  RZS GENERATED POINTS ',i6,' TO ',i6,
     *       ' ELEMENTS ',i6,' TO ',i6)
      call writloga('default',0,logmess,0,ierrw)
C
      goto 9999
 9999 continue
      return
      end
c
      subroutine sphere2_brick(itype,icount,nrt,radius,xc,yc,zc,uradius,
     *                         nr,ir,
     *                         ielement,ibrick)
C
C#######################################################################
C
C     PURPOSE -
C
C     THIS ROUTINE CONTROLS THE GENERATION OF POINTS ON A
C        SPHERE USING PAUL FREDRECKSON AND JOHN BAUMGARDNER ALGORITHM
C        WHICH GENERATES A SPHERE OF POINTS, AT A SPECIFIED
C        RADIUS, FROM THE GRIDDING OF AN ICOSAHEDRON ( OR 10 DIAMONDS
C        PLACED ON THE SURFACE OF THE SPHERE ).
C
C
C        FORMAT: SPHERE/itype=2/npt/radius/xcen/ycen/zcen/velocity
C
C
C     INPUT ARGUMENTS -
C
C
C
C     OUTPUT ARGUMENTS -
C
C
C
C     CHANGE HISTORY -
C
C        HT0823AA-87, HT0908AA-87, FO0210AA-89
C
C
C#######################################################################
C
      implicit real*8 (a-h, o-z)
C
C#######################################################################
C
C
C     ******************************************************************
C
      include 'chydro.h'
      include 'local_element.h'
C
C
C     ******************************************************************
C
C
C     *****************************************************************
C
      pointer (ipxic, xic)
      pointer (ipyic, yic)
      pointer (ipzic, zic)
      REAL*8 xic(1000000), yic(1000000), zic(1000000)
C
C
      pointer (ipitetclr, itetclr )
      integer itetclr(1000000)
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
C
C     ******************************************************************
C
      pointer ( ipt1 , dista(nrt) )
      pointer ( ipt2 , idista(nrt) )
      pointer ( ipt3 , xn(3*nrt) )
C
      character*132 isubname, cmo
C
C
C
C#######################################################################
C
C
      isubname='sphere2_brick'
C
      icenter=0
C
C
      icts=icount+1
      nrt1=int(sqrt(float(nrt+1)/10.0))
C
C     SINCE THE LOGICAL SIDE LENGTH OF A PATCH MUST BE
C       A POWER OF 2 ENFORCE THIS RESTRICTION.
C
      do 10 i1=0,100
         if(nrt1.ge.2**i1.and.nrt1.lt.2**(i1+1)) then
            if(nrt1.eq.2**i1) goto 11
            nrt1=2**i1
            write(*,9000) nrt1
C*****      write(logmess,9000) nrt1
C*****      call writloga('default',0,logmess,0,ierr)
 9000       format('Sphere2: each patch will be ',i5,'**2 points')
            goto 11
         endif
 10      continue
 11      continue
C
C
C       CHECK TO SEE IF THERE IS ENOUGH WORK SPACE.
C
      if(itype.gt.0) then
         npts=(nrt1+1)*(nrt1+1)
      else
         npts=(4*nrt1)+(nrt1+1)+2*nrt1*nrt1
      endif
C
C     ******************************************************************
C
C     GET MEMORY FOR LOCAL VARIABLES.
C
      lenmm1 = 10*npts+2*icenter
      lenmm2 = 3*npts+1000
      call mmgetblk('dista', isubname, ipt1, lenmm1,2, ics)
      call mmgetblk('idista', isubname, ipt2, lenmm1,2, ics)
      call mmgetblk('xn', isubname, ipt3, lenmm2,2, ics)
C
      call cmo_get_name(cmo,ierror)
      call cmo_get_info('xic',cmo,ipxic,lenxic,icmotype,ierror)
C
      length=icount+10*npts+2
      if(length.gt.lenxic) then
         call cmo_set_info('nnodes',cmo,length,1,1,ierror)
         call cmo_newlen(cmo,ierror)
      endif
C
      call cmo_get_info('xic',cmo,ipxic,lenxic,icmotype,ierror)
      call cmo_get_info('yic',cmo,ipyic,lenyic,icmotype,ierror)
      call cmo_get_info('zic',cmo,ipzic,lenzic,icmotype,ierror)
      call cmo_get_info('itetclr',cmo,ipitetclr,lenitetclr,icmotype,ier)
      call cmo_get_info('itettyp',cmo,ipitettyp,lenitettyp,icmotype,ier)
      call cmo_get_info('itetoff',cmo,ipitetoff,lenitetoff,icmotype,ier)
      call cmo_get_info('jtetoff',cmo,ipjtetoff,lenjtetoff,icmotype,ier)
      call cmo_get_info('itet',cmo,ipitet,lenitet,icmotype,ier)
      call cmo_get_info('jtet',cmo,ipjtet,lenjtet,icmotype,ier)
C
      if(ibrick.eq.0) then
         if(radius.le.1.0e-10) then
            icount=icount+1
            xic(icount)=0.0+xc
            yic(icount)=0.0+yc
            zic(icount)=0.0+zc
            goto 9999
         endif
C
         if(icenter.eq.1) then
            icount=icount+1
            xic(icount)=0.0
            yic(icount)=0.0
            zic(icount)=radius
            icount=icount+1
            xic(icount)=0.0
            yic(icount)=0.0
            zic(icount)=-radius
         endif
      endif
C
C
      do i4=1,10
         call ocgrid(xn,i4,nrt1)
         call occonv(itype,icount,xn,i4,nrt1,
     *               xic,yic,zic)
      enddo
C
      if(radius.gt.0.0) then
         do i1=icts,icount
            radxyz=sqrt(xic(i1)*xic(i1)+yic(i1)*yic(i1)+
     *               zic(i1)*zic(i1))
            xic(i1)=xic(i1)*radius/radxyz+xc
            yic(i1)=yic(i1)*radius/radxyz+yc
            zic(i1)=zic(i1)*radius/radxyz+zc
         enddo
      else
         do i1=icts,icount
            xic(i1)=xc
            yic(i1)=yc
            zic(i1)=zc
         enddo
      endif
C
C
C     SORT COORDINATES AND THROW OUT DUPLICATES.
C
      do i1=icts,icount
         dista(i1-icts+1)=0.0
         idista(i1-icts+1)=0
      enddo
      ict1=icts-1
      ict2=icts-1
      do i1=icts,icount
         ict1=ict1+1
         ict2=ict2+1
         idista(i1-icts+1)=ict2
      enddo
      goto 400
C
      idup=1
C     *** It would appear the duplicates must be removed.
      do i1=icts,icount
         dista(i1-icts+1)=0.0
         idista(i1-icts+1)=0
      enddo
      ict1=icts-1
      ict2=icts-1
 290  continue
         ict1=ict1+1
         if(ict1.gt.icount) goto 400
            if(idista(ict1-icts+1).ne.0) goto 290
            xa=xic(ict1)
            ya=yic(ict1)
            za=zic(ict1)
            do i1=ict1+1,icount
               xb=xic(i1)
               yb=yic(i1)
               zb=zic(i1)
               dista(i1-icts+1)=(xa-xb)*(xa-xb)+
     *                          (ya-yb)*(ya-yb)+
     *                          (za-zb)*(za-zb)
            enddo
            ict2=ict2+1
            idista(ict1-icts+1)=ict2
            if(idup.eq.1) then
               do i1=ict1+1,icount
                  if(dista(i1-icts+1).lt.1.0e-10) then
                     idista(i1-icts+1)=-ict2
                  endif
               enddo
            endif
         goto 290
 400  continue
      icount1=icts
      do 410 i1=icts+1,icount
         i2=idista(i1-icts+1)
         if(i2.gt.0) then
            icount1=max(icount1,i2)
            xic(i2)=xic(i1)
            yic(i2)=yic(i1)
            zic(i2)=zic(i1)
         else
            idista(i1-icts+1)=abs(i2)
         endif
 410  continue
      icount=icount1
 9997 continue
C
      if(ibrick.eq.1) then
         if(nr.eq.1.and.ir.eq.nr) then
            ie=ielement
            do k=1,10
               do j=1,nrt1
                  do i=1,nrt1
                     ie=ie+1
                     if1=(k-1)*(nrt1+1)*(nrt1+1)
C*****               itetclr(ie)=k
C*****               itetclr(ie)=ir
                     itettyp(ie)=ifelmqud
                     itetoff(ie)=nelmnen(ifelmqud)*(ie-1)
                     jtetoff(ie)=nelmnef(ifelmqud)*(ie-1)
                     index=itetoff(ie)
                     if(k.le.5) then
                       itet1(index+1)=idista(i  +(j-1)*(nrt1+1)+if1)
                       itet1(index+2)=idista(i+1+(j-1)*(nrt1+1)+if1)
                       itet1(index+3)=idista(i+1+    j*(nrt1+1)+if1)
                       itet1(index+4)=idista(i  +    j*(nrt1+1)+if1)
                     else
                       itet1(index+1)=idista(i  +(j-1)*(nrt1+1)+if1)
                       itet1(index+2)=idista(i+1+(j-1)*(nrt1+1)+if1)
                       itet1(index+3)=idista(i+1+    j*(nrt1+1)+if1)
                       itet1(index+4)=idista(i  +    j*(nrt1+1)+if1)
                     endif
                     index=jtetoff(ie)
                     do idum=1,nelmnef(itettyp(ie))
                        jtet1(index+idum)=-1
                     enddo
                  enddo
               enddo
            enddo
            ielement=ie
         elseif(ir.lt.nr) then
            ie=ielement
            if2=icount-icts+1
            do k=1,10
               do j=1,nrt1
                  do i=1,nrt1
                     ie=ie+1
                     if1=(k-1)*(nrt1+1)*(nrt1+1)
C*****               itetclr(ie)=k
C*****               itetclr(ie)=ir
                     itettyp(ie)=ifelmhex
                     itetoff(ie)=nelmnen(ifelmhex)*(ie-1)
                     jtetoff(ie)=nelmnef(ifelmhex)*(ie-1)
                     index=itetoff(ie)
                     if(k.le.5) then
                       itet1(index+1)=idista(i  +(j-1)*(nrt1+1)+if1)
                       itet1(index+2)=idista(i+1+(j-1)*(nrt1+1)+if1)
                       itet1(index+3)=idista(i+1+    j*(nrt1+1)+if1)
                       itet1(index+4)=idista(i  +    j*(nrt1+1)+if1)
                       itet1(index+5)=idista(i  +(j-1)*(nrt1+1)+if1)+if2
                       itet1(index+6)=idista(i+1+(j-1)*(nrt1+1)+if1)+if2
                       itet1(index+7)=idista(i+1+    j*(nrt1+1)+if1)+if2
                       itet1(index+8)=idista(i  +    j*(nrt1+1)+if1)+if2
                     else
                       itet1(index+5)=idista(i  +(j-1)*(nrt1+1)+if1)
                       itet1(index+6)=idista(i+1+(j-1)*(nrt1+1)+if1)
                       itet1(index+7)=idista(i+1+    j*(nrt1+1)+if1)
                       itet1(index+8)=idista(i  +    j*(nrt1+1)+if1)
                       itet1(index+1)=idista(i  +(j-1)*(nrt1+1)+if1)+if2
                       itet1(index+2)=idista(i+1+(j-1)*(nrt1+1)+if1)+if2
                       itet1(index+3)=idista(i+1+    j*(nrt1+1)+if1)+if2
                       itet1(index+4)=idista(i  +    j*(nrt1+1)+if1)+if2
                     endif
                     index=jtetoff(ie)
                     do idum=1,nelmnef(itettyp(ie))
                        jtet1(index+idum)=-1
                     enddo
                     do idum=1,8
C*****                  nnbox(idum,ie)=nnbox(idum,ie)+icts-1
                     enddo
                  enddo
               enddo
            enddo
            ielement=ie
         endif
      endif
C
C
      if(abs(uradius).lt.1.0e-15) goto 9998
      do 200 i1=icts,icount
         xdiff=xic(i1)-xc
         ydiff=yic(i1)-yc
         zdiff=zic(i1)-zc
         radxyz=sqrt(xdiff*xdiff+ydiff*ydiff+zdiff*zdiff)
C
Cdcg     uic(i1)=uradius*xdiff/radxyz
Cdcg     vic(i1)=uradius*ydiff/radxyz
Cdcg     wic(i1)=uradius*zdiff/radxyz
 200  continue
C
C     ******************************************************************
C
C     RELEASE MEMORY FOR LOCAL VARIABLES IN THE PARTITION
C
 9998 continue
C
      goto 9999
 9999 continue
      call mmrelprt(isubname, ics)
      return
      end
 
