*dk,try4to4x
      subroutine try4to4xv(iepos,it,iflag,ntet,mtet,
     *         xmegah,xmegadet,xmegaerr,
     *         npoints,ntets,toldamage,iopt2to2)
       implicit none
C
C ######################################################################
C
C     PURPOSE -
C
C        This routine attempts flipping 4-to-4 type connections.
C
C     INPUT ARGUMENTS -
C
C        iepos    - the edge position
C        it       - the tet number
C
C     OUTPUT ARGUMENTS -
C
C        iflag    - the success flag:
C                      0 => flip was not performed
C                      1 => flip was performed
C
C     CHANGE HISTORY -
C
C        $Log: try4to4xv.f,v $
C        Revision 2.00  2007/11/09 20:04:05  spchu
C        Import to CVS
C
CPVCS    
CPVCS       Rev 1.11   18 Jun 2003 08:12:04   dcg
CPVCS    check value of iopt2to2, if zero skip interface recons
CPVCS    
CPVCS       Rev 1.10   05 Jan 2001 12:58:08   dcg
CPVCS    use dble in place of float so as not to lose digits
CPVCS
CPVCS       Rev 1.9   Fri Aug 28 14:25:38 1998   dcg
CPVCS    remove single precision constants
CPVCS
CPVCS       Rev 1.8   Wed Jun 17 11:55:50 1998   dcg
CPVCS    call testdamage only if this is an interface flip
CPVCS
CPVCS       Rev 1.7   Tue Jun 16 13:26:22 1998   dcg
CPVCS    more changes for ivoronoi = -2
CPVCS
CPVCS       Rev 1.3   Tue May 26 17:09:28 1998   dcg
CPVCS    test for correct tet material types before attempting flip
C
C ######################################################################
C
      include "cmo.h"
      include "chydro.h"
      include "neibor.h"
C
C ######################################################################
C
      integer ntet(*),mtet(*),
     *  idold(16),idnew(16),jdold(16),jdnew(16)
      integer iepos,it,iflag, i,it2,it3,it4,it5,ifac,ifac2,ifac3,
     *      ifac4, ierror,  npoints,ntets,length,icmotype,ierflg,
     *      i1,i2,i3,i4,i5,i6,ipos2,ipos3,iflg1,iflg2,
     *      ifour,ier,j,k,kpe,nsd,n,m,iofs,jj,nmulti,ipar,i3b,i4b,
     *      ict,imt1a(200),ichain(200),imtx,iflg,ipar1,iopt2to2
      real*8 crosx1,crosy1,crosz1,volume,xxlarge,val1,val2,
     *     vol1,vol2,vol3,vol4,alargenumber,
     *     valinit,en,em,toldamage
      parameter (alargenumber= 1.0d+30)
      real*8 xmegah(*), xmegadet(*),xmegaerr(*)
      logical flip,interface1,interface2,itsttp
      data nmulti/200/
C
      crosx1(i,j,k)=(yic(j)-yic(i))*(zic(k)-zic(i))-
     *              (yic(k)-yic(i))*(zic(j)-zic(i))
      crosy1(i,j,k)=(xic(k)-xic(i))*(zic(j)-zic(i))-
     *              (xic(j)-xic(i))*(zic(k)-zic(i))
      crosz1(i,j,k)=(xic(j)-xic(i))*(yic(k)-yic(i))-
     *              (xic(k)-xic(i))*(yic(j)-yic(i))
      volume(i1,i2,i3,i4)=(xic(i4)-xic(i1))*crosx1(i1,i2,i3)+
     *                    (yic(i4)-yic(i1))*crosy1(i1,i2,i3)+
     *                    (zic(i4)-zic(i1))*crosz1(i1,i2,i3)
C
C ######################################################################
C
C
C
C     ******************************************************************
C     FETCH MESH OBJECT NAME AND POINTER INFORMATION.
C
      if(icmoget.eq.1) then
C
      call cmo_get_name(cmo,ierror)
C
      call cmo_get_info('mbndry',cmo,mbndry,length,icmotype,ierror)
      call cmo_get_info('xic',cmo,ipxic,length,icmotype,ierror)
      call cmo_get_info('yic',cmo,ipyic,length,icmotype,ierror)
      call cmo_get_info('zic',cmo,ipzic,length,icmotype,ierror)
      call cmo_get_info('itetclr',cmo,ipitetclr,length,icmotype,ier)
      call cmo_get_info('itet',cmo,ipitet,length,icmotype,ierror)
      call cmo_get_info('jtet',cmo,ipjtet,length,icmotype,ierror)
C
      endif
C
C     ******************************************************************
C
C
      xxlarge=alargenumber
      val1=xxlarge
      val2=xxlarge
      valinit=1.0e-14
      iflag=0
      ifour=4
C
C     ***************************************************************
C
C     DETERMINE WHETHER OR NOT A 4-TO-4 FLIP IS POSSIBLE
C
      interface1=.false.
      interface2=.false.
      iofs=4*(iepos-1)
      ifac3 = jtet(ielist(iofs+3),it)
      if(iopt2to2.eq.0.and.ifac3.ge.mbndry) go to 9999
      if (ifac3.gt.mbndry) then
         ifac3=ifac3-mbndry
         interface1=.true.
      endif
      ifac4 = jtet(ielist(iofs+4),it)
      if(iopt2to2.eq.0.and.ifac4.ge.mbndry) go to 9999
      if (ifac4.gt.mbndry) then
         ifac4=ifac4-mbndry
         interface2=.true.
      endif
      if(ifac3 .eq.mbndry.or.
     *   ifac4.eq.mbndry) then
            goto 9999
      endif
      it2=0.25*dble(ifac3)+0.9
      it3=0.25*dble(ifac4)+0.9
      i1=itet(ielist(iofs+1),it)
      i2=itet(ielist(iofs+2),it)
      i3=itet(ielist(iofs+3),it)
      i4=itet(ielist(iofs+4),it)
      i5=itet1(ifac3)
      i6=itet1(ifac4)
C
      ipos2=0
      ipar1=0
      if(interface1) then
         call getchain(i4,
     *             ichain,imt1a,nmulti,ict,ipar)
         if(itsttp('intrface',itp1(itet(1,it2))))call getchain(
     *             itet(1,it2),ichain,imt1a,nmulti,ict,ipar1)
         if (ipar.eq.ipar1) ipos2=1
         if(itsttp('intrface',itp1(itet(2,it2))))call getchain(
     *             itet(2,it2),ichain,imt1a,nmulti,ict,ipar1)
         if (ipar.eq.ipar1) ipos2=2
         if(itsttp('intrface',itp1(itet(3,it2))))call getchain(
     *             itet(3,it2),ichain,imt1a,nmulti,ict,ipar1)
         if (ipar.eq.ipar1) ipos2=3
         if(itsttp('intrface',itp1(itet(4,it2))))call getchain(
     *             itet(4,it2),ichain,imt1a,nmulti,ict,ipar1)
         if (ipar.eq.ipar1) ipos2=4
      else
         if(itet(1,it2).eq.i4) ipos2=1
         if(itet(2,it2).eq.i4) ipos2=2
         if(itet(3,it2).eq.i4) ipos2=3
         if(itet(4,it2).eq.i4) ipos2=4
      endif
      if(ipos2.eq.0) go to 9999
      ifac = jtet(ipos2,it2)
      if(iopt2to2.eq.0.and.ifac.ge.mbndry) go to 9999
      if (ifac.gt.mbndry) then
         ifac=ifac-mbndry
         if(.not.interface2)go to 9999
      else
         if(interface2) go to 9999
      endif
      if (ifac.eq.mbndry.or.kfix(1,it2).ne.0.or.
     *   kfix(2,it2).ne.0.or.kfix(3,it2).ne.0.or.
     *   kfix(4,it2).ne.0) then
         goto 9999
      endif
      it4=0.25*dble(ifac)+0.9
      if(it4.eq.it3.or.it4.eq.it) go to 9999
C
      ipos3=0
      ipar1=0
      if(interface2) then
         call getchain(i3,
     *             ichain,imt1a,nmulti,ict,ipar)
         if(itsttp('intrface',itp1(itet(1,it3))))call getchain(
     *             itet(1,it3),ichain,imt1a,nmulti,ict,ipar1)
         if (ipar.eq.ipar1) ipos3=1
         if(itsttp('intrface',itp1(itet(2,it3))))call getchain(
     *             itet(2,it3),ichain,imt1a,nmulti,ict,ipar1)
         if (ipar.eq.ipar1) ipos3=2
        if(itsttp('intrface',itp1(itet(3,it3)))) call getchain(
     *             itet(3,it3),ichain,imt1a,nmulti,ict,ipar1)
         if (ipar.eq.ipar1) ipos3=3
        if(itsttp('intrface',itp1(itet(4,it3)))) call getchain(
     *             itet(4,it3),ichain,imt1a,nmulti,ict,ipar1)
         if (ipar.eq.ipar1) ipos3=4
      else
         if(itet(1,it3).eq.i3) ipos3=1
         if(itet(2,it3).eq.i3) ipos3=2
         if(itet(3,it3).eq.i3) ipos3=3
         if(itet(4,it3).eq.i3) ipos3=4
      endif
      if(ipos3.eq.0) go to 9999
      ifac2 = jtet(ipos3,it3)
      if (ifac2.gt.mbndry) then
         ifac2=ifac2-mbndry
         if(.not.interface1) go to 9999
      else
         if(interface1) go to 9999
      endif
      if (ifac2.eq.mbndry.or.kfix(1,it3).ne.0.or.
     *   kfix(2,it3).ne.0.or.kfix(3,it3).ne.0.or.
     *   kfix(4,it3).ne.0) then
         goto 9999
      endif
      it5=0.25*dble(ifac2)+0.9
C
      if(it4.ne.it5) then
         goto 9999
      endif
      vol1=volume(itet(1,it),itet(2,it),itet(3,it),itet(4,it))
      vol2=volume(itet(1,it2),itet(2,it2),itet(3,it2),itet(4,it2))
      vol3=volume(itet(1,it3),itet(2,it3),itet(3,it3),itet(4,it3))
      vol4=volume(itet(1,it4),itet(2,it4),itet(3,it4),itet(4,it4))
      if(  min(vol1,vol2,vol3,vol4).le.0) goto 9999
C
C     _________________________________________________________
C
C     EVALUATE THE TWO 4-TO-4 POSSIBILITIES
C
C   try flip to connection i3-i5 if ifac4 and ifac are either
C   both interface or all are interior faces
C
      iflg1=0
      iflg2=0
      if((.not.interface2.and..not.interface1).or.
     *   (interface2)) then
         iflg=1
         if(interface2)call testdamage(i1,i3,i2,i5,iflg,toldamage)
         if (iflg.eq.1) then
            call test4to4(i3,i1,i6,i2,i4,i5,iflg1)
         else
            iflg1=0
         endif
      endif
      if (iflg1.eq.1)  then
           if ((imt1(itet(1,it)).ne.imt1(itet(1,it3))).or.
     *       (imt1(itet(1,it2)).ne.imt1(itet(1,it4)))) go to 125
           n=4
           ntet(1)=it
           ntet(2)=it2
           ntet(3)=it3
           ntet(4)=it4
           do j=1,n
              xmegadet(ntet(j))=-1.0
              xmegaerr(ntet(j))=-1.0
           enddo
           m=4
           mtet(1)=ntets+1
           mtet(2)=ntets+2
           mtet(3)=ntets+3
           mtet(4)=ntets+4
           do j=1,m
              xmegadet(mtet(j))=-1.0
              xmegaerr(mtet(j))=-1.0
           enddo
           itet(1,mtet(1))=i5
           itet(2,mtet(1))=i2
           itet(3,mtet(1))=i3
           itet(4,mtet(1))=i4
           itet(1,mtet(2))=i3
           itet(2,mtet(2))=i1
           itet(3,mtet(2))=i5
           itet(4,mtet(2))=i4
           itet(1,mtet(3))=i3
           itet(2,mtet(3))=i5
           itet(3,mtet(3))=i6
           itet(4,mtet(3))=i2
           itet(1,mtet(4))=i1
           itet(2,mtet(4))=i3
           itet(3,mtet(4))=i5
           itet(4,mtet(4))=i6
           kpe=4
           nsd=3
           call b3dnxm (n,ntet,en,m,mtet,em,
     *                         npoints,nsd,xic,yic,zic,xmegah,
     *                         ntets,kpe,itet,xmegadet,xmegaerr,
     *                         flip)
           if(flip) then
C
C  do the flip
C
             iofs=4*(iepos-1)
             i1=itet(ielist(iofs+1),it)
             i2=itet(ielist(iofs+2),it)
             i3=itet(ielist(iofs+3),it)
             i4=itet(ielist(iofs+4),it)
             i5=itet1(ifac3)
             i6=itet1(ifac4)
             if (interface2) then
                imtx=imt1(i6)
                call getchain(i3,
     *             ichain,imt1a,nmulti,ict,ipar)
                 if (ict.eq.0) go to 9999
                 do jj=1,ict
                    if (imt1a(jj) .eq. imtx) then
                       i3b=ichain(jj)
                       goto 120
                    endif
                enddo
 120            continue
                call fnd4to4i(it,it2,it3,it4,i1,i2,i3,i3b,i4,
     *            i5,i6,idold,jdold,
     *            npoints,ntets)
                call flp4to4i(it,it2,it3,it4,idold,jdold,npoints,
     *            ntets)
             else
                call find4to4(it,iepos,it2,it3,it4,i1,i2,i3,i4,
     *            i5,i6,1,idold,jdold,idnew,jdnew,
     *            npoints,ntets,ierflg)
                call flip4to4(it,it2,it3,it4,idold,jdold,npoints,
     *            ntets)
             endif
             iflag=1
             go to 9999
           endif
      endif
C
C   flip to connection i4-i6 if ifac3 and ifac2 are either
C   both interface or all are interior faces
C
 125  if((.not.interface2.and..not.interface1).or.
     *   (interface1)) then
         iflg=1
         if(interface1)call testdamage(i1,i4,i2,i6,iflg,toldamage)
         if (iflg.eq.1) then
            call test4to4(i4,i1,i3,i2,i5,i6,iflg2)
         else
            iflg2=0
         endif
      endif
      if (iflg2.eq.1) then
           if ((imt1(itet(1,it)).ne.imt1(itet(1,it3))).or.
     *       (imt1(itet(1,it2)).ne.imt1(itet(1,it4)))) go to 9999
           n=4
           ntet(1)=it
           ntet(2)=it2
           ntet(3)=it3
           ntet(4)=it4
           do j=1,n
              xmegadet(ntet(j))=-1.0
              xmegaerr(ntet(j))=-1.0
           enddo
           m=4
           mtet(1)=ntets+1
           mtet(2)=ntets+2
           mtet(3)=ntets+3
           mtet(4)=ntets+4
           do j=1,m
              xmegadet(mtet(j))=-1.0
              xmegaerr(mtet(j))=-1.0
           enddo
           itet(1,mtet(1))=i2
           itet(2,mtet(1))=i6
           itet(3,mtet(1))=i4
           itet(4,mtet(1))=i3
           itet(1,mtet(2))=i1
           itet(2,mtet(2))=i6
           itet(3,mtet(2))=i3
           itet(4,mtet(2))=i4
           itet(1,mtet(3))=i2
           itet(2,mtet(3))=i6
           itet(3,mtet(3))=i5
           itet(4,mtet(3))=i4
           itet(1,mtet(4))=i1
           itet(2,mtet(4))=i5
           itet(3,mtet(4))=i6
           itet(4,mtet(4))=i4
           kpe=4
           nsd=3
           call b3dnxm (n,ntet,en,m,mtet,em,
     *                         npoints,nsd,xic,yic,zic,xmegah,
     *                         ntets,kpe,itet,xmegadet,xmegaerr,
     *                         flip)
           if(flip) then
C
C  do the flip
C
             iofs=4*(iepos-1)
             i1=itet(ielist(iofs+1),it)
             i2=itet(ielist(iofs+2),it)
             i3=itet(ielist(iofs+3),it)
             i4=itet(ielist(iofs+4),it)
             i5=itet1(ifac3)
             i6=itet1(ifac4)
             if (interface1) then
                imtx=imt1(i5)
                call getchain(i4,
     *             ichain,imt1a,nmulti,ict,ipar)
                 if (ict.eq.0) go to 9999
                 do jj=1,ict
                    if (imt1a(jj) .eq. imtx) then
                       i4b=ichain(jj)
                       goto 121
                    endif
                enddo
 121            continue
                call fnd4to4ix(it,it2,it3,it4,i1,i2,i3,i4b,i4,
     *            i5,i6,idold,jdold,
     *            npoints,ntets)
                call flp4to4i(it,it2,it3,it4,idold,jdold,npoints,
     *            ntets)
                itetclr(it2)=itetclr(it)
                itetclr(it3)=itetclr(it4)
             else
                call find4to4(it,iepos,it2,it3,it4,i1,i2,i3,i4,
     *            i5,i6,2,idold,jdold,idnew,jdnew,
     *            npoints,ntets,ierflg)
                call flip4to4(it,it2,it3,it4,idnew,jdnew,npoints,
     *            ntets)
             endif
             iflag=1
             go to 9999
           endif
      endif
C
      goto 9999
 9999 continue
      return
      end
