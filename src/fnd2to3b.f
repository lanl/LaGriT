*DK fnd2to3b
      subroutine fnd2to3b(it1,ipos1,it2,ipos2,it3,id,jd,
     *                    npoints,ntets)
C
C ######################################################################
C
C      PURPOSE -
C
C         This routine takes two tets which share a boundary face
C         into three tets:
C            (i1,i2,i3,i4) - (j1,j2,j3,xx)
C            (i1,i3,i2,i5) - (k1,k2,k3,xx)
C            -----------------------------------------
C            (i1,i2,i5,i4) - { (2,it2),(1,it3),j3,k2 }
C            (i2,i3,i5,i4) - { (2,it3),(1,it1),j1,k1 }
C            (i3,i1,i5,i4) - { (2,it1),(1,it2),j2,k3 }
C
C        The shape of the interface may be altered as a result of this
C        flip.
C
C      INPUT ARGUMENTS -
C
C         it1   - the first tet
C         ipos1 - the position of the common face in it1
C
C      OUTPUT ARGUMENTS -
C
C         it2   - the second tet
C         ipos2 - the position of the common face in it2
C         it3   - the new tet number(=ntets+1)
C         id    - the "itet" values of the three new tets
C         jd    - the "jtet" values of the three new tets
C
C     CHANGE HISTORY -
C
C        $Log: fnd2to3b.f,v $
C        Revision 2.00  2007/11/05 19:45:56  spchu
C        Import to CVS
C
CPVCS    
CPVCS       Rev 1.4   30 Sep 2004 10:00:34   dcg
CPVCS    use iand, ior in place of .and., .or. with integer variables
CPVCS    
CPVCS       Rev 1.3   07 Aug 2001 14:20:28   dcg
CPVCS    make implicit none
CPVCS
CPVCS       Rev 1.2   05 Jan 2001 12:57:12   dcg
CPVCS    use dble in place of float so as not to lose digits
CPVCS
CPVCS       Rev 1.1   Tue Feb 08 16:11:56 2000   dcg
CPVCS
CPVCS       Rev 1.0   Tue Feb 08 16:01:20 2000   dcg
CPVCS    Initial revision.
CPVCS
CPVCS       Rev 1.15   Tue Sep 22 11:48:16 1998   dcg
CPVCS    replace idebug.ne.0 with idebug.gt.0
CPVCS
CPVCS       Rev 1.14   Fri Sep 26 13:54:24 1997   dcg
CPVCS    refresh pointers after hmemadjb call
CPVCS
CPVCS       Rev 1.13   Thu Jul 03 15:34:32 1997   dcg
CPVCS    comment out call to rwdmpw
CPVCS
CPVCS       Rev 1.12   Mon Apr 14 16:49:18 1997   pvcs
CPVCS    No change.
CPVCS
CPVCS       Rev 1.11   Thu Jun 27 15:27:08 1996   dcg
CPVCS    add constrainv command to create xcontab table as needed
CPVCS
CPVCS       Rev 1.10   Mon Jun 03 15:14:04 1996   dcg
CPVCS    hp changes
CPVCS
CPVCS       Rev 1.9   Thu May 16 10:26:08 1996   dcg
CPVCS     changes for new interface type 3 and for new icontab, xcontab
CPVCS
CPVCS       Rev 1.8   Mon May 06 12:28:40 1996   dcg
CPVCS    use itsttp to test for point types
CPVCS
CPVCS       Rev 1.7   Tue Mar 05 12:49:28 1996   dcg
CPVCS    remove icn1, int1
CPVCS
CPVCS       Rev 1.6   Tue Feb 27 08:52:28 1996   dcg
CPVCS    check for existence of velocity, pressure, density
CPVCS
CPVCS       Rev 1.5   Fri Feb 23 16:33:20 1996   dcg
CPVCS    remove explicit references to uic,vic,wic,pic,ric,eic
CPVCS
CPVCS       Rev 1.4   12/02/94 16:19:02   dcg
CPVCS     changes required to compile on IBM RS6000
CPVCS
CPVCS       Rev 1.3   12/02/94 15:05:56   het
CPVCS    Added an option for the "cmo" access functions
CPVCS
CPVCS
CPVCS       Rev 1.2   12/01/94 18:48:02   het
CPVCS    Added a data type to the "cmo" calles
CPVCS       and added the "cmo.h" include file.
CPVCS
CPVCS       Rev 1.1   11/17/94 21:52:44   het
CPVCS    Added include files for chydro, neibor, cmerge, comdict. Added calles and
CPVCS    pointer statements for current_mesh_object database access.
C
C ######################################################################
C
      implicit none
      include "cmo.h"
      include "chydro.h"
      include "neibor.h"
      include "consts.h"
C
C ######################################################################
C
      integer nmulti
      parameter (nmulti = 200)
      integer id(12),jd(12),ix(3),ichain0(nmulti),imt0(nmulti),
     *          kpts(3),conopt(9)
      character*8 inodes(1),cdefname
      character*32 cvelnm,cdensnm,cpresnm,isubname
      logical itsttp
      logical ifvels,ifdens,ifpres
      pointer (ipxcontab,xcontab)
      real*8 xcontab(9,1000000)
      pointer (ipvels,vels)
      pointer (ipdens,dens)
      pointer (ippres,pres)
      real*8 vels(3,1000000),dens(1000000),pres(1000000)
      pointer(ipout,out)
      real*8 out(*),rout
      integer iout,ilen,ityp,leni,icmotype,ier,it1,ipos1,it2,it3,
     *  npoints,ione,i5,j1,j3,icr,jtemp,l1,l2,l3,k1,k2,k3,ipos,
     *  kpt,lenxic,icscode,isum5,isum4,isum3,isum2,isum1,it,ict45,
     *  ict123,kcr,isnx,index,ipari5,ichild2,ict0,ierrwrt,iposfnd,
     *  k,ict,ipar,i,imtx2,imtx1,j2,nintero,ipari1,newi5,ntets,ipos2
      real*8 wnorm,unew,vnew,wnew,vnorm,unorm,third
C
C#######################################################################
C
C     MACROS.
C
      integer i0,i1,i2,i3,i4
      iposfnd(i0,i1,i2,i3,i4)=min0(iabs(i1-i0)*4+1,iabs(i2-i0)*4+2,
     *                             iabs(i3-i0)*4+3,iabs(i4-i0)*4+4)
C
C ######################################################################
C
C
C
C     ******************************************************************
C     FETCH MESH OBJECT NAME AND POINTER INFORMATION.
C
      isubname='fnd2to3b'
      cdefname='default'
      if(icmoget.eq.1) then
C
      call cmo_get_name(cmo,ier)
C
      call cmo_get_info('mbndry',cmo,mbndry,leni,icmotype,ier)
      call cmo_get_info('isetwd',cmo,ipisetwd,leni,icmotype,ier)
      call cmo_get_info('imt1',cmo,ipimt1,leni,icmotype,ier)
      call cmo_get_info('itp1',cmo,ipitp1,leni,icmotype,ier)
      call cmo_get_info('icr1',cmo,ipicr1,leni,icmotype,ier)
      call cmo_get_info('isn1',cmo,ipisn1,leni,icmotype,ier)
      call cmo_get_info('xic',cmo,ipxic,leni,icmotype,ier)
      call cmo_get_info('yic',cmo,ipyic,leni,icmotype,ier)
      call cmo_get_info('zic',cmo,ipzic,leni,icmotype,ier)
      call cmo_get_attinfo('velname',cmo,iout,rout,cvelnm,
     *    ipout,ilen,ityp,ier)
      call cmo_get_info(cvelnm,cmo,ipvels,leni,icmotype,ier)
      if(ier.eq.0) then
         ifvels=.true.
      else
         ifvels=.false.
      endif
      call cmo_get_attinfo('densname',cmo,iout,rout,cdensnm,
     *    ipout,ilen,ityp,ier)
      call cmo_get_info(cdensnm,cmo,ipdens,leni,icmotype,ier)
      if(ier.eq.0) then
         ifdens=.true.
      else
         ifdens=.false.
      endif
      call cmo_get_attinfo('presname',cmo,iout,rout,cpresnm,
     *    ipout,ilen,ityp,ier)
      call cmo_get_info(cpresnm,cmo,ippres,leni,icmotype,ier)
      if(ier.eq.0) then
         ifpres=.true.
      else
         ifpres=.false.
      endif
      call cmo_get_info('itetclr',cmo,ipitetclr,leni,icmotype,ier)
      call cmo_get_info('itet',cmo,ipitet,leni,icmotype,ier)
      call cmo_get_info('jtet',cmo,ipjtet,leni,icmotype,ier)
      if (ifvels) then
         call cmo_get_info('xcontab',cmo,ipxcontab,ilen,ityp,ier)
         if(ier.ne.0) then
            call dotaskx3d('constrainv ; finish',ier)
            if(ier.ne.0) call x3d_error(isubname,'dotaskx3d')
         endif
      endif
C
      endif
C
C     ******************************************************************
C
C
      ione=1
      third=0.33333333333333
      icr=0
C
      jtemp=jtet(ipos1,it1)-mbndry
      it2=0.25*dble(jtemp)+0.9
      ipos2=iand((jtemp-1),maskface)+1
C
      i1=itet(iflist(3*ipos1-2),it1)
      i3=itet(iflist(3*ipos1-1),it1)
      i2=itet(iflist(3*ipos1  ),it1)
      i4=itet(ipos1,it1)
      i5=itet(ipos2,it2)
      j1=jtet(iflist(3*ipos1-2),it1)
      j3=jtet(iflist(3*ipos1-1),it1)
      j2=jtet(iflist(3*ipos1  ),it1)
C
C     ******************************************************************
C
C     GET THE COUPLED CHAIN LISTS FOR THE THREE FACE POINTS.
C
      imtx1=imt1(i1)
      imtx2=imt1(itet1(jtemp))
      kpts(1)=i1
      kpts(2)=i2
      kpts(3)=i3
      do 50 i=1,3
         ix(i)=0
         call getchain(kpts(i),ichain0,imt0,nmulti,ict,ipar)
         do 40 k=1,ict
            if (imt0(k) .eq. imtx2) then
               ix(i)=ichain0(k)
               goto 50
            endif
 40      continue
 50   continue
      if (ix(1).eq.0 .or. ix(2).eq.0 .or. ix(3).eq.0) call termcode(1)
C
C     ******************************************************************
C
C     DETERMINE THE ORIENTATION OF THE SECOND TETRAHEDRON.
C
      l1=iflist(3*ipos2-2)
      l2=iflist(3*ipos2-1)
      l3=iflist(3*ipos2)
                                k1=jtet(l1,it2)
      if(itet(l2,it2).eq.ix(1)) k1=jtet(l2,it2)
      if(itet(l3,it2).eq.ix(1)) k1=jtet(l3,it2)
                                k2=jtet(l1,it2)
      if(itet(l2,it2).eq.ix(3)) k2=jtet(l2,it2)
      if(itet(l3,it2).eq.ix(3)) k2=jtet(l3,it2)
                                k3=jtet(l1,it2)
      if(itet(l2,it2).eq.ix(2)) k3=jtet(l2,it2)
      if(itet(l3,it2).eq.ix(2)) k3=jtet(l3,it2)
C
C     ******************************************************************
C
C     CHANGE THE "jtet" POINTERS TO REFLECT THE NEW SHAPE OF THE
C     INTERFACE.
C
      if (k1 .lt. mbndry) then
         k1=k1+mbndry
      elseif (k1 .gt. mbndry) then
         ipos=iposfnd(ix(1),itet(1,it2),itet(2,it2),itet(3,it2),
     *                itet(4,it2))
         kpt=itet1(jtet(ipos,it2))
         if (imt1(kpt) .eq. imt1(i1)) k1=k1-mbndry
      endif
      if (k2 .lt. mbndry) then
         k2=k2+mbndry
      elseif (k2 .gt. mbndry) then
         ipos=iposfnd(ix(3),itet(1,it2),itet(2,it2),itet(3,it2),
     *                itet(4,it2))
         kpt=itet1(jtet(ipos,it2))
         if (imt1(kpt) .eq. imt1(i1)) k2=k2-mbndry
      endif
      if (k3 .lt. mbndry) then
         k3=k3+mbndry
      elseif (k3 .gt. mbndry) then
         ipos=iposfnd(ix(2),itet(1,it2),itet(2,it2),itet(3,it2),
     *                itet(4,it2))
         kpt=itet1(jtet(ipos,it2))
         if (imt1(kpt) .eq. imt1(i1)) k3=k3-mbndry
      endif
C
C     *****************************************************************
C
C     CHANGE POINT "i5" TO AN INTERFACE POINT IF IT IS NOT, AND UPDATE
C     MASS POINT VARIABLES.
C
      if (.not. (itsttp('matlintr',itp1(i5)))) then
C
C        ...............................................................
C        CHANGE A NON-INTERFACE POINT TO AN INTERFACE POINT, ADD THE
C        PARENT POINT, UPDATE SOME OF THE MASS POINT VARIABLES, AND
C        SAVE CHILD INFORMATION FOR A LATER UPDATING OF THE REST OF THE
C        VARIABLES.
C
         npoints=npoints+2
         if(icmoset.eq.1) then
            call cmo_set_info('nnodes',cmo,npoints,1,1,ier)
         endif
         call mmgetlen(ipxic,lenxic,icscode)
         if (npoints .gt. lenxic) then
            call hmemphy()
            if(icmoget.eq.1) then
            call cmo_get_info('mbndry',cmo,mbndry,leni,icmotype,ier)
            call cmo_get_info('isetwd',cmo,ipisetwd,leni,icmotype,
     *                        ier)
            call cmo_get_info('imt1',cmo,ipimt1,leni,icmotype,ier)
            call cmo_get_info('itp1',cmo,ipitp1,leni,icmotype,ier)
            call cmo_get_info('icr1',cmo,ipicr1,leni,icmotype,ier)
            call cmo_get_info('isn1',cmo,ipisn1,leni,icmotype,ier)
            call cmo_get_info('xic',cmo,ipxic,leni,icmotype,ier)
            call cmo_get_info('yic',cmo,ipyic,leni,icmotype,ier)
            call cmo_get_info('zic',cmo,ipzic,leni,icmotype,ier)
            call cmo_get_info(cvelnm,cmo,ipvels,leni,icmotype,ier)
            call cmo_get_info(cdensnm,cmo,ipdens,leni,icmotype,ier)
            call cmo_get_info(cpresnm,cmo,ippres,leni,icmotype,ier)
            call cmo_get_info('itetclr',cmo,ipitetclr,leni,
     *                        icmotype,ier)
            call cmo_get_info('itet',cmo,ipitet,leni,icmotype,ier)
            call cmo_get_info('jtet',cmo,ipjtet,leni,icmotype,ier)
            endif
         endif
         ipar=npoints
         newi5=npoints-1
         if (itp1(i5) .eq. ifitpint) then
            itp1(i5)=ifitpini
            write (logdan,1000) i5
 1000       format('   Change interior pt. to interface pt. ipt=',
     *           i6)
            call writloga("default",0,logdan,0,ierrwrt)
         elseif (itp1(i5).eq.ifitprfl.or.itp1(i5).eq.ifitpfre) then
            itp1(i5)=ifitpinb
            if (itp1(i5).eq.ifitpfre) itp1(i5)=ifitpifb
            icr=icr1(i5)
            if (icr .gt. 0) then
               icr1(ipar)=icr
               icr1(newi5)=icr
            endif
            write (logdan,1015) i5
 1015       format('   Change exterior pt. to interface pt. ipt='
     *         ,i6)
            call writloga("default",0,logdan,0,ierrwrt)
         endif
         itp1(ipar)=ifitpcup
         xic(ipar)=xic(i5)
         yic(ipar)=yic(i5)
         zic(ipar)=zic(i5)
         if (ifvels) then
            vels(1,ipar)=vels(1,i5)
            vels(2,ipar)=vels(2,i5)
            vels(3,ipar)=vels(3,i5)
         endif
         call getchain(i1,ichain0,imt0,nmulti,ict0,ipari1)
         imt1(ipar)=imt1(ipari1)
C*****   xmic(ipar)=xmic(ipari1)
         if(ifdens) dens(ipar)=dens(ipari1)
C*****   rsplic(ipar)=dens(ipar)
         isetwd(ipar)=isetwd(ipari1)
         itp1(newi5)=itp1(i5)
         imt1(newi5)=imt1(i1)
         ichild2=newi5
         write (logdan,1010)i5,newi5,ipar
 1010    format('   Append -   ipt=',i6,'   newipt=',i6,
     *           '   ipar=',i6)
         call writloga("default",0,logdan,0,ierrwrt)
         isn1(i5)=ichild2
         isn1(ichild2)=ipar
         isn1(ipar)=i5
C
C        ,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,
C        ADJUST MEMORY FOR INTERFACE ARRAY AND ADD THE NEW PARENT POINT.
C
         nintero=ninter
         inodes(1)='nodphy'
         call hmemadjb(inodes,1)
 
         call cmo_get_info('mbndry',cmo,mbndry,leni,icmotype,ier)
         call cmo_get_info('isetwd',cmo,ipisetwd,leni,icmotype,ier)
         call cmo_get_info('imt1',cmo,ipimt1,leni,icmotype,ier)
         call cmo_get_info('itp1',cmo,ipitp1,leni,icmotype,ier)
         call cmo_get_info('icr1',cmo,ipicr1,leni,icmotype,ier)
         call cmo_get_info('isn1',cmo,ipisn1,leni,icmotype,ier)
         call cmo_get_info('xic',cmo,ipxic,leni,icmotype,ier)
         call cmo_get_info('yic',cmo,ipyic,leni,icmotype,ier)
         call cmo_get_info('zic',cmo,ipzic,leni,icmotype,ier)
         call cmo_get_info(cvelnm,cmo,ipvels,leni,icmotype,ier)
         call cmo_get_info(cdensnm,cmo,ipdens,leni,icmotype,ier)
         call cmo_get_info(cpresnm,cmo,ippres,leni,icmotype,ier)
         call cmo_get_info('itetclr',cmo,ipitetclr,leni,icmotype,
     *                     ier)
         call cmo_get_info('itet',cmo,ipitet,leni,icmotype,ier)
         call cmo_get_info('jtet',cmo,ipjtet,leni,icmotype,ier)
 
C
         if (ninter .ne. nintero+1) call termcode(1)
         iinter(1,ninter)=ipar
C
C
      else
C
C        ...............................................................
C        IF "i5" IS AN INTERFACE POINT, SEE IF THE MATERIAL OF "it1" IS
C        IN THE CHAIN, AND ADD IT IF NECESSARY.
C
         call getchain(i5,ichain0,imt0,nmulti,ict0,ipari5)
         index=0
         do 75 k=1,ict0
            if (imt0(k) .eq. imt1(i1)) then
               index=k
               goto 60
            endif
 75      continue
C
 60      continue
         if (index .gt. 0) then
            newi5=ichain0(index)
            ichild2=0
         else
            npoints=npoints+1
            if(icmoset.eq.1) then
               call cmo_set_info('nnodes',cmo,npoints,1,1,ier)
            endif
            call mmgetlen(ipxic,lenxic,icscode)
            if (npoints .gt. lenxic) then
               call hmemphy()
               if(icmoget.eq.1) then
               call cmo_get_info('mbndry',cmo,mbndry,leni,
     *                           icmotype,ier)
               call cmo_get_info('isetwd',cmo,ipisetwd,leni,
     *                           icmotype,ier)
               call cmo_get_info('imt1',cmo,ipimt1,leni,icmotype,ier)
               call cmo_get_info('itp1',cmo,ipitp1,leni,icmotype,ier)
               call cmo_get_info('icr1',cmo,ipicr1,leni,icmotype,ier)
               call cmo_get_info('isn1',cmo,ipisn1,leni,icmotype,ier)
               call cmo_get_info('xic',cmo,ipxic,leni,icmotype,ier)
               call cmo_get_info('yic',cmo,ipyic,leni,icmotype,ier)
               call cmo_get_info('zic',cmo,ipzic,leni,icmotype,ier)
               call cmo_get_info(cvelnm,cmo,ipvels,leni,icmotype,ier)
               call cmo_get_info(cdensnm,cmo,ipdens,leni,icmotype,ier)
               call cmo_get_info(cpresnm,cmo,ippres,leni,icmotype,ier)
               call cmo_get_info('itetclr',cmo,ipitetclr,leni,
     *                           icmotype,ier)
               call cmo_get_info('itet',cmo,ipitet,leni,icmotype,ier)
               call cmo_get_info('jtet',cmo,ipjtet,leni,icmotype,ier)
               endif
            endif
            newi5=npoints
            itp1(newi5)=itp1(i5)
            imt1(newi5)=imt1(i1)
            icr=icr1(i5)
            if (icr .gt. 0) then
               icr1(newi5)=icr
            endif
            isnx=isn1(i5)
            isn1(i5)=newi5
            isn1(newi5)=isnx
            ichild2=newi5
            write (logdan,1020) i5,newi5
 1020    format('   Append ipt=',i6,'   newipt=',i6)
            call writloga("default",0,logdan,0,ierrwrt)
         endif
      endif
C
C     .................................................................
C     SET MASS POINT VARIABLES FOR ANY REMAINING CHILDREN.
C
      if (ichild2 .gt. 0) then
         xic(ichild2)=xic(i5)
         yic(ichild2)=yic(i5)
         zic(ichild2)=zic(i5)
         if(ifdens) dens(ichild2)=third*(dens(i1)+dens(i2)+dens(i3))
C*****   rsplic(ichild2)=ric(ichild2)
C
C        ,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,
C        SET THE VELOCITY FOR THE NEW CHILD POINT.
C
         if(ifvels) then
            vels(1,ichild2)=third*(vels(1,i1)+vels(1,i2)+vels(1,i3))
            vels(2,ichild2)=third*(vels(2,i1)+vels(2,i2)+vels(2,i3))
            vels(3,ichild2)=third*(vels(3,i1)+vels(3,i2)+vels(3,i3))
         endif
         if (icr .gt. 0.and.ifvels) then
C
C           ,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,
C           IF THE ICR VALUE IS POSITIVE, GET THE CONSTRAINT OPERATOR.
C
               do 70 kcr=1,9
               conopt(kcr)=xcontab(kcr,i5)
 70         continue
C
C           ,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,
C           FIND THE NORMAL COMPONENT OF THE VELOCITY VECTOR FOR POINT
C           "i5."
C
            unorm=(1.0-conopt(1))*vels(1,i5)-conopt(2)*vels(2,i5)-
     *             conopt(3)*vels(3,i5)
            vnorm=-conopt(4)*vels(1,i5)+(1.0-conopt(5))*vels(2,i5)-
     *             conopt(6)*vels(3,i5)
            wnorm=-conopt(7)*vels(1,i5)-conopt(8)*vels(2,i5)+
     *             (1.0-conopt(9))*vels(3,i5)
C
C           ,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,
C           APPLY THE CONSTRAINT OPERATOR TO THE VELOCITY OF THE NEW
C           POINT AND ADD THE NORMAL COMPONENT FOUND ABOVE.
C
            unew=conopt(1)*vels(1,ichild2)+conopt(2)*vels(2,ichild2)+
     *           conopt(3)*vels(3,ichild2)+unorm
            vnew=conopt(4)*vels(1,ichild2)+conopt(5)*vels(2,ichild2)+
     *           conopt(6)*vels(3,ichild2)+vnorm
            wnew=conopt(7)*vels(1,ichild2)+conopt(8)*vels(2,ichild2)+
     *           conopt(9)*vels(3,ichild2)+wnorm
            vels(1,ichild2)=unew
            vels(2,ichild2)=vnew
            vels(3,ichild2)=wnew
         endif
C
C        ,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,
C        DETERMINE THE REST OF THE MASS POINT VARIABLES.
C
C*****   etotic(ichild2)=third*(etotic(i1)+etotic(i2)+etotic(i3))
C*****   dedmric(ichild2)=third*(dedmric(i1)+dedmric(i2)+dedmric(i3))
C*****   eic(ichild2)=max(etotic(ichild2)-0.5*(vels(1,ichild2)**2+
C*****                      vels(2,ichild2)**2+vels(3,ichild2)**2),0.0)
         if(ifpres) pres(ichild2)=pres(i5)
C*****   tic(ichild2)=tic(i5)
C*****   tric(ichild2)=tric(i5)
C*****   pric(ichild2)=pric(i5)
C*****   xopres(ichild2)=third*(xopres(i1)+xopres(i2)+xopres(i3))
C*****   csqic(ichild2)=third*(csqic(i1)+csqic(i2)+csqic(i3))
C*****   partic(ichild2)=third*(partic(i1)+partic(i2)+partic(i3))
C*****   csqartic(ichild2)=third*(csqartic(i1)+csqartic(i2)+
C*****                     csqartic(i3))
C*****   dellic(ichild2)=third*(dellic(i1)+dellic(i2)+dellic(i3))
C*****   sxxic(ichild2)=third*(sxxic(i1)+sxxic(i2)+sxxic(i3))
C*****   syxic(ichild2)=third*(syxic(i1)+syxic(i2)+syxic(i3))
C*****   szxic(ichild2)=third*(szxic(i1)+szxic(i2)+szxic(i3))
C*****   syyic(ichild2)=third*(syyic(i1)+syyic(i2)+syyic(i3))
C*****   szyic(ichild2)=third*(szyic(i1)+szyic(i2)+szyic(i3))
C*****   gic(ichild2)=third*(gic(i1)+gic(i2)+gic(i3))
C*****   yldic(ichild2)=third*(yldic(i1)+yldic(i2)+yldic(i3))
C*****   epsic(ichild2)=third*(epsic(i1)+epsic(i2)+epsic(i3))
C*****   pbtic(ichild2)=third*(pbtic(i1)+pbtic(i2)+pbtic(i3))
C*****   pbiic(ichild2)=third*(pbiic(i1)+pbiic(i2)+pbiic(i3))
C*****   ecldic(ichild2)=third*(ecldic(i1)+ecldic(i2)+ecldic(i3))
C*****   dudt(ichild2)=third*(dudt(i1)+dudt(i2)+dudt(i3))
C*****   dvdt(ichild2)=third*(dvdt(i1)+dvdt(i2)+dvdt(i3))
C*****   dwdt(ichild2)=third*(dwdt(i1)+dwdt(i2)+dwdt(i3))
C*****   drdt(ichild2)=third*(drdt(i1)+drdt(i2)+drdt(i3))
C*****   dedt(ichild2)=third*(dedt(i1)+dedt(i2)+dedt(i3))
C*****   dphidt(ichild2)=third*(dphidt(i1)+dphidt(i2)+dphidt(i3))
C*****   dpdt(ichild2)=third*(dpdt(i1)+dpdt(i2)+dpdt(i3))
C*****   detotdt(ichild2)=third*(detotdt(i1)+detotdt(i2)+detotdt(i3))
C*****   dsdt(1,ichild2)=third*(dsdt(1,i1)+dsdt(1,i2)+dsdt(1,i3))
C*****   dsdt(2,ichild2)=third*(dsdt(2,i1)+dsdt(2,i2)+dsdt(2,i3))
C*****   dsdt(3,ichild2)=third*(dsdt(3,i1)+dsdt(3,i2)+dsdt(3,i3))
C*****   dsdt(4,ichild2)=third*(dsdt(4,i1)+dsdt(4,i2)+dsdt(4,i3))
C*****   dsdt(5,ichild2)=third*(dsdt(5,i1)+dsdt(5,i2)+dsdt(5,i3))
         isetwd(ichild2)=ior(ior(isetwd(i1),isetwd(i2)),isetwd(i3))
      endif
C
C     ******************************************************************
C
C     MAKE THE THREE NEW TETRAHEDRON ASSIGNMENTS
C
      it3=ntets+1
C
      id(1)=i1
      id(2)=i2
      id(3)=newi5
      id(4)=i4
      jd(1)=4*(it2-1)+2
      jd(2)=4*(it3-1)+1
      jd(3)=j3
      jd(4)=k2
C
      id(5)=i2
      id(6)=i3
      id(7)=newi5
      id(8)=i4
      jd(5)=4*(it3-1)+2
      jd(6)=4*(it1-1)+1
      jd(7)=j1
      jd(8)=k1
C
      id(9) =i3
      id(10)=i1
      id(11)=newi5
      id(12)=i4
      jd(9) =4*(it1-1)+2
      jd(10)=4*(it2-1)+1
      jd(11)=j2
      jd(12)=k3
C
C     ******************************************************************
C
C     DEBUG SECTION.
C
      if(idebug.gt.0) then
         ict123=0
         ict45=0
         do 100 it=1,ntets
            isum1=(itet(1,it)-i1)*(itet(2,it)-i1)*
     *            (itet(3,it)-i1)*(itet(4,it)-i1)
            isum2=(itet(1,it)-i2)*(itet(2,it)-i2)*
     *            (itet(3,it)-i2)*(itet(4,it)-i2)
            isum3=(itet(1,it)-i3)*(itet(2,it)-i3)*
     *            (itet(3,it)-i3)*(itet(4,it)-i3)
            if(isum1.eq.0.and.isum2.eq.0.and.isum3.eq.0) ict123=ict123+1
 100     continue
         do 110 it=1,ntets
            isum4=(itet(1,it)-i4)*(itet(2,it)-i4)*
     *            (itet(3,it)-i4)*(itet(4,it)-i4)
            isum5=(itet(1,it)-newi5)*(itet(2,it)-newi5)*
     *            (itet(3,it)-newi5)*(itet(4,it)-newi5)
            if(isum4.eq.0.and.isum5.eq.0) ict45=ict45+1
 110     continue
         if(ict123.ne.2) then
            write(logdan,9020) it1,it2
            call writloga("default",0,logdan,0,ierrwrt)
            write(logdan,9030) i1,i2,i3
            call writloga("default",0,logdan,0,ierrwrt)
 9020       format("find2to3 - FACE EXISTS MORE THAN TWICE: it1=",i10,
     *             " it2=",i10)
 9030       format("                face:     ",i10,"     ",i10,
     *             "     ",i10)
c            call rwdmpw
         endif
         if(ict45.ne.0) then
            write(logdan,9000) it1,it2
            call writloga("default",0,logdan,0,ierrwrt)
            write(logdan,9010) i4,newi5
            call writloga("default",0,logdan,0,ierrwrt)
 9000       format("find2to3 - LINE EXISTS: it1=",i10," it2=",i10)
 9010       format("          connection:     ",i10,"     ",i10)
c            call rwdmpw
         endif
      endif
      goto 9999
 9999 continue
      end
