*dk,flp2to0b
      subroutine flp2to0b(it1,ipos1,it2,
     *                    npoints,ntets)
       implicit none
      character*8 if1,if2,if3,if4
C ######################################################################
C
C     PURPOSE -
C
C        This routine performs the 2-to-0 flip across a material
C        interface.
C
C     INPUT ARGUMENTS -
C
C        it1      - the first tetrahedron
C        ipos1    - the position of the common face in it1
C        it2      - the second tetrahedron
C
C     OUTPUT ARGUMENTS -
C
C         None
C
C     CHANGE HISTORY -
C
C        $Log:   /pvcs.config/t3d/src/flp2to0b.f_a  $
CPVCS    
CPVCS       Rev 1.10   30 Sep 2004 09:59:02   dcg
CPVCS    use iand in place of .and. with integer variables
CPVCS    
CPVCS       Rev 1.9   05 Jan 2001 12:56:08   dcg
CPVCS    use dble in place of float so as not to lose digits
CPVCS
CPVCS       Rev 1.8   Wed Apr 05 13:34:22 2000   nnc
CPVCS    Minor source modifications required by the Absoft compiler.
CPVCS
CPVCS       Rev 1.7   Wed Apr 07 14:14:16 1999   dcg
CPVCS    use implitcit none
CPVCS
CPVCS       Rev 1.6   Wed Feb 03 15:23:40 1999   dcg
CPVCS    remove calls to fluxing routines and associated memory.
CPVCS
CPVCS       Rev 1.5   Wed Apr 23 11:36:02 1997   dcg
CPVCS    No change.
CPVCS
CPVCS       Rev 1.4   Mon Apr 14 16:48:42 1997   pvcs
CPVCS    No change.
CPVCS
CPVCS       Rev 1.3   12/02/94 15:05:32   het
CPVCS    Added an option for the "cmo" access functions
CPVCS
CPVCS
CPVCS       Rev 1.2   12/01/94 18:47:28   het
CPVCS    Added a data type to the "cmo" calles
CPVCS       and added the "cmo.h" include file.
CPVCS
CPVCS       Rev 1.1   11/17/94 21:51:54   het
CPVCS    Added include files for chydro, neibor, cmerge, comdict. Added calles and
CPVCS    pointer statements for current_mesh_object database access.
CPVCS
CPVCS       Rev 1.0   11/10/94 12:13:10   pvcs
CPVCS    Original version.
C
C ######################################################################
C
      include "cmo.h"
      include "chydro.h"
      include "neibor.h"
C
C ######################################################################
C
      integer nmulti
      parameter (nmulti = 200)
      dimension ichain0(nmulti),imt0(nmulti),ipts(3),ix(3),jd(3),kd(3)
      integer l1,l2,l3,ione,it2x,length,icmotype,ierror,
     *  it1,it2,ipos1,npoints,ntets,i,j,k,ichain0,imt0,
     *  ipts,ix,jd,kd,i1,i2,i3,i4,ier,it1sum,it2sum,ierrdum,
     *  ierrfls,jtemp,ipos2,imtx1,imtx2,ict,ipar,ktemp
      real*8 volit1,volit2,volume,crosx1,crosy1,crosz1
C
C ######################################################################
C
C     DEFINE THE STATEMENT FUNCTIONS NEEDED TO CALCULATE TET VOLUMES.
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
C     ******************************************************************
C     FETCH MESH OBJECT NAME AND POINTER INFORMATION.
C
      if(icmoget.eq.1) then
C
      call cmo_get_name(cmo,ierror)
C
      call cmo_get_info('mbndry',cmo,mbndry,length,icmotype,ierror)
      call cmo_get_info('imt1',cmo,ipimt1,length,icmotype,ierror)
      call cmo_get_info('xic',cmo,ipxic,length,icmotype,ierror)
      call cmo_get_info('yic',cmo,ipyic,length,icmotype,ierror)
      call cmo_get_info('zic',cmo,ipzic,length,icmotype,ierror)
      call cmo_get_info('itetclr',cmo,ipitetclr,length,icmotype,ier)
      call cmo_get_info('itet',cmo,ipitet,length,icmotype,ierror)
      call cmo_get_info('jtet',cmo,ipjtet,length,icmotype,ierror)
C
      endif
C
C
C     ******************************************************************
C     IF THE DEBUG FLAG IS TURNED ON THEN PRINT A SUMMARY OF THE FLIP.
C
      if(idebug.gt.1) then
        it1sum=itet(1,it1)+itet(2,it1)+itet(3,it1)+itet(4,it1)
        volit1=volume(itet(1,it1),itet(2,it1),itet(3,it1),itet(4,it1))
        it2sum=itet(1,it2)+itet(2,it2)+itet(3,it2)+itet(4,it2)
        volit2=volume(itet(1,it2),itet(2,it2),itet(3,it2),itet(4,it2))
        i1=itet(1,it1)
        i2=itet(2,it1)
        i3=itet(3,it1)
        i4=itet(4,it1)
        if1=' '
        if(jtet(1,it1).ge.mbndry) if1='*'
        if2=' '
        if(jtet(2,it1).ge.mbndry) if2='*'
        if3=' '
        if(jtet(3,it1).ge.mbndry) if3='*'
        if4=' '
        if(jtet(4,it1).ge.mbndry) if4='*'
        write(logdan,9000) 'flp2to0b',it1,'old=',
     *                     i1,if1,i2,if2,i3,if3,i4,if4,
     *                     it1sum,volit1
        call writloga('bat',0,logdan,0,ierrdum)
        i1=itet(1,it2)
        i2=itet(2,it2)
        i3=itet(3,it2)
        i4=itet(4,it2)
        if1=' '
        if(jtet(1,it2).ge.mbndry) if1='*'
        if2=' '
        if(jtet(2,it2).ge.mbndry) if2='*'
        if3=' '
        if(jtet(3,it2).ge.mbndry) if3='*'
        if4=' '
        if(jtet(4,it2).ge.mbndry) if4='*'
        write(logdan,9010) '        ',it2,'old=',
     *                     i1,if1,i2,if2,i3,if3,i4,if4,
     *                     it2sum,volit2
        call writloga('bat',0,logdan,0,ierrdum)
 9000   format(a8,' it1=',i8,1x,a4,4(i7,a1),i8,1x,1pe10.3)
 9010   format(a8,' it2=',i8,1x,a4,4(i7,a1),i8,1x,1pe10.3)
        call writfls('bat',ierrfls)
      endif
C
C     ******************************************************************
C     PERFORM THE FLIP.
C
      jtemp=jtet(ipos1,it1)-mbndry
      it2x=0.25*dble(jtemp)+0.9
      if (it2x.ne.it2) call termcode(1)
      ipos2=iand((jtemp-1),maskface)+1
C
      i1=itet(iflist(3*ipos1-2),it1)
      i3=itet(iflist(3*ipos1-1),it1)
      i2=itet(iflist(3*ipos1  ),it1)
      jd(1)=jtet(iflist(3*ipos1-2),it1)
      jd(3)=jtet(iflist(3*ipos1-1),it1)
      jd(2)=jtet(iflist(3*ipos1  ),it1)
C
C     ..................................................................
C
C     GET THE COUPLED CHAIN LISTS FOR THE THREE FACE POINTS.
C
      imtx1=imt1(i1)
      imtx2=imt1(itet1(jtemp))
      ipts(1)=i1
      ipts(2)=i2
      ipts(3)=i3
      do 50 i=1,3
         ix(i)=0
         call getchain(ipts(i),ichain0,imt0,nmulti,ict,ipar)
         do 40 k=1,ict
            if (imt0(k) .eq. imtx2) then
               ix(i)=ichain0(k)
               goto 50
            endif
 40      continue
 50   continue
      if (ix(1).eq.0 .or. ix(2).eq.0 .or. ix(3).eq.0) call termcode(1)
C
C     ..................................................................
C
C     DETERMINE THE ORIENTATION OF THE SECOND TETRAHEDRON.
C
      l1=iflist(3*ipos2-2)
      l2=iflist(3*ipos2-1)
      l3=iflist(3*ipos2)
                                kd(1)=jtet(l1,it2)
      if(itet(l2,it2).eq.ix(1)) kd(1)=jtet(l2,it2)
      if(itet(l3,it2).eq.ix(1)) kd(1)=jtet(l3,it2)
                                kd(2)=jtet(l1,it2)
      if(itet(l2,it2).eq.ix(2)) kd(2)=jtet(l2,it2)
      if(itet(l3,it2).eq.ix(2)) kd(2)=jtet(l3,it2)
                                kd(3)=jtet(l1,it2)
      if(itet(l2,it2).eq.ix(3)) kd(3)=jtet(l2,it2)
      if(itet(l3,it2).eq.ix(3)) kd(3)=jtet(l3,it2)
C
C     ..................................................................
C     MAKE THE 'jtet' ARRAY CONSISTENT.
C
      do 60 i=1,3
         jtemp=jd(i)
         ktemp=kd(i)
         if (jtemp .gt. mbndry) jtemp=jtemp-mbndry
         if (ktemp .gt. mbndry) ktemp=ktemp-mbndry
         if (jtemp.lt.mbndry .and. ktemp.lt.mbndry) then
            if (imt1(itet1(jtemp)) .eq. imt1(itet1(ktemp))) then
               if (jd(i) .gt. mbndry) jd(i)=jd(i)-mbndry
               if (kd(i) .gt. mbndry) kd(i)=kd(i)-mbndry
            else
               if (jd(i) .lt. mbndry) jd(i)=jd(i)+mbndry
               if (kd(i) .lt. mbndry) kd(i)=kd(i)+mbndry
            endif
            jtet1(jtemp)=kd(i)
            jtet1(ktemp)=jd(i)
         else
            if (jtemp .lt. mbndry) jtet1(jtemp)=kd(i)
            if (ktemp .lt. mbndry) jtet1(ktemp)=jd(i)
         endif
 60   continue
C
C
      nvacnt=nvacnt+2
      if(nvacnt.ge.lenvacnt) then
         ione=1
         lenvacnt=nvacnt+100
         call mflip(ione,lenvacnt,'ivacnt')
      endif
      ivacnt(nvacnt-1)=it1
      ivacnt(nvacnt)=it2
C
      if(idebug.gt.1) then
C*****  call tettestd
      endif
      goto 9999
 9999 continue
      return
      end
