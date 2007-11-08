      subroutine flip3to2(it1,it2,it3,id,jd,
     *                    npoints,ntets)
C
       implicit real*8 (a-h,o-z)
C ######################################################################
C
C      PURPOSE -
C
C         This routine switches three tets into two tets
C
C      INPUT ARGUMENTS -
C
C         it1   - the first tet
C         it2   - the second tet
C         it3   - the third tet
C         id    - "itet" coordinates of the two new tets
C         jd    - "jtet" coordinates of the two new tets
C
C      OUTPUT ARGUMENTS -
C
C         none
C
C     CHANGE HISTORY -
C
C        $Log:   /pvcs.config/t3d/src/flip3to2.f_a  $
CPVCS    
CPVCS       Rev 1.5   Wed Feb 03 15:23:30 1999   dcg
CPVCS    remove calls to fluxing routines and associated memory.
CPVCS
CPVCS       Rev 1.4   Mon Apr 14 16:48:34 1997   pvcs
CPVCS    No change.
CPVCS
CPVCS       Rev 1.3   12/02/94 15:05:20   het
CPVCS    Added an option for the "cmo" access functions
CPVCS
CPVCS
CPVCS       Rev 1.2   12/01/94 18:47:08   het
CPVCS    Added a data type to the "cmo" calles
CPVCS       and added the "cmo.h" include file.
CPVCS
CPVCS       Rev 1.1   11/17/94 21:51:32   het
CPVCS    Added include files for chydro, neibor, cmerge, comdict. Added calles and
CPVCS    pointer statements for current_mesh_object database access.
CPVCS
CPVCS       Rev 1.0   11/10/94 12:12:50   pvcs
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
      dimension id(12),jd(12)
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
      call cmo_get_info('imt1',cmo,ipimt1,lenimt1,icmotype,ierror)
      call cmo_get_info('xic',cmo,ipxic,lenxic,icmotype,ierror)
      call cmo_get_info('yic',cmo,ipyic,lenyic,icmotype,ierror)
      call cmo_get_info('zic',cmo,ipzic,lenzic,icmotype,ierror)
      call cmo_get_info('itetclr',cmo,ipitetclr,lenitetclr,icmotype,ier)
      call cmo_get_info('itet',cmo,ipitet,lenitet,icmotype,ierror)
      call cmo_get_info('jtet',cmo,ipjtet,lenjtet,icmotype,ierror)
C
      endif
C
C     ******************************************************************
C
C
      ione=1
C
      itet(1,it3)=-itet(1,it3)
C
      itet(1,it1)=id(1)
      jtet(1,it1)=jd(1)
      itet(2,it1)=id(2)
      jtet(2,it1)=jd(2)
      itet(3,it1)=id(3)
      jtet(3,it1)=jd(3)
      itet(4,it1)=id(4)
      jtet(4,it1)=jd(4)
      itet(1,it2)=id(5)
      jtet(1,it2)=jd(5)
      itet(2,it2)=id(6)
      jtet(2,it2)=jd(6)
      itet(3,it2)=id(7)
      jtet(3,it2)=jd(7)
      itet(4,it2)=id(8)
      jtet(4,it2)=jd(8)
C
C     ******************************************************************
C     MAKE THE JTET ARRAY CONSISTENT
C
      k1=4*(it1-1)
      k2=4*(it2-1)
      do 20 i=1,4
         if(jd(i).lt.mbndry) then
            jtet1(jd(i))=k1+i
         elseif(jd(i).gt.mbndry) then
            jtemp=jd(i)-mbndry
            jtet1(jtemp)=k1+i+mbndry
         endif
         if(jd(i+4).lt.mbndry) then
            jtet1(jd(i+4))=k2+i
         elseif(jd(i+4).gt.mbndry) then
            jtemp=jd(i+4)-mbndry
            jtet1(jtemp)=k2+i+mbndry
         endif
 20   continue
C
C     *****************************************************************
C     UPDATE THE LIST OF HOLES IN THE "itet" ARRAY
C
      nvacnt=nvacnt+1
      if(nvacnt.ge.lenvacnt) then
         lenvacnt=nvacnt+100
         call mflip(ione,lenvacnt,'ivacnt')
      endif
      ivacnt(nvacnt)=it3
C
C     ******************************************************************
C
      if(idebug.gt.1) then
C*****  call tettestd
      endif
C
      goto 9999
 9999 continue
      return
      end
