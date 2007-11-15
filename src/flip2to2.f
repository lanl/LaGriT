*dk,flip2to2
      subroutine flip2to2(it1,it2,id,jd,
     *                    npoints,ntets)
       implicit real*8 (a-h,o-z)
C ######################################################################
C
C     PURPOSE -
C
C        This routine flips connections on boundaries. This routine
C        closely parallels the routine 'flip4to4'.
C
C     INPUT ARGUMENTS -
C
C        it1     - the first tet
C        it2     - the second tet
C        id      - the 'itet' coordinates of the two new tets
C        jd      - the 'jtet' coordinates of the two new tets
C
C     OUTPUT ARGUMENTS -
C
C        None
C
C     CHANGE HISTORY -
C
C        $Log: flip2to2.f,v $
C        Revision 2.00  2007/11/05 19:45:55  spchu
C        Import to CVS
C
CPVCS    
CPVCS       Rev 1.5   Wed Feb 03 15:23:26 1999   dcg
CPVCS    remove calls to fluxing routines and associated memory.
CPVCS
CPVCS       Rev 1.4   Mon Apr 14 16:48:26 1997   pvcs
CPVCS    No change.
CPVCS
CPVCS       Rev 1.3   12/02/94 15:05:14   het
CPVCS    Added an option for the "cmo" access functions
CPVCS
CPVCS
CPVCS       Rev 1.2   12/01/94 18:47:00   het
CPVCS    Added a data type to the "cmo" calles
CPVCS       and added the "cmo.h" include file.
CPVCS
CPVCS       Rev 1.1   11/17/94 21:51:22   het
CPVCS    Added include files for chydro, neibor, cmerge, comdict. Added calles and
CPVCS    pointer statements for current_mesh_object database access.
CPVCS
CPVCS       Rev 1.0   11/10/94 12:12:40   pvcs
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
      dimension id(8),jd(8)
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
C     MAKE THE 'jtet' ARRAY CONSISTENT
C
      if(jd(2).lt.mbndry) then
         jtet1(jd(2))=4*(it1-1)+2
      elseif(jd(2).gt.mbndry) then
         jd2=jd(2)-mbndry
         jtet1(jd2)=4*(it1-1)+2+mbndry
      endif
      if(jd(4).lt.mbndry) then
         jtet1(jd(4))=4*(it1-1)+4
      elseif(jd(4).gt.mbndry) then
         jd4=jd(4)-mbndry
         jtet1(jd4)=4*(it1-1)+4+mbndry
      endif
      if(jd(7).lt.mbndry) then
         jtet1(jd(7))=4*(it2-1)+3
      elseif(jd(7).gt.mbndry) then
         jd7=jd(7)-mbndry
         jtet1(jd7)=4*(it2-1)+3+mbndry
      endif
      if(jd(8).lt.mbndry) then
         jtet1(jd(8))=4*(it2-1)+4
      elseif(jd(8).gt.mbndry) then
         jd8=jd(8)-mbndry
         jtet1(jd8)=4*(it2-1)+4+mbndry
      endif
C
C
      goto 9999
 9999 continue
      return
      end
