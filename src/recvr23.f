*dk,recvr23
      subroutine recvr23(it1,it2,it3,id,jd,ictrecvr,ipiitmp,
     *                   ipjjtmp,ipkttmp,
     *                   npoints,ntets)
       implicit real*8 (a-h,o-z)
C
C ######################################################################
C
C      PURPOSE -
C
C         This routine temporarily takes two tets into three tets and
C         saves information for later recovering the original state of
C         the mesh.  The fluxer is not called.
C
C            (i1,i2,i3,i4) - (j1,j2,j3,xx)
C            (i1,i3,i2,i5) - (k1,k2,k3,xx)
C            -----------------------------------------
C            (i1,i2,i5,i4) - { (2,it2),(1,it3),j3,k2 }
C            (i2,i3,i5,i4) - { (2,it3),(1,it1),j1,k1 }
C            (i3,i1,i5,i4) - { (2,it1),(1,it2),j2,k3 }
C
C      INPUT ARGUMENTS -
C
C         it1   - the first tet
C         it2   - the second tet
C         id    - the "itet" values of the three new tets
C         jd    - the "jtet" values of the three new tets
C         ictrecvr - the current number of tetrahedra in the "recvr"
C                    array.
C         ipiitmp  - pointer to array to store itet information for
C                    later recovering the original mesh.
C         ipjjtmp  - pointer to array to store jtet information for
C                    later recovering the original mesh.
C         ipkttmp  - pointer to array to store tet numbers for
C                    later recovering the original mesh.
C
C      OUTPUT ARGUMENTS -
C
C         ictrecvr - the current number of tetrahedra in the "recvr"
C                    array.
C
C     CHANGE HISTORY -
C
C        $Log:   /pvcs.config/t3d/src/recvr23.f_a  $
CPVCS    
CPVCS       Rev 1.8   05 Jan 2001 12:57:20   dcg
CPVCS    use dble in place of float so as not to lose digits
CPVCS
CPVCS       Rev 1.7   Fri Nov 14 13:43:22 1997   dcg
CPVCS    declare logmess as a character variable
CPVCS
CPVCS       Rev 1.6   Fri Oct 17 11:09:56 1997   dcg
CPVCS    replace print statement with writlog call
CPVCS
CPVCS       Rev 1.5   Fri Sep 26 13:54:06 1997   dcg
CPVCS    refresh pointers after hmemadjb call
CPVCS
CPVCS       Rev 1.4   Mon Apr 14 16:58:46 1997   pvcs
CPVCS    No change.
CPVCS
CPVCS       Rev 1.3   12/02/94 15:06:46   het
CPVCS    Added an option for the "cmo" access functions
CPVCS
CPVCS
CPVCS       Rev 1.2   12/01/94 18:49:06   het
CPVCS    Added a data type to the "cmo" calles
CPVCS       and added the "cmo.h" include file.
CPVCS
CPVCS       Rev 1.1   11/17/94 21:54:46   het
CPVCS    Added include files for chydro, neibor, cmerge, comdict. Added calles and
CPVCS    pointer statements for current_mesh_object database access.
CPVCS
CPVCS       Rev 1.0   11/10/94 12:17:44   pvcs
CPVCS    Original version.
C
C ######################################################################
C
C
      include "cmo.h"
      include "chydro.h"
      include "neibor.h"
C
C ######################################################################
C
      integer id(12),jd(12),itets(3)
      character*132 logmess
      pointer (ipiitmp , iitmp(4,1))
      pointer (ipjjtmp , jjtmp(4,1))
      pointer (ipkttmp , kttmp(1)  )
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
      call mmgetlen(ipitet,lenitet,icscode)
      if(4*(ntets+1).ge.lenitet) then
         write(logmess,'(a,a)') 'Trying to increment nodnbrp arrays in '
     *           , 'recvr23'
         call writloga('default',0,logmess,0,ierrw)
         call hmemadjb('nodnbrp ',1)
 
         call cmo_get_info('imt1',cmo,ipimt1,lenimt1,icmotype,ierror)
         call cmo_get_info('xic',cmo,ipxic,lenxic,icmotype,ierror)
         call cmo_get_info('yic',cmo,ipyic,lenyic,icmotype,ierror)
         call cmo_get_info('zic',cmo,ipzic,lenzic,icmotype,ierror)
         call cmo_get_info('itetclr',cmo,ipitetclr,lenitetclr,icmotype,
     *                     ierror)
         call cmo_get_info('itet',cmo,ipitet,lenitet,icmotype,ierror)
         call cmo_get_info('jtet',cmo,ipjtet,lenjtet,icmotype,ierror)
 
      endif
      ntets=ntets+1
      if(icmoset.eq.1) then
         call cmo_set_info('nelements',cmo,ntets,1,1,ierror)
      endif
      it3=ntets
C
C     ******************************************************************
C     SAVE MESH INFORMATION.
C
      if (kfix(2,it1) .eq. 0) then
         kfix(2,it1)=1
         ictrecvr=ictrecvr+1
         kttmp(ictrecvr)=it1
         iitmp(1,ictrecvr)=itet(1,it1)
         iitmp(2,ictrecvr)=itet(2,it1)
         iitmp(3,ictrecvr)=itet(3,it1)
         iitmp(4,ictrecvr)=itet(4,it1)
         jjtmp(1,ictrecvr)=jtet(1,it1)
         jjtmp(2,ictrecvr)=jtet(2,it1)
         jjtmp(3,ictrecvr)=jtet(3,it1)
         jjtmp(4,ictrecvr)=jtet(4,it1)
      endif
      if (kfix(2,it2) .eq. 0) then
         kfix(2,it2)=1
         ictrecvr=ictrecvr+1
         kttmp(ictrecvr)=it2
         iitmp(1,ictrecvr)=itet(1,it2)
         iitmp(2,ictrecvr)=itet(2,it2)
         iitmp(3,ictrecvr)=itet(3,it2)
         iitmp(4,ictrecvr)=itet(4,it2)
         jjtmp(1,ictrecvr)=jtet(1,it2)
         jjtmp(2,ictrecvr)=jtet(2,it2)
         jjtmp(3,ictrecvr)=jtet(3,it2)
         jjtmp(4,ictrecvr)=jtet(4,it2)
      endif
C
C     ******************************************************************
C     MAKE THE THREE NEW TETRAHEDRON ASSIGNMENTS
C
      itet(1,it1)=id(1)
      jtet(1,it1)=jd(1)
      itet(2,it1)=id(2)
      jtet(2,it1)=jd(2)
      itet(3,it1)=id(3)
      jtet(3,it1)=jd(3)
      itet(4,it1)=id(4)
      jtet(4,it1)=jd(4)
C
      itet(1,it2)=id(5)
      jtet(1,it2)=jd(5)
      itet(2,it2)=id(6)
      jtet(2,it2)=jd(6)
      itet(3,it2)=id(7)
      jtet(3,it2)=jd(7)
      itet(4,it2)=id(8)
      jtet(4,it2)=jd(8)
C
      itet(1,it3)=id(9)
      jtet(1,it3)=jd(9)
      itet(2,it3)=id(10)
      jtet(2,it3)=jd(10)
      itet(3,it3)=id(11)
      jtet(3,it3)=jd(11)
      itet(4,it3)=id(12)
      jtet(4,it3)=jd(12)
C
C
C     ******************************************************************
C     SAVE MESH INFORMATION.
C
      itets(1)=it1
      itets(2)=it2
      itets(3)=it3
      do 150 k=1,3
         itk=itets(k)
         do 100 j=3,4
            jtemp=jtet(j,itk)
            if (jtemp .eq. mbndry) goto 100
            if (jtemp .gt. mbndry) jtemp=jtemp-mbndry
            itx=0.25*dble(jtemp)+0.9
            if (kfix(2,itx) .eq. 0) then
               kfix(2,itx)=1
               ictrecvr=ictrecvr+1
               kttmp(ictrecvr)=itx
               iitmp(1,ictrecvr)=itet(1,itx)
               iitmp(2,ictrecvr)=itet(2,itx)
               iitmp(3,ictrecvr)=itet(3,itx)
               iitmp(4,ictrecvr)=itet(4,itx)
               jjtmp(1,ictrecvr)=jtet(1,itx)
               jjtmp(2,ictrecvr)=jtet(2,itx)
               jjtmp(3,ictrecvr)=jtet(3,itx)
               jjtmp(4,ictrecvr)=jtet(4,itx)
            endif
 100     continue
 150  continue
C
C     ******************************************************************
C     MAKE THE JTET ARRAY CONSISTENT
C
      if(jtet(3,it2).lt.mbndry) then
         jtet1(jtet(3,it2))=4*(it2-1)+3
      elseif(jtet(3,it2).gt.mbndry) then
         jtemp=jtet(3,it2)-mbndry
         jtet1(jtemp)=4*(it2-1)+3+mbndry
      endif
      if(jtet(3,it3).lt.mbndry) then
         jtet1(jtet(3,it3))=4*(it3-1)+3
      elseif(jtet(3,it3).gt.mbndry) then
         jtemp=jtet(3,it3)-mbndry
         jtet1(jtemp)=4*(it3-1)+3+mbndry
      endif
      if(jtet(3,it1).lt.mbndry) then
         jtet1(jtet(3,it1))=4*(it1-1)+3
      elseif(jtet(3,it1).gt.mbndry) then
         jtemp=jtet(3,it1)-mbndry
         jtet1(jtemp)=4*(it1-1)+3+mbndry
      endif
      if(jtet(4,it2).lt.mbndry) then
         jtet1(jtet(4,it2))=4*(it2-1)+4
      elseif(jtet(4,it2).gt.mbndry) then
         jtemp=jtet(4,it2)-mbndry
         jtet1(jtemp)=4*(it2-1)+4+mbndry
      endif
      if(jtet(4,it1).lt.mbndry) then
         jtet1(jtet(4,it1))=4*(it1-1)+4
      elseif(jtet(4,it1).gt.mbndry) then
         jtemp=jtet(4,it1)-mbndry
         jtet1(jtemp)=4*(it1-1)+4+mbndry
      endif
      if(jtet(4,it3).lt.mbndry) then
         jtet1(jtet(4,it3))=4*(it3-1)+4
      elseif(jtet(4,it3).gt.mbndry) then
         jtemp=jtet(4,it3)-mbndry
         jtet1(jtemp)=4*(it3-1)+4+mbndry
      endif
C
C     ******************************************************************
C
C
      goto 9999
 9999 continue
      return
      end
