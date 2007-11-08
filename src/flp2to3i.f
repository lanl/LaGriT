      subroutine flp2to3i(it1,it2,it3,ibdytet,id,jd,
     *                    npoints,ntets)
C
       implicit none
C ######################################################################
C
C      PURPOSE -
C
C        This routine completes the flip set up by the routine
C        'fnd2to3i.'  The result is a flip of connections on
C        the material interface.
C
C      INPUT ARGUMENTS -
C
C         it1   - the first tet
C         it2   - the second tet
C         id    - the 'itet' values of the three new tets
C         jd    - the 'jtet' values of the three new tets
C
C      OUTPUT ARGUMENTS -
C
C         None
C
C      CHANGE HISTORY -
C
C        $Log:   /pvcs.config/t3d/src/flp2to3i.f_a  $
CPVCS    
CPVCS       Rev 1.8   Mon Feb 01 13:33:30 1999   dcg
CPVCS    update itettyp for newly created element
CPVCS    
CPVCS       Rev 1.7   Fri Sep 26 13:54:32 1997   dcg
CPVCS    refresh pointers after hmemadjb call
CPVCS    
CPVCS       Rev 1.6   Fri Apr 25 16:18:42 1997   dcg
CPVCS    set tet colors from node colors
CPVCS
CPVCS       Rev 1.5   Mon Apr 14 16:48:50 1997   pvcs
CPVCS    No change.
CPVCS
CPVCS       Rev 1.4   08/10/95 16:20:30   dcg
CPVCS    replace print * with writloga statements
CPVCS
CPVCS       Rev 1.3   12/02/94 15:05:40   het
CPVCS    Added an option for the "cmo" access functions
CPVCS
CPVCS
CPVCS       Rev 1.2   12/01/94 18:47:36   het
CPVCS    Added a data type to the "cmo" calles
CPVCS       and added the "cmo.h" include file.
CPVCS
CPVCS       Rev 1.1   11/17/94 21:52:06   het
CPVCS    Added include files for chydro, neibor, cmerge, comdict. Added calles and
CPVCS    pointer statements for current_mesh_object database access.
CPVCS
CPVCS       Rev 1.0   11/10/94 12:13:22   pvcs
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
      integer it1,it2,it3,ntets,npoints,ibdytet, id(12),jd(12)
      real*8 crosx1,crosy1,crosz1,volume,volit1,volit2,volit3
      integer i,j,k,i1,i2,i3,i4,ierror,leni,icmotype,jtemp,
     *  it1sum,it2sum,it3sum
      character*4 if1,if2,if3,if4
      character*132 logmess
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
      call cmo_get_info('mbndry',cmo,mbndry,leni,icmotype,ierror)
      call cmo_get_info('imt1',cmo,ipimt1,leni,icmotype,ierror)
      call cmo_get_info('xic',cmo,ipxic,leni,icmotype,ierror)
      call cmo_get_info('yic',cmo,ipyic,leni,icmotype,ierror)
      call cmo_get_info('zic',cmo,ipzic,leni,icmotype,ierror)
      call cmo_get_info('itetclr',cmo,ipitetclr,leni,icmotype,ierror)
      call cmo_get_info('itettyp',cmo,ipitettyp,leni,icmotype,ierror)
      call cmo_get_info('itet',cmo,ipitet,leni,icmotype,ierror)
      call cmo_get_info('jtet',cmo,ipjtet,leni,icmotype,ierror)
C
      endif
C     ******************************************************************
C     MAKE THE THREE NEW TETRAHEDRON ASSIGNMENTS
C
      call mmgetlen(ipitet,leni,ierror)
      if(4*(ntets+1).ge.leni) then
        write(logmess,'(a,a)') 'Trying to increment nodnbrp arrays in ',
     *           'flp2to3i'
        call writloga('default',0,logmess,0,ierror)
         call hmemadjb('nodnbrp ',1)

         call cmo_get_info('imt1',cmo,ipimt1,leni,icmotype,ierror)
         call cmo_get_info('xic',cmo,ipxic,leni,icmotype,ierror)
         call cmo_get_info('yic',cmo,ipyic,leni,icmotype,ierror)
         call cmo_get_info('zic',cmo,ipzic,leni,icmotype,ierror)
         call cmo_get_info('itetclr',cmo,ipitetclr,leni,icmotype,
     *                     ierror)
         call cmo_get_info('itettyp',cmo,ipitettyp,leni,icmotype,
     *                     ierror)
         call cmo_get_info('itet',cmo,ipitet,leni,icmotype,ierror)
         call cmo_get_info('jtet',cmo,ipjtet,leni,icmotype,ierror)

      endif
      ntets=ntets+1
      if(icmoset.eq.1) then
         call cmo_set_info('nelements',cmo,ntets,1,1,ierror)
      endif
C
      it3=ntets
C
      if(ibdytet.eq.1) itetclr(it1)=imt1(id(1))
      itet(1,it1)=id(1)
      jtet(1,it1)=jd(1)
      itet(2,it1)=id(2)
      jtet(2,it1)=jd(2)
      itet(3,it1)=id(3)
      jtet(3,it1)=jd(3)
      itet(4,it1)=id(4)
      jtet(4,it1)=jd(4)
C
      if(ibdytet.eq.2) itetclr(it2)=imt1(id(5))
      itet(1,it2)=id(5)
      jtet(1,it2)=jd(5)
      itet(2,it2)=id(6)
      jtet(2,it2)=jd(6)
      itet(3,it2)=id(7)
      jtet(3,it2)=jd(7)
      itet(4,it2)=id(8)
      jtet(4,it2)=jd(8)
C
      itetclr(it3)=imt1(id(9))
      itettyp(it3)=itettyp(it1)
      itet(1,it3)=id(9)
      jtet(1,it3)=jd(9)
      itet(2,it3)=id(10)
      jtet(2,it3)=jd(10)
      itet(3,it3)=id(11)
      jtet(3,it3)=jd(11)
      itet(4,it3)=id(12)
      jtet(4,it3)=jd(12)
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
      if(idebug.gt.1) then
        it1sum=itet(1,it1)+itet(2,it1)+itet(3,it1)+itet(4,it1)
        volit1=volume(itet(1,it1),itet(2,it1),itet(3,it1),itet(4,it1))
        it2sum=itet(1,it2)+itet(2,it2)+itet(3,it2)+itet(4,it2)
        volit2=volume(itet(1,it2),itet(2,it2),itet(3,it2),itet(4,it2))
        it3sum=itet(1,it3)+itet(2,it3)+itet(3,it3)+itet(4,it3)
        volit3=volume(itet(1,it3),itet(2,it3),itet(3,it3),itet(4,it3))
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
        write(logdan,9000) '        ',it1,'new=',
     *                     i1,if1,i2,if2,i3,if3,i4,if4,
     *                     it1sum,volit1
        call writloga('bat',0,logdan,0,ierror)
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
        write(logdan,9010) '        ',it2,'new=',
     *                     i1,if1,i2,if2,i3,if3,i4,if4,
     *                     it2sum,volit2
        call writloga('bat',0,logdan,0,ierror)
        i1=itet(1,it3)
        i2=itet(2,it3)
        i3=itet(3,it3)
        i4=itet(4,it3)
        if1=' '
        if(jtet(1,it3).ge.mbndry) if1='*'
        if2=' '
        if(jtet(2,it3).ge.mbndry) if2='*'
        if3=' '
        if(jtet(3,it3).ge.mbndry) if3='*'
        if4=' '
        if(jtet(4,it3).ge.mbndry) if4='*'
        write(logdan,9020) '        ',it3,'new=',
     *                     i1,if1,i2,if2,i3,if3,i4,if4,
     *                     it3sum,volit3
        call writloga('bat',0,logdan,0,ierror)
 9000   format(a8,' it1=',i8,1x,a4,4(i7,a1),i8,1x,1pe10.3)
 9010   format(a8,' it2=',i8,1x,a4,4(i7,a1),i8,1x,1pe10.3)
 9020   format(a8,' it3=',i8,1x,a4,4(i7,a1),i8,1x,1pe10.3)
        call writfls('bat',ierror)
C*****  call tettestd
      endif
C
      goto 9999
 9999 continue
      return
      end
