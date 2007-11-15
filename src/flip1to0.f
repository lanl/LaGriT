*dk,flip1to0
      subroutine flip1to0(it1,
     *                    npoints,ntets)
       implicit real*8 (a-h,o-z)
C ######################################################################
C
C     PURPOSE -
C
C        This routine flips removes boundary tets that have exactly
C        two boundary faces.
C
C     INPUT ARGUMENTS -
C
C        it1      - the tet to remove
C
C     OUTPUT ARGUMENTS -
C
C        None
C
C     CHANGE HISTORY -
C
C        $Log: flip1to0.f,v $
C        Revision 2.00  2007/11/05 19:45:55  spchu
C        Import to CVS
C
CPVCS    
CPVCS       Rev 1.7   Wed Feb 03 15:30:02 1999   dcg
CPVCS    remove calls to fluxing routines and associated memory.
CPVCS
CPVCS       Rev 1.6   Mon Apr 14 16:48:20 1997   pvcs
CPVCS    No change.
CPVCS
CPVCS       Rev 1.5   Mon Jun 03 15:13:58 1996   dcg
CPVCS    hp changes
CPVCS
CPVCS       Rev 1.4   Mon May 06 12:28:48 1996   dcg
CPVCS    use itsttp to test for point types
CPVCS
CPVCS       Rev 1.3   12/02/94 15:05:10   het
CPVCS    Added an option for the "cmo" access functions
CPVCS
CPVCS
CPVCS       Rev 1.2   12/01/94 18:46:52   het
CPVCS    Added a data type to the "cmo" calles
CPVCS       and added the "cmo.h" include file.
CPVCS
CPVCS       Rev 1.1   11/17/94 21:51:10   het
CPVCS    Added include files for chydro, neibor, cmerge, comdict. Added calles and
CPVCS    pointer statements for current_mesh_object database access.
CPVCS
CPVCS       Rev 1.0   11/10/94 12:12:28   pvcs
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
      character*4 if1,if2,if3,if4
      logical itsttp
C
C ######################################################################
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
      call cmo_get_info('itp1',cmo,ipitp1,lenitp1,icmotype,ierror)
      call cmo_get_info('xic',cmo,ipxic,lenxic,icmotype,ierror)
      call cmo_get_info('yic',cmo,ipyic,lenyic,icmotype,ierror)
      call cmo_get_info('zic',cmo,ipzic,lenzic,icmotype,ierror)
      call cmo_get_info('itet',cmo,ipitet,lenitet,icmotype,ierror)
      call cmo_get_info('jtet',cmo,ipjtet,lenjtet,icmotype,ierror)
C
      endif
C
C     ******************************************************************
C
C
      iepos=0
      do 10 i=1,6
         i1=ielist(4*(i-1)+3)
         i2=ielist(4*(i-1)+4)
         if(jtet(i1,it1).ge.mbndry.and.jtet(i2,it1).ge.mbndry) then
            iepos=i
            goto 11
         endif
 10   continue
      if(iepos.eq.0) goto 9999
      ip1=itet(1,it1)
      ip2=itet(2,it1)
      ip3=itet(3,it1)
      ip4=itet(4,it1)
      if (.not.(itsttp('inteintf',itp1(ip1))).or.
     *    .not.(itsttp('inteintf',itp1(ip2))).or.
     *    .not.(itsttp('inteintf',itp1(ip3))).or.
     *    .not.(itsttp('inteintf',itp1(ip4)))) go to 9999
C
 11   jtet1(jtet(ielist(4*(i-1)+1),it1))=mbndry
      jtet1(jtet(ielist(4*(i-1)+2),it1))=mbndry
C
C     ..................................................................
C     IF THE DEBUG FLAG IS TURNED ON THEN PRINT A SUMMARY OF THE FLIP.
C
      if(idebug.gt.1) then
        itsum=itet(1,it1)+itet(2,it1)+itet(3,it1)+itet(4,it1)
        volit=volume(itet(1,it1),itet(2,it1),itet(3,it1),itet(4,it1))
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
        write(logdan,9000) 'flip1to0',it1,'old=',
     *                     i1,if1,i2,if2,i3,if3,i4,if4,
     *                     itsum,volit
 9000   format(a8,' it1=',i8,1x,a4,4(i7,a1),i8,1x,1pe10.3)
        call writloga('bat',0,logdan,0,ierrdum)
      endif
C
      itet(1,it1)=-itet(1,it1)
C
      nvacnt=nvacnt+1
      if(nvacnt.ge.lenvacnt) then
         ione=1
         lenvacnt=lenvacnt+100
         call mflip(ione,lenvacnt,'ivacnt')
      endif
      ivacnt(nvacnt)=it1
C
      goto 9999
 9999 continue
      return
      end
