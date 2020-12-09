*dk,flip2to0
      subroutine flip2to0(it1,it2,npoints,ntets)
C
C ######################################################################
C
C     PURPOSE -
C
C        This routine performs the 2-to-0 flip
C
C     INPUT ARGUMENTS -
C
C        it1      - the first tetrahedron
C        it2      - the second tetrahedron
C
C     OUTPUT ARGUMENTS -
C
C         None
C
C     CHANGE HISTORY -
C
C        $Log: flip2to0.f,v $
C        Revision 2.00  2007/11/05 19:45:55  spchu
C        Import to CVS
C
CPVCS    
CPVCS       Rev 1.8   05 Jan 2001 12:55:52   dcg
CPVCS    use dble in place of float so as not to lose digits
CPVCS
CPVCS       Rev 1.7   Fri Apr 02 11:29:06 1999   dcg
CPVCS    use implicit none
CPVCS
CPVCS       Rev 1.6   Wed Feb 03 15:22:50 1999   dcg
CPVCS    remove calls to fluxing routines and associated memory.
CPVCS
CPVCS       Rev 1.5   Mon Apr 14 16:48:22 1997   pvcs
CPVCS    No change.
CPVCS
CPVCS       Rev 1.4   12/02/94 15:05:12   het
CPVCS    Added an option for the "cmo" access functions
CPVCS
CPVCS
CPVCS       Rev 1.3   12/01/94 18:46:56   het
CPVCS    Added a data type to the "cmo" calles
CPVCS       and added the "cmo.h" include file.
CPVCS
CPVCS       Rev 1.2   11/23/94 10:16:52   dcg
CPVCS     declare if1,if2,if3,if4 as character variables
CPVCS
CPVCS       Rev 1.1   11/17/94 21:51:16   het
CPVCS    Added include files for chydro, neibor, cmerge, comdict. Added calles and
CPVCS    pointer statements for current_mesh_object database access.
CPVCS
CPVCS       Rev 1.0   11/10/94 12:12:32   pvcs
CPVCS    Original version.
C
C ######################################################################
C
      implicit none
C
      include "cmo.h"
      include "chydro.h"
      include "neibor.h"

C arguments (it1,it2,npoints,ntets)
      integer it1,it2,npoints,ntets

C variables
      integer i,j,k,i1,i2,i3,i4,ierror,length,icmotype,
     *   it1sum,it2sum,ierrdum,ierrfls,jtemp,itemp,jpack,ipack,ione

      character*8 if1,if2,if3,if4

      real*8 volit1,volit2

C ######################################################################
C
C     DEFINE THE STATEMENT FUNCTIONS NEEDED TO CALCULATE TET VOLUMES.
C
      real*8 crosx1,crosy1,crosz1,volume
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
C BEGIN begin
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
      call cmo_get_info('itp1',cmo,ipitp1,length,icmotype,ierror)
      call cmo_get_info('xic',cmo,ipxic,length,icmotype,ierror)
      call cmo_get_info('yic',cmo,ipyic,length,icmotype,ierror)
      call cmo_get_info('zic',cmo,ipzic,length,icmotype,ierror)
      call cmo_get_info('itetclr',cmo,ipitetclr,length,icmotype,ierror)
      call cmo_get_info('itet',cmo,ipitet,length,icmotype,ierror)
      call cmo_get_info('jtet',cmo,ipjtet,length,icmotype,ierror)
C
      endif
C
C     ******************************************************************
C
 
C     ..................................................................
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
        write(logdan,9000) 'flip2to0',it1,'old=',
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
C     ..................................................................
C     MAKE THE 'jtet' ARRAY CONSISTENT.
C
      do 20 i=1,4
         if(0.25*dble(jtet(i,it1))+0.9.ne.it2) then
            do 10 j=1,4
               if(itet(j,it2).eq.itet(i,it1)) then
                  jtemp=jtet(j,it2)
                  itemp=jtet(i,it1)
                  jpack=jtemp
                  ipack=itemp
                  if (jtemp.lt.mbndry.and.itemp.lt.mbndry) then
                     jtet1(jtemp)=ipack
                     jtet1(itemp)=jpack
                     goto 20
                  endif
                  if (jtemp .gt. mbndry) jtemp=jtemp-mbndry
                  if (itemp .gt. mbndry) itemp=itemp-mbndry
                  if (jtemp.lt.mbndry .and. itemp.lt.mbndry) then
                     if (imt1(itet1(jtemp)).eq.imt1(itet1(itemp))) then
                        if (jpack .gt. mbndry) jpack=jpack-mbndry
                        if (ipack .gt. mbndry) ipack=ipack-mbndry
                     else
                        if (jpack .lt. mbndry) jpack=jpack+mbndry
                        if (ipack .lt. mbndry) ipack=ipack+mbndry
                     endif
                     jtet1(jtemp)=ipack
                     jtet1(itemp)=jpack
                  else
                     if (jtemp .lt. mbndry) then
                        jtet1(jtemp)=ipack
                     endif
                     if (itemp .lt. mbndry) then
                        jtet1(itemp)=jpack
                     endif
                  endif
               endif
 10         continue
         endif
 20   continue
C
C     ..................................................................
C     SQUEEZE OUT THE TWO OVERLAPPING TETRAHEDRA.
C
      do 30 i=1,4
 30   continue
      itet(1,it1)=-itet(1,it1)
      itet(1,it2)=-itet(1,it2)
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
