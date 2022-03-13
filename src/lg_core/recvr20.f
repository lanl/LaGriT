*dk,recvr20
      subroutine recvr20(it1,it2,ictrecvr,ipiitmp,
     *                   ipjjtmp,ipkttmp)
       implicit real*8 (a-h,o-z)
C ######################################################################
C
C     PURPOSE -
C
C        This routine performs a temporary 2-to-0 flip and saves
C        information for later recovering the original state of the
C        mesh.  The fluxer is not called.
C
C
C     INPUT ARGUMENTS -
C
C        it1      - the first tetrahedron
C        it2      - the second tetrahedron
C**JM    ipirecvr - pointer to an array in which to store ijtet
C**JM               information for later recovering the original mesh.
C        ictrecvr - the current number of tetrahedra in the "recvr"
C                   array.
C        ipiitmp  - pointer to array to store itet information for
C                   later recovering the original mesh.
C        ipjjtmp  - pointer to array to store jtet information for
C                   later recovering the original mesh.
C        ipkttmp  - pointer to array to store tet numbers for
C                   later recovering the original mesh.
C
C     OUTPUT ARGUMENTS -
C
C        ictrecvr - the current number of tetrahedra in the "recvr"
C                   array.
C
C     CHANGE HISTORY -
C
C        $Log: recvr20.f,v $
C        Revision 2.00  2007/11/09 20:04:00  spchu
C        Import to CVS
C
CPVCS    
CPVCS       Rev 1.5   05 Jan 2001 12:57:18   dcg
CPVCS    use dble in place of float so as not to lose digits
CPVCS
CPVCS       Rev 1.4   Mon Apr 14 16:58:40 1997   pvcs
CPVCS    No change.
CPVCS
CPVCS       Rev 1.3   12/02/94 15:06:42   het
CPVCS    Added an option for the "cmo" access functions
CPVCS
CPVCS
CPVCS       Rev 1.2   12/01/94 18:49:02   het
CPVCS    Added a data type to the "cmo" calles
CPVCS       and added the "cmo.h" include file.
CPVCS
CPVCS       Rev 1.1   11/17/94 21:54:42   het
CPVCS    Added include files for chydro, neibor, cmerge, comdict. Added calles and
CPVCS    pointer statements for current_mesh_object database access.
CPVCS
CPVCS       Rev 1.0   11/10/94 12:17:40   pvcs
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
C     MAKE THE "jtet" ARRAY CONSISTENT.
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
                     iti=0.25*dble(itemp)+0.9
                     itj=0.25*dble(jtemp)+0.9
                     if (kfix(2,iti) .eq. 0) then
                        kfix(2,iti)=1
                        ictrecvr=ictrecvr+1
                        kttmp(ictrecvr)=iti
                        iitmp(1,ictrecvr)=itet(1,iti)
                        iitmp(2,ictrecvr)=itet(2,iti)
                        iitmp(3,ictrecvr)=itet(3,iti)
                        iitmp(4,ictrecvr)=itet(4,iti)
                        jjtmp(1,ictrecvr)=jtet(1,iti)
                        jjtmp(2,ictrecvr)=jtet(2,iti)
                        jjtmp(3,ictrecvr)=jtet(3,iti)
                        jjtmp(4,ictrecvr)=jtet(4,iti)
                     endif
                     if (kfix(2,itj) .eq. 0) then
                        kfix(2,itj)=1
                        ictrecvr=ictrecvr+1
                        kttmp(ictrecvr)=itj
                        iitmp(1,ictrecvr)=itet(1,itj)
                        iitmp(2,ictrecvr)=itet(2,itj)
                        iitmp(3,ictrecvr)=itet(3,itj)
                        iitmp(4,ictrecvr)=itet(4,itj)
                        jjtmp(1,ictrecvr)=jtet(1,itj)
                        jjtmp(2,ictrecvr)=jtet(2,itj)
                        jjtmp(3,ictrecvr)=jtet(3,itj)
                        jjtmp(4,ictrecvr)=jtet(4,itj)
                     endif
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
                     iti=0.25*dble(itemp)+0.9
                     itj=0.25*dble(jtemp)+0.9
                     if (kfix(2,iti) .eq. 0) then
                        kfix(2,iti)=1
                        ictrecvr=ictrecvr+1
                        kttmp(ictrecvr)=iti
                        iitmp(1,ictrecvr)=itet(1,iti)
                        iitmp(2,ictrecvr)=itet(2,iti)
                        iitmp(3,ictrecvr)=itet(3,iti)
                        iitmp(4,ictrecvr)=itet(4,iti)
                        jjtmp(1,ictrecvr)=jtet(1,iti)
                        jjtmp(2,ictrecvr)=jtet(2,iti)
                        jjtmp(3,ictrecvr)=jtet(3,iti)
                        jjtmp(4,ictrecvr)=jtet(4,iti)
                     endif
                     if (kfix(2,itj) .eq. 0) then
                        kfix(2,itj)=1
                        ictrecvr=ictrecvr+1
                        kttmp(ictrecvr)=itj
                        iitmp(1,ictrecvr)=itet(1,itj)
                        iitmp(2,ictrecvr)=itet(2,itj)
                        iitmp(3,ictrecvr)=itet(3,itj)
                        iitmp(4,ictrecvr)=itet(4,itj)
                        jjtmp(1,ictrecvr)=jtet(1,itj)
                        jjtmp(2,ictrecvr)=jtet(2,itj)
                        jjtmp(3,ictrecvr)=jtet(3,itj)
                        jjtmp(4,ictrecvr)=jtet(4,itj)
                     endif
                     jtet1(jtemp)=ipack
                     jtet1(itemp)=jpack
                  else
                     if (jtemp .lt. mbndry) then
                        itj=0.25*dble(jtemp)+0.9
                        if (kfix(2,itj) .eq. 0) then
                           kfix(2,itj)=1
                           ictrecvr=ictrecvr+1
                           kttmp(ictrecvr)=itj
                           iitmp(1,ictrecvr)=itet(1,itj)
                           iitmp(2,ictrecvr)=itet(2,itj)
                           iitmp(3,ictrecvr)=itet(3,itj)
                           iitmp(4,ictrecvr)=itet(4,itj)
                           jjtmp(1,ictrecvr)=jtet(1,itj)
                           jjtmp(2,ictrecvr)=jtet(2,itj)
                           jjtmp(3,ictrecvr)=jtet(3,itj)
                           jjtmp(4,ictrecvr)=jtet(4,itj)
                        endif
                        jtet1(jtemp)=ipack
                     endif
                     if (itemp .lt. mbndry) then
                        iti=0.25*dble(itemp)+0.9
                        if (kfix(2,iti) .eq. 0) then
                           kfix(2,iti)=1
                           ictrecvr=ictrecvr+1
                           kttmp(ictrecvr)=iti
                           iitmp(1,ictrecvr)=itet(1,iti)
                           iitmp(2,ictrecvr)=itet(2,iti)
                           iitmp(3,ictrecvr)=itet(3,iti)
                           iitmp(4,ictrecvr)=itet(4,iti)
                           jjtmp(1,ictrecvr)=jtet(1,iti)
                           jjtmp(2,ictrecvr)=jtet(2,iti)
                           jjtmp(3,ictrecvr)=jtet(3,iti)
                           jjtmp(4,ictrecvr)=jtet(4,iti)
                        endif
                        jtet1(itemp)=jpack
                     endif
                  endif
               endif
 10         continue
         endif
 20   continue
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
      itet(1,it1)=-itet(1,it1)
      itet(1,it2)=-itet(1,it2)
      do 30 i=1,4
 30   continue
C
      goto 9999
 9999 continue
      return
      end
