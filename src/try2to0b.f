*dk,try2to0b
      subroutine try2to0b(iary,ntet,nflips,nflipsb,it2,
     *                    npoints,ntets)
C
       implicit real*8 (a-h,o-z)
C ######################################################################
C
C     PURPOSE -
C
C        This routine checks a list of tets for the 2-to-0 flip.
C        Flips across material interfaces are considered.
C
C
C     INPUT ARGUMENTS -
C
C        ipiary   - pointer to the array of tets to check.
C        ntet    - the number of tets in the list
C
C     OUTPUT ARGUMENTS -
C
C        flips    - the number of nonboundary flips that have occurred.
C        nflipsb  - the number of boundary flips that have occurred.
C        it2      - the second tet that is removed.
C
C     CHANGE HISTORY -
C
C        $Log:   /pvcs.config/t3d/src/try2to0b.f_a  $
CPVCS    
CPVCS       Rev 1.9   05 Jan 2001 12:57:26   dcg
CPVCS    use dble in place of float so as not to lose digits
CPVCS
CPVCS       Rev 1.8   Mon Apr 14 17:04:56 1997   pvcs
CPVCS    No change.
CPVCS
CPVCS       Rev 1.7   Thu Oct 17 14:05:02 1996   dcg
CPVCS    declare isubname to be type character
CPVCS
CPVCS       Rev 1.6   Tue Oct 01 08:55:20 1996   dcg
CPVCS    move memory manager calls
CPVCS
CPVCS       Rev 1.5   Tue Apr 16 14:34:54 1996   dcg
CPVCS    try2to2b.f try2to4r.f try4to4x.f trymtonr.f
CPVCS    replace pointer ipitets with array itets
CPVCS
CPVCS       Rev 1.4   Tue Mar 05 14:17:44 1996   dcg
CPVCS    remove icn1 int1
CPVCS
CPVCS       Rev 1.3   12/02/94 15:07:02   het
CPVCS    Added an option for the "cmo" access functions
CPVCS
CPVCS
CPVCS       Rev 1.2   12/01/94 18:49:30   het
CPVCS    Added a data type to the "cmo" calles
CPVCS       and added the "cmo.h" include file.
CPVCS
CPVCS       Rev 1.1   11/17/94 21:55:18   het
CPVCS    Added include files for chydro, neibor, cmerge, comdict. Added calles and
CPVCS    pointer statements for current_mesh_object database access.
CPVCS
CPVCS       Rev 1.0   11/10/94 12:19:30   pvcs
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
      parameter (nmulti = 200)
      dimension ichain0(nmulti),imt0(nmulti)
c     pointer( ipiary  , iary(1)  )
      integer iary(1000000)
      pointer (ipint1, int1)
      integer int1(1000000)
      character*8 isubname
C
C ######################################################################
C
C
C
C     ******************************************************************
C     FETCH MESH OBJECT NAME AND POINTER INFORMATION.
C
      isubname='try2to0b'
      if(icmoget.eq.1) then
C
      call cmo_get_name(cmo,ierror)
C
      call cmo_get_info('mbndry',cmo,mbndry,length,icmotype,ierror)
      call cmo_get_info('imt1',cmo,ipimt1,lenimt1,icmotype,ierror)
      call cmo_get_info('itp1',cmo,ipitp1,lenitp1,icmotype,ierror)
      call cmo_get_info('itetclr',cmo,ipitetclr,lenitetclr,icmotype,ier)
      call cmo_get_info('itet',cmo,ipitet,lenitet,icmotype,ierror)
      call cmo_get_info('jtet',cmo,ipjtet,lenjtet,icmotype,ierror)
C
      endif
C
      call mmgetblk('int1',isubname,ipint1,npoints,1,ierror)
      call unpacktp('intrface','set',npoints,ipitp1,ipint1,ierror)
C
C     ******************************************************************
      nflips=0
      nflipsb=0
      do 100 i=1,ntet
         it=iary(i)
         if(it.eq.0.or.itet(1,it).lt.0) goto 100
         imtx=imt1(itet(1,it))
         do 90 j=1,4
            if(jtet(j,it).lt.mbndry) then
               if(itet1(jtet(j,it)).eq.itet(j,it)) then
                  it2=0.25*dble(jtet(j,it))+0.9
                  call flip2to0(it,it2,
     *                          npoints,ntets)
                  iary(i)=0
                  do 10 k=i+1,ntet
                     if(iary(k).eq.it2) iary(k)=0
 10               continue
                  nflips=nflips+1
                  goto 100
               endif
            elseif (jtet(j,it) .gt. mbndry) then
               jtemp=jtet(j,it)-mbndry
               if (int1(itet1(jtemp)) .eq. 0) goto 90
               call getchain(itet1(jtemp),ichain0,imt0,nmulti,ict,ipar)
               index=0
               do 20 k=1,ict
                  if (imt0(k).eq.imtx) then
                     index=k
                     goto 50
                  endif
 20            continue
               if (index.eq.0) goto 90
 50            continue
               kpt2=ichain0(index)
               if (itet(j,it) .eq. kpt2) then
                  it2=0.25*dble(jtemp)+0.9
                  ifpos=j
                  call flp2to0b(it,ifpos,it2,
     *                          npoints,ntets)
                  iary(i)=0
                  do 60 k=i+1,ntet
                     if (iary(k) .eq. it2) iary(k)=0
 60               continue
                  nflipsb=nflipsb+1
                  goto 100
               endif
            endif
C
 90      continue
 100   continue
C
      goto 9999
 9999 continue
      call mmrelprt (isubname,ierr)
      return
      end
