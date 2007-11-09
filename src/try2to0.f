*dk,try2to0
      subroutine try2to0(iary,ntet,nflips,it2,
     *                   npoints,ntets)
      implicit real*8 (a-h,o-z)
C
C ######################################################################
C
C     PURPOSE -
C
C        This routine checks a list of tets for the 2-to-0 flip.
C
C     INPUT ARGUMENTS -
C
C        ipiary   - pointer to the array of tets to check.
C        ntet    - the number of tets in the list
C
C     OUTPUT ARGUMENTS -
C
C        nflips   - the number of flips that have occurred.
C        it2      - the second tet that is removed.
C
C     CHANGE HISTORY -
C
C        $Log:   /pvcs.config/t3d/src/try2to0.f_a  $
CPVCS    
CPVCS       Rev 1.6   05 Jan 2001 12:58:20   dcg
CPVCS    use dble in place of float so as not to lose digits
CPVCS
CPVCS       Rev 1.5   Mon Apr 14 17:04:54 1997   pvcs
CPVCS    No change.
CPVCS
CPVCS       Rev 1.4   Tue Apr 16 14:35:16 1996   dcg
CPVCS    try2to2b.f try2to4r.f try4to4x.f trymtonr.f
CPVCS    replace pointer ipitets with array itets
CPVCS
CPVCS       Rev 1.3   12/02/94 15:07:00   het
CPVCS    Added an option for the "cmo" access functions
CPVCS
CPVCS
CPVCS       Rev 1.2   12/01/94 18:49:26   het
CPVCS    Added a data type to the "cmo" calles
CPVCS       and added the "cmo.h" include file.
CPVCS
CPVCS       Rev 1.1   11/17/94 21:55:16   het
CPVCS    Added include files for chydro, neibor, cmerge, comdict. Added calles and
CPVCS    pointer statements for current_mesh_object database access.
CPVCS
CPVCS       Rev 1.0   11/10/94 12:19:28   pvcs
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
c     pointer( ipiary  , iary(ntet)  )
      integer iary(1000000)
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
      call cmo_get_info('itetclr',cmo,ipitetclr,lenitetclr,icmotype,ier)
      call cmo_get_info('itet',cmo,ipitet,lenitet,icmotype,ierror)
      call cmo_get_info('jtet',cmo,ipjtet,lenjtet,icmotype,ierror)
C
      endif
C
C
C     ******************************************************************
C
C
      nflips=0
      do 30 i=1,ntet
         it=iary(i)
         if(it.eq.0.or.itet(1,it).lt.0) goto 30
         do 20 j=1,4
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
                  goto 30
               endif
            endif
 20      continue
 30   continue
C
      goto 9999
 9999 continue
      return
      end
