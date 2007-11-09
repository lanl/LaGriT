*dk,try2to3i
      subroutine try2to3i(it1,ifpos1,it2,ibdytet,iflip,
     *                    npoints,ntets)
       implicit real*8 (a-h,o-z)
C
C ######################################################################
C
C     PURPOSE -
C
C        This routine checks whether it is feasible to flip a
C        connection on a material interface.  To consider such a flip,
C        the two tetrahedra, it1 and it2, must each have a boundary
C        face along the same connection, and that connection must be
C        only a two-material connection.  If the flip is feasible, that
C        boundary connection will be changed to an interior connection,
C        and a new boundary connection will be drawn between the two
C        points opposite the face common to it1 and it2.  The flip is
C        actually performed by the routine flp2to3i.
C
C     INPUT ARGUMENTS -
C
C        it1      - the first tet
C        ifpos1   - the position of the common face in it1
C
C     OUTPUT ARGUMENTS -
C
C        it2      - the second tet which shares the common face
C        ibdytet  - tells which of the three potential new tets will be
C                   the boundary tet
C        iflip    - 0 => flip is not indicated
C                   1 => flip is indicated
C
C     CHANGE HISTORY -
C
C        $Log: try2to3i.f,v $
C        Revision 2.00  2007/11/09 20:04:05  spchu
C        Import to CVS
C
CPVCS    
CPVCS       Rev 1.7   05 Jan 2001 12:58:12   dcg
CPVCS    use dble in place of float so as not to lose digits
CPVCS
CPVCS       Rev 1.6   Mon Jul 14 15:49:32 1997   dcg
CPVCS    fix argument in call to getchain - previous argument
CPVCS    was imt1 which caused the mesh object attribute imt1
CPVCS    to be overwritten - changed to local array imt0
CPVCS
CPVCS       Rev 1.5   Mon Apr 14 17:05:08 1997   pvcs
CPVCS    No change.
CPVCS
CPVCS       Rev 1.4   12/02/94 16:19:20   dcg
CPVCS     changes required to compile on IBM RS6000
CPVCS
CPVCS       Rev 1.3   12/02/94 15:07:10   het
CPVCS    Added an option for the "cmo" access functions
CPVCS
CPVCS
CPVCS       Rev 1.2   12/01/94 18:49:44   het
CPVCS    Added a data type to the "cmo" calles
CPVCS       and added the "cmo.h" include file.
CPVCS
CPVCS       Rev 1.1   11/17/94 21:55:32   het
CPVCS    Added include files for chydro, neibor, cmerge, comdict. Added calles and
CPVCS    pointer statements for current_mesh_object database access.
CPVCS
CPVCS       Rev 1.0   11/10/94 12:19:50   pvcs
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
      dimension kpts(3),ichain1(nmulti),imt0(nmulti)
C
C ######################################################################
C
C     MACROS.
C
      iposfnd(i0,i1,i2,i3,i4)=min0(iabs(i1-i0)*4+1,iabs(i2-i0)*4+2,
     *                             iabs(i3-i0)*4+3,iabs(i4-i0)*4+4)
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
C     ******************************************************************
C
C
      iflip=0
      it2=0.25*dble(jtet(ifpos1,it1))+0.9
C
C     ******************************************************************
C
C     DETERMINE IF TWO BOUNDARY FACES SHARE THE SAME CONNECTION.
C
      do 500 m=1,4
         if (jtet(m,it1) .le. mbndry) goto 500
         kpos1=m
         kpt=itet(kpos1,it1)
         kpos2=iposfnd(kpt,itet(1,it2),itet(2,it2),itet(3,it2),
     *                 itet(4,it2))
         if (jtet(kpos2,it2) .le. mbndry) goto 500
C
C        ***************************************************************
C
C        VERIFY THAT THE CONNECTION IS A TWO-MATERIAL CONNECTION.
C
         kpts(1)=itet(iflist(3*ifpos1-2),it1)
         kpts(3)=itet(iflist(3*ifpos1-1),it1)
         kpts(2)=itet(iflist(3*ifpos1  ),it1)
         do 300 k=1,3
            ktest=kpts(k)
            if (ktest .eq. kpt) then
C
C              .........................................................
C              DETERMINE WHICH OF THE THREE POTENTIAL NEW TETS WILL
C              BE THE BOUNDARY TET.
C
               if (k .eq. 1) then
                  ibdytet=2
               elseif (k .eq. 2) then
                  ibdytet=3
               else
                 ibdytet=1
               endif
               goto 300
            endif
            call getchain(ktest,ichain1,imt0,nmulti,ict1,ipar)
            if (ict1 .gt. 1)  goto 500
 300     continue
         iflip=1
         goto 9999
 500  continue
C
      goto 9999
 9999 continue
      end
