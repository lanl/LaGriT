*dk,vorfclst
      subroutine vorfclst(itnum,iepos,lst,ioffset,ict,ibndflg,ierr)
       implicit real*8 (a-h,o-z)
C
C ######################################################################
C
C     PURPOSE -
C
C        This routine constructs a list of all tetrahedra surrounding
C        the edge "iepos" in tet "it" in a clockwise fashion.
C
C     INPUT ARGUMENTS -
C
C        itnum     - the tet number
C        iepos     - the edge position
C        lst    - an array to write the tet list
C        ioffset   - an offset to this array ---obsolete
C
C     OUTPUT ARGUMENTS -
C
C        ict       - the number of tetrahedrons in the list
C        ibndflg   - a flag that is set to "1" if a boundary has been
C                    encountered
C        ierr      - a flag that is set to "1" if there is an error
C
C     CHANGE HISTORY -
C
C        $Log: vorfclst.f,v $
C        Revision 2.00  2007/11/09 20:04:06  spchu
C        Import to CVS
C
CPVCS    
CPVCS       Rev 1.6   05 Jan 2001 12:55:58   dcg
CPVCS    use dble in place of float so as not to lose digits
CPVCS
CPVCS       Rev 1.5   Mon Apr 14 17:05:48 1997   pvcs
CPVCS    No change.
CPVCS
CPVCS       Rev 1.4   Thu May 02 11:10:20 1996   dcg
CPVCS    replace nasty pointer arithmetic
CPVCS
CPVCS       Rev 1.3   12/02/94 15:07:18   het
CPVCS    Added an option for the "cmo" access functions
CPVCS
CPVCS
CPVCS       Rev 1.2   12/01/94 18:49:54   het
CPVCS    Added a data type to the "cmo" calles
CPVCS       and added the "cmo.h" include file.
CPVCS
CPVCS       Rev 1.1   11/17/94 21:55:52   het
CPVCS    Added include files for chydro, neibor, cmerge, comdict. Added calles and
CPVCS    pointer statements for current_mesh_object database access.
CPVCS
CPVCS       Rev 1.0   11/10/94 12:20:14   pvcs
CPVCS    Original version.
C
C ##################################################################
C
      include "cmo.h"
      include "chydro.h"
      include "neibor.h"
C
C ##################################################################
C
c     pointer( iplst , lst(1) )
      integer lst(1000000)
C
C ######################################################################
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
      call cmo_get_info('itet',cmo,ipitet,lenitet,icmotype,ierror)
      call cmo_get_info('jtet',cmo,ipjtet,lenjtet,icmotype,ierror)
C
      endif
C
C     ******************************************************************
C
C
c     iplst=iplist+ioffset
      ierr=0
      ibndflg=0
C
      it=itnum
      ict=1
      lst(ict)=it
      ibegpt=itet(ielist(4*(iepos-1)+4),it)
      iendpt=itet(ielist(4*(iepos-1)+3),it)
C
      ipt=ibegpt
      ipto=iendpt
      do 10 i=1,300
         ipos=iposfnd(ipto,itet(1,it),itet(2,it),itet(3,it),itet(4,it))
         if(ipos.lt.1.or.ipos.gt.4) then
            ierr=1
            goto 9999
         endif
         newptr=jtet(ipos,it)
         if(newptr.lt.mbndry) then
            it=0.25*dble(newptr)+0.9
            ict=ict+1
            lst(ict)=it
            ipto=ipt
            ipt=itet1(newptr)
            if(ipt.eq.iendpt) goto 9999
         else
            if(ibndflg.eq.1) then
               ibndflg=2
               goto 9999
            endif
            ibndflg=1
            ipt=iendpt
            ipto=ibegpt
            it=itnum
         endif
 10   continue
C
      ierr=1
C
      goto 9999
 9999 continue
      return
      end
