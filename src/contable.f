*dk,contable
      subroutine contable
      implicit none
C
C ######################################################################
C
C     PURPOSE -
C
C        This routine computes the merging-constraint table for points
C        on the reflective boundaries.
C
C     INPUT ARGUMENTS -
C
C        None
C
C     OUTPUT ARGUMENTS -
C
C        None
C
C     CHANGE HISTORY -
C
C        $Log:   /pvcs.config/t3d/src/contable.f_a  $
CPVCS    
CPVCS       Rev 1.12   Fri May 09 12:03:14 1997   dcg
CPVCS    correctly set flag for merging point on line to
CPVCS    a corner point
CPVCS
CPVCS       Rev 1.11   Mon Apr 14 16:42:28 1997   pvcs
CPVCS    No change.
CPVCS
CPVCS       Rev 1.10   Wed Jun 19 10:18:30 1996   dcg
CPVCS    fixes to merge resulting from surface type changes
CPVCS
CPVCS       Rev 1.9   Wed May 22 10:14:42 1996   dcg
CPVCS    get nconbnd from mesh object
CPVCS
CPVCS       Rev 1.8   Thu May 16 10:22:52 1996   dcg
CPVCS    changes for new interface type 3 and for new icontab, xcontab
CPVCS
CPVCS       Rev 1.7   11/08/95 11:35:02   dcg
CPVCS    fix dimensions on mtable2
CPVCS
CPVCS       Rev 1.6   11/07/95 17:15:46   dcg
CPVCS    change flag to 2 in mmgetblk calls
CPVCS
CPVCS       Rev 1.5   06/27/95 11:10:04   dcg
CPVCS    remove second literal argument in memory management calls
CPVCS
CPVCS       Rev 1.4   02/23/95 20:15:24   het
CPVCS    Correct an error with iptable2 and mtable2
CPVCS
CPVCS       Rev 1.3   01/04/95 22:01:50   llt
CPVCS    unicos changes (made by het)
CPVCS
CPVCS       Rev 1.2   11/17/94 21:32:56   het
CPVCS    Added the calles to the reconnection routines and connected an error with
CPVCS           the management of the mflip memory.
CPVCS
CPVCS
CPVCS       Rev 1.1   11/14/94 12:20:24   het
CPVCS
CPVCS
CPVCS       Rev 1.1   11/14/94 12:18:34   het
CPVCS
CPVCS
CPVCS       Rev 1.0   11/13/94 15:39:58   het
CPVCS    Orginal Version
C
C ######################################################################
C
      include "chydro.h"
      include "consts.h"
      include "cmerge.h"
C
C ######################################################################
C
      character*32 isubname, cmo
C
C
      integer nmatch, jstype, istype,i,j,k,l,ipl,jpl,iconlen,ityp
      integer ics,length,nconbnd,lentab
c
      pointer (ipicontab,icontab)
      integer icontab(50,1000000)
C ######################################################################
C
      isubname='contable'
C
C  GET ICONTAB
C
      call cmo_get_name(cmo,ics)
      call cmo_get_info('icontab',cmo,ipicontab,iconlen,ityp,ics)
      if (ics.ne.0) call x3d_error(isubname,'cmo_get_info')
      call cmo_get_info('nconbnd',cmo,nconbnd,iconlen,ityp,ics)
      if (ics.ne.0) call x3d_error(isubname,'cmo_get_info')
C     GET MEMORY FOR THE CONSTRAINT TABLE "mtable2"
C
C*****length=nconbnd*nconbnd
      length=max(60*60,nconbnd*nconbnd)
C     *** 60 is the maximum number of node types.
C     *** nconbnd is the number of constraints
      call mmfindbk('mtable2',isubname,iptable2,lentab,ics)
      if(ics.ne.0) then
         call mmgetblk('mtable2',isubname,iptable2,length,1,ics)
      else
         call mmnewlen('mtable2',isubname,iptable2,length,ics)
      endif
C
C     ******************************************************************
C
C     COMPUTE THE CONSTRAINT TABLE
C
      do  i=1,nconbnd*nconbnd
         mtable2(i)=0
      enddo
C
      do  i=1,nconbnd
         istype=icontab(2,i)
         ipl=icontab(3,i)
         mtable2(nconbnd*(i-1)+i)=1
         do  j=1,nconbnd
            jstype=icontab(2,j)
            jpl=icontab(3,j)
            if (istype .eq. 2) then
C        SURFACE --> SURFACE ONLY IF SURFACES ARE THE SAME.
               if (jstype.eq.2) then
                   if (jpl.eq.ipl) mtable2(nconbnd*(j-1)+i)=1
C        SURFACE --> POINT ONLY IF THE POINT IS IN THE SURFACE.
C        SURFACE --> LINE ONLY IF THE LINE IS IN THE SURFACE.
               elseif (jstype.eq.1.or.jstype.eq.0) then
                   do k = 1,icontab(1,j)
                      if(icontab(2+k,j).eq.ipl) mtable2(nconbnd*(j-1)+i)
     *                                                  =1
                   enddo
               endif
C        LINE  --> LINE  ONLY IF LINES  ARE THE SAME.
C        LINE  --> POINT ONLY IF THE POINT IS ON THE LINE.
            elseif (istype.eq.1) then
               if (jstype.eq.1.or.jstype.eq.0) then
                   nmatch=0
                   do k=1,icontab(1,i)
                      do l=1,icontab(1,j)
                         if (icontab(2+k,i).eq.icontab(2+l,j))
     *                        nmatch=nmatch+1
                      enddo
                   enddo
                   if(nmatch.eq.icontab(1,i)) mtable2(nconbnd*(j-1)+i)
     *                                                  =1
                endif
             endif
         enddo
      enddo
C
      goto 9999
 9999 continue
      return
      end
