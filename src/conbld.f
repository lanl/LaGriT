      subroutine conbld()
C
C
C#######################################################################
C
C     PURPOSE -
C
C        THIS ROUTINE SETS UP CONSTRAINT TYPES FOR REFLECTIVE BOUNDARY
C        PLANES AND FOR CONSTRAINED INTERIOR INTERFACES.
C
C     INPUT ARGUMENTS -
C
C        NONE
C
C     OUTPUT ARGUMENTS -
C
C        NONE
C
C     CHANGE HISTORY -
C
C        $Log:   /pvcs.config/t3d/src/conbld.f_a  $
CPVCS    
CPVCS       Rev 1.17   18 Apr 2000 13:24:14   dcg
CPVCS    implement release option for surfaces, regions and mregions
CPVCS    
CPVCS       Rev 1.16   28 Mar 2000 14:09:04   dcg
CPVCS    remove include 'machine.h'
CPVCS
CPVCS       Rev 1.15   Wed Mar 01 08:52:00 2000   dcg
CPVCS    change default ioflag to 'l'
CPVCS
CPVCS       Rev 1.14   Wed Dec 17 12:02:30 1997   dcg
CPVCS    set nconbnd to zero if attribute does not exist
CPVCS
CPVCS       Rev 1.13   Mon Apr 14 16:42:20 1997   pvcs
CPVCS    No change.
CPVCS
CPVCS       Rev 1.12   Wed Jun 19 10:17:54 1996   dcg
CPVCS    fixes to merge resulting from surface type changes
CPVCS
CPVCS       Rev 1.11   Wed May 22 15:54:00 1996   dcg
CPVCS    fix set_info('nconbnd'... calls
CPVCS
CPVCS       Rev 1.10   Wed May 22 10:14:32 1996   dcg
CPVCS    get nconbnd from mesh object
CPVCS
CPVCS       Rev 1.9   Thu May 16 10:22:38 1996   dcg
CPVCS    changes for new interface type 3 and for new icontab, xcontab
CPVCS
CPVCS       Rev 1.8   11/07/95 17:15:42   dcg
CPVCS    change flag to 2 in mmgetblk calls
CPVCS
CPVCS       Rev 1.7   06/22/95 11:15:26   dcg
CPVCS    replace character literal 'boundary' with variable cboundy
CPVCS
CPVCS       Rev 1.6   06/07/95 10:59:24   het
CPVCS    Eldon's versions with implicit none
CPVCS
CPVCS       Rev 1.5   03/28/95 12:35:20   het
CPVCS    Add the binary dumpx3d/readx3d commands and correct associated mm-errors.
CPVCS
CPVCS       Rev 1.4   03/23/95 22:57:34   het
CPVCS    Add the model routines and add the cmo name into the idsbs
CPVCS
CPVCS       Rev 1.3   01/23/95 16:58:38   het
CPVCS    Correct errors in the names for some cmo_get_info calles.
CPVCS
CPVCS
CPVCS       Rev 1.2   12/24/94 10:52:10   het
CPVCS    Add include files for chydro.h and comdict.h.
CPVCS
CPVCS
CPVCS       Rev 1.1   11/17/94 21:27:48   het
CPVCS
CPVCS
CPVCS       Rev 1.1   11/16/94 07:51:40   het
CPVCS    Deleted the call to "mmrelptr" at the end of the subroutine. The constraint
CPVCS        table information needs to be preserved for merging and reconnection.
CPVCS
CPVCS       Rev 1.0   11/10/94 12:11:18   pvcs
CPVCS    Original version.
C
C#######################################################################
C
      implicit none
C
C#######################################################################
C
      include "chydro.h"
C
C#######################################################################
      character*32 cmo
      character*132 ibuff
      integer icscode,iconlen
      pointer (ipicontab,icontab)
      integer icontab(50,1000000)
C
C#######################################################################
C
C
C   CHECK TO SEE IF ICONTAB EXISTS FOR THIS CMO
C
      call cmo_get_name(cmo,icscode)
      call mmfindbk('icontab',cmo,ipicontab,iconlen,icscode)
C
      if(icscode.ne.0) then
        ibuff= 'no constrained surfaces in conbld'
        call writloga('default',0,ibuff,0,icscode)
        go to 9999
      endif
C
C     ******************************************************************
C     CALL bndpts TO FIND POINTS THAT LIE ON A BOUNDARY PLANE
C
      call bndpts()
C     ******************************************************************
C
 9999 continue
      return
      end
C
c
      subroutine condel(isurfnum)

C
C
C
C#######################################################################
C
C     PURPOSE -
C
C        THIS ROUTINE REMOVES A CONSTRAINT ENTRY FROM ICONTAB
C
C     INPUT ARGUMENTS -
C
C        isurfnum - surface number of this surface constraint
C
C     OUTPUT ARGUMENTS -
C
C        NONE
C
C
C#######################################################################
C
      implicit none
C
C#######################################################################
C
      include "chydro.h"
C
C#######################################################################
      pointer (ipicontab,icontab)
      integer icontab(50,*)
      character*32 cmo
      integer icscode,iconlen,num,i,j,k,isurfnum,n,nconbnd
C
C   CHECK TO SEE IF ICONTAB EXISTS FOR THIS CMO - if not return
C
      call cmo_get_name(cmo,icscode)
      call cmo_get_intinfo('nconbnd',cmo,nconbnd,iconlen,j,icscode)
      call mmfindbk('icontab',cmo,ipicontab,iconlen,icscode)
      if(icscode.ne.0) go to 9999
c
c  look through all icontabs and remove all references to this
c  surface
c
      do n=1,nconbnd
         num=icontab(1,n)
         if(num.eq.1) then
            if(icontab(3,n).gt.isurfnum) icontab(3,n)=
     *                             icontab(3,n)-1
            if(icontab(3,n).eq.isurfnum) then
c
c  single surface entry - remove entire entry
c
               if(n.lt.nconbnd) then
                  do i=n+1,nconbnd
                     j=icontab(1,i)
                     do k=1,j+2
                        icontab(k,i-1)=icontab(k,i)
                        if(icontab(k,i-1).gt.isurfnum) 
     *                   icontab(k,i-1)=icontab(k,i-1)-1
                     enddo
                  enddo
                endif
                nconbnd=nconbnd-1
                call cmo_set_info('nconbnd',cmo,nconbnd,1,1,icscode)
                icontab(1,nconbnd+1)=0
                icontab(2,nconbnd+1)=0
                icontab(3,nconbnd+1)=0
             endif
          elseif(num.gt.1) then
             i=icontab(1,n)
             do j=3,i+2
                if(icontab(j,n).gt.isurfnum) icontab(j,n)=
     *                             icontab(j,n)-1
                if(icontab(j,n).eq.isurfnum) then
c
c  multiple surface entry - remove just this surface
c
                   if(j.eq.i+2) then
                      icontab(j,n)=0
                   else
                      do k=j+1,i+2
                         icontab(k-1,n)=icontab(k,n)
                         if(icontab(k-1,n).gt.isurfnum) 
     *                   icontab(k-1,n)=icontab(k-1,n)-1
                      enddo
                   endif
                   icontab(1,n)= icontab(1,n)-1 
                   icontab(2,n)=0 
                   if(icontab(1,n).eq.1) icontab(2,n)=2 
                   if(icontab(1,n).eq.2) icontab(2,n)=1 
                   go to 100
               endif
            enddo
         endif
  100    continue
      enddo
 9999 return
      end
C
C
      subroutine conadd(isurfnum)
C
C#######################################################################
C
C     PURPOSE -
C
C        THIS ROUTINE SADDS-A CONSTRAINT ENTRY TO ICONTAB
C
C     INPUT ARGUMENTS -
C
C        isurfnum - surface number of this surface constraint
C
C     OUTPUT ARGUMENTS -
C
C        NONE
C
C
C#######################################################################
C
      implicit none
C
C#######################################################################
C
      include "chydro.h"
C
C#######################################################################
      pointer (ipicontab,icontab)
      integer icontab(50,1000000)
      character*32 cmo
      character*132 ibuff
      integer icscode,iconlen,maxnb,i,j,ninc,isurfnum,n,nconbnd
C
C
C#######################################################################
C
C
C   CHECK TO SEE IF ICONTAB EXISTS FOR THIS CMO
C
      call cmo_get_name(cmo,icscode)
      call cmo_get_intinfo('nconbnd',cmo,nconbnd,iconlen,j,icscode)
      call mmfindbk('icontab',cmo,ipicontab,iconlen,icscode)
C
C   IF NOT THERE CREATE
C   ADD NCON50 VARIABLE TO CMO - LENGTH OF BLOCK
C   ADD ICONTAB TO CMO
C
      if(icscode.ne.0) then
         ibuff='cmo/addatt//ncon50/INT/' //
     *         'scalar/scalar/constant/permanent/l/0' //
     *         ' ; finish'
         call dotaskx3d(ibuff,icscode)
         call cmo_set_info('ncon50',cmo,2500,1,1,icscode)
         ibuff='cmo/addatt//nconbnd/INT/' //
     *         'scalar/scalar/constant/permanent/l/0' //
     *         ' ; finish'
         call dotaskx3d(ibuff,icscode)
         call cmo_set_info('nconbnd',cmo,1,1,1,icscode)
         ibuff='cmo/addatt//icontab/VINT/' //
     *         'scalar/ncon50/constant/permanent/l/0.0' //
     *         ' ; finish'
         call dotaskx3d(ibuff,icscode)
         nconbnd=0
         call mmfindbk('icontab',cmo,ipicontab,iconlen,icscode)
         maxnb=0
         do i=1,nconsurf+5
            do j=1,50
               icontab(j,i) = 0
            enddo
         enddo
      else
C
C   COUNT ENTRIES AND INCREASE SPACE IF NEEDED
C
         n=iconlen/50
         maxnb=0
         do i=1,n
            if(icontab(2,i).eq.2.and.icontab(3,i).gt.maxnb)
     *              maxnb=icontab(3,i)
         enddo
         if(nconbnd+1.gt.n) then
            call cmo_set_info('ncon50',cmo,(nconbnd+10)*50,1,1,icscode)
            ninc=(nconbnd+10-n)*50
            call mmincblk('icontab',cmo,ipicontab,ninc,icscode)
            do i=n+1,nconbnd+10
               do j=1,50
                  icontab(j,i)=0
               enddo
            enddo
         endif
      endif
C
C  ADD NEW SURFACES TO ICONTAB
C
         nconbnd=nconbnd+1
         icontab(1,nconbnd)=1
         icontab(2,nconbnd)=2
         icontab(3,nconbnd)=isurfnum
C
 9999 continue
      call cmo_set_info('nconbnd',cmo,nconbnd,1,1,icscode)
      return
      end
