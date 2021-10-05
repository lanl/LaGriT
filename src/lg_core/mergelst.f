      subroutine mergelst(imsgin,xmsgin,cmsgin,msgtype,nwds,
     &   ierror)
C
C
C #####################################################################
C
C     PURPOSE -
C
C        MERGE merges pairs of points.
C
C     INPUT ARGUMENTS -
C
C         imsgin()  - Integer array of command input tokens
C         xmsgin()  - Real array of command input tokens
C         cmsgin()  - Character array of command input tokens
C         msgtype() - Integer array of command input token types
C         nwds      - Number of command input tokens
C
C     OUTPUT ARGUMENTS -
C
C         ierror - Error Return Code (==0 ==> OK, <>0 ==> Error)
C
C     CHANGE HISTORY -
C
C        $Log: mergelst.f,v $
C        Revision 2.00  2007/11/05 19:46:01  spchu
C        Import to CVS
C
CPVCS    
CPVCS       Rev 1.0   Wed Jun 03 08:39:14 1998   dcg
CPVCS    Initial revision.
CPVCS    
CPVCS       Rev 1.0   Sat May 23 23:50:00 1998   kuprat
CPVCS    Initial revision.
c
c   FORMAT:
c
c   MERGE / isurv1, iremov1 / isurv2, iremov2 / .... / isurvn, iremovn
c
C #####################################################################
 
      implicit none
C
      integer lenptr
      parameter (lenptr=1000000)
 
      integer nwds, imsgin(nwds), msgtype(nwds)
      real*8 xmsgin(nwds)
      character*(*) cmsgin(nwds)
C
      pointer (ipimerge,imerge)
      integer imerge(2,lenptr)

      character*32 isubname,cmo
      character*132 logmess
      integer length,ierror,ierrw,icscode,nmerge,i,ki,kj,ierr,
     &   nsdgeom,ilen,itype,nsdtopo,nen,nef
C
      isubname = 'merge'
      ierror=0
C
C  Check that user has specified a valid mesh object.
c 
      call cmo_get_name(cmo,ierror)
      if(ierror.ne.0) then
         write(logmess,'(a)')
     *      'MERGE: ',cmo,' not a valid mesh object'
         call writloga('default',0,logmess,0,ierrw)
         goto 9999
      endif
c 
c.... Strip off points to be merged from argument list
c   
      if (nwds.lt.3) then
         write(logmess,'(a)')
     &      'MERGE:  No pairs to merge'
         call writloga('default',0,logmess,0,ierrw)
         goto 9999
      endif
c      
      length=nwds
      call mmgetblk("imerge",isubname,ipimerge,length,1,icscode)
C
      nmerge=0
      do i=2,nwds,2
         nmerge=nmerge+1
         ki=imsgin(i)
         kj=imsgin(i+1)
         imerge(1,nmerge)=ki
         imerge(2,nmerge)=kj
      enddo
C
      if (nmerge*2.ne.nwds-1) then
         write(logmess,'(a)')
     &      'MERGE:  Warning---odd number of arguments'
         call writloga('default',0,logmess,0,ierrw)
         write(logmess,'(a)')
     &      'Last argument ignored.'
         call writloga('default',0,logmess,0,ierrw)
      endif
c 
c.... Call appropriate merge routine.
c
      call cmo_get_info('ndimensions_geom',cmo,nsdgeom,ilen,itype,ierr)
      call cmo_get_info('ndimensions_topo',cmo,nsdtopo,ilen,itype,ierr)
      call cmo_get_info('nodes_per_element',cmo,nen,ilen,itype,ierr)
      call cmo_get_info('faces_per_element',cmo,nef,ilen,itype,ierr)
c
      if ((nsdgeom.eq.3 .and. nsdtopo.eq.2 .and.
     &   nen.eq.3 .and. nef.eq.3) .or.
     &   (nsdgeom.eq.3 .and. nsdtopo.eq.3 .and.
     &   nen.eq.4 .and. nef.eq.4)) then
         call mergepts_simplex(imerge,nmerge,cmo,ierr)
      else
         write(logmess,'(a)')
     &      'MERGE:  Merging points not allowed on this type of mesh.'
         call writloga('default',0,logmess,0,ierrw)
      endif
 9999 continue
      call mmrelprt(isubname,icscode)
      return
      end

