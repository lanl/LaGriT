      subroutine rmmat(imsgin,xmsgin,cmsgin,msgtype,nwds,ics)
C
C#######################################################################
C
C      PURPOSE -
C
C      THIS ROUTINE IS USED TO REMOVE POINTS THAT ARE
C      OF A SPECIFIED MATERIAL TYPE.
C      ELEMENTS WITH THE SPECIFIED MATERIAL TYPES ARE FLAGGED
C      BY SETTING THE ELEMENT MATERIAL TYPE NEGATIVE.
C      AFTER USING RMMAT, RMPOINT/COMPRESS
C      WILL DELETE ELEMENTS WHOSE MATERIAL TYPE IS NEGATIVE
C      AND THE DUDDED NODES.
C
C          FORMAT: RMMAT/material number/[all|node|element]/[exclusive]
C
C                 default is:
C                 RMMAT/material number
C                     or
C                 RMMAT/material number/all
C                     removes nodes    with imt    = material number and
C                     removes elements with itetclr= material number
C                 Other options are:
C                 RMMAT/material number/node
C                     removes nodes    with imt    = material number and
C                 RMMAT/material number/element removes
C                     removes elements with itetclr= material number
C                 RMMAT/material number//exclusive
C                     or
C                 RMMAT/material number/all/exclusive
C                     removes everything except nodes    with imt    = material number and
C                     removes everything except elements with itetclr= material number
C
C      INPUT ARGUMENTS -
C
C         NONE
C
C
C      OUTPUT ARGUMENTS -
C
C         NONE
C
C
C      CHANGE HISTORY -
C
C         $Log: rmmat.f,v $
C         Revision 2.00  2007/11/09 20:04:01  spchu
C         Import to CVS
C
CPVCS    
CPVCS       Rev 1.0   07 Feb 2000 17:35:36   dcg
CPVCS    Initial revision.
CPVCS    
CPVCS       Rev 1.10   Mon Dec 13 14:23:28 1999   dcg
CPVCS    change format to allow for more nodes
CPVCS
CPVCS       Rev 1.9   Fri Mar 05 09:46:08 1999   dcg
CPVCS    change error flag return on mmrelprt call
CPVCS
CPVCS       Rev 1.8   Wed Dec 23 11:53:48 1998   gable
CPVCS    Made some modification to documentation header.
CPVCS
CPVCS       Rev 1.7   Wed Dec 23 10:18:54 1998   llt
CPVCS    Added option to remove all material numbers, except specified one
CPVCS
CPVCS       Rev 1.6   Thu May 14 11:55:44 1998   gable
CPVCS    Added option to remove based on node and/or element imt/itetclr
CPVCS
CPVCS       Rev 1.5   Mon Mar 09 12:24:42 1998   dcg
CPVCS    fix error in setting point type (itp1)
CPVCS
CPVCS       Rev 1.4   Mon Apr 14 16:59:54 1997   pvcs
CPVCS    No change.
CPVCS
CPVCS       Rev 1.3   Wed Dec 04 08:15:32 1996   het
CPVCS    Convert exposed interface points to interior or boundary points.
CPVCS
CPVCS       Rev 1.2   Fri Oct 18 12:35:32 1996   dcg
CPVCS    fix parent/child chain if longer than 2
CPVCS    do not delete parent point
CPVCS
CPVCS       Rev 1.1   Fri Jul 26 13:25:22 1996   dcg
CPVCS    fix parent child chain
CPVCS    mark elements for deletion
CPVCS
CPVCS       Rev 1.0   09/20/95 09:46:28   dcg
CPVCS    Initial revision.
CPVCS
CPVCS       Rev 1.38   09/15/95 13:15:30   dcg
CPVCS    temp changes look for 'Cdcg' to make code run
CPVCS    with new mesh_object.h -- these routines will
CPVCS    have to be changed anyway.
CPVCS
CPVCS       Rev 1.37   09/14/95 12:24:24   het
CPVCS    Correct errors
CPVCS
CPVCS       Rev 1.36   09/14/95 02:07:24   het
CPVCS    Fix ipcmoprm errors
CPVCS
CPVCS       Rev 1.35   09/12/95 15:50:22   dcg
CPVCS    remove bad mmgetnam call
CPVCS
CPVCS       Rev 1.34   09/12/95 14:50:46   dcg
CPVCS    IBM changes
CPVCS
CPVCS       Rev 1.33   09/12/95 06:55:34   het
CPVCS    Correct errors with sb_get_info
CPVCS
CPVCS       Rev 1.32   09/11/95 14:42:34   het
CPVCS    Change to the storage block based CMO stuff.
CPVCS
CPVCS       Rev 1.31   08/30/95 21:08:34   het
CPVCS    Put cmo table data into the cmoatt storage block
CPVCS
CPVCS       Rev 1.30   08/29/95 12:15:06   dcg
CPVCS    set length for names to 40 characters
CPVCS
CPVCS       Rev 1.29   08/23/95 06:59:20   het
CPVCS    Remove the CMO prefix from SB-ids
CPVCS
CPVCS       Rev 1.28   08/22/95 06:51:22   het
CPVCS    Split the storage block for CMO variables.
CPVCS
CPVCS       Rev 1.27   08/15/95 18:20:44   het
CPVCS    Cleanup code and correct errors
C
C
C#######################################################################
C
      implicit none
C
      include 'chydro.h'
C
C#######################################################################
C
      integer nwds, imsgin(nwds), msgtype(nwds)
      real*8 xmsgin(nwds)
      character*(*) cmsgin(nwds)
C
C
C#######################################################################
C
      character*132 logmess
C
      pointer ( ipimt1 , imt1 )
      pointer ( ipitp1 , itp1 )
      pointer ( ipisn1 , isn1 )
      pointer ( ipitetclr , itetclr )
      integer itetclr(1000000)
      integer imt1(1000000), itp1(1000000), isn1(1000000)
C
      pointer (ipibnd1, ibnd1)
      integer ibnd1(1000000)
C
C#######################################################################
C
      integer nelements,nnodes,imat,i1,i2,i3,i4,itetmrk,ndel,ics,
     *  itypc,ilenc,icscode,l,ll,itp,isq,it,length
      integer if_rm_node, if_rm_element
      character*32 isubname, cmo
      logical imat_save
C
C#######################################################################
C
      isubname='rmmat'
C
      imat_save = .false.
      if(msgtype(1).eq.1) then
         imat=imsgin(1)
      elseif(msgtype(1).eq.2) then
         imat=xmsgin(1)
      elseif(msgtype(1).eq.3) then
         call get_material_number(cmsgin(1),imat,ics)
      endif
      if(nwds .eq. 1)then
         if_rm_element = 1
         if_rm_node = 1
      elseif(((nwds .eq. 2) .or. (nwds .eq. 3)) .and.
     1        (msgtype(2) .eq. 3))then
         if (     cmsgin(2) .eq. '-def-'
     1       .or. cmsgin(2) .eq. '-all-'
     1       .or. cmsgin(2) .eq. 'all')then
            if_rm_element = 1
            if_rm_node = 1
         elseif(  cmsgin(2) .eq. 'node' )then
            if_rm_element = 0
            if_rm_node = 1
         elseif(  cmsgin(2) .eq. 'element' )then
            if_rm_element = 1
            if_rm_node = 0
         endif
         if ((nwds .eq. 3) .and. (cmsgin(3) .eq. 'exclusive')) then
           imat_save = .true.
         endif
      else
         write(logmess,6000)
         call writloga('default',0,logmess,0,ics)
      endif
C
C     ******************************************************************
C
C     GET MEMORY FOR LOCAL VARIABLES.
C
      call cmo_get_name(cmo,icscode)
C
      call cmo_get_info('nnodes',cmo,nnodes,ilenc,itypc,icscode)
      call cmo_get_info('nelements',cmo,nelements,ilenc,itypc,icscode)
      call cmo_get_info('imt1',cmo,ipimt1,ilenc,itypc,icscode)
      call cmo_get_info('itp1',cmo,ipitp1,ilenc,itypc,icscode)
      call cmo_get_info('isn1',cmo,ipisn1,ilenc,itypc,icscode)
      call cmo_get_info('itetclr',cmo,ipitetclr,ilenc,itypc,icscode)
C
C     ******************************************************************
C
      if(nnodes .eq. 0)then
         write(logmess,3005)
 3005    format('WARNING: RMMAT Number of nodes = 0 ')
         call writloga('default',0,logmess,0,ics)
         write(logmess,3006)
 3006    format('WARNING: RMMAT RETURN no action ')
         call writloga('default',0,logmess,0,ics)
         go to 9998
       endif
C
C     ******************************************************************
C     SET UP AN ARRARY THAT IDENTIFIES THE ALL BOUNDARY NODES.
C          IREAL1 = 1  -> Boundary Node.
C          IREAL1 = 0  -> Not a boundary node.
C
      length=nnodes
      call mmgetblk('ibnd1',isubname,ipibnd1,length,2,icscode)
      if(icscode.ne.0) call x3d_error(isubname,'mmgetblk')
      call unpacktp('boundary','set',length,ipitp1,ipibnd1,icscode)
      if(icscode.ne.0) call x3d_error(isubname,'unpacktp')
C
      ndel=0
      if(if_rm_node .eq. 1)then
      do 100 i1=1,nnodes
         if ((imat_save       .and. (imt1(i1) .ne. imat)) .or.
     .       (.not. imat_save .and. (imt1(i1) .eq. imat))) then
            isq=0
            ndel=ndel+1
            itp=ifitpdud
            if(isn1(i1).ne.0) then
C  parent or child point
C  find length of parent/child loop
C
               l=0
               i2=isn1(i1)
               do while (i1.ne.i2.and.l.lt.100)
                  l=l+1
                  i2=isn1(i2)
               enddo
C  if more than 2 children
C  find child point to remove if i1 is parent -reset i1
C  remove i1 from chain
               if (l.gt.2) then
                  i4=i1
                  if(itp1(i1).eq.ifitpcup) then
                     i2=isn1(i4)
                     i3=i4
                     do ll =1,l
                      if ((imat_save      .and. (imt1(i2).ne.imat)) .or.
     .                    (.not.imat_save .and. (imt1(i2).eq.imat)))then
                           i3=i2
                        else
                           i2=isn1(i2)
                        endif
                     enddo
                     if(i3.eq.i4) go to 100
                     i4=i3
                     itp1(i3)=itp
                  endif
                  i2=isn1(i1)
                  do ll=1,l+1
                     if(isn1(i2).eq.i4)  isn1(i2)=isn1(i4)
                     i2=isn1(i2)
                  enddo
               else
C  if only two children remove parent and this child
C  remove isn1 field from other child
                  i2=i1
                  do ll=1,3
                     i3=isn1(i2)
                     if(itp1(i2).eq.ifitpcup) then
                        itp1(i2)=ifitpdud
                        isn1(i2)=0
                        ndel=ndel+1
                     elseif(i1.ne.i2) then
                        if(ibnd1(i2).eq.0) then
                           itp1(i2)=ifitpint
                        else
                           itp1(i2)=ifitprfl
                        endif
                        isn1(i2)=0
                     endif
                     i2=i3
                  enddo
                  isn1(i1)=isq
                  itp1(i1)=itp
               endif
             else
             itp1(i1)=ifitpdud
            endif
          endif
 100  continue
      endif
 
C
C   mark all elements of this material type for deletion by
C   rmpoint/compress
C
      itetmrk=0
      if(if_rm_element .eq. 1)then
      if (nelements.le.0) go to 9999
      do it=1,nelements
         if ((imat_save       .and. (itetclr(it) .ne. imat)) .or.
     .       (.not. imat_save .and. (itetclr(it) .eq. imat))) then
            itetclr(it)=-itetclr(it)
            itetmrk=itetmrk+1
         endif
      enddo
      endif
C
      ics=0
C
C     ******************************************************************
C
C     RELEASE MEMORY FOR LOCAL VARIABLES IN THE PARTITION
C
 9998 call mmrelprt('rmmat', icscode)
      write(logmess,6000) ndel,itetmrk
 6000 format(' RMMAT DUDDED ',i10,' POINTS: FLAGGED ',i10,' ELEMENTS')
      call writloga('default',0,logmess,0,icscode)
C
      goto 9999
 9999 continue
      return
      end
