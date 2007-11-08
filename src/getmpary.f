*dk,getmpary
      subroutine getmpary(imsgin,xmsgin,cmsgin,msgtype,
     &   ipmpary,mpno,psetname,ierror)
C
C
C #####################################################################
C
C     PURPOSE -
C
C        GETMPARY returns the mass point array MPARY and length
C        MPNO for the current cmo.
C
C     INPUT ARGUMENTS -
C
C         imsgin()  - Integer array of command input tokens
C                        starting at point PSET info is expected.
C         xmsgin()  - Real array of command input tokens
C                        starting at point PSET info is expected.
C         cmsgin()  - Character array of command input tokens
C                        starting at point PSET info is expected.
C         msgtype() - Integer array of command input token types
C                        starting at point PSET info is expected.
C
C     OUTPUT ARGUMENTS -
C
C         ipmpary   - pointer to mass point array
C         mpno      - length of mass point array
C         ierror    - Error Return Code (==0 ==> OK, <>0 ==> Error)
C
C     CHANGE HISTORY -
C
C        $Log: getmpary.f,v $
C        Revision 2.00  2007/11/05 19:45:57  spchu
C        Import to CVS
C
CPVCS    
CPVCS       Rev 1.4   Thu Apr 06 09:05:12 2000   dcg
CPVCS    remove get_info_i calls
CPVCS
CPVCS       Rev 1.3   Mon Apr 12 00:29:24 1999   kuprat
CPVCS    We check if MSGTYPE<=0, and establish default '1,0,0' in
CPVCS    this case.
CPVCS
CPVCS       Rev 1.2   Tue Jan 19 15:04:24 1999   kuprat
CPVCS    We now return PSETNAME.
CPVCS
CPVCS       Rev 1.1   Mon Jan 11 16:42:42 1999   kuprat
CPVCS    We now just accept the command parameter arrays starting at
CPVCS    the point that PSET info is expected.
CPVCS
CPVCS       Rev 1.0   Fri Oct 30 15:20:50 1998   kuprat
CPVCS    Initial revision.
C
C #####################################################################
 
      implicit none
C
 
      integer lenptr
      parameter (lenptr=1000000)
 
      integer imsgin(3), msgtype(3)
      real*8 xmsgin(3)
      character*(*) cmsgin(3)
C
      integer ierror,ierrw
 
      character*132 logmess
      pointer (ipisetwd,isetwd)
      pointer (ipitp1, itp1)
      integer isetwd(lenptr)
      integer itp1(lenptr)
C
      pointer (ipmpary,mpary)
      integer mpary(lenptr)
C
      character*32 ich1,ich2,ich3,cmo,isubname,blkmpary,submpary
     &   ,psetname
      integer length,icmotype,icscode,mpno,ipt1,ipt2,ipt3,nnodes,
     &   inc,len_mpary,ipointi,ipointj
 
      isubname = 'getmpary'
C
      ierror=0
 
C
C  Check that user has specified a valid mesh object.
 
      call cmo_get_name(cmo,ierror)
      if(ierror.ne.0) then
         write(logmess,'(a)')
     *      'GETMPARY: ',cmo,' not a valid mesh object'
         call writloga('default',0,logmess,0,ierrw)
         goto 9999
      endif
 
c.... Get cmo mesh info.
 
      call cmo_get_info('nnodes',cmo,
     *   nnodes,length,icmotype,icscode)
      if (icscode .ne. 0) call x3d_error(isubname,'cmo_get_info')
      call cmo_get_info('isetwd',cmo,
     *   ipisetwd,length,icmotype,icscode)
      if (icscode .ne. 0) call x3d_error(isubname,'cmo_get_info')
      call cmo_get_info('itp1',cmo,
     *   ipitp1,length,icmotype,icscode)
      if (icscode .ne. 0) call x3d_error(isubname,'cmo_get_info')
 
c.... Obtain block and partition name.  WE ASSUME block already exists.
 
      call mmgetnam(ipmpary,blkmpary,submpary,icscode)
      if (icscode .ne. 0) call x3d_error(isubname,'mmgetnam')
 
c.... Get length of block and increment if necessary.
 
      call mmgetlen(ipmpary,len_mpary,icscode)
 
      if (len_mpary.lt.nnodes) then
         inc=nnodes-len_mpary+100
         len_mpary=len_mpary+inc
         call mmincblk(blkmpary,submpary,ipmpary,inc,icscode)
      endif
 
C.... set the point index boundaries
 
C
C
      ich1=' '
      ich2=' '
      ich3=' '
      mpno=0
      if(msgtype(1).le.1) then
         if (msgtype(1).eq.1) then
            ipt1=imsgin(1)
            ipt2=imsgin(2)
            ipt3=imsgin(3)
         else
            ipt1=1
            ipt2=0
            ipt3=0
         endif
C
         call cmo_get_info('ipointi',cmo,
     *                   ipointi,length,icmotype,icscode)
         if (icscode .ne. 0) call x3d_error(isubname,'cmo_get_info')
         call cmo_get_info('ipointj',cmo,
     *                   ipointj,length,icmotype,icscode)
         if (icscode .ne. 0) call x3d_error(isubname,'cmo_get_info')
C
         if(ipt1.eq.0) ipt1=ipointi
         if(ipt2.eq.0) ipt2=ipointj
         if(ipt3.eq.0) ipt3=1
         ich3='-def-'
         call pntlimn(ipt1,ipt2,ipt3,ipmpary,mpno,
     *                                nnodes,isetwd,itp1)
      else
         ich1=cmsgin(1)
         ich2=cmsgin(2)
         ich3=cmsgin(3)
         call pntlimc(ich1,ich2,ich3,ipmpary,mpno,
     *                                nnodes,isetwd,itp1)
      endif
 
      psetname=ich3
 
      write(logmess,'(a,i10)')
     *   'nodes in point set  = ',mpno
      call writloga('default',0,logmess,0,ierrw)
 
 9999 continue
 
      return
      end
 
