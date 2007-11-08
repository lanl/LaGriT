      subroutine cmo_modatt(imsgin,xmsgin,cmsgin,msgtype,nwds,
     *                      ierror_return)
C
C
C#######################################################################
C
C      PURPOSE -
C
C         This Routine Modifies Field Values in an existing Mesh Object.
C
C      INPUT ARGUMENTS -
C
C         imsgin()  - Integer array of command input tokens
C         xmsgin()  - Real array of command input tokens
C         cmsgin()  - Character array of command input tokens
C         msgtype() - Integer array of command input token types
C         nwds      - Number of command input tokens
C
C      OUTPUT ARGUMENTS -
C
C         ierror_return - Error Return Code (==0 ==> OK, <>0 ==> Error)
C
C      CHANGE HISTORY -
C
C         $Log: cmo_modatt.f,v $
C         Revision 2.00  2007/11/05 19:45:49  spchu
C         Import to CVS
C
CPVCS    
CPVCS       Rev 1.0   Thu Jan 20 14:51:04 2000   dcg
CPVCS    Initial revision.
CPVCS    
CPVCS       Rev 1.3   Mon Apr 14 16:41:34 1997   pvcs
CPVCS    No change.
CPVCS    
CPVCS       Rev 1.2   09/11/95 14:44:20   het
CPVCS    Change to the storage block based CMO stuff.
CPVCS    
CPVCS       Rev 1.1   03/15/95 15:23:20   ejl
CPVCS    Finished installing the defaults.
CPVCS
CPVCS       Rev 1.0   02/16/95 13:46:48   dcg
CPVCS     Original version
C
C#######################################################################
C
      implicit none
C
C#######################################################################
C
      include 'cmo_lg.h'
C
C#######################################################################
C
      integer nwds, imsgin(nwds), msgtype(nwds)
      REAL*8 xmsgin(nwds)
      character*32 cmsgin(nwds)
C
      integer ierror_return
C
C#######################################################################
C
C     LOCAL VARIABLE DEFINITION
C
      character*32 cmo_name
      integer ierr, len, icscode
C
      character*132 logmess
C
C#######################################################################
C
      integer icharlnf
C
C#######################################################################
C
C
C
      cmo_name = cmsgin(3)
C
      len=icharlnf(cmo_name)
C
      if(cmo_name(1:len).eq.'-default-') then
C
C....    Modify Attribute in the Default Mesh Object Table.
C
         call cmo_modatt_def(imsgin,xmsgin,cmsgin,msgtype,nwds,
     *                       ierror_return)
C
         if(ierror_return.eq.0) then
C
C....       Verify the Default Mesh Object.
C
            call cmo_verify_def(ierror_return)
C
            if(ierror_return.ne.0) then
               write(logmess,'(a,a,a)')
     *               'CMO_MODATT error: ',
     *               'Mesh Object is not consistent: ', cmo_name
               call writloga('default',0,logmess,0,ierr)
            endif
C
         endif
C
      else
C
C....    Modify Attribute in the Mesh Object Table.
C
         if((cmo_name(1:len).eq.'-cmo-') .or.
     *      (cmo_name(1:len).eq.'-def-')) then
C
C....       Use the Current Mesh Object.
C
            call cmo_get_name(cmo_name,ierror_return)
C
            cmsgin(3)=cmo_name
C
         endif
C
C....    Check to see if the CMO exists.
C
         call cmo_exist(cmo_name,icscode)
C
         if(icscode.ne.0) then
C
            ierror_return=-1
C
            write(logmess,9010) cmo_name
            call writloga('default',0,logmess,0,ierr)
 9010       format('Mesh Object does not exist: ',a32)
C
         else
C
            call cmo_modatt_cmo(imsgin,xmsgin,cmsgin,msgtype,
     *                          nwds,ierror_return)
C
            if(ierror_return.eq.0) then
C
C....          Verify the Mesh Object.
C
               call cmo_verify_cmo(cmo_name,ierror_return)
C
               if(ierror_return.ne.0) then
                  write(logmess,'(a,a,a)')
     *                  'CMO_MODATT error: ',
     *                  'Mesh Object is not consistent: ', cmo_name
                  call writloga('default',0,logmess,0,ierr)
               endif
C
            endif
C
         endif
C
      endif
C
      return
      end
