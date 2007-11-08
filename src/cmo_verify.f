      subroutine cmo_verify(cmo_name,ierror_return)
C
C
C#######################################################################
C
C     PURPOSE -
C
C        This routine verifies the Mesh Object Data.
C
C     INPUT ARGUMENTS -
C
C        cmo_name - (character) Name of Mesh Object.
C
C     OUTPUT ARGUMENTS -
C
C        ierror_return - Error Return Code (==0 ==> OK, <>0 ==> Error)
C
C     CHANGE HISTORY -
C
C        $Log: cmo_verify.f,v $
C        Revision 2.00  2007/11/05 19:45:50  spchu
C        Import to CVS
C
CPVCS    
CPVCS       Rev 1.0   24 Jan 2000 13:24:50   dcg
CPVCS    Initial revision.
CPVCS    
CPVCS       Rev 1.3   Mon Apr 14 16:42:04 1997   pvcs
CPVCS    No change.
CPVCS    
CPVCS       Rev 1.2   03/15/95 15:24:06   ejl
CPVCS    Finished installing the defaults.
CPVCS    
CPVCS       Rev 1.1   02/10/95 14:09:16   ejl
CPVCS    Fix bugs left from last update.
C
C#######################################################################
C
      implicit none
C
C#######################################################################
C
      character*(*) cmo_name
C
      integer ierror_return
C
C#######################################################################
C
C     LOCAL VARIABLE DEFINITION

      integer len
C
C#######################################################################
C
      integer icharlnf
C
C#######################################################################
C
C
C
      len=icharlnf(cmo_name)
C
      if(cmo_name(1:len).eq.'-default-') then
C
C....    Verify the default Mesh Object.
C
         call cmo_verify_def(ierror_return)
C
      else
C
C....    Verify the Mesh Objects.
C
         call cmo_verify_cmo(cmo_name,ierror_return)
C
      endif
C
      return
      end
