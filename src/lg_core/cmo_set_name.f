      subroutine cmo_set_name(cmo_name,ierror_return)
C
C
C#######################################################################
C
C     PURPOSE -
C
C        This routine sets the Current Mesh Object to Name.
C
C     INPUT ARGUMENTS -
C
C        cmo_name - (character) Name of the Mesh Object.
C
C     OUTPUT ARGUMENTS -
C
C        ierror_return - Error Return Code (==0 ==> OK, <>0 ==> Error)
C
C     CHANGE HISTORY -
C
C        $Log: cmo_set_name.f,v $
C        Revision 2.00  2007/11/05 19:45:50  spchu
C        Import to CVS
C
CPVCS    
CPVCS       Rev 1.0   Wed Jan 19 17:29:26 2000   dcg
CPVCS    Initial revision.
CPVCS    
CPVCS       Rev 1.12   Mon Apr 14 16:41:56 1997   pvcs
CPVCS    No change.
CPVCS    
CPVCS       Rev 1.11   03/15/95 15:23:56   ejl
CPVCS    Finished installing the defaults.
CPVCS    
CPVCS       Rev 1.10   02/16/95 09:57:14   ejl
CPVCS    Fixed bugs, fixed hole in the Create command.
CPVCS    Added commands MODATT, LENGTH, MEMORY, & COMPRESS.
CPVCS    
CPVCS       Rev 1.9   02/10/95 14:09:00   ejl
CPVCS    Fix bugs left from last update.
CPVCS    
CPVCS       Rev 1.7   01/30/95 06:22:26   het
CPVCS    Fix several cmo errors
CPVCS    
CPVCS       Rev 1.6   01/24/95 08:52:38   het
CPVCS    Add error checking to the cmo routines.
CPVCS    
CPVCS    
CPVCS       Rev 1.5   01/04/95 22:01:48   llt
CPVCS    unicos changes (made by het)
CPVCS    
CPVCS       Rev 1.4   12/19/94 08:29:28   het
CPVCS    Add the create cmo call if the cmo doesn't already exist.
CPVCS    
CPVCS
CPVCS       Rev 1.3   12/11/94 17:51:52   het
CPVCS    Fixed error related to cmo_create.
CPVCS
CPVCS
CPVCS       Rev 1.2   12/09/94 22:50:56   het
CPVCS    Made changes to support the new cmo_ routines.
CPVCS
CPVCS
CPVCS       Rev 1.1   12/02/94 07:56:54   het
CPVCS    Changed the cmo.h file which caused a conflict
CPVCS        in names for the "cmo" character name.
CPVCS
CPVCS
CPVCS       Rev 1.0   11/14/94 12:04:48   het
CPVCS    Original Version
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

      integer icmo_index
C
C#######################################################################
C
C
C
C.... Get the Index of the Mesh Object.
C
      call cmo_get_index(cmo_name,icmo_index,ierror_return)
C
      if(icmo_index.le.0) then
C
C....    Must create a new Mesh Object.
C
         call cmo_create(cmo_name,ierror_return)
C
      else
C
C....    Mesh Object exists, make it the Current Mesh Object
C
         call cmo_select(cmo_name,ierror_return)
C
      endif
C
      return
      end
