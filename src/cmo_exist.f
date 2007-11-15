      subroutine cmo_exist(cmo_name,ierror_return)
C
C
C#######################################################################
C
C      PURPOSE -
C
C         This Routine Checks fof the Existence of a Mesh Object.
C
C      INPUT ARGUMENTS -
C
C         cmo_name - (character) Mesh Object Name.
C
C      OUTPUT ARGUMENTS -
C
C         ierror_return - Error Return Code (==0 ==> OK, <>0 ==> Error)
C
C      CHANGE HISTORY -
C
C         $Log: cmo_exist.f,v $
C         Revision 2.00  2007/11/05 19:45:48  spchu
C         Import to CVS
C
CPVCS    
CPVCS       Rev 1.0   14 Jan 2000 17:07:58   dcg
CPVCS    Initial revision.
CPVCS    
CPVCS       Rev 1.7   Mon Apr 14 16:41:06 1997   pvcs
CPVCS    No change.
CPVCS    
CPVCS       Rev 1.6   Mon Nov 18 10:29:00 1996   dcg
CPVCS    remove charater literals from argument lists
CPVCS
CPVCS       Rev 1.5   09/11/95 14:43:54   het
CPVCS    Change to the storage block based CMO stuff.
CPVCS
CPVCS       Rev 1.4   03/15/95 15:22:52   ejl
CPVCS    Finished installing the defaults.
CPVCS
CPVCS       Rev 1.3   02/16/95 09:56:08   ejl
CPVCS    Fixed bugs, fixed hole in the Create command.
CPVCS    Added commands MODATT, LENGTH, MEMORY, & COMPRESS.
CPVCS
CPVCS       Rev 1.2   02/10/95 14:07:16   ejl
CPVCS    Fix bugs left from last update.
CPVCS
CPVCS       Rev 1.0   01/17/95 16:27:58   pvcs
CPVCS    Original Version
CPVCS
CPVCS       Rev 1.0   12/09/94 22:49:36   het
CPVCS    Original version.
CPVCS
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
      character*(*) cmo_name
C
      integer ierror_return
C
C#######################################################################
C
C     LOCAL VARIABLE DEFINITION
C
      integer len,icscode,i
      character*32 partname
C
C#######################################################################
C
      partname='define_cmo_lg'
c
c.... Get the list of mesh_object name
c
      call mmfindbk('cmo_names',partname,ipcmo_names,len,icscode)
c
c.... Look for cmo in list
c
      do i=1,number_of_mesh_objects
        if(cmo_name.eq.cmo_names(i)) then
           ierror_return=0
           go to 9999
        endif
      enddo
C
C....    No Mesh Objects defined.
C
      ierror_return=-1
C
 9999 return
      end
