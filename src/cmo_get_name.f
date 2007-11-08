      subroutine cmo_get_name(cmo_name,ierror)
C
C
C#######################################################################
C
C     PURPOSE -
C
C        This routine gets the Current Mesh Object name.
C
C     INPUT ARGUMENTS -
C
C        NONE.
C
C     OUTPUT ARGUMENTS -
C
C        cmo_name - (character) Name of the Current mesh object.
C        ierror   - Error Return Code (==0 ==> OK, <>0 ==> Error).
C
C     CHANGE HISTORY -
C
C        $Log: cmo_get_name.f,v $
C        Revision 2.00  2007/11/05 19:45:49  spchu
C        Import to CVS
C
CPVCS    
CPVCS       Rev 1.0   Wed Jan 19 17:29:16 2000   dcg
CPVCS    Initial revision.
CPVCS    
CPVCS       Rev 1.11   Fri Apr 02 09:45:26 1999   nnc
CPVCS    Null character no longer appended to cmo_name.
CPVCS    
CPVCS       Rev 1.10   Mon Apr 14 16:41:16 1997   pvcs
CPVCS    No change.
CPVCS    
CPVCS       Rev 1.9   03/15/95 15:23:04   ejl
CPVCS    Finished installing the defaults.
CPVCS    
CPVCS       Rev 1.8   02/16/95 09:56:20   ejl
CPVCS    Fixed bugs, fixed hole in the Create command.
CPVCS    Added commands MODATT, LENGTH, MEMORY, & COMPRESS.
CPVCS    
CPVCS       Rev 1.7   02/10/95 14:07:32   ejl
CPVCS    Fix bugs left from last update.
CPVCS    
CPVCS       Rev 1.5   01/24/95 08:52:32   het
CPVCS    Add error checking to the cmo routines.
CPVCS    
CPVCS    
CPVCS       Rev 1.4   01/04/95 22:01:36   llt
CPVCS    unicos changes (made by het)
CPVCS    
CPVCS       Rev 1.3   12/19/94 08:29:26   het
CPVCS    Add the create cmo call if the cmo doesn't already exist.
CPVCS    
CPVCS
CPVCS       Rev 1.2   12/09/94 22:50:54   het
CPVCS    Made changes to support the new cmo_ routines.
CPVCS
CPVCS
CPVCS       Rev 1.1   12/02/94 07:56:50   het
CPVCS    Changed the cmo.h file which caused a conflict
CPVCS        in names for the "cmo" character name.
CPVCS
CPVCS
CPVCS       Rev 1.0   11/14/94 12:04:46   het
CPVCS    Original Version
C
C#######################################################################
C
      implicit none
      
      character*(*) cmo_name
      integer ierror,len,icscode
      character*32 partname

      include 'cmo_lg.h'
      
      ierror = 0
      partname='define_cmo_lg'

      
      if (current_cmo_index .le. 0) then
        cmo_name = '-notset-'
      else
c
c.... Get the list of mesh_object name
c
         call mmfindbk('cmo_names',partname,ipcmo_names,len,icscode)
         if (icscode.ne.0) then
            ierror=-1
            go to 9999
         endif
         cmo_name = cmo_names(current_cmo_index)
      endif
      
      if (cmo_name .eq. '-notset-') ierror = -1

 9999 return
      end
