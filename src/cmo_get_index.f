      subroutine cmo_get_index(cmo_name,icmo_index,ierror_return)
C
C
C#######################################################################
C
C     PURPOSE -
C
C        This routine gets the index of a Mesh Object.
C
C     INPUT ARGUMENTS -
C
C        cmo_name - (character) Name of the Mesh Object.
C
C     OUTPUT ARGUMENTS -
C
C        icmo_index    - (integer) Index of the Mesh Object.
C                        Note that index will return with -1 if not found
C        ierror_return - Error Return Code (==0 ==> OK, <>0 ==> Error).
C
C     CHANGE HISTORY -
C
C        $Log: cmo_get_index.f,v $
C        Revision 2.00  2007/11/05 19:45:48  spchu
C        Import to CVS
C
CPVCS    
CPVCS       Rev 1.0   Wed Jan 19 17:29:12 2000   dcg
CPVCS    Initial revision.
CPVCS    
CPVCS       Rev 1.7   Mon Apr 14 16:41:10 1997   pvcs
CPVCS    No change.
CPVCS    
CPVCS       Rev 1.6   Mon Nov 18 10:29:02 1996   dcg
CPVCS    remove charater literals from argument lists
CPVCS    
CPVCS       Rev 1.5   12/05/95 08:23:22   het
CPVCS    Make changes for UNICOS
CPVCS    
CPVCS       Rev 1.4   09/11/95 14:43:58   het
CPVCS    Change to the storage block based CMO stuff.
CPVCS    
CPVCS       Rev 1.3   03/15/95 15:22:56   ejl
CPVCS    Finished installing the defaults.
CPVCS    
CPVCS       Rev 1.2   02/16/95 09:56:12   ejl
CPVCS    Fixed bugs, fixed hole in the Create command.
CPVCS    Added commands MODATT, LENGTH, MEMORY, & COMPRESS.
CPVCS    
CPVCS       Rev 1.1   02/10/95 14:07:20   ejl
CPVCS    Fix bugs left from last update.
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
      integer icmo_index, ierror_return
C
      integer i,len,icscode
      character*32 partname
C
C
C#######################################################################
C
      icmo_index=-1
      partname='define_cmo_lg'
C
      if(number_of_mesh_objects.gt.1) then
C
C....    Find index of the Mesh Object.
c....    Get the list of mesh_object name
c
         call mmfindbk('cmo_names',partname,ipcmo_names,len,icscode)
         if (icscode.ne.0) then
            ierror_return=-1
            go to 9999
         endif
         do i=1,number_of_mesh_objects
            if(cmo_names(i).eq.cmo_name) then
               icmo_index=i
               ierror_return=0
               go to 9999
            endif
         enddo
C
      else
C
C....    No Mesh Objects defined. 
C
         ierror_return=-1
C
      endif
C
 9999 return
      end
