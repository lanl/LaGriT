      subroutine cmo_release(cmo_name,ierror_return)
C
C
C#######################################################################
C
C      PURPOSE -
C
C         This Routine Releases a Mesh Object.
C
C      INPUT ARGUMENTS -
C
C         cmo_name - (character) Mesh Object Name to be Released.
C
C      OUTPUT ARGUMENTS -
C
C         ierror_return - Error Return Code (==0 ==> Ok, <>0 ==> Error)
C
C      CHANGE HISTORY -
C
C         $Log: cmo_release.f,v $
C         Revision 2.00  2007/11/05 19:45:49  spchu
C         Import to CVS
C
CPVCS    
CPVCS       Rev 1.0   21 Jan 2000 17:03:16   dcg
CPVCS     
CPVCS    
CPVCS       Rev 1.15   Fri Jan 22 17:05:30 1999   dcg
CPVCS    replace len1 with len
CPVCS
CPVCS       Rev 1.14   Mon Apr 14 16:41:48 1997   pvcs
CPVCS    No change.
CPVCS
CPVCS       Rev 1.13   Mon Nov 18 10:33:18 1996   dcg
CPVCS    remove character literals from argument lists
CPVCS
CPVCS       Rev 1.12   Wed Jun 19 10:29:28 1996   het
CPVCS    Use the LIFO stack to select a new current CMO after
CPVCS    releasing the current CMO.
CPVCS
CPVCS       Rev 1.11   09/11/95 14:44:40   het
CPVCS    Change to the storage block based CMO stuff.
CPVCS
CPVCS       Rev 1.10   09/01/95 12:09:08   dcg
CPVCS    release storage block sbcmoatt
CPVCS
CPVCS       Rev 1.9   08/28/95 12:06:28   dcg
CPVCS    release memory for cmo, sbcmoprm memory block
CPVCS
CPVCS       Rev 1.8   05/22/95 15:35:12   ejl
CPVCS    Added nfaces and nedges.
CPVCS
CPVCS       Rev 1.7   03/15/95 15:23:44   ejl
CPVCS    Finished installing the defaults.
CPVCS
CPVCS       Rev 1.6   02/16/95 09:57:02   ejl
CPVCS    Fixed bugs, fixed hole in the Create command.
CPVCS    Added commands MODATT, LENGTH, MEMORY, & COMPRESS.
CPVCS
CPVCS       Rev 1.5   02/10/95 14:08:42   ejl
CPVCS    Fix bugs left from last update.
CPVCS
CPVCS       Rev 1.3   01/24/95 08:52:50   het
CPVCS    Add error checking to the cmo routines.
CPVCS
CPVCS
CPVCS       Rev 1.2   01/23/95 12:38:16   het
CPVCS    Use the table driven attribute management changes.
CPVCS
CPVCS
CPVCS       Rev 1.1   01/04/95 22:01:40   llt
CPVCS    unicos changes (made by het)
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
      integer len,  icscode
      integer ierr, i, index
      character*32 partname
C
      character*132 logmess
C
C
C#######################################################################
C
      integer icharlnf
C
C#######################################################################
C
      len=icharlnf(cmo_name)
      ierror_return = 0
C
      if((cmo_name(1:len).eq.'-cmo-') .or.
     *   (cmo_name(1:len).eq.'-def-')) then
C
C....    Use the Current Mesh Object.
C
         call cmo_get_name(cmo_name,ierror_return)
C
      endif
C
C.... Check to see if the Mesh Object exists.
C     NOTE: This is not an error, just return
C
      call cmo_exist(cmo_name,icscode)
C
      if(icscode.ne.0) then
C
         ierror_return=0
         write(logmess,'(a,a)') 
     *   'Mesh Object release skipped, ' //
     *   'it does not exist: ',
     *     cmo_name(1:icharlnf(cmo_name))
         call writloga('default',0,logmess,0,ierr)
C
      else
C
         ierror_return=0
C
         write(logmess,'(a,a)') '     Released Mesh Object: ', 
     *     cmo_name(1:icharlnf(cmo_name))
           call writloga('default',0,logmess,0,ierr)
C
C....    Delete the entry from the Mesh Object data structures.
C
         partname='define_cmo_lg'
         call mmfindbk('cmo_names',partname,ipcmo_names,len,
     *                   icscode)
         call mmfindbk('cmo_natts',partname,ipcmo_natts,len,
     *                   icscode)
         call cmo_get_index(cmo_name,index,icscode)
         call mmrelprt(cmo_name,icscode)
         do i=index,number_of_mesh_objects
             cmo_names(i)=cmo_names(i+1)
             cmo_natts(i)=cmo_natts(i+1)
         enddo
         cmo_names(number_of_mesh_objects)=' '
         cmo_natts(number_of_mesh_objects)=0
         number_of_mesh_objects=number_of_mesh_objects-1    
C
C....    Reset the name and index of the Current Mesh Object.
C
         if(number_of_mesh_objects.gt.1) then
            current_cmo_index=2
         else
            current_cmo_index=0
C
         endif
C
      endif
C
      return
      end
