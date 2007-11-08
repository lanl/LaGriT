      subroutine cmo_select(cmo_name,ierror_return)
C
C
C#######################################################################
C
C      PURPOSE -
C
C         This Routine Selects A Mesh Object as the
C         Current Mesh Object.
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
C         $Log: cmo_select.f,v $
C         Revision 2.00  2007/11/05 19:45:49  spchu
C         Import to CVS
C
CPVCS    
CPVCS       Rev 1.1   Wed Jan 19 17:29:52 2000   dcg
CPVCS    
CPVCS       Rev 1.9   Fri Apr 02 09:42:52 1999   nnc
CPVCS    Null character no longer appended to current_mesh_object_name.
CPVCS    
CPVCS       Rev 1.8   Mon Apr 14 16:41:50 1997   pvcs
CPVCS    No change.
CPVCS    
CPVCS       Rev 1.7   09/14/95 16:38:16   dcg
CPVCS    remove dependencies on mesh_object.h data structures
CPVCS
CPVCS       Rev 1.6   08/30/95 21:07:28   het
CPVCS    Put cmo table data into the cmoatt storage block
CPVCS
CPVCS       Rev 1.5   03/15/95 15:23:46   ejl
CPVCS    Finished installing the defaults.
CPVCS
CPVCS       Rev 1.4   02/16/95 09:57:06   ejl
CPVCS    Fixed bugs, fixed hole in the Create command.
CPVCS    Added commands MODATT, LENGTH, MEMORY, & COMPRESS.
CPVCS
CPVCS       Rev 1.3   02/10/95 14:08:50   ejl
CPVCS    Fix bugs left from last update.
CPVCS
CPVCS       Rev 1.1   01/04/95 22:01:42   llt
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
      integer icmo_index, len, length
C
      integer ierr, ier
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
c
C
      if(number_of_mesh_objects.gt.1) then
C
C....    Search table for Mesh Object.
C
         call cmo_get_index(cmo_name,icmo_index,ierror_return)
C
         if(icmo_index.le.0) then
C
            ierror_return=-1
            write(logmess,'(a,a)')
     *         'Mesh Object does not exist: ',cmo_name
            call writloga('default',0,logmess,0,ierr)
C
         else
C
            ierror_return=0
C
C....       Set the Index of the Current Mesh Object.
C
            current_cmo_index= icmo_index
C
         endif
C
      else
C
C....    No Mesh Objects are defined.
C
         ierror_return=-1
         write(logmess,'(a,a)') 'No Mesh Object defined: ', cmo_name
         call writloga('default',0,logmess,0,ierr)
C
      endif
C
      return
      end
