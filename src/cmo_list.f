      subroutine cmo_list(ierror_return)
C
C
C#######################################################################
C
C      PURPOSE -
C
C         This routine prints the list of Mesh Objects.
C
C      INPUT ARGUMENTS -
C
C         NONE
C
C      OUTPUT ARGUMENTS -
C
C         ierror_return - Error Return Code (==0 ==> OK, <>0 ==> Error)
C
C      CHANGE HISTORY -
C
C         $Log: cmo_list.f,v $
C         Revision 2.00  2007/11/05 19:45:49  spchu
C         Import to CVS
C
CPVCS    
CPVCS       Rev 1.1   27 Jul 2001 11:04:04   tam
CPVCS    changed -default- to cmo number 0 instead of 1
CPVCS    
CPVCS       Rev 1.0   Thu Jan 20 14:50:58 2000   dcg
CPVCS    Initial revision.
CPVCS    
CPVCS       Rev 1.7   Mon Apr 14 16:41:26 1997   pvcs
CPVCS    No change.
CPVCS    
CPVCS       Rev 1.6   Mon Nov 18 10:29:08 1996   dcg
CPVCS    remove charater literals from argument lists
CPVCS
CPVCS       Rev 1.5   09/11/95 14:44:14   het
CPVCS    Change to the storage block based CMO stuff.
CPVCS
CPVCS       Rev 1.4   03/15/95 15:23:12   ejl
CPVCS    Finished installing the defaults.
CPVCS
CPVCS       Rev 1.3   02/16/95 10:40:36   ejl
CPVCS    Put CR after end statement.
CPVCS
CPVCS       Rev 1.2   02/16/95 09:56:50   ejl
CPVCS    Fixed bugs, fixed hole in the Create command.
CPVCS    Added commands MODATT, LENGTH, MEMORY, & COMPRESS.
CPVCS
CPVCS       Rev 1.1   02/10/95 14:08:04   ejl
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
      integer ierror_return
C
C#######################################################################
C
C     LOCAL VARIABLE DEFINITION
C
      integer i, ierr,  len, inum
C
      character*32 cmo_name,partname
C
      character*132 logmess
C
C#######################################################################
C
      partname='define_cmo_lg'
      if(number_of_mesh_objects.gt.1) then
C
         ierror_return=0
C
         call cmo_get_name(cmo_name,ierr)
         write(logmess,'(a,a)')
     *      'The current-mesh-object(CMO) is: ',cmo_name
         call writloga('default',2,logmess,1,ierr)
         call mmfindbk('cmo_names',partname,ipcmo_names,len,ierr)
C
         inum = 1
         do i=1,number_of_mesh_objects
C
C....       NAME Field.
C
c           default cmo is number 0 cmo
            if(cmo_names(i)(1:9).eq.'-default-') then
              write(logmess,9000) 0,cmo_names(i)
              inum = inum-1
            else
              write(logmess,9000) inum,cmo_names(i)
            endif

            call writloga('default',0,logmess,0,ierr)
 9000       format(i3,4x,'Mesh Object name: ',a)
            inum = inum+1
C
         enddo
C
      else
C
         ierror_return=-1
C
         write(logmess,'(a)') 'No Mesh Objects defined.'
         call writloga('default',0,logmess,0,ierr)
C
      endif
C
      return
      end
