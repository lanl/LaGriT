      subroutine cmo_mm_error(called_from)
C
C
C#######################################################################
C
C      PURPOSE -
C
C         This Routine handles CMO memory Errors.
C
C      INPUT ARGUMENTS -
C
C         cmo_name - Name of Routine with Memory Management error.
C
C      OUTPUT ARGUMENTS -
C
C         NONE
C
C      CHANGE HISTORY -
C
C         $Log: cmo_mm_error.f,v $
C         Revision 2.00  2007/11/05 19:45:49  spchu
C         Import to CVS
C
CPVCS    
CPVCS       Rev 1.0   24 Jan 2000 13:24:42   dcg
CPVCS    Initial revision.
CPVCS    
CPVCS       Rev 1.5   Mon Apr 14 16:41:32 1997   pvcs
CPVCS    No change.
CPVCS    
CPVCS       Rev 1.4   03/15/95 15:23:14   ejl
CPVCS    Finished installing the defaults.
CPVCS    
CPVCS       Rev 1.3   02/16/95 10:40:38   ejl
CPVCS    Put CR after end statement.
CPVCS    
CPVCS       Rev 1.2   02/16/95 09:56:54   ejl
CPVCS    Fixed bugs, fixed hole in the Create command.
CPVCS    Added commands MODATT, LENGTH, MEMORY, & COMPRESS.
CPVCS    
CPVCS       Rev 1.1   02/10/95 14:08:12   ejl
CPVCS    Fix bugs left from last update.
C
C#######################################################################
C
      implicit none
C
C#######################################################################
C
      character*(*) called_from
C
C#######################################################################
C
C     LOCAL VARIABLE DEFINITION
C
      integer ierr
C
      character*132 logmess
C
C#######################################################################
C
C
C
      write(logmess,'(a,a)') 
     *   'Mesh Object Memory Management Error in ',called_from
      call writloga('default',0,logmess,0,ierr)
C
      return
      end
