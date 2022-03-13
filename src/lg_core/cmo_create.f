      subroutine cmo_create(cmo_name,ierror_return)
C
C
C#######################################################################
C
C      PURPOSE -
C
C         This Routine Creates a Mesh Object.
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
C         $Log: cmo_create.f,v $
C         Revision 2.00  2007/11/05 19:45:48  spchu
C         Import to CVS
C
CPVCS    
CPVCS       Rev 1.1   Thu Jan 20 16:08:08 2000   dcg
CPVCS    
CPVCS       Rev 1.24   Fri Nov 05 13:26:56 1999   dcg
CPVCS    remove dictionary dependencies
CPVCS    
CPVCS       Rev 1.23   Mon Apr 14 16:40:08 1997   pvcs
CPVCS    No change.
CPVCS    
CPVCS       Rev 1.22   Mon Nov 18 13:57:52 1996   dcg
CPVCS    define sbname='sbcmoprm' correctly
CPVCS    
CPVCS       Rev 1.21   Mon Nov 18 10:38:20 1996   dcg
CPVCS    remove character literals from argument lists
CPVCS    
CPVCS       Rev 1.20   09/11/95 14:43:24   het
CPVCS    Change to the storage block based CMO stuff.
CPVCS    
CPVCS       Rev 1.19   08/30/95 21:07:30   het
CPVCS    Put cmo table data into the cmoatt storage block
CPVCS
CPVCS       Rev 1.18   08/29/95 12:49:10   dcg
CPVCS    set length for names to 40 characters
CPVCS
CPVCS       Rev 1.17   08/29/95 12:16:04   het
CPVCS    Add the cmoatt storage block for each CMO
CPVCS
CPVCS       Rev 1.16   08/22/95 06:51:52   het
CPVCS    Split the storage block for CMO variables.
CPVCS
CPVCS       Rev 1.15   05/23/95 06:50:26   het
CPVCS    Change dictionary so that they are CMO specific
CPVCS
CPVCS       Rev 1.14   03/15/95 15:22:34   ejl
CPVCS    Finished installing the defaults.
CPVCS
CPVCS       Rev 1.13   02/16/95 09:55:52   ejl
CPVCS    Fixed bugs, fixed hole in the Create command.
CPVCS    Added commands MODATT, LENGTH, MEMORY, & COMPRESS.
CPVCS
CPVCS       Rev 1.12   02/10/95 14:06:44   ejl
CPVCS    Fix bugs left from last update.
CPVCS
CPVCS       Rev 1.10   01/30/95 18:17:44   het
CPVCS    Add the cmo_select call to refresh cmo.h pointers
CPVCS
CPVCS       Rev 1.9   01/30/95 06:22:08   het
CPVCS    Fix several cmo errors
CPVCS
CPVCS       Rev 1.8   01/24/95 08:52:26   het
CPVCS    Add error checking to the cmo routines.
CPVCS
CPVCS
CPVCS       Rev 1.7   01/23/95 17:02:32   het
CPVCS    Correct some character problems with the table driven
CPVCS       cmo attributes.
CPVCS
CPVCS
CPVCS       Rev 1.6   01/23/95 12:38:06   het
CPVCS    Use the table driven attribute management changes.
CPVCS
CPVCS
CPVCS       Rev 1.5   01/04/95 22:01:32   llt
CPVCS    unicos changes (made by het)
CPVCS
CPVCS       Rev 1.4   01/02/95 12:36:38   het
CPVCS
CPVCS
CPVCS       Rev 1.3   01/02/95 12:26:36   het
CPVCS
CPVCS
CPVCS       Rev 1.2   12/21/94 09:50:02   het
CPVCS    Add a call to 'cmo_select()' to select the just created
CPVCS       cmo as the current_cmo.
CPVCS
CPVCS       Rev 1.1   12/11/94 17:51:48   het
CPVCS    Fixed error related to cmo_create.
CPVCS
CPVCS
CPVCS       Rev 1.0   12/09/94 22:49:32   het
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
      integer len
C
      integer ierr, icmonum
C
      character*32 isubname
C
      character*132 logmess
C
      integer icscode, iexist
C
C#######################################################################
C
      integer icharlnf
C
C#######################################################################
C
C
C.... Define the routine name.
C
      isubname='cmo_create'
C
C.... Check name against reserved names.
C
      len=icharlnf(cmo_name)
C
      if((cmo_name(1:len).eq.'-cmo-') .or.
     *   (cmo_name(1:len).eq.'-def-') .or.
     *   (cmo_name(1:len).eq.'-all-') .or.
     *   (cmo_name(1:len).eq.'-default-') .or.
     *   (cmo_name(1:len).eq.'-notset-')) then
C
C....    Name is reserved.
C
         ierror_return=-1
         write(logmess,'(a,a)')
     *      '    ERROR: Mesh Object name is reserved: ',cmo_name
         call writloga('default',0,logmess,0,ierr)
C
      endif
C
C
C.... Search table for Mesh Object.
C
      call cmo_exist(cmo_name,iexist)
C
      if(iexist.eq.0) then
C
C....    Mesh Object already exists.
C
         ierror_return=-1
         write(logmess,'(a,a)')
     *      'Mesh Object already exists: ',cmo_name
         call writloga('default',0,logmess,0,ierr)
C
      else
C
C....    Build a directory entry for the new Mesh Object.
C
         call cmo_create_dir(cmo_name,ierror_return)
C
C
C....    Set the default attributes.
C
         call cmo_set_default(cmo_name,icmonum,ierror_return)
C
C
C....    Set up the new Mesh Object.
C
         call cmo_allocate(cmo_name,ierror_return)
c
C....    Make this the Current Mesh Object.
C
         call cmo_select(cmo_name,ierror_return)
C
      endif
C
      return
      end

