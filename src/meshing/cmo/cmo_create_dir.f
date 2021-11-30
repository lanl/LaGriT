      subroutine cmo_create_dir(cmo_name,ierror_return)
C
C
C#######################################################################
C
C      PURPOSE -
C
C         This routine creates a directory entry for a new  Mesh Object.
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
C         $Log: cmo_create_dir.f,v $
C         Revision 2.00  2007/11/05 19:45:48  spchu
C         Import to CVS
C
CPVCS    
CPVCS       Rev 1.1   16 Mar 2000 15:17:16   dcg
CPVCS    fix calls to mmincblk for cmo_names and cmo_natts
CPVCS    
CPVCS       Rev 1.0   14 Jan 2000 17:07:54   dcg
CPVCS    Initial revision.
CPVCS
CPVCS       Rev 1.3   Mon Apr 14 16:40:58 1997   pvcs
CPVCS    No change.
CPVCS
CPVCS       Rev 1.2   Mon Nov 18 10:28:42 1996   dcg
CPVCS    remove charater literals from argument lists
CPVCS
CPVCS       Rev 1.1   09/21/95 12:23:40   dcg
CPVCS    replace character literals in calling sequences with variables
CPVCS
CPVCS       Rev 1.0   09/20/95 09:47:00   dcg
CPVCS    Initial revision.
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
      integer ierror_return
C
C#######################################################################
C
C     LOCAL VARIABLE DEFINITION
C
      character*32 isubname,partname
      character*132 logmess
C
      integer   iexist, len, ierr, icscode
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
      partname='define_cmo_lg'
c
c.... Get the list of mesh_object name
c
      call mmfindbk('cmo_names',partname,ipcmo_names,len,icscode)
      if (icscode.ne.0) then
         ierror_return=-1
         write(logmess,'(a)') 'Missing cmo_names block'
         go to 9999
      endif
      call mmfindbk('cmo_natts',partname,ipcmo_natts,len,icscode)
c
c.... See if there is space for another mesh object
c
      if (number_of_mesh_objects+1.gt.len) then
         call mmincblk('cmo_names',partname,ipcmo_names,20,icscode)
         call mmincblk('cmo_natts',partname,ipcmo_natts,20,icscode)
      endif
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
         goto 9999
C
      endif
C
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
         number_of_mesh_objects=number_of_mesh_objects+1
         cmo_names(number_of_mesh_objects)=cmo_name
         cmo_natts(number_of_mesh_objects)=number_of_default_attributes
C
C....    Define the Current Mesh Object name.
C
         current_cmo_index = number_of_mesh_objects
C
      endif
C
 9999 continue
C
      return
      end
