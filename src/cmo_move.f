      subroutine cmo_move(cmo_name1,cmo_name2,ierror_return)
C
C
C#######################################################################
C
C      PURPOSE -
C
C         This Routine Renames a Mesh Object.
C
C      INPUT ARGUMENTS -
C
C         cmo_name1 - (character) Derived Mesh_Object Name.
C         cmo_name2 - (character) Master Mesh_Object Name.
C
C      OUTPUT ARGUMENTS -
C
C         ierror_return - Error Return Code (==0 ==> OK, <>0 ==> Error)
C
C      CHANGE HISTORY -
C
C         $Log: cmo_move.f,v $
C         Revision 2.00  2007/11/05 19:45:49  spchu
C         Import to CVS
C
CPVCS    
CPVCS       Rev 1.0   21 Jan 2000 17:03:14   dcg
CPVCS     
CPVCS    
CPVCS       Rev 1.8   Mon Apr 14 16:41:40 1997   pvcs
CPVCS    No change.
CPVCS    
CPVCS       Rev 1.7   Mon Nov 18 10:29:16 1996   dcg
CPVCS    remove charater literals from argument lists
CPVCS    
CPVCS       Rev 1.6   09/11/95 14:44:30   het
CPVCS    Change to the storage block based CMO stuff.
CPVCS    
CPVCS       Rev 1.5   05/23/95 13:41:36   ejl
CPVCS    Fixed error when deleteing cmo
CPVCS    
CPVCS       Rev 1.4   04/10/95 10:38:02   ejl
CPVCS    Fixed problem with reserved names.
CPVCS    
CPVCS    
CPVCS       Rev 1.3   03/15/95 15:23:34   ejl
CPVCS    Finished installing the defaults.
CPVCS
CPVCS       Rev 1.2   02/16/95 09:56:56   ejl
CPVCS    Fixed bugs, fixed hole in the Create command.
CPVCS    Added commands MODATT, LENGTH, MEMORY, & COMPRESS.
CPVCS
CPVCS       Rev 1.1   02/10/95 14:08:26   ejl
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
      character*(*) cmo_name1, cmo_name2
C
      integer ierror_return
C
C#######################################################################
C
C     LOCAL VARIABLE DEFINITION
C
      integer index1,index2,ierror,i,ierr, ier, len, len1, len2,icscode
C
      character*32 partname
      character*132 logmess
C
C#######################################################################
C
      integer icharlnf
C
C#######################################################################
C
      len1=icharlnf(cmo_name1)
C
      if((cmo_name1(1:len1).eq.'-cmo-') .or.
     *   (cmo_name1(1:len1).eq.'-def-')) then
C
C....    Use the Current Mesh Object as the Sink.
C
         call cmo_get_name(cmo_name1,ierror_return)
C
         len1=icharlnf(cmo_name1)
C
      endif
C
      len2=icharlnf(cmo_name2)
C
      if((cmo_name2(1:len2).eq.'-cmo-') .or.
     *   (cmo_name2(1:len2).eq.'-def-')) then
C
C....    Use the Current Mesh Object as the Master.
C
         call cmo_get_name(cmo_name2,ierror_return)
C
         len2=icharlnf(cmo_name2)
C
      endif
C
      len=max(len1, len2)
      if(cmo_name1(1:len) .eq. cmo_name2(1:len)) then
C
C....    The two Mesh Objects are identical.
C
         ierror_return=-1
C
         write(logmess,'(a,a,a)')
     *      'Mesh_Objects are identical: ', cmo_name1, cmo_name2
         call writloga('default',0,logmess,0,ierr)
C
      elseif((cmo_name1(1:len1).eq.'-cmo-') .or.
     *       (cmo_name1(1:len1).eq.'-def-') .or.
     *       (cmo_name1(1:len1).eq.'-all-') .or.
     *       (cmo_name1(1:len1).eq.'-default-') .or.
     *       (cmo_name1(1:len1).eq.'-notset-')) then
C
C....    Name is reserved.
C
         ierror_return=-2
         write(logmess,'(a,a)')
     *      '    ERROR: Mesh Object name is reserved: ',cmo_name1
         call writloga('default',0,logmess,0,ierr)
C
      else
C
C....    Search table for Mesh Object 2.
C
         call cmo_exist(cmo_name2,icscode)
C
         if(icscode.ne.0) then
C
            ierror_return=-3
C
            write(logmess,'(a,a)')
     *            'Master Mesh_Object does not exist: ',cmo_name2
            call writloga('default',0,logmess,0,ierr)
C
         else
C
C....       Does Mesh Object 1 exists?
C
            call cmo_exist(cmo_name1,icscode)
C
            if(icscode.eq.0) then
C....          Release Mesh Object 1 if it exists.
               call cmo_release(cmo_name1,ierror)
            endif
C
C....       Fix up names in Mesh Object data structures.
C
            partname='define_cmo_lg'
            call mmfindbk('cmo_names',partname,ipcmo_names,len,
     *                   icscode)
            call cmo_get_index(cmo_name2,index1,icscode)
            cmo_names(index1)=cmo_name1  
C
C....       Change the Memory Manager Partition.
C
            call mmnamprt(cmo_name1,cmo_name2,ier)
            if(ier.ne.0) call cmo_mm_error('cmo_move')
C
C....       Make this the Current Mesh Object.
C
            call cmo_select(cmo_name1,ierror_return)
C
         endif
C
      endif
C
      return
      end
