      subroutine cmo_set_info(ioption,cmo_name,data,len_data,type_data,
     *                        ierror_return)
C
C
C#######################################################################
C
C     PURPOSE -
C
C        This routine sets Mesh Object information.
C
C     INPUT ARGUMENTS -
C
C        ioption    - (character) The option to be performed.
C        cmo_name   - (character) Name of the Mesh Object.
C        data       - The data to be set.
C        len_data   - The length of the data to be set.
C        type_data  - The type of the data to be set (I=1,R=2,C=3).
C
C     OUTPUT ARGUMENTS -
C
C        ierror_return - Error Return Code (==0 ==> OK, <>0 ==> Error)
C
C     CHANGE HISTORY -
C
C        $Log: cmo_set_info.f,v $
C        Revision 2.00  2007/11/05 19:45:50  spchu
C        Import to CVS
C
CPVCS    
CPVCS       Rev 1.0   Wed Jan 19 17:29:24 2000   dcg
CPVCS    Initial revision.
CPVCS    
CPVCS       Rev 1.16   Mon Apr 14 16:41:54 1997   pvcs
CPVCS    No change.
CPVCS    
CPVCS       Rev 1.15   Mon Nov 18 11:01:06 1996   dcg
CPVCS    remove character literals from argument lists
CPVCS
CPVCS       Rev 1.14   09/14/95 16:38:00   dcg
CPVCS    remove dependencies on mesh_object.h data structures
CPVCS
CPVCS       Rev 1.13   09/11/95 14:44:46   het
CPVCS    Change to the storage block based CMO stuff.
CPVCS
CPVCS       Rev 1.12   08/30/95 21:08:50   het
CPVCS    Put cmo table data into the cmoatt storage block
CPVCS
CPVCS       Rev 1.11   03/15/95 15:23:54   ejl
CPVCS    Finished installing the defaults.
CPVCS
CPVCS       Rev 1.10   02/16/95 09:57:10   ejl
CPVCS    Fixed bugs, fixed hole in the Create command.
CPVCS    Added commands MODATT, LENGTH, MEMORY, & COMPRESS.
CPVCS
CPVCS       Rev 1.9   02/10/95 14:08:56   ejl
CPVCS    Fix bugs left from last update.
CPVCS
CPVCS       Rev 1.7   02/03/95 10:35:24   het
CPVCS    Set the default value of icmoflag
CPVCS
CPVCS       Rev 1.6   01/30/95 06:22:22   het
CPVCS    Fix several cmo errors
CPVCS
CPVCS       Rev 1.5   01/24/95 08:52:56   het
CPVCS    Add error checking to the cmo routines.
CPVCS
CPVCS
CPVCS       Rev 1.4   01/04/95 22:01:46   llt
CPVCS    unicos changes (made by het)
CPVCS
CPVCS       Rev 1.3   01/02/95 15:34:18   het
CPVCS    Corrected a problem with cmo_get_info() calles.
CPVCS
CPVCS
CPVCS       Rev 1.2   12/09/94 22:51:02   het
CPVCS    Made changes to support the new cmo_ routines.
CPVCS
CPVCS
CPVCS       Rev 1.1   12/01/94 18:58:46   het
CPVCS    Added a data variable type to the call.
CPVCS
CPVCS
CPVCS       Rev 1.0   11/14/94 12:04:52   het
CPVCS    Original Version
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
      character*(*) ioption, cmo_name
C
      integer data, len_data, type_data
C
      integer ierror_return
C
C#######################################################################
C
C     LOCAL VARIABLE DEFINITION
C
      integer len, i, icmo_index, natts,length,ierr,icscode
C
      character*132 logmess
      character*32 partname
C
C#######################################################################
C
      integer icharlnf
C
C#######################################################################
C
C
      len=icharlnf(cmo_name)
C
      if((cmo_name(1:len).eq.'-cmo-') .or.
     *   (cmo_name(1:len).eq.'-default-') .or.
     *   (cmo_name(1:len).eq.'-def-')) then
C
C....    Use the Current Mesh Object.
C
         call cmo_get_name(cmo_name,ierror_return)
C
      endif
C
C.... Search table for Mesh Object.
C
      call cmo_get_index(cmo_name,icmo_index,ierror_return)
C
      if(icmo_index.le.0) then
C
C....    Mesh Object does not exist.
C
         ierror_return=-1
         write(logmess,9010) cmo_name
         call writloga('default',0,logmess,0,ierr)
 9010    format('CMO_SET_INFO error:  Mesh Object does not exist: ',a32)
C
      else
C
         partname='define_cmo_lg'
         call mmfindbk('cmo_names',partname,ipcmo_names,len,
     *                   icscode)
         call mmfindbk('cmo_natts',partname,ipcmo_natts,len,
     *                   icscode)
         partname='default_cmo_lg'
         call mmfindbk('cmo_attlist',cmo_name,ipcmo_attlist,
     *                   len,icscode)
         call cmo_get_index(cmo_name,icmo_index,ierror_return)
         if(ierror_return.ne.0) go to 9998
         natts=cmo_natts(icmo_index)
         do i=1,natts
            if(cmo_attlist(number_of_params_per_att*(i-1)+1)
     *             .eq.ioption) then
               if(cmo_attlist(number_of_params_per_att*(i-1)+2)
     *               .eq.'INT') then
                   call mmfindbk('cmo_attparam_idefault'
     *                ,cmo_name,ipcmo_attparam_idefault,
     *                length,icscode)
                   cmo_attparam_idefault(i)=data
                   go to 9999
               elseif(cmo_attlist(number_of_params_per_att*
     *               (i-1)+2).eq.'REAL') then  
                   call mmfindbk('cmo_attparam_rdefault'
     *                ,cmo_name,ipcmo_attparam_rdefault,
     *                length,icscode)  
                   cmo_attparam_rdefault(i) = data
                   go to 9999
               else 
                   ierror_return=-1 
                   go to 9998
               endif
            endif
         enddo
9998     if(ierror_return.ne.0) then
            call x3d_error('cmo_set_info','attribute not found')
         endif
C
      endif
C
9999  return
      end
 
