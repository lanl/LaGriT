      subroutine cmo_set_default(cmo_name,icmo_index,ierror_return)
C
C
C#######################################################################
C
C     PURPOSE -
C
C        This routine copies the parameters from the default
c        mesh object attributes to the new mesh object
C
C     INPUT ARGUMENTS -
C
C        cmo_name   - (character) Name of the Mesh Object.
C        icmo_index - (integer) Index of the Mesh Object.
C
C     OUTPUT ARGUMENTS -
C
C        ierror_return - Error Return Code (==0 ==> OK, <>0 ==> Error)
C
C     CHANGE HISTORY -
C
C        $Log:   /pvcs.config/t3d/src/cmo_set_default_nosb.f_a  $
CPVCS    
CPVCS       Rev 1.1   07 Feb 2000 16:44:54   dcg
CPVCS    
CPVCS       Rev 1.0   14 Jan 2000 17:08:02   dcg
CPVCS    Initial revision.
CPVCS    
CPVCS       Rev 1.8   Mon Apr 14 16:41:52 1997   pvcs
CPVCS    No change.
CPVCS    
CPVCS       Rev 1.7   Mon Nov 18 10:29:22 1996   dcg
CPVCS    remove charater literals from argument lists
CPVCS    
CPVCS       Rev 1.6   09/11/95 14:44:42   het
CPVCS    Change to the storage block based CMO stuff.
CPVCS    
CPVCS       Rev 1.5   08/30/95 21:07:40   het
CPVCS    Put cmo table data into the cmoatt storage block
CPVCS    
CPVCS       Rev 1.4   08/29/95 12:15:08   het
CPVCS    Add the cmoatt storage block for each CMO
CPVCS    
CPVCS       Rev 1.3   03/15/95 15:23:50   ejl
CPVCS    Finished installing the defaults.
CPVCS    
CPVCS       Rev 1.2   02/17/95 07:59:50   ejl
CPVCS    Added nfaces and nedges to default list.
CPVCS    
CPVCS    
CPVCS       Rev 1.1   02/10/95 14:08:54   ejl
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
C
      integer icmo_index
C
      integer ierror_return
C
C#######################################################################
C
C     LOCAL VARIABLE DEFINITION
C
      integer i, icscode, len, length, ierror
      character*32 partname
c
c  list of attribute parameters for new mesh object
c  for each of the names in defcmo_attparam_names
c
      pointer (ipnew_attlist,new_attlist)
      character*32 new_attlist(*)
c
c  list of values for the 'default' parameter for each attribute
c  for new mesh object - divided into integer,real and character types
c
      pointer(ipnew_attparam_idefault,new_attparam_idefault)
      integer new_attparam_idefault(*)
      pointer(ipnew_attparam_rdefault,new_attparam_rdefault)
      real*8 new_attparam_rdefault(*)
      pointer(ipnew_attparam_cdefault,new_attparam_cdefault)
      character*32 new_attparam_cdefault(*)
C
C
C#######################################################################
      partname='default_cmo_lg'
c
c.... Get mesh_object pointers
c
      call mmfindbk('cmo_names',partname,ipcmo_names,len,icscode)
      call mmfindbk('cmo_natts',partname,ipcmo_natts,len,icscode)
      call mmfindbk('cmo_attlist',partname,ipcmo_attlist,len,icscode)
C
c.... Get memory for new mesh object attribute definitions
c
      length=number_of_default_attributes*number_of_params_per_att
      call mmgetblk('cmo_attlist',cmo_name,ipnew_attlist,length,3,
     *  icscode)
c  
C.... Copy the default mesh object attribute parameters into the 
C     new mesh object
C
      length=number_of_params_per_att*number_of_default_attributes
      do i=1,length
        new_attlist(i)=cmo_attlist(i)
      enddo
c
c.... Now copy values of default into new attribute
c
      call mmfindbk ('cmo_attparam_cdefault',partname,
     *  ipcmo_attparam_cdefault,len,ierror)
      call mmfindbk ('cmo_attparam_idefault',partname,
     *  ipcmo_attparam_idefault,len,ierror)
      call mmfindbk ('cmo_attparam_rdefault',partname,
     *  ipcmo_attparam_rdefault,len,ierror)
      length=number_of_default_attributes
      call mmgetblk('cmo_attparam_idefault',cmo_name,
     *     ipnew_attparam_idefault,length,1,icscode)
      call mmgetblk('cmo_attparam_rdefault',cmo_name,
     *     ipnew_attparam_rdefault,length,2,icscode)
      call mmgetblk('cmo_attparam_cdefault',cmo_name,
     *     ipnew_attparam_cdefault,length,3,icscode)
      do i=1,number_of_default_attributes
         new_attparam_cdefault(i)=cmo_attparam_cdefault(i)
         new_attparam_idefault(i)=cmo_attparam_idefault(i)
         new_attparam_rdefault(i)=cmo_attparam_rdefault(i)
      enddo
c
      ierror_return=0
C
      return
      end
