      subroutine cmo_verify_cmo(cmo_name,ierror_return)
C
C
C#######################################################################
C
C     PURPOSE -
C
C        This routine verifies the Mesh Object Data.
C
C     INPUT ARGUMENTS -
C
C        cmo_name - (character) Name of the Mesh Object or '-all-'.
C
C     OUTPUT ARGUMENTS -
C
C        ierror_return - Error Return Code (==0 ==> OK, <>0 ==> Error)
C
C     CHANGE HISTORY -
C
C        $Log:   /pvcs.config/t3d/src/cmo_verify_cmo_nosb.f_a  $
CPVCS    
CPVCS       Rev 1.1   Thu Feb 03 13:12:22 2000   dcg
CPVCS    
CPVCS       Rev 1.8   Mon Apr 14 16:42:06 1997   pvcs
CPVCS    No change.
CPVCS    
CPVCS       Rev 1.7   Mon Nov 18 10:29:26 1996   dcg
CPVCS    remove charater literals from argument lists
CPVCS    
CPVCS       Rev 1.6   09/15/95 10:32:44   dcg
CPVCS    use new storage block structure to get cmo info
CPVCS
CPVCS       Rev 1.5   09/14/95 16:38:34   dcg
CPVCS    remove dependencies on mesh_object.h data structures
CPVCS
CPVCS       Rev 1.4   09/11/95 14:45:08   het
CPVCS    Change to the storage block based CMO stuff.
CPVCS
CPVCS       Rev 1.3   03/15/95 15:24:08   ejl
CPVCS    Finished installing the defaults.
CPVCS
CPVCS       Rev 1.2   02/16/95 09:57:24   ejl
CPVCS    Fixed bugs, fixed hole in the Create command.
CPVCS    Added commands MODATT, LENGTH, MEMORY, & COMPRESS.
CPVCS
CPVCS       Rev 1.1   02/10/95 14:09:34   ejl
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
      integer ierror_return
C
C#######################################################################
C
C     LOCAL VARIABLE DEFINITION
 
      integer icmo_index, i, len, len1, ierr ,itype
C
      integer ifound_length, ifound_rank
C
      character*132 logmess
C
C#######################################################################
C
C
      pointer (ipcmo_pointer, icmo_pointer)
      pointer (ipcmo_pointer, xcmo_pointer)
      integer icmo_pointer(10000000)
      REAL*8 xcmo_pointer(10000000)
C
      integer lncmo_pointer
C
      character*32 cmo_name_sb
C
      integer icscode,iout,ilen
C
      character*32 cname, ctype, clength, crank,partname
C
C#######################################################################
C
      integer icharlnf
C
C#######################################################################
C
      partname='define_cmo_lg'
      ierror_return=0
C
      len1=icharlnf(cmo_name)
C
      if((cmo_name(1:len1).eq.'-cmo-') .or.
     *   (cmo_name(1:len1).eq.'-def-')) then
C
C....    Use the Current Mesh Object.
C
         call cmo_get_name(cmo_name,ierror_return)
C
         len1=icharlnf(cmo_name)
C
      endif
C
C.... Loop over all the Mesh Objects
C
      call mmfindbk('cmo_names',partname,ipcmo_names,len,icscode)
      call mmfindbk('cmo_natts',partname,ipcmo_natts,len,icscode)
C
      do icmo_index=1,number_of_mesh_objects
C
C....    Get the Mesh Object name.
C
         cmo_name_sb=cmo_names(icmo_index)
C
         if(cmo_name(1:len).eq.cmo_name_sb(1:len) .or.
     *      cmo_name(1:len1).eq.'-all-') then
           call mmfindbk('cmo_attlist',cmo_name_sb,ipcmo_attlist,  
     *       len,icscode)
C
C....       Loop over all Attributes for this Mesh Object.
C
            do i=1,cmo_natts(icmo_index)
               cname=cmo_attlist(number_of_params_per_att*(i-1)+1)
               ctype=cmo_attlist(number_of_params_per_att*(i-1)+2)
C
C....          TYPE Field.
C
               if(ctype(1:1).eq.'V') then
C....             Check for a valid type.
C
                  len=icharlnf(ctype)
                  if((ctype(1:len).ne.'VINT') .and.
     *               (ctype(1:len).ne.'VDOUBLE').and.
     *               (ctype(1:len).ne.'VCHAR')) then
C
C....                Invalid Type.
C
                     ierror_return=-3
C
                     write(logmess,'(a,a,a)')
     *                     'Problems with the TYPE field. ',
     *                     cmo_name_sb,ctype
                     call writloga('default',0,logmess,0,ierr)
                     go to 9999
C
                  endif
C
C....             Check that all fields are defined.
C
                  ifound_length=0
                  ifound_rank=0
C
C
C....             LENGTH Field.
C
                  clength=cmo_attlist(number_of_params_per_att*(i-1)+4)
                  call cmo_get_info(clength,cmo_name_sb,iout,ilen,
     *             itype,icscode)
c
C....             Check the LENGTH field.
C
                  if(icscode.eq.0) then
                     ifound_length=1
                  endif
C
C
C....             RANK Field.
C
C
                  crank=cmo_attlist(number_of_params_per_att*(i-1)+3)
                  call cmo_get_info(clength,cmo_name_sb,iout,ilen,
     *             itype,icscode)
C
C....             Check the RANK field.
C
C
                  if(icscode.eq.0) then
                     ifound_rank=1
                  endif
C
                  if (ifound_length.eq.0) then
C
C....                Problems with length field.
C
                     ierror_return=-1
C
                     write(logmess,'(a,a,a)')
     *                  'Problems with the LENGTH field: ',
     *                   cmo_name_sb, clength
                     call writloga('default',0,logmess,0,ierr)
C
                  endif
C
                  if (ifound_rank.eq.0) then
C
C....                Problems with rank field.
C
                     ierror_return=-2
C
                     write(logmess,'(a,a,a)')
     *                     'Problems with the RANK field: ',
     *                      cmo_name_sb,crank
                     call writloga('default',0,logmess,0,ierr)
C
                  endif
C
C....             Check that the CMO-field has been allocated.
C
                  if(ifound_length.eq.0.and.ifound_rank.eq.0) then
                     call mmfindbk(cname,cmo_name_sb,
     *                             ipcmo_pointer,lncmo_pointer,icscode)
                     if (icscode.ne.0) then
C
C....                   Problems with field array.
C
                        ierror_return=-3
C
                        write(logmess,'(a,a,a)')
     *                        'Problems with the CMO field array: ',
     *                         cmo_name_sb,cname
                        call writloga('default',0,logmess,0,ierr)
                     endif
C
                  endif
C
               elseif(ctype.ne.'INT'.and.ctype.ne.'REAL'.and.
     *               ctype.ne.'CHARACTER') then
                        write(logmess,'(a,a,a16,a16)')
     *                       'Problems with the CMO type: ',
     *                         cmo_name_sb,cname,ctype
                        call writloga('default',0,logmess,0,ierr)
               else
               endif
C
            enddo
C
         endif
C
      enddo
C
 9999 return
      end
