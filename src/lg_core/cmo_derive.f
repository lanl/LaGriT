      subroutine cmo_derive(cmo_name1,cmo_name2,ierror_return)
C
C
C#######################################################################
C
C      PURPOSE -
C
C         This Routine Derives a Mesh Object from another Mesh Object.
C
C      INPUT ARGUMENTS -
C
C         cmo_name1 - (character) Derived Mesh Object Name.
C         cmo_name2 - (character) Master Mesh Object Name.
C
C      OUTPUT ARGUMENTS -
C
C         ierror_return - Error Return Code (==0 ==> OK, <>0 ==> Error)
C
C      CHANGE HISTORY -
C
C         $Log: cmo_derive.f,v $
C         Revision 2.00  2007/11/05 19:45:48  spchu
C         Import to CVS
C
CPVCS    
CPVCS       Rev 1.3   Tue Feb 15 13:18:46 2000   dcg
CPVCS    set number of attributes in new mo from number in old
CPVCS
CPVCS       Rev 1.2   07 Feb 2000 16:44:18   dcg
CPVCS
CPVCS       Rev 1.0   Thu Jan 20 16:07:50 2000   dcg
CPVCS     Initial revision.
CPVCS
CPVCS       Rev 1.12   Mon Apr 14 16:41:04 1997   pvcs
CPVCS    No change.
CPVCS
CPVCS       Rev 1.11   Mon Nov 18 10:28:56 1996   dcg
CPVCS    remove charater literals from argument lists
CPVCS
CPVCS       Rev 1.10   Wed Jun 19 10:28:38 1996   het
CPVCS    Zero out number of nodes, edges, faces, elements for cmo/derive/
CPVCS
CPVCS       Rev 1.9   09/11/95 14:43:52   het
CPVCS    Change to the storage block based CMO stuff.
CPVCS
CPVCS       Rev 1.8   08/15/95 18:23:04   het
CPVCS    Cleanup code and correct errors
CPVCS
CPVCS       Rev 1.7   05/19/95 16:45:14   ejl
CPVCS    Fixed bug with copying nnodes.
CPVCS
CPVCS       Rev 1.6   05/19/95 15:59:02   ejl
CPVCS    Fixed error with delete
CPVCS
CPVCS       Rev 1.5   03/15/95 15:22:50   ejl
CPVCS    Finished installing the defaults.
CPVCS
CPVCS       Rev 1.4   02/16/95 09:56:04   ejl
CPVCS    Fixed bugs, fixed hole in the Create command.
CPVCS    Added commands MODATT, LENGTH, MEMORY, & COMPRESS.
CPVCS
CPVCS       Rev 1.3   02/10/95 14:07:14   ejl
CPVCS    Fix bugs left from last update.
CPVCS
CPVCS       Rev 1.1   01/30/95 12:41:26   het
CPVCS    Correct syntax error for the SGI
CPVCS
CPVCS       Rev 1.0   01/30/95 11:41:28   dcg
CPVCS     Original Version
C
C#######################################################################
C
      implicit none
C
C#######################################################################
C
      include 'cmo_lg.h'

C arguments (cmo_name1,cmo_name2,ierror_return)
      character*(*) cmo_name1, cmo_name2
      integer ierror_return

C#######################################################################
C
C     LOCAL VARIABLE DEFINITION
C
      integer i,ierror,icscode,ierr,len,natts,index,ilen,length
      integer icharlnf
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
c
c  list of attribute parameters for new mesh object
c  for each of the names in defcmo_attparam_names
c
      pointer (ipnew_attlist,new_attlist)
      character*32 new_attlist(*)

      character*32 partname
      character*132 logmess
C
C#######################################################################
C BEGIN begin
C
      len=icharlnf(cmo_name1)
C
      if((cmo_name1(1:len).eq.'-cmo-') .or.
     *   (cmo_name1(1:len).eq.'-def-')) then
C
C....    Use the Current Mesh Object as the Sink.
C
         call cmo_get_name(cmo_name1,ierror_return)
C
      endif
C
      len=icharlnf(cmo_name2)
C
      if((cmo_name2(1:len).eq.'-cmo-') .or.
     *   (cmo_name2(1:len).eq.'-def-')) then
C
C....    Use the Current Mesh Object as the Master.
C
         call cmo_get_name(cmo_name2,ierror_return)
C
      endif
C
      len=max(icharlnf(cmo_name1), icharlnf(cmo_name2))
      if(cmo_name1(1:len) .eq. cmo_name2(1:len)) then
C
C....    The two Mesh Objects are identical.
C
         ierror_return=-1
         write(logmess,'(a,a,a)')
     *      'Mesh Objects are identical: ',cmo_name1, cmo_name2
         call writloga('default',0,logmess,0,ierr)
C
      else
C
         if(cmo_name2(1:9).eq.'-default-') then
C
C....       Derive Mesh Object 1 from the Default Mesh object.
C
            call cmo_create(cmo_name1,ierror_return)
C
         else
C
C....       Search table for the Master Mesh Object.
C
            index=0
            partname='define_cmo_lg'
            call mmfindbk('cmo_names',partname,ipcmo_names,ilen,
     *        icscode)
            call mmfindbk('cmo_natts',partname,ipcmo_natts,ilen,
     *        icscode)
            do i=1,number_of_mesh_objects
               if(cmo_names(i).eq.cmo_name2) then
                 index=i
                 natts=cmo_natts(i)
               endif
            enddo
C
C....       Set the Mesh Object2 storage block information.
C
            if(index.eq.0) then
C
               ierror_return=-1
C
               write(logmess,'(a,a)')
     *               'Master Mesh Object does not exist: ', cmo_name2
               call writloga('default',0,logmess,0,ierr)
               go to 9999
C
            endif
C
C....          Does Mesh Object 1 exists?
C
            if(cmo_name1.ne.'-default-' )then
               call cmo_exist(cmo_name1,ierror)
C
               if(ierror.eq.0) then
C....             Release Mesh Object 1 if it exists.
                  call cmo_release(cmo_name1,ierror)
               endif
C
C....          Build a directory entry for the new Mesh Object.
C
               call cmo_create_dir(cmo_name1,ierror_return)
            else
               call mmrelprt('default_cmo_lg',icscode)
               cmo_name1='default_cmo_lg'
            endif
C
C
c.... Get memory for new mesh object attribute definitions
c
            length=natts*number_of_params_per_att
            call mmgetblk('cmo_attlist',cmo_name1,ipnew_attlist,
     *      length,3,icscode)
c
C.... Copy the default mesh object attribute parameters into the
C     new mesh object
C
            length=number_of_params_per_att*natts
            call mmfindbk('cmo_attlist',cmo_name2,ipcmo_attlist,
     *         ilen,icscode)
            do i=1,length
                new_attlist(i)=cmo_attlist(i)
            enddo
c
c.... Now copy values of default into new attribute
c
            call mmfindbk ('cmo_attparam_cdefault',cmo_name2,
     *      ipcmo_attparam_cdefault,len,ierror)
            call mmfindbk ('cmo_attparam_idefault',cmo_name2,
     *      ipcmo_attparam_idefault,len,ierror)
            call mmfindbk ('cmo_attparam_rdefault',cmo_name2,
     *      ipcmo_attparam_rdefault,len,ierror)
            length=natts
            call mmgetblk('cmo_attparam_idefault',cmo_name1,
     *        ipnew_attparam_idefault,length,1,icscode)
            call mmgetblk('cmo_attparam_rdefault',cmo_name1,
     *        ipnew_attparam_rdefault,length,2,icscode)
            call mmgetblk('cmo_attparam_cdefault',cmo_name1,
     *        ipnew_attparam_cdefault,length,3,icscode)
            do i=1,natts
              new_attparam_cdefault(i)=cmo_attparam_cdefault(i)
              new_attparam_idefault(i)=cmo_attparam_idefault(i)
              new_attparam_rdefault(i)=cmo_attparam_rdefault(i)
            enddo
c
C
C....          Make this the Current Mesh Object.
C
            call cmo_select(cmo_name1,ierror_return)
C
C....          Create the Mesh Object.
C
 
               call cmo_set_info('nnodes',cmo_name1,0,1,1,icscode)
               call cmo_set_info('nelements',cmo_name1,0,1,1,icscode)
               call cmo_set_info('nfaces',cmo_name1,0,1,1,icscode)
               call cmo_set_info('nedges',cmo_name1,0,1,1,icscode)
C
c
c..... set the number of attributes in new mesh object
c      to the number in the old one
c
             call cmo_get_index(cmo_name1,index,ierror)
             cmo_natts(index)=natts
               call cmo_allocate(cmo_name1,ierror_return)
C
C....          Make this the Current Mesh Object.
C
            call cmo_select(cmo_name1,ierror_return)
C
         endif
C
      endif
C
 9999 return
      end
