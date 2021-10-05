      subroutine cmo_delatt_cmo(cmo_name,att_name,ierror_return)
C
C
C#######################################################################
C
C      PURPOSE -
C
C         This Routine Deletes an Attribute from an existing Mesh Object.
C
C      INPUT ARGUMENTS -
C
C         cmo_name - (character) Mesh Object Name.
C         att_name - (character) Attribute Name.
C
C      OUTPUT ARGUMENTS -
C
C         ierror_return - Error Return Code (==0 ==> OK, <>0 ==> Error)
C
C      CHANGE HISTORY -
C
C         $Log: cmo_delatt_cmo.f,v $
C         Revision 2.00  2007/11/05 19:45:48  spchu
C         Import to CVS
C
CPVCS    
CPVCS       Rev 1.2   13 Jun 2007 07:20:24   tam
CPVCS    added subroutine name and cmo to clarify error reports
CPVCS    
CPVCS       Rev 1.1   10 Apr 2001 11:04:14   dcg
CPVCS    shorten too long name
CPVCS    
CPVCS       Rev 1.0   21 Jan 2000 17:02:52   dcg
CPVCS     
CPVCS
CPVCS       Rev 1.7   Mon Apr 14 16:41:00 1997   pvcs
CPVCS    No change.
CPVCS
CPVCS       Rev 1.6   Mon Nov 18 10:28:46 1996   dcg
CPVCS    remove charater literals from argument lists
CPVCS
CPVCS       Rev 1.5   09/11/95 14:43:44   het
CPVCS    Change to the storage block based CMO stuff.
CPVCS
CPVCS       Rev 1.4   03/15/95 15:22:42   ejl
CPVCS    Finished installing the defaults.
CPVCS
CPVCS       Rev 1.3   02/16/95 10:39:12   ejl
CPVCS    Put CR after end statement.
CPVCS
CPVCS       Rev 1.2   02/16/95 09:55:58   ejl
CPVCS    Fixed bugs, fixed hole in the Create command.
CPVCS    Added commands MODATT, LENGTH, MEMORY, & COMPRESS.
CPVCS
CPVCS       Rev 1.1   02/10/95 14:06:56   ejl
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
      character*(*) cmo_name, att_name
C
      integer ierror_return
C
C#######################################################################
C
C     LOCAL VARIABLE DEFINITION
C
      integer len1,len2,iatt_index, iexist, icscode,len,i,j,
     *  lenname,icmo_index,ierror,postype,pospers,natts,iatt
      integer ierr, ier
C
      character*32 ctype, cpersistence
      character*16 partname
C
      character*132 logmess
C
      pointer (ipcmo_pointer, icmo_pointer)
      integer icmo_pointer(10000000)
C
C#######################################################################
C
      integer icharlnf
C
C#######################################################################
C
      len1=icharlnf(cmo_name)
      len2=icharlnf(att_name)
      partname='define_cmo_lg'
C
      if((cmo_name(1:len1).eq.'-cmo-') .or.
     *   (cmo_name(1:len1).eq.'-def-')) then
C
C....    Use the Current Mesh Object.
C
         call cmo_get_name(cmo_name,ierror_return)
         len1=icharlnf(cmo_name)
C
      endif
C
C.... Search table for Mesh Object.
C
      call cmo_exist(cmo_name,iexist)
      if(iexist.ne.0) then
C
         ierror_return=-1
C
         write(logmess,'(a,a)') 
     *    'Mesh Object does not exist: ',cmo_name(1:len1)
         call writloga('default',0,logmess,0,ierr)
C
      else
C
C....    Search table for Attribute.
C
         ierror_return=0
c
c  find attribute
c
         call cmo_get_index(cmo_name,icmo_index,ierror)
C
         if(ierror.ne.0) go to 9999
         call mmfindbk('cmo_natts',partname,ipcmo_natts,len,
     *                   icscode)
         natts=cmo_natts(icmo_index)
         call mmfindbk('cmo_attlist',cmo_name,ipcmo_attlist,
     *                   len,icscode)
         do i=1,natts
            if(cmo_attlist(number_of_params_per_att*(i-1)+1)
     *             .eq.att_name) then
               iatt=i
               ierror_return=0
               go to 50
            endif
         enddo
C
C....    The attribute does not exist for this Mesh Object.
C
         ierror_return=-1
C
         write(logmess,'(a,a,a,a)')
     *     'ERROR cmo_delatt_cmo: ',cmo_name(1:len1),
     *     ' Attribute does not exist: ',att_name(1:len2)
         call writloga('default',0,logmess,0,ierr)
C
         go to 9999
C
c   found attribute
c.... Find postions of persistence and type
c
 50      call mmfindbk( 'defcmo_attparam_names',partname,
     *      ipdefcmo_attparam_names,len,icscode)
         do i=1,number_of_default_attparam_name
             if(defcmo_attparam_names(i).eq.'persistence') pospers=i
             if(defcmo_attparam_names(i).eq.'type') postype=i
         enddo
 
C
         cpersistence=cmo_attlist(number_of_params_per_att*(iatt-1)+
     *       pospers)
         if(cpersistence(1:9) .eq. 'permanent') then
C
C....       Cannot delete a Permanent Attribute.
C
            ierror_return=-1
            write(logmess,'(a,a,a,a)')
     *        'ERROR cmo_delatt_cmo: ',cmo_name(1:len1),
     *        ' Attribute is Permanent: ',att_name(1:len2)
            call writloga('default',0,logmess,0,ierr)
            go to 9999
C
         else
C
C....       Remove the Attribute.
C
            ierror_return=0
C
            ctype=cmo_attlist(number_of_params_per_att*(iatt-1)+
     *       postype)
C
            if (ctype(1:1) .eq. 'V') then
C
C....          Release the Memory Managed Array for the Deleted Attribute.
C
               call mmrelblk(att_name,
     *                       cmo_name,
     *                       ipcmo_pointer,ier)
C
               if(ier.ne.0) call cmo_mm_error('cmo_delatt_cmo')
C
            endif
C
C....       Delete the attribute from the list of attributes.
C
            do i=iatt,natts
              do j=1,number_of_params_per_att
                 cmo_attlist(number_of_params_per_att*(i-1)+j)=
     *             cmo_attlist(number_of_params_per_att*(i)+j)
              enddo
            enddo
            call mmfindbk ('cmo_attparam_idefault',cmo_name,
     *        ipcmo_attparam_idefault,len,ierror)
            call mmfindbk ('cmo_attparam_rdefault',cmo_name,
     *       ipcmo_attparam_rdefault,len,ierror)
            call mmfindbk ('cmo_attparam_cdefault',cmo_name,
     *        ipcmo_attparam_cdefault,len,ierror)
            do i=iatt,natts
              cmo_attparam_idefault(i)=cmo_attparam_idefault(i+1)
              cmo_attparam_rdefault(i)=cmo_attparam_rdefault(i+1)
              cmo_attparam_cdefault(i)=cmo_attparam_cdefault(i+1)
            enddo
            cmo_natts(icmo_index)=natts-1
C
         endif
C
      endif
C
 9999 continue
C
      return
      end
