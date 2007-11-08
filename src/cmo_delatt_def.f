      subroutine cmo_delatt_def(att_name,ierror_return)
C
C
C#######################################################################
C
C      PURPOSE -
C
C         This Routine Deletes an Attribute from the Default Mesh Object.
C         This template mesh object is not part of the active cmo list
C         and is named with the reserved word -default-
C
C      INPUT ARGUMENTS -
C
C         att_name - (character) Attribute Name.
C
C      OUTPUT ARGUMENTS -
C
C         ierror_return - Error Return Code (==0 ==> OK, <>0 ==> Error)
C
C      CHANGE HISTORY -
C
C         $Log: cmo_delatt_def.f,v $
C         Revision 2.00  2007/11/05 19:45:48  spchu
C         Import to CVS
C
CPVCS    
CPVCS       Rev 1.2   13 Jun 2007 07:26:40   tam
CPVCS    added  error reports to be clear that the -default- 
CPVCS    attribute is permanent and is not related to the
CPVCS    parameter keyword -def- used in command syntax
CPVCS    
CPVCS       Rev 1.1   10 Apr 2001 11:04:14   dcg
CPVCS    shorten too long name
CPVCS    
CPVCS       Rev 1.0   21 Jan 2000 17:02:56   dcg
CPVCS     
CPVCS
CPVCS       Rev 1.8   Fri Jan 22 09:54:36 1999   dcg
CPVCS    remove extra comma from format
CPVCS
CPVCS       Rev 1.7   Mon Apr 14 16:41:02 1997   pvcs
CPVCS    No change.
CPVCS
CPVCS       Rev 1.6   Mon Nov 18 10:28:52 1996   dcg
CPVCS    remove charater literals from argument lists
CPVCS
CPVCS       Rev 1.5   09/11/95 14:43:48   het
CPVCS    Change to the storage block based CMO stuff.
CPVCS
CPVCS       Rev 1.4   03/15/95 15:22:46   ejl
CPVCS    Finished installing the defaults.
CPVCS
CPVCS       Rev 1.3   02/16/95 10:39:16   ejl
CPVCS    Put CR after end statement.
CPVCS
CPVCS       Rev 1.2   02/16/95 09:56:02   ejl
CPVCS    Fixed bugs, fixed hole in the Create command.
CPVCS    Added commands MODATT, LENGTH, MEMORY, & COMPRESS.
CPVCS
CPVCS       Rev 1.1   02/10/95 14:07:00   ejl
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
      character*(*) att_name
C
      integer ierror_return
C
C#######################################################################
C
C     LOCAL VARIABLE DEFINITION
C
      integer iatt,j,i, len1, ierr, icscode,len,pospers,postype,
     *  ierror
C
      character*32 cpersistence,partname
C
      character*132 logmess
C
C#######################################################################
C
      integer icharlnf
C
C#######################################################################
C
      len1=icharlnf(att_name)
      if (len1.gt.32) len1=32
C
C     -default- is the assumed reserved name for this mesh object 
C     it is used as a template and is not part of the active CMO list
C
C.... Search table for Attribute.

      partname='default_cmo_lg'
      call mmfindbk('cmo_attlist',partname,ipcmo_attlist,
     *                   len,icscode)
      do i=1,number_of_default_attributes
            if(cmo_attlist(number_of_params_per_att*(i-1)+1)
     *             .eq.att_name) then
               iatt=i
               ierror_return=0
               go to 50
            endif
      enddo
C
      ierror_return=-1
      write(logmess,'(a,a,a)')
     *  'ERROR cmo_delatt_def: ',
     *  '-default- cmo attribute not found: ',att_name(1:len1)
      call writloga('default',0,logmess,0,ierr)
      goto 9999
 
C
C
c   found attribute
c.... Find postions of persistence and type
c
 
50    partname='define_cmo_lg'
      call mmfindbk( 'defcmo_attparam_names',partname,
     *      ipdefcmo_attparam_names,len,icscode)
      do i=1,number_of_default_attparam_name
             if(defcmo_attparam_names(i).eq.'persistence') pospers=i
             if(defcmo_attparam_names(i).eq.'type') postype=i
      enddo
      partname='default_cmo_lg'
 
C
      cpersistence=cmo_attlist(number_of_params_per_att*(iatt-1)+
     *       pospers)
      if(cpersistence(1:9) .eq. 'permanent') then
C
C....    Cannot delete a Permanent Attribute.
C
         ierror_return=-1
         write(logmess,'(a,a,a)')
     *    'ERROR cmo_delatt_def: ',
     *    '-default- cmo attribute is Permanent: ',att_name(1:len1)
           call writloga('default',0,logmess,0,ierr)
         go to 9999
      endif
C
      ierror_return=0
C
C....       Delete the attribute from the list of attributes.
C
       do i=iatt,number_of_default_attributes
          do j=1,number_of_params_per_att
                 cmo_attlist(number_of_params_per_att*(i-1)+j)=
     *             cmo_attlist(number_of_params_per_att*(i)+j)
          enddo
       enddo
       call mmfindbk ('cmo_attparam_idefault',partname,
     *        ipcmo_attparam_idefault,len,ierror)
       call mmfindbk ('cmo_attparam_rdefault',partname,
     *       ipcmo_attparam_rdefault,len,ierror)
       call mmfindbk ('cmo_attparam_cdefault',partname,
     *        ipcmo_attparam_cdefault,len,ierror)
       do i=iatt,number_of_default_attributes
              cmo_attparam_idefault(i)=cmo_attparam_idefault(i+1)
              cmo_attparam_rdefault(i)=cmo_attparam_rdefault(i+1)
              cmo_attparam_cdefault(i)=cmo_attparam_cdefault(i+1)
       enddo
       number_of_default_attributes=number_of_default_attributes-1
C
 9999 continue
C
      return
      end
