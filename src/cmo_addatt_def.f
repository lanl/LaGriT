      subroutine cmo_addatt_def(imsgin,xmsgin,cmsgin,msgtype,nwds,
     *                          ierror_return)
C
C
C#######################################################################
C
C      PURPOSE -
C
C         This Routine Adds Attributes to the Default Table..
C
C      INPUT ARGUMENTS -
C
C         imsgin()  - Integer array of command input tokens
C         xmsgin()  - Real array of command input tokens
C         cmsgin()  - Character array of command input tokens
C         msgtype() - Integer array of command input token types
C         nwds      - Number of command input tokens
C
C      OUTPUT ARGUMENTS -
C
C         ierror_return - Error Return Code (==0 ==> OK, <>0 ==> Error)
C
C      CHANGE HISTORY -
C
C         $Log: cmo_addatt_def.f,v $
C         Revision 2.00  2007/11/05 19:45:47  spchu
C         Import to CVS
C
CPVCS    
CPVCS       Rev 1.3   07 Jul 2006 08:48:36   gable
CPVCS    Modified screen output to reduce blank space.
CPVCS    
CPVCS       Rev 1.2   10 Apr 2001 11:04:10   dcg
CPVCS    shorten too long name
CPVCS
CPVCS       Rev 1.1   21 Jan 2000 17:02:38   dcg
CPVCS
CPVCS
CPVCS       Rev 1.8   Tue Nov 02 19:07:48 1999   jtg
CPVCS    fixed hardwired character*32 for cmsgin
CPVCS
CPVCS       Rev 1.7   Mon Apr 14 16:39:52 1997   pvcs
CPVCS    No change.
CPVCS
CPVCS       Rev 1.6   Mon Nov 18 11:00:50 1996   dcg
CPVCS    remove character literals from argument lists
CPVCS
CPVCS       Rev 1.5   09/11/95 14:43:14   het
CPVCS    Change to the storage block based CMO stuff.
CPVCS
CPVCS       Rev 1.4   03/15/95 15:22:16   ejl
CPVCS    Finished installing the defaults.
CPVCS
CPVCS       Rev 1.3   02/16/95 10:22:58   ejl
CPVCS    Put return afer end statement.
CPVCS
CPVCS       Rev 1.2   02/16/95 09:55:40   ejl
CPVCS    Fixed bugs, fixed hole in the Create command.
CPVCS    Added commands MODATT, LENGTH, MEMORY, & COMPRESS.
CPVCS
CPVCS       Rev 1.1   02/10/95 14:06:28   ejl
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
      integer nwds, imsgin(nwds), msgtype(nwds)
      REAL*8 xmsgin(nwds)
      character*(*) cmsgin(nwds)
C
      integer ierror_return
C
C#######################################################################
C
C     LOCAL VARIABLE DEFINITION
C
      character*32 att_name, partname,cmo_name
      integer ierr, iatt_index, nodnx, icscode,i,natts,len,
     *  posname,postype,posrank,poslen,posint,posio,pospers,
     *  lenattlist
C
      character*132 logmess
C
C#######################################################################
C
      integer icharlnf
C
C#######################################################################
C
      partname='default_cmo_lg'
      att_name=cmsgin(4)
      call mmfindbk('cmo_attlist',partname,ipcmo_attlist,
     *                   lenattlist,icscode)
      do i=1,number_of_default_attributes
         if(cmo_attlist(number_of_params_per_att*(i-1)+1)
     *             .eq.att_name) then
C
C....    This is a existing attribute.
C
            ierror_return=-1
C
            write(logmess,'(a,a)')
     *         'CMO_ADDATT error: attribute already exist: ',
     *          att_name(1:icharlnf(att_name))
            call writloga('default',0,logmess,0,ierr)
            go to 9999
         endif
C
      enddo
C
C....    Change the Default Attribute Table.
C
      ierror_return=0
      natts=number_of_default_attributes
      number_of_default_attributes=natts+1
C
C....    See if there is enough space for new attribute
C
      if(lenattlist.lt.(natts+1)*number_of_params_per_att*4) then
        call mmincblk('cmo_attlist',partname,ipcmo_attlist,2000,icscode)
      endif
C
c.... Find postions of name, type, rank and length
      partname='define_cmo_lg'
      call mmfindbk( 'defcmo_attparam_names',partname,
     *      ipdefcmo_attparam_names,len,icscode)
      do i=1,number_of_default_attparam_name
            if(defcmo_attparam_names(i).eq.'name') posname=i
            if(defcmo_attparam_names(i).eq.'type') postype=i
            if(defcmo_attparam_names(i).eq.'rank') posrank=i
            if(defcmo_attparam_names(i).eq.'length') poslen=i
            if(defcmo_attparam_names(i).eq.'interpolation') posint=i
            if(defcmo_attparam_names(i).eq.'persistence') pospers=i
            if(defcmo_attparam_names(i).eq.'ioflag') posio=i
      enddo
      cmo_attlist(number_of_params_per_att*(natts)+posname)=att_name
      cmo_attlist(number_of_params_per_att*(natts)+postype)=cmsgin(5)
      cmo_attlist(number_of_params_per_att*(natts)+posrank)=cmsgin(6)
      cmo_attlist(number_of_params_per_att*(natts)+poslen)=cmsgin(7)
      cmo_attlist(number_of_params_per_att*(natts)+posint)=cmsgin(8)
      cmo_attlist(number_of_params_per_att*(natts)+pospers)=
     *    'temporary'
      cmo_attlist(number_of_params_per_att*(natts)+posio)=cmsgin(10)
C
 
9999  return
      end
