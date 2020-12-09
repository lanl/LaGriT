      subroutine cmo_addatt_cmo(imsgin,xmsgin,cmsgin,msgtype,nwds,
     *                          ierror_return)
C
C
C#######################################################################
C
C      PURPOSE -
C
C         This Routine Adds Attributes to an existing Mesh Object.
C
C      INPUT ARGUMENTS -
C
C         imsgin()   - Integer array of command input tokens
C         xmsgin()   - Real array of command input tokens
C         cmsgin()   - Character array of command input tokens
C         msgtype()  - Integer array of command input token types
C         nwds       - Number of command input tokens
C
C      OUTPUT ARGUMENTS -
C
C         ierror_return - Error Return Code (==0 ==> OK, <>0 ==> Error)
C
C      CHANGE HISTORY -
C
C         $Log: cmo_addatt_cmo.f,v $
C         Revision 2.00  2007/11/05 19:45:47  spchu
C         Import to CVS
C
CPVCS    
CPVCS       Rev 1.9   07 Jul 2006 08:49:12   gable
CPVCS    Modified screen output to reduce blank space.
CPVCS    
CPVCS       Rev 1.8   28 Aug 2001 10:51:42   dcg
CPVCS    set persistence to value input via cmsgin (used to be hard wired 'temporary')
CPVCS
CPVCS       Rev 1.7   10 Apr 2001 11:04:08   dcg
CPVCS    shorten too long name
CPVCS
CPVCS       Rev 1.6   17 Feb 2000 20:44:16   jtg
CPVCS    added verbosity flag in 12th position so tone down screen output
CPVCS    for calls from iterative routines like copyatt_mpary_lg if
CPVCS    desired
CPVCS
CPVCS       Rev 1.5   07 Feb 2000 16:45:42   dcg
CPVCS
CPVCS       Rev 1.4   Tue Feb 01 13:40:02 2000   dcg
CPVCS
CPVCS       Rev 1.3   Mon Jan 31 09:54:56 2000   dcg
CPVCS
CPVCS       Rev 1.14   Tue Nov 02 19:08:16 1999   jtg
CPVCS    fixed hardwired character*32 for cmsgin
CPVCS
CPVCS       Rev 1.13   Mon Apr 14 16:39:50 1997   pvcs
CPVCS    No change.
CPVCS
CPVCS       Rev 1.12   Thu Jan 30 19:35:30 1997   het
CPVCS    Refresh the ipcmoatt pointer because the memory was being
CPVCS       changed but the pointer was not updates.
CPVCS
CPVCS       Rev 1.11   11/13/95 16:21:16   dcg
CPVCS    allocate integer arrays for VINT - real for VDOUBLE
CPVCS
CPVCS       Rev 1.10   11/07/95 17:15:34   dcg
CPVCS    change flag to 2 in mmgetblk calls
CPVCS
CPVCS       Rev 1.9   10/16/95 10:21:16   het
CPVCS    Correct sbnloc/sbnstor pointer problem
CPVCS
CPVCS       Rev 1.8   09/15/95 10:32:18   dcg
CPVCS    fix index field for new attribute
CPVCS
CPVCS       Rev 1.6   09/13/95 16:45:34   dcg
CPVCS    replace character literals in argument lists with
CPVCS    character variables
CPVCS
CPVCS       Rev 1.5   09/11/95 14:43:10   het
CPVCS    Change to the storage block based CMO stuff.
CPVCS
CPVCS       Rev 1.4   03/15/95 15:22:14   ejl
CPVCS    Finished installing the defaults.
CPVCS
CPVCS       Rev 1.3   02/16/95 10:22:54   ejl
CPVCS    Put return afer end statement.
CPVCS
CPVCS       Rev 1.2   02/16/95 09:55:36   ejl
CPVCS    Fixed bugs, fixed hole in the Create command.
CPVCS    Added commands MODATT, LENGTH, MEMORY, & COMPRESS.
CPVCS
CPVCS       Rev 1.1   02/10/95 14:06:26   ejl
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
      character*32 cmo_name, att_name, ctype,crank,clength
      integer j, icmo_index,ierror,len,natts,i,lout,itype,
     *  posname,postype,posrank,poslen,posint,posio,pospers
      integer lentype, length, irank, mmlength, ierr, ier
     *  , icscode, verbosity
C
      character*132 logmess
C
      integer idefault
      real*8 xdefault
      character*32 cdefault
C
      pointer (ipcmo_pointer, icmo_pointer)
      pointer (ipcmo_pointer, xcmo_pointer)
      pointer (ipcmo_pointer, ccmo_pointer)
      integer icmo_pointer(*)
      REAL*8 xcmo_pointer(*)
      character*32 ccmo_pointer(*)
C
      character*32  partname
C
C#######################################################################
C
      integer icharlnf
C
C#######################################################################
C
C
C
 
      if (nwds.ge.12.and.msgtype(12).eq.1) then
         verbosity=imsgin(12)
      else
         verbosity=1
      endif
      if (nwds.lt.4) goto 9998
 
      partname='define_cmo_lg'
      cmo_name = cmsgin(3)
      att_name = cmsgin(4)

C
      if((cmo_name(1:icharlnf(cmo_name))) .eq. '-def-') then
         call cmo_get_name(cmo_name, ier)
         if(ier.ne.0) then
           write(logmess,9000) cmo_name(1:icharlnf(cmo_name))
 9000   format(" ADDATT: CMO found bad mesh object: ",a)
           call writloga('default',0,logmess,0,ier)
           ierror_return = 1
           goto 9999
         endif
      endif

C     Check the mesh object name
      call cmo_exist(cmo_name,ier)
      if(ier.ne.0) then
         write(logmess,'(a,a)')
     *     'ADDATT: Not a valid mesh object: ',
     *      cmo_name(1:icharlnf(cmo_name))
         call writloga('default',1,logmess,1,ier)
         ierror_return = 1
         goto 9999
      endif



C
C.... Check if this a new Attribute.
C
C
      call cmo_get_index(cmo_name,icmo_index,ierror)
C
      if(ierror.ne.0) go to 9998
      call mmfindbk('cmo_natts',partname,ipcmo_natts,len,
     *                   icscode)
      natts=cmo_natts(icmo_index)
      call mmfindbk('cmo_attlist',cmo_name,ipcmo_attlist,
     *                   len,icscode)
      do i=1,natts
         if(cmo_attlist(number_of_params_per_att*(i-1)+1)
     *             .eq.att_name) then
C
C...     Existing Attribute.
C
C
            if (verbosity.gt.0) then
               ierror_return=-1
               write(logmess,'(a,a,2x,a)')
     *            'CMO_ADDATT warning: attribute already exist: ',
     *            cmo_name(1:icharlnf(cmo_name)),
     *            att_name(1:icharlnf(att_name))
               call writloga('default',0,logmess,0,ierr)
            else
               ierror_return=-2
            endif
C
            go to 9999
         endif
      enddo
C
C....    This is a new attribute.
C
      ierror_return=0
      natts=natts+1
      cmo_natts(icmo_index)=natts
C
C....    See if there is enough space for new attribute
C
      call mmfindbk('cmo_attlist',cmo_name,ipcmo_attlist,len,icscode)
      if(len.lt.natts*number_of_params_per_att) then
        call mmincblk('cmo_attlist',cmo_name,ipcmo_attlist,
     *      number_of_params_per_att*20,icscode)
      endif
C
c.... Find postions of name, type, rank and length
 10   call mmfindbk( 'defcmo_attparam_names',partname,
     *      ipdefcmo_attparam_names,len,icscode)
      call mmfindbk( 'cmo_attparam_idefault',cmo_name,
     *      ipcmo_attparam_idefault,len,icscode)
      if(len.lt.natts) call mmincblk( 'cmo_attparam_idefault',
     *      cmo_name,ipcmo_attparam_idefault,20,icscode)
      call mmfindbk( 'cmo_attparam_rdefault',cmo_name,
     *      ipcmo_attparam_rdefault,len,icscode)
      if(len.lt.natts) call mmincblk( 'cmo_attparam_rdefault',
     *      cmo_name,ipcmo_attparam_rdefault,20,icscode)
      call mmfindbk( 'cmo_attparam_cdefault',cmo_name,
     *      ipcmo_attparam_cdefault,len,icscode)
      if(len.lt.natts) call mmincblk( 'cmo_attparam_cdefault',
     *      cmo_name,ipcmo_attparam_cdefault,20,icscode)
      do i=1,number_of_default_attparam_name
            if(defcmo_attparam_names(i).eq.'name') posname=i
            if(defcmo_attparam_names(i).eq.'type') postype=i
            if(defcmo_attparam_names(i).eq.'rank') posrank=i
            if(defcmo_attparam_names(i).eq.'length') poslen=i
            if(defcmo_attparam_names(i).eq.'interpolation') posint=i
            if(defcmo_attparam_names(i).eq.'persistence') pospers=i
            if(defcmo_attparam_names(i).eq.'ioflag') posio=i
      enddo
      cmo_attlist(number_of_params_per_att*(natts-1)+posname)=att_name
      cmo_attlist(number_of_params_per_att*(natts-1)+postype)=cmsgin(5)
      cmo_attlist(number_of_params_per_att*(natts-1)+posrank)=cmsgin(6)
      cmo_attlist(number_of_params_per_att*(natts-1)+poslen)=cmsgin(7)
      cmo_attlist(number_of_params_per_att*(natts-1)+posint)=cmsgin(8)
      cmo_attlist(number_of_params_per_att*(natts-1)+pospers)=
     *    cmsgin(9)
      cmo_attlist(number_of_params_per_att*(natts-1)+posio)=cmsgin(10)
C
C....    Set up the Memory Managed Arrays for the new Attribute.
C
      ctype=cmsgin(5)
      clength=cmsgin(7)
      crank=cmsgin(6)
      lentype=icharlnf(ctype)
C
      if(ctype(1:lentype).eq.'VINT') then
C
         call cmo_get_info(clength,cmo_name,length,lout,itype,
     *                          ierror_return)
         call cmo_get_info(crank,cmo_name,irank,lout,itype,
     *                          ierror_return)
C
         mmlength=max(irank*length,1)
C
         call mmgetblk(att_name,
     *                    cmo_name,
     *                    ipcmo_pointer,mmlength,
     *                    1,ier)
         idefault=imsgin(11)
         if(msgtype(11).eq.2) idefault=nint(xmsgin(11))
C
         if(ier.ne.0) then
               call cmo_mm_error('cmo_addatt_cmo')
         else
               do j=1,mmlength
                  icmo_pointer(j)=idefault
               enddo
         endif
         cmo_attparam_idefault(natts)=idefault
         cmo_attparam_rdefault(natts)=idefault
 
C
      elseif(ctype(1:lentype).eq.'VDOUBLE') then
C
         call cmo_get_info(clength,cmo_name,length,lout,itype,
     *                          ierror_return)
         call cmo_get_info(crank,cmo_name,irank,lout,itype,
     *                          ierror_return)
C
         mmlength=max(irank*length,1)
C
         call mmgetblk(att_name,
     *                    cmo_name,
     *                    ipcmo_pointer,mmlength,
     *                    2,ier)
         xdefault=xmsgin(11)
         if(msgtype(11).eq.1) xdefault=imsgin(11)
C
         if(ier.ne.0) then
               call cmo_mm_error('cmo_addatt_cmo')
         else
               do j=1,length*irank
                  xcmo_pointer(j)=xdefault
               enddo
         endif
         cmo_attparam_rdefault(natts)=xdefault
         cmo_attparam_idefault(natts)=nint(xdefault)
c
      elseif(ctype(1:lentype).eq.'VCHAR') then
C
         call cmo_get_info(clength,cmo_name,length,lout,itype,
     *                          ierror_return)
         call cmo_get_info(crank,cmo_name,irank,lout,itype,
     *                          ierror_return)
         mmlength=max(irank*length,1)
         call mmgetblk(att_name,
     *                    cmo_name,
     *                    ipcmo_pointer,mmlength,
     *                    3,ier)
         cdefault=cmsgin(11)
C
         if(ier.ne.0) then
               call cmo_mm_error('cmo_addatt_cmo')
         else
 
               do j=1,length*irank
                  ccmo_pointer(j)=cdefault
               enddo
         endif
         cmo_attparam_cdefault(natts)=cdefault
C
      elseif(ctype(1:lentype).eq.'INT') then
         idefault=imsgin(11)
         if(msgtype(11).eq.2) idefault=nint(xmsgin(11))
         cmo_attparam_idefault(natts)=idefault
         cmo_attparam_rdefault(natts)=idefault
      elseif(ctype(1:lentype).eq.'REAL') then
         xdefault=xmsgin(11)
         if(msgtype(11).eq.1) xdefault=imsgin(11)
         cmo_attparam_rdefault(natts)=xdefault
         cmo_attparam_idefault(natts)=nint(xdefault)
      elseif(ctype(1:lentype).eq.'CHARACTER') then
         cmo_attparam_cdefault(natts)=cmsgin(11)
C
C
      else
C
C....       Unsupported Type.
C
            ierror_return=1
C
            write(logmess,9060) cmo_name(1:icharlnf(cmo_name)),
     *                          ctype
            call writloga('default',0,logmess,0,ierr)
 9060       format('CMO_ADDATT error: Unsupported Type:',a,a)
C
C
      endif
C
 9998 continue
 9999 return
      end
