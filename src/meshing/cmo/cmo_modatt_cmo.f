      subroutine cmo_modatt_cmo(imsgin,xmsgin,cmsgin,msgtype,
     *                          nwds,ierror_return)
C
C
C#######################################################################
C
C      PURPOSE -
C
C         This Routine Modifies Attribute Fields in a Mesh Object.
C
C      INPUT ARGUMENTS -
C
C         icmo_index - (integer) Index of the Mesh Object.
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
C         $Log: cmo_modatt_cmo.f,v $
C         Revision 2.00  2007/11/05 19:45:49  spchu
C         Import to CVS
C
CPVCS    
CPVCS       Rev 1.4   10 Apr 2001 11:04:18   dcg
CPVCS    shorten too long name
CPVCS
CPVCS       Rev 1.3   Tue Mar 14 16:08:34 2000   dcg
CPVCS    fix typo on cdefault
CPVCS
CPVCS       Rev 1.2   13 Mar 2000 17:01:32   dcg
CPVCS    fix missing replacement statement for modifying the rank of the attribute
CPVCS
CPVCS       Rev 1.0   Thu Jan 20 14:51:00 2000   dcg
CPVCS    Initial revision.
CPVCS
CPVCS       Rev 1.8   Tue Aug 17 11:06:44 1999   dcg
CPVCS    look for special names imt,itp, icr change to imt1,
CPVCS    itp1,icr1
CPVCS
CPVCS       Rev 1.7   Mon Apr 14 16:41:36 1997   pvcs
CPVCS    No change.
CPVCS
CPVCS       Rev 1.6   Wed Mar 06 12:56:34 1996   dcg
CPVCS    modify memory allocation if length changes
CPVCS
CPVCS       Rev 1.5   Tue Mar 05 17:32:50 1996   dcg
CPVCS    call mmnewlen if rank is changed
CPVCS
CPVCS       Rev 1.4   09/29/95 09:12:54   het
CPVCS    Put in added attributes inheritance
CPVCS
CPVCS       Rev 1.3   09/14/95 16:38:48   dcg
CPVCS    remove dependencies on mesh_object.h data structures
CPVCS
CPVCS       Rev 1.2   09/11/95 14:44:24   het
CPVCS    Change to the storage block based CMO stuff.
CPVCS
CPVCS       Rev 1.1   03/15/95 15:23:30   ejl
CPVCS    Finished installing the defaults.
CPVCS
CPVCS       Rev 1.0   02/16/95 13:53:34   dcg
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
      integer nwds, imsgin(nwds), msgtype(nwds)
      REAL*8 xmsgin(nwds)
      character*32 cmsgin(nwds)
C
      integer ierror_return
C
C#######################################################################
C
C     LOCAL VARIABLE DEFINITION
C
      integer ierr,  icscode,iatt,i,len,natts,
     *  lenname,icmo_index,ierror,posname,postype,posrank,
     *  posint,posio,pospers,irank,itype,length,mmlength,
     *  poslen
C
      character*32 cmo_name, att_name, crank, clength
 
C
      character*132 logmess
      character*16 partname
      integer icharlnf
      pointer (ip,ip1)
      real*8 ip1(*)
C
C#######################################################################
C
      partname='define_cmo_lg'
      cmo_name=cmsgin(3)
      att_name=cmsgin(4)
      lenname=icharlnf(att_name)
C look for and modify special names
      if(att_name(1:lenname).eq.'imt') att_name='imt1'
      if(att_name(1:lenname).eq.'itp') att_name='itp1'
      if(att_name(1:lenname).eq.'icr') att_name='icr1'
      if(att_name(1:lenname).eq.'isn') att_name='isn1'
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
      write(logmess,'(a)')
     *         'CMO_MODATT error: attribute does not exist: '
      call writloga('default',0,logmess,0,ierr)
C
      write(logmess,'(a,a)') cmsgin(3), cmsgin(4)
      call writloga('default',0,logmess,0,ierr)
      go to 9999
C
c   found attribute
c.... Find postions of name, type, rank and length
c
 50   call mmfindbk( 'defcmo_attparam_names',partname,
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
C
      if((cmsgin(5)(1:4) .eq. 'type') .or.
     *      (cmsgin(5)(1:4) .eq. 'TYPE')) then
C
C....       Modify the TYPE field.
C
        if(nwds.ge.6.and.msgtype(6).eq.3) then
          cmo_attlist(number_of_params_per_att*(iatt-1)+postype)=
     *         cmsgin(6)
          ierror_return=0
        endif
C
      elseif((cmsgin(5)(1:4) .eq. 'rank') .or.
     *          (cmsgin(5)(1:4) .eq. 'RANK')) then
C
C....       Modify the RANK field.
C
        if(nwds.ge.6.and.msgtype(6).eq.3) then
          cmo_attlist(number_of_params_per_att*(iatt-1)+posrank)=
     *         cmsgin(6)
          call cmo_get_info(cmsgin(6),cmo_name,irank,len,itype,ierr)
          clength=cmo_attlist(number_of_params_per_att*(iatt-1)+poslen)
          call cmo_get_info(clength,cmo_name,length,len,itype,ierr)
          mmlength=max(irank*length,1)
          call mmnewlen(att_name,cmo_name,ip,mmlength,ierr)
        endif
        ierror_return=0
C
      elseif((cmsgin(5)(1:6) .eq. 'length') .or.
     *          (cmsgin(5)(1:6) .eq. 'LENGTH')) then
C
C....       Modify the LENGTH field.
C
        if(nwds.ge.6.and.msgtype(6).eq.3) then
          cmo_attlist(number_of_params_per_att*(iatt-1)+poslen)=
     *         cmsgin(6)
          call cmo_get_info(cmsgin(6),cmo_name,length,len,itype,ierr)
          crank=cmo_attlist(number_of_params_per_att*(iatt-1)+posrank)
          call cmo_get_info(crank,cmo_name,irank,len,itype,ierr)
          mmlength=max(irank*length,1)
          call mmnewlen(att_name,cmo_name,ip,mmlength,ierr)
        endif
        ierror_return=0
C
      elseif((cmsgin(5)(1:6) .eq. 'interp') .or.
     *          (cmsgin(5)(1:6) .eq. 'INTERP')) then
C
C....       Modify the INTERPOLATION field.
C
        if(nwds.ge.6.and.msgtype(6).eq.3) then
           cmo_attlist(number_of_params_per_att*(iatt-1)+posint)=
     *         cmsgin(6)
        endif
        ierror_return=0
C
      elseif((cmsgin(5)(1:7) .eq. 'persist') .or.
     *          (cmsgin(5)(1:7) .eq. 'PERSIST')) then
C
C....       Modify the PERSISTENCE field.
C
        if(nwds.ge.6.and.msgtype(6).eq.3) then
           cmo_attlist(number_of_params_per_att*(iatt-1)+pospers)=
     *         cmsgin(6)
        endif
        ierror_return=0
C
      elseif((cmsgin(5)(1:6) .eq. 'ioflag') .or.
     *          (cmsgin(5)(1:6) .eq. 'IOFLAG')) then
C
C....       Modify the IO field.
C
        if(nwds.ge.6.and.msgtype(6).eq.3) then
           cmo_attlist(number_of_params_per_att*(iatt-1)+posio)=
     *         cmsgin(6)
        endif
        ierror_return=0
C
      elseif((cmsgin(5)(1:7) .eq. 'default') .or.
     *          (cmsgin(5)(1:7) .eq. 'DEFAULT')) then
C
C....       Modify the VALUE field.
C
        if(msgtype(6).eq.1) then
           call mmfindbk( 'cmo_attparam_idefault',cmo_name,
     *      ipcmo_attparam_idefault,len,icscode)
           cmo_attparam_idefault(iatt)=imsgin(6)
        elseif(msgtype(6).eq.2) then
           call mmfindbk( 'cmo_attparam_rdefault',cmo_name,
     *      ipcmo_attparam_rdefault,len,icscode)
           cmo_attparam_rdefault(iatt)=xmsgin(6)
        elseif(msgtype(6).eq.3) then
           call mmfindbk( 'cmo_attparam_cdefault',cmo_name,
     *      ipcmo_attparam_cdefault,len,icscode)
           cmo_attparam_cdefault(iatt)=cmsgin(6)
        endif
        ierror_return=0
 
C
      else
C
C....       The field does not exist.
C
            ierror_return=-1
C
            write(logmess,'(a)')
     *            'CMO_MODATT error: field does not exist: '
            call writloga('default',0,logmess,0,ierr)
C
            write(logmess,'(a,a,a)') cmsgin(3), cmsgin(4), cmsgin(5)
            call writloga('default',0,logmess,0,ierr)
C
      endif
C
9999  return
      end
