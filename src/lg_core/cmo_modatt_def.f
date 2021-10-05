      subroutine cmo_modatt_def(imsgin,xmsgin,cmsgin,msgtype,nwds,
     *                      ierror_return)
C
C
C#######################################################################
C
C      PURPOSE -
C
C         This Routine Modifies Attribute Fields in the Default Table.
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
C         $Log: cmo_modatt_def.f,v $
C         Revision 2.00  2007/11/05 19:45:49  spchu
C         Import to CVS
C
CPVCS    
CPVCS       Rev 1.1   10 Apr 2001 11:04:18   dcg
CPVCS    shorten too long name
CPVCS
CPVCS       Rev 1.0   Thu Jan 20 14:51:02 2000   dcg
CPVCS    Initial revision.
CPVCS
CPVCS       Rev 1.5   Mon Apr 14 16:41:38 1997   pvcs
CPVCS    No change.
CPVCS
CPVCS       Rev 1.4   09/29/95 09:12:56   het
CPVCS    Put in added attributes inheritance
CPVCS
CPVCS       Rev 1.3   09/14/95 12:09:46   dcg
CPVCS    replace character literals in call argument lists
CPVCS
CPVCS       Rev 1.2   09/11/95 14:44:26   het
CPVCS    Change to the storage block based CMO stuff.
CPVCS
CPVCS       Rev 1.1   03/15/95 15:23:26   ejl
CPVCS    Finished installing the defaults.
CPVCS
CPVCS       Rev 1.0   02/16/95 13:53:40   dcg
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
      integer ierr, icscode,i,lenattlist,iatt,len,posname,postype,
     *  poslen,posrank,posio,posint,pospers
C
      character*32 cmo_name, att_name
C
      character*132 logmess
      character*32 partname
C
C#######################################################################
C
C.... Extract the CMO-name and the attribute-name from the message.
C
      cmo_name=cmsgin(3)
      att_name=cmsgin(4)
      partname='default_cmo_lg'
C
C.... Search table for Attribute.
C
      call mmfindbk('cmo_attlist',partname,ipcmo_attlist,
     *                   lenattlist,icscode)
      do i=1,number_of_default_attributes
         if(cmo_attlist(number_of_params_per_att*(i-1)+1)
     *             .eq.att_name) then
           iatt=i
           go to 50
         endif
      enddo
C
C....    The attribute does not exist.
C
      ierror_return=-1
      write(logmess,'(a,a)')
     *         'CMO_MODATT error: attribute does not exist: ', cmsgin(4)
      call writloga('default',0,logmess,0,ierr)
      go to 9999
C
c.... Find postions of name, type, rank and length
c
 50   partname='define_cmo_lg'
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
      if((cmsgin(5)(1:4) .eq. 'type') .or.
     *      (cmsgin(5)(1:4) .eq. 'TYPE')) then
C
 
C....       Modify the TYPE field.
C
        if(nwds.ge.6.and.msgtype(6).eq.3) then
          cmo_attlist(number_of_params_per_att*(iatt-1)+postype)=
     *         cmsgin(6)
        endif
        ierror_return=0
C
      elseif((cmsgin(5)(1:4) .eq. 'rank') .or.
     *          (cmsgin(5)(1:4) .eq. 'RANK')) then
C
C....       Modify the TYPE field.
C
        if(nwds.ge.6.and.msgtype(6).eq.3) then
          cmo_attlist(number_of_params_per_att*(iatt-1)+posrank)=
     *         cmsgin(6)
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
         cmo_attlist(number_of_params_per_att*(iatt-1)+posint)=
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
        partname='default_cmo_lg'
        if(msgtype(6).eq.1) then
           call mmfindbk( 'cmo_attparam_idefault',partname,
     *      ipcmo_attparam_idefault,len,icscode)
           cmo_attparam_idefault(iatt)=imsgin(6)
        elseif(msgtype(6).eq.2) then
           call mmfindbk( 'cmo_attparam_rdefault',partname,
     *      ipcmo_attparam_rdefault,len,icscode)
           cmo_attparam_rdefault(iatt)=xmsgin(6)
        elseif(msgtype(6).eq.3) then
           call mmfindbk( 'cmo_attparam_rdefault',partname,
     *      ipcmo_attparam_cdefault,len,icscode)
           cmo_attparam_cdefault(iatt)=cmsgin(6)
        endif
        ierror_return=0
 
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
            write(logmess,'(a,a)') cmsgin(4), cmsgin(5)
            call writloga('default',0,logmess,0,ierr)
C
C
      endif
C
 9999 return
      end
