      subroutine cmo_set_attparam(att_name,cmo_name,index,ctype,crank,
     *    clen,cinter,cpers,cio,ierror_return)
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
C        att_name   - (character) The attribute to find
C        cmo_name  - (character) Name of the Mesh Object.
C        ctype         - type
C        crank         -  rank
C        clen          -  length
C        cinter        -  interpolation
C        cpers         - persistence
C        cio           - ioflag
c
C     OUTPUT ARGUMENT
c
C        index         - attribute number
C        ierror_return - Error Return Code (==0 ==> OK, <>0 ==> Error).
C
C     CHANGE HISTORY -
C
C        $Log: cmo_set_attparam.f,v $
C        Revision 2.00  2007/11/05 19:45:50  spchu
C        Import to CVS
C
CPVCS    
CPVCS       Rev 1.2   10 Apr 2001 11:04:20   dcg
CPVCS    shorten too long name
CPVCS
CPVCS       Rev 1.1   Wed Apr 05 13:34:14 2000   nnc
CPVCS    Minor source modifications required by the Absoft compiler.
CPVCS
CPVCS       Rev 1.0   Wed Feb 09 10:16:38 2000   dcg
CPVCS    Initial revision.
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
      character*(*) att_name, cmo_name
      character*32 partname,ctype,cpers,cio,clen,crank,cinter,name
      integer posname,postype,posrank,posio,poslen,posint,pospers,
     *  len1,len2,icharlnf
C
      integer  i,natts
C
      integer ierror_return
C
C#######################################################################
C
C     LOCAL VARIABLE DEFINITION
C
      integer len,ierr,icscode,icmo_index,index
C
      character*132 logmess
 
C
C#######################################################################
C
      if((cmo_name.eq.'-cmo-') .or.
     *   (cmo_name.eq.'-default-') .or.
     *   (cmo_name.eq.'-def-')) then
C
C....    Use the Current Mesh Object.
C
         call cmo_get_name(cmo_name,ierror_return)
C
      endif
C
C
C.... Search table for Mesh Object.
C
      call cmo_exist(cmo_name,icscode)
C
C
      if(icscode.ne.0) then
C
         ierror_return=-1
C
         write(logmess,'(a,a)')
     *            'CMO_GET_INFO: Mesh Object does not exist: ', cmo_name
         call writloga('default',0,logmess,0,ierr)
C
      else
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
C
C        Loop through the attributes look for matching attribute
C           name - then check type
C
         partname='define_cmo_lg'
         call mmfindbk('cmo_names',partname,ipcmo_names,len,
     *                   icscode)
         call mmfindbk('cmo_natts',partname,ipcmo_natts,len,
     *                   icscode)
         partname=cmo_name
         call mmfindbk('cmo_attlist',cmo_name,ipcmo_attlist,
     *                   len,icscode)
         call cmo_get_index(cmo_name,icmo_index,ierror_return)
         if(ierror_return.ne.0) go to 9998
         natts=cmo_natts(icmo_index)
         do i=1,natts
            name=cmo_attlist(number_of_params_per_att*(i-1)+posname)
            len1=icharlnf(name)
            len2=icharlnf(att_name)
            if(name(1:len1)
     *             .eq.att_name(1:len2)) then
              index=i
              cmo_attlist(number_of_params_per_att*(i-1)+postype)=ctype
              cmo_attlist(number_of_params_per_att*(i-1)+posrank)=crank
              cmo_attlist(number_of_params_per_att*(i-1)+poslen)=clen
              cmo_attlist(number_of_params_per_att*(i-1)+posint)=cinter
              cmo_attlist(number_of_params_per_att*(i-1)+pospers)=cpers
              cmo_attlist(number_of_params_per_att*(i-1)+posio)=cio
              ierror_return=0
              go to 9999
            endif
         enddo
         ierror_return=1
C
 9998    if(ierror_return.ne.0) then
               write(logmess,9000) cmo_name,att_name
               call writloga('default',0,logmess,0,ierr)
 9000          format('Illegal CMO_SET_ATTINFO option: ',2a32)
         endif
C
      endif
C
 9999 return
      end
