      subroutine cmo_get_intinfo(ioption,cmo_name,
     *                        iout,lout,itype,ierror_return)
C
C
C#######################################################################
C
C     PURPOSE -
C
C        This routine returns Mesh Object information.
C        Note difference that cmo_get_intinfo() iout is integer
C        Note difference that cmo_get_info() iout is real*8 pointer
C
C     INPUT ARGUMENTS -
C
C        ioption   - (character) The option to be performed.
C        cmo_name  - (character) Name of the Mesh Object.
C
C     OUTPUT ARGUMENTS -
C
C        iout    - (integer)  data to be returned.
C        lout    - (integer)  length of the data to be returned.
C        itype   - (integer) type of the data to be returned 
C                  (INT=1 or REAL=2) 
C                   Otherwise -1 for CHARACTER, VINT, VDOUBLE, VCHAR
C 
C        ierror_return - Error Return Code (==0 ==> OK, <>0 ==> Error).
C
C     CHANGE HISTORY -
C
C        $Log: cmo_get_intinfo.f,v $
C        Revision 2.00  2007/11/05 19:45:48  spchu
C        Import to CVS
C
CPVCS    
CPVCS       Rev 1.4   25 May 2000 15:46:50   dcg
CPVCS    remove warning message
CPVCS
CPVCS       Rev 1.3   26 Apr 2000 08:14:24   dcg
CPVCS    set error flag to zero for number_or_attributes option
CPVCS
CPVCS       Rev 1.2   Tue Feb 15 11:20:24 2000   dcg
CPVCS    remove unneeded test
CPVCS
CPVCS       Rev 1.1   Tue Feb 15 10:48:04 2000   dcg
CPVCS
CPVCS       Rev 1.0   Tue Feb 15 08:37:46 2000   dcg
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
C     PARAMETERS

      character*(*) ioption, cmo_name
      integer  iout, lout, itype, ierror_return
C
C#######################################################################
C
C     LOCAL VARIABLE DEFINITION
C
      integer i, len, ierr, icscode,length,icmo_index, natts
C
      character*32 partname
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
     *  'CMO_GET_INTINFO: Mesh Object does not exist: ',cmo_name
         call writloga('default',0,logmess,0,ierr)
C
      else
c
c.... check if want number_of_attributes
c
         if(ioption.eq.'number_of_attributes') then
            partname='define_cmo_lg'
            call mmfindbk('cmo_natts',partname,ipcmo_natts,
     *        len,icscode)
            call cmo_get_index(cmo_name,i,icscode)
            iout=cmo_natts(i)
            ierror_return=0
            go to 9999
         endif
C
C        Loop through the attributes look for matching attribute
C           name - then check type
C
            partname='define_cmo_lg'
            call mmfindbk('cmo_natts',partname,ipcmo_natts,len,
     *                   icscode)
            partname=cmo_name
            call mmfindbk('cmo_attlist',cmo_name,ipcmo_attlist,
     *                   len,icscode)
            call cmo_get_index(cmo_name,icmo_index,ierror_return)
            if(ierror_return.ne.0) go to 9998
            natts=cmo_natts(icmo_index)
            do i=1,natts
               if(cmo_attlist(number_of_params_per_att*(i-1)+1)
     *             .eq.ioption) then
                  if(cmo_attlist(number_of_params_per_att*(i-1)+2)
     *               .eq.'INT') then
                     itype=1
                     lout=1
                     call mmfindbk('cmo_attparam_idefault'
     *                ,cmo_name,ipcmo_attparam_idefault,
     *                length,icscode)
                     iout=cmo_attparam_idefault(i)
                     go to 9999
                  elseif(cmo_attlist(number_of_params_per_att*
     *               (i-1)+2).eq.'REAL') then
                     itype=1
                     lout=1
                     call mmfindbk('cmo_attparam_rdefault'
     *                ,cmo_name,ipcmo_attparam_rdefault,
     *                length,icscode)
                     iout=nint(cmo_attparam_rdefault(i))
                     go to 9999
                  else
                     ierror_return=-1
                     go to 9998
                  endif
               endif
            enddo
            ierror_return=-1
            go to 9998
C
 9998    if(ierror_return.ne.0) then
            itype=-1
            lout=-1
c              write(logmess,9000) cmo_name,ioption
c              call writloga('default',0,logmess,0,ierr)
 9000       format('Illegal CMO_GET_INTINFO option: ',2a32)
         endif
C
      endif
C
 9999 return
      end
