      subroutine cmo_set_attinfo(ioption,cmo_name,idata,rdata,cdata,
     *                        type_data,
     *                        ierror_return)
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
C        ioption    - (character) The option to be performed.
C        cmo_name   - (character) Name of the Mesh Object.
C        idata       - The data to be set. (if integer)
C        rdata       -  (real)
C        cdata       -  (character
C        type_data  - The type of the data to be set (I=1,R=2,C=3).
C
C     OUTPUT ARGUMENTS -
C
C        ierror_return - Error Return Code (==0 ==> OK, <>0 ==> Error)
C
C     CHANGE HISTORY -
C
C        $Log: cmo_set_attinfo.f,v $
C        Revision 2.00  2007/11/05 19:45:50  spchu
C        Import to CVS
C
CPVCS    
CPVCS       Rev 1.3   Thu Feb 17 15:15:44 2000   dcg
CPVCS    use character*(*)
CPVCS
CPVCS       Rev 1.2   Mon Feb 14 08:48:16 2000   dcg
CPVCS    check for VINT VDOUBLE VCHAR types
CPVCS    use character*32 for arguments - because of compiler problems
CPVCS
CPVCS       Rev 1.1   28 Jan 2000 15:57:10   dcg
 
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
      character*(*) ioption, cmo_name
C
      integer idata,  type_data
C
      integer ierror_return
C
C#######################################################################
C
C     LOCAL VARIABLE DEFINITION
C
      integer len, i, icmo_index, natts,length,ierr,icscode
C
      character*132 logmess
      character*32 partname,cdata
      real*8 rdata
C
C#######################################################################
C
      integer icharlnf
C
C#######################################################################
C
C
      len=icharlnf(cmo_name)
C
      if((cmo_name(1:len).eq.'-cmo-') .or.
     *   (cmo_name(1:len).eq.'-default-') .or.
     *   (cmo_name(1:len).eq.'-def-')) then
C
C....    Use the Current Mesh Object.
C
         call cmo_get_name(cmo_name,ierror_return)
C
      endif
C
C.... Search table for Mesh Object.
C
      call cmo_get_index(cmo_name,icmo_index,ierror_return)
C
      if(icmo_index.le.0) then
C
C....    Mesh Object does not exist.
C
         ierror_return=-1
         write(logmess,9010) cmo_name
         call writloga('default',0,logmess,0,ierr)
 9010    format('CMO_SET_INFO error:  Mesh Object does not exist: ',a32)
C
      else
C
         partname='define_cmo_lg'
         call mmfindbk('cmo_names',partname,ipcmo_names,len,
     *                   icscode)
         call mmfindbk('cmo_natts',partname,ipcmo_natts,len,
     *                   icscode)
         partname='default_cmo_lg'
         call mmfindbk('cmo_attlist',cmo_name,ipcmo_attlist,
     *                   len,icscode)
         call cmo_get_index(cmo_name,icmo_index,ierror_return)
         if(ierror_return.ne.0) go to 9998
         natts=cmo_natts(icmo_index)
         do i=1,natts
            if(cmo_attlist(number_of_params_per_att*(i-1)+1)
     *             .eq.ioption) then
               if((cmo_attlist(number_of_params_per_att*(i-1)+2)
     *               .eq.'INT') .or.
     *            (cmo_attlist(number_of_params_per_att*(i-1)+2)
     *               .eq.'VINT')) then
                   call mmfindbk('cmo_attparam_idefault'
     *                ,cmo_name,ipcmo_attparam_idefault,
     *                length,icscode)
                   cmo_attparam_idefault(i)=idata
                   go to 9999
               elseif((cmo_attlist(number_of_params_per_att*
     *               (i-1)+2).eq.'REAL') .or.
     *            (cmo_attlist(number_of_params_per_att*(i-1)+2)
     *               .eq.'VDOUBLE')) then
                   call mmfindbk('cmo_attparam_rdefault'
     *                ,cmo_name,ipcmo_attparam_rdefault,
     *                length,icscode)
                   cmo_attparam_rdefault(i) = rdata
                   go to 9999
               elseif((cmo_attlist(number_of_params_per_att*
     *               (i-1)+2).eq.'CHARACTER') .or.
     *            (cmo_attlist(number_of_params_per_att*(i-1)+2)
     *               .eq.'VCHAR')) then
                   call mmfindbk('cmo_attparam_cdefault'
     *                ,cmo_name,ipcmo_attparam_cdefault,
     *                length,icscode)
                   cmo_attparam_cdefault(i) = cdata
                   go to 9999
               else
                   ierror_return=-1
                   go to 9998
               endif
            endif
         enddo
9998     if(ierror_return.ne.0) then
            call x3d_error('cmo_set_info','attribute not found')
         endif
C
      endif
C
9999  return
      end
 
