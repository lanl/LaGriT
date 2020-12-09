      subroutine cmo_get_attinfo(ioption,cmo_name,iout,rout,cout,
     *                        ipout,lout,itype,ierror_return)
C
C
C#######################################################################
C
C     PURPOSE -
C
C        This routine returns Mesh Object information.
C
C     INPUT ARGUMENTS -
C
C        ioption   - (character) The option to be performed.
C        cmo_name  - (character) Name of the Mesh Object.
C
C     OUTPUT ARGUMENTS -
C
C        iout    - The data to be returned (integer).
C        rout    -  (real)
C        cout    -  (character)
C        ipout   -  (real*8 pointer)
C        lout    - The length of the data to be returned.
C        itype   - The type of the data to be returned (I=1,R=2,C=3
C                                          pointer=4).
C        ierror_return - Error Return Code (==0 ==> OK, <>0 ==> Error).
C
C     CHANGE HISTORY -
C
C        $Log: cmo_get_attinfo.f,v $
C        Revision 2.00  2007/11/05 19:45:48  spchu
C        Import to CVS
C
CPVCS    
CPVCS       Rev 1.3   07 Nov 2000 15:51:50   dcg
CPVCS    comment out warning message
CPVCS
CPVCS       Rev 1.2   Mon Feb 07 11:14:14 2000   dcg
CPVCS
CPVCS       Rev 1.1   28 Jan 2000 15:57:46   dcg
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
C     PARAMATERS

      character*(*) ioption, cmo_name
      character*32 cout
      pointer (ipout,out)
      real*8 out(*)
      real*8 rout
      integer  iout, lout, itype, ierror_return
C
C
C#######################################################################
C
C     LOCAL VARIABLE DEFINITION
C
      integer i, len, ierr, icscode,length,icmo_index, natts
      integer ierror
C
      character*32 partname,ioptfind
      character*132 logmess
 
C
C#######################################################################
C
      iout=0
      rout=0.
      cout=' '
      lout=0
      itype=0
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
C....    Check to see if this is a Mesh Object memory managed array.
C
          ioptfind=ioption
          if(ioption.eq.'imt') ioptfind='imt1'
          if(ioption.eq.'icr') ioptfind='icr1'
          if(ioption.eq.'itp') ioptfind='itp1'
          if(ioption.eq.'isn') ioptfind='isn1'
c
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
                  elseif(cmo_attlist(number_of_params_per_att*(i-1)+2)
     *               .eq.'VINT') then
                     itype=4
                     call mmfindbk(ioption,cmo_name,ipout,length,ierror)
                     lout=length
                     call mmfindbk('cmo_attparam_idefault'
     *                ,cmo_name,ipcmo_attparam_idefault,
     *                length,icscode)
                     iout=cmo_attparam_idefault(i)
                     go to 9999
                  elseif(cmo_attlist(number_of_params_per_att*
     *               (i-1)+2).eq.'REAL') then
                     itype=2
                     lout=1
                     call mmfindbk('cmo_attparam_rdefault'
     *                ,cmo_name,ipcmo_attparam_rdefault,
     *                length,icscode)
                     rout=cmo_attparam_rdefault(i)
                     go to 9999
                  elseif(cmo_attlist(number_of_params_per_att*
     *               (i-1)+2).eq.'VDOUBLE') then
                     itype=4
                     call mmfindbk(ioption,cmo_name,ipout,length,ierror)
                     lout=length
                     call mmfindbk('cmo_attparam_rdefault'
     *                ,cmo_name,ipcmo_attparam_rdefault,
     *                length,icscode)
                     rout=cmo_attparam_rdefault(i)
                     go to 9999
                  elseif(cmo_attlist(number_of_params_per_att*
     *               (i-1)+2).eq.'CHARACTER') then
                     itype=3
                     lout=1
                     call mmfindbk('cmo_attparam_cdefault'
     *                ,cmo_name,ipcmo_attparam_cdefault,
     *                length,icscode)
                     cout=cmo_attparam_cdefault(i)
                     go to 9999
                  elseif(cmo_attlist(number_of_params_per_att*
     *               (i-1)+2).eq.'VCHAR') then
                     itype=4
                     call mmfindbk(ioption,cmo_name,ipout,length,ierror)
                     lout=length
                     call mmfindbk('cmo_attparam_cdefault'
     *                ,cmo_name,ipcmo_attparam_cdefault,
     *                length,icscode)
                     cout=cmo_attparam_cdefault(i)
                     go to 9999
                  else
                     ierror_return=-1
                     go to 9998
                  endif
               endif
         enddo
         ierror_return=1
 
C
 9998    if(ierror_return.ne.0) then
            itype=-1
            lout=-1
c              write(logmess,9000) cmo_name,ioption
c              call writloga('default',0,logmess,0,ierr)
 9000          format('Illegal CMO_GET_ATTINFO option: ',2a32)
         endif
C
      endif
C
 9999 return
      end
