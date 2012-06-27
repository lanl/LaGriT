      subroutine cmo_get_info(ioption,cmo_name,
     *                        ipout,lout,itype,ierror_return)
C
C
C#######################################################################
C
C     PURPOSE - 
C        This routine returns Mesh Object information.
C        Note difference that cmo_get_intinfo() iout is integer
C        Note difference that cmo_get_info() ipout is real*8 pointer 
C
C     INPUT ARGUMENTS -
C
C        ioption   - (character) The option to be performed.
C        cmo_name  - (character) Name of the Mesh Object.
C
C     OUTPUT ARGUMENTS -
C        (TAM - changed iout to ipout and stuff to out)
C
C        ipout   - pointer(ipout,out) 
C                  real*8 out(*)
C                  The data to be returned.
C        lout   - (integer) The length of the data to be returned.
C
C        itype -   The type of the data to be returned (I=1,R=2,C=3).
C           Note what actually happens in code:
C                  (INT=1 or REAL=2)
C                  Otherwise 4 for CHARACTER, VINT, VDOUBLE, VCHAR
C
C        ierror_return - Error Return Code (==0 ==> OK, <>0 ==> Error).
C
C
C     CHANGE HISTORY -
C        April 2000 Replaced all use of cmo_get_i() with cmo_get_info() 
C
C        $Log: cmo_get_info.f,v $
C        Revision 2.00  2007/11/05 19:45:48  spchu
C        Import to CVS
C
CPVCS    
CPVCS       Rev 1.8   08 Jun 2000 07:58:50   dcg
CPVCS    set type to 4 for pointers
CPVCS
CPVCS       Rev 1.7   03 May 2000 12:54:42   dcg
CPVCS    return actual length and type for mesh attribute arrays
CPVCS
CPVCS       Rev 1.6   26 Apr 2000 08:12:28   dcg
CPVCS    see error flag to zero for number_of_attributes call.
CPVCS
CPVCS       Rev 1.5   Wed Apr 05 13:50:22 2000   dcg
CPVCS    comment out error statement about illegal option
CPVCS
CPVCS       Rev 1.4   Tue Feb 15 08:16:04 2000   dcg
CPVCS    declare iout as pointer not integer
CPVCS
CPVCS       Rev 1.3   26 Jan 2000 14:40:06   dcg
CPVCS
CPVCS       Rev 1.19   Tue Oct 06 16:48:40 1998   dcg
CPVCS    make equivalent node attributes imt,imt1, itp,itp1,
CPVCS    icr,icr1,...
CPVCS
CPVCS       Rev 1.18   Wed Dec 17 11:25:10 1997   dcg
CPVCS    declare iout as a pointe
CPVCS
CPVCS       Rev 1.17   Mon Apr 14 16:41:12 1997   pvcs
CPVCS    No change.
CPVCS
CPVCS       Rev 1.16   Wed Mar 06 16:44:04 1996   dcg
CPVCS    print error messages if idebug=1
CPVCS
CPVCS       Rev 1.15   09/14/95 12:09:18   dcg
CPVCS    replace character literals in call argument lists
CPVCS
CPVCS       Rev 1.14   09/13/95 14:32:16   het
CPVCS    Correct an error
CPVCS
CPVCS       Rev 1.13   09/11/95 14:44:00   het
CPVCS    Change to the storage block based CMO stuff.
CPVCS
CPVCS       Rev 1.12   08/30/95 21:08:46   het
CPVCS    Put cmo table data into the cmoatt storage block
CPVCS
CPVCS       Rev 1.11   05/22/95 15:28:24   ejl
CPVCS    Added nfaces and nedges.
CPVCS
CPVCS       Rev 1.10   03/15/95 15:22:58   ejl
CPVCS    Finished installing the defaults.
CPVCS
CPVCS       Rev 1.9   02/16/95 09:56:14   ejl
CPVCS    Fixed bugs, fixed hole in the Create command.
CPVCS    Added commands MODATT, LENGTH, MEMORY, & COMPRESS.
CPVCS
CPVCS       Rev 1.8   02/10/95 14:07:24   ejl
CPVCS    Fix bugs left from last update
CPVCS
CPVCS       Rev 1.6   01/30/95 06:22:12   het
CPVCS    Fix several cmo errors
CPVCS
CPVCS       Rev 1.5   01/24/95 08:52:42   het
CPVCS    Add error checking to the cmo routines.
CPVCS
CPVCS
CPVCS       Rev 1.4   01/04/95 22:01:34   llt
CPVCS    unicos changes (made by het)
CPVCS
CPVCS       Rev 1.3   12/09/94 22:50:58   het
CPVCS    Made changes to support the new cmo_ routines.
CPVCS
CPVCS
CPVCS       Rev 1.2   12/01/94 18:58:44   het
CPVCS    Added a data variable type to the call.
CPVCS
CPVCS
CPVCS       Rev 1.1   11/28/94 14:14:44   het
CPVCS    Add the "mbndry" option.
CPVCS
CPVCS       Rev 1.0   11/14/94 12:04:50   het
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
C     PARAMETERS

      character*(*) ioption, cmo_name
      pointer(ipout, out)
      real*8 out(*)
      integer  lout, itype, ierror_return
C
C#######################################################################
C
C     LOCAL VARIABLE DEFINITION
C
      integer i, len, ierr, icscode,length,icmo_index, natts
C
      character*32 partname,ioptfind
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
     *   'CMO_GET_INFO: Mesh Object does not exist: ', cmo_name
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
            ipout=cmo_natts(i)
            ierror_return=0
            go to 9999
         endif
C
C....    Check to see if this is a Mesh Object memory managed array.
C
 
          ioptfind=ioption
          if(ioption.eq.'imt') ioptfind='imt1'
          if(ioption.eq.'icr') ioptfind='icr1'
          if(ioption.eq.'itp') ioptfind='itp1'
          if(ioption.eq.'isn') ioptfind='isn1'
          call mmfindbk(ioptfind,cmo_name,ipout,lout,icscode)
C
C....    If it is not a memory array then check to see if it is
C           a scalar variable in the Mesh Object storage block.
C        Loop through the attributes look for matching attribute
C           name - then check type
C
         if(icscode.ne.0) then
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
                     ipout=cmo_attparam_idefault(i)
                     go to 9999
                  elseif(cmo_attlist(number_of_params_per_att*
     *               (i-1)+2).eq.'REAL') then
                     itype=1
                     lout=1
                     call mmfindbk('cmo_attparam_rdefault'
     *                ,cmo_name,ipcmo_attparam_rdefault,
     *                length,icscode)
                     ipout=nint(cmo_attparam_rdefault(i))
                     go to 9999
                  else
                     ierror_return=-1
                     go to 9998
                  endif
               endif
            enddo
            ierror_return=-1
            go to 9998
         else
         itype=4
            ierror_return=0
         endif
C
 9998    if(ierror_return.ne.0) then
            itype=-1
            lout=-1
c              write(logmess,9000) cmo_name,ioption
c              call writloga('default',0,logmess,0,ierr)
 9000          format('Illegal CMO_GET_INFO option: ',2a32)
         endif
C
      endif
C
 9999 return
      end
