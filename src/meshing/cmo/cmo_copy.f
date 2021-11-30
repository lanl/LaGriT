      subroutine cmo_copy(cmo_name1,cmo_name2,ierror_return)
C
C
C#######################################################################
C
C      PURPOSE -
C
C         This Routine creates an exact copy of a Mesh Object.
C
C      INPUT ARGUMENTS -
C
C         cmo_name1 - (character) Derived Mesh Object Name.
C         cmo_name2 - (character) Master Mesh Object Name.
C
C      OUTPUT ARGUMENTS -
C
C         ierror_return - Error Return Code (==0 ==> OK, <>0 ==> Error)
C
C      CHANGE HISTORY -
C
C         $Log: cmo_copy.f,v $
C         Revision 2.00  2007/11/05 19:45:48  spchu
C         Import to CVS
C
CPVCS    
CPVCS       Rev 1.6   10 Apr 2001 11:04:12   dcg
CPVCS    shorten too long name
CPVCS
CPVCS       Rev 1.5   13 Apr 2000 09:00:42   nnc
CPVCS    Eliminated multiple variable declarations.
CPVCS
CPVCS       Rev 1.4   Wed Apr 05 14:05:04 2000   dcg
CPVCS    use max of 1, rank*length for mmgetblk call - in case rank or
CPVCS    length is not set
CPVCS
CPVCS       Rev 1.3   Tue Feb 15 16:37:06 2000   dcg
CPVCS    fix lengths on loops
CPVCS    set defaults for all attributes
CPVCS
CPVCS       Rev 1.2   Tue Feb 01 13:43:14 2000   dcg
CPVCS
CPVCS       Rev 1.8   Mon Apr 14 16:40:04 1997   pvcs
CPVCS    No change.
CPVCS
CPVCS       Rev 1.7   Mon Nov 18 10:28:06 1996   dcg
CPVCS    remove charater literals from argument lists
CPVCS
CPVCS       Rev 1.6   Wed Jul 24 17:40:50 1996   dcg
CPVCS    copy number of nodes and number of elements
CPVCS
CPVCS       Rev 1.5   09/11/95 14:43:20   het
CPVCS    Change to the storage block based CMO stuff.
CPVCS
CPVCS       Rev 1.4   05/19/95 15:58:50   ejl
CPVCS    Fixed error with delete
CPVCS
CPVCS       Rev 1.3   03/15/95 15:22:30   ejl
CPVCS    Finished installing the defaults.
CPVCS
CPVCS       Rev 1.2   02/16/95 09:55:50   ejl
CPVCS    Fixed bugs, fixed hole in the Create command.
CPVCS    Added commands MODATT, LENGTH, MEMORY, & COMPRESS.
CPVCS
CPVCS       Rev 1.1   02/10/95 14:06:40   ejl
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
      character*(*) cmo_name1, cmo_name2
C
      integer ierror_return
C
C#######################################################################
C
C     LOCAL VARIABLE DEFINITION
C
      integer i, j, ier, ierr, len
C
      integer irank,mmlength,index,natts,length,itype,
     *  posname,postype,posrank,poslen,posint,posio,pospers
C
      character*132 logmess
C
      pointer (ipcmo_pointer1, icmo_pointer1)
      pointer (ipcmo_pointer1, xcmo_pointer1)
      pointer (ipcmo_pointer1, ccmo_pointer1)
      integer icmo_pointer1(*)
      REAL*8 xcmo_pointer1(*)
      character*32 ccmo_pointer1(*)
C
      pointer (ipcmo_pointer2, icmo_pointer2)
      pointer (ipcmo_pointer2, xcmo_pointer2)
      pointer (ipcmo_pointer2, ccmo_pointer2)
      pointer (ipcmo_attparam_cdefault1,cmo_attparam_cdefault1)
      pointer (ipcmo_attparam_idefault1,cmo_attparam_idefault1)
      pointer (ipcmo_attparam_rdefault1,cmo_attparam_rdefault1)
      integer cmo_attparam_idefault1(*)
      real*8  cmo_attparam_rdefault1(*)
      character*32  cmo_attparam_cdefault1(*)
      integer icmo_pointer2(*)
      REAL*8 xcmo_pointer2(*)
      character*32 ccmo_pointer2(*)
C
      integer icscode, iexist1, iexist2
C
      character*32 cname, ctype, crank, clength
      character*32  partname
C
C#######################################################################
C
      integer icharlnf
C
C#######################################################################
C
      len=icharlnf(cmo_name1)
C
      if((cmo_name1(1:len).eq.'-cmo-') .or.
     *   (cmo_name1(1:len).eq.'-def-')) then
C
C....    Use the Current Mesh Object as the Sink.
C
         call cmo_get_name(cmo_name1,ierror_return)
C
      endif
C
      len=icharlnf(cmo_name2)
C
      if((cmo_name2(1:len).eq.'-cmo-') .or.
     *   (cmo_name2(1:len).eq.'-def-')) then
C
C....    Use the Current Mesh Object as the Master.
C
         call cmo_get_name(cmo_name2,ierror_return)
C
      endif
C
C...  Search table for the Master Mesh Object.
C
      call cmo_exist(cmo_name2,iexist2)
C
      len=max(icharlnf(cmo_name1), icharlnf(cmo_name2))
C
      if(iexist2.ne.0) then
C
         ierror_return=-1
         write(logmess,'(a,a)')
     *         'Master Mesh Object does not exist: ',cmo_name2
         call writloga('default',0,logmess,0,ierr)
         go to 9999
C
      elseif(cmo_name1(1:len) .eq. cmo_name2(1:len)) then
C
C....    The two Mesh Objects are identical.
C
         ierror_return=-1
         write(logmess,'(a,a,a)')
     *      'Mesh Objects are identical: ',cmo_name1, cmo_name2
         call writloga('default',0,logmess,0,ierr)
         go to 9999
C
      endif
C
C....    Derive Mesh Object 1 from the Master Mesh Object 2.
C
      call cmo_derive(cmo_name1,cmo_name2,ierror_return)
C
C....    Search table for the Derived Mesh Object.
C
      call cmo_exist(cmo_name1,iexist1)
C
C...     Search table for the Master Mesh Object (in case of a delete).
C
      call cmo_exist(cmo_name2,iexist2)
C
      if(iexist1.ne.0) then
C
            ierror_return=-1
            write(logmess,'(a,a)')
     *         'Mesh Object does not exist: ',cmo_name1
            call writloga('default',0,logmess,0,ierr)
            go to 9999
C
      endif
C
      ierror_return=0
C
C
C.... Get the Current Mesh Object storage block info.
C
      partname='define_cmo_lg'
      call mmfindbk('cmo_natts',partname,ipcmo_natts,len,
     *                   icscode)
      call cmo_get_index(cmo_name2,index,icscode)
      call mmfindbk('cmo_attlist',cmo_name2,ipcmo_attlist,
     *                   len,icscode)
      natts=cmo_natts(index)
C
c.... Find postions of name, type, rank and length
c
 10   call mmfindbk( 'defcmo_attparam_names',partname,
     *      ipdefcmo_attparam_names,len,icscode)
      call mmfindbk( 'cmo_attparam_idefault',cmo_name2,
     *      ipcmo_attparam_idefault,len,icscode)
      call mmfindbk( 'cmo_attparam_rdefault',cmo_name2,
     *      ipcmo_attparam_rdefault,len,icscode)
      call mmfindbk( 'cmo_attparam_cdefault',cmo_name2,
     *      ipcmo_attparam_cdefault,len,icscode)
      call mmfindbk( 'cmo_attparam_idefault',cmo_name1,
     *      ipcmo_attparam_idefault1,len,icscode)
      call mmfindbk( 'cmo_attparam_rdefault',cmo_name1,
     *      ipcmo_attparam_rdefault1,len,icscode)
      call mmfindbk( 'cmo_attparam_cdefault',cmo_name1,
     *      ipcmo_attparam_cdefault1,len,icscode)
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
      do i=1,natts
         cmo_attparam_idefault1(i)=cmo_attparam_idefault(i)
         cmo_attparam_rdefault1(i)=cmo_attparam_rdefault(i)
         cmo_attparam_cdefault1(i)=cmo_attparam_cdefault(i)
         cname=cmo_attlist(number_of_params_per_att*(i-1)+posname)
         ctype=cmo_attlist(number_of_params_per_att*(i-1)+postype)
         crank=cmo_attlist(number_of_params_per_att*(i-1)+posrank)
         clength=cmo_attlist(number_of_params_per_att*(i-1)+poslen)
C
         if(ctype.eq.'VINT') then
C
C....             Get the length of the actual data.
C
            call mmfindbk(cname,cmo_name2,ipcmo_pointer2,mmlength,ier)
C
            if(ier.ne.0) then
                call cmo_mm_error('cmo_copy')
                call dotask('cmo status; mmprint; finish',ier)
            else
C
C           Set length and get the pointer to the Derived Mesh Object.
C
            call mmnewlen(cname, cmo_name1,ipcmo_pointer1,mmlength,ier)
            if(ier.ne.0) then
                        call cmo_mm_error('cmo_copy')
            else
                  do j=1,mmlength
                           icmo_pointer1(j)=icmo_pointer2(j)
                  enddo
            endif
            endif
C
         elseif(ctype.eq.'VCHAR') then
C
C....             Get the length of the actual data.
C
            call mmfindbk(cname,cmo_name2,ipcmo_pointer2,mmlength,ier)
C
            if(ier.ne.0) then
                call cmo_mm_error('cmo_copy')
                call dotask('cmo status; mmprint; finish',ier)
            else
C
C           Set length and get the pointer to the Derived Mesh Object.
C
            call mmnewlen(cname, cmo_name1,ipcmo_pointer1,mmlength,ier)
            if(ier.ne.0) then
                        call cmo_mm_error('cmo_copy')
            else
                  do j=1,mmlength
                           ccmo_pointer1(j)=ccmo_pointer2(j)
                  enddo
            endif
            endif
C
C
         elseif(ctype.eq.'VDOUBLE')then
C
C....             Get the length of the actual data
            call cmo_get_intinfo(clength,cmo_name2,length,len,itype,
     *                                ierror_return)
            call cmo_get_intinfo(crank,cmo_name2,irank,len,itype,
     *                                ierror_return)
            mmlength=max(1,irank*length)
c
C                 Get the pointer and length of the Master Mesh Object.
C
            call mmfindbk(cname,cmo_name2,
     *                          ipcmo_pointer2,mmlength,ier)
C
             if(ier.ne.0) then
                call cmo_mm_error('cmo_copy')
                call dotask('cmo status; mmprint; finish',ier)
            else
C
C                    Set length and get the pointer to the Derived Mesh Object.
C
                call mmnewlen(cname, cmo_name1,
     *                             ipcmo_pointer1,mmlength,ier)
C
            if(ier.ne.0) then
                    call cmo_mm_error('cmo_copy')
            else
                    do j=1,mmlength
                       xcmo_pointer1(j)=xcmo_pointer2(j)
                    enddo
            endif
            endif
C
         elseif(ctype(1:1).eq.'V') then
C
C....             Unsupported Type.
C
                  ierror_return=1
C
                  write(logmess,9060) cmo_name2(1:icharlnf(cmo_name2)),
     *                                cname(1:icharlnf(cname)),
     *                                ctype(1:icharlnf(ctype))
                  call writloga('default',0,logmess,0,ierr)
 9060             format('CMO_COPY error: Unsupported Type:',a,a,a)
C
         endif
C
      enddo
C
 9999 return
      end
