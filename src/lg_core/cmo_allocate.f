      subroutine cmo_allocate(cmo_name,ierror_return)
C
C
C#######################################################################
C
C      PURPOSE -
C
C         This Routine Allocates Storage for a new Mesh Object.
C
C      INPUT ARGUMENTS -
C
C         cmo_name - (character) Mesh Object Name.
C
C      OUTPUT ARGUMENTS -
C
C         ierror_return - Error Return Code (==0 ==> OK, <>0 ==> Error)
C
C      CHANGE HISTORY -
C
C         $Log: cmo_allocate.f,v $
C         Revision 2.00  2007/11/05 19:45:48  spchu
C         Import to CVS
C
CPVCS    
CPVCS       Rev 1.3   07 Aug 2001 13:40:50   dcg
CPVCS    remove undeclared variable ctype
CPVCS    
CPVCS       Rev 1.2   10 Apr 2001 11:04:10   dcg
CPVCS    shorten too long name
CPVCS    
CPVCS       Rev 1.1   Tue Feb 01 13:41:44 2000   dcg
CPVCS
CPVCS       Rev 1.10   Mon Apr 14 16:39:54 1997   pvcs
CPVCS    No change.
CPVCS
CPVCS       Rev 1.9   11/13/95 16:21:20   dcg
CPVCS    allocate integer arrays for VINT - real for VDOUBLE
CPVCS
CPVCS       Rev 1.8   11/07/95 17:15:36   dcg
CPVCS    change flag to 2 in mmgetblk calls
CPVCS
CPVCS       Rev 1.7   09/14/95 16:38:20   dcg
CPVCS    remove dependencies on mesh_object.h data structures
CPVCS
CPVCS       Rev 1.6   09/14/95 12:09:34   dcg
CPVCS    replace character literals in call argument lists
CPVCS
CPVCS       Rev 1.5   09/11/95 14:43:16   het
CPVCS    Change to the storage block based CMO stuff.
CPVCS
CPVCS       Rev 1.4   03/15/95 15:22:22   ejl
CPVCS    Finished installing the defaults.
CPVCS
CPVCS       Rev 1.3   02/16/95 09:55:42   ejl
CPVCS    Fixed bugs, fixed hole in the Create command.
CPVCS    Added commands MODATT, LENGTH, MEMORY, & COMPRESS.
CPVCS
CPVCS       Rev 1.2   02/10/95 14:06:32   ejl
CPVCS    Fix bugs left from last update.
CPVCS
CPVCS       Rev 1.0   01/30/95 11:41:24   dcg
CPVCS     Original Version
C
C#######################################################################
C
      implicit none
      include 'cmo_lg.h'
C
C#######################################################################
C
C
C#######################################################################
C
      character*(*) cmo_name
C
      integer ierror_return
C
C#######################################################################
C
C     LOCAL VARIABLE DEFINITION
C
      integer i, j, ierr
      integer rank, length,  mmlength, len,index,
     *  posname,postype,poslength,posrank
C
      character*132 logmess
C
      pointer (ipcmo_pointer, icmo_pointer)
      pointer (ipcmo_pointer, xcmo_pointer)
      pointer (ipcmo_pointer, ccmo_pointer)
      integer icmo_pointer(*)
      REAL*8 xcmo_pointer(*)
      character*32 ccmo_pointer(*)
C
      integer icscode
C
      character*32  clength, crank,cdefault
      character*32 partname
C
      integer idefault
      real*8 xdefault
C
C#######################################################################
C
C
C#######################################################################
C
C
      partname='define_cmo_lg'
C
C.... Get the Current Mesh Object attribute info.
C
      call mmfindbk('cmo_attlist',cmo_name,ipcmo_attlist,len,icscode)
C
      if(icscode.ne.0) then
C
         ierror_return=1
         write(logmess,'(a,a)') 'Mesh Object does not exist: ',cmo_name
         call writloga('default',0,logmess,0,ierr)
         go to 9999
C
      else
C
         ierror_return=0
         call mmfindbk('cmo_natts',partname,ipcmo_natts,len,icscode)
         call mmfindbk('cmo_names',partname,ipcmo_names,len,icscode)
         do i=1,number_of_mesh_objects
           if(cmo_names(i).eq.cmo_name) then
              index=i
              go to 10
           endif
         enddo
         ierror_return=1
         write(logmess,'(a,a)') 'Mesh Object does not exist: ',cmo_name
         call writloga('default',0,logmess,0,ierr)
         go to 9999
 
C
c.... Find postions of name, type, rank and length
 10      call mmfindbk( 'defcmo_attparam_names',partname,
     *      ipdefcmo_attparam_names,len,icscode)
         call mmfindbk( 'cmo_attparam_idefault',cmo_name,
     *      ipcmo_attparam_idefault,len,icscode)
         call mmfindbk( 'cmo_attparam_rdefault',cmo_name,
     *      ipcmo_attparam_rdefault,len,icscode)
         call mmfindbk( 'cmo_attparam_cdefault',cmo_name,
     *      ipcmo_attparam_cdefault,len,icscode)
         do i=1,number_of_default_attparam_name
            if(defcmo_attparam_names(i).eq.'name') posname=i
            if(defcmo_attparam_names(i).eq.'type') postype=i
            if(defcmo_attparam_names(i).eq.'rank') posrank=i
            if(defcmo_attparam_names(i).eq.'length') poslength=i
         enddo
         do i=1,cmo_natts(index)
c.... Calculate length desired = length*rank
            clength= cmo_attlist(number_of_params_per_att*(i-1)+
     *          poslength)
            crank= cmo_attlist(number_of_params_per_att*(i-1)+posrank)
            do j=1,cmo_natts(index)
               if(cmo_attlist(number_of_params_per_att*(j-1)+posname)
     *           .eq.clength) then
                  if((cmo_attlist(number_of_params_per_att*(j-1)
     *             +postype).eq.'INT').or.
     *               (cmo_attlist(number_of_params_per_att*(j-1)
     *             +postype).eq.'VINT')) then
                      length= cmo_attparam_idefault(j)
                  elseif(cmo_attlist(number_of_params_per_att*(j-1)+
     *                 postype)
     *             .eq.'VDOUBLE') then
                      length= cmo_attparam_rdefault(j)
                  endif
               elseif(cmo_attlist(number_of_params_per_att*(j-1)+
     *            posname) .eq.crank) then
                  if(cmo_attlist(number_of_params_per_att*(j-1)+postype)
     *             .eq.'VINT') then
                      rank= cmo_attparam_idefault(j)
                  else
                      rank= cmo_attparam_rdefault(j)
                  endif
               endif
            enddo
            mmlength=max(1,length*rank)
            if(cmo_attlist(number_of_params_per_att*(i-1)+postype).eq.
     *       'VINT') then
C
               call mmgetblk(
     *              cmo_attlist(number_of_params_per_att*(i-1)+1),
     *                       cmo_name,
     *                       ipcmo_pointer,mmlength,
     *                       1,ierror_return)
C
               if(ierror_return.ne.0) then
                  call cmo_mm_error('cmo_allocate')
               else
C
C....             DEFAULT Field.
C
                  idefault=cmo_attparam_idefault(i)
                  do j=1,mmlength
                     icmo_pointer(j)=idefault
                  enddo
               endif
C
            elseif(cmo_attlist(number_of_params_per_att*(i-1)+postype)
     *          .eq.'VDOUBLE')then
C
               call mmgetblk(
     *              cmo_attlist(number_of_params_per_att*(i-1)+1),
     *                       cmo_name,
     *                       ipcmo_pointer,mmlength,
     *                       2,ierror_return)
C
               if(ierror_return.ne.0) then
                  call cmo_mm_error('cmo_allocate')
               else
                  xdefault=cmo_attparam_rdefault(i)
                  do j=1,mmlength
                     xcmo_pointer(j)=xdefault
                  enddo
               endif
           elseif(cmo_attlist(number_of_params_per_att*(i-1)+postype)
     *          .eq.'VCHAR')then
C
               call mmgetblk(
     *              cmo_attlist(number_of_params_per_att*(i-1)+1),
     *                       cmo_name,
     *                       ipcmo_pointer,mmlength,
     *                       3,ierror_return)
C
               if(ierror_return.ne.0) then
                  call cmo_mm_error('cmo_allocate')
               else
                  cdefault=cmo_attparam_cdefault(i)
                  do j=1,mmlength
                     ccmo_pointer(j)=cdefault
                  enddo
               endif
C
           elseif(cmo_attlist(number_of_params_per_att*(i-1)+postype)
     *          (1:1).eq.'VCHAR')then
C
C....             Unsupported Type.
C
                  ierror_return=1
C
                  write(logmess,9060) cmo_name,
     *    cmo_attlist(number_of_params_per_att*(i-1)+postype)
                  call writloga('default',0,logmess,0,ierr)
 9060             format('CMO_ALLOCATE error: Unsupported Type:',a,a)
C
            endif
C
         enddo
C
      endif
C
9999  return
      end
