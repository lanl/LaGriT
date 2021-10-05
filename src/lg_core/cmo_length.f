      subroutine cmo_length(cmo_name,attribute,ierror_return)
C
C
C#######################################################################
C
C      PURPOSE -
C
C         This Routine returns the Length of Mesh Object Arrays..
C
C         FORMAT: CMO / LENGTH / name / attribute
C
C      INPUT ARGUMENTS -
C
C         cmo_name  - (character) Mesh Object Name or all.
C         attribute - (character) Mesh Object Attribute or all
C
C      OUTPUT ARGUMENTS -
C
C         ierror_return - Error Return Code (==0 ==> OK, <>0 ==> Error)
C
C      CHANGE HISTORY -
C
C         $Log: cmo_length.f,v $
C         Revision 2.00  2007/11/05 19:45:49  spchu
C         Import to CVS
C
CPVCS    
CPVCS       Rev 1.0   21 Jan 2000 17:03:08   dcg
CPVCS     
CPVCS    
CPVCS       Rev 1.7   Thu Mar 05 16:28:40 1998   dcg
CPVCS    fix typo on name of pointer to cmo storage block
CPVCS    
CPVCS       Rev 1.6   Mon Apr 14 16:41:24 1997   pvcs
CPVCS    No change.
CPVCS    
CPVCS       Rev 1.5   Mon Nov 18 10:29:04 1996   dcg
CPVCS    remove charater literals from argument lists
CPVCS
CPVCS       Rev 1.4   09/11/95 14:44:12   het
CPVCS    Change to the storage block based CMO stuff.
CPVCS
CPVCS       Rev 1.3   03/15/95 15:23:08   ejl
CPVCS    Finished installing the defaults.
CPVCS
CPVCS       Rev 1.2   02/16/95 09:56:48   ejl
CPVCS    Fixed bugs, fixed hole in the Create command.
CPVCS    Added commands MODATT, LENGTH, MEMORY, & COMPRESS.
CPVCS
CPVCS       Rev 1.1   02/10/95 14:08:00   ejl
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
      character*(*) cmo_name, attribute
C
      integer ierror_return
C
C#######################################################################
C
C     LOCAL VARIABLE DEFINITION
C
      integer i, j, len, len1, len2, ier, ierr, ifound1, ifound2
C
      integer length, irank, mmlength, icscode, lentype,ilen,natts,
     *  itype,index,ierror
C
      character*32 cname, crank, clength, ctype,  cinter,cpers,
     *  cio, partname
C
      character*132 logmess
C
      pointer (ipcmo_pointer, icmo_pointer)
      integer icmo_pointer(10000000)
C
C#######################################################################
C
      integer icharlnf
C
C#######################################################################
C
C
      if(number_of_mesh_objects.lt.1) then
C
         ierror_return=0
C
         write(logmess,'(a)')
     *      'No Mesh Objects Defined. '
         call writloga('default',0,logmess,0,ierr)
C
      else
C
         ierror_return=0
C
         len=icharlnf(cmo_name)
C
         if((cmo_name(1:len).eq.'-cmo-') .or.
     *      (cmo_name(1:len).eq.'-def-')) then
C
C....       Use the Current Mesh Object.
C
            call cmo_get_name(cmo_name,ierror_return)
C
            len=icharlnf(cmo_name)
C
         endif
C
         len2=icharlnf(attribute)
C
         ifound1=0
C
         partname='cmo_define_lg'
         call mmfindbk('cmo_names',partname,ipcmo_names,ilen,icscode)

C
         do i=1,number_of_mesh_objects
C
C
C....       Get the CMO_NAME Field.
C
            if(cmo_names(i)(1:len1).eq.cmo_name(1:len1) .or.
     *            cmo_name(1:len).eq.'-all-') then
C
               ifound1=1
C
               write(logmess,'(a,a)') '  Mesh Object: ', cmo_names(i)
               call writloga('default',2,logmess,0,ierr)
C
               write(logmess,9000) 'ATTRIBUTE', 'LENGTH   ',
     *                             'RANK     ', 'MMLENGTH '
               call writloga('default',2,logmess,1,ierr)
 9000          format(4x,a10,14x,3(a10))
C
               ifound2=0
C
C...           Get the Mesh Object attributes
C
               call cmo_get_info('number_of_attributes',cmo_name,natts,
     *            ilen,itype,icscode)
 
               do j=1,natts
                  call cmo_get_attribute_name(cmo_name,j,cname,ier)
                  call cmo_get_attparam(cname,cmo_name,index,ctype,crank
     *              ,clength,cinter,cpers,cio,ierror)
C
                  if(ctype(1:1).eq.'V') then
C
                     len1=max(len2,icharlnf(cname))
C
                     if(attribute(1:len1).eq.cname(1:len1) .or.
     *                  attribute(1:len2).eq.'-all-') then
C
                        ifound2=1
C
C....                   Get the Length.
C
                        call cmo_get_length(cname,cmo_name,length,irank,
     *                                ierror_return)
C
                        call mmfindbk(cname,cmo_names(i),
     *                                ipcmo_pointer,mmlength,ier)
                        if(ier.ne.0) call cmo_mm_error('cmo_length')
C
                        write(logmess,9020)
     *                             cname,length,irank,mmlength
                        call writloga('default',0,logmess,0,ierr)
 9020                   format(4x,a20,3i10)
C
                     endif
C
                  endif
C
               enddo
C
               if(ifound2.le.0) then
C
C....             Attribute does not exist.
C
                  ierror_return=-1
                  write(logmess,9040) attribute
                  call writloga('default',0,logmess,0,ierr)
 9040             format(' Attribute does not exist: ',a32)
C
               endif
C
            endif
C
         enddo
C
         if(ifound1.le.0) then
C
C....       Mesh Object does not exist.
C
            ierror_return=-1
            write(logmess,9060) cmo_name
            call writloga('default',0,logmess,0,ierr)
 9060       format(' Mesh Object does not exist: ',a32)
C
        endif
C
      endif
C
      return
      end
 
