      subroutine cmo_newlen(cmo_name,ierror_return)
C
C
C#######################################################################
C
C      PURPOSE -
C
C         This routine increments a Mesh Object to a new length.
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
C         $Log: cmo_newlen.f,v $
C         Revision 2.00  2007/11/05 19:45:49  spchu
C         Import to CVS
C
CPVCS    
CPVCS       Rev 1.3   21 Apr 2000 07:03:58   gable
CPVCS    Made setting and getting of mbndry value dynamic and problem size dependent.
CPVCS    
CPVCS       Rev 1.2   Mon Mar 13 14:40:58 2000   dcg
CPVCS    use at least a length of 1 when allocating space for attributes
CPVCS    
CPVCS       Rev 1.1   24 Jan 2000 16:20:56   dcg
CPVCS
CPVCS       Rev 1.12   Mon Apr 14 16:41:42 1997   pvcs
CPVCS    No change.
CPVCS
CPVCS       Rev 1.11   Mon Nov 18 10:29:20 1996   dcg
CPVCS    remove charater literals from argument lists
CPVCS
CPVCS       Rev 1.10   11/16/95 17:01:24   het
CPVCS    The old version did a newlen for the -def- CMO
CPVCS
CPVCS       Rev 1.9   09/11/95 14:44:34   het
CPVCS    Change to the storage block based CMO stuff.
CPVCS
CPVCS       Rev 1.8   03/15/95 15:23:38   ejl
CPVCS    Finished installing the defaults.
CPVCS
CPVCS       Rev 1.7   02/16/95 09:57:00   ejl
CPVCS    Fixed bugs, fixed hole in the Create command.
CPVCS    Added commands MODATT, LENGTH, MEMORY, & COMPRESS.
CPVCS
CPVCS       Rev 1.6   02/10/95 14:08:28   ejl
CPVCS    Fix bugs left from last update.
CPVCS
CPVCS       Rev 1.4   01/30/95 18:17:46   het
CPVCS    Add the cmo_select call to refresh cmo.h pointers
CPVCS
CPVCS       Rev 1.3   01/30/95 06:22:16   het
CPVCS    Fix several cmo errors
CPVCS
CPVCS       Rev 1.2   01/23/95 17:02:36   het
CPVCS    Correct some character problems with the table driven
CPVCS       cmo attributes.
CPVCS
CPVCS
CPVCS       Rev 1.1   01/23/95 12:38:14   het
CPVCS    Use the table driven attribute management changes.
CPVCS
CPVCS
CPVCS       Rev 1.0   01/17/95 16:34:58   pvcs
CPVCS    Original version
CPVCS
CPVCS       Rev 1.0   12/09/94 22:49:34   het
CPVCS    Original version.
CPVCS
C
C#######################################################################
C
      implicit none
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
      integer i, ierr, ier
      integer irank, length, mmlength, len,ilen,itype,index,nmcmoatt
C
      character*132 logmess
C
      pointer (ipcmo_pointer, icmo_pointer)
      integer icmo_pointer(10000000)
C
      integer icscode
C
      character*32 cname, ctype,crank, clen,cinter,cpers,cio
C
C#######################################################################
C
      integer icharlnf
C
C#######################################################################
C
      len=icharlnf(cmo_name)
C
      if((cmo_name(1:len).eq.'-cmo-') .or.
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
      call cmo_exist(cmo_name,icscode)
C
      if(icscode.ne.0) then
C
         ierror_return=-1
C
         write(logmess,'(a,a)')
     *      'Mesh Object does not exist: ',cmo_name
         call writloga('default',0,logmess,0,ierr)
C
      else
C
         ierror_return=0
C
C....    Set the new Length of the Mesh Object Memory Managed Arrays.
C
         call cmo_get_info('number_of_attributes',cmo_name,nmcmoatt,
     *   ilen,itype,icscode)
         do i=1,nmcmoatt
C
C....       NAME Field.
C
            call cmo_get_attribute_name(cmo_name,i,
     *                     cname,icscode)
 
            call cmo_get_attparam(cname,cmo_name,index,ctype,crank,
     *        clen,cinter,cpers,cio,icscode)
C
C....       TYPE Field.
C
            if(ctype(1:1).eq.'V') then
C
C....          Memory Managed Attribute.
C
               call cmo_get_length(cname,cmo_name,length,irank,
     *                             ierror_return)
C
               mmlength=max(1,irank*length)
C
               call mmnewlen(cname,
     *                       cmo_name,
     *                       ipcmo_pointer,mmlength,
     *                       ier)
C
               if(ier.ne.0) call cmo_mm_error('cmo_newlen')
C
            endif
C
         enddo
C
C....       Current Mesh Object
C
         call cmo_select(cmo_name,ierror_return)
         call set_mbndry()
C
      endif
C
      return
      end
