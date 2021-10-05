      subroutine cmo_compress(cmo_name,ierror_return)
C
C
C#######################################################################
C
C     PURPOSE -
C
C        This Routine Compresses the Mesh Object Memory Managed Arrays
C        to their minimum size.
C
C     INPUT ARGUMENTS -
C
C        cmo_name - (character) Name of the Mesh Object.
C
C     OUTPUT ARGUMENTS -
C
C        ierror_return - Error Return Code (==0 ==> OK, <>0 ==> Error)
C
C     CHANGE HISTORY -
C
C        $Log: cmo_compress.f,v $
C        Revision 2.00  2007/11/05 19:45:48  spchu
C        Import to CVS
C
CPVCS    
CPVCS       Rev 1.0   24 Jan 2000 13:24:36   dcg
CPVCS    Initial revision.
CPVCS    
CPVCS       Rev 1.2   Mon Apr 14 16:40:00 1997   pvcs
CPVCS    No change.
CPVCS    
CPVCS       Rev 1.1   09/14/95 16:39:06   dcg
CPVCS    remove dependencies on mesh_object.h data structures
CPVCS
CPVCS       Rev 1.0   03/16/95 10:30:38   ejl
CPVCS    New option to compress length of CMO's.
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
      character*(*) cmo_name
      character*32 partname
C
      integer ierror_return
C
C#######################################################################
C
C     LOCAL VARIABLE DEFINITION
C
      integer ilen, i,itype,ics,len
C
C#######################################################################
C
      integer icharlnf
C
C#######################################################################
C
C
      partname='define_cmo_lg'
      len=icharlnf(cmo_name)
C
      if(cmo_name(1:len).eq.'-all-') then
C
C....    Compress all Mesh Objects.
C
         call mmfindbk('cmo_names',partname,ipcmo_names,ilen,itype,ics)
         do i=1,number_of_mesh_objects
C
C....       NAME Field.
C
            cmo_name=cmo_names(i)
            call cmo_newlen(cmo_name,ierror_return)
         enddo
C
      else
C
C....    Compress Mesh Object 'cmo_name'.
C
         call cmo_newlen(cmo_name,ierror_return)
C
      endif
C
      return
      end
