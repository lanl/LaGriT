*dk,memory
      subroutine memory(imsgin,xmsgin,cmsgin,msgtype,nwds,
     *                  ierror_return)
C
C
C#######################################################################
C
C      PURPOSE -
C
C         Create a new pset.
C
C      INPUT ARGUMENTS -
C
C         imsgin()  - Integer array of command input tokens
C         xmsgin()  - Real array of command input tokens
C         cmsgin()  - Character array of command input tokens
C         msgtype() - Integer array of command input token types
C         nwds      - Number of command input tokens
C
C      OUTPUT ARGUMENTS -
C
C         ierror_return - Error Return Code (==0 ==> OK, <>0 ==> Error)
C
C      CHANGE HISTORY -
C
C         $Log:   /pvcs.config/t3d/src/memory.f_a  $
CPVCS    
CPVCS       Rev 1.3   02 May 2001 10:18:40   dcg
CPVCS
CPVCS       Rev 1.2   Mon Apr 14 16:53:26 1997   pvcs
CPVCS    No change.
CPVCS
CPVCS       Rev 1.1   07/17/95 16:11:44   dcg
CPVCS    original version
C
C#######################################################################
C
      implicit none
C
C#######################################################################
C
      integer nwds, imsgin(nwds), msgtype(nwds)
      REAL*8 xmsgin(nwds)
      character*(*) cmsgin(nwds)
C
      integer ierror_return
C
C#######################################################################
C
      integer npoints, nelements
C
      character*32 cmo
C
C#######################################################################
C
C
C
      if(nwds.le.1) then
         npoints   = 1000
         nelements = 6*npoints
      elseif(nwds.le.2) then
         npoints   = imsgin(2)
         nelements = 6*npoints
      else
         npoints   = imsgin(2)
         nelements = imsgin(3)
      endif
C
C.... Get the Current Mesh Object.
C
      call cmo_get_name(cmo, ierror_return)
C
      if(ierror_return .eq. 0) then
C
C....    Adjust length of Memory Managed Arrays.
C
         call cmo_memory(cmo, npoints, nelements, ierror_return)
C
      endif
C
c
      return
      end
*dk,readdump
