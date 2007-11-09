*dk,writfls
      subroutine writfls(iopt,ierr)
C
C
C#######################################################################
C
C     PURPOSE -
C
C        THIS ROUTINE FLUSHES THE LOG(S) OUTPUT BUFFER(S).
C
C     INPUT ARGUMENTS -
C
C        iopt     - THE OUTPUT BUFFER TO FLUSH.
C
C     OUTPUT ARGUMENTS -
C
C        ierr   - ERROR FLAG. = 0  ==> O.K.
C
C     CHANGE HISTORY -
C
C        $Log:   /pvcs.config/t3d/src/writfls.f_a  $
CPVCS    
CPVCS       Rev 1.3   07 Feb 2000 11:10:32   tam
CPVCS    No change.
CPVCS    
CPVCS       Rev 1.2   Mon Apr 14 17:05:58 1997   pvcs
CPVCS    No change.
CPVCS    
CPVCS       Rev 1.1   01/26/95 08:01:16   ejl
CPVCS    Cleaned up, Fixed holes in the logic, implicit none.
CPVCS    Installed logcom1.h and blockini.h
c
c   Rev 1.0   01/26/95 07:58:06   ejl
cCleaned up, Fixed holes in the logic, implicit none
cInstaled logcom1.h and blockini.h
CPVCS    
CPVCS       Rev 1.0   11/10/94 12:20:26   pvcs
CPVCS    Original version.
C
C
C#######################################################################
C
C
      implicit none
C
C#######################################################################
C
      character*(*) iopt
C
      integer ierr
C
C#######################################################################
C
      include 'logcom1.h'
C
C#######################################################################
C
      integer i, iunit, lenname

      character*32 ifile
C
C#######################################################################
C
      integer lunget, lenchar
C
C#######################################################################
C
C
      ierr=1
C
      do i=1,numlogs
         if(logtype(i).eq.iopt.or.logname(i).eq.iopt) then
            ierr=0
            iunit=lunget(logname(i))
            ifile=logname(i)
            lenname=lenchar(ifile)
C****       close(iunit)
C****       open(unit=iunit,file=ifile(1:lenname),
C*****           status='unknown')
         endif
      enddo
C
      return
      end
