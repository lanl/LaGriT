*dk,killcode
      subroutine killcode(imessage)
C
C
C#######################################################################
C
C     PURPOSE -
C
C        TERMINATE CODE WITH MESSAGE AND TRACEBACK.
C
C
C     INPUT ARGUMENTS -
C
C        imessage - TERMINATION MESSAGE
C
C
C     OUTPUT ARGUMENTS -
C
C        NONE
C
C
C     CHANGE HISTORY -
C
C        $Log:   /pvcs.config/t3d/src/killcode.f_a  $
CPVCS    
CPVCS       Rev 1.4   Tue Nov 09 15:59:42 1999   dcg
CPVCS    remove call to sqtcbk (stub routine)
CPVCS
CPVCS       Rev 1.3   Mon Apr 14 16:52:32 1997   pvcs
CPVCS    No change.
CPVCS
CPVCS       Rev 1.2   08/29/95 11:42:18   dcg
CPVCS    set length for names to 40 characters
CPVCS
CPVCS       Rev 1.1   01/26/95 14:01:18   ejl
CPVCS    Cleaned up, implicit none
CPVCS
CPVCS       Rev 1.0   11/10/94 12:15:44   pvcs
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
      character*(*) imessage
C
C#######################################################################
C
      integer ierr
C
      character*40 interfil
C
C#######################################################################
C
C
C     ******************************************************************
C
C     WRITE TERMINATION MESSAGE TO LOG FILES.
C
      write(interfil,9000)
      call writloga('default',2,interfil,0,ierr)
 9000 format('The code will stop with the message:')
      call writloga('default',1,imessage,2,ierr)
C
C     ******************************************************************
C
C     TERMINATE CODE.
C
      call exita(1)
C
C     ******************************************************************
C
      return
      end
