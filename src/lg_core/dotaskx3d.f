*dk,dotaskx3d
      subroutine dotaskx3d(input_message,ierror_return)
C
C
C#######################################################################
C
C      PURPOSE -
C
C         This routine processes all x3d commands.
C
C      INPUT ARGUMENTS -
C
C         input_message - Character string containing the input
C                            message.
C
C      OUTPUT ARGUMENTS -
C
C         ierror_return - ERROR RETURN CODE (==0 ==> OK, <>0 ==> ERROR)
C
C      CHANGE HISTORY -
C
C        $Log: dotaskx3d.f,v $
C        Revision 2.00  2007/11/05 19:45:52  spchu
C        Import to CVS
C
CPVCS    
CPVCS       Rev 1.5   Wed Nov 10 09:18:58 1999   dcg
CPVCS    remove references to icdname
CPVCS
CPVCS       Rev 1.4   Mon Feb 22 16:11:26 1999   dcg
CPVCS     rewrite of command processing to allow for recursion
CPVCS    > ^D
CPVCS
CPVCS       Rev 1.3   Mon Apr 14 16:43:34 1997   pvcs
CPVCS    No change.
CPVCS
CPVCS       Rev 1.2   03/23/95 22:57:44   het
CPVCS    Add the model routines and add the cmo name into the idsbs
CPVCS
CPVCS       Rev 1.1   03/16/95 09:13:02   ejl
CPVCS    Implicit none.
CPVCS
CPVCS       Rev 1.0   01/17/95 16:32:58   pvcs
CPVCS    Original Version
C
C#######################################################################
C
      implicit real*8 (a-h, o-z)
C
C ######################################################################
C
      include "copyrite.h"
C
C#######################################################################
C
      character*(*) input_message
C
      integer ierror_return
C
C#######################################################################
C
      ierror_return=0
C
C
C     ******************************************************************
C
C     SET THE CODE NAME.
C
C
C
C     ******************************************************************
C
C     CALL THE COMMAND PROCESSOR.
C
      call dotask(input_message,ierror_return)
C
      return
      end
