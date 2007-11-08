*DK ocnulvec
      subroutine ocnulvec(v,n)
C
C
C#######################################################################
C
C      PURPOSE -
C
C         THIS ROUTINE PROCESSES ALL X3D COMMANDS.
C
C      INPUT ARGUMENTS -
C
C         input_message - CHARACTER STRING CONTAINING THE INPUT
C                            MESSAGE.
C
C      OUTPUT ARGUMENTS -
C
C         ierror_return - ERROR RETURN CODE (==0 ==> OK, <>0 ==> ERROR)
C
C      CHANGE HISTORY -
C
C        $Log: ocnulvec.f,v $
C        Revision 2.00  2007/11/05 19:46:03  spchu
C        Import to CVS
C
CPVCS    
CPVCS       Rev 1.1   Mon Apr 14 16:56:14 1997   pvcs
CPVCS    No change.
CPVCS    
CPVCS       Rev 1.0   01/17/95 16:39:14   pvcs
CPVCS     Original version
C
C#######################################################################
C
      implicit real*8 (a-h, o-z)
      character*132 logmess
C
C#######################################################################
C
      real v(1)
      do 20 i=1,n
 20   v(i) = 0.
      return
      end
