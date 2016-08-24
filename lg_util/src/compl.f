*dk,compl
      integer function compl(num)
C
C #####################################################################
C
C     PURPOSE -
C
C        None
C
C     INPUT ARGUMENTS -
C
C        None
C
C     OUTPUT ARGUMENTS -
C
C        None
C
C     CHANGE HISTORY -
C
C        $Log: compl.f,v $
C        Revision 2.00  2007/11/03 00:49:10  spchu
C        Import to CVS
C
CPVCS    
CPVCS       Rev 1.2   10 Aug 2005 10:41:30   dcg
CPVCS    replace .not. with  call to function not
CPVCS
CPVCS       Rev 1.1   01/04/95 21:54:16   llt
CPVCS    unicos changes (made by het)
CPVCS
CPVCS       Rev 1.0   11/10/94 12:40:58   pvcs
CPVCS    Original version.
C
C ######################################################################
C
       implicit real*8 (a-h,o-z)
      compl=not(num)
      return
      end
