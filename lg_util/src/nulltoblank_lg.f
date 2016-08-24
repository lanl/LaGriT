      subroutine nulltoblank_lg (iword,length)
C
C
C#######################################################################
C
C      PURPOSE -
C
C      Change terminating null to a blank.
C
C      INPUT ARGUMENTS -
C
C        iword    - (character) A Character String.
C        length   - length of iword
C
C      OUTPUT ARGUMENTS -
C
C        iword     - (character) Modified character variable with null
C                      replaced by a blank
C
C      CHANGE HISTORY -
C        $Log: nulltoblank_lg.f,v $
C        Revision 2.00  2007/11/03 00:49:12  spchu
C        Import to CVS
C
CPVCS    
CPVCS       Rev 1.0   Wed Apr 12 15:08:12 2000   dcg
CPVCS    Initial revision.
C#######################################################################
C
      implicit none
C
C#######################################################################
C
      character*(*) iword
C
C#######################################################################
C
C     LOCAL VARIABLE DEFINITION
C
      integer i,length
C
C#######################################################################
C
C
C.... See if a null exists
C
      do i=length,1,-1
         if(iword(i:i).eq.char(0)) then
             iword(i:i)=' '
         endif
      enddo
 9999 return
      end
