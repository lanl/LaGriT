*dk,icharln
      integer function icharln(iword)
C
C
C#######################################################################
C
C      PURPOSE -
C
C      Find the Length of a Character String by Searching Forward
C         until the first Blank or Null Character is found..
C
C      INPUT ARGUMENTS -
C
C        iword    - (character) A Character String.
C
C      OUTPUT ARGUMENTS -
C
C        icharln  - (integer) The length of the Character String.
C
C      CHANGE HISTORY -
C
C        $Log: icharln.f,v $
C        Revision 2.00  2007/11/03 00:49:11  spchu
C        Import to CVS
C
CPVCS    
CPVCS       Rev 1.0   05/31/95 10:45:52   ejl
CPVCS    New function to return lwength of left-justified character string.
CPVCS    
C
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
      integer lenmax, istop
C
C#######################################################################
C
C
C
      lenmax = len(iword)
C
      istop=0
C
C.... Find the first Blank or Null Character.
C     (the character is tested inside the loop: Fortran may test
C     both sides of an .and., and iword(lenmax+1:lenmax+1) is
C     past the end of iword)
C
      do while (istop .lt. lenmax)
         if ((iword(istop+1:istop+1) .eq. char(0)) .or.
     *       (iword(istop+1:istop+1) .eq. ' ')) exit
C
         istop = istop+1
C
      enddo
C
C.... Number of characters in iword.
C
      icharln = istop
C
      return
      end
