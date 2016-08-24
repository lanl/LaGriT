      integer function icharlnb(iword)
C
C
C#######################################################################
C
C      PURPOSE -
C
C      Find the Length of a Character String by Searching Backwards
C         until the first Non-Blank and Non-Null Character is found.
C
C      INPUT ARGUMENTS -
C
C        iword    - (character) A Character String.
C
C      OUTPUT ARGUMENTS -
C
C        icharlnb  - (integer) The Length of the Character String.
C
C      CHANGE HISTORY -
C
C        $Log: icharlnb.f,v $
C        Revision 2.00  2007/11/03 00:49:11  spchu
C        Import to CVS
C
CPVCS    
CPVCS       Rev 1.1   02/23/95 13:09:22   ejl
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
      integer istop
C
C#######################################################################
C
C
C
      istop=len(iword)
C
C.... Find the first Non-Blank and Non-Null Character.
C
      do while ((istop .gt. 0) .and. 
     *          ((iword(istop:istop) .eq. ' ') .or.
     *           (iword(istop:istop) .eq. char(0))))
C
         istop=istop-1
C
      enddo

      icharlnb=istop
C
      return
      end
