*dk,lenchar
      function lenchar(iword)
C
C#######################################################################
C
C      PURPOSE -
C
C      FIND THE LENGTH OF A CHARACTER STRING BY SEARCHING BACKWARDS
C         UNTIL THE FIRST NON-BLANK CHARACTER IS FOUND.
C
C      INPUT ARGUMENTS -
C
C        iword    - A CHARACTER VARIABLE.
C
C      OUTPUT ARGUMENTS -
C
C        lenchar  - THE LENGTH OF THE CHARACTER STRING.
C
C      CHANGE HISTORY -
C
C        $Log: lenchar.f,v $
C        Revision 2.00  2007/11/03 00:49:12  spchu
C        Import to CVS
C
CPVCS    
CPVCS       Rev 1.0   11/10/94 12:42:58   pvcs
CPVCS    Original version.
C
C#######################################################################
C
      character iword*(*)
C
C#######################################################################
C
      do i=len(iword),1,-1
         if(iword(i:i).ne.' ') then
            istart=1
            istop=i
            goto 100
         endif
      enddo
      istart=1
      istop=len(iword)
 100  continue
      lenchar=istop
      goto 9999
 9999 continue
      return
      end
