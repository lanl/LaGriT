*dk,mflip
      subroutine mflip(memopt1,len,ivec)
       implicit real*8 (a-h,o-z)
C
C #####################################################################
C
C     PURPOSE -
C
C        This routine obtains memory for the tetrahedral arrays
C
C     INPUT ARGUMENTS -
C
C        memopt1 - the memory option:
C                    0 - initialize memory
C                    1 - increment memory
C                    99- release memory
C        len      - length to allocate
C        ivec     - optional specific array
C
C     OUTPUT ARGUMENTS -
C
C        None
C
C     CHANGE HISTORY -
C
C        DF0704AA-87, DF0331AA-88
C
C
C        $Log: mflip.f,v $
C        Revision 2.00  2007/11/05 19:46:02  spchu
C        Import to CVS
C
CPVCS
CPVCS       Rev 1.21   02 Oct 2007 12:40:28   spchu
CPVCS    original version
C
C ######################################################################
C
      include "neibor.h"
C
C ######################################################################
C
      character*8 ivec, cnamf
C
C ######################################################################
C
      cnamf='flipmem'
      ione=1
      len4=len*4
      if(memopt1.eq.1) then
         if(ivec(1:6).eq.'iremov') then
         elseif(ivec(1:5).eq.'iopen') then
            call mmnewlen('iopen ',cnamf,ipiopen ,len ,ics)
         elseif(ivec(1:6).eq.'ivacnt') then
            call mmnewlen('ivacnt',cnamf,ipivacnt,len ,ics)
         elseif(ivec(1:4).eq.'tmp2') then
            call mmnewlen('itmp2 ',cnamf,ipitmp2 ,len ,ics)
         elseif(ivec(1:6).eq.'irclst') then
            call mmnewlen('irclst',cnamf,ipirclst,len ,ics)
         endif
      elseif(memopt1.eq.99) then
         call mmrelblk('iopen ',cnamf,ipiopen ,ics)
         call mmrelblk('ivacnt',cnamf,ipivacnt,ics)
         call mmrelblk('itmp2 ',cnamf,ipitmp2 ,ics)
         call mmrelblk('irclst',cnamf,ipirclst,ics)
      else
         call mmggetbk('iopen ',cnamf,ipiopen ,ione,2,ics)
         call mmggetbk('ivacnt',cnamf,ipivacnt,ione,2,ics)
         call mmggetbk('itmp2 ',cnamf,ipitmp2 ,ione,2,ics)
         call mmggetbk('irclst',cnamf,ipirclst,ione,2,ics)
      endif
      goto 9999
 9999 continue
      return
      end
