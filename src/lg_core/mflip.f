*dk,mflip
      subroutine mflip(memopt1,len,cname)
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
C        cname     - optional specific array
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
      implicit none
C
      include "neibor.h"

C arguments (memopt1,len,cname)
      integer memopt1,len
      character*8 cname

C variables
      character*8 cnamf
      integer ione,len4,ics
C
C ######################################################################
C BEGIN begin
C
C tam - added iremov increment, create, and release july 2012
 
      cnamf='flipmem'
      ione=1
      len4=len*4
      if(memopt1.eq.1) then
         if(cname(1:6).eq.'iremov') then
            call mmnewlen('iremov',cnamf,ipiopen ,len ,ics)
         elseif(cname(1:5).eq.'iopen') then
            call mmnewlen('iopen ',cnamf,ipiopen ,len ,ics)
         elseif(cname(1:6).eq.'ivacnt') then
            call mmnewlen('ivacnt',cnamf,ipivacnt,len ,ics)
         elseif(cname(1:4).eq.'tmp2') then
            call mmnewlen('itmp2 ',cnamf,ipitmp2 ,len ,ics)
         elseif(cname(1:6).eq.'irclst') then
            call mmnewlen('irclst',cnamf,ipirclst,len ,ics)
         endif
      elseif(memopt1.eq.99) then
         call mmrelblk('iremov',cnamf,ipiopen ,ics)
         call mmrelblk('iopen ',cnamf,ipiopen ,ics)
         call mmrelblk('ivacnt',cnamf,ipivacnt,ics)
         call mmrelblk('itmp2 ',cnamf,ipitmp2 ,ics)
         call mmrelblk('irclst',cnamf,ipirclst,ics)
      else
         call mmggetbk('iremov',cnamf,ipiopen ,ione,2,ics)
         call mmggetbk('iopen ',cnamf,ipiopen ,ione,2,ics)
         call mmggetbk('ivacnt',cnamf,ipivacnt,ione,2,ics)
         call mmggetbk('itmp2 ',cnamf,ipitmp2 ,ione,2,ics)
         call mmggetbk('irclst',cnamf,ipirclst,ione,2,ics)
      endif
      goto 9999
 9999 continue
      return
      end
