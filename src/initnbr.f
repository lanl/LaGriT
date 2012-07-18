*dk,initnbr
      subroutine initnbr
C
C #####################################################################
C
C     PURPOSE -
C
C        This routine initializes necessary data for the neibor routines
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
C        $Log: initnbr.f,v $
C        Revision 2.00  2007/11/05 19:45:58  spchu
C        Import to CVS
C
CPVCS    
CPVCS       Rev 1.5   Wed Nov 10 09:56:52 1999   dcg
CPVCS    remove unused code
CPVCS
CPVCS       Rev 1.4   Wed Sep 30 12:09:32 1998   dcg
CPVCS    remove call that allocate storage that is not used
CPVCS
CPVCS       Rev 1.3   Wed Sep 30 11:35:00 1998   dcg
CPVCS    implicit none
CPVCS    remove unused common
CPVCS
CPVCS       Rev 1.2   Mon Apr 14 16:51:46 1997   pvcs
CPVCS    No change.
CPVCS
CPVCS       Rev 1.1   11/17/94 21:53:34   het
CPVCS    Added include files for chydro, neibor, cmerge, comdict. Added calles and
CPVCS    pointer statements for current_mesh_object database access.
CPVCS
CPVCS
CPVCS    Original version.
C
C #####################################################################
      implicit none
C
      include "neibor.h"
C
C #####################################################################
C
C                                                                       cdgnrac2
C     ******************************************************************cdgnrac3
C     POINTERS TO DEGENERACY MEMORY WITH CHARACTERISTIC LENGTH "lendeg" cdgnrac4
C                                                                       cdgnrac5
c     common /dgn/ ipiface,ipivert,iptest,ipit0,ipit1,ipitx,iptmpx,     cdgnrac6
c    *             lendeg                                               cdgnrac7
c     pointer( ipiface  , iface(1)   )                                  cdgnrac8
c     pointer( ipivert  , ivert(3,1) )                                  cdgnrac9
c     pointer( iptest   , test(4,1)  )                                  cdgnra10
c     pointer( ipit0    , it0(2,1)   )                                  cdgnra11
c     pointer( ipit1    , it1(3,1)   )                                  cdgnra12
c     pointer( ipitx    , itx(1)     )                                  cdgnra13
c     pointer( iptmpx   , tmpx(1)    )                                  cdgnra14

      integer izero,ione,idum1
      character*8 cdum2
C
C ####################################################################
C BEGIN begin
C
      ininbr='done'
      cdum2='-notset-'
      izero=0
      ione=1
      idum1=0
C
C     ******************************************************************
C
C     "neibor" DATA
C
      ntetmax=0
      itetcnt=0
C
C     ******************************************************************
C
C     "face" DATA
C
      iflist(1)=2
      iflist(2)=3
      iflist(3)=4
      iflist(4)=1
      iflist(5)=4
      iflist(6)=3
      iflist(7)=1
      iflist(8)=2
      iflist(9)=4
      iflist(10)=1
      iflist(11)=3
      iflist(12)=2
C
C     ******************************************************************
C
C     "face3ds" DATA
C
      iflists(1)=2
      iflists(2)=3
      iflists(3)=1
      iflists(4)=3
      iflists(5)=1
      iflists(6)=2
      iflists(7)=1
      iflists(8)=2
      iflists(9)=3
C
C     ******************************************************************
C
C     "ielist" OR EDGELIST DATA
C
      ielist(1)=1
      ielist(2)=2
      ielist(3)=3
      ielist(4)=4
      ielist(5)=1
      ielist(6)=3
      ielist(7)=4
      ielist(8)=2
      ielist(9)=1
      ielist(10)=4
      ielist(11)=2
      ielist(12)=3
      ielist(13)=2
      ielist(14)=3
      ielist(15)=1
      ielist(16)=4
      ielist(17)=2
      ielist(18)=4
      ielist(19)=3
      ielist(20)=1
      ielist(21)=3
      ielist(22)=4
      ielist(23)=1
      ielist(24)=2
C
C     ******************************************************************
C
C     "ielist2" OR ALTERNATIVE EDGELIST DATA
C
      ielist2(1)=4
      ielist2(2)=5
      ielist2(3)=6
      ielist2(4)=2
      ielist2(5)=3
      ielist2(6)=6
      ielist2(7)=1
      ielist2(8)=3
      ielist2(9)=5
      ielist2(10)=1
      ielist2(11)=2
      ielist2(12)=4
C
C     ******************************************************************
C
C     "ifliplst" OR FLIP-LIST DATA
C
      ifliplst(1)=1
      ifliplst(2)=4
      ifliplst(3)=3
      ifliplst(4)=2
      ifliplst(5)=3
      ifliplst(6)=4
      ifliplst(7)=2
      ifliplst(8)=4
      ifliplst(9)=5
      ifliplst(10)=1
      ifliplst(11)=5
      ifliplst(12)=4
      ifliplst(13)=2
      ifliplst(14)=6
      ifliplst(15)=3
      ifliplst(16)=1
      ifliplst(17)=3
      ifliplst(18)=6
      ifliplst(19)=2
      ifliplst(20)=5
      ifliplst(21)=6
      ifliplst(22)=1
      ifliplst(23)=6
      ifliplst(24)=5
C
C     ******************************************************************
C
C     "itpconv" OR POINT TYPE CONVERSION DATA
C
      itpconv(0)=0
      itpconv(2)=2
      itpconv(10)=1
      itpconv(11)=1
      itpconv(12)=5
C
C     ******************************************************************
C
C     INITIALIZE MEMORY
C
      izero=0
      ione=1
C
C     corrected cdum2 from integer to character cdum2 
      call mflip(izero,idum1,cdum2)
      lenremov=0
      leniopen=0
      lenvacnt=0
      lentmp2=0
      lenrclst=2000
      call mflip(ione,lenrclst,'irclst')
C
      goto 9999
 9999 continue
      return
      end
