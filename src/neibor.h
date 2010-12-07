*CD,neibor
C
C######################################################################
C
C      CHANGE HISTORY -
C
C        $Log: neibor.h,v $
C        Revision 2.00  2007/11/05 19:46:02  spchu
C        Import to CVS
C
CPVCS    
CPVCS       Rev 1.10   Fri Nov 12 09:06:48 1999   dcg
CPVCS    remove unused variables
CPVCS    
CPVCS       Rev 1.9   Tue Oct 26 14:08:10 1999   dcg
CPVCS    No change.
CPVCS    
CPVCS       Rev 1.8   Thu Jan 21 20:58:54 1999   jtg
CPVCS    common blocks moved after declarations and/or saves added
CPVCS    
CPVCS       Rev 1.7   Fri Jun 19 09:40:30 1998   dcg
CPVCS    remove duplicate declarations
CPVCS
CPVCS       Rev 1.6   Thu May 07 11:41:50 1998   dcg
CPVCS    add declarations
CPVCS
CPVCS       Rev 1.5   Mon Apr 14 16:37:32 1997   pvcs
CPVCS    No change.
CPVCS
CPVCS       Rev 1.4   01/04/95 22:07:06   llt
CPVCS    unicos changes (made by het)
CPVCS
CPVCS       Rev 1.3   12/01/94 18:52:10   het
CPVCS    Modified the "cmo" support data structures.
CPVCS
CPVCS
CPVCS       Rev 1.2   11/17/94 21:59:40   het
CPVCS    Modified the pointer declarations to be consistent with the changes to
CPVCS    the mflip subroutine.
CPVCS
CPVCS
CPVCS       Rev 1.1   11/13/94 17:20:08   het
CPVCS
CPVCS
CPVCS       Rev 1.0   11/13/94 15:41:30   het
CPVCS    Orginal Version
C
C######################################################################
C
      character*4      ininbr
      common /csearch/ ininbr
      save   /csearch/
      integer maskface
      parameter ( maskface=3)
      integer        iflist,    iflists
      common /faces/ iflist(12),iflists(9)
      integer        ielist,    ielist2
      common /edges/ ielist(24),ielist2(12)
      integer          itpconv
      common /pnttype/ itpconv(0:50)
      integer           ifliplst
      common /ifliplst/ ifliplst(24)
      integer         irecnall
      common/matrecfl/irecnall
 
      save /faces/
      save /edges/
      save /pnttype/
      save /ifliplst/
      save/matrecfl/

C
C
C     ******************************************************************
C     POINTERS TO "TET" MEMORY WITH CHARACTERISTIC "ntetmax"
C
      integer ntetmax,itetcnt,nfixmax
      pointer( ipkfix   , kfix(4,1)   )
      pointer( ipkfix   , kfix1(1)    )
      pointer( ipxfix   , xfix(4,1)   )
      pointer( ipxfix   , xfix1(1)    )
      pointer( ipifittet , ifittet(1)  )
      integer kfix,kfix1,ifittet
      real *8 xfix,xfix1
      common /tetmem/ ipkfix,ipxfix,ipifittet,ntetmax,itetcnt,nfixmax
      save   /tetmem/
C
C

C     ******************************************************************
C     POINTERS TO "FLIP" MEMORY
C
      integer  lenremov,leniopen,lenvacnt,nvacnt,lentmp2,
     *                 lenrclst,nrecon,nmats,lenrho0l,
     *                 lenaddpt
      real*8 xrclst
      pointer( ipiremov , iremov(1)  )
      pointer( ipiopen  , iopen(1)   )
      pointer( ipivacnt , ivacnt(1)  )
      pointer( ipitmp2  , itmp2(1)   )
      pointer( ipirclst , irclst(1)  )
      pointer( ipxrclst , xrclst(1)  )
      pointer( ipmatrecon , matrecon(1))
      integer iremov,iopen,ivacnt,itmp2,irclst
     *  ,matrecon
 

      common /flipmem/ ipiremov,ipiopen,
     *                 ipivacnt,ipitmp2,ipirclst,ipxrclst,
     *                 ipmatrecon,nmats,
     *                 lenremov,leniopen,lenvacnt,nvacnt,lentmp2,
     *                 lenrclst,nrecon
      save /flipmem/
C
C     ******************************************************************
C     LOG VARIABLES
C
      character*132 logdan
      common /logdata/ logdan
      save /logdata/
C
C     ******************************************************************
 
